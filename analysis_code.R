library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(assertthat)
library(caret)
library(jsonlite)
library(tseries)
library(aTSA)
library(latticeExtra)
library(glmnet)
library(xgboost)
library(cowplot)
library(DCCA)
library(boot)

setwd('~/Documents/Economics/ECON643/Project 1')

#**************************************************************************************************
#********************************** DATA CLEANING AND PROCESSING **********************************
#**************************************************************************************************

#Ok so first we need the average price for each month There's probaby some smart 
#way to do this in Bloomberg directly, but faster for me to just do it here.

raw_nzd_usd_data <- read_xlsx('NZDUSD Data.xlsx')

#First we need to classify every day into a month and then we find the average
nzd_usd_data_by_month <- raw_nzd_usd_data %>%
    mutate(month = month(date), #make some variables for month
           year = year(date)) %>%
    group_by(month, year) %>%
    summarise(mean_usd_price = mean(price_usd)) %>%
    ungroup() %>%
    arrange(year, month)


#alright so now we have average FX data by month, now we need to add it to our data

#load in our average conversion rate data
conversion_rate_data <- read_xlsx('Conversion Rate Data.xlsx', sheet='Quaterly Conversion Rates')

#This gets kinda confusing so pay attention. 
#The "Year Section" column in the conversion_rate_data looks like this:
#    Year Section
#1)  2020 1
#2)  2020 2
#3)  2020 3
#4)  2020 4
#5)  2020 5
#6)  2019 1

#Remember in the Fonterra Milk Price Statements there are 5 "quarters" for each season. 
#So in the 2020 season we have:
#Q1 - from 2019 Aug to 2019 Oct
#Q2 - from 2019 Nov to 2020 Jan
#Q3 - from 2020 Feb to 2020 Apr
#Q4 - from 2020 May to 2020 Jul
#Q5 - from 2020 Aug to 2020 Oct (this overlaps with the next season)

#Hence, for each year section we want to find the actual Year and Month that that quarter starts on,
#and then use that to find the right lagged FX rate.
#That is what we do in the next section

#need to make this into a function, as we use it again when we do our RBNZ projections
#as you can see though this used to be inline code, and if you remove the function part
#it will still run perfectly fine inline
combine_quarterly_conversion_with_nzd_usd <- function(conversion_rate_data, nzd_usd_data_by_month) {
    conversion_rate_data  <- conversion_rate_data %>% 
        #first split up Year Section into a season and quarter
        separate(`Year Section`, c("season", "quarter"), sep = " ") %>%
        mutate(month = case_when(quarter == 1 ~ 8, #Q1 starts in Aug
                                 quarter == 2 ~ 11, #Q2 starts in Nov etc.
                                 quarter == 3 ~ 2, 
                                 quarter == 4 ~ 5,
                                 quarter == 5 ~ 8),
               year = case_when(quarter %in% c(1, 2) ~ as.numeric(season)-1,
                                TRUE ~ as.numeric(season)))
    
    
    
    #now set up some columns for the lagged rate
    for (i in 1:18) {
        conversion_rate_data[, paste0("FX_Lag", i)] = 0
    }
    
    
    #now fill in those lagged rates
    for (i in 1:nrow(conversion_rate_data)) { #for each year section
        conv_year <- conversion_rate_data[i, "year"]
        conv_month <- conversion_rate_data[i, "month"]
        
        for (j in 1:18) { #for each of the 18 lags
            #get the lag date
            lag_date <- as.Date(as.yearmon(paste0(conv_year, "-", conv_month)) - 1/12*j) #minus 1/12 substracts a month from the date
            #find the corresponding FX rate for that date
            price <- nzd_usd_data_by_month %>% 
                filter(year == year(lag_date), month == month(lag_date)) %>%
                pull(mean_usd_price)
            #make sure there is only one price
            assert_that(length(price) == 1)
            
            #now just assign the price to the right entry in our dataframe
            conversion_rate_data[i, paste0("FX_Lag", j)] = as.numeric(price)
        }
    }
    return(conversion_rate_data)
}

#now just run the function we set up
conversion_rate_data <- combine_quarterly_conversion_with_nzd_usd(conversion_rate_data, nzd_usd_data_by_month)

#and we're done! Now we can move on to doing actual regressions

#***************************************************************************************************
#********************************* PREDICTING CONVERSION RATE - Q1 *********************************
#***************************************************************************************************

#**************************************************************************
#************************** LOO CROSS VALIDATION **************************
#**************************************************************************

#so here we want to build a model to predict the quaterly conversion rate from all the past spot exchange rates
#over the preceeding 18 months

predictions = list(
    'OLS' = c(),
    'LASSO' = c(),
    'GB' = c()
)

for (i in 1:nrow(conversion_rate_data)) {
    #remove one sample from the data to create our testing dataset for this iteration
    training_conversion_rate_data <- conversion_rate_data[-c(i),]
    predict_observation <- conversion_rate_data[c(i),]
    
    #Train a basic OLS model
    model_specification <- `Average Conversion Rate` ~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + 
                                                           FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 +
                                                           FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18
    model_OLS <- lm(formula = model_specification, data = training_conversion_rate_data)
    
    #print(summary(model_OLS))
    
    
    #Train a LASSO MODEL
    X <- with(data=training_conversion_rate_data, model.matrix(~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + 
                                                                     FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 +
                                                                     FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18))
    y <- training_conversion_rate_data$`Average Conversion Rate`
    
    model_LASSO <- glmnet(x=X, y=y, alpha=1)
    
    #Train Gradient Boosting Model
    model_GB <- xgboost(data = as.matrix(training_conversion_rate_data[,6:(18+5)]), 
                        label = training_conversion_rate_data$`Average Conversion Rate`, 
                        max.depth = 3, 
                        eta = 0.55, #learning rate 
                        nthread = 2, 
                        nrounds = 10,
                        objective = "reg:squarederror",
                        verbose = F)
    
    
    #now assess prediction accuracy, and add it to our list
    predictions$'OLS' = c(predictions$'OLS', predict(model_OLS, predict_observation))
    #glmnet is fussy so it needs a special line
    predictions$'LASSO' = c(predictions$'LASSO', predict(model_LASSO, 
                                                         with(data=predict_observation, model.matrix(~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 + FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18)),
                                                         s=0.0002465408)) #this is optimal lambda
    predictions$'GB' = c(predictions$'GB', predict(model_GB, as.matrix(predict_observation[,6:(18+5)])))
}

(OLS_MSE <- mean((predictions$'OLS' - conversion_rate_data$`Average Conversion Rate`) ^ 2))
(LASSO_MSE <- mean((predictions$'LASSO' - conversion_rate_data$`Average Conversion Rate`) ^ 2))
(GB_MSE <- mean((predictions$'GB' - conversion_rate_data$`Average Conversion Rate`) ^ 2))

#now calculate R^2 for these models too, as it's more intuitive for humans to interpret
(OLS_R2 <- 1 - sum((conversion_rate_data$`Average Conversion Rate` - predictions$'OLS')^2) / sum((conversion_rate_data$`Average Conversion Rate` - mean(conversion_rate_data$`Average Conversion Rate`))^2))
(LASSO_R2 <- 1 - sum((conversion_rate_data$`Average Conversion Rate` - predictions$'LASSO')^2) / sum((conversion_rate_data$`Average Conversion Rate` - mean(conversion_rate_data$`Average Conversion Rate`))^2))
(GB_R2 <- 1 - sum((conversion_rate_data$`Average Conversion Rate` - predictions$'GB')^2) / sum((conversion_rate_data$`Average Conversion Rate` - mean(conversion_rate_data$`Average Conversion Rate`))^2))


#**************************************************************************
#************************** RESULT VISUALISATION **************************
#**************************************************************************

#now compile the CV results into a nice graph
cv_results <- data.frame(MSE = c(OLS_MSE, LASSO_MSE, GB_MSE),
                         R2 = c(OLS_R2, LASSO_R2, GB_R2),
                        regression_type = c('OLS', 'LASSO', 'Gradient Boosting')) %>%
    pivot_longer(cols=c('MSE', 'R2'), names_to="statistic")

mse_plot <- cv_results %>% 
    filter(statistic == "MSE") %>%
    ggplot(aes(x=regression_type, y=value, width=0.75, fill=regression_type)) +
    geom_bar(stat='identity') + 
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size=20),
          title = element_text(size=16)) +
    scale_y_continuous(expand = c(0.0, 0)) + 
    labs(y="Mean Squared Error", 
         x=element_blank(), 
         title="LOO CV Results for Out-of-Sample MSE in \nPredicting Quarterly Conversion Rates")
#print(mse_plot)

r2_plot <- cv_results %>% 
    filter(statistic == "R2") %>%
    ggplot(aes(x=regression_type, y=value, width=0.75, fill=regression_type)) +
    geom_bar(stat='identity') + 
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size=20),
          title = element_text(size=16)) +
    scale_y_continuous(expand = c(0.0, 0)) +
    coord_cartesian(ylim=c(0.75, 1)) + #this is how we zoom in without bars dissapearing 
    labs(y="R Squared", 
         x=element_blank(), 
         title="LOO CV Results for Out-of-Sample R² in \nPredicting Quarterly Conversion Rates")
#print(r2_plot)


#generate a joint plot
png(file="model_results.png", width=(1280/720)*500, height=500)
plot_grid(r2_plot, mse_plot)
dev.off()


#what are the key differences between OLS's regression coefficients and LASSO's regression coefficients?
#lets make a nie graph to show that too

#first estimate OLS regression with all the data
model_specification <- `Average Conversion Rate` ~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + 
                                                       FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 +
                                                       FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18
model_OLS <- lm(formula = model_specification, data = conversion_rate_data)


#now estimate LASSO with all the data, and pick the best lambda
X <- with(data=training_conversion_rate_data, model.matrix(~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + 
                                                               FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 +
                                                               FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18))
y <- training_conversion_rate_data$`Average Conversion Rate`

#estimate the CV LASSO model, to find the optimal lamda
cv_LASSO <- cv.glmnet(x=X, y=y, alpha=0, intercept=FALSE)

#estimate an actual LASSO model, with the optimal lambda we found in the previous step
model_LASSO <- glmnet(x=X, y=y, alpha=0, lambda=cv_LASSO$lambda.min, intercept=FALSE)


#now set up a dataframe for visualisation
regression_coef <- data.frame(coefficient_name = fct_relevel(as.factor(rownames(coef(model_LASSO))[2:19]), 
                                                             paste0("FX_Lag", 1:18)),
                              LASSO_value = coef(model_LASSO)[2:19],
                              OLS_value = model_OLS$coefficients[1:18])
levels(regression_coef$coefficient_name) <- paste0("Lag", 1:18) #make the name smaller
rownames(regression_coef) <- NULL

#use tydr to put it into a ggplot friendly form
regression_coef <- regression_coef %>% 
    pivot_longer(cols = c('LASSO_value', 'OLS_value'), names_to = 'regression_type', values_to = 'coefficent_value')

#generate the plot
p <- regression_coef %>% ggplot(aes(x=coefficient_name, y=coefficent_value, fill=regression_type, width=0.5)) +
    geom_bar(stat='identity', position='dodge') +
    theme_classic() + 
    theme(legend.position = 'bottom',
          text = element_text(size=19)) +
    labs(x="Lagged FX Coefficient", 
         y="Coefficient Value", 
         title="Coefficient Comparison of LASSO and OLS Predictors",
         fill=element_blank())

png(file="lasso_ols_coefficient_comparison.png", width=(1280/720)*520, height=520)
print(p)
dev.off()


#**************************************************************************
#********************** YEARLY CONVERSION RATE MODEL **********************
#**************************************************************************

#now the second part of the model will be about figuring out the yearly conversion rate
#which is determined by YearlyConverisonRate = sum(wi * QuaterlyConversionRate) where wi
#is some weight. Here we are running a regression to figure out those weights.

#load in the data
yearly_conversion_rate_data <- read_xlsx('./Conversion Rate Data.xlsx', sheet = 'Yearly Conversion Rates')

model_specification <- `Yearly Conversion Rate` ~ Q1 + Q2 + Q3 + Q4 + Q5 + 0
year_conversion_rate_model <- lm(formula=model_specification, data=yearly_conversion_rate_data)

print(summary(year_conversion_rate_model))


#**************************************************************************
#*************************** PREDICTION RESULTS ***************************
#**************************************************************************

#Ok so, to project the 2021 Fonterra conversion rate, we need the average
#exchange rate for the months of April, May, June, and July 2021.
#this data obviously doesn't exist yet, so we'll use the RBNZ forecasts
#as our data instead.

#I'm going to add some RBNZ projectionss
projected_q2_change <- 0.1/100 #they estimate 0.1% change in Q2 and 0% change in Q3
march_exchange_rate <- nzd_usd_data_by_month %>% 
    filter(month == 3, year == 2021) %>% 
    pull(mean_usd_price)
quarterly_multiplier <- (1 + projected_q2_change) ^ (1/3)


RBNZ_projections_nzd_usd <- data.frame(month = c(4, 5, 6, 7),
                               year = 2021,
                               mean_usd_price = c(march_exchange_rate * quarterly_multiplier, 
                                                  march_exchange_rate * quarterly_multiplier^2, 
                                                  march_exchange_rate * quarterly_multiplier^3,
                                                  march_exchange_rate * quarterly_multiplier^3)) #RBNZ projects no change in June 2021

#make a combined dataset that includes these projections
nzd_usd_by_month_inc_projection <- rbind(nzd_usd_data_by_month, RBNZ_projections_nzd_usd) 

#reload the quarterly conversion rate data and modify it for projections to 2021
conversion_rate_data_for_projections <- read_xlsx('Conversion Rate Data.xlsx', sheet='Quaterly Conversion Rates')
#we don't actually use this data but we need to have it here so that combine_quarterly_conversion_with_nzd_usd()
#does what we want it to do
dummy_data <- data.frame(`Year Section` = c('2021 1', '2021 2', '2021 3', '2021 4', '2021 5'),
                         #we're going to predict this, so we just have some place holder data here to keep the code happy
                         `Average Conversion Rate` = 0) 
colnames(dummy_data) <- c("Year Section", "Average Conversion Rate") #names didn't work so just do them again
conversion_rate_data_for_projections <- rbind(dummy_data, conversion_rate_data_for_projections)

#now we just run this to get our data!
conversion_rate_data_for_projections <- combine_quarterly_conversion_with_nzd_usd(conversion_rate_data_for_projections, nzd_usd_by_month_inc_projection)

#ok so now we have the right data, let's just slap this into our LASSO model and predict the future conversion rates
X_pred <- with(data=conversion_rate_data_for_projections[1:5,], model.matrix(~ 0 + FX_Lag1 + FX_Lag2 + FX_Lag3 + FX_Lag4 + FX_Lag5 + FX_Lag6 + 
                                                                                   FX_Lag7 + FX_Lag8 + FX_Lag9 + FX_Lag10 + FX_Lag11 + FX_Lag12 +
                                                                                   FX_Lag13 + FX_Lag14 + FX_Lag15 + FX_Lag16 + FX_Lag17 + FX_Lag18))
#predict using the best lambda from before 
predicted_quaterly_conversion_rates <- predict(model_LASSO, newx=X_pred, s=cv_LASSO$lambda.min)


#nearly done! now we just put this into the quarter -> yearly conversion rate model and we're done
conversion_rate_data_for_projections_year_form <- data.frame(Q1 = predicted_quaterly_conversion_rates[1],
                                                             Q2 = predicted_quaterly_conversion_rates[2],
                                                             Q3 = predicted_quaterly_conversion_rates[3],
                                                             Q4 = predicted_quaterly_conversion_rates[4],
                                                             Q5 = predicted_quaterly_conversion_rates[5])

conversion_rate_2021 <- predict(year_conversion_rate_model, conversion_rate_data_for_projections_year_form)
                                                             
print(paste0("We predict the 2021 Annual Conversion Rate will be ", round(conversion_rate_2021, 4)))                                                     


#now we just have some extra stuff here, that's just adjacent to the prediction

#we want to show readers that TWI and USD are correlated, so let's load in some data and 
#generate a graph
twi_usd_data <- read_xlsx('TWI-monthly.xlsx', sheet="Data", skip = 4) %>%
    transmute(date = `Series Id`,
              TWI = EXRT.MS41.NZB17,
              NZDUSD_price = EXR.MS11.D06)

twi_price_chart <- xyplot(TWI ~ date, twi_usd_data, 
                           type = "l" , 
                           lwd=2, 
                           ylab="TWI")
usd_price_chart <- xyplot(NZDUSD_price ~ date, twi_usd_data, 
                           type = "l" , 
                           lwd=2, 
                           ylab="NZD/USD Exchange Rate")

#combine two graphs to get 2 axis plot
png(file="twi_usd_nzd.png", width=(1280/720)*400, height=400)
doubleYScale(twi_price_chart, usd_price_chart, add.ylab2 = TRUE)
dev.off()





#****************************************************************************************************
#****************************** CORRELATION BETWEEN GDT & NZD/USD - Q2 ******************************
#****************************************************************************************************

#So turns out we shouldn't be using the GTD average price as it's not truely indicative of dairy prices
#See: https://www.globaldairytrade.info/en/gdt-events/how-gdt-events-work/reference-material/

#I downloaded the GTD Index which is on the Global Dairy Trade website. They store it as a JSON file
#but with some fromJSON magic we can fix that...

gdt_price_index_data <- fromJSON(txt='price_indices_ten_years.json')$PriceIndicesTenYears$Events$EventDetails %>%
    transmute(EventDate = as.Date(EventDate, format="%B %d, %Y 12:00:00"),
              PriceIndex = as.numeric(PriceIndex)) %>%
    #now we want to join to NZD/USD data
    left_join(raw_nzd_usd_data, by=c('EventDate' = 'date'))
    
#Note that we only have measurements for days that a GDT auction happened
#I'm pretty sure this is desirable, as just holding the GDT price as fixed is going to mess up our
#correlation stuff, since the underlying process is still going, there just wasn't an auction on
#so it wasn't measured that day

#first draw a graph for the report
#ggplot is highly opposed to dual axis graphs, so we need to use latticeExtra...
gdt_price_chart <- xyplot(PriceIndex ~ EventDate, gdt_price_index_data, 
                          type = "l" , 
                          lwd=2, 
                          ylab="GDT Price Index")
nzd_price_chart <- xyplot(price_usd ~ EventDate, gdt_price_index_data, 
                          type = "l" , 
                          lwd=2, 
                          ylab="NZD/USD Exchange Rate")

#combine two graphs to get 2 axis plot
png(file="GTD_NZD_USD_chart.png", width=(1280/720)*400, height=400)
doubleYScale(gdt_price_chart, nzd_price_chart, text=c("GDT Price Index", "NZD/USD Exchange Rate"), add.ylab2 = TRUE)
dev.off()

#So first ler's try using all the data
cat("\n\n***********************************************************************\n")
cat("************************** USING ALL EX DATA **************************\n")
cat("***********************************************************************\n")

#first just show what is stationary and what is not
print("NZD/USD is non-stationary.")
adf.test(gdt_price_index_data$price_usd)

print("GDT Price Index is non-stationary.")
adf.test(gdt_price_index_data$PriceIndex)

print("NZD/USD first difference is stationary.")
adf.test(diff(gdt_price_index_data$price_usd))

print("GDT Price Index first difference is stationary.")
adf.test(diff(gdt_price_index_data$PriceIndex))

#no evidence of cointegration
coint.test(gdt_price_index_data$PriceIndex, gdt_price_index_data$price_usd)


#Calculate raw correlation between price and exchange rate
pearson_corr <- cor(gdt_price_index_data$PriceIndex, gdt_price_index_data$price_usd)
cat("The Pearson correlation coefficient between the GDT Price Index and the NZD/USD Exchange rate is: ", round(pearson_corr, 3), "\n")

pearson_corr_df <- cor(diff(gdt_price_index_data$PriceIndex), diff(gdt_price_index_data$price_usd))
cat("The Pearson correlation coefficient between the first differences of the GDT Price Index and the NZD/USD Exchange rate is: ", round(pearson_corr_df, 3), "\n")

dcca_corr <- rhodcca(gdt_price_index_data$PriceIndex, gdt_price_index_data$price_usd)
cat("The DCCA correlation between the GDT Price Index and the NZD/USD Exchange rate is: ", round(dcca_corr$rhodcca, 3), "\n")


#Now let's try just using Q2-Q3 data
cat("\n\n***********************************************************************\n")
cat("************************* USING Q2-Q3 EX DATA *************************\n")
cat("***********************************************************************\n")

gdt_price_index_data_q2q3 <- gdt_price_index_data %>%
    filter(month(EventDate) %in% c(11, 12, 1, 2, 3, 4)) #filter to only include months in Q2-Q3

#still no evidence of cointegration, although it's closer
coint.test(gdt_price_index_data_q2q3$PriceIndex, gdt_price_index_data_q2q3$price_usd)

pearson_corr <- cor(gdt_price_index_data_q2q3$PriceIndex, gdt_price_index_data_q2q3$price_usd)
cat("The Pearson correlation coefficient between the GDT Price Index and the NZD/USD Exchange rate is: ", round(pearson_corr, 3), "\n")

pearson_corr_df <- cor(diff(gdt_price_index_data_q2q3$PriceIndex), diff(gdt_price_index_data_q2q3$price_usd))
cat("The Pearson correlation coefficient between the first differences of the GDT Price Index and the NZD/USD Exchange rate is: ", round(pearson_corr_df, 3), "\n")

dcca_corr <- rhodcca(gdt_price_index_data_q2q3$PriceIndex, gdt_price_index_data_q2q3$price_usd)
cat("The DCCA correlation between the GDT Price Index and the NZD/USD Exchange rate is: ", round(dcca_corr$rhodcca, 3), "\n")

#we need to construct boostrap confidence intervals so that we can accept or reject the null

print("Calculating confidence intercals. This might take 3 or 4 minutes.")

get_dcca_corr <- function(data, indices) {
    d <- data[indices,] #allow boot to select the samples it wants
    dcca_corr <- rhodcca(d$PriceIndex, d$price_usd)
    return(dcca_corr$rhodcca)
} 

#bootstrap with 10000 replications
results <- boot(data=gdt_price_index_data_q2q3, statistic=get_dcca_corr, R=500000)

#get bias-corrected, accelerated confidence intervals at the 95% level
print(boot.ci(results, conf=0.95, type="bca"))


