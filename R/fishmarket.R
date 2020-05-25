##########################################################################
#name:fishmarket.R
#author: Sarah Bainn
#description: Analysis on fishmarket dataset
#date last updated:05/25/2020
##########################################################################


library(readstata13)
library(tidyverse)
library(stargazer)
library(sjPlot)
library(lmtest)
library(ivpack)
fish_data <- read.dta13("/cloud/project/Data/fish.dta")
fish_data$lnquantity = log(fish_data$quantity)
fish_data$lnprice = log(fish_data$price)
attach(fish_data)
fish_data$day <- cbind(mon, tues, wed, thurs)
reg <- lm(lnquantity~lnprice+day, data = fish_data)
reg$coefficients[2]
reg.wave = ivreg(lnquantity ~ lnprice + day | day + wave2, data = fish_data)
reg.speed = ivreg(lnquantity ~ lnprice + day | day + speed3, data = fish_data)
tab_model(reg,reg.wave,reg.speed, dv.labels = c("OLS", "Wave IV", "Wind IV"), file = "text", title = "Regression output table of lnQuantity", show.r2 = TRUE, show.fstat = TRUE, show.p = TRUE, show.se = TRUE, collapse.se = TRUE, show.ci = FALSE)
tsls_1 <- lm(lnprice~mon+tues+wed+thurs+wave2 , data = fish_data)
instrFtest <- waldtest(tsls_1,.~.-wave2 )
instrFtest
tsls_1 <- lm(lnprice~wave2 , data = fish_data)
instrFtest <- waldtest(tsls_1,.~.-wave2 )
instrFtest
tsls1 <- lm(lnprice~speed3 , data = fish_data)
instrFtest <- waldtest(tsls1,.~.-speed3)
instrFtest
tab_model(tsls_1, tsls1, collapse.se = "TRUE", collapse.ci = "TRUE", file = "test.html")
