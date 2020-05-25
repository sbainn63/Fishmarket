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
fish_data <- read.dta13("/cloud/project/Data/fish.dta")
fish_data$lnquantity = log(fish_data$quantity)
fish_data$lnprice = log(fish_data$price)
reg <- lm(lnquantity~lnprice+mon+tues+wed+thurs, data = fish_data)
reg$coefficients[2]
tsls_1 <- lm(lnprice~mon+tues+wed+thurs+wave2 , data = fish_data)
p.hat <- fitted.values(tsls_1)
tsls_2 <- lm(lnquantity~p.hat+mon+tues+wed+thurs, data = fish_data)
tsls1 <- lm(lnprice~mon+tues+wed+thurs+speed3 , data = fish_data)
p.hat1 <- fitted.values(tsls1)
tsls2 <- lm(lnquantity~p.hat1+mon+tues+wed+thurs, data = fish_data)
tab_model(reg,tsls_2, tsls2, collapse.se = "TRUE", show.fstat = "TRUE", collapse.ci = "TRUE", file ="text")
tsls_1 <- lm(lnprice~mon+tues+wed+thurs+wave2 , data = fish_data)
instrFtest <- waldtest(tsls_1,.~.-wave2 )
instrFtest
tsls1 <- lm(lnprice~mon+tues+wed+thurs+speed3 , data = fish_data)
instrFtest <- waldtest(tsls1,.~.-speed3)
instrFtest
tab_model(tsls_1, tsls1, collapse.se = "TRUE", collapse.ci = "TRUE", file = "test.html")
