data<- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv")
nrow(data[complete.cases(data),])
data_clean<-na.omit(data)
nrow(data_clean[data_clean$PhoneService=="No" && data_clean$Churn=="Yes",])
phone_data<-data[data$PhoneService=='Yes',]




mosaicplot(Churn~Contract,data=data)

mosaicplot(Churn~Contract+gender,data=data)
mosaicplot(Churn~Contract+SeniorCitizen,data=data)
mosaicplot(Churn~Contract+PhoneService,data=data)
mosaicplot(Churn~Contract+MultipleLines,data=data)
mosaicplot(Churn~Contract+PaperlessBilling,data=data)
mosaicplot(Churn~Contract+PaperlessBilling+PaymentMethod,data=data)
mosaicplot(Churn~Contract+PaymentMethod,data=data)
mosaicplot(Churn~gender+PaymentMethod,data=data)
mosaicplot(Churn~Dependents+PaymentMethod,data=data)
mosaicplot(Churn~Partner+Dependents+PaymentMethod,data=data)
mosaicplot(Churn~Dependents+PaymentMethod,data=data)
data$tenure_d <- cut(data$tenure,seq(0,120,6))
mosaicplot(Churn~Contract+PaymentMethod+tenure_d,data=data)
data$tenure_d <- cut(data$tenure,seq(0,120,24))
mosaicplot(Churn~Contract+PaymentMethod+tenure_d,data=data)
data$tenure_d <- cut(data$tenure,seq(0,90,24))
mosaicplot(Churn~Contract+PaymentMethod+tenure_d,data=data)
data$tenure_d <- cut(data$tenure,c(0,6,12,18,24,30,36,42,48,54,60,72,84,96,200))
mosaicplot(Churn~Contract+PaymentMethod+tenure_d,data=data)
data$tenure_d <- cut(data$tenure,c(0,6,12,24,36,48,60,84,200))
mosaicplot(Churn~Contract+PaymentMethod+tenure_d,data=data)

# there are'nt anyone who has moved on that did not have a phone service.
nrow(data[data$PhoneService=="No" && data$Churn=="Yes",])
# we can eliminate the data for the folks who do not have phone service, since we are interested only in the folks who do, and its only the folks who do that affect the churn number.

phone_data<-data[data$PhoneService=='Yes',]

summary(phone_data$TotalCharges)
phone_data$TotalCharges_q <- cut(phone_data$TotalCharges,c(0,415,1422,4026,8685))
summary(phone_data$tenure)
phone_data$tenure_d <- cut(phone_data$tenure,c(0,9,29,56,72))
summary(phone_data$MonthlyCharges)
phone_data$MonthlyCharges_q <- cut(phone_data$MonthlyCharges,c(0,45,75,92,120))

mosaicplot(Churn~Contract+tenure_d+MonthlyCharges_q,data=phone_data)
mosaicplot(Churn~Contract+tenure_d+TotalCharges_q,data=phone_data)

phone_data_clean<-phone_data[,c("customerID","gender","SeniorCitizen_f","Partner","Dependents","tenure_d","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","TotalCharges_q","MonthlyCharges_q","Churn")]

#X<-as.matrix(phone_data)

#install and load package arules
install.packages("Matrix")
install.packages("arules")
library(Matrix)
library(arules)
#install and load arulesViz
install.packages("arulesViz")
install.packages("robustbase")
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
#install and load knitr
install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
install.packages("lubridate")
library(lubridate)
#install and load plyr
install.packages("plyr")
library(plyr)
library(dplyr)


