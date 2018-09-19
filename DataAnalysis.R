# Laod Data in to a data frame
data<- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv")

# summarize the data frame
summary.data.frame(data)

#Combine redundant levels 
summary(data$MultipleLines)
levels(data$MultipleLines) <- c("No", "No", "Yes")
summary(data$MultipleLines)

summary(data$OnlineSecurity)
levels(data$OnlineSecurity) <- c("No", "No", "Yes")
summary(data$OnlineSecurity)

summary(data$OnlineBackup)
levels(data$OnlineBackup) <- c("No", "No", "Yes")
summary(data$OnlineBackup)

summary(data$DeviceProtection)
levels(data$DeviceProtection) <- c("No", "No", "Yes")
summary(data$DeviceProtection)

summary(data$TechSupport)
levels(data$TechSupport) <- c("No", "No", "Yes")
summary(data$TechSupport)

summary(data$StreamingTV)
levels(data$StreamingTV) <- c("No", "No", "Yes")
summary(data$StreamingTV)

summary(data$StreamingMovies)
levels(data$StreamingMovies) <- c("No", "No", "Yes")
summary(data$StreamingMovies)

# summarize the data frame after readjusting the redundant levels
summary.data.frame(data)

# find missing data - NAs
nrow(data[!complete.cases(data),])
# %age of NAs
(nrow(data[!complete.cases(data),])/nrow(data))*100

# find all obs with tenure =0 
nrow(data[data$tenure==0,])
# discretize tenure as "Tenure"
data$Tenure<-cut(data$tenure,c(0,6,20,40,60,Inf),labels = c("0-6","7-20","21-40","41-60","61+"), include.lowest = TRUE)
summary(data$Tenure)
# drop the continuous variable "tenure"
data$tenure<-NULL

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


