# Laod Data in to a data frame
data<- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv")

# summarize the data frame
summary.data.frame(data)

#Count the number of customers who are not phone service users
nrow(data[data$PhoneService=="No",])
# Check if customers who were not phone service users contributed to churn
nrow(data[data$PhoneService=="No" && data$Churn=="Yes",])
# Narrow the dataset to only phone service users since we are analyzing 
# landline subscriber churn
data<-data[data$PhoneService=='Yes',]

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


### Find Missing or Incomplete Data

# find missing data - NAs
nrow(data[!complete.cases(data),])
# %age of NAs
(nrow(data[!complete.cases(data),])/nrow(data))*100
# We do not want to eliminate these observations just yet, without 
# checking how discriminating the "total charges" varibale is 
# thats the only variable where the NAs are present.
#data_clean<-na.omit(data)
impute<-data[complete.cases(data),c(6,19,20)]
impute$calculatedTotal<-impute$tenure*impute$MonthlyCharges
impute$error<- sqrt( ((impute$calculatedTotal-impute$TotalCharges)/impute$TotalCharges) * ((impute$calculatedTotal-impute$TotalCharges)/impute$TotalCharges) )
plot(impute$error)
summary(impute$error)
quantile(impute$error,probs=seq(0,1,.01))
plot(quantile(impute$error,probs=seq(0,1,.01)))


## Impute value for Total Charges, based on Monthly.

# Summary before imputing
summary(data$TotalCharges)
# impute values
data$TotalCharges[!complete.cases(data)]<-data$MonthlyCharges[!complete.cases(data)]
# summary after imputing.
summary(data$TotalCharges)


### Discretize continuous variables

# Find the bin sizes to discretize tenure by 
# dividing the distribution in to quintiles
quantile(data$tenure,probs = seq(0,1,.2))
# find all obs with tenure =0 
nrow(data[data$tenure==0,])
# discretize tenure as "Tenure"
data$Tenure<-cut(data$tenure,c(0,3,6,12,18,24,60,Inf),labels = c("0-3","4-6","7-12","13-18","19-24","25-60","60+"), include.lowest = TRUE)

summary(data$Tenure)
# drop the continuous variable "tenure"
#data$tenure<-NULL

# Find the bin sizes to discretize monthly charges by 
# dividing the distribution in to quintiles
quantile(data$MonthlyCharges,probs = seq(0,1,.2))
# discretize tenure as "MonthlySpend"
data$MonthlySpend<-cut(data$MonthlyCharges,c(18.25,24.5,66.40,80.70,95.15,Inf),labels = c("18-25","26-65","66-81","82-96","96+"), include.lowest = TRUE)
summary(data$MonthlySpend)
# drop the continuous variable "MonthlyCharges"
#data$MonthlyCharges<-NULL


# Find the bin sizes to discretize monthly charges by 
# dividing the distribution in to quintiles
quantile(data$TotalCharges,probs = seq(0,1,.2), na.rm = TRUE)
# discretize tenure as "TotalSpend"
data$TotalSpend<-cut(data$TotalCharges,c(18,275,972,2158,4748,Inf),labels = c("18-275","276-972","973-2158","2159-4748","4748+"), include.lowest = TRUE)
summary(data$TotalSpend)
# drop the continuous variable "TotalCharges"
#data$TotalCharges<-NULL






# Fix senior Citizens to be a factor
data$SeniorCitizen[data$SeniorCitizen==0]<-"No"
data$SeniorCitizen[data$SeniorCitizen==1]<-"Yes"
data$SeniorCitizen<-factor(data$SeniorCitizen)
summary(data$SeniorCitizen)

### Univariate statistics for the independent variables.

summary(data)

# define a function to plot a given variable, 
# as we will be doing this repeatedly.
univariate_stats <- function(dataDist, title){
  freq<-as.numeric(summary(dataDist))
  ylim = c(0,1.1*max(freq))
  xx <- plot(dataDist,ylim = ylim, main = title, col = c(124,102,459,203,541,368,134) )
  text(x=xx, y=freq, label=freq, pos=3,cex=0.8,col="red")
}




# Plot the Gender
univariate_stats(data$Contract, "Contract")

# Plot the SeniorCitizen
univariate_stats(data$SeniorCitizen, "Senior Customer")

# Plot the Partner
univariate_stats(data$Partner, "Partner")

# Plot the Dependents
univariate_stats(data$Dependents, "Dependents")

# Plot the MultipleLines
univariate_stats(data$MultipleLines, "Has Multiple Lines")

# Plot the InternetService
univariate_stats(data$InternetService, "Has Internet Service")


# Plot the Contract type
univariate_stats(data$Contract, "Contract Type")

# Plot the PaperlessBilling
univariate_stats(data$PaperlessBilling, "PaperlessBilling")

# Plot the Payment Method
univariate_stats(data$PaymentMethod, "Payment Method")

# Plot the Churn
univariate_stats(data$Churn, "Discontinued Service")

# Plot the Tenure
univariate_stats(data$Tenure, "Tenure")

# Plot the Monthly spend
univariate_stats(data$MonthlySpend, "Monthly Spend")

# Plot the TotalSpend
univariate_stats(data$TotalSpend, "Total Spend")


# Plot the Device protection
univariate_stats(data$DeviceProtection, "Device Protection Purchased")

# Plot the Tenure (Continuous)
univariate_stats(data$tenure, "Tenure")

# Plot the MonthlyCharges (Continuous)
univariate_stats(data$MonthlyCharges, "Monthly Charges")

# Plot the TotalCharges (Continuous)
univariate_stats(data$TotalCharges, "Total Charges")




# Bivariate Statistics
# PLot Tenure by Contract Type
barplot(table(data$Tenure,data$Contract), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$Tenure,data$Contract)))

barplot(table(compare$prediction,compare$Churn), col = c(124,102,459,203,541,368,134,505,189),
        legend = rownames(table(compare$prediction,compare$Churn)))


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
mosaicplot(Churn~Tenure,data=data)
mosaicplot(Churn~Tenure+MonthlySpend,data=data)


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

sample<-sample.split(data$Churn,splitRatio=.80)


### Logistic regtression
data$Attrition<- ifelse(data$Churn=="Yes",1,0)
sample<-sample.split(data$Attrition,SplitRatio=.80)
trainingSet <- data[sample,]
testSet <- data[!sample,]
model<-glm(data$Attrition~data$gender+data$SeniorCitizen+data$Partner+data$Dependents+data$MultipleLines+data$InternetService+data$OnlineSecurity+data$OnlineBackup+data$DeviceProtection+data$TechSupport+data$StreamingTV+data$StreamingMovies+data$Contract+data$PaperlessBilling+data$PaymentMethod+data$Tenure+data$MonthlySpend+data$TotalSpend , family = binomial(link = "logit"),data = trainingSet)
summary(model)
model_refined<-glm(Attrition~SeniorCitizen+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+TechSupport+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+Tenure , family = binomial(link = "logit"),data = trainingSet)
summary(model_refined)

## Predict values with logistic regression. threshold = 0.5.
predictions<-ifelse(predict(model_refined,testSet,type="response")>0.5,"Yes","No")
## Convert to a factor.
predictions<-factor(predictions,labels = c("No","Yes"))
summary(predictions)

comparison<-data.frame(predictions,testSet$Churn)
summary(comparison)



