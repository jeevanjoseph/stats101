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
data$Tenure<-cut(data$tenure,c(0,2,6,11,18,24,60,Inf),labels = c("< 3","3-6","7-11","12-18","19-24","25-60","60+"), include.lowest = TRUE)
#data$Tenure<-cut(data$tenure,c(0,6,20,40,61,Inf),labels = c("< 6","6-20","20-40","40-61","61+"), include.lowest = TRUE)
summary(data$Tenure)
# drop the continuous variable "tenure"
#data$tenure<-NULL

# Find the bin sizes to discretize monthly charges by 
# dividing the distribution in to quintiles
quantile(data$MonthlyCharges,probs = seq(0,1,.2))
# discretize tenure as "MonthlySpend"
#data$MonthlySpend<-cut(data$MonthlyCharges,c(18,25,65,80,95,Inf),labels = c("18-25","26-65","65-80","80-95","95+"), include.lowest = TRUE)
data$MonthlySpend<-cut(data$MonthlyCharges,c(15,30,45,70,80,95,Inf),labels = c("15-30","30-45","45-70","70-80","80-95","95+"), include.lowest = TRUE)
summary(data$MonthlySpend)
# drop the continuous variable "MonthlyCharges"
#data$MonthlyCharges<-NULL


# Find the bin sizes to discretize monthly charges by 
# dividing the distribution in to quintiles
quantile(data$TotalCharges,probs = seq(0,1,.2))
# discretize tenure as "TotalSpend"
#data$TotalSpend<-cut(data$TotalCharges,c(18,275,972,2158,4748,Inf),labels = c("18-275","276-972","973-2158","2159-4748","4748+"), include.lowest = TRUE)
data$TotalSpend<-cut(data$TotalCharges,c(0,1000,2000,4000,6000,Inf),labels = c("<1000","1000-2000","2000-4000","4000-6000","6000+"), include.lowest = TRUE)
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
univariate_stats(data$gender, "Gender")
summary(data$gender)

# Plot the SeniorCitizen
univariate_stats(data$SeniorCitizen, "Senior Customer")
summary(data$SeniorCitizen)

# Plot the Partner
univariate_stats(data$Partner, "Partner")
summary(data$Partner)

# Plot the Dependents
univariate_stats(data$Dependents, "Dependents")
summary(data$Dependents)

# Plot the MultipleLines
univariate_stats(data$MultipleLines, "Has Multiple Lines")
summary(data$MultipleLines)

# Plot the InternetService
univariate_stats(data$InternetService, "Has Internet Service")
summary(data$InternetService)


# Plot the Contract type
univariate_stats(data$Contract, "Contract Type")
summary(data$Contract)

# Plot the PaperlessBilling
univariate_stats(data$PaperlessBilling, "PaperlessBilling")
summary(data$PaperlessBilling)

# Plot the Payment Method
univariate_stats(data$PaymentMethod, "Payment Method")
summary(data$PaymentMethod)

# Plot the Churn
univariate_stats(data$Churn, "Discontinued Service")
summary(data$Churn)

# Plot the Tenure
univariate_stats(data$Tenure, "Tenure")
summary(data$Tenure)

# Plot the Monthly spend
univariate_stats(data$MonthlySpend, "Monthly Spend")
plot(data$MonthlyCharges,data$tenure, col = c(124,102,459,203,541,368,134) )


summary(data$MonthlySpend)

# Plot the TotalSpend
univariate_stats(data$TotalSpend, "Total Spend")
plot(data$TotalCharges,data$tenure, col = c(124,102,459,203,541,368,134) )
summary(data$TotalSpend)


# Plot the Device protection
univariate_stats(data$DeviceProtection, "Device Protection Purchased")
summary(data$DeviceProtection)

# Plot the Tenure (Continuous)
univariate_stats(data$tenure, "Tenure")
summary(data$tenure)

# Plot the MonthlyCharges (Continuous)
univariate_stats(data[(data$MonthlyCharges<30 & data$MonthlyCharges>15),]$MonthlyCharges, "15 < Monthly Charges < 30 ")
summary(data[(data$MonthlyCharges<30 & data$MonthlyCharges>15),]$MonthlyCharges)

# plot monthly for high rate subscribers
univariate_stats(data[(data$MonthlyCharges>40),]$MonthlyCharges, "Monthly Charges >40 ")
summary(data[(data$MonthlyCharges>40),]$MonthlyCharges)

# Plot the TotalCharges (Continuous)
univariate_stats(data$TotalCharges, "Total Charges")
summary(data$TotalCharges)




# Bivariate Statistics
# use Chi-Squared test to analyze the dependence.


# Chi-Squared test of Indenpendence - Gender x Churn and plot
chisq.test(table(data$gender,data$Churn))
barplot(table(data$gender,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$gender,data$Churn)), main = "Gender x Churn")

# Chi-Squared test of Indenpendence - SeniorCitizen x Churn and plot
chisq.test(table(data$SeniorCitizen,data$Churn))
barplot(table(data$SeniorCitizen,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$SeniorCitizen,data$Churn)), main="SeniorCitizen x Churn")


# Chi-Squared test of Indenpendence - Partner x Churn and plot
chisq.test(table(data$Partner,data$Churn))
barplot(table(data$Partner,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$Partner,data$Churn)), main="Partner x Churn")


# Chi-Squared test of Indenpendence - Dependents x Churn and plot
chisq.test(table(data$Dependents,data$Churn))
barplot(table(data$Dependents,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$Dependents,data$Churn)), main="Dependents x Churn")


# Chi-Squared test of Indenpendence - Tenure x Churn and plot
chisq.test(table(data$Tenure,data$Churn))
barplot(table(data$Tenure,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$Tenure,data$Churn)), main="Tenure x Churn")


# Chi-Squared test of Indenpendence - MultipleLines x Churn and plot
chisq.test(table(data$MultipleLines,data$Churn))
barplot(table(data$MultipleLines,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$MultipleLines,data$Churn)), main="MultipleLines x Churn")


# Chi-Squared test of Indenpendence - InternetService x Churn and plot
chisq.test(table(data$InternetService,data$Churn))
barplot(table(data$InternetService,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$InternetService,data$Churn)), main="InternetService x Churn")


# Chi-Squared test of Indenpendence - OnlineSecurity x Churn and plot
chisq.test(table(data$OnlineSecurity,data$Churn),correct = FALSE)
barplot(table(data$OnlineSecurity,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$OnlineSecurity,data$Churn)), main="OnlineSecurity x Churn")


# Chi-Squared test of Indenpendence - OnlineBackup x Churn and plot
chisq.test(table(data$OnlineBackup,data$Churn))
barplot(table(data$OnlineBackup,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$OnlineBackup,data$Churn)), main="OnlineBackup x Churn")

# Chi-Squared test of Indenpendence - DeviceProtection x Churn and plot
chisq.test(table(data$DeviceProtection,data$Churn))
barplot(table(data$DeviceProtection,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$DeviceProtection,data$Churn)), main="DeviceProtection x Churn")

# Chi-Squared test of Indenpendence - TechSupport x Churn and plot
chisq.test(table(data$TechSupport,data$Churn))
barplot(table(data$TechSupport,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$TechSupport,data$Churn)), main="TechSupport x Churn")

# Chi-Squared test of Indenpendence - StreamingTV x Churn and plot
chisq.test(table(data$StreamingTV,data$Churn))
barplot(table(data$StreamingTV,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$StreamingTV,data$Churn)), main="StreamingTV x Churn")

# Chi-Squared test of Indenpendence - StreamingMovies x Churn and plot
chisq.test(table(data$StreamingMovies,data$Churn))
barplot(table(data$StreamingMovies,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$StreamingMovies,data$Churn)), main="StreamingMovies x Churn")

# Chi-Squared test of Indenpendence - Contract x Churn and plot
chisq.test(table(data$Contract,data$Churn))
barplot(table(data$Contract,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$Contract,data$Churn)), main="Contract x Churn")

# Chi-Squared test if Indenpendence - PaperlessBilling x Churn and plot
chisq.test(table(data$PaperlessBilling,data$Churn))
barplot(table(data$PaperlessBilling,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$PaperlessBilling,data$Churn)), main="PaperlessBilling x Churn")

# Chi-Squared test of Indenpendence - PaymentMethod x Churn and plot
chisq.test(table(data$PaymentMethod,data$Churn))
barplot(table(data$PaymentMethod,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$PaymentMethod,data$Churn)), main="OnlineSecurity x Churn")

# Chi-Squared test of Indenpendence - MonthlySpend x Churn and plot
chisq.test(table(data$MonthlySpend,data$Churn))
barplot(table(data$MonthlySpend,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$MonthlySpend,data$Churn)), main="MonthlySpend x Churn")
plot(data$MonthlyCharges,data$tenure)


# Chi-Squared test of Indenpendence - TotalSpend x Churn and plot
chisq.test(table(data$TotalSpend,data$Churn))
barplot(table(data$TotalSpend,data$Churn), col = c(124,102,459,203,541,368,134,505,189),	legend = rownames(table(data$TotalSpend,data$Churn)), main="TotalSpend x Churn")
plot(data$TotalCharges,data$tenure)





# Mosaic plots to show the interaction of multiple variables.
mosaicplot(Churn~Contract,data=data,col = c(124,102,459,203,541,368,134,505,189), main = "Churn By Contract")


mosaicplot(Churn~gender,data=data,col = c(124,102,459,203,541,368,134,505,189), main = "Churn By Gender")
mosaicplot(Churn~Tenure+MonthlySpend, data=data, col = c(124,102,459,203,541,368,134,505,189), main = "Churn By Tenure x Monthly Spend")
mosaicplot(Churn~Contract+MonthlySpend, data=data, col = c(124,102,459,203,541,368,134,505,189), main = "Churn By Tenure x Monthly Spend")
mosaicplot(Churn~PaymentMethod+MonthlySpend+Tenure, data=data, col = c(124,102,459,203,541,368,134,505,189), main = "Churn By Payment Method x Monthly Spend x Tenure")


### Logistic regtression - preparing datasets
install.packages("caTools")
install.packages("ROSE")
install.packages('rpart')
library("caTools")
library("ROSE")
library("rpart")
#set.seed(5) ## 84%
#set.seed(30), 49


set.seed(4)
data$Attrition<- ifelse(data$Churn=="Yes",1,0)
sample<-sample.split(data$Attrition,SplitRatio=.90)
trainingSet <- data[sample,]
testSet <- data[!sample,]


# Logistic Regression Model
model<-glm(Attrition~gender+SeniorCitizen+Partner+Dependents+MultipleLines+
             InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+
             TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+
             PaymentMethod+Tenure+MonthlyCharges+
             TotalCharges , family = binomial(link = "logit"),data = trainingSet)
summary(model)



model_refined<-glm(Attrition~MultipleLines+SeniorCitizen+InternetService+
                     OnlineSecurity+TechSupport+ Contract+StreamingTV+
                     StreamingMovies+PaperlessBilling+PaymentMethod+TotalCharges+
                     Tenure , family = binomial(link = "logit"),data = trainingSet)
summary(model_refined)

## Decision tree model
treemodel <- rpart(Attrition~MultipleLines+SeniorCitizen+InternetService+
                     OnlineSecurity+TechSupport+ Contract+StreamingTV+
                     StreamingMovies+PaperlessBilling+PaymentMethod+TotalCharges+
                     Tenure, data=trainingSet)
summary(treemodel)

## Predict values with tree.
tree_prediction <- predict(treemodel,testSet)
roc.curve(testSet$Attrition,tree_prediction)
predictions_tree<-ifelse(tree_prediction>0.5,"Yes","No")
## Convert to a factor.
predictions_tree<-factor(predictions_tree,labels = c("No","Yes"))

summary(predictions_tree)

## Predict values with logistic regression. threshold = 0.5.
predictions_logistic <- predict(model_refined,testSet,type="response")
roc.curve(testSet$Attrition,predictions_logistic)

predictions_refined<-ifelse(predictions_logistic>0.5,"Yes","No")
## Convert to a factor.
predictions_refined<-factor(predictions_refined,labels = c("No","Yes"))

summary(predictions_refined)

comparison<-data.frame(predictions_tree,predictions_refined,testSet$Churn)

summary(comparison)





