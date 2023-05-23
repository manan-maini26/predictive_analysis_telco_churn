customer_churn<-read.csv("C:/Users/HProBook-172/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv")

summary(customer_churn)

unique(customer_churn$Partner)

unique(customer_churn$PaymentMethod)

mean(customer_churn$MonthlyCharges) 

median(customer_churn$MonthlyCharges)

range(customer_churn$MonthlyCharges)

# extracting the null values so over data is clean

sum(is.na(customer_churn))

customer_churn<-na.omit(customer_churn)

#extracting internet service collum

customer_churn$InternetService->customer_internet_service

#extracting paperless billing collum

customer_paperless_billing<-customer_churn$PaperlessBilling

#extracting streaming tv colloum

customer_streaming_tv<-customer_churn$StreamingTV

#extracting 3rd , 6th , 9th colloum

customer_random_colloum<-customer_churn[,c(3,6,9)]

#getting the count of the customer whose internetservice is DSL
count=0
for(i in 1:nrow(customer_churn)){
  if(customer_churn$InternetService[i]=="DSL"){
    count=count+1
  }
}
count

#getting the customer who,s tenure is about 2 months

count=0
i=1
while (i<=nrow(customer_churn)){
  if(customer_churn$tenure[i]==2){
    Count=count+1
  }
  i+i+1
}
count

#customer whose internet service is"DSL"

library("dplyr")

customer_churn %>% filter(InternetService=="DSL") -> customer_dsl
View(customer_churn)
 
#customer who,s contract is month to month

customer_churn %>% filter(Contract=="month to month") -> customer_month
View(customer_month)

#male citizens who's payment method is electronic check

customer_churn %>% filter(gender=="male" & SeniorCitizen==1 & PaymentMethod=="electronic check")->senior_male_electronic
View(senior_male_electronic)

#all the customer who,s tenure is greater than 70 months or thier charges greater than 8000

customer_churn %>% filter(tenure>70 | TotalCharges>8000)-> customer_total_tenure
View(customer_total_tenure)

#count of different levels from the churn coloum

customer_churn %>% count(churn)

#data visualiztion
#bar plot for the 'phone service' colloum

library(ggplot2)

ggplot(data=customer_churn,aes(x="phoneService"))+ geom_bar()

#filling it with the pink colour 

ggplot(data=customer_churn,aes(x="phoneService"))+ geom_bar(fill="pink",col="peru")

#bar plot for internet service colloum

ggplot(data=customer_churn,aes(x="internetservice"))+ geom_bar()
ggplot(data=customer_churn,aes(x="internetservice",fill=internetservice))+ geom_bar()

#scatter plot between 'totalcharges' & 'tenure'

ggplot(data=customer_churn,aes(y=totalcharges,x=tenure))+geom_point()
ggplot(data=customer_churn,aes(y=totalcharges,x=tenure))+geom_point(col="wheat1")

#mapping 'paymentmethod' to col aesthetic 

ggplot(data=customer_churn,aes(y=totalcharges,x=tenure,col=PaymentMethod))+geom_point()

#mappnig gender to aesthetic

ggplot(data=customer_churn,aes(y=totalcharges,x=tenure,col=Gender))+geom_point
ggplot(data=customer_churn,aes(y=totalcharges,x=tenure,col=SeniourCitizen))+geom_point()

#boxplot between tenure and patner

ggplot(data=customer_churn,aes(y=tenure,x=Patner))+geom_boxplot()
ggplot(data=customer_churn,aes(y=tenure,x=Patner))+geom_boxplot(fill="violet")

#build linear regression

install.packages("caTools")

library(caTools)

sample.split(customer_churn$tenure,SplitRatio = 0.70)->split_tag

subset(customer_churn,split_tag==T)->train

subset(customer_churn,split_tag==F)->test

lm(tenure~contract,data = train)->model1

predict(model1,newdata=test)->predicted_values

head(predicted_values)

predict(model1,newdata=test)->predicted_values

head(predicted_values)

cbind(aatual=test$tenure,predicted=predicted_values)->final_data

as.data.frame(final_data)->final_data

head(final_data)

View(final_data)

final_data$Autaul - final_data$predicted ->error

head(error)

cbind(final_data,error)->final_data

head(final_data)

rmsc<-sqrt(mean((final_data$error)^2))

#multi linar regression

library(caTools)

sample.split(customer_churn$tenure,SplitRatio = 0.75)->split_model

subset(customer_churn, split_model==T)->train

subset(customer_churn, split_model==F)->test

lm(monthlycharges~Dependent+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data=train)->mod_multi_linear

predict(mod_multi_linear,newdata=test)->predict_multi_linear

head(predicted_multi_linear)

cbind(actual=test$montlycharges,predicted=predicted_multi_linear)->final_data

as.data.frame(final_data)-> final_data

head(final_data)

final_data$actual-final_data$predicted->error

cbind(final_data,error)->final_data

head(final_data)

rmse_ml<-sqrt(mean(final_data$error)^2)
rmse
rmsl_ml















































