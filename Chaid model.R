setwd("C:\\Users\\User\\Desktop\\Data Science")

car <- read.csv("Car Price.csv")
str(car)
#boxplot(car)
sapply(car,function(x) is.na(x))
sum(is.na(car))
summary(car)
#car$engine_type[which(car$engine_type=="l")] <- "ohcf"
boxplot(car$Car_Price_category,car$symboling)
boxplot(car$symboling)
#boxplot(car$normalized_losses)
#car$normalized_losses[which(car$normalized_losses>198)] <- 197
boxplot(car$wheel_base)
car$wheel_base[which(car$wheel_base<=50)] <- 86.6
boxplot(car$wheel_base)
boxplot(car$length)
car$length[which(car$length>210)] <- 208.1
boxplot(car$length)
boxplot(car$width)
boxplot(car$height)
boxplot(car$curb_weight)
boxplot(car$engine_size)
car$engine_size[which(car$engine_size>210)] <- 209
sum(is.na(car$horsepower))
boxplot(car$horsepower) 
car$horsepower[which(car$horsepower>185)] <- 184
boxplot(car$horsepower)
boxplot(car$peak_rpm)
car$peak_rpm[which(car$peak_rpm>6001)] <- 6000
boxplot(car$peak_rpm)
boxplot(car$city_mpg)
car$city_mpg[which(car$city_mpg>39)] <- 39
boxplot(car$highway_mpg)
boxplot(car$bore_new)
boxplot(car$stroke)
car$stroke[which(car$stroke <2.6)] <- 2.64
car$stroke[which(car$stroke>3.91)] <- 3.9
boxplot(car$compression_ratio)
car$compression_ratio[which(car$compression_ratio>12)] <- 11.5
boxplot(car$horsepower)
boxplot(car$stroke_new)
car$stroke_new[which(car$stroke_new <2.6)] <- 2.64
car$stroke_new[which(car$stroke_new>3.91)] <- 3.9
boxplot(car$horsepower_new)
car$horsepower_new[which(car$horsepower_new>185)] <- 184
boxplot(car$peak_rpm_new)
car$peak_rpm_new[which(car$peak_rpm_new>6001)] <- 6000
#checking the box plot after removing outliers
boxplot(car$symboling)
boxplot(car$normalized_losses)
boxplot(car$wheel_base)
boxplot(car$length)
boxplot(car$width)
boxplot(car$height)
boxplot(car$curb_weight)
boxplot(car$engine_size)
boxplot(car$horsepower) 
boxplot(car$horsepower)
boxplot(car$peak_rpm)
boxplot(car$city_mpg)
boxplot(car$highway_mpg)
boxplot(car$bore_new)
boxplot(car$stroke)
boxplot(car$compression_ratio)
boxplot(car$horsepower)
boxplot(car$stroke_new)
boxplot(car$horsepower_new)
boxplot(car$peak_rpm_new)
str(car)
summary(car)
#missing value treatment
#car$normalized_losses[is.na(car$normalized_losses)]=122
car$num_of_doors[is.na(car$num_of_doors)]="two"
car$engine_location[is.na(car$engine_location)]="front"
car$bore[is.na(car$bore)]=3.298
car$stroke[is.na(car$stroke)]=3.363
car$horsepower[is.na(car$horsepower)]=100.6
car$peak_rpm[is.na(car$peak_rpm)]=5176
sum(is.na(car))
car$Car_Price_category=ifelse(car$Car_Price_category=="Lowprice",0,1)
#table(tree$CREDIT_RATING,tree$CREDIT_CARDS)
library(dplyr)
sapply(car, function(x)sum(is.na(x)))
library(tidyr)
check <-car %>% select_if(is.numeric)
names(check)
cor(check)
check1 <- car %>% select_if(is.factor)
names(check1)
chisq.test(table(car$make,car$Car_Price_category)) #significant
chisq.test(table(car$Car_Price_category,car$symboling)) #significant
car$Car_Price_category=as.factor(car$Car_Price_category)
str(car$normalized_losses)
t.test(car$normalized_losses~car$Car_Price_category)# not significant
t.test(car$wheel_base~car$Car_Price_category)#significant
chisq.test(table(car$Car_Price_category,car$fuel_type)) #significant
chisq.test(table(car$Car_Price_category,car$aspiration))#not  significant
chisq.test(table(car$Car_Price_category,car$num_of_doors))#not signficant
chisq.test(table(car$Car_Price_category,car$body_style))# significant
chisq.test(table(car$Car_Price_category,car$drive_wheels))#significant
chisq.test(table(car$Car_Price_category,car$engine_location))#significant
chisq.test(table(car$Car_Price_category,car$engine_type))#significant
chisq.test(table(car$Car_Price_category,car$num_of_cylinders))#sigificant
chisq.test(table(car$Car_Price_category,car$num_of_doors_new))#not significant
chisq.test(table(car$Car_Price_category,car$drive_wheel_new))#significant
chisq.test(table(car$Car_Price_category,car$engine_location_new))#significant
t.test(car$length~car$Car_Price_category)#significant
t.test(car$width~car$Car_Price_category)#significant
t.test(car$height~car$Car_Price_category)#significant
t.test(car$curb_weight~car$Car_Price_category)#significant
t.test(car$engine_size~car$Car_Price_category)#significant
t.test(car$bore~car$Car_Price_category)#significant
t.test(car$stroke~car$Car_Price_category)#significant
t.test(car$compression_ratio~car$Car_Price_category)#significant
t.test(car$horsepower~car$Car_Price_category)#significant
t.test(car$peak_rpm~car$Car_Price_category)#not significant
t.test(car$city_mpg~car$Car_Price_category)#significant
t.test(car$highway_mpg~car$Car_Price_category)#significant
t.test(car$horsepower_new~car$Car_Price_category)#significant
t.test(car$peak_rpm_new~car$Car_Price_category)#not significant
#car <- SMOTE(Car_Price_category~., car,perc.over = 100,k=5, perc.under=200)
set.seed(4444)
library(caret)

#c1 <- creatdatapa(n=nrow(car),0.7*nrow(car))
#library(smotefamily
#install.packages("DMwR")
library(DMwR)
car = SMOTE(Car_Price_category~., car, perc.over = 200, k = 5, perc.under = 200)
#library(caret)

c1 <- createDataPartition(car$Car_Price_category, p = 0.7, list = F )
#head(c1)
str(car)
#alias(car$make)
train <- car[c1,]
test <- car[-c1,]
#View(test)
#write.csv(test,file="testmyapp.csv")
index(-c1)
dim(train)
dim(test)
prop.table(table(car$Car_Price_category))
prop.table(table(train$Car_Price_category))
prop.table(table(test$Car_Price_category))
#library(smotefamily)
#install.packages("DMwR")
#library(DMwR)
library(car)

##### decision tree#####
#####CHAID Model####

library(rpart) 
library(rattle)	
library(rpart.plot)

library(RColorBrewer)
library(party)		
library(partykit)	
library(caret)
train2 <- car[c1,]

#### chaid model

quantile(train2$wheel_base)

train2$wheel_base <- ifelse(train2$wheel_base>=96,"High",
                           ifelse(train2$wheel_base>=94,"Medium","Low"))
str(train2$wheel_base)
quantile(train2$length)

train2$length <- ifelse(train2$length>=173,"High",
                       ifelse(train2$length>=164,"Medium",
                              "Low"))
quantile(train2$height)

train2$height <- ifelse(train2$height>=54,"High",
                       ifelse(train2$height>=50,"Medium",
                              "Low"))

quantile(train2$width)

train2$width <- ifelse(train2$width>=66,"High",
                      ifelse(train2$width>=64,"Medium",
                             "Low"))

quantile(train2$engine_size)

train2$engine_size <- ifelse(train2$engine_size>=146,"High",
                            ifelse(train2$engine_size>=92,"Medium",
                                   "Low"))



quantile(train2$highway_mpg)

train2$highway_mpg <- ifelse(train2$highway_mpg>=38,"High",
                            ifelse(train2$highway_mpg>=26,"Medium",
                                   "Low"))
quantile(train2$bore_new)

train2$bore_new <- ifelse(train2$bore_new>=3.5,"High",
                         ifelse(train2$bore_new>=3,"Medium",
                                "Low"))

quantile(train2$stroke_new)

train2$stroke_new <- ifelse(train2$stroke_new>=3.4,"High",
                           ifelse(train2$stroke_new>=3.1,"Medium",
                                  "Low"))

quantile(train2$horsepower_new)

train2$horsepower_new <- ifelse(train2$horsepower_new>=121,"High",
                               ifelse(train2$horsepower_new>=70,"Medium",
                                      "Low"))
train2$drive_wheel_new <- as.factor(train2$drive_wheel_new)
train2$engine_location <- as.factor(train2$engine_location)
train2$fuel_system <- as.factor(train2$fuel_system)
train2$fuel_type <- as.factor(train2$fuel_type)
train2$drive_wheels <- as.factor(train2$drive_wheels)
train2$body_style <- as.factor(train2$body_style)
train2$wheel_base <- as.factor(train2$wheel_base)
train2$length <- as.factor(train2$length)
train2$width <- as.factor(train2$width)
train2$height <- as.factor(train2$height)
train2$engine_size <- as.factor(train2$engine_size)
train2$highway_mpg <- as.factor(train2$highway_mpg)
train2$bore_new <- as.factor(train2$bore_new)
train2$stroke_new <- as.factor(train2$stroke_new)
train2$horsepower_new <- as.factor(train2$horsepower_new)
str(train2)
dim(train2)
#dchaid <- train[,-35]
#dim(dchaid)
#dchaid <- train[,-34]
#str(dchaid)
#chaidD <- dchaid[,-34]
#dim(chaidD)
#str(chaidD)
train2$Car_Price_category <- as.factor(train2$Car_Price_category)

write.csv(train2,"traindatCHAID.csv",row.names = F)
#install.packages("CHAID", repos="http://R-Forge.R-project.org") 

#install.packages("CHAID")
library("CHAID")
ch1 <- chaid(Car_Price_category~wheel_base+ length+ width+ height
             + engine_size+ highway_mpg
             + bore_new+ stroke_new+ horsepower_new+drive_wheel_new
             +engine_location_new+fuel_system+wheel_base
             +fuel_type+ drive_wheels+ body_style,
             data=train2)
plot(ch1)

Chaidmodel <- as.data.frame(predict(ch1,train2,type="response"))
CHAIDModelProb <- as.data.frame(predict(ch1,train2,type="prob"))
train2 <- cbind(train2,Chaidmodel,CHAIDModelProb)
#table(train2$Car_Price_category,train2$Chaidmodel)
dim(train2)
names(train2)[34] <- "ChaidModel"
y <- table(train2$Car_Price_category,train2$ChaidModel)
y
## Sensitivity and Specifity
prop.table(y,1)
##Overall Acc``
sum(diag(x))/sum(x)
#write.csv(train,"threeModelOutput1.csv", row.names =F )

