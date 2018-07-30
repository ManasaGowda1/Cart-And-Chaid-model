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
#car = SMOTE(Car_Price_category~., car, perc.over = 200, k = 5, perc.under = 200)
#library(caret)

c1 <- createDataPartition(car$Car_Price_category, p = 0.7, list = F )

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
#####CART Model####

library(rpart) 
library(rattle)	
library(rpart.plot)

library(RColorBrewer)
library(party)		
library(partykit)	
library(caret)
train1 <- car[c1,]
test1 <- car[-c1,]

d1 <- rpart(Car_Price_category~symboling
            +wheel_base+ length+ width+ height
            + engine_size+ highway_mpg
            + bore_new+ stroke_new+ horsepower_new+drive_wheel_new
            +engine_location_new+fuel_system
            +fuel_type+ drive_wheels+ body_style,
            data=train1)
rpart.plot(d1)
train1$CartModel <- predict(d1,train1,type='class')
train1$CARTPROB <- predict(d1,train1,type='prob')
rpart.plot(d1,cex =1,type=2)
prop.table(table(train1$Car_Price_category,train1$CartModel))
test1$CartModel <- predict(d1,test1,type='class')
test1$CARTPROB <- predict(d1,test1,type='prob')
rpart.plot(d1,cex =1,type=2)
prop.table(table(test1$Car_Price_category,test1$CartModel))



