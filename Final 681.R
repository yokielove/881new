#MA 681
#Final Project

library(RMySQL)
library(dplyr)
library(ggplot2)

#Read data from MySQL database 
mydb=dbConnect(MySQL(),user='Hanxi',password='Hanxiword',dbname='cars2000_2016',host='ma881second.ctfpkiwhxkbl.us-east-1.rds.amazonaws.com')

#table name
dbListTables(mydb)

#choose the colnames of table
dbListFields(mydb,"cars2000")

#read cars2000 data 
cars2000<-dbSendQuery(mydb,'select*from cars2000')
cars2000<-dbFetch(cars2000)
head(cars2000)
qplot(cars2000$`Greenhouse Gas Score`,binwidth=.5)
qplot(cars2000$`Cmb MPG`,binwidth=.5)

#For some reason, i have to run this code again to connect another dataset.
mydb=dbConnect(MySQL(),user='Hanxi',password='Hanxiword',dbname='cars2000_2016',host='ma881second.ctfpkiwhxkbl.us-east-1.rds.amazonaws.com')

#read cars2016 data
cars2016<-dbSendQuery(mydb,'select*from cars2016')
cars2016<-dbFetch(cars2016)
head(cars2016)
qplot(cars2016$`Greenhouse Gas Score`,binwidth=.5)
qplot(cars2016$`Cmb MPG`,binwidth=.5)

#compare the greenhouse between 2000 and 2016 on boxplot
boxplot(cars2000$`Greenhouse Gas Score`,cars2016$`Greenhouse Gas Score`)

#compare the cmb MPG between 2000 and 2016 on boxplot
boxplot(cars2000$`Cmb MPG`,cars2016$`Cmb MPG`)

#summary the data
summary(cars2000$`Greenhouse Gas Score`)
summary(cars2016$`Greenhouse Gas Score`)

#Based on the summary, we can see that the min, 1st quantile, median , mean, 3rd quantile and max all 
#increase from 2000 to 2016.

# is the greenhouse gas score of 2000 and 2016 different from the gas mileage of the other cars in the list?
#I am going to use T-test to test it.
t.test(cars2000$`Greenhouse Gas Score`,cars2016$`Greenhouse Gas Score`)


#Since P-value < 0.001, therefore we can said that it shows that there is a significant difference between 
#two groups'greenhouse gas score

#How has gas mileage changed between 2000 and 2016
#I am going to use T-test to test if there is a difference.
t.test(cars2000$`Cmb MPG`,cars2016$`Cmb MPG`)

#since p-value<0.001,therefore we can said that it shows that there is a significant difference between 
#2000 and 2016 on cmb MPG. 

summary(cars2000$`Cmb MPG`)
summary(cars2016$`Cmb MPG`)

#Base on the summary data, we can see that the min, 1st quantile, median, mean, 3rd quantile and max all increase
#from 2000 to 2016 on cmb MPG. 

write.csv(cars2000,file="f:/Program Files/RStudio/cars2000.csv")
cars2000<- read.csv("F:/Program Files/RStudio/cars2000.csv")

write.csv(cars2016,file="f:/Program Files/RStudio/cars2016.csv")
cars2016<- read.csv("F:/Program Files/RStudio/cars2016.csv")

#use dplyr to produce the same subsets of the cars2000_2016 data

head(cars2000)
head(cars2016)

subset1<-filter(cars2000)
subset2<-filter(cars2016)

#Use the data to perform a hypothesis test

#H0 : there is no difference between two subsets'greenhouse gas score
#H1 : there is a difference between two subsets'greenhouse gas score

#means for two subsets
mean1<-mean(subset1$Greenhouse.Gas.Score)
mean2<-mean(subset2$Greenhouse.Gas.Score)

#standard deviation for two subsets
sd1<-sd(subset1$Greenhouse.Gas.Score)
sd2<-sd(subset2$Greenhouse.Gas.Score)

#length for two subsets
length1<-length(subset1$Greenhouse.Gas.Score)
length2<-length(subset2$Greenhouse.Gas.Score)

hypothesis<-(mean1-mean2)/sqrt((sd1)^2/length1+(sd2)^2/length2)

#to calcuate the p-value
2*pnorm(-abs(hypothesis))

#since the p-value <0.001. therefore we reject the null hypothesis,
#So that there is a significant difference between two subset's greenhouse gas score.

#then, again Use the data to perform a hypothesis test

#H0 : there is no difference between two subsets'MPG
#H1 : there is a difference between two subsets'MPG

#I choose cmb mpg, since cmb is the combination for all mpg

#means for two subsets
mean1<-mean(subset1$Cmb.MPG)
mean2<-mean(subset2$Cmb.MPG)

#standard deviation for two subsets
sd1<-sd(subset1$Cmb.MPG)
sd2<-sd(subset2$Cmb.MPG)

#length for two subsets
length1<-length(subset1$Cmb.MPG)
length2<-length(subset2$Cmb.MPG)

hypothesis<-(mean1-mean2)/sqrt((sd1)^2/length1+(sd2)^2/length2)

#to calcuate the p-value
2*pnorm(-abs(hypothesis))

#since the p-value <0.001. therefore we reject the null hypothesis,
#So that there is a significant difference between two subsets' cmb mpg.

#Using t-test to compare them.

t.test(cars2000$Greenhouse.Gas.Score,cars2016$Greenhouse.Gas.Score,alternative = 'greater')

#Then we do a permutation test to compare both groups

samp <- c(cars2016$Greenhouse.Gas.Score,cars2000$Greenhouse.Gas.Score)
l <- length(samp)
cars.n<- length(cars2000$Greenhouse.Gas.Score)

dmeanT <- NULL

for(i in 1:1000){
  
  sampler = sample((1:l),cars.n,replace=FALSE)
  cars2000.s = samp[sampler]
  cars2016.s = samp[-sampler]
  dmean.s = mean(cars2000.s)-mean(cars2016.s)
  dmeanT[i] = dmean.s
}

qplot(dmeanT,binwidth=.25)

f <- ecdf(dmeanT)
quantile(dmeanT,.95)
quantile(dmeanT,.05)
summary(dmeanT)

# observed difference

mean(cars2000$Greenhouse.Gas.Score)-mean(cars2016$Greenhouse.Gas.Score)

#It show that in 2016, the greenhouse gas score significant increase by 1.864 on average more than year 2000.

#then again do the same thing on cmb mpg

samp <- c(cars2016$Cmb.MPG,cars2000$Cmb.MPG)
l <- length(samp)
cars.n<- length(cars2000$Cmb.MPG)

dmeanT <- NULL

for(i in 1:1000){
  
  sampler = sample((1:l),cars.n,replace=FALSE)
  cars2000.s = samp[sampler]
  cars2016.s = samp[-sampler]
  dmean.s = mean(cars2000.s)-mean(cars2016.s)
  dmeanT[i] = dmean.s
}

qplot(dmeanT,binwidth=.25)

f <- ecdf(dmeanT)
quantile(dmeanT,.95)
quantile(dmeanT,.05)
summary(dmeanT)

# observed difference

mean(cars2000$Cmb.MPG)-mean(cars2016$Cmb.MPG)

#it shows that the cmb mpg increased by 8.17 units from 2000 to 2016.

#based on the two permatation above, we can see that the greenhous gas score increase while the cmb mpg increase
# from 2000 to 2016

cars2000 <- read.csv("F:/MSSP/2015FALL/MA 681/Final/cars2000.csv")
attach(cars2000)

cars2016 <- read.csv("F:/MSSP/2015FALL/MA 681/Final/cars2016.csv")
attach(cars2016)

#combine cars2000 and cars2016

cars<-cbind(cars2000,cars2016)
head(cars)

#Data cleaning for cars dataset

cars<-cars[,!duplicated(colnames(cars),fromLast = F)]
head(cars)

#We are doing multiple regression to see which factor is most significant to effect greenhouse gas score.
#For factor, I use all facotr that contain number.

regout<-lm(Greenhouse~Displ+Cyl+AirPollution+CityMPG+HwyMPG+CmbMPG+Comb.CO2)
summary(regout)

#From this regression we can see that the City MPG, Hwy MPG ,Cmb MPG and Comb.co2 are all have p-value less than
#0.001, therefore those facotrs are significant associate with greenhous gas score. 

#When City mpg increase 1 unit, greenhouse gas score decrease 0.1830 unit
#When Hwy mpg increase 1 unit, greenhouse gas score decrease 0.07428 unit
#When Cmb mpg increase 1 unit, greenhouse gas score increase 0.2712 unit
#When comb.co2 increase 1 unit, greenhouse gas score decrease 0.0144 unit

#the p-value for this model is less than 0.001, which means that at lease one variable is significantly associate
#with the greenhouse gas score.

#the model R-squared is 0.9502, which means 95.02% of tge variability in greenhouse gas score is explained 
#by the set of variables in this model. 

plot(regout)

#the plot shows that it is a little bit not that well fitted than residuals.

#USe the same regression model on cmb mpg again.

regout1<-lm(CmbMPG~Displ+Cyl+AirPollution+CityMPG+HwyMPG+Greenhouse+Comb.CO2)
summary(regout1)

#From this regression, we can see that the Displ, City MPG, Hwy MPG, greenhouse gas score and comb.co2
#all have p-value less than 0.05, therefore those variable are all significantly associate with the cmb mpg.

#When Displ increase 1 unit, cmb mpg decrease 0.06974 unit.
#when city mpg increase 1 unit, cmb mpg increase 0.625 unit.
#when hwy mpg increase 1 unit, cmb mpg increase 0.3343 unit.
#when greenhouse gas score increase 1 unit, cmb mpg increase 0.3081 unit.
#when comb.co2 increase 1 unit, cmb mpg increase 0.0022 unit.

#the p-value for this regression model is also less than 0.001, which means that at lease one variable is significantly associate
#with the cmb mpg.

#the model R-squared is 0.9967, which means 99.67% of tge variability in cmb mpg is explained 
#by the set of variables in this model. which is pretty well explain. 

plot(regout1)

#the plot shows that the residual is fitted the model and plot.

#According to regression model, we can see there is a relationship between greenhouse gas score and mpg.

#I download this from the webiste you give us, and this csv contain all model year from 1984 to 2016
vehicles <- read.csv("F:/MSSP/2015FALL/MA 681/Final/vehicles.csv")
vehicles1<-unlist(vehicles)
  
head(vehicles1)

vehicles1 <- data.frame(vehicles1[1:227])
colnames(vehicles1) <- "x"

qplot(x, data=vehicles1, geom = "histogram",binwidth=.15)

mean(vehicles1$x)
var(vehicles1$x)

alpha <- mean(vehicles1$x)^2/var(vehicles1$x) #18.53374
lambda <- mean(vehicles1$x)/var(vehicles1$x)  #1.044536

ggplot(vehicles1,aes(x))+geom_histogram(binwidth=.3,color="black",fill="white")
ggplot(vehicles1,aes(x))+geom_density()

ggplot(vehicles1,aes(x))+geom_histogram(aes(y=..density..),
                                    binwidth=.3,
                                    color="black",fill="white")+
  geom_density(alpha=0.2,fill="#FF6666")

#becase the data is from 1984 to 2016, so the graph also so the trend from 1984 to 2016.

# find  max likelihood
x1 <- vehicles1$x

n <- length(vehicles1$x)
# remember we know how to MINIMIZE so
# setup theta <- c(alpha,lambda)
# and 

alpha <- mean(vehicles1$x)^2/var(vehicles1$x)  # 18.53374
lambda <- mean(vehicles1$x)/var(vehicles1$x)   # 1.044536

set.seed(50)
a<-rgamma(n=227,shape = 18.53374,scale = 1.044536)
a.hat<-18.53374

B <- 1000 
Tboot <- rep(0,B)

for(i in 1:B){
  a.s<- sample(a, 227, replace=TRUE) 
  Tboot[i] <-mean(a.s)^2/var(a.s)
}

se <- sqrt(var(Tboot)) #standard error
se 

#a way to find 95% CI
upper<-a.hat+2*se 
upper
lower<-a.hat-2*se
lower

#Lambda

set.seed(50)
L<-rgamma(n=227,shape = 16.577,scale =1.00214)
L.hat<-16.577

B <- 1000 
Tboot <- rep(0,B)

for(i in 1:B){
  L.s<- sample(a, 227, replace=TRUE) 
  Tboot[i] <-mean(L.s)/var(L.s)
}

se <- sqrt(var(Tboot)) #standard error
se 

#a way to find 95% CI
upper<-L.hat+2*se 
upper
lower<-L.hat-2*se
lower

minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+(theta[1]-1)*sum(log(x1))-theta[2]*sum(x1))}

max.likelihood <- nlminb(start=c(16.577, 1.00214), obj = minus.likelihood)

max.likelihood$par

#Alpha
set.seed(50)
a<-rgamma(n=227,shape = 22.629521,scale = 1.275368)
a.hat<-22.629521

B <- 1000 
Tboot <- rep(0,B)

for(i in 1:B){
  a.s<- sample(a, 227, replace=TRUE) 
  Tboot[i] <-mean(a.s)^2/var(a.s)
}

se <- sqrt(var(Tboot)) #standard error
se 

#a way to find 95% CI
upper<-a.hat+2*se 
upper
lower<-a.hat-2*se
lower

#Lambda

set.seed(50)
L<-rgamma(n=227,shape = 19.43575,scale =1.00314)
L.hat<-19.43575

B <- 1000 
Tboot <- rep(0,B)

for(i in 1:B){
  L.s<- sample(a, 227, replace=TRUE) 
  Tboot[i] <-mean(L.s)/var(L.s)
}

se <- sqrt(var(Tboot)) #standard error
se 

#a way to find 95% CI
upper<-L.hat+2*se 
upper
lower<-L.hat-2*se
lower


