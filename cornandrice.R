library(ggplot2)
cornrice<-read.csv("corn-rice.csv", header=TRUE)
attach(cornrice)
head(cornrice)

#Data Cleaning, To remove all D
cornrice[cornrice == "(D)"] <- NA
cornrice[cornrice == " (D)"] <- NA

cornrice$value <- as.numeric(cornrice$Value)
cornrice$CV <- as.numeric(cornrice$CV)

#coefficent of variation= CV= the ratio of the standard deviation to the mean, CV =sd/mean

corn.rice <- data.frame(cornrice$Year, cornrice$State,cornrice$Commodity,cornrice$value,cornrice$CV,stringsAsFactors = F)
head(corn.rice)

colnames(corn.rice) <- c("year", "state", "crop", "value", "CV")
head(corn.rice)

tmp<-colnames(corn.rice)
tmp

cr1 <- data.frame(corn.rice,stringsAsFactors = F)

qplot(x=year,y=value,geom="point",data=cr1)


qplot(data = cr1, x = year, binwidth = 0.5)

qplot(x = state, y = value  , data = cr1,  geom = "point", shape = crop)

qplot(data=cr1,x=year,y=value,binwidth=1)
qplot(data=cr1,x=year,y=state,binwidth=1)
qplot(data=cr1,x=year,y=CV,binwidth=1)

qplot(x=state, data=cr1, geom="histogram")
qplot(data=cr1,x=state,y=value,geom="point")

qplot(factor(year),data=cr1,geom="bar",fill=factor(crop))

qplot(factor(year), stat= "bin", data=cr1, geom="bar", fill=factor(crop))

qplot(factor(year), value, stat= "identity", data=cr1, geom="bar", fill=factor(crop))

qplot(factor(year), value, data=cr1, geom="bar", stat= "identity", position= "dodge", fill=factor(crop))

qplot(factor(year), CV, data=cr1, geom="bar", stat= "identity", position= "dodge", fill=factor(crop))

qplot(factor(state), value, data=cr1, geom="bar", stat= "identity", position= "dodge", fill=factor(crop))

qplot(factor(state), CV, data=cr1, geom="bar", stat= "identity", position= "dodge", fill=factor(crop))


