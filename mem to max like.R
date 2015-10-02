library(ggplot2)
data <- read.csv("C:/Users/yiton/Downloads/illinois rain 1960-1964.csv", header=FALSE)
data1 <- unlist(data)
head(data1)
tail(data1)
data1 <- data.frame(data1[1:227])
colnames(data1) <- "x"

qplot(x, data=data1, geom = "histogram",binwidth=.3)


mean(data1$x)
var(data1$x)


alpha <- mean(data1$x)^2/var(data1$x)
lambda <- mean(data1$x)/var(data1$x)

# Homework -- produce a density plot of the 
# gamma distribution with these parameters
# see http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# for instructions on how to plot
ggplot(data1, aes(x)) + geom_density()

## homework # 2
## now bootstrap -- samples (n=227) from gamma(alpha, lambda)
## we get variance for alpha and lambda
## note lambda = mean/var, alpha = mean^2

# se for alpha
set.seed(50) 
x <- rgamma(n=100, shape = alpha , scale = lambda)
x.hat <- mean(x)/var(x)

n <- length(x)

B <- 1000
Tboot <- rep(0,B)

for(i in 1:B){
  xstar <- sample(x,n, replace=TRUE)
  Tboot[i] <- mean(xstar)/var(xstar)
}

se <- sqrt(var(Tboot))
se
Normal <- c(x.hat - 2*se, x.hat + 2*se)
Normal


# se for lambda
set.seed(50) 
x <- rgamma(n=100, shape = alpha , scale = lambda)
x2.hat <- mean(x)^2/var(x)

n <- length(x)

B <- 1000
Tboot <- rep(0,B)

for(i in 1:B){
  xstar <- sample(x,n, replace=TRUE)
  Tboot[i] <- mean(xstar)^2/var(xstar)
}

se <- sqrt(var(Tboot))
se
Normal <- c(x2.hat - 2*se, x2.hat + 2*se)
Normal
## 



###################
##  max likelihood


# try nlminb

func <- function(y){(y[1]-3)^2 + (y[2]+1)^2}
min.func <- nlminb(start=c(1,1), obj= func)
min.func$par
# comes up with the obvious answer


# now lets use it to get max likelihood
x1 <- data1$x

n <- length(data1$x)
# remember we know how to MINIMIZE so
# setup theta <- c(alpha,lambda)
# and 

minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+(theta[1]-1)*sum(log(x1))-theta[2]*sum(x1))}

max.likelihood <- nlminb(start=c(.3762, 1.6767), obj = minus.likelihood)

max.likelihood$par


# homework bootstrap to get standard errors for alpha and lambda





