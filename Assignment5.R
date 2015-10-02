#MA 881
#Assignment 5
#Worksheet 1

#1.Warm up

#(a)
a<-function(x){
  if(is.numeric(x)==F)stop()
  return(list(mean(x),median(x),var(x)))
  }
  
#(b)
b<-function(x){
  for(i in 0:10)
  {
    (exp(x)*(x^i))/(factorial(i))
  }
}

#(c)
c<-function(x){
  if(is.character(x)==F)stop(
    print(x)
  )
}

#(d)
d<-function(k){
  k1=as.integer(k)
  if(k1 !=k)stop
  i=1
  x=0
  while(x[i] != k){
    if(runif(1)<.5)D = 1
    else D = -1
    i = i+1
    x[i] = x[i-1]+ D
  }
  return(data.frame(x))
}

#2.Moving Averages

#(a)
ma3 <- function(y){
  n <- length(y)-2
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(y[(i):(i+2)])
    i=i+1
  }
  print(t(z))
}
#Check
y <- c(10,20,30,40,50,60,70,80,90,100,110)
ma3(y)

#(b)
ma3k <- function(x,k){
  n <- length(x)-(k-1)
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(x[(i):(i+(k-1))])
    i=i+1
  }
  print(t(z))
}
#Check
x <- c(10,20,30,40,50,60,70,80,90,100,110)
ma3k(x,10)
ma3k(x,12)

#(c)

#(d)
ma3kk <- function(x,k){
  if(k>length(x)){
    stop("k > length of x")
  }else
    n <- length(x)-(k-1)
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(x[(i):(i+(k-1))])
    i=i+1
  }
  print(t(z))
}
#check
ma3kk(x,12)

#(e)
ma3kkk <- function(x,k){
  if(k>length(x)){
    stop("k > length of x")
  }else if(k==1){
    z=mean(x)
  }else{
    n <- length(x)-(k-1)
    z <- matrix(c(rep(0,n)),nrow = 1)
    i=1
    while(i<=n){
      z[i] <- mean(x[(i):(i+(k-1))])
      i=i+1
    }
  }
  print(t(z))
}
#Check
ma3kkk(x,10)
ma3kkk(x,12)
ma3kkk(x,1)

#(3) Optional Plot
fun <- function(x,z){
  n = length(x)
  y <- rep(0,n)
  for (i in 1:n){
    if(x[i]>=4 | x[i]<=-4){
      y[i]=NA
    }
    if (x[i]<0 & x[i]>-4){
      y[i] <- x[i]^2+2*x[i]+3
    }
    if (x[i]>=0 & x[i]<2){
      y[i] <-x[i]+3
    }
    if(x[i]>=2 & x[i]<4){
      y[i] <-x[i]^2+4*x[i]-7
    }
  }
  print(y)
  if (z==TRUE){
    plot(x,y)
  }
}

fun(-2,TRUE)
fun(1,FALSE)
fun(3,TRUE)
fun(5,FALSE)

#(4) Matrix Input
double <- function(a){
  b <- which(a%%2==1)
  n <- length(b)
  i=1
  for(i in 1:n){
    a[b[i]] <- 2*a[b[i]]
  }
  print(a)
}
#Check
a <- matrix(c(1,5,-2,1,2,-1,3,6,-3), nrow=3)
a
double(a)

#(5) Poisson Process

#(a)
pois <- function(lambda,m){
  time = rexp(1,lambda)
  tmp = 0
  i = 0
  while(time < m){
    print(c("tmp" = tmp,"time" = time))
    i = i + 1
    tmp <- rexp(1,lambda)
    time=time+tmp
  }
  print(i)
}
pois(5,1)

#(b)
pois.l <- function(lambda,m){
  time = rexp(1,lambda)
  tmp = 0
  i = 0
  while(time < m){
    i = i + 1
    tmp <- rexp(1,lambda)
    time=time+tmp
  }
  print(i)
}

pois.l(5,1)

n=1
len <- rep(0,10000)
while(n <= 10000){
  len[n] <- pois.l(5,1)
  n=n+1
}

hist(len)
mean(len)
var(len)



  
  

  