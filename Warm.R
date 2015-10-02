#a
mmv <- function(x){
  list("mean"=mean(x),"median"=median(x),"variance"=var(x))
}
mmv(1:10)

#b
sumex <- function(x,n){
  d <- rep(0,n)
  for(i in 1:n){
    d[i] <- (x^i)*exp(-x)/factorial(i)
    }
  sum(d)
}
sumex(3,3)
#Check
(exp(-3)*(3^1)/1) + (exp(-3)*(3^2)/2) + (exp(-3)*(3^3)/(3*2*1))

#c
checkchar <- function(x){
  n=length(x)
  for(i in 1:n){
  print((ifelse(is.character(x[[i]])==TRUE, "yes","no")))
  }
}
#Check
a <- list("Hello",b="world",c=7,d=TRUE)
checkchar(a)

#d
walk <- function(k){
  x=0
  while(x!=k){
    if(runif(1)<0.5){
      D=1
    }else if(runif(1)>=0.5){
      D=-1
    }
    x=x+D
    print(x)
  }
}
#Check
walk(5)
