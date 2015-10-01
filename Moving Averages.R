#a
ma3 <- function(v){
  n <- length(v)-2
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(v[(i):(i+2)])
    i=i+1
  }
  print(t(z))
}
#Check
v <- c(10,20,30,40,50,60,70,80,90,100,110)
ma3(v)

#b
ma3k <- function(v,k){
  n <- length(v)-(k-1)
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(v[(i):(i+(k-1))])
    i=i+1
  }
  print(t(z))
}
#Check
v <- c(10,20,30,40,50,60,70,80,90,100,110)
ma3k(v,10)
ma3k(v,12)

#d
#Stop()
ma3kk <- function(v,k){
  if(k>length(v)){
    stop("k > length of x")
  }else
  n <- length(v)-(k-1)
  z <- matrix(c(rep(0,n)),nrow = 1)
  i=1
  while(i<=n){
    z[i] <- mean(v[(i):(i+(k-1))])
    i=i+1
  }
  print(t(z))
}
#check
ma3kk(v,12)

#e
ma3kkk <- function(v,k){
  if(k>length(v)){
    stop("k > length of x")
  }else if(k==1){
    z=mean(v)
  }else{
    n <- length(v)-(k-1)
    z <- matrix(c(rep(0,n)),nrow = 1)
    i=1
    while(i<=n){
      z[i] <- mean(v[(i):(i+(k-1))])
      i=i+1
      }
    }
  print(t(z))
}
#Check
ma3kkk(v,10)
ma3kkk(v,12)
ma3kkk(v,1)