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

#Check:
a= c(-2,1,3,5)
fun(a,FALSE)
