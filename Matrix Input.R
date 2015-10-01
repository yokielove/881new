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
