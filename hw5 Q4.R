#Q4 : matrix input
doubleodd <- function(a){
 
  n <- length(a)
  i=1
  for(i in 1:n){
    if( a[i]%%2==1){
      a[i] <- 2*a[i]
    }
    
  }
  print(a)
}

#Check
a <- matrix(c(1:9), nrow=3)
a
doubleodd(a)
