mi <- function(a)
{
  a[a%%2 == 1] <- 2*a[a%%2 == 1]
  print(a)
}
# check
a <- matrix(c(1,5,-2,1,2,-1,3,6,-3), nrow=3)
mi(a)
b <- matrix(1:12, nrow = 4)
mi(b)