fun <- function(x,z){
  if(x>=4 | x<=-4){
    stop("Invalid input")
    }
  if (x<0){
    y <- x^2+2*x+3
    }
  if (x>=0 & x<2){
    y <-x+3
    }
  if(x>=2){
    y<-x^2+4*x-7
    }
  print(c(x,y))
  if (z==TRUE){
    plot(x,y)
  }
}

fun(-2,TRUE)
fun(1,FALSE)
fun(3,TRUE)
fun(5,FALSE)