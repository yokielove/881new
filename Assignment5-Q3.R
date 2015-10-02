# 3. Optional Plot
fx <- function(x, plot)
{
  i <- 1
  y <- rep(0, length(x))
  while(i <= length(x))
  {
    if(x[i] <= -4 | x[i] >= 4)
    {
      y[i] <- "NA"
      i = i + 1
    }else if(x[i] < 0)
    {
      y[i] <- x[i]^2 + 2*x[i] + 3
      i = i + 1
    }else if(x[i] < 2)
    {
      y[i] <- x[i] + 3
      i = i + 1
    }else if(x[i] >= 2)
    {
      y[i] <- x[i]^2 + 4*x[i] - 7
      i = i + 1
    }
  }
  if(plot == TRUE)
  {
    plot(x, y)  
  }
  return(y)
}
fx(-4:4, TRUE)