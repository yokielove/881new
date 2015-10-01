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


#b
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
