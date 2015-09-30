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

#(5) Poisson Process

#(a)
time<-0
out<-0
pois<-function(lamda,M){
    while(time<M){
      tmp<-rexp(1,lamda)
      time<-time+tmp
      out<-c(out,tmp)
    }
    print(out)
}

pois(5,1)

#(b)

  
  

  