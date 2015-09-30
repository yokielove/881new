str1="http://www.ndbc.noaa.gov/view_text_file.php?filename="
str2=".txt.gz&dir=data/historical/stdmet/"

library(ggplot2)

buoynum<-"51201"
y1<-2011
y2<-2014
data<-NULL
while(y1<=y2){
  fyl<-paste0(str1,buoynum,"h",y1,str2)
  header<-scan(fyl,nlines=1,what=character())
  buoy<-read.table(fyl,skip=2,header=FALSE)
  colnames(buoy)<-header
  buoy$WTMP[buoy$WTMP==999]<-NA
  buoy$WVHT[buoy$WVHT==999]<-NA
  buoy$DPD[buoy$DPD==999]<-NA
  TMP<-c(data,buoy$WTMP)
  HT<-c(data,buoy$WVHT)
  PD<-c(data,buoy$DPD)
  cat(paste(y1))
  y1<-y1+1
}
plot(TMP,type="l")
plot(HT,type="l")
plot(PD,type="l")
qplot(PD,HT)
cor(PD,HT)



   
