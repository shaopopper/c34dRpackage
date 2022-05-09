describe<-function(x,na.omit=TRUE){   #na.omit=TRUE不忽略缺失指
  if (na.omit)
    Miss<-sum(is.na(x)==TRUE)   #统计缺失值
  x<-x[!is.na(x)]  #去掉缺失值
  m<-mean(x)
  n<-length(x)  #不是缺失值的个数
  s<-sd(x)
  skew<-sum((x-m)^3/s^3)/n
  kurt<-sum((x-m)^4/s^4)/n-3
  #Normal_ks=ks.test(x,"pnorm")[["p.value"]]
  Noraml_sh=shapiro.test(x)[["p.value"]]
  return(round(c(n=n,Miss=Miss,mean=m,stdev=s,
                 skew=skew,kurtosis=kurt,
                 #Normal_ks=Normal_ks
                 Noraml_sh=Noraml_sh
  )
  ,3))
}
describe_by<-function(x,na.omit=TRUE)sapply(x,describe,na.omit=TRUE)
