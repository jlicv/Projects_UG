library(zoo)
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset")
#找出某个行业的上游和下游各K个行业，并将它们的序列画在一起。
#读取所需的数据：
yieldall=read.csv('close-yield-all.csv',check.names = FALSE,row.names=1)
weeklyyield=read.csv('weeklyyield.csv',check.names = FALSE,row.names=1)
weeklyvol=read.csv('weekly-vol.csv',check.names = FALSE,row.names=1)
Flow12=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
Marketyield=read.csv("万德A股指数-收益率序列-转制为列-时间与行业同步.csv",header=TRUE,check.names='FALSE')

##################################
#####获取每个时间点的统计窗口#####
##################################
timesequence.kmonth=function(k,binstart,times,numbin){
  library(lubridate)
  binstart=ymd(binstart)
  #要注意的是，时区里面，是往给定的时间点前推12个月。
  #例如，我这里写的是2002年4月1日，我要做的是，在这个时间点前，推12个月。将这12月的结果纳入结果。
  start.sequence=binstart + days(0:numbin)*times
  end.sequence=start.sequence %m+% months(k)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}

Probcalculation=function(threshold,marketyield,givenyield){
  #如果做Frechet变换，由于是Frechet在(0,1)区间上是一个单调函数；
  #也就是说，变换后各个数值的序还是不变的；
  #所以，变换的意义是什么？
  #研究中我们关心的是分位数，还是说百分比。我觉得是百分比。
  #所以，这个变换应该是有意义的。
  #但是，它定义的这个概率，用的是百分数，变换不变换，都不影响这个概率的计算。
  Twoyield=cbind.data.frame('m'=marketyield,'i'=givenyield)
  colnames(Twoyield)=c("m","i")
  #例如说，应该是前10%的，就挑出来前10%的样本先。
  Marketbelowthreshold=which(Twoyield[,'m']<quantile(Twoyield[,'m'],threshold))
  Givenbelowthreshold=which(Twoyield[,'i']<quantile(Twoyield[,'i'],threshold))
  Probabilityconditioned=length(intersect(Marketbelowthreshold,Givenbelowthreshold))/length(Marketbelowthreshold)
  return(Probabilityconditioned)  
}
#这部分是为了查看说，每个行业之间的渐进相关性有多大。
Changeoverquantile=function(marketyield,givenyield){
  result=c()
  quantilesequence=seq(1,0.01,-0.01)
  for(i in 1:length(quantilesequence))
    result[i]=Probcalculation(quantilesequence[i],marketyield,givenyield)
  ret=cbind.data.frame(quantilesequence,result)
  return(ret)
}

# retout=Changeoverquantile(Twoyield$`Market-Yield`,as.matrix(t(yieldall[15,])))
# plot(retout)

#################################
#########获取计算的公式##########
#################################
CrashProbCount.Oneindex.Week=function(threshold=0.05,datebrackets,yieldseries,marketyield,weightmatrix){
  CrashProb_All=data.frame(row.names = rownames(yieldseries))
  for (i in 1:nrow(datebrackets)){
    YieldGet_Sector=yieldseries[,which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    if(is.vector(YieldGet_Sector)){
      YieldPrev_Sector=yieldseries[,which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
      #lambda的引入是考虑有时候只取到一个样本（例如，国庆黄金周）时候，计算收益率和波动率时的样本数量太少的问题。 
      lambda=1/3
      YieldGet_Sector=cbind.data.frame(lambda*YieldPrev_Sector,(1/lambda)*YieldGet_Sector)
    }
    YieldGet_Sector=t(YieldGet_Sector)
   
    YieldGet_Market=marketyield[which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    CrashProbcalculation=function(threshold,marketyield,givenyield){
      Twoyield=cbind.data.frame('m'=marketyield,'i'=givenyield)
      colnames(Twoyield)=c("m","i")
      Marketbelowthreshold=which(Twoyield[,'m']<quantile(Twoyield[,'m'],threshold))
      Givenbelowthreshold=which(Twoyield[,'i']<quantile(Twoyield[,'i'],threshold))
      CrashProbabilityconditioned=length(intersect(Marketbelowthreshold,Givenbelowthreshold))/length(Marketbelowthreshold)
      #例如说，应该是前10%的，就跳出来前10%的样本先。
      return(CrashProbabilityconditioned)  
    }
    
    CrashProb_Out=CrashProbcalculation(threshold,YieldGet_Market,YieldGet_Sector)  
    CrashProb_All=cbind.data.frame(CrashProb_All,CrashProb_Out)
  }
  colnames(CrashProb_All)=paste(week(datebrackets[,1]), year(datebrackets[,1]), sep = "-")
  return(CrashProb_All)
}

upperandlower.getnames=function(k,sectorname,flow){
  diag(flow)=0
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperstream.topk=round(upperstream.topk/10000,digits=1)
  upperk.names=names(upperstream.topk)
  upperweight=upperstream.topk/sum(upperstream.topk)
  upperweight=round(upperweight,digits=3)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerstream.topk=round(lowerstream.topk/10000,digits=1)
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  lowerweight=round(lowerweight,digits=3)
  rbind.data.frame('upper'=upperk.names,'upperflow'=upperstream.topk,'upperprop'=upperweight,'lower'=lowerk.names,'lowerflow'=lowerstream.topk,'lowerprop'=lowerweight)
}

upperandlowerindex.CrashProb=function(thresholdvalue,interval,k,sectorname,yieldall,marketyieldall,flow){
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  sectoryield.weekly=CrashProbCount.Oneindex.Week(datebrackets = interval,marketyield = marketyieldall,yieldseries = sectoryield.matrix,weightmatrix = flow,threshold = thresholdvalue)
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperk.names=names(upperstream.topk)
  #上游行业权重的计算，计算方法是基于流量比例的大小。
  upperweight=upperstream.topk/sum(upperstream.topk)
  #点乘，得到重组的序列。
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[upperk.names,])
  upperyield.matrix = as.data.frame(upperyield.matrix)
  #得到了上游K个行业的周收益率。
  upperyield.weekly=CrashProbCount.Oneindex.Week(datebrackets = interval,marketyield = marketyieldall,yieldseries = upperyield.matrix,weightmatrix = flow,threshold = thresholdvalue)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowerk.names,])
  loweryield.matrix = as.data.frame(loweryield.matrix)
  loweryield.weekly=CrashProbCount.Oneindex.Week(datebrackets = interval,marketyield = marketyieldall,yieldseries = loweryield.matrix,weightmatrix = flow,threshold = thresholdvalue)
  combinedyield.weekly=rbind.data.frame(sectoryield.weekly,upperyield.weekly,loweryield.weekly)
  uppernames=paste(sectorname,'upper',k,sep='-')
  lowernames=paste(sectorname,'lower',k,sep='-')
  rownames(combinedyield.weekly)=c(sectorname,uppernames,lowernames)
  return(combinedyield.weekly)
}

weekly.upperandlower.allsector=function(crashpercentile=0.05,interval,label,k){
  upperandlowerindex.all=data.frame(row.names=c("sector-crash",paste("upper-top-crash",k),paste("lower-top-crash",k)))
  for(i in 1:length(label)){
    sectorquery=label[i]
    topksectornames=upperandlower.getnames(k,sectorquery,Flow12)
    colnames(topksectornames)=c(1:k)
    upperandlowerindex.CrashProb.df=upperandlowerindex.CrashProb(thresholdvalue = crashpercentile,interval = interval,k = k,sectorname = sectorquery,yieldall = yieldall,marketyieldall = Marketyield$`Log Return`,flow = Flow12)
    upperandlowerindex.CrashProb.df=as.matrix(upperandlowerindex.CrashProb.df)
    #对序列作延滞的处理，相当于分别去头和去尾，再组合在一起。
    CrashProb.sector=tail(upperandlowerindex.CrashProb.df[1,],-1)
    CrashProb.sector.upper=head(upperandlowerindex.CrashProb.df[2,],-1)
    CrashProb.sector.lower=head(upperandlowerindex.CrashProb.df[3,],-1)
    all.df=cbind.data.frame('CrashProb-sector'=CrashProb.sector,'uppersector-CrashProb'=CrashProb.sector.upper,'lowersector-CrashProb'=CrashProb.sector.lower)
    all.df=as.matrix(all.df)
    upperandlowerindex.all=cbind.data.frame(upperandlowerindex.all,t(all.df))
  }
  return(upperandlowerindex.all)
}

label=colnames(Flow12)
annual.sequence.biweekly=timesequence.kmonth(12,"2001-04-01",14,363)
annual.sequence.monthly=timesequence.kmonth(12,"2001-04-01",30,170)
# annual.sequence=timesequence.kmonth(12,"2001-04-01",50)
pt1=proc.time()
#函数运行的时间比较长，把所有行业遍历完需要耗时20分钟。
allout.biweekly.5percent=weekly.upperandlower.allsector(0.05,annual.sequence.biweekly,label,5)
allout.biweekly.10percent=weekly.upperandlower.allsector(0.10,annual.sequence.biweekly,label,5)
allout.monthly.5percent=weekly.upperandlower.allsector(0.05,annual.sequence.monthly,label,5)
allout.monthly.10percent=weekly.upperandlower.allsector(0.10,annual.sequence.monthly,label,5)
proc.time()-pt1
allout.biweekly.df.5percent=as.data.frame(t(allout.biweekly.5percent))
allout.biweekly.df.10percent=as.data.frame(t(allout.biweekly.10percent))
allout.monthly.df.5percent=as.data.frame(t(allout.monthly.5percent))
allout.monthly.df.10percent=as.data.frame(t(allout.monthly.10percent))
colnames(allout.biweekly.df.5percent)=c("sector","upper","lower")
colnames(allout.biweekly.df.10percent)=c("sector","upper","lower")
colnames(allout.monthly.df.5percent)=c("sector","upper","lower")
colnames(allout.monthly.df.10percent)=c("sector","upper","lower")
allout.biweekly.lm.5percent=lm(sector~lower+upper,data=allout.biweekly.df.5percent)
allout.biweekly.lm.10percent=lm(sector~lower+upper,data=allout.biweely.df.10percent)
allout.monthly.lm.5percent=lm(sector~lower+upper,data=allout.monthly.df.5percent)
allout.monthly.lm.10percent=lm(sector~lower+upper,data=allout.monthly.df.10percent)
summary(allout.biweekly.lm.5percent)
summary(allout.biweekly.lm.10percent)
summary(allout.monthly.lm.5percent)
summary(allout.monthly.lm.10percent)
save.image("Crash-calculation-0610-week-month.RData")