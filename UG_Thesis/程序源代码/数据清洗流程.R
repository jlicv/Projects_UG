#加载的包
library(lubridate)
library(zoo)
#设置数据存放路径
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset/") 
#读取日收益率序列
yieldall=read.csv('close-yield-all.csv',check.names = FALSE,row.names=1)
#读取流量矩阵
Flow12=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
#划分周
timesequence.week=function(binstart,binend,numbin){
  #Figure out the data interval you want to subset.
  binstart=ymd(binstart)
  binend=ymd(binend)
  start.sequence=binstart+weeks(1:numbin)
  end.sequence=binend+weeks(1:numbin)
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}
#指数波动率计算
inner.volatility=function(datebrackets,yieldmatrix.alltime,weightmatrix)
{
  sd.all=list()
  weightmatrix=as.matrix(weightmatrix)
  for (i in 1:nrow(datebrackets)){
    YieldGet=yieldmatrix.alltime[rownames(weightmatrix),which(colnames(yieldmatrix.alltime)>=datebrackets[i,1]&colnames(yieldmatrix.alltime)<=datebrackets[i,2])]
    if(is.vector(YieldGet)){
    YieldPrev=yieldmatrix.alltime[rownames(weightmatrix),which(colnames(yieldmatrix.alltime)>=datebrackets[i-1,1] & colnames(yieldmatrix.alltime)<=datebrackets[i-1,2])]
    lambda=1/3
    YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    close.vol.cov=cov(t(YieldGet))
    deltatrecip=sqrt(240)
    sd.all[[i]]=cbind.data.frame(rownames(close.vol.cov),sqrt(diag(close.vol.cov))*deltatrecip,datebrackets[i,2])
    colnames(sd.all[[i]])=c("Indexname","innervol","date")
  }
  return(sd.all)
}
#计算周收益率序列
YieldCount.Week=function(datebrackets,yieldseries,weightmatrix){
  YieldWeek=data.frame(row.names = rownames(weightmatrix))
  for (i in 1:nrow(datebrackets)){
    YieldGet=yieldseries[rownames(weightmatrix),which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    if(is.vector(YieldGet)){
    YieldPrev=yieldseries[rownames(weightmatrix),which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
    lambda=1/3
    YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    Yieldbrackets=rowSums(YieldGet)
    YieldWeek=cbind.data.frame(YieldWeek,Yieldbrackets)
  }
    colnames(YieldWeek)=paste(week(datebrackets[,1]), year(datebrackets[,1]), sep = "-")
  return(YieldWeek)
}
#

#将数据作一个重排，行名为指数，列名为时间。
paste.prop.bycolumn=function(datalist)
{
  pasteout=data.frame(row.names = rownames(datalist[[1]]))
  pasteout.date=c()
  for (i in 1:length(datalist)){
    pasteout.date[i]=as.character(unique(datalist[[i]]$date))
    pasteout=cbind.data.frame(pasteout,datalist[[i]]$innervol)
  }
  colnames(pasteout)=pasteout.date
  return(pasteout)
}
#这一节是为了得到每个指数每个周的波动率序列
weekly.sequence=timesequence.week("2001-03-28","2001-04-03",780)
weeklyvol=inner.volatility(weekly.sequence,yieldall,Flow12)
vol.weekly.paste=paste.prop.bycolumn(weeklyvol)
weeklyvol.zoo=zooreg(t(vol.weekly.paste),as.yearmon("2001-04"),freq=52)
write.csv(vol.weekly.paste,"weekly-vol.csv")
#对波动率序列进行画图
png('weeklyvol.png', width=8, height=8, units='in', res=600,type="cairo")
plot(weeklyvol.zoo[,'883111.WI'],ylab="收益率序列",xlab="时间",family = 'SimSun')
dev.off()
#这一节是为了得到每个指数的周收益率序列
weeklyyield=YieldCount.Week(datebrackets = weekly.sequence,yieldall,Flow12)
weeklyyield.zoo=zooreg(t(weeklyyield),as.yearmon("2001-04"),freq=52)
write.csv(weeklyyield,"weeklyyield.csv")
#对收益率序列进行画图
png('weeklyyield.png', width=8, height=8, units='in', res=600,type="cairo")
plot(weeklyyield.zoo[,'883111.WI'],ylab="收益率序列",xlab="时间",family = 'SimSun')
dev.off()

topk.names=function(k,sectorname,flow){
  diag(flow)=0
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperstream.topk=round(upperstream.topk/10000,digits=1)
  upperk.names=names(upperstream.topk)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerstream.topk=round(lowerstream.topk/10000,digits=1)
  lowerk.names=names(lowerstream.topk)
  return(rbind.data.frame(t(upperk.names),t(lowerk.names)))
}
sectorquery
topk.names.out=topk.names(5,sectorquery,Flow12)
topk.names.out=t(topk.names.out)
#收益率绘图部分
topk.upper.yield.plus.one=cbind.data.frame(weeklyyield.zoo[,sectorquery],weeklyyield.zoo[,topk.names.out[,1]])
colnames(topk.upper.yield.plus.one)[1]=sectorquery
top.upper.yield.zoo=zooreg(topk.upper.yield.plus.one,as.yearmon("2001-04"),freq=52)
png('topk-upper-plus-one-weeklyyield-combined.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.upper.yield.zoo,facets=FALSE)
dev.off()
png('topk-upper-plus-one-weeklyyield-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.upper.yield.zoo)
dev.off()

topk.lower.yield.plus.one=cbind.data.frame(weeklyyield.zoo[,sectorquery],weeklyyield.zoo[,topk.names.out[,2]])
colnames(topk.lower.yield.plus.one)[1]=sectorquery
top.lower.yield.zoo=zooreg(topk.lower.yield.plus.one,as.yearmon("2001-04"),freq=52)
png('topk-lower-plus-one-weeklyyield-combined.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.lower.yield.zoo,facets=FALSE)
dev.off()
png('topk-lower-plus-one-weeklyyield-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.lower.yield.zoo)
dev.off()

#波动率绘图部分
topk.upper.vol.plus.one=cbind.data.frame(weeklyvol.zoo[,sectorquery],weeklyyield.zoo[,topk.names.out[,1]])
colnames(topk.upper.vol.plus.one)[1]=sectorquery
top.upper.vol.zoo=zooreg(topk.upper.vol.plus.one,as.yearmon("2001-04"),freq=52)
png('topk-upper-plus-one-weeklyvol-combined.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.upper.vol.zoo,facets=FALSE)
dev.off()
png('topk-upper-plus-one-weeklyvol-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.upper.zoo)
dev.off()

topk.lower.vol.plus.one=cbind.data.frame(weeklyvol.zoo[,sectorquery],weeklyyield.zoo[,topk.names.out[,2]])
colnames(topk.lower.vol.plus.one)[1]=sectorquery
top.lower.vol.zoo=zooreg(topk.lower.vol.plus.one,as.yearmon("2001-04"),freq=52)
png('topk-lower-plus-one-weeklyvol-combined.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.lower.vol.zoo,facets=FALSE)
dev.off()
png('topk-lower-plus-one-weeklyvol-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(top.lower.vol.zoo)
dev.off()

