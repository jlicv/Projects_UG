#加载需要的包，如果没有安装上述的包，请用install.packages(c("package name))的方式安装.
library(zoo)
library(lubridate)
#设置工作目录，按需要更改。
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset") 
#找出某个行业的上游和下游各K个行业，并将它们的序列画在一起。
yieldall=read.csv('close-yield-all.csv',check.names = FALSE,row.names=1)
weeklyyield=read.csv('weeklyyield.csv',check.names = FALSE,row.names=1)
weeklyvol=read.csv('weekly-vol.csv',check.names = FALSE,row.names=1)
Flow12=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
#按照数据频次，例如，频次为周，则确定每个周里，包含的日期的天数。
timesequence.week=function(binstart,binend,numbin){
  binstart=ymd(binstart)
  binend=ymd(binend)
  start.sequence=binstart+weeks(1:numbin)
  end.sequence=binend+weeks(1:numbin)
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}
#计算单个序列的周收益率。
YieldCount.Oneindex.Week=function(datebrackets,yieldseries,weightmatrix){
  YieldWeek=data.frame(row.names = rownames(yieldseries))
  for (i in 1:nrow(datebrackets)){
    YieldGet=yieldseries[,which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    if(is.vector(YieldGet)){
      YieldPrev=yieldseries[,which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
      #lambda的引入是考虑有时候只取到一个样本（例如，国庆黄金周）时候，计算收益率和波动率时的样本数量太少的问题。 
      lambda=1/3
      YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    Yieldbrackets=rowSums(YieldGet)
    YieldWeek=cbind.data.frame(YieldWeek,Yieldbrackets)
  }
  colnames(YieldWeek)=paste(week(datebrackets[,1]), year(datebrackets[,1]), sep = "-")
  return(YieldWeek)
}
#这个函数是为了取上游和下游的K个行业在行业指数流量表当中的名称以及流量。
#k代表要取的行业的数目，sectorname代表待研究的行业的名称，是该行业对应的行业指数的交易代号。
#flow是行业指数流量表。
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
#这个是为了取上游和下游K个行业的收益率序列的。
#k,sectorname,flow的含义同上面的函数。
#yieldall是所有行业指数的日对数收益率序列。

upperandlowerindex.yield=function(k,sectorname,yieldall,flow){
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  sectoryield.weekly=YieldCount.Oneindex.Week(datebrackets = weekly.sequence,sectoryield.matrix,flow)
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperk.names=names(upperstream.topk)
  #上游行业权重的计算，计算方法是基于流量比例的大小。
  upperweight=upperstream.topk/sum(upperstream.topk)
  #点乘，得到重组的序列。
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[upperk.names,]) 
  upperyield.matrix = as.data.frame(upperyield.matrix)
  #得到了上游K个行业的周收益率
  upperyield.weekly=YieldCount.Oneindex.Week(datebrackets = weekly.sequence,upperyield.matrix,flow)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowerk.names,]) 
  loweryield.matrix = as.data.frame(loweryield.matrix)
  loweryield.weekly=YieldCount.Oneindex.Week(datebrackets = weekly.sequence,loweryield.matrix,flow)
  combinedyield.weekly=rbind.data.frame(sectoryield.weekly,upperyield.weekly,loweryield.weekly)
  uppernames=paste(sectorname,'upper',k,sep='-')
  lowernames=paste(sectorname,'lower',k,sep='-')
  rownames(combinedyield.weekly)=c(sectorname,uppernames,lowernames)
  return(combinedyield.weekly)
}
#计算一个行业的周波动率序列。
#datebrackets是前面timesequence.week的输出，在这里作为取周数据的间隔点，传入。
#yieldmatrix.alltime是所有证监会行业指数的日对数收益率矩阵。
#weightmatrix是行业指数的中间流量表。他是通过对投入产出表作转换得到的。
inner.volatility.another=function(datebrackets,yieldmatrix.alltime,weightmatrix){
  sd.all=list()
  #对角线取0，因为这样子做可以确保取不到行业自身。
  diag(weightmatrix)=0
  weightmatrix=as.matrix(weightmatrix)
  for (i in 1:nrow(datebrackets)){
    #得到一个时间点的收益率序列，作循环。
    YieldGet=yieldmatrix.alltime[,which(colnames(yieldmatrix.alltime)>=datebrackets[i,1]&colnames(yieldmatrix.alltime)<=datebrackets[i,2])]
    if(is.vector(YieldGet)){
      YieldPrev=yieldmatrix.alltime[,which(colnames(yieldmatrix.alltime)>=datebrackets[i-1,1] & colnames(yieldmatrix.alltime)<=datebrackets[i-1,2])]
      lambda=1/3
      YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    #先去了收益率，再去计算波动率，这里取协方差以后中的对角元，就是方差。
    #转置是因为R语言中数据框是以列的形式组合的向量，转置以后它在明白变量是行业间的方差，而不是时间的方差。
    close.vol.cov=cov(t(YieldGet))
    deltatrecip=sqrt(240)
    sd.all[[i]]=cbind.data.frame(rownames(close.vol.cov),sqrt(diag(close.vol.cov))*deltatrecip,datebrackets[i,2])
    colnames(sd.all[[i]])=c("Indexname","innervol","date")
  }
  return(sd.all)
}
#取上游行业和下游行业的周收益率。
#k,sectorname,yieldall,flow的含义同上面函数。
index.upperandlower.weeklyvol=function(k,sectorname,yieldall,flow){
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  uppernames=names(upperstream.topk)
  upperweight=upperstream.topk/sum(upperstream.topk)
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[uppernames,])
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowernames=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowernames,]) 
  uppernames=paste(sectorname,'upper',k,sep='-')
  lowernames=paste(sectorname,'lower',k,sep='-')
  combinedkyield=rbind.data.frame(sectoryield.matrix,upperyield.matrix,loweryield.matrix)
  rownames(combinedkyield)=c(sectorname,uppernames,lowernames)
  #为了计算波动率，先处理好上游行业和下游行业的收益率，与yield结合在一起，得到组合好的收益率序列，再传进去，给他计算波动率。
  combined.vol.weekly=inner.volatility.another(weekly.sequence,combinedkyield,flow)
}

library(stargazer)
#填写待查询的行业的交易代号。
sectorquery='883111.WI'
#找出头K个行业的名称，流量，将它写入到topksectornames里面。
topksectornames=upperandlower.getnames(5,sectorquery,Flow12)
colnames(topksectornames)=c(1:5)
#再将这个结果转化为LATEX的代码。
stargazer(topksectornames[,1:5],summary=FALSE)
#取上游和下游最相关的行业的周收益率序列。
upperandlowerindex.yield.df=upperandlowerindex.yield(5,sectorquery,yieldall,Flow12)
#再将它转化为R语言中时间序列，R语言中的时间序列用zoo类比较好，因为它很灵活。
upperandlowerindex.yield.zoo=zooreg(t(upperandlowerindex.yield.df),as.yearmon("2001-04"),freq=52)
#对于波动率序列，也是同理。
upperandlower.weeklyvol=index.upperandlower.weeklyvol(5,sectorquery,yieldall,Flow12)
upperandlower.weeklyvol.paste=paste.prop.bycolumn(upperandlower.weeklyvol)
upperandlower.weeklyvol.zoo=zooreg(t(upperandlower.weeklyvol.paste),as.yearmon("2001-04"),freq=52)

#这一部分是把输出结果绘制成图片的。
library(ggfortify)
png('weeklyyield-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(upperandlowerindex.yield.zoo)
dev.off()
png('weeklyvol-single.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(upperandlower.weeklyvol.zoo)
dev.off()
png('weeklyyield-combined.png', width=6, height=6, units='in', res=600,type="cairo")
#Facets这个选项是把多个数据输出到同一张图里面，而不是一张图里多个子图。
autoplot(upperandlowerindex.yield.zoo, facets = FALSE)
dev.off()
png('weeklyvol-combined.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(upperandlower.weeklyvol.zoo,facets=FALSE)
dev.off()
