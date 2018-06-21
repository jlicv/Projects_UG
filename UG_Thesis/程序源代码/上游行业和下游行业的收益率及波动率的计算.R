#加载需要的包，如果没有安装上述的包，请用install.packages(c("package name))的方式安装.
library(zoo)
library(lubridate)
library(stargazer)
library(plm)
library(lmtest)
#设置工作目录，按需要更改。
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset")
#找出某个行业的上游和下游各K个行业，并将它们的序列画在一起。
#读取所需的数据：
yieldall=read.csv('close-yield-all.csv',check.names = FALSE,row.names=1)
weeklyyield=read.csv('weeklyyield.csv',check.names = FALSE,row.names=1)
weeklyvol=read.csv('weekly-vol.csv',check.names = FALSE,row.names=1)
Flow12=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
label=colnames(Flow12)
#按照数据频次，例如，频次为周，则确定每个周里，包含的日期的天数。
timesequence.week=function(binstart,binend,numbin){
  binstart=ymd(binstart)
  binend=ymd(binend)
  start.sequence=binstart+weeks(1:numbin)
  end.sequence=binend+weeks(1:numbin)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}
timesequence.kweek=function(k,binstart,binend,numbin){
  binstart=ymd(binstart)
  binend=ymd(binend)
  datejump=seq(1:numbin,k)
  start.sequence=binstart+weeks(datejump)
  end.sequence=binend+weeks(datejump)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}

timesequence.kmonth=function(k,binstart,binend,numbin){
  binstart=ymd(binstart)
  binend=ymd(binend)
  datejump=seq(1:numbin,by=k)
  start.sequence=binstart+months(datejump)
  end.sequence=binend+months(datejump)
  #把时区的内容去掉。
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
    #对数收益率可以直接加和。得到周收益率。
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
  #得到了上游K个行业的周收益率。
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
    #这是一个年化系数
    deltatrecip=sqrt(240)
    sd.all[[i]]=cbind.data.frame(rownames(close.vol.cov),sqrt(diag(close.vol.cov))*deltatrecip,datebrackets[i,2])
    colnames(sd.all[[i]])=c("Indexname","innervol","date")
  }
  return(sd.all)
}
#这是计算一个行业的K个上游行业和K个下游行业的周波动率序列。
#k,sectorname,yieldall,flow的含义与前面相同。
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
#这是计算全体行业各自的上游行业和下游行业的波动率和收益率的序列的函数。
weekly.upperandlower.allsector=function(label,k){
  upperandlowerindex.all=data.frame(row.names=c("sector-yield",paste("upper-top-yield",k),paste("lower-top-yield",k),"sector-vol",paste("upper-top-vol",k),paste("lower-top-vol",k)))
  for(i in 1:length(label)){
    sectorquery=label[i]
    topksectornames=upperandlower.getnames(k,sectorquery,Flow12)
    colnames(topksectornames)=c(1:k)
    upperandlowerindex.yield.df=upperandlowerindex.yield(k,sectorquery,yieldall,Flow12)
    upperandlower.weeklyvol=index.upperandlower.weeklyvol(k,sectorquery,yieldall,Flow12)
    upperandlower.weeklyvol.paste=paste.prop.bycolumn(upperandlower.weeklyvol)
    upperandlowerindex.yield.df=as.matrix(upperandlowerindex.yield.df)
    #对序列作延滞的处理，相当于分别去头和去尾，再组合在一起。
    yield.sector=tail(upperandlowerindex.yield.df[1,],-1)
    yield.sector.upper=head(upperandlowerindex.yield.df[2,],-1)
    yield.sector.lower=head(upperandlowerindex.yield.df[3,],-1)
    vol.sector=data.matrix(upperandlower.weeklyvol.paste[1,-1])
    vol.sector.upper=data.matrix(upperandlower.weeklyvol.paste[2,-ncol(upperandlower.weeklyvol.paste)])
    vol.sector.lower=data.matrix(upperandlower.weeklyvol.paste[3,-ncol(upperandlower.weeklyvol.paste)])
    vol.df=cbind.data.frame('volsector'=t(vol.sector),'uppersector-vol'=t(vol.sector.upper),'lowersector-vol'=t(vol.sector.lower))
    all.df=cbind.data.frame('yieldsector'=yield.sector,'uppersector-yield'=yield.sector.upper,'lowersector-yield'=yield.sector.lower,vol.df)
    all.df=as.matrix(all.df)
    upperandlowerindex.all=cbind.data.frame(upperandlowerindex.all,t(all.df))
  }
  return(upperandlowerindex.all)
}
#获取所有行业的名字。
label=colnames(Flow12)
weekly.sequence=timesequence.week("2001-03-28","2001-04-03",780)
pt1=proc.time()
#函数运行的时间比较长，把所有行业遍历完需要耗时20分钟。
weeklyyield.allout=weekly.upperandlower.allsector(label,10)
proc.time()-pt1
weeklyyield.allout.df=weeklyyield.allout
weeklyyield.allout=as.matrix(weeklyyield.allout)
#收益率回归的结果。
yield.lm.all=lm(weeklyyield.allout[1,]~weeklyyield.allout[2,]+weeklyyield.allout[3,]);summary(yield.lm.all)
#残差部分也给画出来。
yield.lm.residual=residuals(yield.lm.all)
yield.lm.residual.zoo=zooreg(yield.lm.residual)
png('weeklyyield-residual-all.png', width=6, height=6, units='in', res=600,type="cairo")
#我感觉残差部分有一些地方的值会突然升高，也和一些行业里面，
#囊括的指数太少，而这些行业在某些阶段被游资炒作，根本就背离了行业关联。
#导致指数的走势也背离了行业间关联。
#例如说，证监会研究实验发展，883177.WI。
autoplot(yield.lm.residual.zoo)
dev.off()

#波动率回归的结果
vol.lm.all=lm(weeklyyield.allout[4,]~weeklyyield.allout[5,]+weeklyyield.allout[6,]);summary(vol.lm.all)
#残差部分也给你画出来。
vol.lm.residual=residuals(vol.lm.all)
vol.lm.residual.zoo=zooreg(vol.lm.residual)
png('weeklyvol-residual-all.png', width=6, height=6, units='in', res=600,type="cairo")
autoplot(vol.lm.residual.zoo)
dev.off()

#以下是部分行业的结果
#在这里填写要查询的行业的交易代号
sectorquery='883111.WI'
#查询的行业的深度，即包括的企业的数目。
k=10
#开始查询。
#对于每个行业，取K个上游和K个下游的行业。
topksectornames=upperandlower.getnames(k,sectorquery,Flow12)
colnames(topksectornames)=c(1:k)
upperandlowerindex.yield.df=upperandlowerindex.yield(k,sectorquery,yieldall,Flow12)
upperandlowerindex.yield.zoo=zooreg(t(upperandlowerindex.yield.df),as.yearmon("2001-04"),freq=52)

upperandlower.weeklyvol=index.upperandlower.weeklyvol(k,sectorquery,yieldall,Flow12)
upperandlower.weeklyvol.paste=paste.prop.bycolumn(upperandlower.weeklyvol)
upperandlower.weeklyvol.zoo=zooreg(t(upperandlower.weeklyvol.paste),as.yearmon("2001-04"),freq=52)

upperandlowerindex.yield.df=as.matrix(upperandlowerindex.yield.df)
#对序列作延滞的处理，相当于分别去头和去尾，再组合在一起。
yield.sector=tail(upperandlowerindex.yield.df[1,],-1)
yield.sector.upper=head(upperandlowerindex.yield.df[2,],-1)
yield.sector.lower=head(upperandlowerindex.yield.df[3,],-1)
yield.df=cbind.data.frame('yieldsector'=yield.sector,'uppersector'=yield.sector.upper,'lowersector'=yield.sector.lower)
#收益率回归的结果。
yield.lm.another=lm(upperandlowerindex.yield.df[1,]~upperandlowerindex.yield.df[2,]+upperandlowerindex.yield.df[3,]);summary(yield.lm.another)
#将上游和下游的波动率序列变化为矩阵，方便后续的处理。
upperandlower.weeklyvol.paste=as.matrix(upperandlower.weeklyvol.paste)
#分别去头和去尾，作延滞的处理。
vol.sector.upper=head(upperandlower.weeklyvol.paste[2,],-1)
vol.sector.lower=head(upperandlower.weeklyvol.paste[3,],-1)
vol.sector=tail(upperandlower.weeklyvol.paste[1,],-1)
vol.df=cbind.data.frame('volsector'=vol.sector,'uppersector'=vol.sector.upper,'lowersector'=vol.sector.lower)
upperandlower.vol.matrix=as.matrix(vol.df)
vol.all.sector.df=cbind.data.frame(upperandlower.vol.matrix)
#再做回归
vol.lm=lm(volsector~.,data=vol.df);summary(vol.lm)
#探究二阶的回归的影响。思路与上面的例子是相同的。
vol.sector=tail(upperandlower.weeklyvol.paste[1,],-2)
vol.sector.upper.1=head(upperandlower.weeklyvol.paste[2,],-1)
vol.sector.upper.1=tail(vol.sector.upper.1,-1)
vol.sector.upper.2=head(upperandlower.weeklyvol.paste[2,],-2)
vol.sector.lower.1=head(upperandlower.weeklyvol.paste[3,],-1)
vol.sector.lower.1=tail(vol.sector.lower.1,-1)
vol.sector.lower.2=head(upperandlower.weeklyvol.paste[3,],-2)
#重组的序列。
vol.df.2=cbind.data.frame('volsector'=vol.sector,'uppersector.1'=vol.sector.upper.1,'uppersector.2'=vol.sector.upper.2,'lowersector.1'=vol.sector.lower.1,'lowersector.2'=vol.sector.lower.2)
vol.df.2.another=as.data.frame(t(vol.df.2))
vol.lm.2=lm(volsector~.,data=vol.df.2);summary(vol.lm.2)
vol.lm.2=lm(volsector~.,data=vol.df.2.another);summary(vol.lm.2.another)
#这些是一些图像的包。做三维的可展示的部分。
library(scatterplot3d)
library(car)
library(rgl)
upperandlower.yield.matrix=as.matrix(yield.df)
upperandlower.yield.matrix=t(upperandlower.yield.matrix)
scatter3d(upperandlower.yield.matrix[,1],upperandlower.yield.matrix[,2],upperandlower.yield.matrix[,3],surface.alpha = 0.5,xlab="Sector",ylab="Upper sector",zlab="Lower sector",main="Weekly Yield Regression")
stargazer(yield.lm)
stargazer(vol.lm)
stargazer(vol.lm.2)
