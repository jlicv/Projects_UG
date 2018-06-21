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
  library(lubridate)
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
  library(lubridate)
  binstart=ymd(binstart)
  binend=ymd(binend)
  datejump=seq(from=1,to=numbin,by=k)
  start.sequence=binstart+weeks(datejump)
  end.sequence=binend+weeks(datejump)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
}

timesequence.kmonth=function(k,binstart,binend,numbin){
  library(lubridate)
  binstart=ymd(binstart)
  binend=ymd(binend)
  datejump=seq(from=1,to=numbin,by=k)
  start.sequence=binstart %m+% months(datejump)
  end.sequence=binend %m+% months(datejump)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  return(cbind(start.sequence,end.sequence))
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

NCSKEWCount.Oneindex.Week=function(datebrackets,yieldseries,weightmatrix){
  NCSKEW_Week=data.frame(row.names = rownames(yieldseries))
  for (i in 1:nrow(datebrackets)){
    library(moments)
    YieldGet=yieldseries[,which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    if(is.vector(YieldGet)){
      YieldPrev=yieldseries[,which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
      #lambda的引入是考虑有时候只取到一个样本（例如，国庆黄金周）时候，计算收益率和波动率时的样本数量太少的问题。 
      lambda=1/3
      YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    #对数收益率可以直接加和。得到周收益率。
    n1=ncol(YieldGet)
    YieldGet=t(YieldGet)
    if(n1>2)
      NCSKEWbrackets=(-1)*moment(YieldGet,3)*n1*(n1-1)^(3/2)/((n1-1)*(n1-2)*moment(YieldGet,2)^(3/2))
    if(n1<=2)
      NCSKEWbrackets=NA
    NCSKEW_Week=cbind.data.frame(NCSKEW_Week,NCSKEWbrackets)
  }
  colnames(NCSKEW_Week)=paste(week(datebrackets[,1]), year(datebrackets[,1]), sep = "-")
  return(NCSKEW_Week)
}

DUVOLCount.Oneindex.Week=function(datebrackets,yieldseries,weightmatrix){
  DUVOL_Week=data.frame(row.names = rownames(yieldseries))
  for (i in 1:nrow(datebrackets)){
    library(moments)
    YieldGet=yieldseries[,which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2]),drop=FALSE]
    if(ncol(YieldGet)<2){
      YieldPrev=yieldseries[,which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
      #lambda的引入是考虑有时候只取到一个样本（例如，国庆黄金周）时候，计算收益率和波动率时的样本数量太少的问题。 
      lambda=1/3
      if(ncol(YieldGet)==0)
        YieldGet=YieldPrev
      if(ncol(YieldGet)>0)
        YieldGet=cbind.data.frame(lambda*YieldPrev,(1/lambda)*YieldGet)
    }
    #对数收益率可以直接加和。得到周收益率。
    positive=YieldGet[1,which(YieldGet>0),drop=FALSE]
    negative=YieldGet[1,which(YieldGet<=0),drop=FALSE]
    np=ncol(positive)
    nn=ncol(negative)
    if(np>1)
      positive=t(positive)
    if(nn>1)
      negative=t(negative)
    if(np <= 1 || nn<=1 )
      DUVOLbrackets=NA
    if(np >1 & nn >1)
      DUVOLbrackets=log(moment(negative,2)*(np-1)/((nn-1)*moment(positive,2)))
    DUVOL_Week=cbind.data.frame(DUVOL_Week,DUVOLbrackets)
  }
  colnames(DUVOL_Week)=paste(week(datebrackets[,1]), year(datebrackets[,1]), sep = "-")
  return(DUVOL_Week)
}


upperandlowerindex.NCSKEW=function(interval,k,sectorname,yieldall,flow){
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  sectoryield.weekly=NCSKEWCount.Oneindex.Week(datebrackets = interval,sectoryield.matrix,flow)
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperk.names=names(upperstream.topk)
  #上游行业权重的计算，计算方法是基于流量比例的大小。
  upperweight=upperstream.topk/sum(upperstream.topk)
  #点乘，得到重组的序列。
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[upperk.names,])
  upperyield.matrix = as.data.frame(upperyield.matrix)
  #得到了上游K个行业的周收益率。
  upperyield.weekly=NCSKEWCount.Oneindex.Week(datebrackets = interval,upperyield.matrix,flow)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowerk.names,])
  loweryield.matrix = as.data.frame(loweryield.matrix)
  loweryield.weekly=NCSKEWCount.Oneindex.Week(datebrackets = interval,loweryield.matrix,flow)
  combinedyield.weekly=rbind.data.frame(sectoryield.weekly,upperyield.weekly,loweryield.weekly)
  uppernames=paste(sectorname,'upper',k,sep='-')
  lowernames=paste(sectorname,'lower',k,sep='-')
  rownames(combinedyield.weekly)=c(sectorname,uppernames,lowernames)
  return(combinedyield.weekly)
}

upperandlowerindex.DUVOL=function(interval,k,sectorname,yieldall,flow){
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  sectoryield.weekly=DUVOLCount.Oneindex.Week(datebrackets = interval,sectoryield.matrix,flow)
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperk.names=names(upperstream.topk)
  #上游行业权重的计算，计算方法是基于流量比例的大小。
  upperweight=upperstream.topk/sum(upperstream.topk)
  #点乘，得到重组的序列。
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[upperk.names,])
  upperyield.matrix = as.data.frame(upperyield.matrix)
  #得到了上游K个行业的周收益率。
  upperyield.weekly=DUVOLCount.Oneindex.Week(datebrackets = interval,upperyield.matrix,flow)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowerk.names,])
  loweryield.matrix = as.data.frame(loweryield.matrix)
  loweryield.weekly=DUVOLCount.Oneindex.Week(datebrackets = interval,loweryield.matrix,flow)
  combinedyield.weekly=rbind.data.frame(sectoryield.weekly,upperyield.weekly,loweryield.weekly)
  uppernames=paste(sectorname,'upper',k,sep='-')
  lowernames=paste(sectorname,'lower',k,sep='-')
  rownames(combinedyield.weekly)=c(sectorname,uppernames,lowernames)
  return(combinedyield.weekly)
}


weekly.upperandlower.allsector=function(interval,label,k){
  upperandlowerindex.all=data.frame(row.names=c("sector-yield",paste("upper-top-yield",k),paste("lower-top-yield",k),"sector-vol",paste("upper-top-vol",k),paste("lower-top-vol",k)))
  for(i in 1:length(label)){
    sectorquery=label[i]
    topksectornames=upperandlower.getnames(k,sectorquery,Flow12)
    colnames(topksectornames)=c(1:k)
    upperandlowerindex.DUVOL.df=upperandlowerindex.DUVOL(interval,k,sectorquery,yieldall,Flow12)
    upperandlowerindex.NCSKEW.df=upperandlowerindex.NCSKEW(interval,k,sectorquery,yieldall,Flow12)
    # upperandlower.weeklyvol.paste=paste.prop.bycolumn(upperandlower.weeklyvol)
    upperandlowerindex.DUVOL.df=as.matrix(upperandlowerindex.DUVOL.df)
    upperandlowerindex.NCSKEW.df=as.matrix(upperandlowerindex.NCSKEW.df)
    #对序列作延滞的处理，相当于分别去头和去尾，再组合在一起。
    NCSKEW.sector=tail(upperandlowerindex.NCSKEW.df[1,],-1)
    NCSKEW.sector.upper=head(upperandlowerindex.NCSKEW.df[2,],-1)
    NCSKEW.sector.lower=head(upperandlowerindex.NCSKEW.df[3,],-1)
    DUVOL.sector=tail(upperandlowerindex.DUVOL.df[1,],-1)
    DUVOL.sector.upper=head(upperandlowerindex.DUVOL.df[2,],-1)
    DUVOL.sector.lower=head(upperandlowerindex.DUVOL.df[3,],-1)
    # vol.sector=data.matrix(upperandlower.weeklyvol.paste[1,-1])
    # vol.sector.upper=data.matrix(upperandlower.weeklyvol.paste[2,-ncol(upperandlower.weeklyvol.paste)])
    # vol.sector.lower=data.matrix(upperandlower.weeklyvol.paste[3,-ncol(upperandlower.weeklyvol.paste)])
    # vol.df=cbind.data.frame('volsector'=t(vol.sector),'uppersector-vol'=t(vol.sector.upper),'lowersector-vol'=t(vol.sector.lower))
    all.df=cbind.data.frame('NCSKEW-sector'=NCSKEW.sector,'uppersector-NCSKEW'=NCSKEW.sector.upper,'lowersector-NCSKEW'=NCSKEW.sector.lower,'DUVOL-sector'=DUVOL.sector,'uppersector-DUVOL'=DUVOL.sector.upper,'lowersector-DUVOL'=DUVOL.sector.lower)
    all.df=as.matrix(all.df)
    upperandlowerindex.all=cbind.data.frame(upperandlowerindex.all,t(all.df))
  }
  return(upperandlowerindex.all)
}


label=colnames(Flow12)
# weekly.sequence=timesequence.week("2001-03-28","2001-04-03",780)
biweekly.sequence=timesequence.kweek(2,"2001-03-28","2001-04-10",780)
monthly.sequence=timesequence.kmonth(1,"2001-02-28","2001-03-27",180)
pt1=proc.time()
#函数运行的时间比较长，把所有行业遍历完需要耗时20分钟。
weekly.allout=weekly.upperandlower.allsector(biweekly.sequence,label,5)
monthly.allout=weekly.upperandlower.allsector(monthly.sequence,label,5)
proc.time()-pt1
weekly.allout.df=weekly.allout
weekly.allout=as.matrix(weekly.allout.df)
monthly.allout.df=monthly.allout
monthly.allout=as.matrix(monthly.allout.df)

# weekly.allout=weekly.allout[which(is.infinite(weekly.allout),arr.ind = TRUE)]=0
# monthly.allout=monthly.allout[which(is.infinite(monthly.allout),arr.ind = TRUE)]=0

rownames(weekly.allout)=c("NCSKEW","NCSKEW_u","NCSKEW_l","DUVOL","DUVOL_u","DUVOL_l")
rownames(monthly.allout)=c("NCSKEW","NCSKEW_u","NCSKEW_l","DUVOL","DUVOL_u","DUVOL_l")
NCSKEW.weekly.lm=lm(NCSKEW~NCSKEW_u+NCSKEW_l,data=as.data.frame(t(weekly.allout)));summary(NCSKEW.weekly.lm)
NCSKEW.monthly.lm=lm(NCSKEW~NCSKEW_u+NCSKEW_l,data=as.data.frame(t(monthly.allout)));summary(NCSKEW.monthly.lm)

#DUVOL里面有很多是NA的。
allna=weekly.allout['DUVOL',which(is.na(weekly.allout['DUVOL',]))]
length(allna)
allna_u=weekly.allout['DUVOL_u',which(is.na(weekly.allout['DUVOL_u',]))]
length(allna_u)
allna_l=weekly.allout['DUVOL_u',which(is.na(weekly.allout['DUVOL_u',]))]
length(allna_l)
weekly.allout['DUVOL_u',which(is.na(weekly.allout['DUVOL_u',]))]=0
weekly.allout['DUVOL_u',which(is.na(weekly.allout['DUVOL_u',]))]=0
weekly.allout['DUVOL',which(is.infinite(t(weekly.allout)[,'DUVOL'])==TRUE)]=0
DUVOL.weekly.lm=lm(DUVOL~DUVOL_u+DUVOL_l,data=as.data.frame(t(weekly.allout)));summary(DUVOL.weekly.lm)
monthly.allout['DUVOL_u',which(is.na(monthly.allout['DUVOL_u',]))]=0
monthly.allout['DUVOL_u',which(is.na(monthly.allout['DUVOL_u',]))]=0
monthly.allout['DUVOL',which(is.infinite(t(monthly.allout)[,'DUVOL'])==TRUE)]=0
DUVOL.monthly.lm=lm(DUVOL~DUVOL_u+DUVOL_l,data=as.data.frame(t(monthly.allout)));summary(DUVOL.monthly.lm)
monthly.allout.zoo=zooreg(t(monthly.allout))
#残差部分也给画出来。
NCSKEW.lm.residual=residuals(NCSKEW.lm.all)
NCSKEW.lm.residual.zoo=zooreg(NCSKEW.weekly.lm)
png('weeklyNCSKEW-residual-all.png', width=6, height=6, units='in', res=600,type="cairo")
#我感觉残差部分有一些地方的值会突然升高，也和一些行业里面，
#囊括的指数太少，而这些行业在某些阶段被游资炒作，根本就背离了行业关联。
#导致指数的走势也背离了行业间关联。
#例如说，证监会研究实验发展，883177.WI。
autoplot(NCSKEW.lm.residual.zoo)
dev.off()
