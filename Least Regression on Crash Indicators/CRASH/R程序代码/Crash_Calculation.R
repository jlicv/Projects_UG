#zoo包是为了给时间序列用的。可以把数据框转成zoo类对象，然后用ggfortify包的autoplot函数，作一个图。
library(zoo)
setwd("D://Jason's//刘老师//崩盘-指数和股指期货//数据集")
#找出某个行业的上游和下游各K个行业，并将它们的序列画在一起。
#读取所需的数据：
#行业指数日对数收益率全体
yieldall=read.csv('close-yield-all.csv',check.names = FALSE,row.names=1)
#行业指数周对数收益率全体
weeklyyield=read.csv('weeklyyield.csv',check.names = FALSE,row.names=1)
#行业指数周波动率全体
weeklyvol=read.csv('weekly-vol.csv',check.names = FALSE,row.names=1)
#网络流量
Flow12=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
#A股指数日对数收益率全体
Marketyield=read.csv("万德A股指数-收益率序列-转制为列-时间与行业同步.csv",header=TRUE,check.names='FALSE')

##################################
#####获取每个时间点的统计窗口#####
##################################
#思路是，我先要确定采样点的频率，即多久采一次样，还要决定每一个点采样的区间有多长，最后是从哪里开始采样，采多少个点。
#第一个参数是挑选的区间，例如，我要取以start.sequence的第一个，往后取12个月；
#第二个参数是开始访问的区间，写的格式应该是几年几月几号这样；
#第三个参数是采样点的频率，双周就是14，一个月就是30，例如，我从02年1月1日开始，往前取，取到01年1月1日，下一个点，就是02年2月1日，取到01年2月1日这样；
#第四个是样本点的数量，可以取100个，200个。按需要来，但不要溢出数据本身的范围。
timesequence.kmonth=function(k,binstart,times,numbin){
  library(lubridate)
  #开始的时间点，用ymd转成lubridate的格式。
  binstart=ymd(binstart)
  start.sequence=binstart + days(0:numbin)*times
  #要注意的是，时区里面，若k=12，则是往给定的时间点前推12个月。
  #例如，我这里写的是2002年4月1日，我要做的是，在这个时间点前，推12个月。将这12月的结果纳入结果。
  end.sequence=start.sequence %m+% months(k)
  #把时区的内容去掉。
  start.sequence=gsub("UTC","",start.sequence)
  end.sequence=gsub("UTC","",end.sequence)
  #把取好的区间的结果返回。
  return(cbind(start.sequence,end.sequence))
}
#第一个参数是临界值，是一个百分数，取值范围是(0,1)
#第二个参数是抓取出的大盘的收益率，是一个矩阵。
#第三个参数是抓取出来的要研究的资产的收益率，是一个矩阵，有日期，有对数收益率。
#注意第二个参数和第三个参数的日期要对齐。
Probcalculation=function(threshold,marketyield,givenyield){
  #如果做Frechet变换，由于是Frechet在(0,1)区间上是一个单调函数；
  #也就是说，变换后各个数值的序还是不变的；
  #所以，变换的意义是什么？
  #研究中我们关心的是分位数，还是说百分比。我觉得是百分比。
  #所以，这个变换应该是有意义的。
  #但是，它定义的这个概率，用的是百分数，变换不变换，都不影响这个概率的计算。
  #把得到的收益率序列绑定在一起，得到一个数据框，注意这里用的不是cbind而是cbind.data.frame
  Twoyield=cbind.data.frame('m'=marketyield,'i'=givenyield)
  #再把它命名。
  colnames(Twoyield)=c("m","i")
  #例如说，应该是前10%的，就挑出来前10%的样本。
  #挑出来收益率后10%的样本，并且返回它的行号。
  Marketbelowthreshold=which(Twoyield[,'m']<quantile(Twoyield[,'m'],threshold))
  #类似地，挑出来大盘收益率后10%的样本，返回它的行号。
  Givenbelowthreshold=which(Twoyield[,'i']<quantile(Twoyield[,'i'],threshold))
  #计算行号的交，除与大盘收益率后10%的样本数，得到频率，这个频率就是概率。
  Probabilityconditioned=length(intersect(Marketbelowthreshold,Givenbelowthreshold))/length(Marketbelowthreshold)
  #返回概率。
  return(Probabilityconditioned)  
}
#这部分是为了查看说，每个行业之间随着分位数的变化，条件概率是怎么变化的，继而可以得知，渐进相关性有多大。
#第一个参数是大盘的收益率序列。
#第二个参数是给定的行业的收益率序列。
Changeoverquantile=function(marketyield,givenyield){
  result=c()
  #把百分位数序列生成。
  quantilesequence=seq(1,0.01,-0.01)
  #循环地调用概率计算函数。
  for(i in 1:length(quantilesequence))
  #返回概率序列。
    result[i]=Probcalculation(quantilesequence[i],marketyield,givenyield)
  ret=cbind.data.frame(quantilesequence,result)
  return(ret)
}

#################################
#########获取计算的公式##########
#################################
#第一个Argument是临界值。
#第二个Argument是一个数据框，第一列是开始的时间点，第二列是结束的时间点，每一行对应一个时间点。
#第三个和第四个Argument分别是大盘和需要研究的一个行业指数的日对数收益率序列全体，日期要对齐。
#第五个Argument是流量矩阵的权重，取的是2012年的行业流量矩阵。
#留意这个是特定一个行业的崩盘概率的函数。
CrashProbCount.Oneindex=function(threshold=0.05,datebrackets,yieldseries,marketyield,weightmatrix){
  #先初始化一个记录崩盘概率的矩阵，行名是行业名，列名在下面定义，列明都是日期。
  CrashProb_All=data.frame(row.names = rownames(yieldseries))
  for (i in 1:nrow(datebrackets)){
    #取出对应的时间区间，从开头到结尾的该行业指数的日对数收益率序列。
    YieldGet_Sector=yieldseries[,which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    #假如说在这个时间区间里面，只有一个样本，那么，为了补齐数据方便估计，我们将它和上一周的序列作一个拼接处理。
    if(is.vector(YieldGet_Sector)){
      #这是取上一个区间的该行业指数的日对数收益率序列。
      YieldPrev_Sector=yieldseries[,which(colnames(yieldseries)>=datebrackets[i-1,1] & colnames(yieldseries)<=datebrackets[i-1,2])]
      #lambda的引入是考虑有时候只取到一个样本（例如，国庆黄金周）时候，计算收益率和波动率时的样本数量太少的问题。 
      lambda=1/3
      #再绑起来。
      YieldGet_Sector=cbind.data.frame(lambda*YieldPrev_Sector,(1/lambda)*YieldGet_Sector)
    }
    #再将得到的结果转置一下，变成列，方便绑定。
    YieldGet_Sector=t(YieldGet_Sector)
    #和上面的步骤类似，取大盘的收益率序列。
    YieldGet_Market=marketyield[which(colnames(yieldseries)>=datebrackets[i,1] & colnames(yieldseries)<=datebrackets[i,2])]
    #和Probcalculation函数一模一样。
    CrashProbcalculation=function(threshold,marketyield,givenyield){
      #把得到的收益率序列绑定在一起，得到一个数据框，注意这里用的不是cbind而是cbind.data.frame
      Twoyield=cbind.data.frame('m'=marketyield,'i'=givenyield)
      #再把它命名。
      colnames(Twoyield)=c("m","i")
      #例如说，应该是前10%的，就挑出来前10%的样本。
      #挑出来收益率后10%的样本，并且返回它的行号。
      Marketbelowthreshold=which(Twoyield[,'m']<quantile(Twoyield[,'m'],threshold))
      #类似地，挑出来大盘收益率后10%的样本，返回它的行号。
      Givenbelowthreshold=which(Twoyield[,'i']<quantile(Twoyield[,'i'],threshold))
      #计算行号的交，除与大盘收益率后10%的样本数，得到频率，这个频率就是概率。
      Probabilityconditioned=length(intersect(Marketbelowthreshold,Givenbelowthreshold))/length(Marketbelowthreshold)
      #返回概率。
      return(Probabilityconditioned)  
    }
    #定义完了就调用它。    
    CrashProb_Out=CrashProbcalculation(threshold,YieldGet_Market,YieldGet_Sector)  
    #算出来一个行业的，就把它绑定进去。
    CrashProb_All=cbind.data.frame(CrashProb_All,CrashProb_Out)
  }
  #再重新命名，用的结束点的时间。
  colnames(CrashProb_All)=paste(week(datebrackets[,2]), year(datebrackets[,2]), sep = "-")
  return(CrashProb_All)
}
#这个函数是为了获取上下游行业的名称，以及行业之间的流量的权重。
#第一个参数是上下游各应当包括几个行业。
#第二个是待研究的行业的名称。
#第三个是流量矩阵。
upperandlower.getnames=function(k,sectorname,flow){
  #流量矩阵的对角元设置为0，可以保证我的结果不受自身到自身的流量影响。
  diag(flow)=0
  #将到这个行业的产出流量取出来，然后，排序，按照降序的方式，接着取出头K个。
  #为什么要转置了再取这么麻烦呢？因为R的数据框当中，内容是按照列进行存储的。如果取列，返回的是一个向量。
  #而按行取，则可以取到包括行业名的数据框。所以后面才能够用names把行业指数代号给抓取出来。
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  #换量纲，从万换到亿，并保留一位小数。
  upperstream.topk=round(upperstream.topk/10000,digits=1)
  #得到行业的名字。
  upperk.names=names(upperstream.topk)
  #计算权重，是在头k个行业当中的比例。
  upperweight=upperstream.topk/sum(upperstream.topk)
  upperweight=round(upperweight,digits=3)
  #下游的计算原理同上游，只不过是把行改成了列，不赘述了。
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerstream.topk=round(lowerstream.topk/10000,digits=1)
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  lowerweight=round(lowerweight,digits=3)
  #把名字的结果绑定，输出。
  rbind.data.frame('upper'=upperk.names,'upperflow'=upperstream.topk,'upperprop'=upperweight,'lower'=lowerk.names,'lowerflow'=lowerstream.topk,'lowerprop'=lowerweight)
}
#这个是计算一个行业的上下游的Crash概率的函数。
upperandlowerindex.CrashProb=function(thresholdvalue,interval,k,sectorname,yieldall,marketyieldall,flow){
  
  diag(flow)=0
  sectoryield.matrix=yieldall[sectorname,]
  sectoryield.weekly=CrashProbCount.Oneindex(datebrackets = interval,marketyield = marketyieldall,yieldseries = sectoryield.matrix,weightmatrix = flow,threshold = thresholdvalue)
  upperstream.topk=sort(t(flow)[sectorname,],decreasing = TRUE)[1:k]
  upperk.names=names(upperstream.topk)
  #上游行业权重的计算，计算方法是基于流量比例的大小。
  upperweight=upperstream.topk/sum(upperstream.topk)
  #点乘，得到重组的序列。
  upperyield.matrix = t(data.matrix(upperweight)) %*% data.matrix(yieldall[upperk.names,])
  upperyield.matrix = as.data.frame(upperyield.matrix)
  #得到了上游K个行业的周收益率。
  upperyield.weekly=CrashProbCount.Oneindex(datebrackets = interval,marketyield = marketyieldall,yieldseries = upperyield.matrix,weightmatrix = flow,threshold = thresholdvalue)
  lowerstream.topk=sort(flow[sectorname,],decreasing = TRUE)[1:k]
  lowerk.names=names(lowerstream.topk)
  lowerweight=lowerstream.topk/sum(lowerstream.topk)
  loweryield.matrix = data.matrix(lowerweight) %*% data.matrix(yieldall[lowerk.names,])
  loweryield.matrix = as.data.frame(loweryield.matrix)
  loweryield.weekly=CrashProbCount.Oneindex(datebrackets = interval,marketyield = marketyieldall,yieldseries = loweryield.matrix,weightmatrix = flow,threshold = thresholdvalue)
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
library(zoo)
library(ggfortify)
annual.allout.zoo.5percent=zooreg(annual.allout.df.5precent)
autoplot(annual.allout.zoo.5percent)
