#输入收益率序列，输入网络权重。
#输出回归分析序列。
#调用zoo时间序列，对上下游行业进行关联分析。
library(zoo)
library(plyr)
#读取数据，处理，时间序列集合。
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset/") 
myyield=read.csv("yield-12-bymonth-01to16.csv",header=TRUE,check.names='FALSE',row.names=1)
myyield=as.matrix(myyield)
monthyield.zoo=zooreg(t(myyield),as.yearmon("2001-06"),freq=12)
#读取投入产出的网络流量，行业标签。
Flow12.re=read.csv("Flow12-re.csv",header=TRUE,check.names='FALSE',row.names=1)
label=unique(rownames(Flow12.re))
industries.whole=data.frame()
#初始化完成，开始处理收益率序列。
topsectors.yield=function(k){
  for (i in 1:length(label)){
    #将上游产业中最大的K个找出来，然后编号。
    Insector.Num=which(Flow12.re[,i] %in% sort(Flow12.re[,i],decreasing = TRUE)[1:k])
    if(length(which(Insector.Num==i))!=0)
    Insector.Num=Insector.Num[-which(Insector.Num==i)]
    #将下游产业中最大的K个给它找出，然后编号。
    Outsector.Num=which(Flow12.re[i,] %in% sort(Flow12.re[i,],decreasing = TRUE)[1:k])
    if(length(which(Outsector.Num==i))!=0)
    Outsector.Num=Outsector.Num[-which(Outsector.Num==i)]
    #把Names取出来是为了方便取上下游关联行业的收益率序列
    Insector.Names=rownames(Flow12.re)[Insector.Num]
    Outsector.Names=rownames(Flow12.re)[Outsector.Num]
    Insector.yield.zoo=monthyield.zoo[,Insector.Names]
    #取出收益率序列以后，再对他们分别重命名。
    #重命名是一个折衷的办法。尽管会丢失变量名的信息，
    #但重命名后，合并起来的列丰富了样本数。
    colnames(Insector.yield.zoo)=paste('i',1:length(Insector.Num),sep="")
    #操作同理。
    Outsector.yield.zoo=monthyield.zoo[,Outsector.Names]
    colnames(Outsector.yield.zoo)=paste('o',1:length(Outsector.Num),sep="")
    #确定取滞后多少阶，在这里暂时定位1阶，也可以按需要处理。
    Insector.lag=lag(Insector.yield.zoo,1)
    Outsector.lag=lag(Outsector.yield.zoo,1)
    #取完滞后阶以后对齐数据。也就是要把头尾分别掐掉一些，
    Insector.lag.trimhead=head(Insector.lag,-1)
    Outsector.lag.trimhead=head(Outsector.lag,-1)
    yield.trimtail=tail(monthyield.zoo[,i],-1)
    #这里取一个Index是为了制作标签，标签用于面板数据的数据框的建造。
    Index=rep(label[i],times=length(yield.trimtail))
    #将时间，本行业的收益率，上下游收益率，标签绑在一起
    industries.df=cbind.data.frame("time"=slot(yield.trimtail,"names"),yield.trimtail,Insector.lag.trimhead,Outsector.lag.trimhead,Index)
    #每取一个行业，就把它绑在一起。这里用rbind.fill可以适应不定长度的列。
    #但这也引发了一个后面剪去空白列的需求。
    industries.whole=rbind.fill(industries.df,industries.whole)
  }
  return(industries.whole)
}
#好像10个的时候解释力更强一些。
k=5
temp=topsectors.yield(k)
#去掉多余的空白列。
temp=temp[,1:(2*k+3)]
#转化为面板数据。
pdataframe=pdata.frame(temp,c('time','Index'))
#收益率序列处理完毕。
#回归分析部分。
pdataframe.plm=plm(yield.trimtail~i1+i2+i3+i4+o1+o2+o3+o4,data=pdataframe)
summary(pdataframe.plm)
#修正误差部分。
coeftest(pdataframe.plm, vcov=function(x) vcovHC(x, cluster="time", type="HC1"))
vcovDC <- function(x, ...){
  vcovHC(x, cluster="group", ...) + vcovHC(x, cluster="time", ...) - 
    vcovHC(x, method="white1", ...)
}
#输出美化部分
library(stargazer)
#生成的代码直接拷贝到LATEX文档里面去，生成很好看的表格。
stargazer(pdataframe.plm)