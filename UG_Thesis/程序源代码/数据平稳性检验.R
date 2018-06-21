#调用包
library("urca")
#在这个函数中，我一般是不调用外部已经写好的csv的，而是直接先把函数运行一遍，让他在命名空间里存在。
#然后，再把同一空间下，其他文件的变元调用做平稳性检验。
#填补缺失值再做序列测试。
naapproxout=na.approx(upperandlower.vol.matrix)
#这个是平稳性结果的批量检验，将一堆数据传进去，他就会逐行地为数据做平稳性检验，然后将结果输出来。
stationary.test.loop=function(dataset){
  for (i in 1:nrow(dataset)){
    dataset.adf1=ur.df(dataset[i,],type = 'none',selectlags='AIC')
    #取一下平稳性检验的结果出来。
    out1=slot(dataset.adf1,"teststat")
    #取99%的置信区间下的临界值。
    out1.cval=slot(dataset.adf1,"cval")[,'1pct']
    #再把结果组合在一起。
    out1.all=rbind.data.frame('out'=out1,'thres'=out1.cval,"nonstationary"=ifelse(abs(out1.cval)<abs(out1),TRUE,FALSE))
    colnames(out1.all)=rownames(dataset)[i]
    stationary.out=cbind.data.frame(stationary.out,out1.all)
  }
  return(stationary.out)
}
loopout=stationary.test.loop(myvol)
stationary.out=data.frame(row.names = c("value,thres,below"))
stationary.test.loop.trend=function(dataset){
  for (i in 1:nrow(dataset)){  
    dataset.adf3=ur.df(dataset[i,],type = 'trend')
    out3=slot(dataset.adf3,"teststat")
    out3.cval=slot(dataset.adf3,"cval")[,'1pct']
    out3.all=rbind.data.frame('out'=out3,'thres'=out3.cval,"nonstationary"=ifelse(abs(out3.cval)<abs(out3),TRUE,FALSE))
    colnames(out3.all)=rep(rownames(dataset)[i],times=ncol(out3.all))
    stationary.out=cbind.data.frame(stationary.out,out3.all)
  }
  return(stationary.out)  
}
#做平稳性检验
stationary.test.loop(t(naapproxout))
stationary.out=data.frame(row.names = c("value,thres,below"))

#把结果输出为LATEX中的代码。
library(stargazer)
stargazer(loopout.trend)
