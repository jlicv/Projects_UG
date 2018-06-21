# 这一部分的注释是中英混杂的，非常抱歉，当时写代码的状态不太好，实在不想思考编码转换导致中文显示乱码的问题，就直接写英文注释了。
# 后来编码问题解决了，注释都用中文写了。
# 如果有任何疑惑，请再联系我吧。
# Input:The I/O Account, the Book translate indices and sectors. 
# Output:Recombined Direct coefficient Table based on indices.
setwd("C://Users//Jason//Desktop//Network Origins of Aggregate Fluctuations//ChinaDataset") 
#这些都是投入产出表的中间流量部分。
Flow02=read.csv("2002-123-raw.csv",header=TRUE,check.names='FALSE',row.names=1)
Flow07=read.csv("2007-135-raw.csv",header=TRUE,check.names='FALSE',row.names=1)
Flow12=read.csv("2012-139-raw.csv",header=TRUE,check.names='FALSE',row.names=1)
#这是单独读入的总投入的部分。
TotalInput=read.csv("ChinaTotalInput.csv",header=FALSE,row.names=1,colClasses = "character")
#这些是对应关系表。
Book02=read.csv("02-行业与指数对应关系.csv",as.is=TRUE,header=TRUE,row.names=1,colClasses='character')
Book07=read.csv("07-行业与指数对应关系.csv",as.is=TRUE,header=TRUE,row.names=1,colClasses='character')
Book12=read.csv("12-行业与指数对应关系.csv",as.is=TRUE,header=TRUE,row.names=1,colClasses='character')
#先做一个矩阵类型的转换，便于后面做重组的操作。
Flow02.matrix=as.matrix(Flow02)
Flow07.matrix=as.matrix(Flow07)
Flow12.matrix=as.matrix(Flow12)

IndexList02=row.names(Book02)
IndexList07=row.names(Book07)
IndexList12=row.names(Book12)

CCTotal02=as.numeric(TotalInput['2002Val',])
CCTotal02=CCTotal02[!is.na(CCTotal02)]
names(CCTotal02)=TotalInput['2002Var',][TotalInput['2002Var',]!=""]
CCTotal07=as.numeric(TotalInput['2007Val',])
CCTotal07=CCTotal07[!is.na(CCTotal07)]
names(CCTotal07)=TotalInput['2007Var',][TotalInput['2007Var',]!=""]
CCTotal12=as.numeric(TotalInput['2012Val',])
CCTotal12=CCTotal12[!is.na(CCTotal12)]
names(CCTotal12)=TotalInput['2012Var',][TotalInput['2012Var',]!=""]

Flow12.Re=Recombination(IndexList12,Book12,Flow12.matrix)
write.csv(Flow12.Re,"Flow12-re.csv")
Flow12.Total.re=TotalRecombination(IndexList12,Book12,CCTotal12)
#这些nstd的后缀的意思是全部是以比例为计算的，此时列的加和都是1，这跟Acemoglu(2012)那篇文章的处理的方法是一样的。
#这样做只是当时为了探究网络结构对冲击传导速度的影响。
CC12.propnstd=propnstd(as.matrix(Flow12.Re),Flow12.Total.re)

Flow07.Re=Recombination(IndexList07,Book07,Flow07.matrix)
Flow07.Total.re=TotalRecombination(IndexList07,Book07,CCTotal07)
CC07.propnstd=propnstd(as.matrix(Flow07.Re),Flow07.Total.re)

Flow02.Re=Recombination(IndexList02,Book02,Flow02.matrix)
Flow02.Total.re=TotalRecombination(IndexList02,Book02,CCTotal02)
CC02.propnstd=propnstd(as.matrix(Flow02.Re),Flow02.Total.re)

write.csv(CC12.purchases.method2,'CC12prop-purchase-re.csv')
write.csv(CC12.sales.method2,'CC12prop-sale-re.csv')
write.csv(CC07.propnstd,'CC07propnstd-re.csv')
write.csv(CC02.propnstd,'CC02propnstd-re.csv')

#Functions required to generate recombined prop matrix.

propnstd=function(mat1,total)
{
  mat2=matrix(0,nrow(mat1),ncol(mat1))
  for(i in 1:nrow(mat1))
  {
    if(colSums(mat1)[i]!=0)
      mat2[,i]=mat1[,i]/total[i]
    if(colSums(mat1)[i]==0)
      next
  }
  rownames(mat2)=rownames(mat1)
  colnames(mat2)=colnames(mat1)
  return(mat2)
}

propnstd.purchases.method2=function(mat1)
{
  mat2=matrix(0,nrow(mat1),ncol(mat1))
  for(i in 1:nrow(mat1))
  {
    if(colSums(mat1)[i]!=0)
     mat2[,i]=mat1[,i]/colSums(mat1)[i]
    if(colSums(mat1)[i]==0)
      next
  }
  rownames(mat2)=rownames(mat1)
  colnames(mat2)=colnames(mat1)
  return(mat2)
}

propnstd.sales.method2=function(mat1)
{
  mat2=matrix(0,nrow(mat1),ncol(mat1))
  for(i in 1:nrow(mat1))
  {
    if(rowSums(mat1)[i]!=0)
      mat2[i,]=mat1[i,]/rowSums(mat1)[i]
    if(rowSums(mat1)[i]==0)
      next
  }
  rownames(mat2)=rownames(mat1)
  colnames(mat2)=colnames(mat1)
  return(mat2)
}

Adapter=function(vector)
{
  to=character(length(vector))
  for (i in 1:length(vector))
  {
    out=capture.output(vector[i])
    to[i]=trin.adhoc(out)
  }
  return(to)
}
trin.adhoc=function(string)
{
  string=gsub("^.*?>","",string[2])
  return(string)
}
Query=function(codes,List)
{
  List.secondary=List[,-2:-3]
  queryout=sapply(1:nrow(List.secondary),function(m) 
    ifelse(any(!is.na(z <- match(codes,List.secondary[m,]))),rownames(List.secondary)[m],0))
  if(length(unique(queryout))==1) #This is not elegant but NO alternative.
    return("0")
  else
    return(queryout[queryout!="0"])
}

Query.inverse=function(quotes,List)
{
  List.secondary=List[,-2:-3]
  rownum=sapply(1:nrow(List.secondary),function(m) 
    ifelse(any(!is.na(z <- match(quotes,rownames(List.secondary[m,])))),m,0))
  queryout=List.secondary[rownum!=0,-1]
  return(as.vector(unlist(queryout))[as.vector(unlist(queryout))!=""])
}

Incode=function(degreesorted,quantile)
{
  if(quantile<1&&quantile>0)
  {
    incode=degreesorted[degreesorted>quantile(degreesorted,probs=quantile)]
  } 
  return(incode)
}
#这一部分是重组函数，主要原理是，得到映射表(Book)以后。根据Book先对列作一个合并重组。
#再将列合并重组得到的新的矩阵，做行的重组。
#从而得到行业指数的中间流量表。
#函数的传入部分用英文写了，应该不是很难看懂吧。。。
#List: Name of the columns and rows of the recombined matrix.
#Book: A list showing the mapping between names of recombined matrix and orginal matrix.
#Table: The original matrix that is to be recombined.

Recombination=function(List,Book,Table){
  rownames(Table)=colnames(Table)
  Flowtable=matrix(0,nrow=length(List),ncol=ncol(Table))
  Second.Flowtable=matrix(0,nrow=length(List),ncol=length(List))
  rownames(Second.Flowtable)=rownames(Book)
  colnames(Second.Flowtable)=rownames(Book)
  rownames(Flowtable)=List
  colnames(Flowtable)=colnames(Table)
  for(i in 1:length(List))
  {
    outcode=Query.inverse(List[i],Book)
    if(length(outcode)!=1)
      Flowtable[i,]=colSums(Table[outcode,])
    else
      Flowtable[i,]=Table[outcode,]
    #Next, The operations should be done on the FlowTable
  }
  Flowtabletemp=Flowtable
  for(i in 1:length(List))
  {
    outcode=Query.inverse(List[i],Book)
    if(length(outcode)!=1)
      Second.Flowtable[,i]=rowSums(Flowtable[,outcode])
    else
      Second.Flowtable[,i]=Flowtable[,outcode]
  }
  return(Second.Flowtable)
}

TotalRecombination=function(List,Book,Vector){
  Vector.re=rep(x = 0,times=length(List))
  names(Vector.re)=List
  for(i in 1:length(List))
  {
    outcode=Query.inverse(List[i],Book)
    if(length(outcode)!=1)
      Vector.re[i]=sum(Vector[outcode])
    else
      Vector.re[i]=Vector[outcode]
  }
  return (Vector.re)
}

