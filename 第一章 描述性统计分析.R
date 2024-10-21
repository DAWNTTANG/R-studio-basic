####1.数据描述
  ##1.1集中程度
  #数值型平均数(算术平均数)
mean(x,trim=0,na.rm=TRUE)
#trim设置极端值怎么舍弃，计算前可以去掉与均值离差较大的数据的比例，缺省值为0，即保留全部数据；
#当na.rm = TRUE时，允许数据中有缺失数据且自动跳过。
  #位置型平均数
median()
quantile()
#分位数quantile(x, probs = seq(0, 1, 0.25),na.rm = FALSE,names = TRUE, type = 7, ...)
w<-c(75.0, 64.0,47.4, 66.9, 62.2, 62.2, 58.7, 63.5, 66.6, 64.0, 57.0, 69.0, 56.9, 50.0,72.0)
mean(w)
median(w)
quantile(w)
quantile(w,c(0.5,0.75)) 
w[12]<-NA  #制造一个缺失值
mean(w)
mean(w,na.rm=T)
  ##顺序
sort()
#sort(x, partial=NULL, na.last =NA, decreasing = FALSE, method = c("shell", "quick"), index.return = FALSE)
rank()
#不是排序，产生秩
order()
#返回的是元素的原有下标。可以通过中括号用于向量、数据框特定位置上的元素。
sort(w)
mytext<-c('R','PHP','Python','Java','Ruby')
sort(mytext) #sort还可以给字符向量排序

rank(w)
rank(mytext)

a<-order(w) #order()返回值保存在新对象里，便于下一步进行元素选区的  操作
w[a]
  ##分散程度（离中趋势、变异指标，衡量数据的分散性和变异程度。常用的有极差、四分位差、平均差，方差、标准差和离散系数）
var(x,y=NULL,na.rm = FALSE,use)
#方差
sd(x,na.rm = FALSE)
#标准差
cov()
#协方差矩阵
cor()
#相关系数矩阵
cv <- 100*sd(w)/mean(w)  #计算标准差系数
css <- sum((w-mean(w))^2)  #计算校正平方和
install.packages("moments") #moments包里有计算峰度和偏度的函数
library(moments)
skewness(w)
kurtosis(w)
#偏度系数是刻划数据对称性的指标。样本关于均值对称的其偏度为0；右侧更分散的数据偏度系数为正，左侧更分散的数据偏度系数为负。
#峰度系数刻画数据分布纵向特征，当数据的总体分布为正态分布时，峰度系数近似为0；当分布较正态分布的尾部更分散时，峰度系数为正；否则为负。
  ####2.数据分布特征
  ##正态分布函数
dnorm(x,mean=0,sd=1,log=FALSE)
#密度函数
pnorm(q,mean=0,sd=1,lower.tail = TRUE,log.p = FALSE)
#分布函数
#x、q是数值型变量构成向量
qnorm(p,sd=1,lower.tail = TRUE,log.p = FALSE)
#p是由概率组成的向量
#log.p是逻辑变量，当它为TRUE时，函数的返回值是对数正态分布；lower.tail也是逻辑变量，设置大于临界值还是小于临界值。                              
rnorm(n,mean=0,sd=1)
#n是生成随机数的个数
  ##直方图、经验分布图与正态分布QQ图
hist(w, freq=FALSE,xlab="成绩分组",col="blue")
#histogrm直方图适合总体连续型分布
plot(ecdf(w),verticals = TRUE, do.p = FALSE)  
#生成经验分布图适用于离散型总体分布，verticals控制竖直线条添加，do.p设置数据点的添加。
qqnorm(w)
qqline(w)
#qq图，鉴别样本分布形态
boxplot(w)
#箱型图能看出变量中心趋势、发散、离群值
  ##正态性检验
shapiro.test(w)
#shapiro-wilk-正态分布检验>0.05拒绝原假设，样本属于总体
ks.test(x,y, ..., alternative = c("two.sided", "less", "greater"),  exact = NULL)
ks.test(w,"pnorm")
#Kolmogorov-Smirnov-关于经验分布拟合检验，D=sup|Fn(x)-F0(x)|

  ####3.数据的合并和转换
  ##数据框的合并
merge(x,y,by=intersect(names(x),names(y)),by.x=by,by.y=by,all=FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"),incomparables = NULL, ...)
#x, y：要合并的两个数据表。
#by：用于匹配的列名或列的编号。
#by.x, by.y：当两个数据表中的列名不一样时，可以使用这两个参数指定x和y中用于匹配的列名。
#all：一个逻辑值，用于指定是否包括没有匹配的行。
#all.x, all.y：在有未匹配行的情况下，指定哪个数据表中的行应该包含在结果中。
#sort：一个逻辑值，用于指定是否对结果进行排序。
#suffix.x, suffix.y：当两个数据表中的列名冲突时，可以使用这两个参数指定附加到列名末尾的后缀。
#incomparables：一个列表，用于指定在比较过程中应该视为不可比较的值。

m<-data.frame(manager=1:5,work=c(10,15,8,14,15))
merge(leadership,m) 
  ##矩阵合并
cbind(x,y)
#列列合并，行数一致
rbind()
#行行合并，列数相同
  ##数据子集提取
w[,3:10]  #选取3—10列
w[,c(-8,-9)] #剔除第8和9列
w[w$w1 == "M"  &  w$w1 > 30, ]
subset(w, age >= 35 | age < 26,  select = c(q1, q2, q3, q4))#提取向量、矩阵或数据框的子集
w[sample(1:nrow(w),3,replace =  F),] #随机抽样

  ####4.缺失数据寻找
  ##缺失数据定位
summary(w)  #观察
which(is.na(leadership$q4))#得到缺失值所在向量的位置。
which(complete.cases(leadership)==F)
#得到缺失值的行
dotchart(leadership$age) #观察异常值的分布。
  ##缺失数据的删除
table(is.na())
sum(is.na())
mean(is.na()) #缺失值个数占全部样本数据比例
na.omit()#删除含缺失值的整行

w[2]<-NA
is.na(w) #函数返回的是由逻辑值构成的单一元素向量。
mean(is.na(w)) 
sum(is.na(w))
na.omit(w)

  ####5.缺失数据的填补
install.packages("DMwR2")
library(DMwR2)
table(is.na(w))
colSums(is.na(w)) #缺失值按列计数
which(!complete.cases(w)) #有缺失值的行号
algae[!complete.cases(w), ] #按行展示
manyNAs(w, 0.2) #找出缺失值个数较多的观测，返回的是行号。默认筛选比例是0.2，缺失变量占全部变量20%。
  ##集中趋势填补缺失值
w2<-na.omit(w)
w3<-w[-manyNAs(w),] #删去两行
w3<-centralImputation(w) #直接填补

centralImputation()#样本集中趋势填补
  ##变量相关性填补缺失
#首先观察变量的相关系数矩阵，symnum用于美化、简化相关系数矩阵
symnum(cor(w[,4:18], use="complete.obs"))
#识别出相关性较强的变量，建立线性模型，估计模型参数。
w3<-w[-manyNAs(w),] #先删除缺失值较多的观测
colSums(is.na(w3)) 
model1<-lm(formula=PO4~oPO4, data=w3)
#利用线性模型预测缺失值、补齐缺失值。
w3[28,"PO4"] <- 42.897+1.293*w3[28,"oPO4"] 
  ##变量相似性填补缺失值，最近邻法
knnImputation(data, k = 10, scale = T, meth = "weighAvg",  distData = NULL)
#k设置近邻个数，默认10个；meth设置信息提取方法，默认的是加权平均，还可以选”中位数”。


setwd("C:\\Users\\唐艺倩\\Downloads")
air2<-read.csv("data2.csv",sep=",",header = T)
install.packages("moments")
library(moments)
str(air2)
ddata<- function(air2, na.omit=FALSE){
  if(na.omit){
    air2 <- air2[!is.na(air2)] 
    }
  return(c(
    mean <- mean(air2),
    max<-max(air2),
    min<-min(air2),
    median<-median(air2),
    sd <- sd(air2),
    skewness<-skewness(air2),
    kurtosis<-kurtosis(air2)
    ))
}
myvars <- c("PM2.5",	"PM10",	"CO",	"NO2",	"SO2",	"O3","ws","p","pr","T","h")
results <- sapply(air2[myvars], ddata)  
row.names(results) <- c("Mean", "Max", "Min", "Median", "SD", "Skewness", "Kurtosis")  
results
install.packages("openxlsx")
library(openxlsx)
write.xlsx(results,file = "2.xlsx")



par(mfrow=c(3,4))
metrics <- c("PM2.5", "PM10", "CO", "NO2", "SO2", "O3","ws","p","pr","T","h")  

for (metric in metrics){
  qqnorm(air2[[metric]], main=paste(metric, "Q-Q 图"))  
  qqline(air2[[metric]], col="red")  
}

for (metric in metrics) {  
boxplot(air2[[metric]], main=paste(metric, "箱线图"), ylab=metric)
}


