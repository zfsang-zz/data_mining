library(adabag);  #安装adabag软件包 

set.seed(0);  #设随机数种子 

samp=c(sample(1:50,25) ,sample(51:100,25),sample(101:150,25));  #对三个品

#种每种随机抽取50%作为训练集

a=boosting(Species~.,data=iris[samp,],mfinal=15);  #实施以分类树为基分类

#器的adaboost算法

a.predt=predict.boosting(a,newdata=iris[samp,],type=" Species");  #对训练集做预测

a.predt  #给出预测结果

a.pred=predict.boosting(a,newdata=iris[-samp,],type=" Species");  #对测试集做预测

a.pred  #给出预测结果

barplot(a$importance)  #输出变量的相对重要性的条形图

#比较决策树方法与adaboost方法，输入如下程序：

data(BreastCancer)

l = length(BreastCancer[,1])

sub = sample(1:l,2*l/3) #随机选择2/3样本作为训练集，其余作为测试集

BC.rpart = rpart(Class~.,data=BreastCancer[sub,-1], maxdepth=3)

BC.rpart.pred = predict(BC.rpart,newdata=BreastCancer[-sub,-
                                                        
                                                        1],type="class")

tb =table(BC.rpart.pred,BreastCancer$Class[-sub])

error.rpart = 1-(sum(diag(tb))/sum(tb))

tb

error.rpart #以上为决策树分类方法

BC.adaboost = boosting(Class ~.,data=BreastCancer[,-1],mfinal=20)

BC.adaboost.pred = predict.boosting(BC.adaboost,newdata=BreastCancer[-
                                                                       
                                                                       sub,-1])

BC.adaboost.pred$confusion

BC.adaboost.pred$error #以上为adaboost方法

#仿照上面的过程分析Vehicle数据，输入如下程序：

data(Vehicle)

l = length(Vehicle[,1])

sub = sample(1:l,2*l/3)

Vehicle.rpart = rpart(Class~.,data=Vehicle[sub,],maxdepth=5)

Vehicle.rpart.pred = predict(Vehicle.rpart,newdata=Vehicle[-sub, 
                                                           
                                                           ],type="class")

tb = table(Vehicle.rpart.pred,Vehicle$Class[-sub])

error.rpart = 1-(sum(diag(tb))/sum(tb))

tb

error.rpart

Vehicle.adaboost = boosting(Class ~.,data=Vehicle[sub, ],mfinal=25)

Vehicle.adaboost.pred = 
  
  predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])

Vehicle.adaboost.pred$confusion

Vehicle.adaboost.pred$error

evol.train=errorevol(Vehicle.adaboost,newdata=Vehicle[sub, ])

evol.test=errorevol(Vehicle.adaboost,newdata=Vehicle[-sub, ])

plot(evol.test$error, type="l", main="AdaBoost error VS number of trees",
     
     xlab="Iterations", ylab="Error", col = "red")

lines(evol.train$error, cex = .5 ,col="blue", lty=2)

legend("topright", c("test","train"), col = c("red", "blue"), lty=1:2) #

#训练集和测试集分类误差的比较

#利用bagging方法分析Vehicle数据。输入如下程序：

library(mlbench);  #安装mlbench软件包

data(Vehicle);  #调入数据 

n=nrow(Vehicle);

set.seed(0);  #设随机数种子 

samp= sample(1:n,n/2);  #随机抽取一半作为训练集

a=bagging(Class~.,data= Vehicle[samp,],mfinal=25);  #实施以分类树为基分类器的bagging算法

a.predt=predict.bagging(a,newdata= Vehicle[samp,],type="Class");  #对训练集做预测

a.predt$class

a.predt$confusion

a.predt$error #给出结果

a.pred=predict.bagging(a,newdata= Vehicle[-samp,],type="Class");  #对测试集做预测

a.pred$class

a.pred$confusion

a.pred$error #给出结果

barplot(a$importance)  #输出bagging的变量重要图

#利用随机森林方法分析波士顿房价数据，用13个变量预测平均房价medv。输入如下程序：

library(randomForest);

B1=read.table("E:/study/Boston.txt",header=T);#读入数据

attach(B1);#激活变量名

w=randomForest(medv~.,data=B1,importance=T);

par(mfrow=c(1,2));for(i in 1:2) 
  
  barplot(t(round(importance(w),2))[i,],cex.names=0.5)  #画重要性图

#分析鸢尾花数据，输入如下程序：

library(randomForest);

data(iris)

set.seed(117);

samp=c(sample(1:50,25) ,sample(51:100,25),sample(101:150,25));

ws= randomForest(Species~., iris[samp,]);

wpt=predict(ws,iris[samp,]); #对训练集做预测

table(observed= iris[samp, "Species"],predicted=wpt); #输出预测结果

wp=predict(ws,iris[-samp,]);  #对测试集做预测

table(observed= iris[-samp, "Species"],predicted=wp); #输出预测结果

# 分析鸢尾花数据的变量重要性和观测值亲近性，输入如下程序：

library(randomForest);

data(iris)

w=randomForest(Species~.,data=iris, importance=TRUE, proximity=TRUE);

par(mfrow=c(1,2));for(i in 1:2) barplot(t 
                                        
                                        (importance(w))[i,],cex.names=0.7);  #画重要性图

par(mfrow=c(1,2));aa=eigen(w$proximity)[[2]]; 

plot(aa[,1:2],pch=(1:3)[as.numeric(iris$Species)],cex=0.7,main=" ",xlab=" 
     
     ",ylab=" "); 

MDSplot(w,iris$Species,palette=rep(1,3),pch=as.numeric(iris$Species), 
        
        xlab=" ",ylab=" ")  #把各个点的亲近度在二维图上显示出来。不同记号表示不同鸢尾花的种类。

# 分析数据airquality，输入如下程序：

library(randomForest);

data(airquality);

ozone.rf=randomForest(Ozone~.,data=airquality,mtry=3,importance=T,na.action=na.
                      
                      omit);

par(mfrow=c(1,2));for(i in 1:2) barplot(importance(ozone.rf)[,i])

#######################################################

Intall.package(“e1071”)

setwd("G:/digits/trainingDigits")

names<-list.files("G:/digits/trainingDigits")

data<-paste("train",1:1934,sep="")

for(i in 1:length(names))
  
  
  
  assign(data[i],as.vector(as.matrix(read.fwf(names[i],widths=rep(1,32)))))

label<-rep(0:9,c(189,198,195,199,186,187,195,201,180,204))

data1<-get(data[1])

for(i in 2:length(names))
  
  data1<-rbind(data1,get(data[i]))

m <- svm(data1,label,cross=10,type="C-classification")

summary(m)

pred<-fitted(m)

table(pred,label)

setwd("G:/digits/testDigits")

names<-list.files("G:/digits/testDigits")

data<-paste("train",1:1934,sep="")

for(i in 1:length(names))
  
  
  
  assign(data[i],as.vector(as.matrix(read.fwf(names[i],widths=rep(1,32)))))

data2<-get(data[1])

for(i in 2:length(names))
  
  data2<-rbind(data2,get(data[i]))

pred<-predict(m,data2)

labeltest<-rep(0:9,c(87,97,92,85,114,108,87,96,91,89))

table(pred,labeltest)