library(MASS)  #安装MASS软件包 

data(shuttle)  #调入数据 

set.seed(2)  #设随机数种子 

m=256  #样本量 

samp=sample(1:m,floor(m/10))  #抽取10%的下标为测试集 

tsamp=setdiff(1:m,samp)  #剩余作为训练数据集的下标 

library(rpart)  #安装rpart软件包 

b=rpart(use~.,shuttle,subset=tsamp)  #利用训练数据集生成决策树 

b  #输出决策树结果 

plot(b);text(b)  #画出决策树的树状图

t(table(predict(b,shuttle[tsamp,],type="class"),shuttle[tsamp,7]))  #输出训练集分类结果

t(table(predict(b,shuttle[samp,],type="class"),shuttle[samp,7]))   #输出测试集分类结果

#仿照上述程序分析iris数据，将下述程序补充完整（iris是R中的固有数据，不用另行输入）：

set.seed(10)  #设随机数种子 

m=150  #样本量 

samp=c(sample(1:50,25),sample(51:100,25),sample(101:150,25))  #对三个品种

#每种随机抽取50%的下标为测试集 

tsamp=ssetdiff(1:m,samp)etdiff(1:m,samp)  #剩余作为训练数据集的下标 

library(rpart)  #安装rpart软件包 

b=r rpart(species~.,shuttle,subset=tsamp) bsetsamp)  #利用训练数据集生成决策树 

b  #输出决策树结果 

plot(b);text(b)  #画出决策树的树状图

t(table(predict(b,iris[tsamp,],type="class"),iris[tsamp,5]))  #输出训练集分类结果

t(table(predict(b,iris[samp,],type="class"),iris[samp,5]))   #输出测试集分类结果

#利用决策树方法分析波士顿房价数据。首先对数据进行预处理输入如下程序：

b1=read.table("E:/study/bh.txt",header=T);#读入数据

attach(b1);#激活变量名

par(mfrow=c(2,2));#准备画四张图

plot(crim,medv,main="crim变换前");

plot(crim,lstat,main="crim变换前");

plot(log(crim),medv,main="crim变换后");

plot(log(crim),lstat ,main="crim变换后");

b1$crim=log(b1$crim);#对crim进行变换

#对数据进行决策树分类。输入如下程序：

library(tree);

b=tree(medv~lstat+crim,b1);

plot(b);

text(b);

plot(b1$lstat, b1$crim,xlab="lstat",ylab="crim");

partition.tree(b,add=TRUE,cex=1.5)

#使用鸢尾花（Iris）数据为例利用CART算法进行分类。CART算法在R中的程序包是tree，函数也是tree。

#在R Console窗中输入如下程序：

library(tree); #安装tree软件包

ir.tr <- tree(Species ~., iris); #对品种进行CART分类

ir.tr; #输出决策树

summary(ir.tr); 

plot(ir.tr);  

text(ir.tr) #画决策树图

#利用tree程序包分析shuttle数据，将如下程序补充完整，使用前253个数据进行决策树分类，对余下的三个数据进行预测。绘制出决策树并输出预测分类结果

library(tree);

data(shuttle, package="MASS")

shuttle.tr <- tree(use ~ ., shuttle, subset=1:253, mindev=1e-6, minsize=2) 

shuttle.tr; 

plot(shuttle.tr); 

text(shuttle.tr)

shuttle1 <- shuttle[254:256, ] # 3 missing cases

predict(shuttle.tr, shuttle1) 

#9、 利用R中的程序包RWeka使用Fisher于1936年发表的鸢尾花（Iris）数据为例实现C4.5算法。输入程序如下：

library(RWeka)

library(party)

data(iris)

m1 <- J48(Species ~ ., data = iris)

m1

table(iris$Species, predict(m1))

write_to_dot(m1)

if(require("party", quietly = TRUE)) plot(m1)