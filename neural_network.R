library(nnet);  #安装nnet软件包 

library(mlbench);  #安装mlbench软件包

data(Vehicle);  #调入数据 

n=length(Vehicle[,1]); #样本量

set.seed(1);  #设随机数种子 

samp=sample(1:n,n/2);  #随机选择半数观测作为训练集

b=class.ind(Vehicle$Class);  #生成类别的示性函数

test.cl=function(true,pred){true<-
                              
                              max.col(true);cres=max.col(pred);table(true,cres)};

a=nnet(Vehicle[samp,-19],b[samp,],size=3,rang=0.1,decay=5e-4,maxit=200);  

#利用训练集中前18个变量作为输入变量，隐藏层有3个节点，初始随机权值在[-0.1,0.1]，权值是逐渐衰减的。

test.cl(b[samp,],predict(a,Vehicle[samp,-19])) #给出训练集分类结果

test.cl(b[-samp,],predict(a,Vehicle[-samp,-19]));#给出测试集分类结果

# 构建隐藏层包含15个节点的网络。接着上面的语句输入如下程序：

a=nnet(Vehicle[samp,-19],b[samp,],size=15,rang=0.1,decay=5e-
         
         4,maxit=10000);

test.cl(b[samp,],predict(a,Vehicle[samp,-19]));

test.cl(b[-samp,],predict(a,Vehicle[-samp,-19]));

# 利用人工神经网络方法分析鸢尾花数据。输入如下程序：

library(nnet);data(iris3)

dim(iris3)

ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3]) 

targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )

samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25)) # use 

half the iris data

ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1, decay = 5e-
              
              4, maxit = 200) 

test.cl <- function(true, pred) { true <- max.col(true); cres <- 
                                    
                                    max.col(pred); table(true, cres) } 

test.cl(targets[-samp,], predict(ir1, ir[-samp,])) 

# 或者输入如下程序：

data(iris3)

ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                  
                  species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))

ir.nn2 <- nnet(species ~ ., data = ird, subset = samp, size = 2, rang = 
                 
                 0.1,
               
               decay = 5e-4, maxit = 200)

table(ird$species[-samp], predict(ir.nn2, ird[-samp,], type = "class"))

# 用R语言class包的knn函数来做KNN算法分类，由于R中的datasets包已有
#iris数据，以及对应的3维数据iris3，所以在R中可直接调用iris3数据。决
#定每组取前30个数据为训练数据，用来生成分类规则，取每组后20个数据为
#测试数据，测试分类的准确率。

library(class)

data(iris3)

train <- rbind(iris3[1:30,,1], iris3[1:30,,2], iris3[1:30,,3]) #选每个品种的前30个数据为训练数据

test <- rbind(iris3[31:50,,1], iris3[31:50,,2], iris3[31:50,,3])#剩下的为测试数据

cl <- factor(c(rep("s",30), rep("c",30), rep("v",30)))

a=knn(train, test, cl, k = 3, prob=TRUE) #进行KNN算法分类，k=3，输出投票比率

attributes(.Last.value)

# 利用kknn软件包分析数据ionosphere，这是关于雷达是否被电离层反射的具有351个观测值的数据。自变量34个，因变量class为分类变量，取两个值：一是的确被电离层反射，标为“好”（g），否则就是“不好”（b）。随机选择大约一半的数据作为训练集，剩下的作为测试集。

library(kknn);  #激活软件包

data(ionosphere);  #激活数据

set.seed(1);  #设定随机种子

n=nrow(ionosphere);#样本量

test=sample(1:n,n/2);  #随机选取测试集

a=kknn(class~.,ionosphere[-test,],ionosphere[test,]);  #使用最近邻方法

table(ionosphere[test,]$class,a$fit)  #查看结果