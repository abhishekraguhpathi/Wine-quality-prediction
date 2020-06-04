install.packages("caret")
install.packages("class")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("gridExtra")
library(ggplot2)
library(gmodels)
library(caret)
library(gridExtra)
library(class)

#reading data set
wine_data<-read.csv(file.choose())
str(wine_data)
colnames(wine_data)<-c("Fixedacidity","Volatileacidity","Citricacid","Residalsgar","Chlorides"
                       ,"Freeslsulphurdioxide","Totalsulphurdioxide","density"
                       ,"pH","Sulphates","Alcohol","Quality")

wine_data$Qualityn<-wine_data$Quality
wine_data$Quality[which(wine_data$Quality %in% c(0,1,2))]="Very Bad"
wine_data$Quality[which(wine_data$Quality %in% c(3,4,5))]="Mediocre"
wine_data$Quality[which(wine_data$Quality %in% c(6,7,8))]="Normal"
wine_data$Quality[which(wine_data$Quality %in% c(9,10))]="Excellent"
wine_data$Quality<-factor(wine_data$Quality)
str(wine_data)

#plots to see how quality changes w.r.t other parameters
attach(wine_data)
a<-ggplot(data=wine_data,aes(Qualityn,Fixedacidity))+geom_jitter()+geom_smooth(method ="lm" )
b<-ggplot(data=wine_data,aes(Qualityn,Volatileacidity))+geom_jitter()+geom_smooth(method ="lm" )
c<-ggplot(data=wine_data,aes(Qualityn,Citricacid))+geom_jitter()+geom_smooth(method ="lm" )
d<-ggplot(data=wine_data,aes(Qualityn,Residalsgar))+geom_jitter()+geom_smooth(method ="lm" )
e<-ggplot(data=wine_data,aes(Qualityn,Chlorides))+geom_jitter()+geom_smooth(method ="lm" )
f<-ggplot(data=wine_data,aes(Qualityn,Freeslsulphurdioxide))+geom_jitter()+geom_smooth(method ="lm" )
g<-ggplot(data=wine_data,aes(Qualityn,Totalsulphurdioxide))+geom_jitter()+geom_smooth(method ="lm" )
h<-ggplot(data=wine_data,aes(Qualityn,density))+geom_jitter()+geom_smooth(method ="lm" )
i<-ggplot(data=wine_data,aes(Qualityn,pH))+geom_jitter()+geom_smooth(method ="lm" )
j<-ggplot(data=wine_data,aes(Qualityn,Sulphates))+geom_jitter()+geom_smooth(method ="lm" )
k<-ggplot(data=wine_data,aes(Qualityn,Alcohol))+geom_jitter()+geom_smooth(method ="lm" )

#arranging plots in grid for better examination
grid_1=grid.arrange(a,b,c,d,e)
grid_2=grid.arrange(f,g,h,i,j,k)

#Splitting data into train and test
set.seed(1234)
index<-createDataPartition(wine_data$Quality, p=.7, list=F)
Train<-wine_data[index,]
Test<-wine_data[-index,]


#Training
ctrl<-trainControl(method = "repeatedcv", number=10,repeats = 3,classProbs = TRUE)
knn_c<-train(Quality~., 
             data=Train,
             method="knn",
             metric="Accuracy", 
             trControl=ctrl, 
             preProcess=c("center", "scale"),
             tuneLength=5,
             tuneGrid=expand.grid(k=1:39 ))
knn_c

#prediction and confusion matrix
pred<-predict(knn_c,newdata = Test)
confusionMatrix(pred,Test$Quality)

#initial accracy  96.03%
#95% CI:(0.9387, 0.976)
#accuracy level after using tunegrid for k=1:39=96.66%

