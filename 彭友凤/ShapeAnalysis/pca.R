#https://blog.csdn.net/geekfocus/article/details/118501467

library(stats)
library(dplyr)
library(ggfortify)
wine.pca<-princomp(iris[,-5],cor=T,scores=T) 
#默认方差矩阵(cor=F),改为cor=T则结果与prcomp相同
summary(wine.pca) #各主成份的SVD值以及相对方差
wine.pca$loading #特征向量，回归系数
wine.pca$score
screenplot(wine.pca) #方差分布图
biplot(wine.pca,scale=F) #碎石图,直接把x与rotation绘图，而不标准化

pca1<-iris%>%select(-5)%>%prcomp()
autoplot(pca1,data = iris,col= 'Species',size=2,
         loadings =T,loadings.label = TRUE,
         frame = TRUE,frame.type='norm',
         label = TRUE, label.size = 3
)+  theme_classic()