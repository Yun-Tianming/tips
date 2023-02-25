#参考  https://www.jianshu.com/p/97368848f727
#参考  https://zhuanlan.zhihu.com/p/140531637
# https://www.jianshu.com/p/a1f5fdf6f0b8
#置信区间  https://www.jb51.net/article/227433.htm
#1,核实plastic概率；2，重写预测函数区间
library(ggplot2)

a=read.csv("C:/Users/mfy/Desktop/耿/glmdata.csv", header=T, na.strings=c("NA"))
a$length
table(a$plastic)

# fit full model
fit.full <- glm(plastic ~ length+weight+cz+wz,
                data=a,family=binomial())
summary(fit.full)


# length
x <- a$length
y <- a$plastic
new <- data.frame(x = seq(15,30, 0.5)) 
predL <- predict(lm(y ~ x), new, interval = "confidence")
#matplot(new$x, predL,
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

x1 = seq(15, 30, 0.5)#等于new，但不在frame里

dat_plot <- data.frame(x1, predL[,1], predL[,2], predL[,3])
p=ggplot(dat_plot, aes(x = x1, color = 'r', fill = 'r')) +                  # x轴在此处添加，目的为了置信区间与拟合线共享同一个x
  geom_ribbon(aes(ymin = predL[,2], ymax = predL[,3]), alpha = 0.2) +  # 添加置信区间
  geom_line(aes(y = predL[,1]))+theme_classic()   # 添加拟合线
p + theme(legend.position = "none")+xlab("体长/mm")+ylab("概率")
ggsave("C:/Users/mfy/Desktop/耿/体长.png", #文件名称及其类型，一般通过改变后缀生成相应格式的图片
       width = 8,#宽
       height = 6, #高
       units = "in",#单位
       dpi = 600)#设置分辨率



# weight
x <- a$weight
y <- a$plastic
new2 <- data.frame(x = seq(60, 660, 20)) 
predW <- predict(lm(y ~ x), new2, interval = "confidence")

#matplot(new2$x, predW,
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


x2 = seq(60, 660, 20) #等于new，但不在frame里

dat_plot <- data.frame(x2, predW[,1], predW[,2], predW[,3])
p=ggplot(dat_plot, aes(x = x2, color = 'r', fill = 'r')) +                  # x轴在此处添加，目的为了置信区间与拟合线共享同一个x
  geom_ribbon(aes(ymin = predW[,2], ymax = predW[,3]), alpha = 0.2) +  # 添加置信区间
  geom_line(aes(y = predW[,1]))+theme_classic()   # 添加拟合线
p + theme(legend.position = "none")+xlab("体重/g")+ylab("概率")
ggsave("C:/Users/mfy/Desktop/耿/体重.png", #文件名称及其类型，一般通过改变后缀生成相应格式的图片
       width = 8,#宽
       height = 6, #高
       units = "in",#单位
       dpi = 600)#设置分辨率

# cz
x <- a$cz
y <- a$plastic
new3 <- data.frame(x = seq(1, 20, 0.5)) 
predCz <- predict(lm(y ~ x), new3, interval = "confidence")

#matplot(new2$x, predW,
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


x3 = seq(1, 20, 0.5) #等于new，但不在frame里

dat_plot <- data.frame(x3, predCz[,1], predCz[,2], predCz[,3])
p=ggplot(dat_plot, aes(x = x3, color = 'r', fill = 'r')) +                  # x轴在此处添加，目的为了置信区间与拟合线共享同一个x
  geom_ribbon(aes(ymin = predCz[,2], ymax = predCz[,3]), alpha = 0.2) +  # 添加置信区间
  geom_line(aes(y = predCz[,1]))+theme_classic()   # 添加拟合线
p + theme(legend.position = "none")+xlab("肠重/g")+ylab("概率")
ggsave("C:/Users/mfy/Desktop/耿/肠重.png", #文件名称及其类型，一般通过改变后缀生成相应格式的图片
       width = 8,#宽
       height = 6, #高
       units = "in",#单位
       dpi = 600)#设置分辨率



# wz
x <- a$wz
y <- a$plastic
new4 <- data.frame(x = seq(1, 40, 0.5)) 
predWz <- predict(lm(y ~ x), new4, interval = "confidence")

#matplot(new2$x, predW,
#        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")


x4 = seq(1, 40, 0.5) #等于new，但不在frame里

dat_plot <- data.frame(x4, predWz[,1], predWz[,2], predWz[,3])
p=ggplot(dat_plot, aes(x = x4, color = 'r', fill = 'r')) +                  # x轴在此处添加，目的为了置信区间与拟合线共享同一个x
  geom_ribbon(aes(ymin = predWz[,2], ymax = predWz[,3]), alpha = 0.2) +  # 添加置信区间
  geom_line(aes(y = predWz[,1]))+theme_classic()   # 添加拟合线
p + theme(legend.position = "none")+xlab("胃重/g")+ylab("概率")
ggsave("C:/Users/mfy/Desktop/耿/胃重.png", #文件名称及其类型，一般通过改变后缀生成相应格式的图片
       width = 8,#宽
       height = 6, #高
       units = "in",#单位
       dpi = 600)#设置分辨率








# 堆叠
# 保证二者长度一样，在each里确定
x1 = seq(15, 30, 0.5)
x2 = seq(60, 660, 30)
dat_plot <- data.frame(rbind(cbind(x1, predL[,1], predL[,2], predL[,3]), cbind(x2, predW[,1], predW[,2], predW[,3])))
names(dat_plot) <- c("x", "y", "ci_l", "ci_r")
dat_plot$group <- rep(c("length", "weight"),each=31)

ggplot(dat_plot, aes(x = x1, color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_l, ymax = ci_r), alpha = 0.3) +  # alpha 修改透明度
  geom_line(aes(y = y))

