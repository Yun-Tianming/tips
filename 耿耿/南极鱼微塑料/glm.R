#参考  https://www.jianshu.com/p/97368848f727
#参考  https://zhuanlan.zhihu.com/p/140531637
# get summary statistics
data(Affairs, package="AER")
Affairs
summary(Affairs)  #基本信息统计
table(Affairs$affairs) #次数统计


# create binary outcome variable
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)



# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=binomial())
summary(fit.full)