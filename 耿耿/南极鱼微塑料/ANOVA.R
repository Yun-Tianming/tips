# 参考 https://www.jianshu.com/p/fd6d3d7e3afe

a=read.csv("novadata.csv", header=T, na.strings=c("NA"))
table(a$amount)

fit <- aov(amount~type, data = a)
summary(fit)