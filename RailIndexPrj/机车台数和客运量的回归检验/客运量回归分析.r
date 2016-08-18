#客运量与机车台数
passanger<-read.csv("locomotive-PV.csv",head=T)
lm.sol<-lm(y~x1,data=passanger) 
summary(lm.sol)
m<lm.sol
plot(m)#绘制残差图 
plot(fitted(lm.sol),resid(lm.sol)) #残差与回归值的散点图
qqnorm(resid(lm.sol))
