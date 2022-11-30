
y=c(112,118,132,129,121,135,148,148,136,119,104,118,115,126,141,135,125,149,170,170,158,133,
    114,140,145,150,178,163,172,178,199,199,184,162,146,166,171,180,193,181,183,218,230,242,209,191,172,194,196,196,236,235,229,243,264,272,237,211,180,201,204,188,235,227,234,264,302,293,259,229,203,229,242,233,267,269,270,315,364,347,312,274,237,278,284,277,317,313,318,374,413,405,355,306,271,306,315,301,356,348,355,422,465,467,404,347,305,336,340,318,362,348,363,435,491,505,404,359,310,337,360,342,406,396,420,472,548,559,463,407,362,405,417,391,419,461,472,535,622,606,508,461,390,432)
x=matrix(c(rep(0,144*23)),144,23);
for(i in 1:144)
{
  x[i,1]=i;
}
for(i in 1:12)
{
  for(j in 1:11)
  {
    x[(i-1)*12+j,j+1]=1;
    x[(i-1)*12+j,j+12]=(i-1)*12+j;
  }
}
fit0=lm(y~x);
summary(fit0);
e0=fit0$residuals;
par(mfrow=c(1,2))
acf(e0); pacf(e0)

fit1=arima(y,xreg=x,order=c(7,0,0)); 
fit1
coef=fit1$coef
coef
e1=fit1$residuals;
e1
1-sum(e1^2)/sum((y-mean(y))^2)
par(mfrow=c(2,1))
acf(e1); pacf(e1);
tsdiag(fit1)

fit2 = arima(y, xreg = x, order=c(10,0,0)); 
fit2
coef=fit2$coef
coef
e2=fit2$residuals;
e2
1-sum(e2^2)/sum((y-mean(y))^2)
par(mfrow=c(2,1))
acf(e2); pacf(e2);
tsdiag(fit2)

xnew=matrix(c(rep(0,12*23)),12,23);
for(i in 1:11)
{
  xnew[i,1]=i+144;
  xnew[i,i+1]=1;
  xnew[i,i+12]=i+144;
}
xnew[12,1]=156;
yhat = y - fit2$residuals;
pred = predict(fit2, newxreg = xnew, n.ahead=12)
pred$pred
pred$se
n=144; t=1:n;
dev.off()
plot(t,y, type="o",xlim=c(1,n+12),ylim=c(0,800),col=1,xlab="years",ylab="passengers",xaxt="n")
axis(side=1, at = seq(1, n+12, by = 12),labels=1949:1961)
par(new=T)
y1=c(yhat, pred$pred[1:12])
t1=1:(n+12)
plot(t1,y1, pch=5,type="b",xlim=c(1,n+12),ylim=c(0,800),col=2,xlab="",ylab="",xaxt="n")
y11 = y1+1.96*pred$se[1:12]
y12 = y1-1.96*pred$se[1:12]
par(new=T)
plot(t1,y11, type="l",xlim=c(1,n+12),ylim=c(0,800),col=3,xlab="",ylab="",xaxt="n")
par(new=T)
plot(t1,y12, type="l",xlim=c(1,n+12),ylim=c(0,800),col=3,xlab="",ylab="",xaxt="n")