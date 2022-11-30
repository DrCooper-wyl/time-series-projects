# AR(1)
z=c(1.0445, -0.1338, 0.6706, 0.3755, -0.5110, -0.2352, 0.1595,
    1.6258, -1.6739, 2.4478, -3.1019, 2.6860, -0.9905, 1.2113, -0.0929, 0.9905, 0.5213,
    -0.1139, -0.4062, 0.5438)
x=matrix(c(rep(1,19),z[1:19]),19,2);
y=z[2:20]
u=solve(t(x)%*%x)
beta=u%*%(t(x)%*%y)
beta
fit=x%*%beta
par(mfrow=c(1,5))
plot(1:20,z,type="l",xlim=c(1,20),ylim=c(-3.5,3.5))
acf(z)
pacf(z)
plot(1:20,z,type="l",xlim=c(1,20),ylim=c(-3.5,3.5),col=1)
par(new=T)
plot(2:20,fit,type="b",xlim=c(1,20),ylim=c(-3.5,3.5),col=2)

ar.ols(z,order=1,demean=FALSE,intercept=TRUE)

#plot(1:20,c(1.0445,fit),type="b",xlim=c(1,20),ylim=c(-3.5,3.5),col=2)

fit1=arima(z,order=c(1,0,0))
fit1
beta1=coef(fit1)
fit2=x%*%beta1-beta1[1]*beta1[2]
plot(1:20,z,type="l",xlim=c(1,20),ylim=c(-3.5,3.5),col=1)
par(new=T)
plot(2:20,fit2,type="b",xlim=c(1,20),ylim=c(-3.5,3.5),col=2)


predict(fit1,n.ahead=5)

