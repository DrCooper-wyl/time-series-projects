#Estimation and Point Prediction
n=8;
t=c(1,2,3,4,5,6,7,8);
y=c(211,233,214,249,248,263,262,279);
a=sum(t);
b=sum(t*t);
c=sum(y);
d=sum(t*y);
beta0=(b*c-a*d)/(n*b-a^2)
beta1=(n*d-a*c)/(n*b-a^2)
c(beta0, beta1)
t1=1:12
data=beta0+beta1*t1
y=c(y,data[9:12])
#plot(t1,y, xlim=c(1,12),ylim=c(200,320),col=2,xlab="",ylab="")
plot(t1[1:8],y[1:8], xlim=c(1,12),ylim=c(200,320),col="blue",pch=8,xlab="",ylab="")
par(new=T)
plot(t1,data,type="l",xlim=c(1,12),ylim=c(200,320),axes=F,col=5,xlab="quarter",ylab="sales")
points(t1,data,pch=5,col=5)


#MSE
x=matrix(c(rep(1,8),1:8),8,2);
y=c(211,233,214,249,248,263,262,279);
t(x)%*%x
t(x)%*%y
beta=solve(t(x)%*%x)%*%(t(x)%*%y)
t1=1:12
xnew=matrix(c(rep(1,12),1:12),12,2);
data=xnew%*%beta
y=c(y,data[9:12])
plot(t1,y, xlim=c(1,12),ylim=c(200,320),col=2,xlab="",ylab="")
par(new=T)
plot(t1,data,type="l",xlim=c(1,12),ylim=c(200,320),axes=F,col=3,xlab="quarter",ylab="sales")

#quadratic function trend
n=12;
t=1:12;
y=c(201,291,346,416,451,486,549,579,527,582,589,562);
x=matrix(c(rep(1,12),1:12,(1:12)^2),12,3)
t(x)%*%x
t(x)%*%y
beta=solve(t(x)%*%x)%*%(t(x)%*%y)
beta
t1=1:16
x1=matrix(c(rep(1,16),1:16,(1:16)^2),16,3)
data=x1%*%beta
y=c(y,data[13:16])
plot(t1,y, xlim=c(1,16),ylim=c(200,650),col=2,xlab="",ylab="")
par(new=T)
plot(t1,data,type="l",xlim=c(1,16),ylim=c(200,650),axes=F,col=3,xlab="quarter",ylab="loan")

