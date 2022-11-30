#calculate theta
n=8;
t=c(1,2,3,4,5,6,7,8);
y=c(211,233,214,249,248,263,262,279);
plot(t,y, xlim=c(1,12),ylim=c(200,350),col=2,xlab="",ylab="")
x=matrix(c(rep(1,8),1:8),8,2);
t(x)%*%x
t(x)%*%y
beta=solve(t(x)%*%x)%*%(t(x)%*%y)
beta
x1=matrix(c(rep(1,12),1:12),12,2)
data=x1%*%beta
data
s=sqrt(sum((y-data[1:8])^2)/(8-2))
s
invx=solve(t(x)%*%x)
a1=data[9]-1.96*s*sqrt(1+c(1,9)%*%invx%*%c(1,9))
a2=data[9]+1.96*s*sqrt(1+c(1,9)%*%invx%*%c(1,9))
b1=data[10]-1.96*s*sqrt(1+c(1,10)%*%invx%*%c(1,10))
b2=data[10]+1.96*s*sqrt(1+c(1,10)%*%invx%*%c(1,10))
c1=data[11]-1.96*s*sqrt(1+c(1,11)%*%invx%*%c(1,11))
c2=data[11]+1.96*s*sqrt(1+c(1,11)%*%invx%*%c(1,11))
d1=data[12]-1.96*s*sqrt(1+c(1,12)%*%invx%*%c(1,12))
d2=data[12]+1.96*s*sqrt(1+c(1,12)%*%invx%*%c(1,12))
par(new=T)
t1=1:12
plot(t1,data,type="l",xlim=c(1,12),ylim=c(200,350),axes=F,col=3,xlab="quarter",ylab="sales")
par(new=T)
plot(c(9,9),c(a1,a2),type ="l",xlim=c(1,12),ylim=c(200,350),axes=F,xlab = "",ylab = "")
par(new=T)
plot(c(10,10),c(b1,b2),type ="l",xlim=c(1,12),ylim=c(200,350),axes=F,xlab = "",ylab = "")
par(new=T)
plot(c(11,11),c(c1,c2),type ="l",xlim=c(1,12),ylim=c(200,350),axes=F,xlab = "",ylab = "")
par(new=T)
plot(c(12,12),c(d1,d2),type ="l",xlim=c(1,12),ylim=c(200,350),axes=F,xlab = "",ylab = "")
length=c(a2-a1,b2-b1,c2-c1,d2-d1)
length

#autocorrelation in the error terms
n=20;
t=1:n;
y=c(15.91,9.8,17.16,16.68,15.53,22.66,31.01,8.62,45.82,10.97,45.46,28.69,36.75,37.75,41.18,42.67,46.05,43.7,53.08,47.56);
length(y)
par(mfrow=c(2,1))
plot(t,y, xlim=c(1,n),ylim=c(0,60),col=2,xlab="",ylab="")
x=matrix(c(rep(1,n),1:n),n,2);
t(x)%*%x
t(x)%*%y
beta=solve(t(x)%*%x)%*%(t(x)%*%y)
beta
par(new=T)
data=x%*%beta
plot(t,data,type="l",xlim=c(1,n),ylim=c(0,60),col=2,xlab="",ylab="")
err=y-x%*%beta
plot(t,err,xlim=c(1,n),ylim=c(-20,20),col=2,xlab="",ylab="")
d=sum((err[1:19]-err[2:20])^2)/sum(err^2)
d
r=sum((err[1:19]*err[2:20]))/sum(err^2)
r
