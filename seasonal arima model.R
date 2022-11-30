library(astsa)
y=c(-0.17, 1.77, -0.05, 0.66, 1.06, 0.40, -0.34, 0.80, 0.41, 1.49, 0.28, 1.65, -1.21, -1.57, -1.38, 1.52, 1.74, -0.52, -0.63, 1.50, -0.30, -0.62, -1.43, -1.23, 1.37, 0.52, 1.68, -2.20, -1.07, -0.54, -0.83, -1.64, 1.28, 0.23, 0.62, -0.85, -0.74, 0.89, -1.76, 2.83, -1.44, 0.88, 0.92, -0.70, -2.45, -2.04, -0.70, 1.57, 0.89, -0.86)
par(mfrow=c(3,1))
plot(y)
lines(1:length(y),y)
acf(y,lag.max=30)
pacf(y,lag.max=30)
fit = arima(y, seasonal = list(order=c(1, 0, 0), period=12))
fit
tsdiag(fit)
sarima(y,0,0,0,1,0,0,12)

#Quarterly Propane gas bills
y=c(344.39, 246.63, 131.53, 288.87, 313.45, 189.76,
    179.1, 221.1, 246.84, 209, 51.21, 133.89, 277.01, 197.98, 50.68,
    218.08, 365.1, 207.51, 54.63, 214.09, 267, 230.28, 230.32, 426.41,
    467.06, 306.03, 253.23, 279.46, 336.56, 196.67, 152.15, 319.67,
    440, 315.04, 216.42, 339.78, 434.66, 399.66, 330.8, 539.78)

dev.off()
graphics.off()
plot(y)
lines(1:40,y)

par(mfrow=c(1,2))
acf(y,lag.max=30)
pacf(y,lag.max=30)

u=y[2:40]-y[1:39]
v=y[5:40]-y[1:36]
par(mfrow=c(2,2))
acf(u)
pacf(u)
acf(v)
pacf(v)
fita1 = arima(y, order=c(0,0,1), seasonal=list(order=c(0,1,1), period=4))
tsdiag(fita1)
fits1=sarima(y,0,0,1,0,1,1,4)

fita2 = arima(y, order=c(0,0,2),seasonal=list(order=c(0,1,1),period=4))

tsdiag(fita2)
fits2=sarima(y,0,0,2,0,1,1,4)

