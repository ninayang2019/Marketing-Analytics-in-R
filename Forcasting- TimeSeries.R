##  Deliverables for HW 3
data<-read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)
temp = data[,11]
xx = ts(temp, frequency = 52, start = c(2015,1))
plot.ts(xx)
xd = decompose(xx) 
xd.trend = xd$trend
xd.seasonal = xd$seasonal
xd.random = xd$random
plot.ts(cbind(xx,xd.trend, xd.seasonal, xd.random))
# 1. Try different combinations of alpha, beta, gamma as on/off on temperature series
out1 <- HoltWinters(xx,beta=TRUE, gamma=TRUE)
out2 <- HoltWinters(xx,beta=TRUE, gamma=FALSE)
out3 <- HoltWinters(xx,beta=FALSE, gamma=TRUE)
out4 <- HoltWinters(xx,beta=FALSE, gamma=FALSE)

# 2. Note down SSE values and compute information criteria (AIC, AICC, BIC). Recommend the best model
#SSE
print(paste("SSE for models:",out1$SSE,out2$SSE,out3$SSE,out4$SSE))

#aic bic & aicc
aic= function(model){
  n=length(model$fitted)
  p=length(model$x)
  sse = model$SSE
  aic= (2/n)*p + n*log(sse/n)
  return(aic)
}

bic= function(model){
  n=length(model$fitted)
  p=length(model$x)
  sse = model$SSE
  bic= (log(n)/n)*p + n*log(sse/n)
  return(bic)
}

aicc = function(model){
  n = length(model$fitted)
  p = length(model$x)
  aicc = aic(model) + 2*p*(p+1)/(n-p-1)
  return(aicc)}

print(paste("AIC for models:",aic(out1),aic(out2),aic(out3),aic(out4)))
print(paste("BIC for models:",bic(out1),bic(out2),bic(out3),bic(out4)))
print(paste("AICc for models:",aicc(out1),aicc(out2),aicc(out3),aicc(out4)))

# 3. For the retained model, present the plots of data series, trend and seasonal components
out3$fitted
plot(out3)
plot(out3$x)
plot(out3$fitted)

# 4. Make out-of-sample forecast for 26 weeks with confidence bands. 
library("forecast")
out3_forecast = forecast:::forecast.HoltWinters(out3, h = 26, level = c(68, 95))	
plot(out3_forecast)
