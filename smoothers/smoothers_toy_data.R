df = read.table("http://www.stats.ox.ac.uk/~nicholls/CompStats/bpdat.txt", header = TRUE)
df = df[(order(df$home)),]

#bpxplot for the data
boxplot(df$home, df$hospital, ylab = "Blood pressure", names = c("Home", "Hospital"))

#we expect positive correlation
#we see hospital higher than home for most patients
plot(df$home, df$hospital, ylim = c(68,110), xlim = c(68,110), pch = 16, xlab = 'Home', ylab = 'Hospital')
lines(c(1:200),c(1:200),lty = 'dashed')
library(KernSmooth)

#We can start by thinking about kernel smoothers. We have very few data points and they are quite well-spaced so using a 'box 
# kernel' is not a good idea because the smooth will be  very discrete (step fnc) for whatever bandwidth we use
box_kernel1 <- ksmooth(df$home, df$hospital,kernel='box',bandwidth=10)
box_kernel2 <- ksmooth(df$home, df$hospital,kernel='box',bandwidth=25)
plot(df$home, df$hospital, ylim = c(68,110), xlim = c(68,110), pch = 16, xlab = 'Home', ylab = 'Hospital')
lines(box_kernel1,col=2,lwd=2)
lines(box_kernel2,col=3,lwd=2)
legend("topleft",c("h = 10","h = 25"),col=c(2, 3), pch = 16)

#We can fix this issue by having a continuous kernel (normal kernel), by picking two arbitrary bandwidths we can already see that
#the result seems quite sensible
normal_kernel1 <- ksmooth(df$home, df$hospital,kernel='normal',bandwidth=10)
normal_kernel2 <- ksmooth(df$home, df$hospital,kernel='normal',bandwidth=3)#plug 55
normal_kernel3 <- ksmooth(df$home, df$hospital,kernel='normal',bandwidth=30)
plot(df$home, df$hospital, ylim = c(68,110), xlim = c(68,110), pch = 16, xlab = 'Home', ylab = 'Hospital')
lines(normal_kernel1,col=2,lwd=2)
lines(normal_kernel2,col=3,lwd=1)
lines(normal_kernel3,col=4,lwd=1)
legend("topleft",c("h = 10","h = 3", "h = 30"),col=c(2, 3,4), pch = 16)

#optimal bandwidth
h <- c(1:50)
mse <- NULL
for(i in 1:length(h)){
  k <- ksmooth(df$home, df$hospital,kernel='normal',bandwidth=h[i], x.points = df$home)
  if (i == 10){
    print(k$y - df$hospital)
  }
  if (i == 50){
    print(k$y - df$hospital)
  }
  
  mse[i] <- mean((k$y-df$hospital)^2)
}


mse_cv <- NULL
for(i in 1:length(h)){
  d<-rep(NA,length(df$home))
  #for each h knock out each point in turn and get (observed-predicted)^2
  for(j in 1:length(df$home)){
    k <- ksmooth(df$home[-j],df$hospital[-j],kernel='normal',bandwidth=h[i],x.points = df$home)
    d[j]<-(k$y[j]-df$hospital[j])^2
  }
  mse_cv[i] <- mean(d) #the LOO-CV estimate of the MSE for this h-value
}


par(mar=c(4.5,4.5,1.5,1))
plot(h,mse,type='l', xlab='Bandwidth h',ylab='MSSE and RSS')#cex.lab=1.5,cex.axis=1.25
lines(h,mse_cv,lty=2); legend("topleft",c("MSSE LOOCV","RSS for fit"),lty=c(2,1))
#dev.off()

#optimal choice of bandwidth
(h.star<-h[which.min(mse_cv)])

plot(df$home, df$hospital, ylim = c(68,110), xlim = c(68,110), pch = 16, xlab = 'Home', ylab = 'Hospital')
lines(normal_kernel1,col=2,lwd=2)

#we see that we get a very similar smooth using optimal bandwidth criterion selection for a local regression, h = 4.3
k3 <- locpoly(df$home, df$hospital,bandwidth=dpill(df$home,df$hospital),degree=1)
lines(k3,col=1,lwd=1.5)
legend("topleft",c("NW smooth, h = 10","LLR smooth h = 4.3 "),col=c(2,1), pch = 16)




fnc = function(delta, df){
  df2 = data.frame(df)
  df2$diffs = df2$home - df2$hospital + delta
  df2$sign = rep(0,22)
  df2$sign[df2$diffs>0] = 1
  df2$diffs = abs(df2$home - df2$hospital + delta)
  df2$rank =  rank(df2$diffs)
  obs = sum(df2$sign*df2$rank)
  
  low = 0
  high = 0
  for(k in 1:10000){
    sim = numeric(22)
    for(j in 1:22){
      sim[j] = sample(c(0,1),1)
    }
    sim1 = sim*df2$rank
    sim2 = sum(sim1)
    
    if(sim2 <= obs){
      low = low +1
    }
    if(sim2 >= obs){
      high = high +1
    }
  }
  
  low = low/10000
  high = high/10000
  return(2*min(c(low,high)))
}

fnc(0.5,df)
fnc(6,df)
