install.packages("quantmod") #
library("quantmod")
set.seed(154)
ftse100 <-new.env()
getSymbols("^FTSE", env = ftse100, src = "yahoo",from =as.Date("1984-03-01"), to =as.Date("2021-02-04"))
FTSE <- ftse100$FTSE

#creating time series of log returns
returns = diff(log(na.omit(FTSE$FTSE.Close)))
returns = returns[2:length(returns)]
names(returns)=c("FTSE logReturn")

#matrix form for ts
returns.mat = coredata(returns)
dates = index(returns)

plot(returns)
plot(returns["1987"]) #big shock occured in Oct
plot(returns["2008/2009"]) #lot of volatility in Oct-Nov 08
plot(returns["2019/"])
hist(returns, breaks = 50)
qqnorm(returns)
qqline(returns, col = "steelblue", lwd = 2) #it has fatter tails than a normal
ecdf.returns = ecdf(returns.mat)
plot(ecdf.returns)

#estimated quantiles (on the whole dataset)
q = c(0.1,0.25,0.5,0.75,0.99)
vec_quantiles = numeric(5)
#vec_quantiles = quantile(returns, probs = q, names = FALSE)
for (i in 1:5){
  vec_quantiles[i] = quantile(returns, probs = q[i], names = FALSE)
  
}

#outputs quantiles for B bootstrap samples
bootstrap_quantiles = function(B,q){
  bt_matrix = matrix(0,B,length(q))
  for (i in 1:B){
    samples = sample(returns, size = length(returns), replace = TRUE)
    bt_matrix[i,] = quantile(samples, probs = q, names = FALSE)
  }
  return(bt_matrix)
}

bt_quantiles = bootstrap_quantiles(10000, q)

#finding 99%-confidence interval for the median (no assumption on distribution)
interval = quantile(bt_quantiles[,3], probs = c(0.01,0.99), names = FALSE)
# [1] 0.0001653013 0.0006813506

bt_variance = var(bt_quantiles[,3])
normal_conf_int = c(median(returns.mat) - sqrt(bt_variance)*qnorm(0.99), median(returns.mat) + sqrt(bt_variance)*qnorm(0.99))
# [1] 0.0001836771 0.0007285793

