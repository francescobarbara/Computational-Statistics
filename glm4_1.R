beta = c(-1.9253, 0.3126, -0.6693, -0.1725)
X = matrix(c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0),6,4 )
pi = exp(X%*%beta)/(1+exp(X%*%beta))
m = c(458,147,494,384,127,464)
mu = m*pi
w = mu*(m-mu)/m
ww = as.vector(w)
W = diag(ww)
I = t(X)%*%W%*%X


bf <- data.frame(disease=c(77,19,47,48,16,31),
                 healthy=c(381,128,447,336,111,433),
                 gender=c('M','M','M','F','F','F'),
                 feed=c('Bottle','Suppl','Breast','Bottle','Suppl','Breast'))
X <- model.matrix(cbind(bf$disease,bf$healthy) ~ bf$gender + bf$feed)

y = bf$disease
meann = sum(bf$disease)/6
mu = rep(c(meann),6)
m = bf$disease + bf$healthy
W = diag(c(mu*(m-mu)/m))
eta = log(mu/(m - mu))
z = eta + m/(mu*(m-mu))*(y - mu)



k <- 1
dif <- 1
beta <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%z
while((k < 1000) && (dif>1e-05)){
  k <- k+1
  betao <- beta
  eta <- X %*% beta
  mu <- m*exp(eta)/(1 + exp(eta))
  z <- eta + (y - mu)/mu
  W <- diag(c(mu))
  beta <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%z
  dif <- abs(betao-beta)
}

matrix = solve(t(X)%*%W%*%X)
standard_err = numeric(4)
for (i in c(1:4)){
  standard_err[i] = sqrt(matrix[i,i])
}