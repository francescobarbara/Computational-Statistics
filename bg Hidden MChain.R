#alpha forward recursion

alpha_recursion = function(y, mu, A, B)
{
  K = length(mu)
  T = length(y)
  alpha = matrix(0,nrow=T,ncol=K)
  for (j in 1:K) alpha[1,j] = B[j,y[1]]*sum(A[,j]*mu)
  for (t in 2:T) for (j in 1:K) alpha[t,j] = B[j,y[t]]*sum(A[,j]*alpha[t-1,])
  return(alpha)
}

#beta backward recursion

beta_recursion = function(y, mu, A, B)
{
  K = length(mu)
  T = length(y)
  beta = matrix(0,nrow=T,ncol=K)
  for (j in 1:K) beta[T,j] = 1
  for (t in T:2) for (i in 1:K) beta[t-1,i] = sum(B[,y[t]]*A[i,]*beta[t,])
  return(beta)
}

#Viterbi's algorithm

viterbi = function(y, mu, A, B)
{
  K = length(mu)
  T = length(y)
  m = matrix(0,nrow=T,ncol=K)
  m0 = matrix(0,nrow=1,ncol=K)
  x.map = rep(0,T)
  # Backward
  for (i in 1:K) m[T,i] = 1
  for (t in T:2) for (i in 1:K) m[t-1,i] = max(B[,y[t]]*A[i,]*m[t,])
  for (i in 1:K) m0[i] = max(B[,y[1]]*A[i,]*m[1,])
  #Forward
  x0.map = which.max(mu*m0)
  x.map[1] = which.max(B[,y[1]]*A[x0.map,]*m[1,])
  for (t in 2:T) x.map[t] = which.max(B[,y[t]]*A[x.map[t-1],]*m[t,])
  return(x.map)
}


#genes data, transition probs, and emission probs

A = matrix(c(0.8,0.2,0.2,0.8),2, 2)
B = matrix(c(0.4,0.05,0.1,0.4,0.1,0.5,0.4,0.05),2,4)
T = 20
mu = c(0.5,0.5)
y = c(1,2,4,3,4,1,4,1,2,4,1,2,2,3,2,2,4,3,2,2)

alpha = alpha_recursion(y, mu, A, B)
beta = beta_recursion(y, mu, A, B)

filtering = matrix(data=NA,nrow=T,ncol=2)
for (t in 1:T) filtering[t,] = alpha[t,]/sum(alpha[t,])

smoothing = matrix(data=NA,nrow=T,ncol=2)
for (t in 1:T) smoothing[t,]=alpha[t,]*beta[t,]/sum(alpha[t,]*beta[t,])

max_post_est = viterbi(y, mu, A, B)

#plotting probabilities that locus t is a coding locus for each t
plot(c(1:20), smoothing[,1], pch = 16)
