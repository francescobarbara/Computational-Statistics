

x = c(8.3, 8.3, 12.1, 12.1, 17.0, 17.0, 17.0, 24.3, 24.3, 24.3, 33.6)
y = c(224, 312, 362, 521, 640, 539, 728, 945, 738, 759, 663)
plot(x,y, pch = 16)
m1 = lm(y~x)


alpha_hat = 201.77
beta_hat = 21.24
epsilon_hat = m1$residuals
B = 10000
alphas = numeric(B)
betas = numeric(B)
for(i in 1:B){
  indices = sample(c(1:11), size = 11, replace = TRUE)
  x_s = numeric(11)
  y_s = numeric(11)
  for (j in 1:11){
    x_s[j] = x[indices[j]]
    y_s[j] = y[indices[j]]
  }
  m = lm(x_s~y_s)
  alphas[i] = m$coefficients[1]
  betas[i] = m$coefficients[2]
}

sigma1 = 1/B*sum((alphas - 1/B*sum(alphas))^2)  
#non-parametric paired bootstrap estimate for variance of alpha is:
#9.770519

sigma2 = 1/B*sum((betas - 1/B*sum(betas))^2)
#non-parametric paired bootstrap estimate for variance of beta is:
#3.648219e-05

B = 10000
alphas2 = numeric(B)
betas2 = numeric(B)
for(i in 1:B){
  epsilon_s = sample(epsilon_hat, size = 11, replace = TRUE)
  for (j in 1:11){
    y_s[j] = alpha_hat + beta_hat*x[j] + epsilon_s[j]
  }
  m = lm(x_s~y_s)
  alphas2[i] = m$coefficients[1]
  betas2[i] = m$coefficients[2]
}

sigma1 = 1/B*sum((alphas2 - 1/B*sum(alphas2))^2)  
#semi-parametric bootstrap estimate for variance of alpha


sigma2 = 1/B*sum((betas2 - 1/B*sum(betas2))^2)
#semi-parametric bootstrap estimate for variance of beta 





x = rexp(10,1)
  
fnc = function(vec){
  return(log((10/sum(vec))))
}
omega_hat = fnc(x)
B = 10000
omegas = numeric(B)
for(i in 1:B){
  x_s = sample(x,10,replace = TRUE)
  omegas[i] = fnc(x_s)
}
variance_est = 1/B*(sum((omegas - mean(omegas))^2))
#non parametric bootstrap estimate for variance is 0.066
alpha = 0.05
c_i = c(2*fnc(x) - quantile(omegas, 1 - alpha/2), 2*fnc(x) - quantile(omegas, alpha/2))
#confidence interval for omega (-0.739,0.275) it contains omega_hat = -0.127 in my case
#and also the true value of omega which is zero


B = 10000
omegas = numeric(B)
for(i in 1:B){
  x_s = rexp(10, exp(omega_hat))
  omegas[i] = fnc(x_s)
}
variance_est_2 = 1/B*(sum((omegas - mean(omegas))^2))
#parametric bootstrap estimate for variance is 0.104
alpha = 0.05
c_i_2 = c(2*fnc(x) - quantile(omegas, 1 - alpha/2), 2*fnc(x) - quantile(omegas, alpha/2))
#confidence interval for omega (-0.854, 0.418) it contains omega_hat = -0.127 in my case
#and also the true value of omega which is zero



B = 10000
omegas = numeric(B)
for(i in 1:B){
  x_s = rexp(10, 1)
  omegas[i] = fnc(x_s)
}
variance_est_3 = 1/B*(sum((omegas - mean(omegas))^2))
#parametric bootstrap estimate for variance is 0.105
alpha = 0.05
c_i_3 = c(2*fnc(x) - quantile(omegas, 1 - alpha/2), 2*fnc(x) - quantile(omegas, alpha/2))
#confidence interval for omega (-0.997, 0.281) it contains omega_hat = -0.127 in my case
#and also the true value of omega which is zero  


