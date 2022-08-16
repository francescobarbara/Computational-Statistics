set.seed(540)
n = length(df$docvis)

#Fitting full GLM
m0 = glm(docvis ~ ., data = df, family = "poisson")
summary(m0)

#Dropping hispanic
m1 = glm(docvis ~ . -hispanic, data = df, family = "poisson")

#Chisq test
1 - pchisq(1.4, 1) #0.23 so we have no evidence against H0
summary(m1)


#Bootstrap variance algorithm
B = 1000

beta_bootstrap = data.frame(matrix(0,B,9))
for (i in 1:B){
  sam = sample(1:n, replace = TRUE)
  df_temp = df[sam,]
  model = glm(docvis ~ . -hispanic, data = df_temp, family = "poisson")
  beta_bootstrap[i,] = model$coefficients
}
variance_matrix_bootstrap = var(beta_bootstrap)

options(digits = 3)
variance_matrix_bootstrap

#Taking the square root of the diagonal entries 
#to get standard errors
std_devs = numeric(9)
for(i in 1:9){
  std_devs[i] = sqrt(variance_matrix_bootstrap[i,i])
}

std_devs

#90% confidence intervals for beta_{i}
conf_int = matrix(0,9,2)
for (i in 1:9){
  conf_int[i, 1] = m1$coefficients[i] - std_devs[i]*qnorm(0.95)
  conf_int[i, 2] = m1$coefficients[i] + std_devs[i]*qnorm(0.95)
}


#testing if we can simplify our model.
#let h_0: beduc = b_lincome = b_married = 0 h_1: 
ml_cov = matrix(c(variance_matrix_bootstrap[3,3], 
                  variance_matrix_bootstrap[3,4], variance_matrix_bootstrap[3,6],
                  variance_matrix_bootstrap[3,4],
  variance_matrix_bootstrap[4,4], variance_matrix_bootstrap[4,6], 
  variance_matrix_bootstrap[3,6], variance_matrix_bootstrap[4,6], 
  variance_matrix_bootstrap[6,6]), 3,3)
v1 = c(m1$coefficients[3], m1$coefficients[4], m1$coefficients[6])
t(v1)%*% solve(ml_cov) %*% v1

1-pchisq(4.36, df = 3) #so can delete married and lincome and educ

mm2 = glm(docvis ~ . -hispanic - educ - 
            married - lincome, data = df, family = "poisson")
summary(mm2)

#Calculating bootstrap covariance for simplified model
B = 1000

beta_bootstrap2 = data.frame(matrix(0,B,6))
for (i in 1:B){
  sam = sample(1:n, replace = TRUE)
  df_temp = df[sam,]
  model = glm(docvis ~ . -hispanic- educ - 
                married - lincome, data = df_temp, family = "poisson")
  beta_bootstrap2[i,] = model$coefficients
}
variance_matrix_bootstrap2 = var(beta_bootstrap2)

std_devs2 = numeric(6)
for(i in 1:6){
  std_devs2[i] = sqrt(variance_matrix_bootstrap2[i,i])
}

std_devs2

options(digits = 3)
variance_matrix_bootstrap2
summary(mm2)

options(digits = 5)
tests = numeric(6)
tests[1] = 2*(1- pnorm(-mm2$coefficients[1]/std_devs2[1]))
for (i in 2:6){
  tests[i] = 2*(1 - pnorm(mm2$coefficients[i]/std_devs2[i]))  
}

#Wald tests with bootstra st. err.
tests 
