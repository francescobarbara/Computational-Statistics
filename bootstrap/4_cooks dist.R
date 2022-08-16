par(mfrow = c(1,1))
plot(fitted(m1), xlab = 'Obs sorted by hhincome', ylab='Fitted Values')

par(mfrow = c(1,2))
p <- m1$rank
n <- nrow(model.frame(m1))
plot(influence(m1)$hat/(p/n), ylab='Leverage / (p/n)', xlab = 'Obs sorted by hhincome')
plot(cooks.distance(m1), xlab = 'Obs sorted by hhincome', ylab = "Cook's distance")


vec = which(cooks.distance(m1) >0.07) 

df[vec,1] #the influence is so high because their residuals are huge because number of visits are around 20

df2 = df[-vec,]

m3 = glm(docvis ~ ., data = df2, family = "poisson")
summary(m3)

m4 = glm(docvis ~ . - hispanic, data = df2, family = "poisson")
summary(m4)
#the Wald test confirms our previous variable selection (i.e. only remove hispanic)


#evita di rimetterli
par(mfrow = c(1,2))
p <- m4$rank
n <- nrow(model.frame(m1))
plot(influence(m4)$hat/(p/n), ylab='Leverage / (p/n)', xlab = 'Obs sorted by hhincome')
plot(cooks.distance(m4), xlab = 'Obs sorted by hhincome', ylab = "Cook's distance")


#obtaining bootstrap cov matrix for the coefficients of m4
B = 1000
n = length(df2$docvis)
beta_bootstrap33 = data.frame(matrix(0,B,9))
for (i in 1:B){
  sam = sample(1:n, replace = TRUE)
  df_temp = df2[sam,]
  model = glm(docvis ~ . -hispanic, data = df_temp, family = "poisson")
  beta_bootstrap33[i,] = model$coefficients
}
variance_matrix_bootstrap33 = var(beta_bootstrap33)

std_devs33 = numeric(9)
for(i in 1:9){
  std_devs33[i] = sqrt(variance_matrix_bootstrap33[i,i])
}

#we can see that the bootstrap est for the std. deviations of betas are 2 times higher than
#standard errors aggiungi notazione fine pagina frank

conf_int = matrix(0,9,2)
for (i in 1:9){
  conf_int[i, 1] = m1$coefficients[i] - std_devs[i]*qnorm(0.95)
  conf_int[i, 2] = m1$coefficients[i] + std_devs[i]*qnorm(0.95)
}


#we free from summary of model 4 that the least significant coeff is beta_educ is the least sign
#and that its estimated aclue is close to zero. Let's do a Wald test

2*(1 - pnorm(m1$coefficients[3]/std_devs[3]))
#0.14 so we delete educ

#do samething for age
2*(1 - pnorm(m1$coefficients[2]/std_devs[2]))
#0.08 with modified df2
