

par(mfrow=c(1,5))
barplot(prop.table(table(df$female, df$privins), 1)[,2], beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Female', ylab='Prop of private ins')
barplot(prop.table(table(df$white, df$privins), 1)[,2], beside=TRUE,
        col=c("skyblue","steelblue"), xlab='White', ylab='Prop of private ins')
barplot(prop.table(table(df$hisp, df$privins), 1)[,2], beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Hispanic', ylab='Prop of private ins')
barplot(prop.table(table(df$married, df$privins), 1)[,2], beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Married', ylab='Prop of private ins')
barplot(prop.table(table(df$retired, df$privins), 1)[,2], beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Retired', ylab='Prop of private ins')

#WE SEE MAJORITY HAVE ZERO LIMITATIONS. WE MIGHT CONSIDER USING A BINARY VARIABLE (ZERO LIM, MORE THAN ONE) INSTEAD.
#HOWEVER, SEE THAT PROPRTION OF PRIV INS IS SIMILAR AMONG 2 TO 5. MIGHT USE 3 CLASSES (0, 1, 2 OR MORE)
options(digits = 3)
prop.table(table(df$adl, df$privins), 1)
table(df$adl, df$privins)

#WE HAVE VERY FEW CHORNIC 7 OR 8, SO THAT DATA MIGHT BE UNRELIABLE, MIGHT CONSIDER SPLITTING IN 2 (OR THREE) GROUPS
prop.table(table(df$chronic, df$privins), 1)
table(df$chronic, df$privins)

table(df$educyrs, df$privins)#LOTS 12 HIGH SCHOOL
prop.table(table(df$educyrs, df$privins),1)#from the table we see that it decreases lineraly, so might make sense to treat it as a cts variable expect negative coeff
summary(df$ed)

par(mfrow = c(1,3))
plot(prop.table(table(df$adl, df$privins), 1)[,2] ~ c(0:5), pch = 16, xlab = 'Limitations', ylab = 'Prop of private insurance')
plot(prop.table(table(df$chronic, df$privins), 1)[,2] ~ c(0:8), pch = 16, xlab = 'Chronic Conditions', ylab = 'Prop of private insurance')
plot(prop.table(table(df$educyrs, df$privins),1)[,2] ~ c(3:17), pch = 16, xlab = 'Education Years', ylab = 'Prop of private insurance')

summary(df$hhincome)
summary(df$age)
#PEOPLE WHO PURCHASE PRIVATE INSURANCE SEEM TO BE WEALTHIER ON AVERAGE. MEANWHILE THERE IS NO
#SIGNIFICANT DIFFERENCE FOR WHAT CONCERNS AGE. ALSO, THERE ARE MANY DATAPOINTS FOR HHINCOME
#FALLING OUTSIDE 1.5 INTERQUARTILE RANGE, THAT MIGHT BE A CAUSE OF CONCERN WHEN ASSESSING THE QUALITY OF OUR MODEL
par(mfrow = c(1,3))
boxplot(df$age ~ df$privins  ,xlab = "Private Insurance", ylab = 'Age')
boxplot(df$hhincome ~ df$privins  ,xlab = "Private Insurance", ylab = 'Household Income')
boxplot(df$lincome ~ df$privins  ,xlab = "Private Insurance", ylab = 'log(Household Income)')


#ADD NEW COLS TO DATASET
df2 = within(hins, {
  adl_f = factor(adl)
  chronic_f = factor(chronic)
  adl_b = 1*(adl >= 1)
  chronic_b = 1*(chronic >= 1)
  adl_bb = 1*(adl>=2)
  
  adl2 = 1*(adl >=2)
  chronic2 = 1*(chronic >= 2)
  chronic3 = 1*(chronic >= 3)
  chronic4 = 1*(chronic >= 4)})


#SECTION 2
#COMMENTS ON WHY LOGISTIC REGR

m0 = glm(privins ~ age + female + white + hisp + married + educyrs + hhincome + lincome + retired + adl_f + chronic_f, data = df2, family = "binomial")
summary(m0)

m1 = glm(privins ~ age + female + white + hisp + married + educyrs + hhincome + lincome + retired + adl + chronic, data = df2, family = "binomial")
summary(m1)

m2 = glm(privins ~ age + female + white + hisp + married + educyrs + hhincome + lincome + retired + adl_b + chronic_b, data = df2, family = "binomial")
summary(m2)
#why this choice of binary variables, in adl 90% of observations are in adl = 0, for chronic majority have between 0 and 3, we see from table
#that 1 to 3 have similar percentages
#SIMILARLY TO WHAT DONE WITH FULL MODEL, IF WE TRY TO INCLUDE A SUBSET OF THE BINARY INDICATORS, THE P-VALUE IS NEVER SIGNIFICANT
#(BECAUSE THE CONTRIBUTION TO THE THE LOG LIKELIHOOD IS TO SMALL TO GIVE SMALL P-VALUES FOR THE CI SQ TESTS)

#SINCE M1 AND M2 HAVE THE SAME DIMENSION OF THE PARATìMETER SPACE, AND M2 HAS SMALLER RESIDUAL DEVIANCE, WE DECIDE TO TREAT chronic and adl as binary

#MODIFYING DATASET:
df3 <-within(df, {
  adl <- 1*(adl>=1)
  chronic <- 1*(chronic >=1)})

#GOING BACK TO OUR SECOND PROBLEM, THE HIGH HHINCOME OBS.
#IF WE FIND VIA BRUTE FORCE

mod_df3 <- within(df3, {
  y <- privins
  privins <- NULL
  
})
library(leaps)
library(bestglm)

bic_best_glm <-
  bestglm(Xy = mod_df3,
          family = binomial,
          IC = "BIC",                
          method = "exhaustive")
aic_best_glm <-
  bestglm(Xy = mod_df3,
          family = binomial,
          IC = "AIC",              
          method = "exhaustive")
bic_best_glm$BestModels
aic_best_glm$BestModels

#BEST MODEL FOR BIC IS 1+HISP+EDUCYRS+HHINCOME+LINCOME, WITHOUT FURTHER ADO WE CAN SEE FROM THE DIAGNOSTIC PLOTS WHAT GOES WRONG IN THE MODEL
#THE NEGATIVE COEFFICIENTS OF HHINCOME OUTWEIGHTS THE POSITIVE ONE FOR LARGE OBSERVATIONS AND HENCE, WE END UP WITH VERY LOW PREDICTIONS 
#EVEN THOUGH THE MAJORITY OF OBS WITH VERY HIGH INCOME HAVE PRIVATE INSURANCE
m7 = glm(privins ~ hisp + educyrs  + lincome+ hhincome, data = df3, family = "binomial" )
summary(m7)
m8 = glm(privins ~ ., data = df3, family = "binomial" )
summary(m8)


vec = numeric(31)
x = numeric(31)
for (i in 1:31){
  t = sum(df$privins[(100*(i-1)):(100*i - 1)])/100
  vec[i] = t
  x[i] = 100*i
} 

par(mfrow = c(1,1))
plot(fitted(m7), xlab = 'Obs sorted by hhincome', ylab='Fitted Values')
points(x,vec, pch = 19, col = 'red', cex = 1.5)
par(mfrow = c(1,2))
p <- m7$rank
n <- nrow(model.frame(m7))
plot(influence(m7)$hat/(p/n), ylab='Leverage / (p/n)', xlab = 'Obs sorted by hhincome')
plot(cooks.distance(m7), xlab = 'Obs sorted by hhincome', ylab = "Cook's distance")
abline(h=8/(n-2*p),lty=2)

#WE HAVE TWO OPTIONS, EITHER DELETE HHINCOME FROM OUR MODEL OR MODIFY THE OBSERVATIONS THAT WERE OUTLIERS IN the box plot.
#BOTH IN OUR FULL MODEL AND IN THE ONE WITH SMALLEST BIC WE CAN SEE THAT HHINCOME IS HIGHLY SIGNIFICANT
#THIS SUGGESTS THAT OUR SECOND OPTION MIGHT WORK BETTER.SO WE SET EVERY VALUE ABOVE MEAN + 1.5STD TO THIS VALUE
#THIS GIVES A LOWER RESIDUAL DEVIANCE SO IT IS TO BE PREFERRED

thold = mean(df3$hhincome)+ 1.5*sqrt(var(df3$hhincome))

df4 <-within(df3, {
  n_hhincome = hhincome*(hhincome <= thold)+ thold*(hhincome > thold)
  n_lincome = log(n_hhincome)})

#WE GET SMALLER RESID DEVIANCE BUT SAME DIMENSION OF PARAMETER SPACE, THIS SHOWS US THAT
#IT IS A GOOD WA TO TREAT THE HIGH VALUES WE SAW IN HHINCOME. ALSO THINK OF LAW OF DIMINISHING RETURNS

m9 <- glm(privins~ .-n_hhincome - n_lincome , data = df4, family = "binomial")
summary(m9)
m10 = glm(privins~ . -hhincome - lincome, data = df4, family = "binomial"  )
summary(m10)

par(mfrow = c(1,1))
plot(fitted(m10), xlab = 'Obs sorted by hhincome', ylab='Fitted Values')
points(x,vec, pch = 19, col = 'red', cex = 1.5)
#par(mfrow = c(1,2))
#p <- m9$rank
#n <- nrow(model.frame(m10))
#plot(influence(m10)$hat/(p/n), ylab='Leverage / (p/n)', xlab = 'Obs sorted by hhincome') #MAYBE DELETE THE THREE PLOTS, LEAVE ONLY RED ONE
#plot(cooks.distance(m10), xlab = 'Obs sorted by hhincome', ylab = "Cook's distance")
#abline(h=8/(n-2*p),lty=2)
#plot(rstandard(m10))

#THESE VALUES MAKE SENSE FOR FITTED VALUES
#remodifying dataset
df5 <-within(df4, {
  n_hhincome = hhincome*(hhincome <= thold)+ thold*(hhincome > thold)
  n_lincome = log(n_hhincome)
  hhincome = NULL
  lincome = NULL})

mod_df5 <- within(df5, {
  y <- privins
  privins <- NULL
})


bbic_best_glm <-
  bestglm(Xy = mod_df5,
          family = binomial,
          IC = "BIC",                
          method = "exhaustive")
aaic_best_glm <-
  bestglm(Xy = mod_df5,
          family = binomial,
          IC = "AIC",              
          method = "exhaustive")
bbic_best_glm$BestModels
aaic_best_glm$BestModels

m11 = glm(privins~ ., data = df5, family = "binomial"  )
summary(m11) #res dev 3676.8
m12 = glm(privins~ . - female - white - married - retired, data = df5, family = "binomial"  )
summary(m12) #res dev 3683.0
1 - pchisq(6.2, df = 4) #0.19 so we can eliminate them
#in m12 age is not significant, the test used is however a Wald test, however
#the likelihood ratio procedure has the best agreement between the true and asymptotic distributions.
#so we do a likelihood ratio test to decide whether to keep it

#quello sopra e inutile. Now in m12
m13  = glm(privins~ . - age - female - white - married - retired, data = df5, family = "binomial"  )
summary(m13) #res dev 3686.2
1 - pchisq(3.2, df = 1) #so exclude age, as slightly above 0.05 cutoff

m14 = glm(privins~ . - adl - age - female - white - married - retired, data = df5, family = "binomial"  )
summary(m14) #res dev 3688.4
1 - pchisq(3.2, df = 1) #so exclude adl, as slightly above 0.05 cutoff

#we just saw how some variables could have very easily been included or not in the model depending on our discretion.
#SINCE STEPWISE MODEL SELECTION WE CARRIED OUT IS ORDER-DEPENDENT, to check that WHETHER TEHRE ARE BETTER MODELS
#WE CAN FIND THE BEST MODEL FOR ANY FIXED NUMBER OF PARAMETERS AND COMPUTE THE RELATIVE AIC AND BIC MEASURE
#AS WE FAVOUR INTERPRETABILITY (WE WANT TO PENALISE COMPLEX MODELS MORE), WE PICK BIC AS OUR CRITERION, FROM TABLE BELOW
#WE SEE THAT THE MODEL CHOSEN IS THE BEST ONE
#The best model according to bic had only 3 parameters, while for Aic had 9. I think our model with 5 explanatory variables, 
#is a good compromise between the two

#add tables

par(mfrow = c(1,1))
bic_vec = bbic_best_glm$Subsets[,14]
aic_vec = aaic_best_glm$Subsets[,14]
bic_vec = bic_vec[2:12]
aic_vec = aic_vec[2:12]
y = c(bic_vec, aic_vec)
x = rep(c(1:11),2)
plot(x,y, pch = 16, col = c(rep(c(1),11), rep(c(2),11)), ylab = '', xlab = 'Number of explanatory variables')
legend("topright", c( "AIC", "BIC") , col = c(2,1) , pch = 16)


par(mfrow = c(1,1))
#plot(fitted(m14), xlab = 'Obs sorted by hhincome', ylab='Fitted Values') #delete this one
points(x,vec, pch = 19, col = 'red', cex = 1.5)
par(mfrow = c(1,2))
p <- m14$rank
n <- nrow(model.frame(m14))
plot(influence(m14)$hat/(p/n), ylab='Leverage / (p/n)', xlab = 'Obs sorted by hhincome')
plot(cooks.distance(m14), xlab = 'Obs sorted by hhincome', ylab = "Cook's distance")
abline(h=8/(n-2*p),lty=2)
plot(rstandard(m14), ylab = "Standardised deviance residuals", xlab = "Obs sorted by hhincome")
#the st residuals (as we are modelling bernoulli rv are not very helpful), but still they have 
#approx unit variance which is good. WE see from Cook's distances plot that there are quite
#a few obs with low hhincome and high cook's distance. THis has a very natural explanation:
#our pi_hat when hhincome is low is around 0.1 hence, the few obs with ins = 1, when removed
#will change the parameters significantly
#On the other hand, as hhincome grows pi_hat approaches 0.5 and so both residuals and cook's distances are not so axtreme
#the diagnostics plots look reasonable, when age. Indeed the pts with cook's distance aove 0.006 have income less thatn $2500
#but have purchased private insurance nonetheless. So there is no reason to delete them. Also the patterns in the leverage plot have a very natural
#explanation. Since most of our variables are binary, the effect of influence (if we fix those) depends only on income and education.
#and so, as our observations in the plot are sorted by income, the patterns emerge. As a matter of fact, all observations with leverage over four times
#bigger than the average are hispanic ('the line in the lev plot')
 

v = which(cooks.distance(m14) > 0.006)
df5[v,]
w = which(influence(m14)$hat/(p/n) > 4)
df5[w,]

options(digits = 2)
beta <- summary(m14)$coef[2:6,1]
se <- summary(m14)$coef[2:6,2]
cval <- qnorm(0.975)
lower <- beta-cval*se
upper <- beta+cval*se
ci95 <- cbind(lower,beta,upper)
