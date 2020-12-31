racetimes <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/racetimes.csv", stringsAsFactors = TRUE)

#INITIAL EXPLORATION


#WE SEE THAT FUNCTION LOOKS CONVEX, MAKES SENSE AS ATHLETES GET TIRED
plot(time ~ dist, data = racetimes, main = "Time vs Distance", pch = as.numeric(racetimes$sex) + 15, col = as.numeric(racetimes$category))
legend("topright", c( "Long F", "Medium F", "Short F", "Long M", "Medium M", "Short M") , col = rep(c(1,2, 3), 2) , pch = c(16,16,16,17,17,17))

#SOME DECENT CORRELATION, NOT AS STRONG AS WITH DIST
plot(time ~ climb, data = racetimes, main = "Time vs Climb", pch = as.numeric(racetimes$sex) + 15, col = as.numeric(racetimes$category))
legend("topright", c( "Long F", "Medium F", "Short F", "Long M", "Medium M", "Short M") , col = rep(c(1,2, 3), 2) , pch = c(16,16,16,17,17,17))
plot( climb ~ dist , data = racetimes, main = "Climb vs Distance", pch = 16)

#INDICATES MULTIPLICATIVE RELATION/EFFECT
sex_difference = rep(c(0), 34)
for (i in 1:35){
  sex_difference[i] = - racetimes$time[34+i] + racetimes$time[i]
}
distance = racetimes$dist[1:35]
plot(sex_difference ~ distance, main = "Sex difference vs Distance", pch = 16 )



#MODIFYING DATASET


racetimes <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/racetimes.csv", stringsAsFactors = TRUE)
racetimes$sex = as.numeric(racetimes$sex == 'F')
racetimes$medium = as.numeric(racetimes$category == 'Medium')
racetimes$long = as.numeric(racetimes$category == 'Long') #MODIFYING AS R GIVES SOME PROBLEMS WITH CATEGORICAL VAARS WHEN INTERCEPT NO TINCLUDED
racetimes$cateogry = NULL

#PROOF WITH A LINEAR MODEL WITH NO INTERACTIONS WE SEE RESIDUALS HAVE UNEVEN VARIANCE

lm1 = lm(time ~ ., data = racetimes)
plot(fitted(lm1),rstudent(lm1),xlab="Fitted values",pch=16, main = "Studentised Residuals vs Fitted Values", 
     ylab = "Studentised Residuals")
abline(0,0,lty = 2)



#physical condiseration EITHER DIST OR DIST^2 ARE PROP TO VARIANCE, ^2 case better and even more physical sense(weather example)
w = racetimes$dist
z = lm(time ~ dist*climb*sex -1, data = racetimes, weight = (1/w))
x = lm(time ~ dist*climb*sex -1, data = racetimes, weight = (1/w)^2)
plot(fitted(z),rstudent(z),xlab="Fitted values",pch=16, main = "Variance ~ Distance, Residuals plot ", 
     ylab = "Studentised Residuals")
abline(0,0,lty = 2)
plot(fitted(x),rstudent(x),xlab="Fitted values",pch=16, main = "Variance ~ Distance^2, Residuals plot ", 
     ylab = "Studentised Residuals")
abline(0,0,lty = 2)

#chosen our weight function, and our starting natural model, test hp of no intercept with F test
y = lm(time ~ dist*climb*sex, data = racetimes, weight = 1/w^2)
anova(x,y)
#VALUE 0.5, TAKING INTO CONSIDERATION PHYSICAL

#cATEGORY INFORMATION IS INCLUDED IN DIST TEST NO CATEGORY WITH F TEST
z =  lm(time ~ medium + long + dist*climb*sex -1, data = racetimes, weight = (1/w)^2)
anova(x,z) #AROUND THE 5% THRESHOLD 3.5%, FAVOURING INTERPRETABILITY I DECIDE NOT TO INCLUDE THEM

#NOW BACKWARDS SELECTION
anova(x)
#WE SEE CLIMB:SEX CAN BE EXCLUDED, SAME FOR TRIPLE TERM


lmfit1 = lm(time ~ dist + climb + sex + dist:climb + dist:sex -1, data = racetimes, weight = (1/w)^2)
#4 POINTS (RACES 14,20,48,53) HAVE ABS(ST RES) > 2 SO SHOW MISFIT AND WE WILL NEED TO INVESTIGATE AS THE COULD POTENTIALLY BE OUTLIERS
plot(fitted(lmfit1),rstudent(lmfit1),xlab="Fitted values",pch=16, main = "Residuals plot ", 
     ylab = "Studentised Residuals")
abline(0,0,lty = 2)
qqnorm(rstudent(lmfit1), pch = 16) #comment briefly saying it looks ok
qqline(rstudent(lmfit1))
which(abs(rstudent(lmfit1)) >2)


#OUTLIER ANALYSIS
n= 68
p = 5
plot(hatvalues(lmfit1), ylabel = "Leverage Component", main = "Leverage", pch = 16)
abline(2*p/n, 0, lty = 2)
plot(cooks.distance(lmfit1), ylabel = "Cook's Distance", main = "Influence", pch = 16)
abline(8/(n - 2*p), 0, lty = 2)
which(abs(cooks.distance(lmfit1)) >0.15) #14,48 small and from the same race nice!


#DELETE OUTLIERS
new = racetimes[-c(14,48),]
ww = w[-c(14,48)]
lmfit2 = lm(time ~ dist + climb + sex + dist:climb + dist:sex -1, data = new, weight = (1/ww)^2)

#DO BIC and aic, choose bic as we want to favour interpretability
#aggiungi tavola dei which
library(leaps)
b <- regsubsets(time ~ dist + climb + sex + dist:climb + dist:sex -1, data = new, weight = (1/ww)^2)
rs <- summary(b)
rs$which
AIC <- 66*log(rs$rss/66) + c(2:5)*2
plot(AIC ~ I(2:5), main = "AIC",ylab = "AIC", xlab = "Number of Predictors", pch = 16)
BIC <- 66*log(rs$rss/66) + c(2:5)*2*log(66)
plot(BIC ~ I(2:5), main = "BIC", ylab = "BIC", xlab = "Number of Predictors", pch = 16) #WE SEE BEST IS DIST + D:C + D:S AS EXPECTED!

#NEW OUTLIER ANALYSIS
lmfit3 = lm(time ~ dist + dist:climb + dist:sex -1, data = new, weight = (1/ww)^2)
n = 66
p = 3
plot(hatvalues(lmfit3),  main = "Leverage", ylabel = "Leverage Component", pch = 16)
abline(2*p/n, 0, lty = 2)
plot(cooks.distance(lmfit3),main = "Influence", ylabel = "Cook's Distance",  pch = 16)
abline(8/(n - 2*p), 0, lty = 2)
which(abs(cooks.distance(lmfit3)) >0.13) #only one outlier slightly above the threshold which we decide to keep


#BETA HAT FOR LMFIT3
#MATRIX FOR THE 4 NEW OBSERVATIONS
coeff = c(4.088816624, 0.001159558, 0.995837017)
mat = matrix (c(5,2,8,50,330,200,2000,3000,1,1,1,1), ncol = 3, nrow= 4)

#PREDICTED TIMES FOR THE 4 OBS USING MODEL3
out = numeric(4)
for (i in 1:4){
  out[i] = (mat[i,1]*coeff[1] + mat[i,2]*mat[i,1]*coeff[2] + mat[i,3]*mat[i][1]*coeff[3])
  
}

#FITTED VALUES Y PRIME, WEIGTHED DATA MATRIX ETC
out_p = out / mat[,1]
mat_p = diag(1/mat[,1]) %*% mat  
RSS_p = sum(((1/ww)^2)*residuals(lmfit1)^2)
S_p = sqrt(RSS_p/66)
X = (data.matrix(new)[,c(3,4,5)])
X_p = diag(1/X[,1]) %*% X
XtX_p_inv = solve(t(X_p)%*%X_p)

#SQRT OF 1 MINUS ETC
value = numeric(4)
for (i in 1:4){
  value[i] = S_p * sqrt(1 + t(mat_p[i,]) %*% XtX_p_inv %*% (mat_p[i,]))
}


#these are the intervals for y prime
interval_low = out_p - qt(0.975, df = 63) *value
interval_high =out_p + qt(0.975, df = 63) *value

#PREDICTION INTERVALS AND PREDICTED VALUES
i_l = mat[,1] * interval_low
i_h = mat[,1] * interval_high
out
i_l
i_h
