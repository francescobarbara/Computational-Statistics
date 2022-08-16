

prop.table(table(df$female, df$docvis), 1)

#merge obs more than 10
reduced_docvis = numeric(length(df$female))
for (i in 1:length(df$female)){
  if (df$docvis[i] < 10){
    reduced_docvis[i] = df$docvis[i]
  }
  else{
    reduced_docvis[i] = 10
  }
}

#strong corr for gender

tab2 = prop.table(table(df$female, reduced_docvis), 1)
tab2 = tab2[1,]/ tab2[2,] - 1
barplot(tab2, beside=TRUE,
        col=c("skyblue"), xlab='Yealry visits', ylab='Increase in Proportion from male to female by gender', names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))

#correlation for married seems weak, maybe to a boxplot in this case. Easier

barplot(prop.table(table(df$married, reduced_docvis), 1), beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Yealr visits', ylab='Proportion by gender', names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))
#quanto è diversa la percentuale per sposati e non sposati. Doesn't seem to be highly sign
tab = prop.table(table(df$married, reduced_docvis), 1)
tab = tab[1,]/ tab[2,] -1
barplot(tab, beside=TRUE,
        col=c("skyblue"), xlab='Yealry visits', ylab='Difference in Proportion by marital status', names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))

#non white tend to go to the doctor less frequently, maybe do boxplot here too
prop.table(table(df$white, reduced_docvis), 1)


#dofference is huge, include barchart
barplot(prop.table(table(df$hispanic, reduced_docvis), 1), beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Yealr visits', ylab='Proportion by gender', names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))


par(mfrow = c(1,1))
#huge difference
barplot(prop.table(table(df$private, reduced_docvis), 1), beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Yealr visits', ylab='Proportion by gender', names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))





#Summary for continuous variables
summary(df$lincome)
summary(df$educ)

#Barchart for chronic
barplot(prop.table(table(df$chronic, 
                         reduced_docvis), 1), beside=TRUE,
        col=c("skyblue","steelblue"), xlab='Yealry doctor visits',
        ylab='Proportion by chronic', 
        names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))

#Table for ethnicities
table(df$white, df$hispanic)

#Subsetting dataframe based on number of visits
df_temporary1 = subset(df, docvis <= 2)
df_temporary2 = subset(df, docvis > 2 & docvis < 10)
df_temporary3 = subset(df, docvis >= 10)
summary(df_temporary1$age)
summary(df_temporary2$age)
summary(df_temporary3$age)

barplot(prop.table(table(df$female, reduced_docvis), 1), beside=TRUE,
          col=c("skyblue","steelblue"), xlab='Yealry doctor visits',
        ylab= 'Proportion by gender', 
        names.arg = c(0,1,2,3,4,5,6,7,8,9,"10+"))