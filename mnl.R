#setwd('C:/Users/Chris/Documents/R/Thesis')
library(foreign) #essential for this
library(nnet) #essential for this because it contains the multinom function used
#to actually estimate the model 
library(reshape2) #essential for this

#Idea for code drawn from here: http://www.ats.ucla.edu/stat/r/dae/mlogit.htm 

#PRELIMINARY STUFF, LIKE SETTING BASELINE, PREPARING DATA, INTRODUCING FUNCTIONS

Tdata$MostImpObst <- as.factor(Tdata$MostImpObst) 
#because Tdata is currently coded as numeric
#but must be factor for the multinom function
Tdata$MostImpResp <- as.factor(Tdata$MostImpResp)
Tdata$MostImpObst2 <- relevel(Tdata$MostImpObst, ref = '13') #must first choose 
#reference category for the outcome as a baseline. This choose "no most important"
Tdata$MostImpResp2 <- relevel(Tdata$MostImpResp, ref = '12') #sets "no most important"
#as the baseline 
test <- multinom(Tdata$MostImpObst2 ~ Tdata$Char[,4]) #regression most important obstacle
#on the entire educational background variable, so every category within education
summary(test) #coefficients are log odds of choosing the numbered obstacle
#over the reference category, which is "No most important." 
z <- summary(test)$coefficients/summary(test)$standard.errors #to obtain p-values because
#R package does not do this automatically (Wald test)
p <- (1 - pnorm(abs(z), 0, 1))*2 
#two-tailed z test http://www.stat.yale.edu/Courses/1997-98/101/sigtest.htm




#A MODEL USING "ECONOMICS" AND "GERMAN" AS DUMMY VARIABLES

Tdata$econ <- Tdata$Char[,4] == 'Economics' 
test2 <- multinom(Tdata$MostImpObst2 ~ Tdata$econ) #with only a dummy variable set to 
#economics
test3 <- multinom(Tdata$MostImpResp2 ~ Tdata$econ)
Tdata$german <- Tdata$Char[,3] == 'Germany'
test4 <- multinom(Tdata$MostImpObst2 ~ Tdata$econ + Tdata$german) #model with 
#dummy variables for Germany as country and economics as education 
summary(test4)
z <- summary(test4)$coefficients/summary(test4)$standard.errors #to obtain p-values because
#R package does not do this automatically (Wald test)
p <- (1 - pnorm(abs(z), 0, 1))*2 
p #statistically significant results for a number of choices relative to baseline
exp(coef(test4)) #coefficients generally look far more reasonable now, w/ a couple
#big exceptions
head(pp <- fitted(test4)) #determines predicted probabilities of each outcome of 
#dependent variable 



#ORDERED LOGIT 
#source: http://www.ats.ucla.edu/stat/r/dae/ologit.htm
library(MASS)
m <- polr(as.factor(Tdata$NuLiObst[,1]) ~ Tdata$german + Tdata$econ, Hess=TRUE)
#Dependent variable must be as factor. HESS returns the observed information 
#matrix from optimization, which is used to get standard errors.
summary(m)
(ctable <- coef(summary(m))) #first step of significance testing
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 #2 of 3 
(ctable <- cbind(ctable, "p value" = p)) #3 of 3
exp(coef(m)) #issue w/interpretation is that these are odds of simply moving from 
#one Likert level to another if someone has an economics background or comes from Germany.
#Doesn't tell us odds of going from a specific level to another specific level 
#This is called the "proportional odds assumption" or the "parallel regression assumption"
#but this is an assumption that has to actually be tested to prove its validity.  
m2 <- multinom(as.factor(Tdata$NuLiObst[,1]) ~ Tdata$german + Tdata$econ)
summary(m2)
exp(coef(m2))







#LINGERING QUESTIONS

#question is how to interpret multiple coefficients (say, multiple dummies)
#and the cumulative effect on odds

#alternative to ordered logit could be perhaps running binary but using "important" vs
#"unimportant"

#the predicted probabilities command and ggplot graphing 

#significance testing with smaller sample sizes (not z-applicable)

