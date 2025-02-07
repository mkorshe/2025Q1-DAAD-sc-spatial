#Setting working directory
#setwd("D:/R training/regression analysis")
#Or Session - Set working directory - Choose directory

#Installing the required packages
#install.packages("tidyverse")
#install.packages("stargazer")
#install.packages("psych")
#install.packages("GGally")
#install.packages("car")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages('corrplot')
#install.packages('mfx')
#install.packages('fixest')
#To run code put the cursor on the line with code and press Ctrl + Enter or "Run" button

#Loading the required packages
library(tidyverse) # Data manipulation & cleaning
library(stargazer) #Regression & summary statistics tables
library(psych) #summary statistics
library(GGally) #Pairwise correlation plot
library(corrplot) #Alternative way to do pairwise correlation plot
library(car) #Additional commands for regression analysis
library(lmtest) #For robust standard errors
library(sandwich) #For robust standard errors
library(mfx) #Marginal errors for logit and probit regressions
library(fixest) #Faster regression analysis
#loading the data
#more details about the dataset available at: https://www.kaggle.com/marklvl/bike-sharing-dataset
bike_share <- read_csv("6.Introduction to Regression analysis/day.csv")

####Exercise 1####
#Examining the data
#Note: usually do more exploratory analysis before regression
#but we do not have time
names(bike_share)
summary(bike_share)
describe(bike_share)
str(bike_share)

#Making a linear regression
?lm
model1 <- lm(cnt~temp+hum+windspeed, data = bike_share)
summary(model1)

#sometimes relationship is not linear
#quadratic relationship 
ggplot(bike_share)+geom_smooth(aes(temp, cnt))
bike_share$temp_sq <- bike_share$temp*bike_share$temp
model2 <- lm(cnt~temp+temp_sq+hum+windspeed, data = bike_share)
summary(model2)

###Same with fixest package
model2_fixest <- feols(cnt~temp+temp_sq+hum+windspeed, data = bike_share)
summary(model2_fixest)
####Exercise 2####
# adding binary and categorical data
unique(bike_share$workingday)
unique(bike_share$season)
#note that coefficients on the other variables changed. 
#We will discuss why it happened later.
#NOTE: here I add categorical data incorrectly
model3 <- lm(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+season, data = bike_share)
summary(model3)

#Now making sure categorical data is added correctly
model4 <- lm(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+factor(season), data = bike_share)
summary(model4)

#interaction variables#
#
model5 <- lm(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+factor(season)+holiday*factor(season), data = bike_share)
summary(model5)
model6 <- lm(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+factor(season)+holiday*temp, data = bike_share)
summary(model6)

##Same with fixest package
model6_fixest <- feols(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+factor(season)+holiday*temp, data = bike_share)
summary(model6_fixest)

#exporting the results

stargazer(model2, model4, type = "text")


stargazer(model2, model4, type = "html", out = "tables.doc")

stargazer(model2, model4)

##Alternative: etable command from fixest package
?etable
etable(model2_fixest, model6_fixest)
####Exercise 3####

###multicollinearity
#rule of thumb: vif less than 10 BUT if you have a reason it is okay
model7 <- lm(cnt~temp+atemp+hum+windspeed, data = bike_share)
summary(model7)
summary(model1)
cor(bike_share$temp, bike_share$atemp)
vif(model7)
#let's check multicollinearity of the previous model
vif(model1)
vif(model2)
vif(model4)

#an easy way to check correlation between several variables at the same time
bike_share_small <- bike_share |> dplyr::select(temp, hum, windspeed, atemp)
ggpairs(bike_share_small)
?corrplot
corrplot(cor(bike_share_small), method = 'number', type = 'lower')

##corr with temp and atemp is very high - shouldn't include them together

#what happens when you have a perfect multicollinearity

bike_share$spring <- bike_share$season==1
bike_share$summer <- bike_share$season==2
bike_share$fall <- bike_share$season==3
bike_share$winter <- bike_share$season==4

model8 <- lm(cnt~temp+temp_sq+atemp+hum+windspeed+holiday+workingday+spring+summer+fall+winter, data = bike_share)
summary(model8)

###Heteroskedasticity##
#Visual inspection
ggplot(bike_share)+geom_point(aes(temp, cnt))
#Breusch-Pagan test
?bptest
bptest(model4)
#p-value very small. reject null hypothesis of no heteroskedasticity
summary(model4)
#robust standard errors
#Rule of thumb is that robust standard errors (or clustered standard errors) are needed when you have heteroskedasticity
#Usually, you should use robust standard errors even if you do not have heteroskedasticity
#Note: by default robust standard errors are calculated slightly differently than in STATA. 
#If you want your results to be replicable in STATA you need to use a slightly different formula
#For more details see: https://ryansafner.com/post/replicating-statas-robust-option-for-ols-standard-errors-in-r/
model4_robust <- coeftest(model4, vcovHC(model4))
model4_robust
### To replicate STATA results, you need to specify HC1 option
model4_robust_stata <- coeftest(model4, vcovHC(model4, "HC1"))
model4_robust_stata

##RObust standard errors for fixest package
#Note that the robust standard errors are calculated differently in fixest package
#The resutls are the same as in STATA or when using HC1 option in coeftest
model4_fixest <- feols(cnt~temp+temp_sq+hum+windspeed+holiday+workingday+factor(season), vcov = "hetero", data = bike_share)
summary(model4_fixest)


###Endogeneity and omitted relevant variables##
## The results change when we include additional variables
summary(model1)
summary(model4)
## There is no simple solution to endogeneity, you need to study causal inference methods to address it

####Exercise 4 ###
###Binary dependent variable
## For this exericise we load a different dataset
#https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers
##This data provides details on approx. 10 000 bank customers
#some of them stopped using credit card but some of them continue using it
#Your task is to explain why some people stopped using credit card
credit_card <- read_csv(("6.Introduction to Regression analysis/BankChurners.csv"))
names(credit_card)
#### Note that our main variable of interest is not numeric so we need to recode it
unique(credit_card$Attrition_Flag)
credit_card$attrition <- ifelse(credit_card$Attrition_Flag == 'Attrited Customer',1,0)

##Let's build a very simple model. First, usual OLS model
model1_binary <- lm(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category), data = credit_card)
summary(model1_binary)

model2_binary <- glm(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category), family = binomial(link = 'logit'), data = credit_card)
summary(model2_binary)
model3_binary <- glm(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category), family = binomial(link = "probit"), data = credit_card)
summary(model3_binary)

##Same with fixest package
model2_fixest <- feglm(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category), family = binomial(link = 'logit'), data = credit_card)
summary(model2_fixest)

##But, the coefficients in these models are not easily interpreted
#To have the same interpretation as we had in OLS, let's calculate the marginal effects
#By default marginal effect is calculated at means
##There are also other ways to calculate marginal effects
#but for time reasons we will use the simplest one

model2_me <- logitmfx(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category),data = credit_card)
model2_me
model3_me <- probitmfx(attrition~Customer_Age+factor(Gender)+Months_on_book+factor(Card_Category),data = credit_card)
model3_me

?logitmfx
##Comparing the models using the stargazer command we used before
#Note that the output of the logitmfx and probitmfx
#is a little different to the output of a standard regression
#Therefore for the results to be displayed, we need to use a somewhat more complicated command
stargazer(model1_binary, model2_binary, model3_binary,
          coef = list(NULL, model2_me$mfxest[,1], model3_me$mfxest[,1]), 
          se = list(NULL, model2_me$mfxest[,2], model3_me$mfxest[,2])
          ,type = 'text')



##Now your turn
###choose dataset
##some examples
#1) European Social Survey https://www.kaggle.com/pascalbliem/european-social-survey-ess-8-ed21-201617
#2)Chicago taxi trips https://www.kaggle.com/chicago/chicago-taxi-trips-bq 
#3) FIFA players dataset https://www.kaggle.com/datasets/stefanoleone992/fifa-22-complete-player-dataset
#4) Admissions dataset https://www.kaggle.com/mohansacharya/graduate-admissions 
#5) LEGO sets dataset https://www.kaggle.com/mterzolo/lego-sets
#or use your own data
#make your own regression