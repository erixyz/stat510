library(faraway)
library(ISLR)
library(alr4)
library(leaps)
library(corrplot)

df <- read.csv("C:/Users/casti/Downloads/boston_comp1.csv")
df$CHAS <- factor(df$CHAS)

# correlation plot
cont.df <- subset(df, select = -c(CHAS))
corrplot(cor(cont.df), method = 'color')

# variable selection with AIC
mod1.redu <- lm(MEDV~1, data = df)
mod1.full <- lm(MEDV~., data = df)

step(mod1.redu, scope = list(lower = mod1.redu, upper = mod1.full))

df.reg <- subset(df, select = -c(MEDV, CHAS))


# summary + F-test of remaining predictors
mod2 <- lm(MEDV ~ LSTAT + RM + PTRATIO + CHAS + B + DIS + NOX + ZN + CRIM + RAD + TAX, data = df)
summary(mod2)
anova(mod2, mod1.full) # confirms AGE and INDUS are not significant

par(mfrow=c(2,2))
plot(mod2)

# fine-tuning the model
plot(lm(MEDV~LSTAT, data = df)) # quadratic shape in residual vs. fitted 
plot(lm(MEDV~RM, data = df)) # quadratic shape in residual vs. fitted 

mod3 <- update(mod2, .~.+I(LSTAT^2) + I(RM^2))
summary(mod3) # ZN lost significance, drop to preserve parsimony

mod4 <- update(mod3, .~. - ZN)
summary(mod4)

# finding interaction terms
add1(mod4, ~.+LSTAT*RM + CRIM*RM + CRIM*DIS + CRIM*B + NOX*B
     + NOX*RM + RM*DIS + DIS*B + RAD*DIS + RAD*B
     + TAX*RM + TAX*DIS + TAX*B + PTRATIO*RM + B*RM, data = df, test ='F')

# none of the above interactions are worth adding to the model.
# marginal increase to R^2. will further complicate model.
# reduces significance of main effect.

# dropping predictors TAX, CHAS, B, and RAD since they only increase R^2 by 2% overall
plot(mod4)
# check multicollinearity
vif(mod4.1)
# LSTAT and RM, along with their quadratic terms have high values. this is to be expected.

# transforming response variable
mod.boxcox = boxCox(mod4, lambda = seq(0, 0.5, length = 10))
medv1 <- df$MEDV^(1/3)

mod5 <- lm(medv1~LSTAT+I(LSTAT^2)+RM+I(RM^2)+PTRATIO+CHAS+B+DIS+NOX+CRIM+RAD+TAX, data = df)
summary(mod5)
plot(mod5)
# some points are affecting normality. drop these troublemakers and refit model
# notice that most of these points are worth 500k. most expensive in dataset.

# refitting model with points dropped
df1 <- df[-c(365,369,370,371,372,373), ]
mod6 <- lm(MEDV~LSTAT+I(LSTAT^2)+RM+I(RM^2)+PTRATIO+CHAS+B+DIS+NOX+CRIM+RAD+TAX, data = df1)
summary(mod6)

mod.boxcox = boxCox(mod6, lambda = seq(0, 1, length = 10))
medv2 <- df1$MEDV^(2/3)
LSTAT2 <- df1$LSTAT^2
RM2 <- df1$RM^2

mod7 <- lm(medv2~LSTAT+LSTAT2+RM+RM2+PTRATIO+CHAS+B+DIS+NOX+CRIM+RAD+TAX, data = df1)
summary(mod7)

newdata = data.frame(LSTAT = mean(df1$LSTAT), LSTAT2 = mean(LSTAT2), RM = mean(df1$RM), RM2 = mean(RM2),
                     PTRATIO = mean(df1$PTRATIO), CHAS = as.factor(1), B = mean(df1$B), DIS = mean(df1$DIS),
                     NOX = min(df1$NOX), CRIM = mean(df1$CRIM), RAD = mean(df1$RAD), TAX = min(df1$TAX))

predict(mod7, newdata, interval = 'predict')


# Fixing normality using Liao method:

df <- read.csv("C:/Users/casti/Downloads/boston_comp1.csv")
df$CHAS <- factor(df$CHAS)
df$MEDV <- (df$MEDV - min(df$MEDV))/(max(df$MEDV) - min(df$MEDV)) + 0.001

mod0.redu <- lm(MEDV~1, data = df)
mod0.full <- lm(MEDV~., data = df)

step(mod0.redu, scope = list(lower = mod0.redu, upper = mod0.full))

mod1 <- lm(MEDV ~ LSTAT + RM + PTRATIO + CHAS + B + DIS + NOX + ZN + CRIM + RAD + TAX, data = df)
add1(mod1, ~.+LSTAT*RM + CRIM*RM + CRIM*DIS + CRIM*B + NOX*B
     + NOX*RM + RM*DIS + DIS*B + RAD*DIS + RAD*B
     + TAX*RM + TAX*DIS + TAX*B + PTRATIO*RM + B*RM, data = df, test ='F')

mod2 <- update(mod1, .~.+LSTAT:RM)
add1(mod2, ~.+ CRIM*RM + CRIM*DIS + CRIM*B + NOX*B
     + NOX*RM + RM*DIS + DIS*B + RAD*DIS + RAD*B
     + TAX*RM + TAX*DIS + TAX*B + PTRATIO*RM + B*RM, data = df, test ='F')

mod3 <- update(mod2, .~.+RM:TAX)
add1(mod3, ~.+ CRIM*RM + CRIM*DIS + CRIM*B + NOX*B
     + NOX*RM + RM*DIS + DIS*B + RAD*DIS + RAD*B
     + TAX*DIS + TAX*B + PTRATIO*RM + B*RM, data = df, test ='F')

mod4 <- update(mod3, .~.+RM:PTRATIO -ZN -B)
mod.boxcox = boxCox(mod4, lambda = seq(0.2, 0.8, length = 10))

df$medv1 <- df$MEDV^(2/3)
mod5 <- lm(medv1~LSTAT+RM+PTRATIO+CHAS+DIS+NOX+CRIM+RAD+TAX+LSTAT:RM+RM:TAX+RM:PTRATIO, data = df)
mod6 <- update(mod5, .~. , data = df[-c(229,372,365,366,369,370,371,373,381),])

mod7 <- update(mod6, .~. -RM:TAX -TAX -RAD)
