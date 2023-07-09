install.packages("ggplot2")
install.packages("reshape2") 
install.packages("ggcorrplot")
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(ggpubr)
library(lmtest)
library(car)
library(olsrr)
library(caret)
library(pls)
setwd("D:/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Datasets per costruzione")

df<- read.csv("/SCUOLA/Milano Data Science/PRIMO ANNO/STATISTICAL LEARNING AND MACHINE LEARNING/Statistical learning/Datasets per costruzione/df_csv.csv")
#observations 136 and 66 are dropped because cause problems in the analysis
df<-df[-c(66),]
df<-df[-c(136),]
df %>% glimpse()

#PART 1: EDA
df$Country <- as.factor(df$Country)
df$Religion <- as.factor(df$Religion)
levels(df$Religion)

#looking for outliers
summary(df$Fertility)
boxplot(df$Fertility)# no outlier
#-----------------------------
summary(df$Life.expectancy)
boxplot(df$Life.expectancy) #no outlier
#------------------------------
summary(df$Unemployment)
boxplot(df$Unemployment) #yes 
outliers_Unemployment <- boxplot.stats(df$Unemployment)$out
persone_Unemployment <- df[df$Unemployment %in% outliers_Unemployment, ]$Country
persone_Unemployment #Africa: Botswana,Djibouti,Gabon, Libya, South Africa, Sudan,Medio Oriente: Iraq, Jordan, Yemen
#----------------------------
summary(df$Alcohol)
boxplot(df$Alcohol)#no outlier
#---------------------------
summary(df$Years.school)
boxplot(df$Years.school) #no outlier
#---------------------------
summary(df$Contraceptive)
mean_contraceptive <- aggregate(Contraceptive ~ Religion, data = df, FUN = mean)
barplot(mean_contraceptive$Contraceptive, names.arg = mean_contraceptive$Religion, 
        xlab = "Religion", ylab = "Use of contraceptive Rate", main = "Use of contraceptive Rate by Religion")
#islam mostlty
boxplot(df$Contraceptive)#no outlier
#---------------------------
summary(df$Maternity.days)
boxplot((df$Maternity.days))
outliers_Maternity <- boxplot.stats(df$Maternity.days)$out
persone_Maternity <- df[df$Maternity.days %in% outliers_Maternity, ]$Country
persone_Maternity
#---------------------------
summary(df$GNI)
boxplot(df$GNI)
df$log_gni <- log(df$GNI)
df <- df[, -c(10)]
summary(df$log_gni)
boxplot(df$log_gni)
#--------------------------
summary(df$Freedom)
boxplot(df$Freedom) #no outlier
#---------------------------
summary(df$Migration.rate)
boxplot(df$Migration.rate)
conteggio_per_religione <- table(df$Religion)
mean_fertility <- aggregate(Fertility ~ Religion, data = df, FUN = mean)
barplot(mean_fertility$Fertility, names.arg = mean_fertility$Religion, 
        xlab = "Religion", ylab = "Fertility Rate", main = "Fertility Rate by Religion")
#islamismo e cristianesimo di più
ggplot(mean_fertility, aes(x = Religion, y = Fertility)) +
  geom_bar(stat = "identity", fill = "orchid2") +
  labs(x = "Religion", y = "Fertility Rate", title = "Fertility rate by religion")

df_num <- df[,-c(1,2)]
#CORRELATION BETWEEN PREDICTORS

dataset.cor = cor(df_num, method = c("spearman"))
dataset.cor
corr_mat <- round(dataset.cor,2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var1, Var2, label = value),
            color = "black", size = 3)+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))+
  coord_fixed()

#SL: MULTIPLE LINEAR REGRESSION

#describe the target variable
summary(df$Fertility)
qqnorm(df$Fertility)
shapiro.test(df$Fertility)
#the p value is low, I reject the null hypothesis: the variable isn't normally distributed
ggqqplot(df$Fertility)
ggdensity(df, x = "Fertility", fill = "lightgray", title = "Fertility distribution")+
  stat_overlay_normal_density(color="red", linetype = "dashed")
#try to use the logarithm
df$logFertility = log(df$Fertility)
qqnorm(df$logFertility)
#I improve a little bit the situation
shapiro.test(df$logFertility)
#normality is rejected
ggqqplot(df$logFertility)
ggdensity(df, x = "logFertility", fill = "lightgray", title = "LogFertility distribution")+
  stat_overlay_normal_density(color="red", linetype = "dashed")
#better but always not normally distributed
#maybe a Box Cox transformation could improve the situation
#----------------------------------------------------------------------------
df_reg <- subset(df, select = -c(Country, logFertility))
reg <- lm(Fertility~., data=df_reg)
summary(reg) #R^2 0.8
reg1 <- lm(log(Fertility)~., data=df_reg)  
summary(reg1)
RSE <- sigma(reg1)/mean(df_reg$Fertility)
RSE
sqrt(mean(reg1$residuals^2))
#significance: Religion, life expectancy, years of school,
#contraceptive, maternity days, log gni
#----------------------------------------------------------------------
#LINEARITY
plot(reg)
plot(reg1)

#HOMOSCEDASTICITY
bptest(reg1)
ncvTest(reg1)
#p-value is above 0.05, so the homoscedasticity hypothesis is accepted: there is homoscedasticity
plot(reg1)

# NORMALITY OF THE RESIDUALS 
qqnorm(resid(reg1), main = "Normal Q-Q Plot Linear Regression Model", col = "darkgrey")
qqline(resid(reg1), col = "dodgerblue", lwd = 2)

shapiro.test(resid(reg1))
#the p-value is above 0.05: we accept the null hypothesis
#that data were sampled from normal distribution

#MULTICOLLINEARITY: With multicollinearity, the regression coefficients are still 
#consistent but are no longer reliable since the standard errors are inflated. It means that the model's predictive 
#power is not reduced, but the coefficients may not be statistically significant with a Type II error.
#variance inflation factor

ols_vif_tol(reg1)
#a Tolerance of <0.1 might indicate multicollinearity: not in my case
#a VIF exceeding 5 requires further investigation, whereas VIFs above 
#10 indicate multicollinearity. Ideally, the Variance Inflation Factors are below 3
#some VIF are bigger than 3 so maybe there would be multicollinearity
#VIF between 1 and 5 there is moderate correlation between a given predictor 
#variable and other predictor variables in the model.

#OUTLIER AND LEVERAGE POINTS
#for outliers
df_reg1 <- subset(df, select = -c(Country,Religion))
meltData <- melt(df_reg1)
theme_update(plot.title = element_text(hjust = 0.5))
p <- ggplot(meltData, aes(factor(variable), value)) + geom_boxplot() + 
  facet_wrap(~variable, scale="free")+xlab("Variables")+ylab("Distribution")+
  labs(title="Distribution of each variable")
p
plot(reg1)

ols_plot_resid_lev(reg1)

ols_plot_resid_stud(reg1)

hats <- as.data.frame(hatvalues(reg1))
plot(hatvalues(reg1), type = 'h', ylab="Hat values of the linear regression", main="Leverage")

nrow(df_reg)
country_leverage <- which(hats > (2*11/nrow(df_reg)))

#SL: K-FOLD CROSS VALIDATION
set.seed(115)
train.control <- trainControl(method = "cv", number=10)
model <- train(log(Fertility)~., data = df_reg, method="lm", trControl = train.control)
print(model)

#CROSS VALIDATION TO TEST LINEAR REGRESSION
set.seed(17)
training_obs <- df_reg$Fertility %>% createDataPartition(p = 0.8, list = FALSE)
train <- df_reg[training_obs, ]
test <- df_reg[-training_obs, ]
modelCV <- lm(log(Fertility) ~ ., data = train)
predictions <- modelCV %>% predict(test)
data.frame(R_squared = R2(predictions, test$Fertility),
           RMSE = RMSE(predictions, test$Fertility),
           MAE = MAE(predictions, test$Fertility))
RMSE(predictions, test$Fertility)/mean(test$Fertility)

#BEST SUBSET SELECTION to solve multicollinearity: tutte sono usate
library(leaps)
regfit.full=regsubsets(log(Fertility)~.,data=df_reg, nvmax=11) 
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
which.max(reg.summary$rsq)
plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS", type="l")
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted RSq", type="l")
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
which.min(reg.summary$bic)
plot(regfit.full, scale="r2")
plot(regfit.full,scale="Cp")

#SL: PARTIAL LEAST SQUARES REGRESSION
set.seed(17)
training_obs <- df_reg$Fertility %>% createDataPartition(p = 0.8, list = FALSE)
train <- df_reg[training_obs, -c(1)]
test <- df_reg[-training_obs, -c(1)]
pls.fit=plsr(Fertility~., data = train, scale = TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, )

model <- train(Fertility~., data = train, method = "pls", scale = TRUE, 
               trControl = trainControl("cv", number = 10),
               tuneLength = 10)
# Plot model RMSE vs different values of components
plot(model)
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune
summary(model$finalModel)
#Make predictions
predictions <- model %>% predict(test)
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, test$Fertility),
  Rsquare = caret::R2(predictions, test$Fertility)
)

