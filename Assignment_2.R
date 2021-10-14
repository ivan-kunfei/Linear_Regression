# 1
library(dplyr)
data <- read.csv("D:/BostonUniversity/AD699_Data_Mining/Assignment_2/nba_contracts.csv")

# 2
data$ppg <- (data$PTS / data$GP)
View(data)

# 3
library(ggplot2)
ggplot(data, aes(x=ppg, y=AVG_SALARY)) + geom_point() + geom_smooth(method="lm", se=TRUE)


# 4
# significant with 0.8 cor, and 
cor.test(data$AVG_SALARY, data$ppg)

# 5
set.seed(30)

length = count(data)$n

shuffle <- sample_n(data, length)

index = round(length*0.6)
train <-slice(shuffle, 1:index)
valid <- slice(shuffle, index+1:length)

# 6
simple_linear_model <- lm(AVG_SALARY ~ ppg, train)
summary(simple_linear_model)

# 7
data_2 <- data
data_2$predict_salary <- predict(simple_linear_model, data_2)
data_2$residuals <- data_2$predict_salary-data$AVG_SALARY
# 7.a
filter(data_2, data2$residuals == max(data_2$residuals)) %>% select(NAME,ppg,predict_salary,AVG_SALARY,residuals)
# 7.b
filter(data_2, data2$residuals == min(data_2$residuals)) %>% select(NAME,ppg,predict_salary,AVG_SALARY,residuals)

# 8
939640 * 22.89744 - 426043
939640 * 15.23171 - 426043

# 9
library(forecast)
train_acc <- accuracy(train$AVG_SALARY, predict(simple_linear_model,train))
valid_acc <- accuracy(test$AVG_SALARY, predict(simple_linear_model,valid))
train_acc
valid_acc

# 10
# std <- sd(data$AVG_SALARY)
std <- sqrt(sum((data_2$AVG_SALARY - mean(data_2$AVG_SALARY))^2)/length)
model_acc <- accuracy(data_2$AVG_SALARY, data_2$predict_salary)
model_acc


# PART 2
# 1
# identical name  no discipline
table(data$NAME)

train <- subset(train,select = -c(NAME))
valid <- subset(valid,select=-c(NAME))
# 2
library(leaps)
rgsbst<-regsubsets(AVG_SALARY ~., data=train, nbest=1,nvmax=NULL, method='exhaustive')
rgsbst_sum <- summary(rgsbst)
rgsbst_sum
which.max(rgsbst_sum$rsq) #24

multiple_linear_model <- lm(AVG_SALARY ~ CONTRACT_START + CONTRACT_END + AGE + GP + 
                             W+MIN+PTS + FGM + FGA + FG. + X3PM + X3PA + X3P. + FTA + FT. + OREB + DREB + AST+TOV+STL+BLK+PF+X...+ppg,train)
summary(multiple_linear_model)  # R-squared: 0.8127

# 3
library(car)
vif <- vif(multiple_linear_model)
vif <- sort(vif, decreasing=FALSE)
barplot(vif, horiz=TRUE)
vif
# values grater than 5: 
# ONLY  AGE X3P. FT. BLK  <5

# 4 calculate vif value for PTS, since its vif is the highest
model_PTS <- lm(PTS ~ CONTRACT_START + CONTRACT_END + AGE + GP + 
                  W+MIN + FGM + FGA + FG. + X3PM + X3PA + X3P. + FTA + FT. + OREB + DREB + AST+TOV+STL+BLK+PF+X...+ppg,train)
SST_PTS <- sum((train$PTS - mean(train$PTS))^2)
SSR_PTS <- sum((model_PTS$fitted.values - mean(train$PTS))^2)

R_squared_PTS <- SSR_PTS / SST_PTS
R_squared_PTS  # 0.998
vif_PTS <- 1/(1-R_squared_PTS)
vif_PTS # 735.4378

# 5
cor_df <- select(train, CONTRACT_START , CONTRACT_END ,AGE,GP , 
                   W,MIN,PTS , FGM , FGA , FG. , X3PM , X3PA , X3P. , FTA , FT. , OREB , DREB , AST,TOV,STL,BLK,PF,X...,ppg)
cor(cor_df)
# >0.7 : 
# CONTRACT_START & CONTRACT_END                1  y
# CONTRACT_END & CONTRACT_START                1  y
# GP & W,MIN,PF                                3  y
# W & GP                                       1  y         
# MIN & GP | PTS,FGM,FGA,FTA,DREB,TOV,STL,PF   9  y
# PTS & MIN| FGM,FGA,FTA,TOV,STL,ppg           7  y
# FGM & MIN,PTS | FGA,FTA,DREB,TOV,ppg         7  y
# FGA & MIN,PTS,FGM | FTA,TOV,STL,TOV,STL,ppg  9  y
# FG. & OREB                                   1  y
# X3PM & X3PA                                  1  y
# X3PA & X3PM |                                1  y
# FTA & PTS,FGM,FGA | TOV,ppg                  5  y
# OREB & FG. | DREB                            2  y
# DREB & MIN,FGM,OREB | BLK,PF                 5  y
# AST & TOV,STL                                2  y
# TOV & MIN,PTS,FGM,FGA,FTA,AST | STL,ppg      8  y
# STL & MIN,FGA,AST,TOV |                      4  y
# BLK & DREB  |                                1  y               
# PF & GP,MIN,DREB |                           3  y
# ppg & PTS,FGM,FGA,FTA,TOV |                  5  y

# remove  CONTRACT_END, MIN, FGA, TOV, PTS,  FTA, X3PA, ppg, DREB, OREB,STL,GP  # 12

# remain 12
cor_df_2 <- select(train, CONTRACT_START, AGE, W, FGM, FG., X3PM, X3P., FT., AST, BLK, PF, X...) 
cor(cor_df_2)

# 6
new_model <- lm(AVG_SALARY ~ CONTRACT_START+ AGE+W+ FGM+ FG.+ X3PM+ X3P.+ FT.+ AST+ BLK+ PF+ X..., train)
summary(new_model)  # R-squared: 0.6344

# 7

vif(new_model)

# 8
SST_new_model <- sum((train$AVG_SALARY - mean(train$AVG_SALARY) )^2)
SST_new_model

# 9
SSR_new_model <- sum((new_model$fitted.values - mean(train$AVG_SALARY))^2)
SSR_new_model

# 10
R_squared_new_model <- SSR_new_model / SST_new_model
R_squared_new_model  # 0.6344
summary(new_model)  # R-squared : 0.6344

# 11
library(visualize)
# df means degrees of freedom
train_length <- count(train)$n
freedom_degree <- train_length - 12 - 1 
visualize.t(stat = c(-1.746,1.746), df=freedom_degree, section= "tails") 

# 12
SSE_new_model <- SST_new_model - SSR_new_model
F_value <- (SSR_new_model/12) / (SSE_new_model/(train_length-12-1)) 
F_value # 15.33
visualize.f(stat=15.32928, section = "upper")


# 13
# CONTRACT_START:2019 AGE: 24  Name:Ivan  w:40, FGM: 250  FG. 50  x3pm:30  x3p.:30 
#  FT. : 80  AST:200  BLK:20   pf: 50  X...: 300
# get salary: 12898792
predict(new_model, data.frame(CONTRACT_START=2019,AGE=24,W=40,FGM=250,FG.=50,X3PM=30,X3P.=30,FT.=80,AST=200,BLK=20,PF=50,X...=300))

# 14
# MSE and RMSE of MLR is less than SLR 
# MAPE of SLR on valid set is 96.87756 which is bad.
# train
accuracy(train$AVG_SALARY, predict(simple_linear_model, train))
accuracy(train$AVG_SALARY, predict(new_model, train))
# valid
accuracy(valid$AVG_SALARY, predict(simple_linear_model, valid))
accuracy(valid$AVG_SALARY, predict(new_model, valid))


