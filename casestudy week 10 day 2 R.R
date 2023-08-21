library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o)

df<- fread('crimes.csv')
##Data understanding
df %>% skimr::skim()
##Data Preparation

#Step1: Inpute missing values
library(inspectdf)
df %>% inspect_na()
#Step2:Variable encoding
df %>% glimpse()
#Step3:Multicollinearity Check
target <- 'ViolentCrimesPerPop'
features <- df %>%  select(-ViolentCrimesPerPop) %>%  names()

f<-as.formula(paste(target,paste(features, collapse = "+"), sep = "~")) 

glm <- glm(f,data = df)
glm %>% summary()

while (glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = ' + '), sep = ' ~ '))
  glm <- glm(f, data = df)
}
glm %>% faraway::vif() %>% sort(decreasing = T) %>% names()->features
df<-df %>% select(target,features)

#Step4:Scaling
df %>% glimpse()
df<-df %>% scale() %>% as.data.frame()
df %>% glimpse

#Modelling
library(h2o)
h2o.init()

h2o_data<-df %>% as.h2o()

#Step1: Train/Test split
h2o_data<-h2o_data %>% h2o.splitFrame(ratios=0.8, seed =123)
train<-h2o_data[[1]]
test<-h2o_data[[2]]

target <- 'ViolentCrimesPerPop'
features <- df %>%  select(-ViolentCrimesPerPop) %>%  names()

#Step 2. fit the model
model<-h2o.glm(
  x=features, y= target,
  training_frame = train,
  validation_frame = test,
  nfolds=10, seed=123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>% 
  as.data.frame() %>% 
  select(names, p_value) %>% 
  mutate(p_value = round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(desc(p_value))

##Step3 Remove unsignificant values from data
while(model@model$coefficients_table %>% 
      as.data.frame() %>% 
      select(names, p_value) %>% 
      mutate(p_value = round(p_value,3)) %>% 
      .[-1,] %>% 
      arrange(desc(p_value)) %>% 
      .[1,2]> 0.05) {
  model@model$coefficients_table %>% 
    as.data.frame() %>%
    dplyr::select(names,p_value) %>% 
    mutate(p_value = round(p_value,3)) %>% 
    filter(! is.nan(p_value)) %>% 
    .[-1,] %>% 
    arrange(desc(p_value)) %>% 
    .[1,1] -> v
  features <- features[features!=v]
  train_h2o <- train %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  
  model<-h2o.glm(
    x=features, y= target,
    training_frame = train,
    validation_frame = test,
    nfolds=10, seed=123,
    lambda = 0, compute_p_value = T)}

# Check the update p_values

model@model$coefficients_table %>% 
  as.data.frame() %>%
  dplyr::select(names,p_value) %>% 
  mutate(p_value = round(p_value,3))

#Step 4 Predictions

y_pred<- model %>%  h2o.predict(newdata = test) %>%  as.data.frame()

#Evaluotion

test_set <- test %>% as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict

#RMSE
RMSE = sqrt(mean(residuals^2))

#MAE
MAE = mean(abs(residuals))

#R Squared
y_test_mean = mean(test_set$ViolentCrimesPerPop)

#Total sum of squares
tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)

#Residual sum of squares
rss = sum(residuals^2)
R2 = 1- (rss/tss)
R2

#Adjusted R squared

n<-test_set %>% nrow()

#number of independent variables
k<-features %>%  length()
Adjusted_R2 = 1 - (1-R2)*((n-1)/(n-k-1))

#Comparison
tibble(RMSE =round(RMSE,1),
       R2, Adjusted_R2)

#Actual VS Predicted Plot(test)

my_data <- cbind(predicted=y_pred$predict,
                 observed = test_set$ViolentCrimesPerPop) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted Crimes",
       y="Actual Crimes",
       title=glue('Test:Adjusted R2={round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))

g %>% ggplotly()

#Overfitting Check

y_pred_train <- model %>%  h2o.predict(newdata = train) %>%  as.data.frame()

train_set <- train %>%  as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))

y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1- (rss/tss);
R2_train

n<- train_set %>%  nrow()
k<- features %>%  length()

Adjusted_R2_train = 1 - (1-R2_train)*((n-1)/(n-k-1))

#Actual VS Predicted plot(training)

my_data_train <- cbind(predicted=y_pred_train$predict,
                 observed = train_set$ViolentCrimesPerPop) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted Crimes",
       y="Actual Crimes",
       title=glue('Train:Adjusted R2={round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))

g_train %>%  ggplotly()

#comparisson of plots
library(patchwork)
g_train + g
