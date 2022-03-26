install.packages('MLmetrics')
install.packages("tree")
install.packages("randomForest")

library(tree)
library(MLmetrics)
library(tidyverse)
library(randomForest)

student.math <- read.csv("student-mat.csv",
                           sep = ";",
                           header = T
)

student.por <- read.csv("student-por.csv",
                          sep = ";",
                          header = T
)

# Visualize contents of the datasets
View(student.math)
View(student.por)

# statistical summary of the numerical columns of the datasets
summary(student.math) 
summary(student.por) 

# removing G1 and G2
student.por <- student.por %>%
  mutate_at(vars(-age,-G3,-absences),.funs=funs(factor)) %>%
  select(-c(G1,G2))

student.math <- student.math %>%
  mutate_at(vars(-age,-G3,-absences),.funs=funs(factor)) %>%
  select(-c(G1,G2))

# visualize the datasets
View(student.math)
View(student.por)

#### Data visualization ####
plot(as.factor(student.math$G3),col = "darkBlue",xlab="Math Final Score (G3)",ylab="Quantity")
plot(as.factor(student.por$G3),col = "darkBlue",xlab="Portuguese Final Score (G3)",ylab="Quantity")

ggplot(data = student.por,aes(x=G3))+
  geom_density(aes(fill=internet),alpha=.9)+
  labs(y="Frequency",x="Portuguese Final Score (G3)")

ggplot(data = student.math,aes(x=G3))+
  geom_density(aes(fill=internet),alpha=.9)+
  labs(y="Frequency",x="Math Final Score (G3)")

ggplot(data = student.math,aes(x=G3))+
  geom_bar(aes(fill=romantic),alpha=.9)+
  labs(y="Amount",x="Math Final Score (G3)")

ggplot(data = student.por,aes(x=G3))+
  geom_bar(aes(fill=romantic),alpha=.9)+
  labs(y="Amount",x="Portuguese Final Score (G3)")

# check for missing values
colSums(is.na(student.math))
colSums(is.na(student.por))

# Sample and split the dataset into train(70%) and test(30%) sets
RNGkind(sample.kind = "Rounding")
set.seed(23)

math_intrain <- sample(nrow(student.math),nrow(student.math)*.7)
math_grade_train <- student.math[math_intrain,]
math_grade_test <- student.math[-math_intrain,]

por_intrain <- sample(nrow(student.por),nrow(student.por)*.7)
por_grade_train <- student.por[por_intrain,]
por_grade_test <- student.por[-por_intrain,]

# modelling 
math.model1 <- lm(formula = G3~.,data = math_grade_train)
por.model1 <- lm(formula = G3~.,data = por_grade_train)

math.model2 <- glm(formula = G3~.,family = poisson,data = math_grade_train)
por.model2 <- glm(formula = G3~.,family = poisson,data = por_grade_train)


# compare performance
anova(math.model1, math.model2, test='F')
anova(por.model1, por.model2, test='F')

math_grade_test$G3_predicted1 <- round(predict(object = math.model1,newdata = math_grade_test),2)
math_grade_test$G3_predicted2 <- round(predict(object = math.model2,newdata = math_grade_test),2)
por_grade_test$G3_predicted1 <- round(predict(object = por.model1,newdata = por_grade_test),2)
por_grade_test$G3_predicted2 <- round(predict(object = por.model2,newdata = por_grade_test),2)

View(math_grade_test)
View(por_grade_test)

RMSE(y_pred = math_grade_test$G3_predicted1 ,y_true = math_grade_test$G3)
RMSE(y_pred = math_grade_test$G3_predicted2 ,y_true = math_grade_test$G3)
RMSE(y_pred = por_grade_test$G3_predicted1 ,y_true = por_grade_test$G3)
RMSE(y_pred = por_grade_test$G3_predicted2 ,y_true = por_grade_test$G3)

# Linear regression models have a better performance compared to the poisson regression models. This is 
# shown by the lower Root Mean Squared Error (RMSE) of the linear regression models. 
