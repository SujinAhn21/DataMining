# 로지스틱 회귀분석 with test-train data set

# 작업 디렉토리 설정
setwd("E:/R")
getwd()

# 데이터 불러오기
# - bankruptcyData라는 데이터셋을 불러옴.
# - 이 데이터셋은 1500개의 관측치와 50개의 변수를 포함함.
bankruptcyData <- read.csv("A123.csv")
str(bankruptcyData)

# 출력 변수를 범주형으로 변환
bankruptcyData$output <- factor(bankruptcyData$output, levels = c("nonbank", "bank"))

# 데이터셋 나누기(train-test split)
set.seed(123)
library(caret)
trainIndex <- createDataPartition(bankruptcyData$output, p = 0.65, list = FALSE)
train_data <- bankruptcyData[trainIndex, ]
test_data <- bankruptcyData[-trainIndex, ]


# 로지스틱 회귀분석 수행 (분류 모형)
# p-값이 0.05보다 작은 유의미한 변수들만..
result <- glm(output ~ A1280 + A1170 + A1160 + A1230 + F6450 + A1020,
              data = train_data, family = binomial)
# 이를 통해 유의미한 변수들/유의미하지 않은 변수들을 판별함 
summary(result) 

##### 유의미한 변수찾기 #####
# summary(result)에서 Coefficients 부분만 추출
coefficients_summary <- summary(result)$coefficients

# p-value가 0.05보다 작은 변수 필터링
significant_vars <- rownames(coefficients_summary)[coefficients_summary[, "Pr(>|z|)"] < 0.05]

# 결과 출력
print(significant_vars)
############################

# 모델 유의성 검정
anova(result, test = "Chisq")

# train data 예측 수행
predicted_prob_train <- predict(result, newdata = train_data, type = "response")
predicted_class_train <- ifelse(predicted_prob_train > 0.5, "bank", "nonbank")

# test data 예측 수행
predicted_prob_test <- predict(result, newdata = test_data, type = "response")
predicted_class_test <- ifelse(predicted_prob_test > 0.5, "bank", "nonbank")

# factor 수준(level) 일치시키기
predicted_class_train <- factor(predicted_class_train, levels = levels(train_data$output))
predicted_class_test <- factor(predicted_class_test, levels = levels(test_data$output))


# Confusion Matrix 계산
library(caret)
conf_matrix_train <- confusionMatrix(predicted_class_train, train_data$output)
conf_matrix_test <- confusionMatrix(predicted_class_test, test_data$output)


# Confusion Matrix 출력
print(conf_matrix_train)
print(conf_matrix_test)

# 모델 정확도
accuracy_train <- conf_matrix_train$overall['Accuracy']
print(paste("Train Data Accuracy:", round(accuracy_train, 4)))
accuracy_test <- conf_matrix_test$overall['Accuracy']
print(paste("Test Data Accuracy:", round(accuracy_test, 4)))




