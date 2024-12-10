library(randomForest)
# 작업 디렉토리 설정
setwd("E:/R")
getwd()

# 데이터 불러오기
bankruptcyData <- read.csv("A123.csv")

# 데이터 구조 확인
#str(bankruptcyData)

# 출력 변수를 범주형으로 변환
bankruptcyData$output <- factor(bankruptcyData$output, levels = c("nonbank", "bank"))

# 데이터 나누기 (Training/Test Split)
set.seed(123) # 결과 재현을 위한 시드 설정
library(caret)

# 데이터 나누기
trainIndex <- createDataPartition(bankruptcyData$output, p = 0.65, list = FALSE)
train_data <- bankruptcyData[trainIndex, ]
test_data <- bankruptcyData[-trainIndex, ]
#head(train_data)

# +++++++ 변수 중요도 분석 +++++++
# 신경망 모델 훈련(다층 퍼셉트론)
library(nnet)
model <- train(output ~ ., data = train_data, method = "nnet", linout = FALSE, trace = FALSE)

# 모델 예측
predictions <- predict(model, newdata = test_data)

# confusionMatrix를 사용한 모델 성능 평가
cm <- confusionMatrix(predictions, test_data$output)
print(cm)

# 신경망 모델에서 변수 중요도 추출 (randomForest를 통해)
rf_model <- randomForest(output ~ ., data = train_data)

# 변수 중요도 출력
importance(rf_model)

# 변수 중요도 시각화
# varImpPlot(rf_model)

#++++++++++++++++++++++++++++++++

# 데이터 분할 확인
cat("Training set class distribution:\n")
print(table(train_data$output))
cat("Test set class distribution:\n")
print(table(test_data$output))

# 의사결정나무 모델 구축
library(party)

# 모델 학습
# minsplit : maxdepth : minbucket 의 비율
control <- ctree_control(maxdepth = 5,
                         minsplit = 20,
                         minbucket = 10)
# maxdepth: 의사결정나무의 최대 depth (부모마디는 포함하지 않음)
# minbucket: 끝마디 데이터 갯수의 최소값
# minsplit: 나무를 분리하기 전 필요한 끝마디 전 노드의 데이터 갯수 최소값

tree_model <- ctree(output ~ A1280 + F6450 + C3070 +
                      B2280 + A1170 + A1160 + A1230,
                    data = train_data,
                    control = control)
print(tree_model)
# plot(tree_model)
#plot(tree_model, type = "simple")

# 모델 요약
summary(tree_model)

#예측
predictions_train <- predict(tree_model,
                             newdata = train_data,
                             type = "response")
predictions_test <- predict(tree_model,
                            newdata = test_data,
                            type = "response")

# Confusion Matrix 생성
conf_matrix_train <- table(Predicted = predictions_train, Actual = train_data$output)
conf_matrix_test <- table(Predicted = predictions_test, Actual = test_data$output)

# Confusion Matrix 출력
cat("Confusion Matrix for Train Data \n")
print(conf_matrix_train)
cat("Confusion Matrix for Test Data \n")
print(conf_matrix_test)

# Accuracy 계산
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
cat("Train Data Accuracy: ", accuracy_train, "\n")
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
cat("Test Data Accuracy: ", accuracy_test, "\n")



