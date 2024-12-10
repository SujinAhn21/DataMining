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

vdata <- read.csv("A4.csv")
str(vdata)

predict_prob <- predict(result, newdata=vdata, type="response") #모델에 의하여 예측된 값(확률) 구하기

predict_class <- ifelse(predict_prob > 0.5, "bank", "nonbank") # 예측된 클래스 구하기

#R에서 생성한 데이터 파일로 저장하기

write.csv(predict_class, "LogisticRegression.csv")



