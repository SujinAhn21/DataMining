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

# 로지스틱 회귀분석 수행 (분류 모형)
# p-값이 0.05보다 작은 유의미한 변수들만..
result <- glm(output ~ A1280 + A1170 + A1160 + A1230 + F6450 + A1020,
              data = bankruptcyData, family = binomial)
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

# 예측 수행
predicted_prob <- predict(result, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, "bank", "nonbank")

# Confusion Matrix 계산
library(caret)
conf_matrix <- confusionMatrix(factor(predicted_class, levels = c("nonbank", "bank")), bankruptcyData$output)

# Confusion Matrix 출력
print(conf_matrix)

# 모델 정확도
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 4)))
