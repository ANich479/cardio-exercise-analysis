#HBD_TEAMPROJECT
rm(list=ls())

#HBD_TEAMPROJECT
library(tableone)

data <- read.csv("cardio_train.csv", sep = ";")

# 데이터 확인
head(data)
dim(data) # 70000 13
colnames(data)
str(data) #(참고: 나이가 일수로 되어있음)

# 데이터 전처리 (age 일수 -> 년수 변경, weight, height, 수축기혈압, 이완기혈압 이상치 제거)
summary(data$age)
data$age <- floor(data$age / 365.25)
head(data)
str(data)

# 수축기혈압 이상치제거 
summary(data$ap_hi) #min: -150, max: 16020
boxplot(data$ap_hi, main = "수축기 혈압(ap_hi)의 박스플롯") #이상치 존재

# Q1 <- quantile(data$ap_hi, 0.25)
# Q3 <- quantile(data$ap_hi, 0.75)
# IQR <- Q3 - Q1
# lower <- Q1 - 1.5 * IQR
# upper <- Q3 + 1.5 * IQR
# data <- subset(data, ap_hi >= lower & ap_hi <= upper) #박스플롯 기준 이상치
data <- subset(data, ap_hi >= 90 & ap_hi <= 200) #보편적인 정상범위: 90~200
nrow(data) #69579

# 이완기혈압 이상치제거
summary(data$ap_lo) #min: 0, max: 10000
boxplot(data$ap_lo, main = "이완기 혈압(ap_lo)의 박스플롯") #이상치 존재

# Q1 <- quantile(data$ap_lo, 0.25)
# Q3 <- quantile(data$ap_lo, 0.75)
# IQR <- Q3 - Q1
# lower <- Q1 - 1.5 * IQR
# upper <- Q3 + 1.5 * IQR
# data <- subset(data, ap_lo >= lower & ap_lo <= upper) #박스플롯 기준 이상치
data <- subset(data, ap_lo >= 30 & ap_lo <= 120) #보편적인 정상범위: 30~120
nrow(data) #68525

# 몸무게 이상치제거
summary(data$weight)
boxplot(data$weight, main = "몸무게(weight)의 박스플롯") #이상치 존재
Q1 <- quantile(data$weight, 0.25)
Q3 <- quantile(data$weight, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
data <- subset(data, weight >= lower & weight <= upper) #박스플롯 기준 이상치
nrow(data) #66790

summary(data$height)
boxplot(data$height, main = "키(height))의 박스플롯") #이상치 존재
Q1 <- quantile(data$height, 0.25)
Q3 <- quantile(data$height, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
data <- subset(data, height >= lower & height <= upper) #박스플롯 기준 이상치
nrow(data) #66346


#------------------------------------------------------------------------------#

#1. 공변량 분포 확인
# 모든 공변량 (처치/반응 변수 제외)
covariates <- c("age","gender","height","weight","ap_hi","ap_lo",
                "cholesterol","gluc","smoke","alco")

# 그중 범주형만 따로
factorVars <- c("gender","cholesterol","gluc","smoke","alco")


# tableone 생성
table1 <- CreateTableOne(vars = covariates, strata = "active",
                         data = data, factorVars = factorVars)

# 공변량 분포 + 표준화된 평균 차이(SMD) 출력
print(table1, smd = TRUE)

# 각 변수의 SMD가 모두 0.1보다 작기 때문에 균형적임. -> 성향 매칭 안해도 됨 

#------------------------------------------------------------------------------#

#2. 변수 선택
# step 1 변수 후보 정하기(p < 0.2)
summary(glm(cardio ~active, family = binomial(), data = data)) 
summary(glm(cardio ~age, family = binomial(), data = data))
summary(glm(cardio ~factor(gender), family = binomial(), data = data)) #0.2이상 
summary(glm(cardio ~height, family = binomial(), data = data))
summary(glm(cardio ~weight, family = binomial(), data = data))
summary(glm(cardio ~ap_hi, family = binomial(), data = data))
summary(glm(cardio ~ap_lo, family = binomial(), data = data))
summary(glm(cardio ~factor(cholesterol), family = binomial(), data = data))
summary(glm(cardio ~factor(gluc), family = binomial(), data = data))
summary(glm(cardio ~factor(smoke), family = binomial(), data = data))
summary(glm(cardio ~factor(alco), family = binomial(), data = data))

# gender변수 제외
# 다른 변수는 p-value가 0.2보다 작기 때문에 모두 변수 후보가 됨!

#------------------------------------------------------------------------------#
# step 2 
# backward selection으로 변수 선택 
res <- glm(cardio~ active+age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
             factor(gluc)+smoke+alco, family = binomial(), data = data) 
summary(res)

# 제거해야할 변수 없음 (gluc2는 해석상 비유의미)

# forward selection
null_model <- glm(cardio ~ 1, family = binomial(), data = data)
forward_model <- step(null_model, scope = formula(res), 
                      direction = "forward", trace = FALSE)
summary(forward_model)

# stepwise selection
stepwise_model <- step(null_model, scope = formula(res),
                       direction = "both", trace = FALSE)
summary(stepwise_model)

# => 3개의 선택방법에서 모두 비유의미한 변수가 없다고 나옴(즉, 삭제해야하는 변수가 없음)
# ap_hi, age, cholesterol, weight, active, ap_lo, gluc, smoke, alco, height 로 총 10개 선택됨

# 최종모델
Final_M <- glm(cardio~ active+age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
                 factor(gluc)+smoke+alco, family = binomial(), data = data)

#------------------------------------------------------------------------------#

#3. 교호작용 및 AUC 해석
# 운동 여부와 다른 주요 변수들 간의 상호작용이 심혈관 질환에 영향을 주는가?

# active*age 0.00564 (교호작용존재)
summary(glm(cardio~ active*age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
              factor(gluc)+smoke+alco, family = binomial(), data = data))

# active*height 0.240
summary(glm(cardio~ active*height+age+weight+ap_hi+ap_lo+factor(cholesterol)+
              factor(gluc)+smoke+alco, family = binomial(), data = data))

# active*weight 0.160 
summary(glm(cardio~ active*weight+age+height+ap_hi+ap_lo+factor(cholesterol)+
              factor(gluc)+smoke+alco, family = binomial(), data = data))

# active*ap_hi 1.19e-07 (교호작용존재)
summary(glm(cardio~ active*ap_hi+ap_lo+age+height+weight+factor(cholesterol)+
              factor(gluc)+smoke+alco, family = binomial(), data = data))

# active*factor(cholesterol)2 0.997488    
# active*factor(cholesterol)3 0.000176 (교호작용존재)
summary(glm(cardio~ active*factor(cholesterol)+age+height+weight+ap_hi+ap_lo+
              factor(gluc)+smoke+alco, family = binomial(), data = data))

# active*factor(gluc)2  0.800    
# active*factor(gluc)3  0.282  
summary(glm(cardio~ active*factor(gluc)+age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
              smoke+alco, family = binomial(), data = data))

# active*smoke 0.073
summary(glm(cardio~ active*smoke+age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
              factor(gluc)+alco, family = binomial(), data = data))

# active*alco 0.9783
summary(glm(cardio~ active*alco+age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
              factor(gluc)+smoke, family = binomial(), data = data))

# active*ap_lo 0.02153 (교호작용존재)
summary(glm(cardio~ active*ap_lo+alco+age+height+weight+ap_hi+factor(cholesterol)+
              factor(gluc)+smoke, family = binomial(), data = data))

#교호작용 존재 변수: age, ap_hi, cholesterol, gluc, ap_lo
#최종모델에 교호작용 반영 여부
#최종모델과 교호작용을 반영한 모델 비교
library(pROC)

#active*age
Final_M2 <- glm(cardio~ active*age+height+weight+ap_hi+ap_lo+factor(cholesterol)+
                  factor(gluc)+smoke+alco, family = binomial(), data = data)

#active*cholesterol
Final_M3 <- glm(cardio~ active*factor(cholesterol)+age+height+weight+ap_hi+ap_lo+
                  factor(gluc)+smoke+alco, family = binomial(), data = data)

#active*ap_hi
Final_M4 <- glm(cardio~ active*ap_hi+factor(cholesterol)+age+height+weight+ap_lo+
                  factor(gluc)+smoke+alco, family = binomial(), data = data)

#active*ap_lo
Final_M5 <- glm(cardio~ active*ap_lo+factor(cholesterol)+age+height+weight+ap_hi+
                  factor(gluc)+smoke+alco, family = binomial(), data = data)
# 예측 확률 구하기
pred <- predict(Final_M, type = "response")
pred2 <- predict(Final_M2, type = "response")
pred3 <- predict(Final_M3, type = "response")
pred4 <- predict(Final_M4, type = "response")
pred5 <- predict(Final_M5, type = "response")

# ROC 객체 생성
roc_obj <- roc(response = data$cardio, predictor = pred)
roc_obj2 <- roc(response = data$cardio, predictor = pred2)
roc_obj3 <- roc(response = data$cardio, predictor = pred3)
roc_obj4 <- roc(response = data$cardio, predictor = pred4)
roc_obj5 <- roc(response = data$cardio, predictor = pred5)

# AUC 값 출력
auc_value <- auc(roc_obj)
auc_value2 <- auc(roc_obj2)
auc_value3 <- auc(roc_obj3)
auc_value4 <- auc(roc_obj4)
auc_value5 <- auc(roc_obj5)

#비교
aucs <- c(base = auc_value, interaction_age = auc_value2, interaction_chol = auc_value3,
          interaction_ap_hi = auc_value4, interaction_ap_lo = auc_value5)
round(aucs, 4)

Final_M_full <- glm(cardio ~ active*age + active*ap_hi + active*ap_lo + active*factor(cholesterol)+height+weight+factor(gluc)+smoke+alco, family = binomial(), data = data)
pred6 <- predict(Final_M_full, type = "response")
roc_obj6 <- roc(response = data$cardio, predictor = pred6)
(auc_value6 <- auc(roc_obj6))

# ROC 곡선 그래프 그리기
plot(roc_obj, col = "black", lwd = 2, lty = 1, main = "ROC Curve - Interaction Models")
lines(roc_obj2, col = "blue", lwd = 2, lty = 2)    # 점선
lines(roc_obj3, col = "red", lwd = 2, lty = 3)     # 점선-긴점선
lines(roc_obj4, col = "darkgreen", lwd = 2, lty = 4) # 점-점-선

legend("bottomright", 
       legend = c("Base", "Active*Age", "Active*Chol", "Active*ap_hi"),
       col = c("black", "blue", "red", "darkgreen"), 
       lty = c(1, 2, 3, 4), 
       lwd = 2)

#몇 가지 실험 결과 교호작용 반영시 해석의 복잡도 대비 성능 향상이 크지 않으므로 기존 모델(Final_M) 유지

#------------------------------------------------------------------------------#

# 결과 해석
#오즈비 및 신뢰구간 해석
library(jtools)
jtools::summ(Final_M, exp=T, digit=4)


#이 아래부터 수정할거 수정필요...

#운동을 하는 그룹은 운동을 하지 않는 그룹에 비해 심장질환발생의 오즈비가 0.79배임
#따라서 운동을 하는 사람은 운동을 하지 않는 사람에 비해 심혈관 질환에 걸릴 오즈가 21% 낮다고 할 수 있음
#age, height, weight, ap_hi, ap_lo 는 유의하지만 영향이 미미함
# 콜레스테롤이 높은 사람은 정상에 비해 심장질환발생의 오즈비가 1.45배이고 매우 높은 사람은 정상에 비해 약 3배임.
# 즉, 콜레스테롤이 높을 수록 심장질환발생 확률이 높음
# gluc2(정상에 비해 혈당 높음)은 통계적으로 유의하지 않으므로 해석에서 배제함
#gluc3(정상에 비해 혈당 매우 높음)/smoke/alco의 or이 1 미만이므로 
# 일반적인 의학 상식과 반대되는 결과를 보이므로 해석 유의 

#추가
#술을 마시는 사람의 오즈가 약 19.7% 낮음
#통계적으로는 유의하지만, 음주의 강도나 빈도 정보 없이 단순 음주 여부만으로 해석하기엔 한계가 있음.

#흡연자는 비흡연자보다 오즈가 15% 낮음
#흡연자 대부분이 상대적으로 젊거나 활동적인 집단일 수 있음
#다른 위험 요인(예: 고혈압, 고콜레스테롤)이 비흡연자에게 더 집중되어 있을 수 있음

#혈당(gluc) 변수가 단독으로는 오즈비가 1보다 낮게 나오나 처치 변수 외의 다른 변수(cholesterol 등)와의 교호작용에 #의해 해당 결과가 나왔을 가능성
#분석 목적이 운동이 심혈관 질환에 미치는 영향이기 때문에 최종모델에 굳이 포함하지 않음(실험 결과 오히려 모델의 #성능이 떨어짐 0.7902)

#정리
#흡연 및 음주 변수는 통계적으로 유의한 결과를 보였으나, 기존 의학적 지식과 상충되는 방향성을 보임
#해당 결과는 다른 공변량들과의 상호작용 또는 혼란 요인의 영향일 수 있으며, 단순한 인과적 결론보다는 제한된 맥락 #내에서 신중하게 접근할 필요가 있음
#해당 프로젝트의 분석 목적은 운동이 심혈관 질환에 미치는 영향을 파악하는 것이므로 위 변수들에 대한 추가적인 분석은 진행하지 않았음


#------------------------------------------------------------------------------#

model1 <- glm(cardio ~ gluc * cholesterol + age + ap_hi + active + weight, 
              data = data, family = binomial)
summary(model1)

library(interactions)
interact_plot(model1, pred = gluc, modx = cholesterol, 
              plot.points = TRUE, interval = TRUE)

#그래프로 시각화하기
library(ggplot2)

#해석적 타당성이 있는 변수들로 그래프 그리기
# 나이

ggplot(data, aes(x = factor(cardio), y = age)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Cardio", y = "Age", title = "Age by Cardio")


# 나이 그룹 변수 만들기 (연령은 일 단위로 저장되어 있음)
data$age_group <- cut(data$age, breaks = c(20, 30, 40, 50, 60, 70), right = FALSE)
ggplot(data, aes(x = age_group, fill = factor(cardio))) +
  geom_bar(position = "fill") +
  labs(x = "Age Group", y = "Proportion", fill = "Cardio", title = "Cardio by Age Group")
#전체적으로 cardio = 1에서의 중앙값, 사분위범위, 최대값이 더 큼
#나이가 많을수록 심혈관 질환의 가능성이 높다는 것을 나타냄

# 수축기 혈압
ggplot(data, aes(x = factor(cardio), y = ap_hi)) +
  geom_boxplot(fill = "lavender") +
  labs(x = "Cardio", y = "Systolic BP (ap_hi)", title = "Systolic BP by Cardio")
#cardio = 1 인 그룹에서 수축기 혈압의 중앙값을 비롯한 전체적인 분포가 더 높음
#cardio = 0 일 때 이상치 값들은 고혈압이지만 다른 위험요인은 적은 고위험군일 가능성

# 체중
ggplot(data, aes(x = factor(cardio), y = weight)) +
  geom_boxplot(fill = "lightpink") +
  labs(x = "Cardio", y = "Weight", title = "Weight by Cardio")
#질환 보유자가 평균적으로 몸무게가 약간 더 높을 수 있으나 분포가 상당수 겹쳐있고 이상치가 많아
#체중 단독으로 질환 여부를 명확히 구분하기 어려우므로
#키와 체중을 동시에 고려하는 bmi 지표로 분석 시도 후 재해석 고려

# 콜레스테롤
ggplot(data, aes(x = factor(cholesterol), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  labs(x = "Cholesterol", y = "Proportion", fill = "Cardio", title = "Cholesterol by Cardio")

# 혈당
ggplot(data, aes(x = factor(gluc), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  labs(x = "Glucose", y = "Proportion", fill = "Cardio", title = "Glucose by Cardio")
#고혈당도 심혈관 질환 위험과 강하게 관련되어 있음
#혈당이 높을수록 심혈관 질환 발생 비율 증가

#콜레스테롤 & 혈당과 질환 관계
ggplot(data, aes(x = factor(cholesterol), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  facet_wrap(~ factor(gluc)) +
  labs(x = "Cholesterol Level", y = "Proportion", fill = "Cardio",
       title = "Cardio by Cholesterol and Glucose Levels")

# 운동 여부에 따른 질환 비율
ggplot(data, aes(x = factor(active), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  labs(x = "Physical Activity (active)", y = "Proportion", fill = "Cardio",
       title = "Cardio Disease Rate by Physical Activity")
#큰 차이 없어보이나 표본 수가 많고 통계적으로는 유의한 차이

# 예측 확률 분포
ggplot(data.frame(pred, cardio = factor(data$cardio)), aes(x = pred, fill = cardio)) +
  geom_density(alpha = 0.5) +
  labs(x = "Predicted Probability", fill = "Cardio", title = "Distribution of Predicted Probabilities")
#질환이 없는 그룹은 낮은 예측 확률 영역에, 질환이 있는 그룹은 높은 확률 영역에 각각 분포
#이는 모델이 두 그룹을 일정 수준 이상 구분하고 있으며, 예측 확률이 실제 질환 발생 여부를 반영하는 데 의미 있는 분별력을 가지고 있음을 시사


#최종 요약
#주요결과 
#운동(active)은 심혈관 질환 발생 오즈가 약 0.21배
#주요 위험 요인: 고령, 높은 수축기 혈압, 고콜레스테롤
#연령: 나이가 많을수록 운동의 효과가 커짐
#혈압/콜레스테롤: 높을수록 운동의 효과가 감소

#해석 및 시사점
#정기적인 신체 활동은 심혈관 질환 예방에 긍정적인 영향
#운동효과는 연령,혈압,콜레스테롤 수준에 따라 달라질 수 있음(교호작용)

aucs <- c(base = auc_value, interaction_age = auc_value2, interaction_chol = auc_value3)
round(aucs, 4)


# ROC 곡선 시각화
plot(roc_obj, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  