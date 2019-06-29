library(dplyr)
library(car)
library(gvlma)
library(leaps)
library(MASS)

a <- data.frame(아버지.Height = c(150, 160, 170, 180, 190),
                아들.Height = c(176, 179, 182, 181, 185)
                )
a_lm <- lm(아들.Height ~ 아버지.Height, data = a)

summary(a_lm)
# Estimate (intercept = 146.6, 아버지의 키  = 0.2)
# y = 0.2x + 146.6
predicValue <- function(x) return(0.2 * x + 146.6)

predicValue(165)


b <- data.frame(월소득 = c(100, 200, 300, 400, 500),
                카드사용량 = c(30, 70, 85, 140, 197)
                )
head(b)
b_lm <- lm( 카드사용량 ~ 월소득, data = b)

summary(b_lm)
# Estimate (intercept = -16.8, 월소득 = 0.404)
# y = 0.404x -16.8
predicValue2 <- function(x) return(0.404 * x -16.8)

predicValue2(250)


c <- mtcars %>% dplyr::select(disp, hp)
head(c)
c_lm = lm(hp ~ disp, data = c)

summary(c_lm)
# Estimate (intercept = 45.7345, disp = 0.4375)
# y = 0.4375x + 45.7345


# Boston data를 불러오고 칼럼을 확인한다.
d <- Boston
str(d)
# chas, rad 데이터는 이산형 데이터로 간주하여 factor 처리한다.
d$chas <- as.factor(d$chas)
d$rad <- as.factor(d$rad)

# 산점도를 그려 데이터와의 관계를 눈으로 확인한다.
pairs(d)
# medv 데이터와 나머지 데이터간의 연관성이 있다고 눈으로 판단한다.

# 선형모델 제작
null=lm(medv~1, data=d)
full=lm(medv~., data=d)

# 각 독립계수의 분산팽창계수값을 통한 다중공산성 존재 여부 판단
vif(full) > 10
# rad 값이 다중공산성이 있기에 데이터 정재를 할 필요가 있다.
# 이번 분석은 그냥 포함한 상태로 분석을 한다.

# medv 데이터에 관하여 각 데이터가 얼마나 관여를 하고 있는지 간단하게 알아본다.
summary(full)
anova(full)

# step() 분석을 통한 AIC 비교한다.
d_lm_forw <- step(null, scope=list(lower=null, upper=full), direction="forward")
# forward 방식으로 분석한 회귀모델은 다음과 같다.
d_lm_forw
d_lm_back <- step(full, data=d, direction="backward")
# backward 방식으로 분석한 회귀모델은 다음과 같다.
d_lm_back
d_lm_both <- step(null, scope = list(upper=full), data=d, direction="both")
# both 방식으로 분석한 회귀모델은 다음과 같다.
d_lm_both
# AIC를 비교한다.
AIC(d_lm_back, d_lm_both, d_lm_forw)
# forward, both로 분석한 회귀모델은 같고 세 모델 모두 같은 AIC 값을 지닌다.
summary(d_lm_forw)
summary(d_lm_back)
summary(d_lm_both)
# 하지만 다시 분석해보면 무의미한 변수가 제거되지 않았다.
# 정확한 변수 필터링이 어려워 step 분석은 한계점이 있다고 판단한다.

# leaps package를 통해 전체 모델에 대한 전부분집합 분석을 진행한다.
# leaps의 regsubsets로 BIC 분석
d_regsubsets <- regsubsets(medv ~  ., data = d, method ='exhaustive')
d_summary <- summary(d_regsubsets)
d_bicInfo <- summary(d_regsubsets)$bic
Min.val <- which.min(d_bicInfo)
d_bic <- coef(d_regsubsets, Min.val)
d_bicInfo[Min.val]
# BIC 값이 가장 작은 조합은 8번째이며 bic의 값은 -600.1663이다. 그 모델은 다음과 같다.
d_bic

d_bic_lm <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat, data = Boston)

summary(d_bic_lm)
anova(d_bic_lm)
# 설명 변수가 모두 유의미하다.

# d_bic_lm 모델이 타당한지 판단한다.
# 판단은 정규성, 등분산성, 선형성, 독립성을 중심으로 판단한다.

par(mfrow = c(2,2))
plot(d_bic_lm)
# 정규성과 선형성에 문제가 있는 것 같아 자세히 분석한다.

par(mfrow = c(1,1))
shapiro.test(d_bic_lm$residuals)
# p-value <= 0.05 이므로 정규성이 아니라고 판단한다.

gv_d_bic_lm <- gvlma(d_bic_lm)
summary(gv_d_bic_lm)
# 모든 p-value <= 0.05 이므로 선형성, 등분산성이 아니라고 판단한다.

durbinWatsonTest(d_bic_lm)
# p-value <= 0.05 이므로 독립성이 아니라고 판단한다.
# 모형 d_bic_lm은 기본 가정을 모두 만족하지 못한다.

# 모델을 수정하기 위해 2차항을 추가로 해보자.
# 의심가는 변수를 찾기 위해 crPlots를 사용한다.
crPlots(d_bic_lm)
# 검토 결과 chas, rm, lstat에 대한 선형성이 의심이 가므로 3가지 변수에 대해 추가한다.

d_bic_lm_2 <- lm(medv ~ zn + chas + nox + rm + I(rm^2)+ dis + ptratio + black + lstat + I(lstat^2), data = Boston)

summary(d_bic_lm_2)
# zn 이 무의미하므로 제거하고 다시 구축한다.

d_bic_lm_3 <- lm(medv ~ chas + nox + rm + I(rm^2)+ dis + ptratio + black + lstat + I(lstat^2), data = Boston)
summary(d_bic_lm_3)
# 이제 모든 값이 유의미하도고 판단되므로 다시 그래프로 판단한다.

par(mfrow = c(2,2))
plot(d_bic_lm_3)
# 이상치가 365, 369, 372, 373이 나타나므로 이상치라 판단하고 제거한다.
Boston1 <- Boston[-c(365, 369, 372, 373),]
d_bic_lm_4 <- lm(medv ~ chas + nox + rm + I(rm^2)+ dis + ptratio + black + lstat + I(lstat^2), data = Boston1)

summary(d_bic_lm_4)
plot(d_bic_lm_4)
# 데이터를 제거해도 이상치가 계속 발생하기에 종료한다.
# 마지막으로 다중공선성(분산팽창계수)를 판단하여 점검하도록 한다.

vif(d_bic_lm_4)
# chas변수는 이산형 변수이므로 제거하고 표준화를 하여 다시 판단한다.
Boston2 <- Boston1[,-4]
Boston2 <- as.data.frame(scale(Boston2))
d_bic_lm_5 <- lm(medv ~ nox + rm + I(rm^2)+ dis + ptratio + black + lstat + I(lstat^2), data = Boston2)


summary(d_bic_lm_5)
vif(d_bic_lm_5) > 4

# lstat 을 제외한 나머지는 어느정도 다중공선성 문제를 벗어난다고 판단한다.
# 결국 다른 문제는 없으므로 d_bic_lm_4를 최종 모델로 선정한다.
d_bic_lm_4
