library(dplyr)
library(car)
library(leaps)
library(MASS)

a <- data.frame(아버지.Height = c(150, 160, 170, 180, 190),
                아들.Height = c(176, 179, 182, 181, 185)
                )
head(a)
a_lm <- lm( formula = a$아들.Height ~ a$아버지.Height)

summary(a_lm)
# Estimate (intercept = 146.6, 아버지의 키  = 0.2)
# y = 0.2x + 146.6
predicValue <- function(x) return(0.2 * x + 146.6)

predicValue(165)


b <- data.frame(월소득 = c(100, 200, 300, 400, 500),
                카드사용량 = c(30, 70, 85, 140, 197)
                )
head(b)
b_lm <- lm( formula = b$카드사용량 ~ b$월소득)

summary(b_lm)
# Estimate (intercept = -16.8, 월소득 = 0.404)
# y = 0.404x -16.8
predicValue2 <- function(x) return(0.404 * x -16.8)

predicValue2(250)


c <- mtcars %>% dplyr::select(disp, hp)
head(c)
c_lm = lm(formula = c$hp ~ c$disp)

summary(c_lm)
# Estimate (intercept = 45.7345, disp = 0.4375)
# y = 0.4375x + 45.7345

d <- Boston
head(d)

# 선형모델 제작
null=lm(medv~1, data=d)
full=lm(medv~., data=d)

# 각 독립계수의 분산팽창계수값을 통한 다중공산성 존재 여부 판단
vif(full) > 4
# nox, rad, tax의 값이 다중공산성이 있기에 데이터 정재를 할 필요가 있다.
# 이번 분석은 그냥 포함한 상태로 분석을 한다.

# step() 분석을 통한 AIC 비교
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

# leaps의 regsubsets로 BIC 분석
d_regsubsets <- regsubsets(medv ~  ., data = d, method ='exhaustive', nbest = 13)
d_summary <- summary(d_regsubsets)
d_bicInfo <- summary(d_regsubsets)$bic
Min.val <- which.min(d_bicInfo)
d_bic <- coef(d_regsubsets, Min.val)
d_bicInfo[Min.val]
# BIC 값이 가장 작은 조합은 92번째이며 그 모델은 다음과 같다.
d_bic