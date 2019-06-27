# 1. 단일표본 평균에 대한 추정 및 검정사례

x <- rnorm(1000)

t.test(x)

# t.test는 추정과 검정을 동시에 수행
# 추정된 내용 : 모평균 값이 -0.0024이고, 모평균이 95%인 신뢰구간은 -0.0605 ~ 0.0653
# 가설검정 : 귀무가설은 "모평균=0"으로, p-value 0.94은 0.05보다 크므로 기각할 수 없음. 
# 즉, "모평균 = 0"임을 부정할만한 근거가 부족함

## t.test() function View
## t.test(x, ...)

## Default S3 method:
# t.test(x, y = NULL,
#        alternative = c("two.sided", "less", "greater"),
#        mu = 0, paired = FALSE, var.equal = FALSE,
#        conf.level = 0.95, ...)
# 
# ## S3 method for class 'formula'
# t.test(formula, data, subset, na.action, ...)
# formula -> 종속변수 ~ 독립변수

# example
# t.test(extra ~ group, data = sleep, alternative = "less")
# t.test(extra ~ group, data = sleep, paired = TRUE)

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 2. 2개 표본평균에 대한 추정 및 검정 사례 : 두 개 집단의 평균 간에 차이가 있음을 검증하는 방법이다.

data(sleep)

t.test(extra~group, data=sleep, paired=F, var.equal=T)

# p-value = 0.07919 > 0.05 이므로 귀무가설(모평균에 차이가 없다)을 기각할 수 없음(즉, 두 표본의 평균이 다르다고 할 만한 근거가 부족함)
  

# 3. 대응 표본 평균검정

t.test(extra~group, data=sleep, paired=T, var.equal=T)

# p-value 0.002833 < 0.05 이므로 귀무가설(모평균에 차이가 없다)을 기각할 수 있음(즉, 두 표본의 평균은 다르다고 할 수 있음)
  

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 4. 이표본분산

data(iris)

var.test(iris$Sepal.Width,iris$Sepal.Length)

# 두 변수의 분산의 비율이 1이 아닌 즉, 분산이 같지 않다는 대립가설에 대한 F-test 결과입니다. p-value = 3.595e-14 < 유의수준 = 0.05 이므로, 95% 신뢰도로 두 표본의 분산은 같지 않다고 할 수 있습니다.

## var.test() function View
# var.test(x, ...)
# 
# ## Default S3 method:
# var.test(x, y, ratio = 1,
#          alternative = c("two.sided", "less", "greater"),
#          conf.level = 0.95, ...)
# 
# ## S3 method for class 'formula'
# var.test(formula, data, subset, na.action, ...)

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 5. 일표본비율

prop.test(42,100)

# prop.test와 binom.test를 이용해 100번의 시도 중 42번 성공하였다면, 
# 확률이 0.5라고 할 수 있는지 test한 사례입니다. 
# 두 사례 모두 p-value = 0.1336 > 유의수준 = 0.05 이므로, 
# 95% 신뢰도로 성공확률은 0.5라고 할 수 있다는 결론이 도출됩니다.(귀무가설을 기각하지 않음)

# prop.test() function View
# prop.test(x, n, p = NULL,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95, correct = TRUE)

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 6. 이표본비율(귀무가설 : 성공/실패의 확률이 같다)

prop.test(c(44,55),c(100,90))
# 100번 중 44번 성공한 경우와 90번 중 55번 성공한 경우, 
# 두 표번의 성공확률이 같다고 할 수 있는지를 검증한 결과로서, 
# p-value = 0.02697 < 유의수준 = 0.05 이므로, 95% 신뢰도로 성공확률이 같지 않다고 할 수 있습니다. 따라서 귀무가설을 기각합니다.

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 7. 상관계수 검정 : 양측검정에서 0.05 이하면 귀무가설을 기각합니다.

cor.test(c(1,2,3,4,5),c(1,0,3,4,5), method="pearson")
# 두 벡터 간의 상관관계가 있는지를 검증하는 것으로 p-value = 0.02937로 0.05보다 작으므로 
# 상관관계가 있다고 할 수 있습니다. 그러므로 두 벡터가 독립이라는 귀무가설을 기각합니다.

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 8. 독립성 검정

chisq.test(iris$Sepal.Width,iris$Sepal.Length)
# p-value = 0.1735, 95%신뢰도로 둘 간에는 유의미한 관계가 있다고 할 수 있습니다. 
# 따라서 두 변수가 독립이라는 귀무가설을 기각합니다.

# chisq.test() function View
# chisq.test(x, y = NULL, correct = TRUE,
#            p = rep(1/length(x), length(x)), rescale.p = FALSE,
#            simulate.p.value = FALSE, B = 2000)
#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 9. 정규분포의 값 여부 검정

shapiro.test(rnorm(1000))
# Shapiro-Wilk Normality Test는 3~5000개의 숫자로 이루어진 벡터가 정규분포를 이룬다고 할 수 있는지를 평가하는 테스트 입니다. 
# 결측값이 허용되며, 위의 개수 제한에는 해당되지 않습니다. 
# 즉, 비결측값의 개수만 3~5000개의 제한이 있으며, 아래 테스트 결과는 p-value = 0.887로서 0.05보다 크므로 
# 신뢰도 95%로 정규분포를 따른다고 할 수 있습니다.

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 10. 정규분포여부 검정

ks.test(rnorm(100),runif(100))
# Kolmogorov-Smirnov Test로서 두 번째 벡터가 숫자 벡터이므로, 
# 두 벡터가 같은 모집단에서 추출되었다는 것이 귀무가설입니다. 
# 결측값이 포함된 경우 자동으로 이를 제거하고 test를 수행하며, 
# 아래 테스트 결과는 p-value = 2.778e-11으로 0.05보다 작으므로 
# 95%의 신뢰도로 두 벡터는 서로 다른 모집단에서 추출되었다고 할 수 있습니다.

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

# 11. 일원분산분석

## lm() , anova() function View
# lm(formula, data, subset, weights, na.action,
#    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#    singular.ok = TRUE, contrasts = NULL, offset, ...)
# anova(object, ...)