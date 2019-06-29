library(dplyr)
library(ggplot2)
library(MASS)

#1
mtcars

mtcarsA1 <- mtcars %>% dplyr::filter(am == 1) %>% dplyr::select(mpg)
mtcarsA0 <- mtcars %>% dplyr::filter(am == 0) %>% dplyr::select(mpg)

# 정규성 검사
shapiro.test(mtcarsA0$mpg)
shapiro.test(mtcarsA1$mpg)
# 두 검사 모두 p-value >= 0.05 이므로 모두 정규성을 만족한다.
# 정규성이 아닐 시 Wilcoxon rank sum - test를 실시한다.

# 등분산성 테스트
var.test(mtcarsA0$mpg, mtcarsA1$mpg)
# p-value(0.06) >=0.05 이므로 등분산성을 만족한다. 통계적으로 유의하다.

t.test(x = mtcarsA1$mpg, y = mtcarsA0$mpg, mu = 0)
# p-value(0.001) <= 0.05 이므로 대립가설을 채택한다. 대립가설 채택으로 기어의 종류에 따라 mpg 차이가 난다.

#2
USA93 <- Cars93 %>% dplyr::filter(Origin == 'USA') %>% dplyr::select(Price)
non_USA93 <- Cars93 %>% dplyr::filter(Origin == 'non-USA') %>% dplyr::select(Price)

#정규성 검사
shapiro.test(USA93$Price)
shapiro.test(non_USA93$Price)
# 두 검사 모두 p-value <= 0.05 이므로 모두 정규성을 만족하지 않는다.

# 정규성이 아닐 시 Wilcoxon rank sum - test를 실시한다.
wilcox.test(y = USA93$Price, x = non_USA93$Price)
# p-value(0.7) >= 0.05 이므로 귀무가설을 채택한다. 생산국에 따른 차의 가격 차이는 없다.

# 등분산성 테스트
var.test(y = non_USA93$Price, x = USA93$Price)
# p-value(0.01) <=0.05 이므로 등분산성을 만족하지 않는다.

# 정규성을 가진다고 가정하고 two sample - test를 진행한다.
t.test(non_USA93$Price, USA93$Price, var.equal = F)
# p-value(0.3) >= 0.05 이므로 귀무가설을 채택한다. 생산국에 따른 차의 가격 차이는 없다.



#3
mpgSub <- mpg %>% dplyr::filter(class == 'subcompact') %>% dplyr::select(hwy)
mpgMid <- mpg %>% dplyr::filter(class == 'midsize') %>% dplyr::select(hwy)

#정규성 검사
shapiro.test(mpgSub$hwy)
shapiro.test(mpgMid$hwy)
# 두 검사 모두 p-value <= 0.05 이므로 모두 정규성을 만족하지 않는다.
# 하지만 정규성을 가진다고 가정하고 테스트한다.

# 등분산성 테스트
var.test(y = mpgMid$hwy, x = mpgSub$hwy)
# p-value(***) <=0.05 이므로 등분산성을 만족하지 않는다.

# 정규성을 가진다고 가정하고 two sample - test를 진행한다.
t.test(y = mpgMid$hwy, x = mpgSub$hwy, mu = 0, var.equal = F)
# p-value(0.4) >= 0.05 이므로 귀무가설을 채택한다. class에 따른 연비 차이는 없다.

mpgR <- mpg %>% dplyr::filter(fl == 'r') %>% dplyr::select(cty)
mpgP <- mpg %>% dplyr::filter(fl == 'p') %>% dplyr::select(cty)

#정규성 검사
shapiro.test(mpgR$cty)
shapiro.test(mpgP$cty)
# 두 검사 모두 p-value <= 0.05 이므로 모두 정규성을 만족하지 않는다.
# 하지만 정규성을 가진다고 가정하고 테스트한다.

# 등분산성 테스트
var.test(y = mpgP$cty, x = mpgR$cty)
# p-value(0.04) <=0.05 이므로 등분산성을 만족하지 않는다.

t.test(y = mpgP$cty, x = mpgR$cty, mu = 0, var.equal = F)
# p-value(0.2) >= 0.05 이므로 귀무가설을 채택한다. 휘발유에 따른 연비 차이는 없다.

mpgF <- mpg %>% dplyr::filter(class == 'subcompact' & drv == 'f') %>% dplyr::select(cty)
mpgRr <- mpg %>% dplyr::filter(class == 'subcompact' & drv == 'r') %>% dplyr::select(cty)

#정규성 검사
shapiro.test(mpgF$cty)
shapiro.test(mpgRr$cty)
# 두 검사 모두 p-value >= 0.05 이므로 모두 정규성을 만족한다.

# 등분산성 테스트
var.test(y = mpgRr$cty, x = mpgF$cty)
# p-value(0.04) <=0.05 이므로 등분산성을 만족하지 않는다.

t.test(y = mpgRr$cty, mpgF$cty, mu = 0, var.equal = F)
# p-value(***) <= 0.05 이므로 대립가설을 채택한다. 전륜과 후륜에 따른 도시 연비 차이는 난다.

#4
sample1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
sample2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

#정규성 검사
shapiro.test(sample1)
shapiro.test(sample2)
# 두 검사 모두 p-value >= 0.05 이므로 모두 정규성을 만족한다.


t.test(sample1, sample2, paired = T)
# p-value(0.006) <= 0.05 이므로 대립가설을 채택한다. 당뇨병 치료제는 효과가 있다.

#5
sample3 <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
sample4 <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

#정규성 검사
shapiro.test(sample3)
shapiro.test(sample4)
# 두 검사 모두 p-value >= 0.05 이므로 모두 정규성을 만족한다.

t.test(sample3, sample4, paired = T)
# p-value(0.008) <= 0.05 이므로 대립가설을 채택한다. 재질의 차이가 있다.