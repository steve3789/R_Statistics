library(car)
library(dplyr)

# 1

dbinom(3 , 6, 1/3)
qnorm(.8, mean = 170, sd = 6)
qchisq(.95, df = 3)
pt(.975, df = 2)
pnorm(1)

# 2
# x
# o
# x
# o
# o

#3
iris_setosa_SepalLength <- iris %>% filter(Species == 'setosa') %>% select(Sepal.Length)

t.test(iris_setosa_SepalLength)
# 4.905 <= x <= 5.106

#4
1-pbinom(8, 10, 7/10)
pbinom(8, 10, 7/10) - pbinom(5, 10, 7/10)

#5

kor_al <- c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)

t.test(kor_al, mu = 8.1)
# p-value(0.04562) <= 0.05 이므로 귀무가설 기각, 대립가설 채택, 평균 알코올 섭취량은 달라졌다.

#6.
rangenorm <- function(from, to, mean, sd){ 
  return(pnorm(to, mean = mean, sd = sd) - pnorm(from, mean = mean, sd = sd))
}
rangenorm(-1.96, 1.96, 0, 1)

#7.
mpg_sub <- mpg %>% filter(class == 'subcompact') %>% select(cty)
mpg_mid <- mpg %>% filter(class == 'midsize') %>% select(cty)

# 정규성 검사 패스

var.test(mpg_sub$cty, mpg_mid$cty)
# p-value <= 0.05 등분산성 만족안함

t.test(mpg_sub$cty, mpg_mid$cty, var.equal = F)
# p-value(0.0592) >= 0.05 이므로 귀무가설 채택, 도시 연비는 연관이 없다.

mpg_r <- mpg %>% filter(fl == 'r') %>% select(hwy)
mpg_p <- mpg %>% filter(fl == 'p') %>% select(hwy)

# 정규성 검사 패스

var.test(mpg_r$hwy, mpg_p$hwy)
# # p-value <= 0.05 등분산성 만족 안함

t.test(mpg_r$hwy, mpg_p$hwy, var.equal = F)
# p-value(0.0015) <= 0.05 이므로 이므로 귀무가설 기각, 대립가설 채택,  고속도로 연비는 연관이 있다.

#8
chisq.test(x = c(322, 109, 99,2 9), 
           p = c(9/16, 3/16, 3/16, 1/16))
# p-value(0.6) >= 0.05 이므로 이므로 귀무가설 채택, 비율은 유효하다.

#9
summary(lm(women$weight ~ women$height))
# y = 3.45x.height -87.51667

