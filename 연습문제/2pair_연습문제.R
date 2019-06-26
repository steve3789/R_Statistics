library(dplyr)
library(ggplot2)
library(MASS)

#1
mtcarsA1 <- mtcars %>% filter(am == 1) %>% select(mpg)
mtcarsA0 <- mtcars %>% filter(am == 0) %>% select(mpg)

var.test(mtcarsA0$mpg, mtcarsA1$mpg)
# p-value(0.06) >=0.05 이므로 등분산성을 만족한다. 통계적으로 유의하다.

t.test(x = mtcarsA1$mpg, y = mtcarsA0$mpg, mu = 0)
# p-value(0.001) <= 0.05 이므로 대립가설을 채택한다. 대립가설 채택으로 기어의 종류에 따라 mpg 차이가 난다.

#2
USA93 <- Cars93 %>% filter(Origin == 'USA') %>% dplyr::select(Price, MPG)
non_USA93 <- Cars93 %>% filter(Origin == 'non-USA') %>% dplyr::select(Price)

t.test(non_USA93$Price, USA93$Price, mu = 0)
# p-value(0.32) >= 0.05 이므로 귀무가설을 채택한다. 생산국에 따른 차의 가격 차이는 없다.


#3
mpgSub <- mpg %>% filter(class == 'subcompact') %>% dplyr::select(hwy)
mpgMid <- mpg %>% filter(class == 'midsize') %>% dplyr::select(hwy)

t.test(mpgMid$hwy, mpgSub$hwy, mu = 0)
# p-value(0.38) >= 0.05 이므로 귀무가설을 채택한다. class에 따른 연비 차이는 없다.

mpgR <- mpg %>% filter(fl == 'r') %>% dplyr::select(cty)
mpgP <- mpg %>% filter(fl == 'p') %>% dplyr::select(cty)

t.test(mpgR$cty, mpgP$cty, mu = 0)
# p-value(0.22) >= 0.05 이므로 귀무가설을 채택한다. 휘발유에 따른 연비 차이는 없다.


mpgF <- mpg %>% filter(class == 'subcompact' & drv == 'f') %>% dplyr::select(cty)
mpgRr <- mpg %>% filter(class == 'subcompact' & drv == 'r') %>% dplyr::select(cty)

t.test(mpgF$cty, mpgRr$cty, mu = 0)
# p-value(1.759 * 10^-6) <= 0.05 이므로 대립가설을 채택한다. 전륜과 후륜에 따른 도시 연비 차이는 난다.

#4
sample1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
sample2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

t.test(sample1, sample2, paired = T)
# p-value(0.006) <= 0.05 이므로 대립가설을 채택한다. 당뇨병 치료제는 효과가 있다.

#5
sample3 <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
sample4 <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)

t.test(sample3, sample4, paired = T)
# p-value(0.008) <= 0.05 이므로 대립가설을 채택한다. 재질의 차이가 있다.