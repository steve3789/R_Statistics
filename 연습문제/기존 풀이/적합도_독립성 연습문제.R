library(lawstat)

a1 <- data.frame(group = 1, ppm = c(5, 7, 6, 8, 6, 
                                   7, 8, 8, 6, 10))
a2 <- data.frame(group = 2, ppm = c(6, 8, 9, 11 ,13, 
                                   12 ,10, 8, 9, 10))
a3 <- data.frame(group = 3, ppm = c(14, 25, 26, 18, 19, 
                                   22, 21, 16, 20, 30))
a <- rbind(a1,a2,a3)

shapiro.test(a1$ppm)
shapiro.test(a2$ppm)
shapiro.test(a3$ppm)


# 모두 p-value >= 0.05 이므로 귀무가설을 채택한다. 3개 호수의 산소량은 정규성을 가진다.


# 각 호수 별 산소량에 대한 분산이 모두 동일한 지 검정한다. 
# 이에 대해서는 Levene의 검,정과 Bartlett의 검정이 있다. 
# 모두 세집단 이상의 경우에 대해 등분산성을 검정 시 사용하는 검정법이고 해당 통계량이 특정한 분포에 근사한다는 성질을 이용한다. 
levene.test(a$ppm, a$group)
bartlett.test(a$ppm, a$group) 
anova(lm(y = a$ppm, x = a$group, data = a))

# 모두 p-value <= 0.05 이므로 대립가설을 채택한다. 3개 호수의 산소량 등분산성을 가지지 않는다.

# 각 호수에서 어느 곳에서 차이가 발생하는지 확인한다.
a4 <- lm(ppm ~ group, data = a)
a5 <- aov(a4)
TukeyHSD(a5)

head(a)
ow <- lm(ppm ~ 호수, data = a)
anova(ow)

# p-value(0) <= 0.05 이므로 대립가설을 채택한다. 3개 호수의 산소량은 같지 않다.

b1 <- data.frame(채소 = 'A', 가격 = c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7))
b2 <- data.frame(채소 = 'B', 가격 = c(14.7, 16.3, 15.5, 15.2, 16.3, 13.5, 15.4))
b3 <- data.frame(채소 = 'C', 가격 = c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8))
b <- rbind(b1,b2,b3)
head(b)
ow <- lm(가격 ~ 채소, data = b)
anova(ow)
# p-value(0.98) >= 0.05 이므로 귀무가설을 채택한다. 3개의 채소 가격은 같다.

c <- data.frame( 합격 = 64, 불합격 = 16)
rownames(c) <- '빈도'
head(c)
chisq.test(c, p = c(0.85, 0.15))
# p-value(0.21) >= 0.05 이므로 귀무가설을 채택한다. 공정의 부적합품률은 15%이다.

d <- data.frame('한갑이상' = c(23, 31, 13),
                '한갑이하' = c(21, 48, 23),
                '안피움' = c(63, 159, 119))
rownames(d) <- c('반병 이상', '반병 이하', '못마심')
head(d)
chisq.test(d)
# p-value(0.01) <= 0.05 이므로 대립가설을 채택한다. 음주량과 흡연량 사이에는 연관이 있다.