library(prob)
library(ggplot2)
library(dplyr)
# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ 
# 코인토스
tosscoin(3)

# 확률 계산
tosscoin(2, makespace = T)

# 다이스 굴리기
head(rolldie(2),10)

# 조합
head(urnsamples(1:6, size = 3, replace = F), 10)

# 중복
head(urnsamples(1:6, size = 3, replace = T), 10)

# 응용
head(urnsamples(c(rep('R',3), rep('B', 2)), size = 2), 10)

# 기대값, 분산 계산
x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum(x * px)
VX <- sum(x^2 * px) - EX^2

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ 
# B(6, 1/3)에 대한 확률 계산
n <- 6
p <- 1/3
x <- 0:n

# 이항분포 확률 계산
# X = 2 일때, 2번 성공할 때의 확률
dbinom(2, size = n, prob = p)
# X = 4 일때, 4번 성공할 때의 확률
dbinom(4, size = n, prob = p)

# 각 확률분포에 대한 성공 확률
px <- dbinom(x, size = n, prob = p)
xpx <- data.frame(x, px)
ggplot(xpx, aes(x, px)) + geom_bar (stat = 'identity') + geom_text(aes(label=round(px,3)),nudge_y = 0.01)

# X <= 2 일때, sum(0, 1, 2)의 성공확률
pbinom(2, size = n, prob = p)
# X <= 4 일때, sum(0, 1, 2, 3, 4)의 성공확률
pbinom(4, size = n, prob = p)
# 2<= X <= 4 일때, sum(02, 3, 4)의 성공확률
pbinom(4, size = n, prob = p) - pbinom(2, size = n, prob = p)

# pbinom이 10% 이상인 지점
qbinom(0.1, size = n, prob = p)
# pbinom이 50% 이상인 지점
qbinom(0.5, size = n, prob = p)

# 시도가 n, 확률이 p 인 이항분포를 10번 시도한 Array
rbinom(10, size = n, prob = p)

# 기댓값과 분산
n <- 6
p <- 1/3
x <- 0:n

px <- dbinom(x, size = n, prob = p)
ex <- sum(x * px)
#ex2 <- sum(x^2 * px)
# varx <- ex2 - ex^2
vx <- sum(x^2 * px) - ex^2

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ 
# 정규분포 함수의 사용
options(digits = 3)
mu <- 170
sig <- 6
ll <- mu - 3 * sig
ul <- mu + 3 * sig

x <- seq(ll, ul, by = 0.01)
nd <- dnorm(x, mean = mu, sd = sig)
xnd <- data.frame(x, nd)

ggplot(xnd, aes(x, nd)) + geom_point()

pnorm(mu, mean = mu, sd = sig)
pnorm(158, mean = mu, sd = sig)
pnorm(180, mean = mu, sd = sig) - pnorm(160, mean = mu, sd = sig)

qnorm(0.25, mean = mu, sd = sig)
qnorm(0.5, mean = mu, sd = sig)
qnorm(0.75, mean = mu, sd = sig)

smp <- rnorm(40000, mean = mu, sd = sig)

df_smp <- data.frame(rnorm = smp)

ggplot(df_smp, aes(rnorm)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, color = "#FF6666", size = 1)

#정규분포 특징
mu <- 0
sig <- 1

p0.05 <- qnorm(0.05, mean = mu, sd = sig)
p0.025 <- qnorm(0.05, mean = mu, sd = sig)

pnorm(1.645) - pnorm(-1.645)
pnorm(1.96) - pnorm(-1.96)