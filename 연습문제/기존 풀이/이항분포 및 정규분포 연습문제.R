library(prob)
library(dplyr)
options(digits = 3)

# 1 - 1 x
# 1 - 2 o
# 1 - 3 x
# 1 - 4 o

# 2. 10번 시도해서 7번 성공할 확률
n <- 10
p <- 4/5
print(dbinom(7, n, p))

# 3. 불량품 2개 이하
n <- 20
p <- 5/100
print(pbinom(2, n, p))

# 4. 적어도 2명 이상
n <- 20
p <- 20/100
print(pbinom(20, n, p) - pbinom(1, n, p))

# 5. 주사위 눈금 합이 6
a <- data.frame(rolldie(2))
b <- a %>% filter(a$X1 + a$X2 == 6)
print(length(rownames(b))/length(rownames(a)))

# 6. 수명 750이하
mu <- 800
sigma <- 40
print(pnorm(750, mean = mu, sd = sigma))

# 7 - 2. 20년 이상
mu <- 11
sigma <- sqrt(16)
print(1 - pnorm(20, mean = mu, sd = sigma))

# 7 - 2. 상위 10%
print(qnorm(0.9, mean = mu, sd = sigma))

# 8. 80점 이상 90점 이하
mu <- 70
sigma <- 8
print(pnorm(90, mean = mu, sd = sigma) - pnorm(79, mean = mu, sd = sigma))

# 9. H(0) + H(2)
mu <- 1.5
sigma <- 2
print(pnorm(3, mean = mu, sd = sigma) - pnorm(2, mean = mu, sd = sigma) + (pnorm(0, mean = mu, sd = 2) - pnorm(-1, mean = mu, sd = 2)))
