library(ggplot2)
library(gridExtra)
# 표본분포
m10 <- rep(NA, 1000)
m40 <- rep(NA, 1000)

m10

for( i in 1 : 1000){
  m10[i] <- mean(rnorm(10))
  m40[i] <- mean(rnorm(40))
}

options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))

df_m10 <- data.frame(m10)
df_m40 <- data.frame(m40)

ggplot(df_m10, aes(df_m10$m10)) + geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + geom_density(fill = 'black', alpha = 0.3) +
  lims(x=c(-1,1)) 
ggplot(df_m10, aes(df_m40$m40)) + geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + geom_density(fill = 'black', alpha = 0.3) +
  lims(x=c(-1,1))

# 중심극한정리

n <- 1000
r.1.mean = rep(NA, n)
r.2.mean = rep(NA, n)

for( i in 1:n){
  r.1.mean[i] = mean(rnorm(4, mean = 3, sd = 1))
  r.2.mean[i] = mean(rnorm(4, mean = 170, sd = 6))
}

df_r.1.mean <- data.frame(r.1.mean)
df_r.2.mean <- data.frame(r.2.mean)

ggplot(df_r.1.mean, aes(df_r.1.mean$r.1.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) 
  
ggplot(df_r.2.mean, aes(df_r.2.mean$r.2.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) 
  
# 모집단의 평균과 표준편차가 존재하는 임의의 분포일 때
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean <- rep(NA, n)
b.4.mean <- rep(NA, n)
b.64.mean <- rep(NA, n)
b.512.mean <- rep(NA, n)

for ( i in 1:n ){
  b.2.mean[i] <- mean(rbinom(2, size = t, prob = p))
  b.4.mean[i] <- mean(rbinom(4, size = t, prob = p))
  b.64.mean[i] <- mean(rbinom(64, size = t, prob = p))
  b.512.mean[i] <- mean(rbinom(512, size = t, prob = p))
}

df_b.2.mean <- data.frame(b.2.mean)
df_b.4.mean <- data.frame(b.4.mean)
df_b.64.mean <- data.frame(b.64.mean)
df_b.512.mean <- data.frame(b.512.mean)

a <- ggplot(df_b.2.mean, aes(df_b.2.mean$b.2.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) +
  lims(x= c(0,4))

b <- ggplot(df_b.4.mean, aes(df_b.4.mean$b.4.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) +
  lims(x= c(0,4))

c <- ggplot(df_b.64.mean, aes(df_b.64.mean$b.64.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) +
  lims(x= c(0,4))

d <- ggplot(df_b.512.mean, aes(df_b.512.mean$b.512.mean)) + 
  geom_histogram(fill = 'skyblue', color = 'black',aes( y = ..density..)) + 
  geom_density(fill = 'black', alpha = 0.3, linetype = 'dashed', size = 1) +
  lims(x= c(0,4))

grid.arrange(a,b,c,d, ncol = 2)
