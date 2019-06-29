library(ggplot2)
library(gridExtra)

n <- 1000
df <- 5
ch.2.mean <- rep(NA, n)
ch.8.mean <- rep(NA, n)
ch.32.mean <- rep(NA, n)
ch.128.mean <- rep(NA, n)

for(i in 1:n) {
  ch.2.mean[i] <- mean(rchisq(2, df=df))
  ch.8.mean[i] <- mean(rchisq(8, df=df))
  ch.32.mean[i] <- mean(rchisq(32, df=df))
  ch.128.mean[i] <- mean(rchisq(128, df=df))
}

m <- df
s <- sqrt(2 * df)

# chi 4
a<- data.frame(ch.2.mean)
a1 <- ggplot(data = a, aes(a$ch.2.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# chi 16
b<- data.frame(ch.8.mean)
b1 <- ggplot(data = b, aes(b$ch.8.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# chi 64
c<- data.frame(ch.32.mean)
c1 <- ggplot(data = c, aes(c$ch.32.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# chi 256
d <- data.frame(ch.128.mean)
d1 <- ggplot(data = d, aes(d$ch.128.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

grid.arrange(a1, b1, c1, d1, ncol = 2, top = '카이제곱 표본 크기별 분포 df= 5')


# T 분포
n <- 1000
df <- 5
t.2.mean <- rep(NA, n)
t.4.mean <- rep(NA, n)
t.8.mean <- rep(NA, n)
t.16.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  t.2.mean[i] <- mean(rt(2, df=df))
  t.4.mean[i] <- mean(rt(4, df=df))
  t.8.mean[i] <- mean(rt(8, df=df))
  t.16.mean[i] <- mean(rt(16, df=df))
}

# t(df=3)의 평균과 표준편차
m <- 0
s <- sqrt(df / (df - 2))

# t 2
a<- data.frame(t.2.mean)

a1 <- ggplot(data = a, aes(a$t.2.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# t 4
b<- data.frame(t.4.mean)
b1 <- ggplot(data = b, aes(b$t.4.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)
# t 8
c<- data.frame(t.8.mean)
c1 <- ggplot(data = c, aes(c$t.8.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# t 16
d<- data.frame(t.16.mean)
d1 <- ggplot(data = d, aes(d$t.16.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

grid.arrange(a1, b1, c1, d1, ncol = 2, top = 'T븐포 표본 크기별 분포 df= 5')


# F 분포
n <- 1000
df1 <- 3; df2 <- 5
f.4.mean <- rep(NA, n)
f.16.mean <- rep(NA, n)
f.64.mean <- rep(NA, n)
f.256.mean <- rep(NA, n)

# 표본 크기별로 1000번의 표본추출로 표본평균을 구함
for(i in 1:n) {
  f.4.mean[i] <- mean(rf(4, df1=df1, df2=df2))
  f.16.mean[i] <- mean(rf(16, df1=df1, df2=df2))
  f.64.mean[i] <- mean(rf(64, df1=df1, df2=df2))
  f.256.mean[i] <- mean(rf(256, df1=df1, df2=df2))
}

# f(df1=3, df2=5)의 평균과 표준편차
m <- df2 / (df2 - 2)
s <- sqrt(2 * df2^2 * (df1+df2-2) /(df1 * (df2-2)^2 * (df2-4)))


# f 4
a<- data.frame(f.4.mean)
a1 <- ggplot(data = a, aes(f.4.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)
a1
# f 16
b<- data.frame(f.16.mean)
b1 <- ggplot(data = b, aes(b$f.16.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)
# t 64
c<- data.frame(f.64.mean)
c1 <- ggplot(data = c, aes(c$f.64.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

# t 256
d<- data.frame(f.256.mean)
d1 <- ggplot(data = d, aes(d$f.256.mean)) + geom_histogram(aes(y= ..density..), color = 'red') + 
  geom_density(adjust = 2, size = 2, color = 'red', fill = 'red', alpha = 0.25)

grid.arrange(a1, b1, c1, d1, ncol = 2, top = 'F 븐포 표본 크기별 분포 df= (3, 5)')