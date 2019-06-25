# 카이제곱분포의 자유도에 따른 모양 변화

df <- c(1, 3, 5, 10)
x <- seq(0, 20, by=0.01)

chi2.1 <- data.frame(dchisq(x, df[1]))
chi2.3 <- data.frame(dchisq(x, df[2]))
chi2.5 <- data.frame(dchisq(x, df[3]))
chi2.10 <- data.frame(dchisq(x, df[4]))

ggplot(chi2.1, aes(x = x, y = chi2.1$dchisq.x..df.1..))+geom_line(color = 'red', size = 2) + lims(y = c(0,0.3)) +
  geom_line(data = chi2.3, aes(x = x, y = chi2.3$dchisq.x..df.2..), color = 'green', size = 2) +
  geom_line(data = chi2.5, aes(x = x, y = chi2.5$dchisq.x..df.3..), color = 'yellow', size = 2) +
  geom_line(data = chi2.10, aes(x = x, y = chi2.10$dchisq.x..df.4..), color = 'blue', size = 2)


# T분포의 자유도에 따른 모양 변화

df <- c(1, 2, 8, 30)
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
t.1 <- data.frame(dt(x, df=df[1]))
t.2 <- data.frame(dt(x, df=df[2]))
t.8 <- data.frame(dt(x, df=df[3]))
t.30 <- data.frame(dt(x, df=df[4]))

ggplot(t.1, aes(x = x, y = t.1$dt.x..df...df.1..))+geom_line(color = 'red', size = 2) +
  geom_line(data = t.2, aes(x = x, y = t.2$dt.x..df...df.2..), color = 'green', size = 2) +
  geom_line(data = t.8, aes(x = x, y = t.8$dt.x..df...df.3..), color = 'yellow', size = 2) +
  geom_line(data = t.30, aes(x = x, y = t.30$dt.x..df...df.4..), color = 'blue', size = 2)

# F분포의자유도에 따른 모양 변화

df1 <- c(3, 10)
df2 <- c(5, 20)
x <- seq(0, 2, by=0.01)

f3.5 <- data.frame(df(x, df1[1], df2[1]))
f3.20 <- data.frame(df(x, df1[1], df2[2]))
f10.5 <- data.frame(df(x, df1[2], df2[1]))
f10.20 <- data.frame(df(x, df1[2], df2[2]))

ggplot(f3.5, aes(x = x, y = f3.5$df.x..df1.1...df2.1..))+geom_line(color = 'red', size = 2) +
  geom_line(data = f3.20, aes(x = x, y = f3.20$df.x..df1.1...df2.2..), color = 'green', size = 2) +
  geom_line(data = f10.5, aes(x = x, y = f10.5$df.x..df1.2...df2.1..), color = 'yellow', size = 2) +
  geom_line(data = f10.20, aes(x = x, y = f10.20$df.x..df1.2...df2.2..), color = 'blue', size = 2) +
  scale_color_manual()
