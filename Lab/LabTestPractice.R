library(ggplot2)
library(plotly)
## 1

x1=seq(0,100)
f1_pdf = function (x) {
  choose (200-x, 100)* 2^(x-200)
}

### a)
x1a=seq(10,100)
sum(f1_pdf (x1a))

### b)
f1_E = function (x) {
  x*f1_pdf(x)
}
sum(f1_E(x1))

### c)
f1_E2 = function (x) {
  (x^2)*f1_pdf(x)
}
sum(f1_E2(x1))

### d)
f1d = function (x) {
  ((x+1)^(-2))*f1_pdf(x)
}
sum (f1d(x1))


## 2
f2_pdf = function (x) {
  (2/9)*(x+1)*(2-x)
}

### a
f2_cdf = function (x) {
  (-1/27)*((x+1)^2)*(2*x-7)
}

### b
f2_cdf(9/5)

### c
f2_E = function () {
  integrate (f=function(x){x*f2_pdf(x)}, -1, 2)
}

f2_E3 = function () {
  integrate (f=function(x){(x^3)*f2_pdf(x)}, -1, 2)
}
f2_E3 ()

## 3

### a)
p3a = ggplot() +
  geom_boxplot(mapping = aes(x="Grith", y=trees$Girth)) +
  geom_boxplot(mapping = aes(x="Height", y=trees$Height)) +
  geom_boxplot(mapping = aes(x="Volume", y=trees$Volume))
ggplotly(p3a)

### b)
summary(trees$Girth, digits = 8)
summary(trees$Height, digits = 8)
summary(trees$Volume, digits = 8)

### c)
mean(trees$Height)
sd(trees$Height)

### d)
p3d = ggplot(data=trees, mapping=aes(x=Height*Girth*Girth, y=Volume)) +
  geom_point()+
  stat_smooth(method=lm)
ggplotly(p3d)

### e)
fit3e=lm(trees$Volume ~ trees$Height )
fit3e$coef[1]
fit3e$coef[2]

### g)
p3g = ggplot(data=trees) +
  stat_qq(mapping=aes(sample=Volume),distribution = qnorm,dparams = list(mean=mean(trees$Volume), sd=sd(trees$Volume)) )

ggplotly(p3g)

qqnorm(trees$Volume)
qqline(trees$Volume)

qqnorm(trees$Height)
qqline(trees$Height)

shapiro.test(trees$Volume)

x <- rt(5000,200)
shapiro.test(x)
nortest::ad.test(x)
