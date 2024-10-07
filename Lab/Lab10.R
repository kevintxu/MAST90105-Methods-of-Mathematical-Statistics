library(ggplot2)
library(plotly)
library(readxl)

#Question 8
x=t.test(rnorm(10), conf.level = 0.63)
x
names(x)
x$conf.int

f = function(t) {
  x=t.test(rnorm(t))
  as.vector(x$conf.int)
}
f(10)
f(20)
t = as.matrix(rep(10,1000))
C = t(apply(t,1,f))
C1 = apply(t,1,f)

matplot(C,type = "l")
p8 = ggplot() +
  geom_line (mapping=aes(x=c(row(C)[,1]),y=C[,1])) +
  geom_line (mapping=aes(x=c(row(C)[,2]),y=C[,2]), colour="red") +
  geom_abline(intercept = 0, slope = 0)

ggplotly(p8)

num = (C[,1]<0)&(C[,2]>0)
sum(num)/nrow(C)
