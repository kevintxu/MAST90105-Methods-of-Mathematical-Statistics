# 1

x = seq(0,20)
f1pdf = function (x) {
  choose(99+x,x)*choose(219-x,20-x)/choose(319,20)
}

## a)
x_a=seq(10,20)
sum(f1pdf(x_a))

## b)
f1_E1 = function (x) {
  x*f1pdf(x)
}
sum(f1_E1(x))

## c)
f1_E2 = function (x) {
  (x^2)*f1pdf(x)
}
sum(f1_E2(x)) - sum(f1_E1(x))^2


## d)
f1_E_c  = function (x) {
  (x * exp(-x))*f1pdf(x)
}
sum(f1_E_c (x))


# 2
## a
boxplot(cars$speed,cars$dist)

## b
summary(cars$dist, digits = 5)

## c
mean(cars$dist)
sd(cars$dist)

## d
plot(cars$speed, cars$dist)

## e
q3e = lm (cars$dist~cars$speed)

## f
abline(lm (cars$dist~cars$speed))

## g
qqnorm(cars$speed)
qqline(cars$speed)

qqnorm(cars$dist)
qqline(cars$dist)