library(ggplot2)
library(plotly)
library(MASS)
tasmania = read.csv("C:/Users/kevin/OneDrive - The University of Melbourne/MAST90105 Methods of Mathematical Statistics/Lab/EditedRainfall.csv")

#task1
dim(tasmania) # check data dimension
names(tasmania[,1:5]) # names of the first 5 columns

year=tasmania[,1]
s1=tasmania[,2]
s2=tasmania[,3]

summary(s1, digits = 8)
summary(s2, digits = 8)

sd(tasmania$`Burnie (Round Hill)`); IQR(tasmania$`Burnie (Round Hill)`) # IQR and sample standard deviaion
sd(tasmania$`Cape Grim (Woolnorth)`, na.rm = TRUE); IQR(tasmania$`Cape Grim (Woolnorth)`, na.rm = TRUE) # IQR and sample standard deviaion

hist(s1, freq=FALSE, xlab="Extreme rainfall (Burnie, Tas)", breaks = 13)
smooth.density = density(s1) # fits a smooth curve
points(smooth.density , type="l", lty=2, col=2) # adds a smooth curve

p1 = ggplot(data=tasmania, mapping=aes(x=Burnie..Round.Hill.)) +
  geom_histogram(binwidth = 5) + 
  xlab("Extreme rainfall (Burnie, Tas)") +
  geom_density()
ggplotly(p1)

ggplotly(ggplot(mapping=aes(x=smooth.density$x,y=smooth.density$y)) +
  geom_line())

boxplot(s1,s2, names=c("Burnie Is", "Cape Grim"), col = c("yellow", "orange"))
p2=ggplot(data=tasmania) +
  geom_boxplot(mapping=aes(x="Burnie..Round.Hill.", y=Burnie..Round.Hill.), fill="yellow") +
  geom_boxplot(mapping=aes(x="Cape.Grim..Woolnorth.", y=Cape.Grim..Woolnorth.), fill="orange")
ggplotly(p2)

ecdf1 = ecdf(s1); ecdf2 = ecdf(s2)
plot(ecdf1)
plot(ecdf2, col=2, add=TRUE )
p3 = ggplot(data=tasmania) +
  stat_ecdf(mapping = aes(x=sort(Burnie..Round.Hill.))) +
  stat_ecdf(mapping = aes(x=sort(Cape.Grim..Woolnorth., na.last = TRUE)), colour="red")
ggplotly(p3)

qqnorm(s1, main ="Normal QQ plot for S1") # normal QQ plot
qqline(s1)
p4 = ggplot(data=tasmania) + 
  stat_qq(mapping = aes(sample = Burnie..Round.Hill.)) 
ggplotly(p4)

Finv = function(p){-log(-log(p))} # quantile function
p = (1:20)/21
y = sort(tasmania$Burnie..Round.Hill.) # order statistics
x = Finv(p) # theoretical quantiles
plot(x, y, ylab="Sample quantiles", xlab="EV quantiles")
fit = lm(y ~ x) # this computes and plots the "best fitting line" (more details in next 
abline(fit)

p4 = ggplot(data=tasmania, mapping = aes(x=Finv(p), y = sort(Burnie..Round.Hill.))) + 
  geom_point() +
  xlab("EV quantiles") +
  ylab("Sample quantiles") +
  stat_smooth(method=lm) +
  geom_abline(intercept = fit$coef[[1]], slope = fit$coef[[2]])
ggplotly(p4)


#Task 3
mu.hat = mean(s1); mu.hat
n = length(s1)
sigma.hat = sqrt((n-1)*var(s1)/n)

dgumbel <- function(x,mu,sigma){
  exp((mu - x)/sigma - exp((mu - x)/sigma))/sigma
}

gumbel.fit = fitdistr(x=s1, densfun = dgumbel, start = list(mu=50,sigma=10))
gumbel.fit
