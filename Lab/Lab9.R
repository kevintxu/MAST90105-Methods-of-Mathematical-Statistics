library(ggplot2)
library(plotly)
library(readxl)

Mozart <- read_excel("C:/Users/kevin/OneDrive - The University of Melbourne/MAST90105 Methods of Mathematical Statistics/Lab/Mozart.xls")
#View(Mozart)

#Question 7
golden=(1+sqrt(5))/2
Mozart["newX"]=Mozart$Expos+Mozart$DevRecap
fit7Intercept = lm (DevRecap ~ Expos, data = Mozart)
fit7WithoutIntercept = lm (DevRecap ~ 0+Expos, data = Mozart)
fit7InterceptN = lm (DevRecap ~ newX, data = Mozart)
fit7WithoutInterceptN = lm (DevRecap ~ 0+newX, data = Mozart)
p7a = ggplot(data=Mozart) +

  geom_point(mapping=aes(x=newX,y=DevRecap), colour="gold")+
  stat_smooth(method=lm,mapping=aes(x=newX,y=DevRecap))+
  geom_abline(intercept = 0, slope = fit7WithoutInterceptN$coefficients[[1]], colour="orange")+
  geom_abline(intercept = fit7InterceptN$coefficients[[1]], slope = fit7InterceptN$coefficients[[2]], colour="orange")+
  stat_function(fun=function(x){x*golden}) +
  coord_cartesian(ylim = c(0, 200)) +
  geom_point(mapping=aes(x=Expos,y=DevRecap), colour="red")+
  stat_smooth(method=lm,mapping=aes(x=Expos,y=DevRecap))+
  geom_abline(intercept = 0, slope = fit7WithoutIntercept$coefficients[[1]], colour="pink")+
  geom_abline(intercept = fit7Intercept$coefficients[[1]], slope = fit7Intercept$coefficients[[2]], colour="pink")

mean(Mozart$newX/Mozart$DevRecap)
golden
ggplotly(p7a)
