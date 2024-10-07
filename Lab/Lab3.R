#Question 1
#a
i=3
X=function(i){
  choose(3,2)*choose(17,i-3)/choose(20,i-1)*(1/(20-i+1))
}
X(i)
#b
i=4
X(i)
#c
i=seq(3,20,2)
print(sum(X(i)))
#d
i=seq(4,20,2)
sum(X(i))

#Question 2
#a
f=function(x){
  choose (20,x)*choose(100-20,5-x)/choose(100, 5)
}
#b
x=0
f(x)
#c
x=0:3
sum(f(x))
#d
x=0:5
E=function(x){
   sum(x*f(x))
}
E(x)
#e
sum(x^2*f(x))
#f
Var=function(x){
  sum(x^2*f(x))-(E(x))^2
}
Var(x)
#g
plot(x,f(x))

#Question 3
at_least_one_match_with_replacement = function (n){
  1-(1-1/n)^n
}
at_least_one_match_without_replacement = function (n){
  k=0:n
  1-sum(((-1)^k)/factorial(k))
}
at_least_one_match_with_replacement(1)
match.f=function(n, simsize, rep=TRUE){
  freq=0
  for(i in 1:simsize){
    sam=sample(1:n, size=n,replace=rep)
    freq=freq + (sum(sam==1:n)>=1)
  }
  freq/simsize
}

match.f(n=1,simsize=1000,rep=TRUE)
match.f(n=3,simsize=1000,rep=TRUE)
match.f(n=10,simsize=1000,rep=TRUE)
match.f(n=15,simsize=1000,rep=TRUE)
match.f(n=100,simsize=1000,rep=TRUE)
match.f(n=10000,simsize=1000,rep=TRUE)
match.f(n=1000000,simsize=1000,rep=TRUE)
match.f(n=1,simsize=1000,rep=FALSE)
match.f(n=3,simsize=1000,rep=FALSE)
match.f(n=10,simsize=1000,rep=FALSE)
match.f(n=15,simsize=1000,rep=FALSE)
match.f(n=100,simsize=1000,rep=FALSE)
match.f(n=10000,simsize=1000,rep=FALSE)
match.f(n=1000000,simsize=1000,rep=FALSE)