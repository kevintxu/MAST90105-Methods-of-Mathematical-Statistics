%% Question 1
% a
i = 3;
X(i)
% b
i=4;
X(i)
% c
i = 3:2:20;
sum(arrayfun(@X,i))
% d
i = 4:2:20;
sum(arrayfun (@X, i))


%% Question 2
% b
x = 0;
f(x)
% c
x=0:3;
sum(arrayfun(@f,x))
% d
x=0:5;
arrayfun(@E, x)
% e
sum((x.^2).*arrayfun(@f,x))
% f
Var(x)
%% Functions
function y = X(i)
y=nchoosek(3,2)*nchoosek(17,i-3)/nchoosek(20,i-1)*(1/(20-i+1));
end
function y = f(i)
y=nchoosek(20,i)*nchoosek(100-20,5-i)/nchoosek(100,5);
end
function y = E(x)
y=sum(x.*arrayfun(@f,x));
end
function y = Var(x)
y=sum((x.^2).*arrayfun(@f,x))-(E(x)).^2;
end