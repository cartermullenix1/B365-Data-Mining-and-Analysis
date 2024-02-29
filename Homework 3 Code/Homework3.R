
##### Question 1 #####

## Part E ##

n = 10000
p = rep(0,n)
f = rep(0,n)

for (i in 1:n) {
  p[i] = sample(1:3, 1, prob=c(.2,.3,.5))
  if (p[i] == 1){
    f[i] = (runif(1) < .3)
  }else if (p[i] ==2) {
    f[i] = (runif(1) < .5)
  } else {
    f[i] = (runif(1) < .9)
  }
}

cat("P(A|favor) = ", sum(p == 1 & f) / sum(f), "\n")




##### Question 2 #####

## Part A ##

ucb = UCBAdmissions

apply(ucb,c("Dept","Admit"),sum)
mosaicplot(apply(ucb,c("Dept","Gender"),sum))


## Part B ##

apply(ucb,c("Dept","Gender"),sum)
mosaicplot(apply(ucb,c("Dept","Gender"),sum))


## Part C ##

ucb[,,"F"]
mosaicplot(ucb[,,"F"])


## Part D ##

apply(ucb["Rejected",,],"Gender",sum)




##### Question 3 #####

## Part A ##

s = c(rep(1,50),rep(2,50),rep(3,50))
pairs(iris[,1:4],pch=s)




##### Question 4 #####

## Part A ##

n = 1000
r = 3
o = matrix(0,n,r)
for (i in 1:n) {
  d = sample(1:3,1,prob=c(.5,.25,.25))
  if (d == 1) {
    o[i,] = sample(1:6,3,replace=T,prob=rep(1/6,6))
  } else if (d == 2) {
    o[i,] = sample(1:6,3,replace=T,prob=c(2/9,1/9,2/9,1/9,2/9,1/9))
  } else {
    o[i,] = sample(1:6,3,replace=T,prob=c(1/9,1/9,1/9,2/9,2/9,2/9))
  }
}


## Part D ##

n = 1000
r = 3
o = matrix(0,n,r)
p = matrix(0,3,6)
p[1,] = rep(1/6,6)
p[2,] = c(2/9,1/9,2/9,1/9,2/9,1/9)
p[3,] = c(1/9,1/9,1/9,2/9,2/9,2/9)
previous = c(.5,.25,.25)
t = rep(0,n)
e = rep(0,n)
for (i in 1:n) {
  t[i] = sample(1:3,1,prob = previous)
  x = o[i,] = sample(1:6,3,replace=T,prob=p[t[i],])
  joint = previous * p[,x[1]] * p[,x[2]] * p[,x[3]]
  e[i] = which.max(joint)
}

cat("estimated prob of correct = ", sum(e==t)/n, "\n")




##### Question 5 #####

## Part A ##

x = read.csv("three_related_vars.csv")
mosaicplot(table(x$A,x$B))

## Part B ##

mosaicplot(table(x[x$C==1,c("A","B")]))
mosaicplot(table(x[x$C==0,c("A","B")]))

## Part C ##

n = 1000
A = rep(0,n)
B = rep(0,n)
C = rep(0,n)
for (i in 1:n) {
  C[i] = runif(1) < .7
  if (C[i]){
    A[i] = runif(1) < .8
    B[i] = runif(1) < .8
  } else {
    A[i] = runif(1) < .2
    B[i] = runif(1) < .2
  }
}

