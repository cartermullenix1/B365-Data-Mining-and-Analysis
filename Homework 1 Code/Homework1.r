##### Problem 1 #####

## Part A ##

trials = 10000

wins_of_A = rep(0,trials)

for (i in 1:trials) {
  while(1) {
    A = runif(1) < .5
    B = runif(1) < .5
    C = runif(1) < .5
    if (A != B | B != C) break
  }
  wins_of_A[i] = (A != B & B == C)
}

prob = sum(wins_of_A)/trials

cat("P(A) is:", prob, "\n")




##### Problem 2 #####

## Part A ##

1/sqrt(n) = 0.005

n = 40000

trials = 40000

wins_of_A = rep(0,trials)

for (i in 1:trials) {
  while(1) {
    A = runif(1) < .5
    B = runif(1) < .5
    if (A | B) break
  }
  wins_of_A = A
}

prob = sum(wins_of_A)/trials

cat("The 95% Confidence Interval of P(A) is ", prob, " +/- ", .005, "\n")




##### Problem 4 #####

##  Part A ##

trials = 300

phat = sum(runif(trials)<.1)/trials

margin = 1.96*sqrt(phat*(1-phat)/trials)

cat(phat, "+/-", margin, "\n")


## Part B ##

N = 300

l = rep(0,N)

u = rep(0,N)

trial = (runif(N))

for (n in 1:N) {
  phat = sum(trial[1:n]/n)
  margin = 1.96*sqrt(phat*(1-phat)/n)
  l[n] = phat - margin
  u[n] = phat + margin
}

plot(1:N, u, pch = '*')
points(1:N, l, pch = '+')


## Part C ##

n = 300
p = .1
N = 1000

contains = rep(0,N)

for (i in 1:N) {
  phat = sum(runif(n) < p)/n
  margin = 1.96*sqrt(phat*(1-phat)/n)
  contains[i] = (p > (phat - margin)) & (p < (phat + margin))
}

cat("The contained fraction is: ", sum(contains)/N)




##### Problem 6 #####

sum(runif(1) > cumsum(p)) + 1


