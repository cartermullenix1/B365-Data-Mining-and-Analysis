##### Problem 4 #####

## Part A ##

n = 1000000
A = rep(0,n)
B = rep(0,n)

for (i in 1:n) {
  x = runif(1)
  A[i] = (x <.5)
  B[i] = ((2*x) %% 1) < .5
}
phata = sum(A)/n
phatb = sum(B)/n
phatab = sum(A + B)/n
cat(phata = "pm ", 1.96*sqrt(phata*(1-phata)/n), "\n")
cat(phatb = "pm ", 1.96*sqrt(phatb*(1-phatb)/n), "\n")
cat(phatab = "pm ", 1.96*sqrt(phatab*(1-phatab)/n), "\n")





##### Problem 6 #####

## Part C ##

population_size = 10000
prob_T = 0.30
prob_S_given_T = 0.90
prob_S_given_not_T = 0.30

trait_T = rbinom(population_size, 1, prob_T)
trait_S = rep(0, population_size)

for (i in 1:population_size) {
  if (trait_T[i] == 1) {
    trait_S[i] = rbinom(1, 1, prob_S_given_T)
  } else {
    trait_S[i] = rbinom(1, 1, prob_S_given_not_T)
  }
}

prop_T = mean(trait_T)
prop_S = mean(trait_S)

cat("Proportion of population with trait T:", prop_T, "\n")
cat("Proportion of population with trait S:", prop_S, "\n")

