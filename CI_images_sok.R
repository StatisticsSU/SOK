prop.test(c(204, 182), c(509, 536), alternative = "greater")
sqrt(3.9437)

library(openintro)
normTail(0, 1, U = 2, main = "A Standard Normal Distribution")
?normTail

par(mfrow = c(1,2))
# Calculate and save z_obs
z_obs <- (.44-0.4)/sqrt(.4*(1-.4)/500)
# Look at the value
z_obs
# load the openintro package
library(openintro)
# Look at the probability of getting a value to the right of z_obs
normTail(0, 1, U = z_obs)
# Find the probability
pnorm(z_obs, lower.tail = FALSE)
?pnorm

v <- numeric(1e4)
x <- c(rep(1,40),numeric(60))
for (i in 1:1e4){
  v[i] <- mean(sample(x, 500, replace = TRUE))
}

hist(v, main = "The Randomization Test Procedure")
abline(v = .44, lty = 2, lwd = 2, col = "red")
sum(v>=.44)

prop.test(220,500,p=0.4, alternative = "greater")
