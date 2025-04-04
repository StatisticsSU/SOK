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


v <- numeric(1e4)
x <- c(rep(1,40),numeric(60))
for (i in 1:1e4){
  v[i] <- mean(sample(x, 500, replace = TRUE))
}
hist(v, main = "The Randomization Test Procedure")
abline(v = .44, lty = 2, lwd = 2, col = "red")

par(mfrow=c(1,1))
plot(table(v), main = "The Randomization Test Procedue, p0 = 0.40")
abline(v = .44, lty = 1, lwd = 1, col = "red")
mean(v>=.44)

# The bootstrap
n <- 5000
novus <- c(rep(1, 204), numeric(305))
mean(novus)
w <- numeric(n)
for (i in 1:n){
  w[i] <- mean(sample(novus, 500, replace = TRUE))
}
hist(w, main = "Kristersson, mar-25, 5000 Bootstrapstickprov")
lower.upper <- quantile(w, c(.025, .975))
lower.upper
abline(v = lower.upper[1], col = "red", lty = 3, lwd = 2)
abline(v = lower.upper[2], col = "red", lty = 3, lwd = 2)
prop.test(204, 500)
0.4008-0.04257
0.4008+0.04257
lower.upper
