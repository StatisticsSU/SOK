# körs bara en gång
# install.packages("openintro")
# ladda paketet som innehåller sexdistrimineringsdata
library(openintro)
# titta på datasettets 6 första rader
head(sex_discrimination)
# skapa en korstabell 
table(sex_discrimination$sex,sex_discrimination$decision)
male_promotion <- table(sex_discrimination$sex,sex_discrimination$decision)[1,1]/24
female_promotion <- table(sex_discrimination$sex,sex_discrimination$decision)[2,1]/24
male_promotion-female_promotion

# View(sex_discrimination)
# fördela decision slumpmässigt m gånger
set.seed(17)
m <- 1e3
male_minus_female <- numeric(m)
for (i in 1:m){
  shuffled_decision <- sex_discrimination$decision[sample(1:48, 48)]
  table(sex_discrimination$sex, shuffled_decision)
  male_promotion <- table(sex_discrimination$sex, shuffled_decision)[1,1]/24
  female_promotion <- table(sex_discrimination$sex, shuffled_decision)[2,1]/24
  male_minus_female[i] <- male_promotion - female_promotion
}

m_minus_f_rounded <- round(male_minus_female, 2)
table(m_minus_f_rounded)
sum(m_minus_f_rounded>.29)
sum(m_minus_f_rounded>=.29)/m
plot(table(m_minus_f_rounded),
     main = "1000 simuleringar",
     ylab = "Antal",
     xlab = "Skillnad i andel befodrade, män - kvinnor")

m <- 1e3
male_minus_female <- numeric(m)
for (i in 1:m){
  shuffled_decision <- sex_discrimination$decision[sample(1:48, 48)]
  table(sex_discrimination$sex, shuffled_decision)
  male_promotion <- table(sex_discrimination$sex, shuffled_decision)[1,1]/24
  female_promotion <- table(sex_discrimination$sex, shuffled_decision)[2,1]/24
  male_minus_female[i] <- male_promotion - female_promotion
}


prop.test(3, 62)

liver <- c(c(1,1,1),numeric(59))
n <- 1e4
res <- numeric(n)
for (i in 1:n){
  res[i] <- mean(sample(liver, 62, replace = TRUE))
}
hist(res)
quantile(res, c(0.025, 0.975))
