v <- numeric(1e4)

for (j in 1:500){
  for (i in 1:1e4){
    x <- mean(sample(c(1, 0), j, replace = TRUE))
    v[i] <- x
    
  }
  
  table(v)
  plot(table(v), main = paste("Stickprovsstorlek = ", j))
  readline(prompt="Press [enter] to continue")
  
}



