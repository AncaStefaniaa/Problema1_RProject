#Problema 1
#Exercitiul 1

#media si variatia pentru distributii
p1ex1 <- function() {
  pois = rpois(1000, 50)
  print(var(pois))
  print(mean(pois))
  
  binom = rbinom(1000, 15, 0.2)
  print(var(binom))
  print(mean(binom))
  
  exp = rexp(1000, 5)
  print(var(exp))
  print(mean(exp))
  
  norm = rnorm(1000, 200)
  print(var(norm))
  print(mean(norm))
}

#Exercitiul 2

p1ex2_3 <- function() {
  #voi afisa 2 grafice pe o linie
  par(mfrow = c(1, 2))
  
  
  #declar 2 vectori care retin culorile testelor si valorile
  plotColours <- c("magenta", "dark red", "violetred3","salmon", "blue")
  testValues <- c("1", "2", "5", "10", "100")
  
  #------------------POISSON--------------------#
  
  #graficele pentru densitate
  pois1 = data.frame(density = dpois(0:20, 1), mass = ppois(0:20, 1), distribution = rpois(0:20, 1))
  pois2 = data.frame(density = dpois(0:20, 2), mass = ppois(0:20, 2), distribution = rpois(0:20, 2))
  pois3 = data.frame(density = dpois(0:20, 5), mass = ppois(0:20, 5), distribution = rpois(0:20, 5))
  pois4 = data.frame(density = dpois(0:20, 10), mass = ppois(0:20, 10), distribution = rpois(0:20, 10))
  pois5 = data.frame(density = dpois(0:20, 100), mass = ppois(0:20, 100), distribution = rpois(0:20, 100))
  
  
  plot(pois1$density, type = "o", col = "magenta", main = "Poisson Density") 
  lines(pois2$density, type = "o", col = "dark red")
  lines(pois3$density, type = "o", col = "violetred3") 
  lines(pois4$density, type = "o", col = "salmon") 
  lines(pois5$density, type = "o", col = "blue") 
  legend("topright", testValues, col = plotColours, pch=1)
  
  #graficele pentru masa
  plot(pois1$mass, type = "o", col = "magenta", main = "Poisson Mass") 
  lines(pois2$mass, type = "o", col = "dark red")
  lines(pois3$mass, type = "o", col = "violetred3") 
  lines(pois4$mass, type = "o", col = "salmon") 
  lines(pois5$mass, type = "o", col = "blue") 
  legend("bottomright", testValues, col = plotColours, pch=1)
  
  
  #voi afisa 2 grafice pe o linie
  par(mfrow = c(1, 2))
  testValues1 <- c("0.01", "0.1", "0.5", "0.75", "0.95")
  
  #------------------BINOMIALA--------------------#
  
  binom1 = data.frame(density = dbinom(0:20, 30, 0.01), mass = pbinom(0:20, 30, 0.01), distribution = rbinom(0:20, 30, 0.01))
  binom2 = data.frame(density = dbinom(0:20, 30, 0.1), mass = pbinom(0:20, 30, 0.1), distribution = rbinom(0:20, 30, 0.1))
  binom3 = data.frame(density = dbinom(0:20, 30, 0.5), mass = pbinom(0:20, 30, 0.5), distribution = rbinom(0:20, 30, 0.5))
  binom4 = data.frame(density = dbinom(0:20, 30, 0.75), mass = pbinom(0:20, 30, 0.75), distribution = rbinom(0:20, 30, 0.75))
  binom5 = data.frame(density = dbinom(0:20, 30, 0.95), mass = pbinom(0:20, 30, 0.95), distribution = rbinom(0:20, 30, 0.95))
  
  #grafic pentru densitate
  plot(binom1$density, type = "o", col = "magenta", main = "Binomial Density") 
  lines(binom2$density, type = "o", col = "dark red")
  lines(binom3$density, type = "o", col = "violetred3") 
  lines(binom4$density, type = "o", col = "salmon") 
  lines(binom5$density, type = "o", col = "blue") 
  legend("topright", testValues1, col = plotColours, pch=1)
  
  #grafic pentru masa
  plot(binom1$mass, type = "o", col = "magenta", main = "Binomial Mass") 
  lines(binom2$mass, type = "o", col = "dark red")
  lines(binom3$mass, type = "o", col = "violetred3") 
  lines(binom4$mass, type = "o", col = "salmon") 
  lines(binom5$mass, type = "o", col = "blue") 
  legend("bottomright", testValues1, col = plotColours, pch=1)
  
  #------------------EXPONEntiala--------------------#
  par(mfrow = c(1, 2))
  exp1 = data.frame(density = dexp(0:20, 1), mass = pexp(0:20, 1), distribution = rexp(0:20, 1))
  exp2 = data.frame(density = dexp(0:20, 2), mass = pexp(0:20, 2), distribution = rexp(0:20, 2))
  exp3 = data.frame(density = dexp(0:20, 5), mass = pexp(0:20, 5), distribution = rexp(0:20, 5))
  exp4 = data.frame(density = dexp(0:20, 10), mass = pexp(0:20, 10), distribution = rexp(0:20, 10))
  exp5 = data.frame(density = dexp(0:20, 100), mass = pexp(0:20, 100), distribution = rexp(0:20, 100))
  
  #grafic pentru densitate
  plot(exp1$density, type = "o", col = "magenta", main = "Exponential Density") 
  lines(exp2$density, type = "o", col = "dark red")
  lines(exp3$density, type = "o", col = "violetred3") 
  lines(exp4$density, type = "o", col = "salmon") 
  lines(exp5$density, type = "o", col = "blue") 
  legend("topright", testValues, col = plotColours, pch=1)
  
  #grafic pentru masa
  plot(exp1$mass, type = "o", col = "magenta", main = "Exponential Mass") 
  lines(exp2$mass, type = "o", col = "dark red")
  lines(exp3$mass, type = "o", col = "violetred3") 
  lines(exp4$mass, type = "o", col = "salmon") 
  lines(exp5$mass, type = "o", col = "blue") 
  legend("bottomright", testValues, col = plotColours, pch=1)
  
  
  #------------------NORMALA--------------------#
  #nuj ce valori sa dau =)))!!!!!!!!!!!!!!!!!!!!!!!!!!!
  par(mfrow = c(1, 2))
  norm1 = data.frame(density = dnorm(0:200, 2.5, 0.5), mass = pnorm(0:200, 0.5, 0.5), distribution = rnorm(0:200, 0.5, 0.5))
  norm2 = data.frame(density = dnorm(0:200, 2, 0.2), mass = pnorm(0:200, 0.5, 0.5), distribution = rnorm(0:200, 0.5, 0.5))
  norm3 = data.frame(density = dnorm(0:200, 5, 0.01), mass = pnorm(0:200, 0.5, 0.5), distribution = rnorm(0:200, 0.5, 0.5))
  norm4 = data.frame(density = dnorm(0:200, 0, 0.02), mass = pnorm(0:200, 0.5, 0.5), distribution = rnorm(0:200, 0.5, 0.5))
  norm5 = data.frame(density = dnorm(0:200, 0, 0.2), mass = pnorm(0:200, 0.5, 0.5), distribution = rnorm(0:200, 0.5, 0.5))
  
  #grafic pentru densitate
  plot(norm1$density, type = "o", col = "magenta", main = "Normal Density") 
  lines(norm2$density, type = "o", col = "dark red")
  lines(norm3$density, type = "o", col = "violetred3") 
  lines(norm4$density, type = "o", col = "salmon") 
  lines(norm5$density, type = "o", col = "blue") 
  legend("topright", testValues, col = plotColours, pch=1)
  
  #grafic pentru masa
  plot(norm1$mass, type = "o", col = "magenta", main = "Normal Mass") 
  lines(norm2$mass, type = "o", col = "dark red")
  lines(norm3$mass, type = "o", col = "violetred3") 
  lines(norm4$mass, type = "o", col = "salmon") 
  lines(norm5$mass, type = "o", col = "blue") 
  legend("bottomright", testValues, col = plotColours, pch=1)
  
  
  
  
  #Exercitiul 3
  par(mfrow = c(2, 2))
  plot(pois1$distribution, type = "o", col = "magenta", main = "Poisson Density") 
  lines(pois2$distribution, type = "o", col = "dark red")
  lines(pois3$distribution, type = "o", col = "violetred3") 
  lines(pois4$distribution, type = "o", col = "salmon") 
  lines(pois5$distribution, type = "o", col = "blue") 
  legend("topright", testValues, col = plotColours, pch=1)
  
  plot(binom1$distribution, type = "o", col = "magenta", main = "Binomial Distribution") 
  lines(binom2$distribution, type = "o", col = "dark red")
  lines(binom3$distribution, type = "o", col = "violetred3") 
  lines(binom4$distribution, type = "o", col = "salmon") 
  lines(binom5$distribution, type = "o", col = "blue") 
  legend("topright", testValues1, col = plotColours, pch=1)
  
  plot(exp1$distribution, type = "o", col = "magenta", main = "Exponential Distribution") 
  lines(exp2$distribution, type = "o", col = "dark red")
  lines(exp3$distribution, type = "o", col = "violetred3") 
  lines(exp4$distribution, type = "o", col = "salmon") 
  lines(exp5$distribution, type = "o", col = "blue") 
  legend("bottomright", testValues, col = plotColours, pch=1)
  
  plot(norm1$distribution, type = "o", col = "magenta", main = "Normal Distribution") 
  lines(norm2$distribution, type = "o", col = "dark red")
  lines(norm3$distribution, type = "o", col = "violetred3") 
  lines(norm4$distribution, type = "o", col = "salmon") 
  lines(norm5$distribution, type = "o", col = "blue") 
  legend("topright", testValues, col = plotColours, pch=1)
}


#Exercitiul 4
#calculez variabilele folosite in functii

p1ex4 <- function(n, p){
  
  aproximarePoisson <- function(k, lambda){
    suma <- 0
    for(x in 0:k)
      suma <- suma + exp(-lambda) * (lambda ^ x)/factorial(x)
    return(suma)
  }
  
  aproximareNormalaTLC <- function(k, n, p){
    return(dnorm((k - n * p) / sqrt(n * p * (1 - p)), mean = 0, sd = 1))
  }
  
  aproximareNormalaFactorCorectie <- function(k, n, p){
    return(dnorm((k - 0.5 - n * p) / sqrt(n * p * (1 - p)), mean = 0, sd = 1))
  }
  
  aproximareCampPaulson <- function(c, miu, sigma){
    return(dnorm((c - miu) / sigma, mean = 0, sd = 1))
  }
  
  raspunsuri <- matrix(ncol = 6, nrow = 10);

  for(k in 1:10) {
    a <- 1 / (9 * (n - k))
    b <- 1 / (9 * (k + 1))
    r <- ((k + 1) * (1 - p)) / (p * (n - k))
    sigmaPatrat <- a + b * r ^ (2/3)
    c <- (1 - b) * r ^ (1/3)
    miu <- 1 - a
    lambda <- n * p
    
    aprox_a <- aproximarePoisson(k, lambda)
    aprox_b <- aproximareNormalaTLC(k, n, p)
    aprox_c <- aproximareNormalaFactorCorectie(k, n, p)
    aprox_d <- aproximareCampPaulson(c, miu, sqrt(sigmaPatrat))
    
    raspunsuri[k, 1] <- k
    raspunsuri[k, 2] <- dbinom(k, n, p)
    raspunsuri[k, 3] <- aprox_a
    raspunsuri[k, 4] <- aprox_b
    raspunsuri[k, 5] <- aprox_c
    raspunsuri[k, 6] <- aprox_d
  }

  return(raspunsuri)
}

#test <- p1ex4(25, 0.05)

p1ex5 <- function(){
  
  for(n in c(25, 50, 100))
    for(p in c(0.05, 0.1)){
      
      matrice <- p1ex4(n, p)
      max1 <- -1
      max2 <- -1
      max3 <- -1
      max4 <- -1
      for(k in 1:10){
        if(abs(matrice[k, 2] - matrice[k, 3]) > max1)
          max1 <- abs(matrice[k, 2] - matrice[k, 3])
        if(abs(matrice[k, 2] - matrice[k, 4]) > max2)
          max2 <- abs(matrice[k, 2] - matrice[k, 4])
        if(abs(matrice[k, 2] - matrice[k, 5]) > max3)
          max3 <- abs(matrice[k, 2] - matrice[k, 5])
        if(abs(matrice[k, 2] - matrice[k, 6]) > max4)
          max4 <- abs(matrice[k, 2] - matrice[k, 6])
      }
      
      maxPoints <- c(max1, max2, max3, max4)
      
      
      plot(NA, NA, main = "Max Points", pct = 10)
      points(1, max1, col = "green")
      points(2, max2, col = "green")
      points(3, max3, col = "green")
      points(4, max4, col = "green")
      #points(maxPoints, col = "magenta")
    }
}

p1ex5();


