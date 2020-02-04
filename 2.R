func <- function(x, a) {
  x ^ (a - 1) * exp(-x)
}


fgam <- function(a) {
  if (a == 1) {
    1
  } else if (a %% 1 == 0) {
    factorial(a - 1)
  } else if (a == 1 / 2) {
    sqrt(pi)
  } else if (a > 1) {
    (a - 1) * fgam(a - 1)
  }
  else {
    integrate(func, 0, Inf, a)$value
  }
}


fbet <- function(a, b) {
  if (a > 0 && b > 0 && a + b == 1) {
    pi / sin(a * pi)
  }
  fgam(a) * fgam(b) / fgam(a + b)
}


gamma_function <- function(a, b) {
  function(x) {
    if (x > 0 && a > 0 && b > 0) {
      1 / (b ^ a * fgam(a)) * x ^ (a - 1) * exp(-x / b)
    }
    else {
      0
    }
  }
  
}


beta_function <- function(a, b) {
  if (a > 0 && b > 0) {
    function(x) {
      if (0 < x && x < 1) {
        1 / fbet(a, b) * (x ^ (a - 1)) * ((1 - x) ^ (b - 1))
      }
      else {
        #0 * x ^ 0
        dbeta(x, shape1 = a, shape2 = b)
      }
    }
  }
  
}


# 1. P(X < 3)
# P(x < 3) = F(3)
fprobgamma1 <- function(a, b) {
  integrate(gamma_function(a, b), 0, 3)$value
}


# 2. P(2 < X < 5)
# P(2 < X < 5) = F(5) - F(2)
fprobgamma2 <- function(a, b) {
  integrate(gamma_function(a, b), 0, 5)$value - integrate(gamma_function(a, b), 0, 2)$value
}


# 3. P(3 < X < 4 | X > 2)
# P(3 < X < 4 | X > 2) = P(3 < X < 4) / P(X > 2) = (F(4) - F(3)) / (1 - F(2)) 
fprobgamma3 <- function(a, b) {
  (integrate(gamma_function(a, b), 0, 4)$value - integrate(gamma_function(a, b), 0, 3)$value) / 
    (1 - integrate(gamma_function(a, b), 0, 2)$value)
}


# 4. P(Y > 2)
# P(Y > 2) = 1 - G(2)
fprobbeta4 <- function(a, b) {
  1 - integrate(beta_function(a, b), 0, 2)$value
}


# 5. P(4 < Y < 6)
# P(4 < Y < 6) = G(6) - G(4)
fprobbeta5 <- function(a, b) {
  integrate(beta_function(a, b), 0, 6)$value - integrate(beta_function(a, b), 0, 4)$value
}


# 6. P(0 < Y < 1 | Y < 7)
# P(0 < Y < 1 | Y < 7) = P(0 < Y < 1) / P(Y < 7) = (G(1) - G(0)) / G(7)
fprobbeta6 <- function(a, b) {
  integrate(beta_function(a, b), 0, 1)$value - integrate(beta_function(a, b), 0, 0)$value / 
    integrate(beta_function(a, b), 0, 7)$value
}


# X + Y
fsum <- Vectorize(function(x, a, b) {
  integrate(function(t) {
    gamma_function(a, b)(x - t) * beta_function(a, b)(t)
  }, 0, 1)$value
})


# 7. P(X + Y < 5) 
fprob7 <- function(a, b) {
  integrate(fsum, 0, 5, a, b)$value
}


# X - Y
fdiff <- Vectorize(function(x, a, b) {
  integrate(function(t) {
    gamma_function(a, b)(x + t) * beta_function(a, b)(t)
  }, 0, 1)$value
})


# 8. P(X - Y > 0.5)
fprob8 <- function(a, b) {
  1 - integrate(fdiff, 0, 0.5, a, b)$value
}


# 9. P(X + Y > 3 | X - Y > 0.5)
fprob9 <- function(a, b) {
  (1 - integrate(fsum, 0, 3, a, b)$value) / (1 - integrate(fdiff, 0, 0.5, a, b)$value)
}


print_results <- function() {
  results <- matrix(nrow = 9, ncol = 2)
  rownames(results) <- 1:9
  colnames(results) <- c("User", "System")
  
  a = 3
  b = 2
  results[1, 1] <- fprobgamma1(a, b)
  results[1, 2] <- pgamma(3, shape = a, scale = b)
  
  results[2, 1] <- fprobgamma2(a, b)
  results[2, 2] <- pgamma(5, shape = a, scale = b) - pgamma(2, shape = a, scale = b)
  
  results[3, 1] <- fprobgamma3(a, b)
  results[3, 2] <- (pgamma(4, shape = a, scale = b) - pgamma(3, shape = a, scale = b)) /
    pgamma(2, shape = a, scale = b, lower.tail = FALSE)
  
  results[4, 1] <- fprobbeta4(a, b)
  results[4, 2] <- pbeta(2, shape1 = a, shape2 = b, lower.tail = FALSE)
  
  results[5, 1] <- fprobbeta5(a, b)
  results[5, 2] <- pbeta(6, shape1 = a, shape2 = b) - pbeta(4, shape1 = a, shape2 = b)
  
  results[6, 1] <- fprobbeta6(a, b)
  results[6, 2] <- (pbeta(1, shape1 = a, shape2 = b) - pbeta(0, shape1 = a, shape2 = b)) / 
    pbeta(7, shape1 = a, shape2 = b)
  
  results[7, 1] <- fprob7(a, b)
  results[7, 2] <- 0
  
  results[8, 1] <- fprob8(a, b)
  results[8, 2] <- 0
  
  results[9, 1] <- fprob9(a, b)
  results[9, 2] <- 0
  
  results
}

print_results()