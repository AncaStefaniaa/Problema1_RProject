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
  function(x) {
    if (0 < x && x < 1 && a > 0 && b > 0) {
      1 / fbet(a, b) * (x ^ (a - 1)) * ((1 - x) ^ (b - 1))
    }
    else {
      0
    }
  }
}


# 1. P(X < 3)
fprobgamma1 <- function(a, b) {
  integrate(Vectorize(gamma_function(a, b)), 0, 3)$value
}


# 2. P(2 < X < 5)
fprobgamma2 <- function(a, b) {
  integrate(Vectorize(gamma_function(a, b)), 2, 5)$value
}


# 3. P(3 < X < 4 | X > 2)
fprobgamma3 <- function(a, b) {
  integrate(Vectorize(gamma_function(a, b)), 3, 4)$value / integrate(Vectorize(gamma_function(a, b)), 2, Inf)$value
}


# 4. P(Y > 2)
fprobbeta4 <- function(a, b) {
  integrate(Vectorize(beta_function(a, b)), 2, Inf)$value
}


# 5. P(4 < Y < 6)
fprobbeta5 <- function(a, b) {
  integrate(Vectorize(beta_function(a, b)), 4, 6)$value
}


# 6. P(0 < Y < 1 | Y < 7)
fprobbeta6 <- function(a, b) {
  integrate(Vectorize(beta_function(a, b)), 0, 1)$value / integrate(Vectorize(beta_function(a, b)), 0, 7)$value
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
  integrate(fdiff, 0.5, Inf, a, b)$value
}


# 9. P(X + Y > 3 | X - Y > 0.5)
fprob9 <- function(a, b) {
  integrate(fsum, 3, Inf, a, b)$value / integrate(fdiff, 0.5, Inf, a, b)$value
}


print_results <- function(a, b) {
  results <- matrix(nrow = 9, ncol = 2)
  rownames(results) <- 1:9
  colnames(results) <- c("User", "System")
  
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
  results[7, 2] <- fprob7(a, b)
  
  results[8, 1] <- fprob8(a, b)
  results[8, 2] <- fprob8(a, b)
  
  results[9, 1] <- fprob9(a, b)
  results[9, 2] <- fprob9(a, b)
  
  results
}

print_results(3, 2)