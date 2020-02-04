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


gamma_function <- function(x, a, b) {
  if (x > 0 && a > 0 && b > 0) {
    1 / (b ^ a * fgam(a)) * x ^ (a - 1) * exp(-x / b)
  }
  else {
    stop('The conditions have not been met')
  }
}


beta_function <- function(x, a, b) {
  if (0 < x && x < 1 && a > 0 && b > 0) {
    1 / fbet(a, b) * x ^ (a - 1) * (1 - x) ^ (b - 1)
  }
  else {
    0
  }
}


# 1. P(X < 3)
# P(x < 3) = F(3)
fprobgamma1 <- function(a, b) {
  integrate(gamma_function, 0, 3, a, b)$value
}


# 2. P(2 < X < 5)
# P(2 < X < 5) = F(5) - F(2)
fprobgamma2 <- function(a, b) {
  integrate(gamma_function, 0, 5, a, b)$value - integrate(gamma_function, 0, 2, a, b)$value
}


# 3. P(3 < X < 4 | X > 2)
# P(3 < X < 4 | X > 2) = P(3 < X < 4) / P(X > 2) = (F(4) - F(3)) / (1 - F(2)) 
fprobgamma3 <- function(a, b) {
  (integrate(gamma_function, 0, 4, a, b)$value - integrate(gamma_function, 0, 3, a, b)$value) / 
    (1 - integrate(gamma_function, 0, 2, a, b)$value)
}


# 4. P(Y > 2)
# P(Y > 2) = 1 - G(2)
fprobbeta4 <- function(a, b) {
  1 - integrate(beta_function, 0, 2, a, b)$value
}


# 5. P(4 < Y < 6)
# P(4 < Y < 6) = G(6) - G(4)
fprobbeta5 <- function(a, b) {
  integrate(beta_function, 0, 6, a, b)$value - integrate(beta_function, 0, 4, a, b)$value
}


# 6. P(0 < Y < 1 | Y < 7)
# P(0 < Y < 1 | Y < 7) = P(0 < Y < 1) / P(Y < 7) = (G(1) - G(0)) / G(7)
fprobbeta6 <- function(a, b) {
  integrate(beta_function, 0, 1, a, b)$value - integrate(beta_function, 0, 0, a, b)$value / 
    integrate(beta_function, 0, 7, a, b)$value
}


print_results <- function() {
  a <- 1:2
  b <- c("User", "System")
  results <- table(a, b)
  results
}


a = 2
b = 3
fprobgamma1(a, b)
pgamma(3, shape = a, scale = b)


fprobgamma2(a, b)
pgamma(5, shape = a, scale = b) - pgamma(2, shape = a, scale = b)


# fprobbeta4(a, b)
pbeta(2, shape1 = a, shape2 = b, lower.tail = FALSE)