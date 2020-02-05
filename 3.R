#' 
#' @brief Function that generates the P [i] repartition for 
#' the random variable X. It will be located on the bonus column (m + 1).
#' 
#' @returns the modified matrix  A.
#'
piGenerate <- function(A, n, m){


  remaining = 1

  for (line in 1 : (n - 1)){

    A [line, m + 1] = runif (1, min = 0, max = (remaining / (n - line + 1)) * 2 )
    remaining = remaining - A [line, m + 1]

  }

  A [n, m + 1] = remaining
  return (A)
}



#' 
#' @brief Function that generates the Q [j] repartition for 
#' the random variable Y. It will be located on the bonus line (n + 1).
#' 
#' @returns the modified matrix   A.
#' 
qjGenerate <- function(A, n, m){
  
  
  remaining = 1
  
  for (column in 1 : (m - 1)){
    
    A [n + 1, column] = runif (1, min = 0, max = (remaining / (m - column + 1)) * 2 )
    remaining = remaining - A [n + 1, column]
    
  }
  
  A [n + 1, m] = remaining
  
  return(A)
}

#' 
#' @brief Function that generates the X[i] and Y[j] random variables (without the
#' associated probabilities. The numbers will be generated on the (m + 2)th column
#' and (n + 2)th row.
#' We assume n and m are under 1000.
#' 
#' @returns the modified matrix A.
#'                                                                    
generateXAndY <- function (A, n, m){
  
  maxLength <- 20
  viz <- vector(mode = "logical", length = maxLength)
  
  i <- 1
  while (i <= n){
    
    number <- as.integer (runif(1, min = 1, max = maxLength))
    
    if (viz[number] == FALSE){
      viz[number] = TRUE
      i <- i + 1
    }
    
  }
  
  i <- 1
  for(line in 1 : n){
    
    while(viz[i] == FALSE)
      i <- i + 1
    
    A[line, m + 2] <- as.integer(i - maxLength / 2)
    viz[i] = FALSE
  }
  
  i <- 1
  while (i <= m){
    
    number <- as.integer (runif(1, min = 1, max = maxLength))
    
    if (viz[number] == FALSE){
      viz[number] = TRUE
      i <- i + 1
    }
    
  }
  
  i <- 1
  for(column in 1 : m){
    
    while(viz[i] == FALSE)
      i <- i + 1
    
    A[n + 2, column] <- as.integer(i - maxLength / 2)
    viz[i] = FALSE
    
  }
  
  return (A)
  
}

#' 
#' @brief Function that verifies if there cand be added and extra NaN
#' on the last line or the last column of the joint distribution matrix
#' for two random variables X and Y.
#' 
#' @returns the modified matrix   A.
#' 
bonusNaN <- function (A, n, m){
  
  # particular case for a bonus NaN value on the last row
  for (column in 1 : (m - 1)){
    
    if (is.na (A[n, column]) && is.na (A[n, column + 1])){
      
      A[n + 1, column + 1] <- NaN;
      break
    }
    
  }
  
  
  #particular case for a bonus NaN value on the last column
  for (line in 1 : (n - 1)){
    
    if (is.na (A[line, m]) && is.na (A[line + 1, m])){
      
      A[line + 1, m + 1] <- NaN;
      break
    }
    
  }
  
  return (A)
  
}



#' 
#' @brief Function that generates the  pi[i][j] repartition for the random
#' variables X and Y. It will be located in the inner matrix between
#' the lines (1, n) and columns (1, m).
#' 
#' @returns the modified matrix   A.
#' 
innerContentGenerate <- function(A, n, m){
  
  for (line in 1 : n){
    
    for(column in 1 : m){
      
      if (line != column - 1 && line != column){
        
        A [line, column] <- A [line, m + 1] * A [n + 1, column]
      
      }
      
    }
    
  }
  
  A <- bonusNaN (A, n, m)
  
  return (A)
}


#' 
#' @brief Exercise 3, paragraph a. Function that generates an incomplete 
#' joint distribution matrix between the INDEPENDENT random variables X and Y.
#' 
#' @returns the generated matrix   A.
#'
frepcomgenINDEPENDENT <- function (n = 0, m = 0){
  
  A = matrix (nrow = n + 2, ncol = m + 2)
  
  A <- piGenerate (A, n, m)
  
  A <- qjGenerate (A, n, m)
  
  A <- innerContentGenerate (A, n, m)
  
  A <- generateXAndY (A, n ,m)
  
  A [n + 1, m + 1] <- 1
  
  return (A)
  
}



#' 
#' @brief Function that generates a most probably dependent joint
#' distribution between the random variables X and Y. The function generates
#' every number, including P[i] and Q[j]. 
#' 
#' @returns the modified matrix   A.
#' 
fullGenerate <- function (A, n, m){
  
  # generate the random numbers
  for (line in 1 : n){
    
    for(column in 1 : m){
      
      A [line, column] <- as.integer (runif(1, min = 0, max = 50))
      
      A [n + 1, column] <- A [n + 1, column] + A [line, column] #bottom sum
      A [line, m + 1] <- A [line, m + 1] + A [line, column]     #right sum
      A [n + 1, m + 1] <- A [n + 1, m + 1] + A[line, column]    #total sum
      
    }
    
  }
  
  #now transport them into a [0,1] space
  for (line in 1 : (n + 1)){
    
    for(column in 1 : (m + 1)){

      A [line, column] <- A [line, column] / A [n + 1, m + 1]
      
    }
    
  }
  
  
  return (A)
  
}



#' 
#' @brief Function that deleted the unnecessary values from the joint 
#' distribution between two random variables X and Y.
#' 
#' @returns the modified matrix   A.
#' 
deleteUnnecessary <- function (A, n, m){
  
  line <- 1
  column <- 1
  
  while (line <= n && column <= m){
    
      A [line, column] <- NaN
      if(column + 1 <= m){
        A[line, column + 1] = NaN
      }
      line <- line + 1
      column <- column + 1
  }
  
  A <- bonusNaN (A, n, m)
  
  return (A)
  
}



#' 
#' @brief Exercise 3, paragraph a. Function that generates an incomplete 
#' joint distribution matrix between the PROBABLY DEPENDENT
#' random variables X and Y.
#' 
#' @returns the generated matrix   A.
#'
frepcomgenRANDOM <- function (n = 0, m = 0){
  
  A = matrix (data = 0, nrow = n + 2, ncol = m + 2)
  
  A <- fullGenerate (A, n, m)
  
  A <- deleteUnnecessary (A, n, m)
  
  A <- generateXAndY (A, n ,m)
  
  return (A)
  
}



#' 
#' @brief Exercise 3, paragraph b. Fills out the empty spaces of the inner
#' uncompleted join distribution matrix between two random variables X and Y.
#' 
#' @returns the modified matrix   A.
#'
fillInner <- function (A, n, m){
  
  line <- 1
  column <- 1
  
  while (line <= n && column <= m){
    
    A [line, column] <- A[n + 1, column]
    for (i in 1 : n){
      
      if (line != i){
        
        A[line, column] <- A[line, column] - A[i, column]
        
      }
      
    }
    
    if(column + 1 <= m){
      
      A [line, column + 1] <- A[line, m + 1]
      for (j in 1 : m){
        
        if (column + 1 != j){
          
          A[line, column + 1] <- A[line, column + 1] - A[line, j]
          
        }
        
      }
    }
    
    
    line <- line + 1
    column <- column + 1
  }
  
  return (A)
}


#' 
#' @brief Exercise 3, paragraph b. Fills out the empty spaces of the outer
#' uncompleted join distribution matrix between two random variables X and Y.
#' 
#' @returns the modified matrix A.
#'
fillOuter <- function (A, n, m){
 
  # particular case for a bonus NaN value on the last row
  for (column in 1 : m){
    
    if (is.na (A[n + 1, column])){
      
      A [n + 1, column] <- A [n + 1, m + 1]
      
      for (j in 1 : m)
        if (j != column)
          A [n + 1, column] <- A[n + 1, column] - A[n + 1, j]
    
      break
      
    }
    
  }
  
  
  #particular case for a bonus NaN value on the last column
  for (line in 1 : n){
    
    if (is.na (A[line, m + 1])){
      
      A [line, m + 1] <- A [n + 1, m + 1]
      
      for (i in 1 : n)
        if (i != line)
          A [line, m + 1] <- A[line, m + 1] - A[i, m + 1]
        
        break
        
    }
    
  }
  
  return (A)
   
}
  


#' 
#' @brief Exercise 3, paragraph b. Fills out the empty spaces of the uncompleted
#' join distribution matrix between two random variables X and Y.
#' 
#' @returns the modified matrix @b A.
#'
fcomplrepcom <- function (A, n, m){
  
  A <- fillInner (A, n, m)
  
  A <- fillOuter (A, n ,m)
  
  return (A)
  
}

#' 
#' @brief Function that calculates the median of X and Y from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' A.K.A. E[XY]
#' 
#' @returns the median @b median.
#' 
medianXY <- function (A, n, m, coefx, coefy){
  
  median <- 0
  
  for(line in 1 : n)
    for(column in 1 : m)
      median <- median + coefx * coefy * A [n + 2, column] * A [line, m + 2] * A[line, column]
  
  return (median)
}


#' 
#' @brief Function that calculates the median of X from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' A.K.A. E[X]
#' 
#' @returns the median @b median.
#' 
medianX <- function (A, n, m, coef){
  
  median <- 0
  
  for(column in 1 : m)
      median <- median + coef * A [n + 2, column] * A[n + 1, column]
    
  return (median)
}

#' 
#' @brief Function that calculates the median of Y from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' A.K.A. E[Y]
#' 
#' @returns the median @b median.
#' 
medianY <- function (A, n, m, coef){
  
  median <- 0
  
  for(line in 1 : n)
      median <- median + coef * A [line, m + 2] * A[line, m + 1]
  
  return (median)
}



#' 
#' @brief Exercise 3, paragraph c1. Calculates the covergence from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' @returns the covergence @b cov.
#' 
P3C1 <- function (A, n, m, coefx, coefy){
  
  
  cov <- medianXY (A, n, m, coefx, coefy)
  cov <- cov - medianX (A, n, m, coefx) * medianY (A, n, m, coefy)
  
  return (cov)
}


#' 
#' @brief Exercise 3, paragraph c2. Calculates the probability P(0 < X < 5 | Y > 4)
#' from the joint distribution matrix between two random variables X and Y. 
#' 
#' @returns the covergence @b probability.
#' 
P3C2 <- function (A, n, m){

  probability <- 0

  line <- -1
  for (i in 1 : n)
    if (A [i, m + 2] > 4){
      line <- i
      break
    }

  column <- -1
  for (j in 1 : m)
    if (A [n + 2, j] > 0){
      column <- j
      break
    }


  if (column == -1 || line == -1)
    return (probability)

  for (i in line : n)
    for (j in column : m)
      if (A [n + 2, j] < 5){
        probability <- probability + A [i, j]
      } else {
        break
      }

  return (probability)


}


#' 
#' @brief Exercise 3, paragraph c2. Calculates the probability P(X > 3 | Y < 7)
#' from the joint distribution matrix between two random variables X and Y. 
#' 
#' @returns the covergence @b probability.
#'
P3C3 <- function (A, n, m){
  
  probability <- 0
  
  # the line from X
  column <- -1
  for (j in 1 : m)
    if (A [n + 2, j] > 3){
      column <- j
      break
    }
  # Y starts from 1 until it becomes greater or equal with 7
  line <- 1
  
  # if we have no possible outcome
  if(column == -1)
    return (probability)
  
  for (line in 1 : n)
    if(A[line, m + 2] >= 7){
      break
    } else {
      
      for(j in column : m)
        probability <- probability + A[line, j]
      
    }
  
  return (probability) 
  
  
  
}

#' 
#' @brief Exercise 3, paragraph D1. Calculates if two random variables X and Y are
#' independent or not.
#' 
#' @returns 0 if the values are NOT independent and 1 otherwise.
#'
fverind <- function (A, n, m){
  
  independence <- 1  
  for (line in 1 : n)
    for (column in 1 : m)
      if(A [n + 1, column] * A [line, m + 1] - A[line, column] > 0.001){
        independence <- 0
        return (independence)
      }
  
  return (independence)
}



#' 
#' @brief Function that calculates the median of X^2 from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' A.K.A. E[X^2]
#' 
#' @returns the median @b median.
#' 
medianXSquared <- function (A, n ,m){
  
  median <- 0
  
  for(column in 1 : m)
    median <- median + A [n + 2, column] * A [n + 2, column] * A [n + 1, column]
  
  return (median)
  
}



#' 
#' @brief Function that calculates the median of Y^2 from the joint 
#' distribution matrix between two random variables X and Y.
#' 
#' A.K.A. E[Y^2]
#' 
#' @returns the median @b median.
#' 
medianYSquared <- function (A, n ,m){
  
  median <- 0
  
  for(line in 1 : n)
    median <- median + A [line, m + 2] * A [line, m + 2] * A [line, m + 1]
  
  return (median)
  
}

#' 
#' @brief Calculates the variation of the random variable X.
#' 
#' 
#' @returns the variation.
#'
variationX <- function (A, n, m){
  
  variation <- medianXSquared (A, n, m) - medianX(A, n, m, 1) * medianX(A, n, m, 1)
  return (variation)
}

#' 
#' @brief Calculates the variation of the random variable Y.
#' 
#' 
#' @returns the variation.
#'
variationY <- function (A, n, m){
  
  variation <- medianYSquared (A, n, m) - medianY(A, n, m, 1) * medianY(A, n, m, 1)
  return (variation)
}

#' 
#' @brief Exercise 3, paragraph D2. Calculates if two random variables X and Y are
#' corelated or not.
#' 
#' @returns 0 if the values are NOT corelant and 1 otherwise.
#'
fvernecor <- function (A, n, m){
  
  ro <- P3C1 (A, n, m, 1, 1)
  
  squareRoot <- sqrt(variationX (A, n, m) * variationY (A, n, m))
  
  ro <- ro / squareRoot
  
  
  print (abs(ro))
  
  if (abs(ro) < 0.25)
    return (1)
  
  return (0)
}


n <- 2  #numar de linii
m <- 3  #numar de coloane

A <- frepcomgenINDEPENDENT (n, m)
print("A: Matricea independenta creata incomplet este:")
A

A <- fcomplrepcom (A, n, m)
print("B: Matricea independenta creata complet este:")
A

B <- frepcomgenRANDOM (n, m)
print("A: Matricea cel mai probabil dependenta creata incomplet este:")
B

B <- fcomplrepcom (B, n, m)
print("B: Matricea cel mai probabil dependenta creata complet este:")
B

print("C) 1):")
P3C1 (B, n, m, 3, 4)# 3C1

print("C) 2):")
P3C2 (B, n, m)# 3C2

print("C) 3):")
P3C3 (B, n, m)# 3C3


print("D) 1)   0 pentru neindependente, 1 pentru independente:")
fverind (B, n, m)# 3D1 maybe dependant
fverind (A, n, m)# 3D1 independant


print("D) 2)   0 pentru necorelate, 1 pentru corelate:")
fvernecor (B, n, m)
fvernecor (A, n, m)# for the independent