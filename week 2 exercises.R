library(tidyverse)
fixpt_own.r <- function(x0, tol, maxit){
  xold <- x0
  xnew <- g(xold)
  i <- 1
  cat("At iteration 1 the value of x is", xnew, "\n")
  while(abs(xnew - xold) > tol && i < maxit){
    xold <- xnew
    xnew <- g(xold)
    i <- i+1
    cat("At iteration", i, "the value of x is", xnew, "\n")
  }
  if(abs(xnew - xold) > tol){
    cat("Algorithm failed to converge")
    return(NULL)
  } else{
    cat("Algorithm converged")
    return(xnew)
  }
}

gf <- function(x){
  g <- (1 + x^(-1) - log(x))
  return(g) #why does this not work if I use the whole derivative for the bisection/secant method?
}
fixpt_own.r(3, 1e-6, 400)


bisection <- function(x1, x2, tol, maxit){
  if(g(x1)*g(x2) > 0){ 
    cat("The initial interval does not contain the root")
    return(NULL)
  }
  i <- 1
  while(g(x1)*g(x2) <0 && abs(x1 - x2) > tol && i < maxit){
    xnew <- 0.5*(x1 + x2)
    if(g(x1)*g(xnew)<0){
      x2 <- xnew
    } else {
      x1 <- xnew
    }
    i <- i + 1
    cat("At iteration", i, "your interval is", x1, x2, "\n")
  }
}

secant <- function(x1, x2, tol, maxit){
  i <- 1
  while((abs(x2 - x1) > tol) && (i < maxit)){
    xnew <- x2 - g(x2) * (x2 - x1)/(g(x2) - g(x1))
    x1 <- x2
    x2 <- xnew
    i <- i+1
    cat("The interval contains", x1, "and", x2, "\n")
  }
  if(abs(x2 - x1) > tol){
    cat("Algorithm failed to converge")
    return(NULL)
  } else{
    cat("Algorithm converged")
    return(x2)
  }
}
secant(0.5,6,0.0001,150)

g <- function(S,K,r,sigma,t){
  d1 <- (log(S/K) + (r + 0.5* sigma^2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  C <- S*pnorm(d1) - pnorm(d2) * K * exp(-r*t)
  return(C)
}
f.sigma <- function(sigma){
  g(S =100, K=105, t=20, r=0.01/365, sigma) - 1.70
}

s <- function(x1, x2, tol, maxit){
  i <- 1
  while(abs(x2 - x1) > tol && i < maxit){
  xnew <- x2 - f.sigma(x2) * (x2 - x1)/(f.sigma(x2) - f.sigma(x1))
  x2 <- xnew
  x1 <- x2
  i <- i+1
  cat("At iteration", i, "your value is", xnew, "\n")
  }
  if(abs(x2 - x1) > tol){
    cat("Algorithm failed to converge")
    return(NULL)
  } else{
    cat("Algorithm converged")
    return(x2)
  }
}
s(0.001,0.5, 1e-6,100)













