library(tidyverse)
df <- data.frame(c(rep(1.389,42),rep(4.661,48),rep(9.991,24),rep(15.482,18),rep(20.232,15),rep(26.616,14),rep(40.278,16),rep(56.414,12),rep(74.985,6),rep(106.851,11),rep(184.735,5),rep(264.025,4),rep(300,3)))
names(df) <- "average"

gamma <- function(a){ #create the gamma function, being used in the gamma distribution
  fun <- function(x){ #create the function being used in the integral
    x^(a-1)*exp(-x)
  }
  int <- integrate(fun, 0, Inf) #integrate the function
  int$value #return numeric answer
}

loglik <- function(p,z){ #create the loglikelihood function using the gamma function, parameters p and data z
  a <- p[1]
  b <- p[2]
  ll <- z^(a-1)/(gamma(a)*b^a)*exp(-z/b)
  sum(log(ll))
}

p <- c(1,2) #inital values
m <- optim(p, loglik, hessian = TRUE,control = list(fnscale=-1), z = df$average) #optimization
#we know that the mean of the gamma distribution is equal to a*b. By comparing the mean of the fitted gammadistribution
#by the mean of the data we can see whether we have done it right
mean(df$average)
m$par[1]*m$par[2]
#as they are both close to 33.5 we get the idea that the optimization went well. We can check this visually:
x.grid <- seq(0, to = 300, by =0.01)
estimated.density <- x.grid^(m$par[1]-1)/(gamma(m$par[1])*m$par[2]^m$par[1])*exp(-x.grid/m$par[2])
dd <- data.frame(x.grid, estimated.density)

ggplot(df) +
  geom_histogram(aes(x=df$average, stat(density)), binwidth = 5) + #plot the data
  geom_line(data = dd, aes(x=x.grid, y=estimated.density), color = "red")+ #plot the density function
  ylim(0,0.1) #only show y from 0 to 0.1 to make it visually more pleasant

df2 <- data.frame(c(rep(1.25,101),rep(5,132),rep(10,61),rep(15,50),rep(20,29),rep(27.5,50),rep(40,28),rep(57.5,40),rep(77.5,22),rep(106.25,27),rep(150,19),rep(200,6),rep(275,10), rep(412.5,4), rep(500, 5)))
names(df2) <- "average"
m2 <- optim(p, loglik, hessian = TRUE,control = list(fnscale=-1), z = df2$average)
estimated.density2 <- x.grid^(m2$par[1]-1)/(gamma(m2$par[1])*m2$par[2]^m2$par[1])*exp(-x.grid/m2$par[2])
dd2 <- data.frame(x.grid, estimated.density2)

ggplot(df2) +
  geom_histogram(aes(x=df2$average, stat(density)), binwidth = 5) + #plot the data
  geom_line(data = dd2, aes(x=x.grid, y=estimated.density2), color = "red")+ #plot the density function
  ylim(0,0.1) #only show y from 0 to 0.1 to make it visually more pleasant

#Assuming that the payments x_i follow a gamma distribution with parameters a and b, the point 
#estimate for the expected payment is the E(X_i) = a*b
expected.payment <- m$par[1]*m$par[2]

