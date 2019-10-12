
library(wooldridge)
df <- data.frame(hprice1)
data.matrix <- as.matrix(hprice1[,c("assess","bdrms","lotsize","sqrft","colonial")])

loglik <- function(p,z){
  beta <- p[1:5]
  sigma <- p[6]
  y <- log(data.matrix[,1])
  eps <- (y - beta[1] - z[,2:5] %*% beta[2:5]) #y - X'b
  ll <- dnorm(eps/sigma,log=TRUE) - log(abs(sigma)) #density function of normal distribution with mean (e/sigma) and log is taken. 
  #why divide by sigma??                   
  sum(ll)
  #-nrow(z)*log(sigma)-0.5*sum((eps/sigma)^2)
}
p0 <- c(5,0,0,0,0,2)
m <- optim(p0,loglik,method="BFGS",control=list(fnscale=-1,trace=10),hessian=TRUE,z=data.matrix)
rbind(m$par,sqrt(diag(solve(-m$hessian))))


m.ols <- lm(log(assess)~bdrms+lotsize+sqrft+colonial,data=hprice1)
summary(m.ols)

elasticity.at.mean <- mean(hprice1$lotsize) * m$par[3]
var.coefficient <- solve(-m$hessian)[3,3]
var.elasticity <- mean(hprice1$lotsize)^2 * var.coefficient
# upper bound
elasticity.at.mean + qnorm(0.975)* sqrt(var.elasticity)
