#PART ONE: Simulating S
#Define function to simulate stock prices using a Geometric Brownian Motion model
simulate_stock_prices <- function(mu,sigma,S0,t){
  S <- list(S0)
  for (i in 1:t){ #Implement a for loop to simulate the stock prices at each time step i from 1 to t
    Z <- rnorm(n=1,mean=0,sd=1) #Generate a random number Z from a normal distribution
    S_i <- S[[i]]*exp(mu-0.5*sigma^2 + sqrt(sigma^2)*Z) #Use Geometric Brownian Motion equation to compute the stock price at time i
    S[[i+1]] <- S_i #Append the stock price at time i to the list of simulated stock prices
  }
  return(S) #Return the list of simulated stock prices
}

#Simulate stock prices for 50 time steps with mu=0.1, sigma2=0.25, S0=100, and t(days)=50
S <- simulate_stock_prices(0.1,0.25,100,50)

#Plot stock prices for 50 time steps with mu=0.1, sigma2=0.25, S0=100, and t(days)=50
X <- unlist(S)
plot(X,type="l",ylab="Stock Price",xlab="Time")

#Simulate stock prices for 100 time steps with mu=0.1, sigma2=0.25, and S0=100
S <- simulate_stock_prices(0.1,0.25,100,100)
print(S)
#Plot stock prices for 100 time steps with mu=0.1, sigma2=0.25, and S0=100
X <- unlist(S)
plot(X,type="l",ylab="Stock Price",xlab="Time")


#PART TWO: Estimating ES(t)
#set parameters for simulation
mu <- 0.01 #drift parameter
sigma <- 0.01 #volatility parameter
t <- 50 #time(days)

#Generate random samples from a normal distribution with mean 0 and sd 1
n_sim <- 10000 #number of simulations (10,000)
z <- rnorm(n_sim)

#Like earlier, Compute stock price at time t using the Geometric Brownian Motion model
s_t <- exp(mu-0.5*sigma^2+sqrt(sigma^2)*z)

#Estimate ES(t) and P(S(t) > S(0))
es_t <- mean(s_t)
p_s_t_gt_s_0 <- sum(s_t > 1)/n_sim

#Calculate the 95% confidence intervals
ci_es_t <- qt(c(0.025, 0.975), df = n_sim - 1) * sd(s_t) / sqrt(n_sim) + es_t
ci_p_gt_S0 <- qt(c(0.025, 0.975), df = n_sim - 1) * sqrt(p_s_t_gt_s_0 * (1 - p_s_t_gt_s_0) / n_sim)

cat("Estimate of ES(t):", es_t, "\n")
cat("95% confidence interval for ES(t):", ci_es_t, "\n")
cat("Estimate of P(S(t) > S(0)):", p_s_t_gt_s_0, "\n")
cat("95% confidence interval for P(S(t) > S(0)):", ci_p_gt_S0)

#PART THREE: Down-and-out call option
#Set the values for mu, sigma^2, K, t, and B
mu <- 0.1
sigma2 <- 0.1
K <- 1
t <- 10
B <- 0.8
n_sim <- 10000 #Set the number of simulations

#Simulate S(1), ... , S(t)
S <- matrix(NA, nrow = n_sim, ncol = t)
for (i in 1:n_sim) {
  S[i, 1] <- 1
  for (j in 2:t) {
    S[i, j] <- S[i, j-1] * exp(mu - 0.5 * sigma2 + sqrt(sigma2) * rnorm(1))
  }
}

#Calculate V(t)
V <- rep(NA, n_sim)
for (i in 1:n_sim) {
  if (S[i, t] >= K && min(S[i, ]) > B) {
    V[i] <- S[i, t] - K
  } else {
    V[i] <- 0
  }
}

#Estimate the cumulative distribution function of V(t)
cdf_V <- ecdf(V)
plot(cdf_V, xlab = "V(t)", ylab = "Cumulative distribution function")
