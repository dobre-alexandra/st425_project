
library(tidyverse)
library(forecast)
library(scales)
library(extraDistr)
library(gganimate)
library(transformr)
library(ggplot2)
library(magick)
library(gifski)

#Problem 1
#Part C: Algorithm that generates n numbers from a tPoisson(lambda, k) distribution: 

generate_tpoisson <- function(lambda, k, n) {
  if (k < 0) {
    warning("Truncation point k must be greater than or equal to 0. Setting k to 0")
    k <- 0 
  }
  samples <- c() 
  while (length(samples) < n) {  
    poisson_samples <- rpois(n, lambda)    
    truncated_samples <- poisson_samples[poisson_samples > k] 
    samples <- c(samples, truncated_samples)    
  }
  return(samples[1:n]) 
}

# Example
set.seed(47)  
tpoisson_samples <- generate_tpoisson(lambda = 8, k = 7, n = 500)
head(tpoisson_samples)
length(tpoisson_samples)
var(tpoisson_samples)

# Creating an animated histogram to show how the generate_tpoisson function works:
generate_tpoisson_animation <- function(lambda, k_values, n) { 
  data <- lapply(k_values, function(k) { 
    truncated_data <- generate_tpoisson(lambda, k, n)
    data.frame(k = k, value = truncated_data) 
  })
  df <- do.call(rbind, data) 
  
  p <- ggplot(df, aes(x = value)) + 
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Truncated Poisson") +
    theme_minimal() +
    transition_states(k, transition_length = 2, state_length = 1) + 
    ggtitle("Truncated Poisson Distribution, k = {closest_state}")
  
  animate(p, duration = 10, fps = 20, width = 800, height = 600) 
}
tpoisexample <- generate_tpoisson_animation(8, 5:10, 500) 
anim_save("example_tpois.gif", tpoisexample)



# Part D:
# importing the Assignment Data 
X <- read.csv("/Users/anniechapman/Desktop/OTHER/Data/Assignment.Data.csv" , header = FALSE ) 
X <- as.numeric(X$V1)
hist(X, breaks = 10, col = "lightgreen", 
     main = "Histogram of Assignment Data", 
     xlab = "Value", ylab = "Frequency")

#f(λ): Score function - the derivative of the log-likelihood with respect to λ. If this was set to 0 it would be the MLE
f_λ <- function(lambda, X) { 
  n <- length(X)
  sum_X <- sum(X[2:n]) 
  term1 <- -n 
  term2 <- (1 / lambda) * sum_X
  term3 <- (n * (lambda * exp(-lambda))) / (1 - lambda * exp(-lambda) - exp(-lambda)) 
  return(term1 + term2 - term3)
}

#f'(λ): The derivative of f(λ) with respect to λ - the second derivative of the log-likelihood
f_prime_λ <- function(lambda, X) {
  n <- length(X)
  sum_X <- sum(X[2:n]) 
  
  # Derivative calculations for each term
  term1_prime <- 0 
  term2_prime <- -sum_X / (lambda^2)
  
  # Derivative of term3
  numerator <- (
    ((-n * exp(-lambda)) + (n * exp(-lambda) * lambda) * (1 - lambda * exp(-lambda) - exp(-lambda))) - 
      (( -n * (lambda * exp(-lambda)) * (-2 * exp(-lambda) - lambda * exp(-lambda))))
  )
  denominator <- (1 - lambda * exp(-lambda) - exp(-lambda))^2
  term3_prime <- numerator / denominator
  
  return(term2_prime - term3_prime) 
}

# Newton-Raphson implementation to solve for MLE:
newton_raphson <- function(X, initial_lambda = 1, tol = 1e-6, max_iter = 100) { 
  lambda <- initial_lambda 
  for (i in 1:max_iter) {
    f_value <- f_λ(lambda, X)
    f_prime_value <- f_prime_λ(lambda, X)
    lambda_new <- lambda - (f_value / f_prime_value)
    
    if (abs(lambda_new - lambda) < tol) {
      return(lambda_new) 
    }    
    lambda <- lambda_new    
  }
  stop("Newton-Raphson did not converge") 
}

λ_mle <- newton_raphson(X, 2, 1e-6, 1000) 
cat("Estimated MLE of λ:", λ_mle, "\n")

# find CI / SE of MLE lambda: 
bootstrap_ci <- function(X, B = 1000) {
  mle_bootstrap <- numeric(B)
  for (b in 1:B) {
    X_boot <- sample(X, length(X), replace = TRUE)
    mle_bootstrap[b] <- newton_raphson(X_boot, 2, 1e-6, 1000) 
  }
  se_bootstrap <- sd(mle_bootstrap) 
  ci_lower <- quantile(mle_bootstrap, 0.025)
  ci_upper <- quantile(mle_bootstrap, 0.975)
  return(list(se = se_bootstrap, ci = c(ci_lower, ci_upper)))
}

bootstrap_results <- bootstrap_ci(X)
cat("Bootstrap 95% CI for λ:", bootstrap_results$ci, "\n")
ci_lower <- bootstrap_results$ci[1]
ci_upper <- bootstrap_results$ci[2]


#Visual of assignment data
png("data_plot.png", width = 1200, height = 600, res = 150)
par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  
hist_data <- hist(X, plot = FALSE)  
hist(X, 
     breaks = 12, 
     col = "gray80",  
     border = "white", 
     main = "Assignment Data", 
     xlab = "Value", 
     ylab = "Frequency", 
     xlim = c(0, 8), 
     ylim = c(0, 1.2 * max(hist_data$counts)), 
     cex.axis = 1.0,  
     cex.lab = 1.0,    
     cex.main = 1.6)   

abline(v = λ_mle, col = "red", lwd = 2, lty = 1)      
abline(v = ci_lower, col = "black", lwd = 2, lty = 2)  
abline(v = ci_upper, col = "black", lwd = 2, lty = 2)  

legend("topright", 
       legend = c("MLE", "Confidence Intervals"), 
       col = c("red", "black"), 
       lty = c(1, 2), 
       lwd = 2, 
       cex = 1.2, 
       bty = "n")  

# Part E: does the algorithm from part C work effectively for all values of K and λ?
print(system.time({
  generate_tpoisson(5,20,1)
}))

print(system.time({
  generate_tpoisson(5,25,1)
}))

#Part F: f) For λ = 8, use your simulation to plot separately the mean and the variance of tPoisson(λ, k) against k for k = 0, 1, …, 20.
# coding our derivation of the mean and variance for the truncated poisson:
calculate_qk <- function(lambda, k, n) { 
  qk <- 1 - sum(dpois(0:k, lambda)) 
  qk_minus_1 <- 1 - sum(dpois(0:(k-1), lambda)) 
  qk_minus_2 <- 1 - sum(dpois(0:(k-2), lambda))
  return(c(qk, qk_minus_1, qk_minus_2)) 
} 

# "theoretical" values are created using the formulas we derived for the mean and variance. 
# "simulated" values are created from using mean() and var() in r

# formula for the mean and variance:  
truncated_poisson_theoretical_mean_var <- function(lambda, k) { 
  qk_values <- calculate_qk(lambda, k) 
  qk <- qk_values[1] 
  qk_minus_1 <- qk_values[2] 
  qk_minus_2 <- qk_values[3] 
  
  mean_k_theoretical <- (lambda * qk_minus_1) / qk 
  
  var_k_theoretical <- ((lambda^2 * (qk * qk_minus_2 - qk_minus_1^2)) / (qk^2) + (lambda * (qk_minus_1))/qk) 
  
  return(list(mean = mean_k_theoretical, variance = var_k_theoretical))
}

lambda <- 8
n <- 10000 

mean_values_sim <- numeric(21)
variance_values_sim <- numeric(21) 
mean_values_theoretical <- numeric(21)
variance_values_theoretical <- numeric(21)

# generating the mean and variance values for k = 0, 1, ..., 20
for (k in 0:20) {
  
  if(length(k) > 1) { 
    stop("Error: k should be a single value, not a vector") 
  }
  
  samples <- generate_tpoisson(lambda, k, n)  
  mean_values_sim[k + 1] <- mean(samples)  
  variance_values_sim[k + 1] <- var(samples) 
  
  results_theoretical <- truncated_poisson_theoretical_mean_var(lambda, k) 
  mean_values_theoretical[k + 1] <- results_theoretical$mean  
  variance_values_theoretical[k + 1] <- results_theoretical$variance 
}


png("tpoisson_plots_neutral.png", width = 1200, height = 600, res = 150)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2) + 0.1, oma = c(0, 0, 2, 0), family = "serif")
mean_color <- "gray30"
variance_color <- "gray50"

plot(0:20, mean_values_theoretical,  
     type = "o", 
     col = mean_color, 
     pch = 16, 
     lwd = 2, 
     main = "Mean",
     xlab = "Truncation Point (k)", 
     ylab = "Mean",
     cex.axis = 1.2,   
     cex.lab = 1.0,    
     cex.main = 1.6)   

plot(0:20, variance_values_theoretical, 
     type = "o",                       
     col = variance_color,        
     pch = 16,                         
     lwd = 2,                           
     main = "Variance", 
     xlab = "Truncation Point (k)", 
     ylab = "Variance",
     cex.axis = 1.2,   
     cex.lab = 1.0,    
     cex.main = 1.6)   

mtext("Mean & Variance of tPoisson(λ = 8, k = 0:20)", 
      outer = TRUE, 
      cex = 1.8, 
      font = 2)
png("tpoisson_plots.png", width = 1200, height = 600, res = 150)
dev.off()

# animated variance plot: 
varvalues <- c( 8.0000000, 7.8487939, 7.5570374, 6.9862337, 6.2151185, 5.3667437, 4.5518620, 3.8329364, 3.2294092, 2.7358722, 2.3367959, 2.0147497, 1.7539576, 1.5413696, 1.3666433, 1.2217462, 1.1004880, 0.9981030, 0.9109105, 0.8360515, 0.7712902) # values saved from what was calculated for theoretical variance

k <- 0:20
var_df <- data.frame(k = as.numeric(k), variance = varvalues)

animated_plot <- ggplot(var_df, aes(x = k, y = variance)) +
  geom_line(color = "mediumpurple3", linewidth = 1.5, alpha = 0.8) + 
  geom_point(color = "mediumpurple4", size = 3) + 
  labs(
    title = "Variance tPoisson(8, k = 0:20)",
    x = "Truncation Point (k)",
    y = "Variance"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 20), breaks = 0:20) +
  transition_reveal(k)

anim <- animate(animated_plot, nframes = 50, fps = 10, renderer = gifski_renderer(), frame_format = "%d")
anim_save("animated_variance_plot.gif", animation = anim)

# animated mean plot: 
meanvals <- c( 8.000000,  8.021535,  8.087076,  8.239144,  8.508701,  8.906110,  9.423054, 10.041339, 10.740667, 11.502824, 12.313042, 13.159945, 14.034974, 14.931746, 15.845498, 16.772657, 17.710519, 18.657022, 19.610572, 20.569928, 21.534110) # values saved from what was calculated for theoretical mean

k <- 0:20
mean_df <- data.frame(k = as.numeric(k), mean = meanvals)

animated_plot <- ggplot(mean_df, aes(x = k, y = mean)) +
  geom_line(color = "orangered3", linewidth = 1.5, alpha = 0.8) + 
  geom_point(color = "orangered4", size = 3) + 
  labs(
    title = "Mean of tPoisson(8, k = 0:20)",
    x = "Truncation Point (k)",
    y = "Mean"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 20), breaks = 0:20) +
  transition_reveal(k)

anim <- animate(animated_plot, nframes = 50, fps = 10, renderer = gifski_renderer(), frame_format = "%d")
anim_save("animated_mean_plot.gif", animation = anim)



# plotting the "theoretical" values vs. the "simulated" values 
par(mfrow = c(1, 2))  

# mean 
plot(0:20, mean_values_sim,  type = "l", col = alpha("blue", 0.9), pch = 19, lwd = 1.5, 
     main = "Simulated Mean vs Theoretical Mean of tPoisson(8, k)",
     xlab = "k", ylab = "Mean")
lines(0:20, mean_values_theoretical, type = "l", col = alpha("red", 0.6), pch = 19, lty = 2, lwd = 2)
legend("bottomright", legend = c("Simulated", "Theoretical"), 
       col = c(alpha("blue", 0.8), alpha("red", 0.8)), 
       lty = 1:2, lwd = 1.5:2, bty = "n")

# variance 
plot(0:20, variance_values_sim, type = "l", col = alpha("blue", 0.8), pch = 19, lwd = 1.5,
     main = "Simulated Variance vs Theoretical Variance of tPoisson(8, k)",
     xlab = "k", ylab = "Variance")
lines(0:20, variance_values_theoretical, type = "l", col = alpha("red", 0.8), pch = 19, lty = 2, lwd = 2)
legend("topright", legend = c("Simulated", "Theoretical"), 
       col = c(alpha("blue", 0.8), alpha("red", 0.8)), 
       lty = 1:2, lwd = 1.5:2, bty = "n")

