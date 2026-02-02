partial_bounds <- function(outcome, check, guess_rate,
						   a = 0, b = 1, conf_level = 0.95) {
	Delta_coef_lo <- c(1, a/(1-guess_rate), -1/(1-guess_rate))
	Delta_coef_hi <- c(1, b/(1-guess_rate), -1/(1-guess_rate))
	
	X <- cbind(outcome, (1 - check), outcome * (1- check))
	Xbar <- colMeans(X)
	root_N <- sqrt(nrow(X))
	
	theta_lo <- c(Delta_coef_lo %*% Xbar)
	theta_hi <- c(Delta_coef_hi %*% Xbar)
	
	V_X <- cov(X)
	sd_lo <- sqrt(c(t(Delta_coef_lo) %*% V_X %*% Delta_coef_lo))
	sd_hi <- sqrt(c(t(Delta_coef_hi) %*% V_X %*% Delta_coef_hi))
	sd_max <- max(sd_hi, sd_lo)
	delta_hat <- theta_hi - theta_lo
	
	dist <- function(crit) {
		left <- pnorm(crit + root_N * delta_hat / sd_max) - pnorm(-crit)
		return(abs(left - conf_level))
	}
	res <- optim(1.0, dist, method = "Brent", lower = 0, upper = 10)
	crit <- res$par
	
	ci_lo <- max(min(theta_lo - crit * sd_lo / root_N, 1), 0)
	ci_hi <- max(min(theta_hi + crit * sd_hi / root_N, 1), 0)
	
	return(c(ci_lo, ci_hi))
}


study3partialID <- study3 %>% 
	filter(alignment == " In-Party Shooter") %>% 
	select(passed, alignment, justified, supportactions, charged) %>%
	mutate(y1 = justified,
		   y2 = if_else(supportactions >= 4, 1, 0),
		   y3 = charged,
		   c = if_else(passed == 'Engaged Respondent', 1, 0))

# Justified
mean_se(study3partialID$y1[study3partialID$c==1], mult=2) * 100
partial_bounds(outcome = study3partialID$y1, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0, b = 1) * 100
partial_bounds(outcome = study3partialID$y1, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0, b = 0.25) * 100

# Support
mean_se(study3partialID$y2[study3partialID$c==1], mult=2) * 100
partial_bounds(outcome = study3partialID$y2, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0, b = 1) * 100
partial_bounds(outcome = study3partialID$y2, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0, b = 0.2) * 100

# Charge
mean_se(study3partialID$y3[study3partialID$c==1], mult=2) * 100
partial_bounds(outcome = study3partialID$y3, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0, b = 1) * 100
partial_bounds(outcome = study3partialID$y3, check = study3partialID$c,
			   guess_rate = 1.0/7.0, a = 0.8, b = 1.0) * 100
