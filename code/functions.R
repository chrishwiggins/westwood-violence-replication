lower_ci <- function(mean, se, n, conf_level = 0.95){
	lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
	upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
	require(Hmisc)
	nx <- length(x)
	df <- nx - 1
	vx <- Hmisc::wtd.var(x, weights, normwt = TRUE) ## From Hmisc
	mx <- weighted.mean(x, weights)
	stderr <- sqrt(vx/nx)
	tstat <- mx/stderr ## not mx - mu
	alpha <- 1 - conf.level
	cint <- qt(1 - alpha/2, df)
	cint <- tstat + c(-cint, cint)
	cint * stderr
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

