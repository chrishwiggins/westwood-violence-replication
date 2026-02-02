m1 <- lm(supportactions01 ~ bussperryc01, data = study2[study2$passed=="Engaged Respondent" & study2$alignment == "In-Party Shooter",])
m3 <- lm(supportactions01 ~ bussperryc01, data = study2[study2$alignment == "In-Party Shooter",])
m4 <- lm(km ~ bussperryc01, data = study2[study2$passed=="Engaged Respondent",])
m6 <- lm(km ~ bussperryc01, data = study2)

stargazer(m1,m3,m4,m6,
		  column.labels = c("Our Measure (Engaged)", "Our Measure (Full Sample)", "Kalmoe-Mason (Engaged)", "Kalmoe-Mason (Full Sample)"), 
		  dep.var.labels.include = F,
		  covariate.labels = c("Buss Perry (0-1)", "Intercept"),
		  star.cutoffs = c(.05,.01,.001),
		  label = "tab:study2agression",
		  ci = T,
		  title = "Support for Violence by Aggression",
		  type="latex",
		  out="../results/aggression.tex"
)


m1_3 <- lm(supportactions01 ~ bussperry, data = study2[study2$passed=="Engaged Respondent" & study2$alignment == "In-Party Shooter",])
m3_3 <- lm(supportactions01 ~ bussperry, data = study2[study2$alignment == "In-Party Shooter",])
m4_3 <- lm(km ~ bussperry, data = study2[study2$passed=="Engaged Respondent",])
m6_3 <- lm(km ~ bussperry, data = study2)

stargazer(m1_3,m3_3,m4_3,m6_3,
		  column.labels = c("Our Measure (Engaged)", "Our Measure (Full Sample)", "Kalmoe-Mason (Engaged)", "Kalmoe-Mason (Full Sample)"), 
		  dep.var.labels.include = F,
		  covariate.labels = c("Buss Perry - Medium","Buss Perry - High", "Intercept"),
		  star.cutoffs = c(.05,.01,.001),
		  label = "tab:study2agression3",
		  ci = T,
		  title = "Support for Violence by Aggression Binned in Terciles",
		  type="latex",
		  out="../results/aggression3.tex"
)

