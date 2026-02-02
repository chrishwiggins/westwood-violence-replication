#######################
# Causal incentive
#######################

study3$passed01 <- 0
study3$passed01[study3$passed == 'Engaged Respondent'] <-1

study3$incentivize_agree <- study3$Q2
study3$incentivize_agree <- str_replace(study3$incentivize_agree, "\"","")
study3$incentivize_agree <- str_replace(study3$incentivize_agree, "\"","")
study3$incentivize_agree <- str_replace(study3$incentivize_agree, "“","")
study3$incentivize_agree <- str_replace(study3$incentivize_agree, "”","")
study3$incentivize_agree <- tolower(study3$incentivize_agree)

study3$incentive_passed <- 0
study3$incentive_passed[study3$incentivize_agree == "i agree" | study3$incentivize_agree == "__na__"] <- 1	

study3incent <- study3[study3$incentive_passed == 1 & study3$passed == "Engaged Respondent",]

study3incent <- study3incent[!is.na(study3incent$Q1),]

passed <- lm(passed01~incentivize, data=study3, weights=weight)

prop.table(table(study3$incentivize, study3$passed), margin=1)





stargazer(passed, 
		  column.labels = c("Passed"), 
		  dep.var.labels.include = F,
		  #covariate.labels = c("Incentivized","Intercept"),
		  star.cutoffs = c(.1,.05,.01,.001),
		  label = "tab:study3incentpassed",
		  ci = T,
		  title = "Study 3: Passing Engagement Test by Incentive Arm",
		  type="latex",
		  out="../results/study3incentpassed.tex"
)


support <- lm(supportactions~incentivize*alignment, data=study3incent, weights=weight)
justified <- lm(justified~incentivize*alignment, data=study3incent, weights=weight)
charged <- lm(charged~incentivize*alignment, data=study3incent, weights=weight)

stargazer(support, justified, charged, 
		  column.labels = c("Support", "Justification", "Charged"), 
		  dep.var.labels.include = F,
		  #covariate.labels = c("Incentivized","Intercept"),
		  star.cutoffs = c(.05,.01,.001),
		  label = "tab:study3icentmain",
		  ci = T,
		  title = "Study 3: Justification, Support and Charges by Political Alignment by Incentive Arm",
		  type="latex",
		  out="../results/study3incent.tex"
)

