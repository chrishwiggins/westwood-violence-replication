
#################
# Cheerleading and trolling
#################

study3$cheer <- 0 
study3$cheer[study3$Q5 == "Photo A - Trump Inauguration"] <- 1

study3 %>% group_by(passed, cheer) %>% summarise(mean_violence = weighted.mean(supportactions, w=weight, na.rm=T), count = n())


support <- lm(supportactions~alignment * cheer, data=study3, weights=weight)
justified <- lm(justified~alignment * cheer, data=study3, weights=weight)
charged <- lm(charged~alignment * cheer, data=study3, weights=weight)

stargazer(support, justified, charged, 
		  column.labels = c("Support", "Justification", "Charged"), 
		  dep.var.labels.include = F,
		  covariate.labels = c("OutParty Shooter","Cheerleader", "Cheerleader X OutParty","Intercept"),
		  star.cutoffs = c(.05,.01,.001),
		  label = "tab:study3cheer",
		  ci = T,
		  title = "Cheerleading, Justifcation, Support and Charges by Political Alignment ",
		  type="latex",
		  out="../results/study3cheer.tex"
)


study3$shark <- 0 
study3$shark[study3$Q6 == "Yes"] <- 1

study3 %>% group_by(passed, shark) %>% summarise(mean_violence = weighted.mean(supportactions, w=weight, na.rm=T), count = n())


support <- lm(supportactions~alignment * shark, data=study3, weights=weight)
justified <- lm(justified~alignment * shark, data=study3, weights=weight)
charged <- lm(charged~alignment * shark, data=study3, weights=weight)

stargazer(support, justified, charged, 
		  column.labels = c("Support", "Justification", "Charged"), 
		  dep.var.labels.include = F,
		  covariate.labels = c("OutParty Shooter","Shark Bite", "Shark Bite X OutParty","Intercept"),
		  star.cutoffs = c(.05,.01,.001),
		  label = "tab:study3troll",
		  ci = T,
		  title = "Trolling, Justifcation, Support and Charges by Political Alignment ",
		  type="latex",
		  out="../results/study3trolling.tex"
)
weighted.mean(study3$supportactions[study3$passed == "Engaged Respondent"], w=study3$weight[study3$passed == "Engaged Respondent"]) -
	weighted.mean(study3$supportactions[study3$cheer==0 & study3$shark==0 & study3$passed == "Engaged Respondent"], w=study3$weight[study3$cheer==0 & study3$shark==0 & study3$passed == "Engaged Respondent"]
	)

c<- study3 %>% group_by(passed) %>% summarise(mean_violence = weighted.mean(violent, w=weight, na.rm=T), count = n())
