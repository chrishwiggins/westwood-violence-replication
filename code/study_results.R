# pooled values

mean(study1$justified[study1$alignment == "In-Party Driver" | study1$alignment == "Out-Party Driver"])

mean(study1$justified[study1$alignment != "In-Party Driver" | study1$alignment != "Out-Party Driver"])

mean(study2$justified[study2$alignment != "Apoltical Shooter"])

mean(study2$justified[study2$alignment == "Apoltical Shooter"])

wtd.mean(study3$justified, weights=study3$weight)

# % inattentive 
study1$violent<- ifelse(study1$Q77>1, 1, 0)
study2$violent<- ifelse(study2$Q77> 1,1,  0)
a<- study1 %>% group_by(passed) %>% summarise(mean_violence = mean(violent, na.rm=T), count = n())
b<- study2 %>% group_by(passed) %>% summarise(mean_violence = mean(violent, na.rm=T), count = n())

a$count[1] / (a$count[1]+ a$count[2])
b$count[1] / (b$count[1]+ b$count[2])

study3$passed01 <- 0
study3$passed01[study3$passed == "Engaged Respondent"] <- 1

1-weighted.mean(study3$passed01, w=study3$weight)

# Engaged v. disengaged results: Justification

prop.table(table(study1$passed[study1$alignment != "Apolitical Driver"], study1$justified[study1$alignment != "Apolitical Driver"]), margin=1)

prop.table(table(study1$passed[study1$alignment == "Apolitical Driver"], study1$justified[study1$alignment == "Apolitical Driver"]), margin=1)

prop.table(table(study2$passed[study2$alignment != "Apoltical Shooter"], study2$justified[study2$alignment != "Apoltical Shooter"]), margin=1)

prop.table(table(study2$passed[study2$alignment == "Apoltical Shooter"], study2$justified[study2$alignment == "Apoltical Shooter"]), margin=1)

#prop.table(GDAtools::wtable(study3$passed,study3$justified,  w = study3$weight)[1:2,1:2], margin=1)

# Engaged v. disengaged results: charged

prop.table(table(study1$passed[study1$alignment != "Apolitical Driver"], study1$charged[study1$alignment != "Apolitical Driver"]), margin=1)

prop.table(table(study2$passed[study2$alignment != "Apoltical Shooter"], study2$charged[study2$alignment != "Apoltical Shooter"]), margin=1)


#prop.table(GDAtools::wtable(study3$passed,study3$charged,  w = study3$weight)[1:2,1:2], margin=1)







study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(justified, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(supportactions >3, na.rm = TRUE),
			  ssd = sd(supportactions, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = 1-mean(charged, na.rm = TRUE),
			  ssd = sd(charged, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

17.1/2.15


study3 %>%
	group_by(alignment, passed) %>%
	summarise(smean = weighted.mean(justified, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(justified, weights=weight)[1],
			  upper = weighted.ttest.ci(justified, weights=weight)[2])

32.6/5.98

study3 %>%
	group_by(alignment, passed) %>%
	summarise(smean = weighted.mean(supportactions >3, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(supportactions, weights=weight)[1],
			  upper = weighted.ttest.ci(supportactions, weights=weight)[2])

26.10/2.89

study3 %>%
	group_by(alignment, passed) %>%
	summarise(smean = 1-weighted.mean(charged, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(charged, weights=weight)[1],
			  upper = weighted.ttest.ci(charged, weights=weight)[2])

15.1/3.98
