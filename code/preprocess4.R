##########################
# Study 4
##########################

study4 <- data[data$experiment=="Sentencing",]

# check for attentiveness
study4$passed <- "Disengaged Respondent"
study4$passed[study4$Q82 == "Oregon"] <- "Engaged Respondent"

study4$nullify <- 0
study4$nullify[study4$Q53 == "Community service"] <- 1
study4$pardon <- recode(study4$Q76, "Strongly support" = 5, "Support"=4, "Neither support nor oppose"=3, "Oppose"=2,"Strongly oppose" = 1)

study4$sentence <- as.factor(study4$Q53)
study4$sentence <- fct_relevel(study4$sentence, "Community service",          
							   "1 - 3 days in jail",
							   "4 - 30 days in jail",
							   "2 - 3 months in jail",                
							   "4 - 6 months in jail",
							   "7 months to 1 year in jail",
							   "2 - 5 years in prison",
							   "6 - 10 years in prison",
							   "11 - 15 years in prison",
							   "16 - 20 years in prison",
							   "More than 20 years in prison")

study4$item.crime <- as.factor(study4$item.crime)

study4$crime <- recode_factor(study4$item.crime,  "protesting without a permit" = "Protesting without\na Permit",
							  "vandalism" = "Vandalism",
							  "assault" = "Assault",
							  "arson" = "Arson",
							  "assault with a deadly weapon" = "Assault with a\nDeadly Weapon",
							  "murder" = "Murder")


