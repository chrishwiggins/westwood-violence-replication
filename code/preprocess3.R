study3 <- read_csv("../data/study3.csv")

study3$incentivize <-recode(study3$experiment_randomization1, "Experimental group" = "Incentivized", "Control group" = "Control")


study3$passed <- "Disengaged Respondent"
study3$passed[study3$Q12 == "Iowa"] <-"Engaged Respondent"

study3$supportactions <- recode(study3$Q13, "Strongly support" = 5, "Support"=4, "Neither support nor oppose"=3, "Oppose"=2,"Strongly oppose" = 1)

study3$justified <-recode(study3$Q14, "Justified" = 1, "Unjustified" = 0)
study3$charged <-recode(study3$Q15, "Yes" = 1, "No" = 0)


study3$alignment <- NA
study3$alignment[study3$experiment_randomization2 == "Republican/Republican focus" & study3$pid3=="Republican"] <- " Out-Party Shooter"
study3$alignment[study3$experiment_randomization2 == "Republican/Republican focus" & study3$pid3=="Democrat"] <- " In-Party Shooter"
study3$alignment[study3$experiment_randomization2 == "Democrat/Democratic focus" & study3$pid3=="Democrat"] <- " Out-Party Shooter"
study3$alignment[study3$experiment_randomization2 == "Democrat/Democratic focus" & study3$pid3=="Republican"] <- " In-Party Shooter"

study3$alignment <- as.factor(study3$alignment)
