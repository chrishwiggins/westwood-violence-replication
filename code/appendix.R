# Helper functions

tab <- function(...) {
	texreg(stars = numeric(0),
		   digits = 2,
		   include.ci = FALSE,
		   include.rsquared = FALSE,
		   include.adjrs = FALSE,
		   include.rmse = FALSE,
		   table = FALSE,
		   ...)
}

combo <- function(x, y) paste0(x, " * ", rep(y, each = length(x)))

# Summary Statistics ------------------------------------------------------

study_stats <- function(df, study_num) {
  out <- df %>% 
    select(age, gender, race, pid) %>%
    mutate(age = as.integer(age),
           race = factor(race)) %>%
    st(out = "latex", digits = 2) %>% 
    capture.output()
  out <- out[-1]
  out <- out[-(c(-1, 0) + length(out))]
  write(out, paste0("../results/study", study_num, "_stats.tex"))
}

study1 %>% study_stats(1)
study2 %>% study_stats(2)
study4 %>% study_stats(4)
study5 %>% study_stats(5)

out <- study3 %>% 
	select(age4, gender4, race4, pid3) %>%
	st(out = "latex", digits = 2) %>% 
	capture.output()
out <- out[-1]
out <- out[-(c(-1, 0) + length(out))]
write(out, "../results/study3_stats.tex")

# Study 1 --------------------------------------------------------

# Main results (general support)

cell_names <- c("Apolitical Driver 2", "Democrat Driver", "Republican Driver")
passed_names <- c("Engaged Respondent")
pid_names <- c("Republican")

m <- list(
	lm_robust(supportactions ~ cell, data = study1, se_type = "HC1"),
	lm_robust(supportactions ~ cell*passed, data = study1, se_type = "HC1"), # by attentiveness
	lm_robust(justified ~ cell, data = study1, se_type = "HC1"),
	lm_robust(justified ~ cell*passed, data = study1, se_type = "HC1"),
	lm_robust(charged ~ cell, data = study1, se_type = "HC1"),
	lm_robust(charged ~ cell*passed, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_cellpassed.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, passed_names,
		  					  combo(cell_names, passed_names)))


# Main results (general support by party)
m <- list(
	lm_robust(supportactions ~ cell, data = study1, se_type = "HC1"),
	lm_robust(supportactions ~ cell*pid, data = study1, se_type = "HC1"), # by attentiveness
	lm_robust(justified ~ cell, data = study1, se_type = "HC1"),
	lm_robust(justified ~ cell*pid, data = study1, se_type = "HC1"),
	lm_robust(charged ~ cell, data = study1, se_type = "HC1"),
	lm_robust(charged ~ cell*pid, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_cellpid.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, pid_names,
		  					  combo(cell_names, pid_names)))



pid7_names <- c("Strong Dem.", "Weak Dem.", "Lean Dem.",
				"Lean Rep.", "Weak Rep.", "Strong Rep.")

study1 <- study1 %>% 
	mutate(pid7 = case_when(
		Q11 == "Democratic Party" ~ pid7_names[3],
		Q11 == "Republican Party" ~ pid7_names[4],
		Q12 == 1 ~ pid7_names[6], 
		Q12 == 0 ~ pid7_names[5],
		Q13 == 1 ~ pid7_names[1],
		Q13 == 0 ~ pid7_names[2]),
		pid7 = factor(pid7, levels = pid7_names))


m <- list(
	lm_robust(supportactions ~ cell*pid7, data = study1, se_type = "HC1"),
	lm_robust(justified ~ cell*pid7, data = study1, se_type = "HC1"),
	lm_robust(charged ~ cell*pid7, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_cellpid7.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 1),
		  custom.coef.names = c(NA, cell_names, pid7_names[-1],
		  					  combo(cell_names, pid7_names[-1])))


align_names <- c("In-Party Driver", 'Out-Party Driver')

m <- list(
	lm_robust(supportactions ~ alignment, data = study1, se_type = "HC1"),
	lm_robust(justified ~ alignment, data = study1, se_type = "HC1"),
	lm_robust(charged ~ alignment, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_alignment.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names))


# main result, comparing the two out-party treatments

# version = political vs. apolitical
# partisantreatment = story
study1 <- study1 %>% 
	mutate(compare_group = case_when(
		pid == "Democrat" & partisantreatment == 2 ~ "Out-Party Comparison",
		pid == "Republican" & partisantreatment == 1 ~ "Out-Party Comparison",
		pid == "Democrat" & partisantreatment == 1 ~ "In-Party Comparison",
		pid == "Republican" & partisantreatment == 2 ~ "In-Party Comparison"))

study1_out <- study1 %>% filter(compare_group == "Out-Party Comparison") %>%
	mutate(vtreat = if_else(version == 1, "Out-Party Driver", "Apolitical Driver"))
study1_in <- study1 %>% filter(compare_group == "In-Party Comparison") %>% 
	mutate(vtreat = if_else(version == 1, "In-Party Driver", "Apolitical Driver"))

m <- list(
	lm_robust(supportactions ~ vtreat, data = study1_out, se_type = "HC1"),
	lm_robust(supportactions ~ vtreat, data = study1_in, se_type = "HC1"),
	lm_robust(justified ~ vtreat, data = study1_out, se_type = "HC1"),
	lm_robust(justified ~ vtreat, data = study1_in, se_type = "HC1"),
	lm_robust(charged ~ vtreat, data = study1_out, se_type = "HC1"),
	lm_robust(charged ~ vtreat, data = study1_in, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_motives.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, "Out-Party Driver", "In-Party Driver"))




# Prospective violence and social desirability

SD_names <- paste0(c("Medium", "High"), " SD")

lm_robust(Q77 ~ marlowcrowne, data = study1, se_type = "HC1") %>% 
	list("Use Violence" = .) %>% 
	tab(file = "../results/study1_Q77_marlowecrowne.tex",
		custom.coef.names = c(NA, SD_names))

#marlowe-crowne

m <- list()
m[["supportactions"]] <- lm_robust(supportactions ~ alignment * marlowcrowne, data = study1, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * marlowcrowne, data = study1, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * marlowcrowne, data = study1, se_type = "HC1")
m %>% tab(file = "../results/study1_outcomes_marlowecrowne.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, SD_names, combo(align_names, SD_names)))

#buss-perry
agg_names <- c("Medium Aggression", "High Aggresion")

m[["supportactions"]] <- lm_robust(supportactions ~ alignment * bussperry, data = study1, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * bussperry, data = study1, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * bussperry, data = study1, se_type = "HC1")
m %>% tab(file = "../results/study1_outcomes_bussperry.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, agg_names, combo(align_names, agg_names)))

#political interest
interest_names <- c("Pol. Interest")

m[["supportactions"]] <- lm_robust(supportactions ~ alignment * partscale, data = study1, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * partscale, data = study1, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * partscale, data = study1, se_type = "HC1")
m %>% tab(file = "../results/study1_outcomes_partscale.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, interest_names, combo(align_names, interest_names)))


#kalmoe mason
## CN added code re: Q32
study1$affpol.raw<-study1$inparty-study1$outparty
study1$Q36.num<-as.numeric(study1$Q36=="Yes")
study1.attn<-study1 %>% filter(passed == "Engaged Respondent")




km <- c("Moral Threat", "Human", "Evil",
		"Injure Democrats", "Injure Republicans", "Use Violence")

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q32, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q32, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q32, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km1.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[1],
		  					  combo(align_names, km[1])))


m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q33, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q33, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q33, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km2.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[2],
		  					  combo(align_names, km[2])))

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q34, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q34, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q34, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km3.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[3],
		  					  combo(align_names, km[3])))

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q35, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q35, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q35, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km4.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[4],
		  					  combo(align_names, km[4])))

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q36, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q36, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q36, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km5.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[5],
		  					  combo(align_names, km[5])))


m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q77, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q77, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q77, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_km6.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, km[6],
		  					  combo(align_names, km[6])))

#affpol 

affpol_names <- c("Medium AP", "High AP")

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * affectivepolarization, data = study1, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * affectivepolarization, data = study1, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * affectivepolarization, data = study1, se_type = "HC1")
)
m %>% tab(file = "../results/study1_outcomes_affective.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, affpol_names,
		  					  combo(align_names, affpol_names)))




# Study 4 --------------------------------------------------------

study4 <- study4 %>% 
	mutate(pid7 = case_when(
		Q11 == "Democratic Party" ~ pid7_names[3],
		Q11 == "Republican Party" ~ pid7_names[4],
		Q12 == 1 ~ pid7_names[6], 
		Q12 == 0 ~ pid7_names[5],
		Q13 == 1 ~ pid7_names[1],
		Q13 == 0 ~ pid7_names[2]),
		pid7 = factor(pid7, levels = pid7_names))

crimes <- c("Assault", "Assault w/Deadly Weapon", "Murder",
			"Protest w/out Permit", "Vandalism")

pass_names <- c("Engaged Respondent")

# pardon = likert for pardon (Q76)
# nullify = community service on sentence (Q53)

m <- list(
	lm_robust(pardon~item.crime, data=study4, se_type = "HC1"),
	lm_robust(pardon~item.crime*passed, data=study4, se_type = "HC1"),
	lm_robust(nullify~item.crime, data=study4, se_type = "HC1"),
	lm_robust(nullify~item.crime*passed, data=study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_passed.tex",
		  custom.model.names = rep(c("Pardon", "Nullify"), each = 2),
		  custom.coef.names = c(NA, crimes, pass_names,
		  					  combo(crimes, pass_names)))

# by pid
pid_names <- c("Republican")

m <- list(
	lm_robust(pardon~item.crime, data=study4, se_type = "HC1"),
	lm_robust(pardon~item.crime*pid, data=study4, se_type = "HC1"),
	lm_robust(nullify~item.crime, data=study4, se_type = "HC1"),
	lm_robust(nullify~item.crime*pid, data=study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_pid.tex",
		  custom.model.names = rep(c("Pardon", "Nullify"), each = 2),
		  custom.coef.names = c(NA, crimes, pid_names,
		  					  combo(crimes, pid_names)))

m <- list(
	lm_robust(pardon~item.crime*pid7, data=study4, se_type = "HC1"),
	lm_robust(nullify~item.crime*pid7, data=study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_pid7.tex",
		  custom.model.names = rep(c("Pardon", "Nullify"), each = 1),
		  custom.coef.names = c(NA, crimes, pid7_names[-1],
		  					  combo(crimes, pid7_names[-1])))


# robustness

#marlow-crowne
m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * marlowcrowne, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * marlowcrowne, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_marlowecrowne.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, SD_names,
		  					  combo(crimes, SD_names)))

#buss-perr
m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * bussperry, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * bussperry, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_bussperry.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, agg_names,
		  					  combo(crimes, agg_names)))

#political interest

m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * partscale, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * partscale, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_partscale.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, interest_names,
		  					  combo(crimes, interest_names)))

#kalmoe mason

m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q32, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q32, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km1.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[1],
		  					  combo(crimes, km[1])))

m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q33, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q33, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km2.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[2],
		  					  combo(crimes, km[2])))

m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q34, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q34, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km3.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[3],
		  					  combo(crimes, km[3])))

m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q35, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q35, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km4.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[4],
		  					  combo(crimes, km[4])))


m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q36, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q36, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km5.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[5],
		  					  combo(crimes, km[5])))


m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * Q77, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * Q77, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_km6.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, km[6],
		  					  combo(crimes, km[6])))


#affpol
m <- list(
	"pardon" = lm_robust(pardon ~ item.crime * affectivepolarization, data = study4, se_type = "HC1"),
	"nullify" = lm_robust(nullify ~ item.crime * affectivepolarization, data = study4, se_type = "HC1")
)
m %>% tab(file = "../results/study4_outcomes_affective.tex",
		  custom.model.names = c("Pardon", "Nullify"),
		  custom.coef.names = c(NA, crimes, affpol_names,
		  					  combo(crimes, affpol_names)))






# Study 2 -----------------------------------------------------------------

study2 <- study2 %>% 
	mutate(pid7 = case_when(
		Q11 == "Democratic Party" ~ pid7_names[3],
		Q11 == "Republican Party" ~ pid7_names[4],
		Q12 == 1 ~ pid7_names[6], 
		Q12 == 0 ~ pid7_names[5],
		Q13 == 1 ~ pid7_names[1],
		Q13 == 0 ~ pid7_names[2]),
		pid7 = factor(pid7, levels = pid7_names))

cell_names <- c("Democrat Shooter", "Republican Shooter")
passed_names <- c("Engaged Respondent")
pid_names <- c("Republican")

m <- list(
	lm_robust(supportactions ~ cell, data = study2, se_type = "HC1"),
	lm_robust(supportactions ~ cell*passed, data = study2, se_type = "HC1"), # by attentiveness
	lm_robust(justified ~ cell, data = study2, se_type = "HC1"),
	lm_robust(justified ~ cell*passed, data = study2, se_type = "HC1"),
	lm_robust(charged ~ cell, data = study2, se_type = "HC1"),
	lm_robust(charged ~ cell*passed, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_cellpassed.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, passed_names,
		  					  combo(cell_names, passed_names)))

# Main results (general support by party)
m <- list(
	lm_robust(supportactions ~ cell, data = study2, se_type = "HC1"),
	lm_robust(supportactions ~ cell*pid, data = study2, se_type = "HC1"), # by attentiveness
	lm_robust(justified ~ cell, data = study2, se_type = "HC1"),
	lm_robust(justified ~ cell*pid, data = study2, se_type = "HC1"),
	lm_robust(charged ~ cell, data = study2, se_type = "HC1"),
	lm_robust(charged ~ cell*pid, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_cellpid.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, pid_names,
		  					  combo(cell_names, pid_names)))

m <- list(
	lm_robust(supportactions ~ cell*pid7, data = study2, se_type = "HC1"),
	lm_robust(justified ~ cell*pid7, data = study2, se_type = "HC1"),
	lm_robust(charged ~ cell*pid7, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_pid7.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 1),
		  custom.coef.names = c(NA, cell_names, pid7_names[-1],
		  					  combo(cell_names, pid7_names[-1])))



# Main results by in- and out-party

align_names <- c("In-Party and Partisan", "Out-Party and Partisan")

study2 <- study2 %>% 
	mutate(alignment = case_when(
		version == 1 & pid == "Democrat" ~ align_names[1], 
		version == 2  & pid == "Democrat" ~ align_names[2], 
		version == 1 & pid == "Republican" ~ align_names[2], 
		version == 2 & pid == "Republican" ~ align_names[1], 
		version == 3 ~ "Non-Partisan"),
		alignment = fct_relevel(alignment, "Non-Partisan", align_names[1]))

m <- list(
	lm_robust(supportactions ~ alignment, data = study2, se_type = "HC1"),
	lm_robust(justified ~ alignment, data = study2, se_type = "HC1"),
	lm_robust(charged ~ alignment, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_alignment.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names))

# robustness

# Prospective violence and social desirability

lm_robust(Q77 ~ marlowcrowne, data = study2, se_type = "HC1") %>% 
	list("Use Violence" = .) %>% 
	tab(file = "../results/study2_Q77_marlowecrowne.tex",
		custom.coef.names = c(NA, SD_names))

#marlowe-crowne

m <- list()
m[["supportactions"]] <- lm_robust(supportactions ~ alignment * marlowcrowne, data = study2, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * marlowcrowne, data = study2, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * marlowcrowne, data = study2, se_type = "HC1")
m %>% tab(file = "../results/study2_outcomes_marlowecrowne.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, SD_names, combo(align_names, SD_names)))

#buss-perry
agg_names <- c("Medium Aggression", "High Aggresion")

m[["supportactions"]] <- lm_robust(supportactions ~ alignment * bussperry, data = study2, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * bussperry, data = study2, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * bussperry, data = study2, se_type = "HC1")
m %>% tab(file = "../results/study2_outcomes_bussperry.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, agg_names, combo(align_names, agg_names)))

#political interest
interest_names <- c("Pol. Interest")

m[["supportactions"]] <- lm_robust(supportactions ~ alignment * partscale, data = study2, se_type = "HC1")
m[["justified"]] <- lm_robust(justified ~ alignment * partscale, data = study2, se_type = "HC1")
m[["charged"]] <- lm_robust(charged ~ alignment * partscale, data = study2, se_type = "HC1")
m %>% tab(file = "../results/study2_outcomes_partscale.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, interest_names, combo(align_names, interest_names)))


# There is only one Kalmoe-Mason question in this study, Q77
m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * Q77, data = study2, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * Q77, data = study2, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * Q77, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_km6.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, "Use Violence",
		  					  combo(align_names, "Use Violence")))

#affpol 

affpol_names <- c("Medium AP", "High AP")

m <- list(
	"supportactions" = lm_robust(supportactions ~ alignment * affectivepolarization, data = study2, se_type = "HC1"),
	"justified" = lm_robust(justified ~ alignment * affectivepolarization, data = study2, se_type = "HC1"),
	"charged" = lm_robust(charged ~ alignment * affectivepolarization, data = study2, se_type = "HC1")
)
m %>% tab(file = "../results/study2_outcomes_affective.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names, affpol_names,
		  					  combo(align_names, affpol_names)))









# Study 5 -----------------------------------------------------------------

# {rep,dem}distance = abs(estimated_support - true_support) for violence
# {rep,dem}distance = abs(estimated_support - true_support) for violence

study5 <- study5 %>% 
	mutate(pid7 = case_when(
		Q11 == "Democratic Party" ~ pid7_names[3],
		Q11 == "Republican Party" ~ pid7_names[4],
		Q12 == 1 ~ pid7_names[6], 
		Q12 == 0 ~ pid7_names[5],
		Q13 == 1 ~ pid7_names[1],
		Q13 == 0 ~ pid7_names[2]),
		pid7 = factor(pid7, levels = pid7_names))

outcomes <- c("Rep. Dist.", "Dem. Dist.",
			  "Rep. Sup.", "Dem. Sup.",
			  "In-Party Sup.", "Out-Party Sup.")

cells <- c("Incentivized")
pass_names <- c("Engaged Respondent")

study5 <- study5 %>% 
	mutate(study5cell = fct_relevel(factor(study5cell), "No Incentive"))

# main results
m <- list(
	lm_robust(repdistance~study5cell, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_cell.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells))


# by attentiveness
# main results
m <- list(
	lm_robust(repdistance~study5cell*passed, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell*passed, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell*passed, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell*passed, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell*passed, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell*passed, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_passed.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, pass_names, combo(cells, pass_names)))


# by pid
# main results
m <- list(
	lm_robust(repdistance~study5cell*pid, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell*pid, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell*pid, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell*pid, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_pid.tex",
		  custom.model.names = outcomes[1:4],
		  custom.coef.names = c(NA, cells, pid_names, combo(cells, pid_names)))


m <- list(
	lm_robust(repdistance~study5cell*pid7, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell*pid7, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell*pid7, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell*pid7, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_pid7.tex",
		  custom.model.names = outcomes[1:4],
		  custom.coef.names = c(NA, cells, pid7_names[-1], combo(cells, pid7_names[-1])))







#marlow-crowne
m <- list(
	lm_robust(repdistance~study5cell * marlowcrowne, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell* marlowcrowne, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell* marlowcrowne, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell* marlowcrowne, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell* marlowcrowne, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell* marlowcrowne, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_marlowecrowne.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, SD_names, combo(cells, SD_names)))


#buss-perry
m <- list(
	lm_robust(repdistance~study5cell* bussperry, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell* bussperry, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell* bussperry, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell* bussperry, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell* bussperry, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell* bussperry, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_bussperry.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, agg_names, combo(cells, agg_names)))


#political interest
m <- list(
	lm_robust(repdistance~study5cell* partscale, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell* partscale, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell* partscale, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell* partscale, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell* partscale, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell* partscale, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_partscale.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, interest_names, combo(cells, interest_names)))


#kalmoe mason

m <- list(
	lm_robust(repdistance~study5cell * Q77, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell * Q77, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell * Q77, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell * Q77, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell * Q77, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell * Q77, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_km6.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, "Use Violence", combo(cells, "Use Violence")))

#affpol
m <- list(
	lm_robust(repdistance~study5cell* affectivepolarization, data=study5, se_type = "HC1"),
	lm_robust(demdistance~study5cell* affectivepolarization, data=study5, se_type = "HC1"),
	lm_robust(repsupport~study5cell* affectivepolarization, data=study5, se_type = "HC1"),
	lm_robust(demsupport~study5cell* affectivepolarization, data=study5, se_type = "HC1"),
	lm_robust(inpartysupport~study5cell* affectivepolarization, data=study5, se_type = "HC1"),
	lm_robust(outpartysupport~study5cell* affectivepolarization, data=study5, se_type = "HC1")
)
m %>% tab(file = "../results/study5_outcomes_affective.tex",
		  custom.model.names = outcomes,
		  custom.coef.names = c(NA, cells, affpol_names, combo(cells, affpol_names)))




# Study 3 -----------------------------------------------------------------


# Main results (general support)

cell_names <- c("Republican Shooter")
passed_names <- c("Engaged Respondent")
pid_names <- c("Republican")
align_names <- c("Out-Party Shooter")

study3 <- study3 %>% 
	mutate(cell = experiment_randomization2)

m <- list(
	lm_robust(supportactions ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(supportactions ~ cell*passed, data = study3, se_type = "HC1", weights = weight), # by attentiveness
	lm_robust(justified ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(justified ~ cell*passed, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ cell*passed, data = study3, se_type = "HC1", weights = weight)
)
m %>% tab(file = "../results/study3_outcomes_cellpassed.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, passed_names,
		  					  combo(cell_names, passed_names)))


# Main results (general support by party)
m <- list(
	lm_robust(supportactions ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(supportactions ~ cell*pid3, data = study3, se_type = "HC1", weights = weight), # by attentiveness
	lm_robust(justified ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(justified ~ cell*pid3, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ cell, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ cell*pid3, data = study3, se_type = "HC1", weights = weight)
)
m %>% tab(file = "../results/study3_outcomes_cellpid.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 2),
		  custom.coef.names = c(NA, cell_names, pid_names,
		  					  combo(cell_names, pid_names)))



pid7_names <- c("Strong Dem.", "Weak Dem.", "Weak Rep.", "Strong Rep.")
study3 <- study3 %>%
	mutate(pid7 = case_when(
		pid7 == 'Not very strong Democrat' ~ pid7_names[2],
		pid7 == 'Not very strong Republican' ~ pid7_names[3],
		pid7 == 'Strong Democrat' ~ pid7_names[1],
		pid7 == 'Strong Republican' ~ pid7_names[4]
		),
		pid7 = factor(pid7, levels = pid7_names))

m <- list(
	lm_robust(supportactions ~ cell*pid7, data = study3, se_type = "HC1", weights = weight),
	lm_robust(justified ~ cell*pid7, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ cell*pid7, data = study3, se_type = "HC1", weights = weight)
)
m %>% tab(file = "../results/study3_outcomes_cellpid7.tex",
		  custom.model.names = rep(c("Support", "Justifed", "Charged"), each = 1),
		  custom.coef.names = c(NA, cell_names, pid7_names[-1],
		  					  combo(cell_names, pid7_names[-1])))

m <- list(
	lm_robust(supportactions ~ alignment, data = study3, se_type = "HC1", weights = weight),
	lm_robust(justified ~ alignment, data = study3, se_type = "HC1", weights = weight),
	lm_robust(charged ~ alignment, data = study3, se_type = "HC1", weights = weight)
)
m %>% tab(file = "../results/study3_outcomes_alignment.tex",
		  custom.model.names = c("Support", "Justifed", "Charged"),
		  custom.coef.names = c(NA, align_names))
