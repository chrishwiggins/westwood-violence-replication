study1$passed01 <- 0
study1$passed01[study1$passed == "Engaged Respondent"] <- 1

study1$race2 <- study1$race
study1$race2[study1$race2 !="White/Caucasian"] <- "Non-White"
study1$race2 <- as.factor(study1$race2)
study1$race2 <- relevel(study1$race2, "White/Caucasian")

study1$educ4 <- NA
study1$educ4[study1$education == "Less than High School"] <- "Less than High School"
study1$educ4[study1$education == "High School / GED"  | study1$education == "Some College"] <- "High School"
study1$educ4[study1$education == "2-year College Degree"  | study1$education == "4-year College Degree"] <- "College"
study1$educ4[study1$education == "Doctoral Degree"  | study1$education == "Masters Degree" | study1$education == "Professional Degree (JD, MD)"] <- "Advanced Degree"
study1$educ4<- as.factor(study1$educ4)
levels(study1$educ4)
study1$educ4 <- relevel(study1$educ4, "Less than High School")
study1$income <- relevel(study1$income, "Less than 30,000")
study1_engagement <- lm("passed01~as.numeric(age) +  gender + race2 + pid + educ4 + income", data=study1)


study2$passed01 <- 0
study2$passed01[study2$passed == "Engaged Respondent"] <- 1

study2$race2 <- study2$race
study2$race2[study2$race2 !="White/Caucasian"] <- "Non-White"
study2$race2 <- as.factor(study2$race2)
study2$race2 <- relevel(study2$race2, "White/Caucasian")

study2$educ4 <- NA
study2$educ4[study2$education == "Less than High School"] <- "Less than High School"
study2$educ4[study2$education == "High School / GED"  | study2$education == "Some College"] <- "High School"
study2$educ4[study2$education == "2-year College Degree"  | study2$education == "4-year College Degree"] <- "College"
study2$educ4[study2$education == "Doctoral Degree"  | study2$education == "Masters Degree" | study2$education == "Professional Degree (JD, MD)"] <- "Advanced Degree"
study2$educ4<- as.factor(study2$educ4)
levels(study2$educ4)
study2$educ4 <- relevel(study2$educ4, "Less than High School")

study2$income <- str_replace_all(study2$income, '\\$', '')
study2$income <- as.factor(study2$income)
study2$income <- relevel(study2$income, "Less than 30,000")
study2_engagement <- lm("passed01~as.numeric(age) +  gender + race2 + pid + educ4 + income", data=study2)


study4$passed01 <- 0
study4$passed01[study4$passed == "Engaged Respondent"] <- 1

study4$race2 <- study4$race
study4$race2[study4$race2 !="White/Caucasian"] <- "Non-White"
study4$race2 <- as.factor(study4$race2)
study4$race2 <- relevel(study4$race2, "White/Caucasian")

study4$educ4 <- NA
study4$educ4[study4$education == "Less than High School"] <- "Less than High School"
study4$educ4[study4$education == "High School / GED"  | study4$education == "Some College"] <- "High School"
study4$educ4[study4$education == "2-year College Degree"  | study4$education == "4-year College Degree"] <- "College"
study4$educ4[study4$education == "Doctoral Degree"  | study4$education == "Masters Degree" | study4$education == "Professional Degree (JD, MD)"] <- "Advanced Degree"
study4$educ4<- as.factor(study4$educ4)

study4$educ4 <- relevel(study4$educ4, "Less than High School")
study4$income <- relevel(study4$income, "Less than 30,000")
study4_engagement <- lm("passed01~as.numeric(age) +  gender + race2 + pid + educ4 + income", data=study4)


modelset <- stargazer(study1_engagement,study2_engagement,study4_engagement,
					  column.labels = c("Study 1", "Study 2", "Study 3"), 
					  dep.var.labels.include = F,
					  covariate.labels = c("Age", "Male", "White", "Republican", "Advanced Degree", "College", "High School", "$100k +", "$30k-39k", "$40k-49k", "$50k-59k", "$60k-69k", "$70k-79k", "$80k-89k", "$90k-99k",  "Intercept"),
					  star.cutoffs = c(.05,.01,.001),
					  label = "appendix:tab:engagement",
					  ci = F,
					  title = "Predicting Passing the Engagement Check Studies 1-3",
					  type="latex"
)