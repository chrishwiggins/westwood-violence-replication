data2 <- read_csv("../data/study25.csv")

table(data2$gc)
data2 <- data2 %>%
	filter(gc==1)

#recode leaners
data2$Q10[data2$Q11 == "Democratic Party"] <- "Democrat"
data2$Q10[data2$Q11 == "Republican Party"] <- "Republican"
data2$pid <- data2$Q10
data2$pid <- as.factor(data2$pid)

# covariates
data2$gender <- as.factor(data2$Q4)
data2$income <- as.factor(data2$Q7)
data2$education <- as.factor(data2$Q8)
data2$age <- data2$Q14
data2$race <- data2$Q5

# strong partisans
data2$Q12<-recode(data2$Q12, "Strong Republican" = 1, "Not a strong Republican" = 0)
data2$Q13<-recode(data2$Q13, "Strong Democrat" = 1, "Not a strong Democrat" = 0)

data2$strongpartisan <- 0
data2$strongpartisan[data2$pid=="Republican"] <- data2$Q12[data2$pid=="Republican"]
data2$strongpartisan[data2$pid=="Democrat"] <- data2$Q13[data2$pid=="Democrat"]

#recode experiments and conditions

data2$experiment <- recode(data2$experiment, "1" = "Vignette (Rep)", "2" = "Expressiveness")

#study 1 
data2$cell <- NA
data2$cell[data2$version == 1] <- "Democrat\nShooter"
data2$cell[data2$version == 2] <- "Republican\nShooter"
data2$cell[data2$version == 3] <- "Apolitical\nShooter"

#study 2 
data2$study5cell <- NA
data2$study5cell[data2$payprompt == 1] <- "No Incentive"
data2$study5cell[data2$payprompt == 2] <- "Incentive"

# create controls

#affpol
data2$affectivepolarization <- NA
data2$inparty <- NA
data2$outparty <- NA

data2$inparty[which(data2$pid=="Democrat")] <- data2$Q30_2[which(data2$pid=="Democrat")]
data2$inparty[which(data2$pid=="Republican")] <- data2$Q31_2[which(data2$pid=="Republican")]

data2$outparty[which(data2$pid=="Republican")] <- data2$Q30_2[which(data2$pid=="Republican")]
data2$outparty[which(data2$pid=="Democrat")] <- data2$Q31_2[which(data2$pid=="Democrat")]

data2$affectivepolarization <- data2$inparty -data2$outparty

data2$affectivepolarization <- quantcut(data2$affectivepolarization, q=3, labels = c("Low", "Medium", "High"))
as.character(data2$Q20)
# Marlow-Crowne
data2$Q20<-recode(as.character(data2$Q20), "TRUE" = 1, "FALSE" = 0)
data2$Q21<-recode(as.character(data2$Q21), "TRUE" = 1, "FALSE" = 0)
data2$Q22<-recode(as.character(data2$Q22), "TRUE" = 1, "FALSE" = 0)
data2$Q23<-recode(as.character(data2$Q23), "TRUE" = 1, "FALSE" = 0)
data2$Q24<-recode(as.character(data2$Q24), "TRUE" = 1, "FALSE" = 0)
data2$Q25<-recode(as.character(data2$Q25), "TRUE" = 1, "FALSE" = 0)
data2$Q26<-recode(as.character(data2$Q26), "TRUE" = 1, "FALSE" = 0)
data2$Q27<-recode(as.character(data2$Q27), "TRUE" = 1, "FALSE" = 0)
data2$Q28<-recode(as.character(data2$Q28), "TRUE" = 1, "FALSE" = 0)
data2$Q29<-recode(as.character(data2$Q29), "TRUE" = 1, "FALSE" = 0)

data2$marlowcrowne <- (data2$Q20 + data2$Q21 + data2$Q22 + data2$Q23 + data2$Q24 + data2$Q25 + data2$Q26 + data2$Q27 + data2$Q28 + data2$Q29)/10

data2$marlowcrowne <- quantcut(data2$marlowcrowne, q=3, labels = c("Low", "Medium", "High"))

# Short-Form Buss-Perry Aggression Questionnaire
data2$Q63<-recode(data2$Q63, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q64<-recode(data2$Q64, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q65<-recode(data2$Q65, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q66<-recode(data2$Q66, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q67<-recode(data2$Q67, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q68<-recode(data2$Q68, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q69<-recode(data2$Q69, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q70<-recode(data2$Q70, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q71<-recode(data2$Q71, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q72<-recode(data2$Q72, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q73<-recode(data2$Q73, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data2$Q75<-recode(data2$Q75, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)

data2$bussperry <- (data2$Q63 + data2$Q64 + data2$Q65 + data2$Q66 + data2$Q67 + data2$Q68 + data2$Q69 + data2$Q70 + data2$Q71 + data2$Q72 + data2$Q73 + data2$Q75)/12

data2$bussperryc <- data2$bussperry
data2$bussperry <- quantcut(data2$bussperry, q=3, labels = c("Low", "Medium", "High"))

# Kalmoe-Mason 

data2$Q77<-recode(data2$Q77, "1 - Not at all" = 1, "2"=2, "3"=3, "4"=4,"5 - A great deal" = 5)

#political engagement index
data2$Q16<-recode(data2$Q16, "Yes" = 1, "No" = 0)
data2$Q17<-recode(data2$Q17, "Yes" = 1, "No" = 0)
data2$Q18<-recode(data2$Q18, "Yes" = 1, "No" = 0)

data2$partscale <- (data2$Q16 + data2$Q17 + data2$Q18)/3

#### Note that in our PAP we intended to bin this variable.  This fails.  Instead we use the raw scale.

#data2$partscale <- quantcut(data$partscale, q=3, labels = c("Low", "Medium", "High"))


###################
# Study 2
###################
study2 <- data2[data2$experiment == "Vignette (Rep)",]

# attention check
study2$passed <- "Disengaged Respondent"
study2$passed[study2$Q43 == "Iowa"] <- "Engaged Respondent" 


table(study2$passed, study2$cell)
table(study2$passed)

# recode DVs
study2$supportactions <- NA
study2$supportactions <- study2$Q44
study2$supportactions <- recode(study2$supportactions, "Strongly support" = 5, "Support"=4, "Neither support nor oppose"=3, "Oppose"=2,"Strongly oppose" = 1)

study2$supportactions_dum <- 0
study2$supportactions_dum[study2$supportactions==4 |study2$supportactions==5] <-1

study2$justified <- NA
study2$justified <- study2$Q45
study2$justified <-recode(study2$justified, "Justified" = 1, "Unjustified" = 0)

study2$charged <- NA
study2$charged <- study2$Q46

study2$charged <-recode(study2$charged, "Yes" = 1, "No" = 0)

study2$alignment <- NA
study2$alignment[study2$version == 1 & study2$pid == "Democrat"] <- "Out-Party Shooter"
study2$alignment[study2$version == 2  & study2$pid == "Democrat"] <- "In-Party Shooter"

study2$alignment[study2$version == 1 & study2$pid == "Republican"] <- "In-Party Shooter"
study2$alignment[study2$version == 2 & study2$pid == "Republican"] <- "Out-Party Shooter"

study2$alignment[study2$version == 3] <- "Apoltical Shooter"

study2$alignment <- as.factor(study2$alignment)


##### Recodes for cor analysis
study2$supportactions01 <- range01(study2$supportactions)

study2$bussperryc01 <- range01(study2$bussperryc)


study2$passed01 <- 0
study2$passed01[study2$passed == "Engaged Respondent"] <- 1

study2$km <- 0
study2$km[study2$Q77 !=1] <- 1