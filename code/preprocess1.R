
data <- read_csv("../data/study14.csv")

table(data$gc)
data <- data %>%
	filter(gc==1)

##########################
# Study 1
##########################

#recode leaners
data$Q10[data$Q11 == "Democratic Party"] <- "Democrat"
data$Q10[data$Q11 == "Republican Party"] <- "Republican"
data$pid <- data$Q10
data$pid <- as.factor(data$pid)

# covariates
data$gender <- as.factor(data$Q4)
data$income <- as.factor(data$Q7)
data$education <- as.factor(data$Q8)
data$age <- data$Q14
data$race <- data$Q5

# strong partisans
data$Q12<-recode(data$Q12, "Strong Republican" = 1, "Not a strong Republican" = 0)
data$Q13<-recode(data$Q13, "Strong Democrat" = 1, "Not a strong Democrat" = 0)

data$strongpartisan <- 0
data$strongpartisan[data$pid=="Republican"] <- data$Q12[data$pid=="Republican"]
data$strongpartisan[data$pid=="Democrat"] <- data$Q13[data$pid=="Democrat"]

#recode experiments and conditions

data$experiment <- recode(data$experiment, "1" = "Vignette", "2" = "Sentencing")

#study 1 
data$cell <- NA
data$cell[data$version == 1 & data$partisantreatment == 1] <- "Republican Driver\n(Story 1)"
data$cell[data$version == 2 & data$partisantreatment == 1] <- "Apolitical Driver\n(Story 1)"
data$cell[data$version == 1 & data$partisantreatment == 2] <- "Democrat Driver\n(Story 2)"
data$cell[data$version == 2 & data$partisantreatment == 2] <- "Apolitical Driver\n(Story 2)"

# create controls

#affpol
data$affectivepolarization <- NA
data$inparty <- NA
data$outparty <- NA

data$inparty[which(data$pid=="Democrat")] <- data$Q30_2[which(data$pid=="Democrat")]
data$inparty[which(data$pid=="Republican")] <- data$Q31_2[which(data$pid=="Republican")]

data$outparty[which(data$pid=="Republican")] <- data$Q30_2[which(data$pid=="Republican")]
data$outparty[which(data$pid=="Democrat")] <- data$Q31_2[which(data$pid=="Democrat")]

data$affectivepolarization <- data$inparty -data$outparty

data$affectivepolarization <- quantcut(data$affectivepolarization, q=3, labels = c("Low", "Medium", "High"))
as.character(data$Q20)
# Marlow-Crowne
data$Q20<-recode(as.character(data$Q20), "TRUE" = 1, "FALSE" = 0)
data$Q21<-recode(as.character(data$Q21), "TRUE" = 1, "FALSE" = 0)
data$Q22<-recode(as.character(data$Q22), "TRUE" = 1, "FALSE" = 0)
data$Q23<-recode(as.character(data$Q23), "TRUE" = 1, "FALSE" = 0)
data$Q24<-recode(as.character(data$Q24), "TRUE" = 1, "FALSE" = 0)
data$Q25<-recode(as.character(data$Q25), "TRUE" = 1, "FALSE" = 0)
data$Q26<-recode(as.character(data$Q26), "TRUE" = 1, "FALSE" = 0)
data$Q27<-recode(as.character(data$Q27), "TRUE" = 1, "FALSE" = 0)
data$Q28<-recode(as.character(data$Q28), "TRUE" = 1, "FALSE" = 0)
data$Q29<-recode(as.character(data$Q29), "TRUE" = 1, "FALSE" = 0)

data$marlowcrowne <- (data$Q20 + data$Q21 + data$Q22 + data$Q23 + data$Q24 + data$Q25 + data$Q26 + data$Q27 + data$Q28 + data$Q29)/10

data$marlowcrowne <- quantcut(data$marlowcrowne, q=3, labels = c("Low", "Medium", "High"))

# Short-Form Buss-Perry Aggression Questionnaire
data$Q63<-recode(data$Q63, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q64<-recode(data$Q64, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q65<-recode(data$Q65, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q66<-recode(data$Q66, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q67<-recode(data$Q67, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q68<-recode(data$Q68, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q69<-recode(data$Q69, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q70<-recode(data$Q70, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q71<-recode(data$Q71, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q72<-recode(data$Q72, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q73<-recode(data$Q73, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)
data$Q75<-recode(data$Q75, "1- Very unlike me" = 1, "2"=2, "3"=3, "4"=4,"5- Very like me" = 5)

data$bussperry <- (data$Q63 + data$Q64 + data$Q65 + data$Q66 + data$Q67 + data$Q68 + data$Q69 + data$Q70 + data$Q71 + data$Q72 + data$Q73 + data$Q75)/12

data$bussperry <- quantcut(data$bussperry, q=3, labels = c("Low", "Medium", "High"))

# Kalmoe-Mason 
data$Q32<-recode(data$Q32, "Strongly agree" = 5, "Somewhat agree"=4, "Neither agree nor disagree"=3, "Somewhat disagree"=2,"Strongly disagree" = 1)
data$Q33<-recode(data$Q33, "Strongly agree" = 5, "Somewhat agree"=4, "Neither agree nor disagree"=3, "Somewhat disagree"=2,"Strongly disagree" = 1)
data$Q34<-recode(data$Q34, "Strongly agree" = 5, "Somewhat agree"=4, "Neither agree nor disagree"=3, "Somewhat disagree"=2,"Strongly disagree" = 1)

data$Q35<-recode(data$Q35, "Yes" = 1, "No" = 0)
data$Q35<-recode(data$Q36, "Yes" = 1, "No" = 0)

#
data$Q77<-recode(data$Q77, "1 - Not at all" = 1, "2"=2, "3"=3, "4"=4,"5 - A great deal" = 5)
names(data)
#political engagement index
data$Q16<-recode(data$Q16, "Yes" = 1, "No" = 0)
data$Q17<-recode(data$Q17, "Yes" = 1, "No" = 0)
data$Q18<-recode(data$Q18, "Yes" = 1, "No" = 0)

data$partscale <- (data$Q16 + data$Q17 + data$Q18)/3

#### Note that in our PAP we intended to bin this variable.  This fails.  Instead we use the raw scale.

#data$partscale <- quantcut(data$partscale, q=3, labels = c("Low", "Medium", "High"))


###################
# Study 1
###################
study1 <- data[data$experiment == "Vignette",]

# Experiment 1: Florida Democrat attacks voter registration drive
# Experiment 2: Oregon Republicans attack Democratic protesters

# attention check
study1$passed <- "Disengaged Respondent"
study1$passed[study1$Q43 == "Florida" & study1$partisantreatment==1] <- "Engaged Respondent"
study1$passed[study1$Q49 == "Oregon" & study1$partisantreatment==2] <- "Engaged Respondent"

table(study1$passed, study1$partisantreatment)
table(study1$passed)

# recode DVs
study1$supportactions <- NA
study1$supportactions[study1$partisantreatment==1] <- study1$Q44[study1$partisantreatment==1]
study1$supportactions[study1$partisantreatment==2] <- study1$Q50[study1$partisantreatment==2]
study1$supportactions <- recode(study1$supportactions, "Strongly support" = 5, "Support"=4, "Neither support nor oppose"=3, "Oppose"=2,"Strongly oppose" = 1)

study1$justified <- NA
study1$justified[study1$partisantreatment==1] <- study1$Q45[study1$partisantreatment==1]
study1$justified[study1$partisantreatment==2] <- study1$Q51[study1$partisantreatment==2]
study1$justified <-recode(study1$justified, "Justified" = 1, "Unjustified" = 0)

study1$charged <- NA
study1$charged[study1$partisantreatment==1] <- study1$Q46[study1$partisantreatment==1]
study1$charged[study1$partisantreatment==2] <- study1$Q52[study1$partisantreatment==2]
study1$charged <-recode(study1$charged, "Yes" = 1, "No" = 0)

study1$alignment <- NA
study1$alignment[study1$version == 1 & study1$partisantreatment == 2 & study1$pid == "Democrat"] <- "Out-Party Driver"
study1$alignment[study1$version == 2 & study1$partisantreatment == 2 & study1$pid == "Democrat"] <- "Apolitical Driver\n(Story 2)"
study1$alignment[study1$version == 1 & study1$partisantreatment == 1 & study1$pid == "Democrat"] <- "In-Party Driver"
study1$alignment[study1$version == 2 & study1$partisantreatment == 1 & study1$pid == "Democrat"] <- "Apolitical Driver\n(Story 1)"

study1$alignment[study1$version == 1 & study1$partisantreatment == 2 & study1$pid == "Republican"] <- "In-Party Driver"
study1$alignment[study1$version == 2 & study1$partisantreatment == 2 & study1$pid == "Republican"] <- "Apolitical Driver\n(Story 1)"
study1$alignment[study1$version == 1 & study1$partisantreatment == 1 & study1$pid == "Republican"] <- "Out-Party Driver"
study1$alignment[study1$version == 2 & study1$partisantreatment == 1 & study1$pid == "Republican"] <- "Apolitical Driver\n(Story 2)"

study1$alignment <- as.factor(study1$alignment)


study1$alignment<- as.factor(ifelse(study1$alignment=='Apolitical Driver\n(Story 1)'| study1$alignment=='Apolitical Driver\n(Story 2)', 'Apolitical Driver', as.character(study1$alignment)))

