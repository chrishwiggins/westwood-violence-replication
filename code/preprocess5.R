##########################
# Study 5
##########################

study5 <- data2[data2$experiment=="Expressiveness",]

# check for attentiveness
study5$passed <- "Disengaged Respondent"
study5$passed[study5$Q86 == "Oregon"] <- "Engaged Respondent "

study5$study5cell[study5$payprompt == 1] <- "No Incentive"
study5$study5cell[study5$payprompt == 2] <- "Incentive"

study5$repsupport <- study5$Q93_2
study5$demsupport <- study5$Q90_2
names(study5)
study5$inpartysupport <- NA
study5$outpartysupport <- NA

true_dem <- 9#X
true_rep <- 9#Y


study5$inpartysupport[study5$pid == "Democrat"] <- study5$demsupport[study5$pid == "Democrat"]
study5$outpartysupport[study5$pid == "Democrat"] <- study5$repsupport[study5$pid == "Democrat"]

study5$inpartysupport[study5$pid == "Republican"] <- study5$repsupport[study5$pid == "Republican"] 
study5$outpartysupport[study5$pid == "Republican"] <- study5$demsupport[study5$pid == "Republican"] 


#  Nine percent of Republicans and Democrats say that, in general, violence is at least occasionally acceptable. CCES

#compute distance
study5$repdistance <- abs(study5$repsupport - true_rep)
study5$demdistance <- abs(study5$demsupport - true_dem)



