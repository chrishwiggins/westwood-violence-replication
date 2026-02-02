#######################
# Table 1
#######################

study1$violent<- ifelse(study1$Q77>1, 1, 0)
study2$violent<- ifelse(study2$Q77> 1,1,  0)
study3$violent <- 0
study3$violent[study3$Q8 !="Not at all"] <- 1

a<- study1 %>% group_by(passed) %>% summarise(mean_violence = mean(violent, na.rm=T), count = n())
b<- study2 %>% group_by(passed) %>% summarise(mean_violence = mean(violent, na.rm=T), count = n())
c<- study3 %>% group_by(passed) %>% summarise(mean_violence = weighted.mean(violent, w=weight, na.rm=T), count = n())


mean(study1$violent, na.rm=T)
mean(study2$violent, na.rm=T)
weighted.mean(study3$violent, w=study3$weight)

#GDAtools::wtable(study3$passed,study3$violent,  w = study3$weight)

study3$passed01 <- 0
study3$passed01[study3$passed == "Engaged Respondent"] <- 1

1-weighted.mean(study3$passed01, w=study3$weight)
