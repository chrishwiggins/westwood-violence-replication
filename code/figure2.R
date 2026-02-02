#######################
# Figure 2
#######################

##### justified

plot1ajustifieddata <- study1 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(justified, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plot1bjustifieddata <- study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(justified, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plot1cjustifieddata <- study3 %>%
	filter(!is.na(alignment)) %>%
	group_by(alignment, passed) %>%
	summarise(smean = weighted.mean(justified, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(justified, weights=weight)[1],
			  upper = weighted.ttest.ci(justified, weights=weight)[2])


plot1ajustifieddata$passed <- as.factor(plot1ajustifieddata$passed)

plot1justifiedmerged <- bind_rows(plot1ajustifieddata, plot1bjustifieddata, plot1cjustifieddata)

levels(plot1justifiedmerged$alignment)
plot1justifiedmerged$alignment <- fct_relevel(plot1justifiedmerged$alignment,  " In-Party Shooter"," Out-Party Shooter","In-Party Shooter","Out-Party Shooter","Apoltical Shooter", "In-Party Driver", "Out-Party Driver","Apolitical Driver")

plot1justifiedmerged$alignment

plot1bjustified <- ggplot(plot1justifiedmerged, aes(x = smean, y = alignment, color=passed) ) +
	ggtitle("Suspect is Justified") +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), color = rep(c("#D55E00","#0072B2"),8), position = position_dodge(width=.51),show.legend=FALSE) +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 5.5, alpha = .2,fill = "darkgray") +
	geom_point(size = 9, shape=rep(21:22,8), fill ="white", position = position_dodge(width=.51), color = rep(c("#D55E00","#0072B2"),8)) + theme_bw() + 
	geom_text(size=3.5,	color=rep(c("#D55E00","#0072B2"),8),
			  aes(y = as.numeric(alignment), 
			  	x = smean, 
			  	label = gsub("0\\.", "\\.",sprintf("%.2f", round(smean, 2)))),position = position_dodge(width=.51)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="none") +
	guides(color=guide_legend("Comprehension Check", override.aes = list(shape=c(19,20), size=4))) + 
	xlab("Proportion Saying Suspect is Justified (95% CI)") +
	xlim(-.01,1) + 
	ylab("") +
	geom_hline(yintercept=c(1.5,2.5, 3.5, 4.5, 5.5, 6.5, 7.5), linetype="dotted") + 
	geom_hline(yintercept=c(2.5, 5.5), linetype="solid", size=2) +
	annotate("text", x = .9, y=8, label = "Study 1\n(Qualtrics)",  size=4) +
	annotate("text", x = .9, y=5, label = "Study 2\n(Qualtrics)",  size=4) +
	annotate("text", x = .9, y=2, label = "Study 3\n(YouGov)", size=4) +
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) 

plot1bjustified

###### support

plot1asupportdata <- study1 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(supportactions, na.rm = TRUE),
			  ssd = sd(supportactions, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plot1asupportdata$passed <- as.factor(plot1asupportdata$passed)

plot1bsupportdata <- study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(supportactions, na.rm = TRUE),
			  ssd = sd(supportactions, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))


plot1csupportdata <- study3 %>%
	filter(!is.na(alignment)) %>%
	group_by(alignment, passed) %>%
	summarise(smean = weighted.mean(supportactions, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(supportactions, weights=weight)[1],
			  upper = weighted.ttest.ci(supportactions, weights=weight)[2])

plot1supportmerged <- bind_rows(plot1asupportdata, plot1bsupportdata, plot1csupportdata)

plot1supportmerged$alignment <- as.factor(plot1supportmerged$alignment)

plot1supportmerged$alignment <- fct_relevel(plot1justifiedmerged$alignment,  " In-Party Shooter"," Out-Party Shooter","In-Party Shooter","Out-Party Shooter","Apoltical Shooter", "In-Party Driver", "Out-Party Driver","Apolitical Driver")

plot1bsupport <- ggplot(plot1supportmerged, aes(x = smean, y = alignment, color=passed) ) +
	ggtitle("Support for Suspect") +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), color = rep(c("#D55E00","#0072B2"),8), position = position_dodge(width=.51),show.legend=FALSE) +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 5.5, alpha = .2,fill = "darkgray") +
	geom_point(size = 9, shape=rep(21:22,8), fill ="white", position = position_dodge(width=.51), color = rep(c("#D55E00","#0072B2"),8)) + theme_bw() + 
	geom_text(size=3,	color=rep(c("#D55E00","#0072B2"),8),
			  aes(y = as.numeric(alignment), 
			  	x = smean, 
			  	label = gsub("0\\.", "\\.",sprintf("%.2f", round(smean, 2)))),position = position_dodge(width=.51)) +
	theme(legend.position="none") +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	guides(color=guide_legend("Comprehension Check", override.aes = list(shape=19, size=4))) + 
	xlab("Mean Support for Suspect's Actions (95% CI)") +
	xlim(1,5) + 
	ylab("") +
	geom_hline(yintercept=c(1.5,2.5, 3.5, 4.5, 5.5, 6.5, 7.5), linetype="dotted") + 
	geom_hline(yintercept=c(2.5, 5.5), linetype="solid", size=2) +
	annotate("text", x = 4.7, y=8, label = "Study 1\n(Qualtrics)", size=4) +
	annotate("text", x = 4.7, y=5, label = "Study 2\n(Qualtrics)",  size=4) +
	annotate("text", x = 4.7, y=2, label = "Study 3\n(YouGov)",  size=4) +
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) 
plot1bsupport

###### charged

plot1achargeddata <- study1 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(charged, na.rm = TRUE),
			  ssd = sd(charged, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plot1achargeddata$passed <- as.factor(plot1achargeddata$passed)

plot1bchargeddata <- study2 %>%
	group_by(alignment, passed) %>%
	summarise(smean = mean(charged, na.rm = TRUE),
			  ssd = sd(charged, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plot1cchargeddata <- study3 %>%
	filter(!is.na(alignment)) %>%
	group_by(alignment, passed) %>%
	summarise(smean = weighted.mean(charged, w=weight, na.rm = TRUE),
			  ssd = sd(justified, na.rm = TRUE),
			  count = n(),
			  se=0,
			  lower = weighted.ttest.ci(charged, weights=weight)[1],
			  upper = weighted.ttest.ci(charged, weights=weight)[2])

plot1chargedmerged <- bind_rows(plot1achargeddata, plot1bchargeddata,plot1cchargeddata)

plot1chargedmerged$alignment <- as.factor(plot1chargedmerged$alignment)

plot1chargedmerged$alignment <- fct_relevel(plot1justifiedmerged$alignment,  " In-Party Shooter"," Out-Party Shooter","In-Party Shooter","Out-Party Shooter","Apoltical Shooter", "In-Party Driver", "Out-Party Driver","Apolitical Driver")

plot1bcharged <- ggplot(plot1chargedmerged, aes(x = smean, y = alignment, color=passed) ) +
	ggtitle("Suspect Should be Charged") +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), color = rep(c("#D55E00","#0072B2"),8), position = position_dodge(width=.51),show.legend=FALSE) +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 5.5, alpha = .2,fill = "darkgray") +
	geom_point(size = 9, shape=rep(21:22,8), fill ="white", position = position_dodge(width=.51), color = rep(c("#D55E00","#0072B2"),8)) + theme_bw() + 
	geom_text(size=3.5,	color=rep(c("#D55E00","#0072B2"),8),
			  aes(y = as.numeric(alignment), 
			  	x = smean, 
			  	label = gsub("0\\.", "\\.",sprintf("%.2f", round(smean, 2)))),position = position_dodge(width=.51)) +
	theme(legend.position="none") +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	guides(color=guide_legend("Comprehension Check", override.aes = list(shape=19, size=4))) + 
	xlab("Proportion Supporting Charging the Suspect (95% CI)") +
	xlim(0,1.01) + 
	ylab("") +
	geom_hline(yintercept=c(1.5,2.5, 3.5, 4.5, 5.5, 6.5, 7.5), linetype="dotted") + 
	geom_hline(yintercept=c(2.5, 5.5), linetype="solid", size=2) +
	annotate("text", x = 0.1, y=8, label = "Study 1\n(Qualtrics)",  size=4) +
	annotate("text", x = 0.1, y=5, label = "Study 2\n(Qualtrics)", size=4) +
	annotate("text", x = 0.1, y=2, label = "Study 3\n(YouGov)", size=4) +
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) 
plot1bcharged

######### legend

plot1ajustifieddata2 <- plot1ajustifieddata

plot1ajustifieddata2$passed <- fct_relevel(plot1ajustifieddata2$passed,  "Engaged Respondent", "Disengaged Respondent")

plot1legend <- ggplot(plot1ajustifieddata2, aes(x = smean, y = alignment, color=passed) ) +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), position = position_dodge(width=.51),show.legend=FALSE) +
	geom_point(size = 9, shape=21, fill ="white",position = position_dodge(width=.51)) + theme_bw() + 
	guides(color=guide_legend("", override.aes = list(shape=c(15,19),size=4, color=c("#0072B2","#D55E00")))) + 
	theme(legend.position="bottom") 

########## final plots

legend <- cowplot::get_legend(plot1legend)

title <- ggdraw() + draw_label("Support for Violence Among Engaged \nand Disengaged Respondents", fontface='bold')             



plot1top <- plot_grid(plot1bjustified, plot1bsupport, plot1bcharged, label_size = 12, ncol=1,rel_heights=c(1,1,1), labels = c("A","B","C"))
(plot1 <- plot_grid(title, plot1top, legend,  ncol = 1,rel_heights=c(.05,1,.05)))
ggsave("../results/plot2.pdf", width=6, height=14)



