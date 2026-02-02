#######################
# Governor Pardon Plot
#######################

plotGovdata <- study4 %>%
	group_by(passed, crime) %>%
	summarise(smean = mean(pardon, na.rm = TRUE),
			  ssd = sd(pardon, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))

plotGovdata$crime <- as.factor(plotGovdata$crime)

plotGovdata$crime <- fct_relevel(plotGovdata$crime, "Protesting without\na Permit", "Vandalism", "Assault", "Arson", "Assault with a\nDeadly Weapon", "Murder")

ggplot(plotGovdata, aes(x = smean, y = crime, color=passed) ) +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), position = position_dodge(width=.5),show.legend=FALSE) +
	geom_point(size = 9, shape=21, fill ="white",position = position_dodge(width=.5)) + theme_bw() + 
	geom_text(size=3,aes(y = as.numeric(crime), 
						 x = smean, 
						 label = gsub("0\\.", "\\.",sprintf("%.2f", round(smean, 2)))),position = position_dodge(width=.5)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="bottom") +
	guides(color=guide_legend("", override.aes = list(shape=19, size=4))) + 
	xlab("Mean Support for a\nGubernatorial Pardon (95% CI)") +
	ylab("") +
	xlim(1,5) + 
	geom_vline(xintercept = 3, linetype = "dotted") +
	ggtitle("")

ggsave("../results/plotgovernor.pdf", width=6, height=4)