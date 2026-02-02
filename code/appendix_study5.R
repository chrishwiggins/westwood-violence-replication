#######################
# Study 4
#######################

#individual justification

study5$indjustification <- 0
study5$indjustification[study5$Q77 >=4] <- 1
mean(study5$indjustification)

plot4data <- study5 %>%
	group_by(passed, study5cell) %>%
	summarise(smean = mean(inpartysupport, na.rm = TRUE),
			  ssd = sd(inpartysupport, na.rm = TRUE),
			  count = n()) %>%
	mutate(se = ssd / sqrt(count),
		   lower = lower_ci(smean, se, count),
		   upper = upper_ci(smean, se, count))



plot4data$study5cell <- as.factor(plot4data$study5cell)

ggplot(plot4data, aes(x = smean, y = study5cell, color=passed) ) +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), position = position_dodge(width=.5),show.legend=FALSE) +
	geom_point(size = 9, shape=21, fill ="white",position = position_dodge(width=.5)) + theme_bw() + 
	geom_text(size=4,aes(y = as.numeric(study5cell), 
						 x = smean, 
						 label = gsub("0\\.", "\\.",round(smean, 0))),position = position_dodge(width=.5)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="bottom") +
	guides(color=guide_legend("", override.aes = list(shape=19, size=4))) + 
	xlab("Percieved Proportion of\nOwn Party Supporting Violence (95% CI)") +
	ylab("") +
	xlim(5,45) +
	annotate("text", x = 9, y=2.45, label = "<- CCES Estimate from (2)", hjust=-0, size=4) +
	annotate("text", x = 10.5, y=1.45, label = "<- Mean individual support\nfrom this survey", hjust=0, size=4) +
	geom_vline(xintercept = 9, linetype = "dotted") +
	geom_vline(xintercept = mean(study5$indjustification)* 100, linetype = "dotted") +
	
	ggtitle("")

ggsave("../results/plotstudy4.pdf", width=5.5, height=3)