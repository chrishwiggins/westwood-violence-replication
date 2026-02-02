#######################
# Figure 4
#######################

study1$Q77f <- as.factor(study1$Q77)
q77justified <- summary(lm(justified ~ alignment * Q77f, data = study1))
eff<-effect(q77justified,term="alignment * Q77f",as.table=T)
dataeff<-as.data.frame(eff)


dataeff$Q77f<- as.factor(dataeff$Q77f)

ggplot(data=dataeff, aes(y=Q77f, x=fit, color=alignment)) + 
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper),position = position_dodge(width=.75),show.legend=FALSE) +
	geom_point(size = 9, shape=21, fill ="white",position = position_dodge(width=.75)) + theme_bw() + 
	geom_text(size=3.5,aes(y = as.numeric(Q77f), 
						   x = fit, 
						   label = gsub("0\\.", "\\.",sprintf("%.2f", round(fit, 2)))),position = position_dodge(width=.75)) +
	
	ylab("Survey Self-Reported Indicator of\nJustified Political Violence (95% CI)")+
	xlab("Predicted Probability that the Driver is Justified") + 
	theme_bw() + 
	theme(legend.position="bottom") +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	guides(color=guide_legend("", override.aes = list(shape=19, size=4))) + 
	coord_flip() +
	geom_hline(yintercept=c(1.5,2.5,3.5,4.5), linetype="dotted")


ggsave("../results/plot4.pdf", width=6, height=5)