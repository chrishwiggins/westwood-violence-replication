m1 <- lm(supportactions01 ~ bussperryc01, data = study2[study2$passed=="Engaged Respondent" & study2$alignment == "In-Party Shooter",])
m3 <- lm(supportactions01 ~ bussperryc01, data = study2[study2$alignment == "In-Party Shooter",])
m4 <- lm(km ~ bussperryc01, data = study2[study2$passed=="Engaged Respondent",])
m6 <- lm(km ~ bussperryc01, data = study2)

plot7 <- tibble(
	cell=numeric(),
	dv = character(),
	engaged = character(),
	est = numeric(),
	lower = numeric(),
	upper = numeric(),
)


plot7 <- plot7 %>% add_row(cell = 1, dv = "Our Measure", engaged= "Engaged Respondents", est = coef(m1)[2], lower = confint(m1)[2,1], upper = confint(m1)[2,2])
#plot7 <- plot7 %>% add_row(cell = 2, dv = "Our Measure", engaged= "Disengaged Respondents", est = coef(m2), lower = confint(m2)[1], upper = confint(m2)[2])
plot7 <- plot7 %>% add_row(cell = 3, dv = "Our Measure", engaged= "All Respondents", est = coef(m3)[2], lower = confint(m3)[2,1], upper = confint(m3)[2,2])
plot7 <- plot7 %>% add_row(cell = 4, dv = "Kalmoe-Mason's Measure", engaged= "Engaged Respondents", est = coef(m4)[2], lower = confint(m4)[2,1], upper = confint(m4)[2,2])
#plot7 <- plot7 %>% add_row(cell = 5, dv = "Kalmoe-Mason's Measure", engaged= "Disengaged Respondents", est = coef(m5), lower = confint(m5)[1], upper = confint(m5)[2])
plot7 <- plot7 %>% add_row(cell = 6, dv = "Kalmoe-Mason's Measure", engaged= "All Respondents", est = coef(m6)[2], lower = confint(m6)[2,2], upper = confint(m6)[2,2])



plot7$engaged <- fct_relevel(plot7$engaged, "Engaged Respondents","All Respondents")

plot7final <- ggplot(plot7, aes(x = est, y = engaged, color=engaged) ) +
	ggtitle("") +
	geom_errorbarh(height = 0, aes(xmin = lower, xmax = upper), color = rep(c("#0072B2", "#D55E00"),2), position = position_dodge(width=.51),show.legend=FALSE) +
	geom_point(size = 9, shape=rep(21:22,2), fill ="white", position = position_dodge(width=.51), color = rep(c("#0072B2", "#D55E00"),2)) + theme_bw() + 
	geom_text(size=3.5,	color=rep(c("#0072B2", "#D55E00"),2),
			  aes(y = c(1,2,1,2), 
			  	x = est, 
			  	label = gsub("0\\.", "\\.",sprintf("%.2f", round(est, 2)))),position = position_dodge(width=.51)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="none") +
	guides(color=guide_legend("Comprehension Check", override.aes = list(shape=c(19,20), size=4))) + 
	xlab("Estimated Relationship Between\nAggression and Violence (95% CI)") +
	xlim(-.01,1.4) + 
	ylab("") +
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	facet_wrap(.~dv, ncol=1)

title <- ggdraw() + draw_label("Disengaged Respondents Inflate Correlates of Violence", fontface='bold')

(plot7top <- plot_grid(title, plot7final, ncol=1, rel_heights=c(0.1, 1)) )



ggsave("../results/plot7.pdf", width=6, height=3)



