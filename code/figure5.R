#######################
# Figure 4
#######################

study4plotdata <- study4 %>%
	group_by(passed, crime, sentence) %>%
	summarise(n = n()) %>%
	mutate(freq = n / sum(n)) %>%
	ungroup() %>%
	complete(passed, crime, sentence,
			 fill = list(n = 0, freq = 0))

study4plotdata$passed <- as.factor(study4plotdata$passed)

plot5 <-ggplot(data=study4plotdata, aes(x=sentence, y=freq, fill=passed)) +
	theme_bw() +
	geom_bar(stat="identity", position = 'dodge') +
	ylim(0,1)+
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="bottom") +
	theme(axis.text.x = element_text(angle =50, vjust = 1, hjust=1)) +
	ylab("Proportion Reccomending") +
	xlab("Proposed Sentence") +
	guides(fill=guide_legend("")) + 
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	facet_grid(crime~., scales = "free")

title <- ggdraw() + draw_label("Distributions of Proposed Sentances Among\nEngaged and Disengaged Respondents", fontface='bold')


(plot5top <- plot_grid(title, plot5, ncol=1, rel_heights=c(0.05, 1)) )


ggsave("../results/plot5.pdf", width=5, height=12)