#######################
# Figure 6
#######################

(prop <- ggplot(study2, aes(x = bussperryc01, y = passed01 )) +
		scale_fill_manual(values=c("#F0E442", "#0072B2")) +
		geom_smooth(color="#D55E00", fill= "#D55E00") +
		theme_bw() + 
		ylab("Proportion Passing Engagment Test") +
		xlab("") +
		theme(legend.position="none") +
		geom_hline(yintercept=.5, linetype="dotted", size=1) +
		theme(axis.text.y = element_text(size = 10),
			  axis.title.x = element_text(size = 12, vjust = -.1),
			  panel.grid.major = element_blank(),
			  panel.grid.minor = element_blank(),
			  axis.text.x = element_blank(),) +
		scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
		scale_y_continuous(limits = c(0,1), expand = c(0, 0))  +
		theme(plot.margin = unit(c(.2, .5, 0, .2), "cm")))

(hist <- ggplot(data=study2, aes(bussperryc01), color=passed) + 
		geom_density(aes(fill=passed), alpha=.4)  +
		scale_fill_manual(values=c("#F0E442", "#0072B2")) +
		theme_bw() + 
		ylab("Density\n") +
		xlab("Buss-Perry Aggression\nRescaled 0-1") +
		theme(legend.position="none") +
		theme(axis.text.y = element_text(size = 10),
			  axis.text.x = element_text(size = 10),
			  axis.title.x = element_text(size = 12, vjust = -.1),
			  panel.grid.major = element_blank(),
			  panel.grid.minor = element_blank()) +
		scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
		scale_y_continuous(limits = c(0,2.5), expand = c(0, 0)) +
		theme(plot.margin = unit(c(0, .5, .2, .2), "cm")))


plotlegend <- ggplot(data=study2, aes(bussperryc01), color=passed) + 
	geom_density(aes(fill=passed), alpha=.4)  +
	scale_fill_manual(values=c("#F0E442", "#0072B2")) +
	theme(legend.position="bottom") +
	guides(fill=guide_legend(""))

legend <- cowplot::get_legend(plotlegend)

title <- ggdraw() + draw_label("Problem of Disengagement in Measure of Aggression ", fontface='bold')

plottop <- plot_grid(prop, hist, label_size = 12, ncol=1,rel_heights=c(1,.6), labels = c("A","B"))
plot <- plot_grid(title, plottop, legend,  ncol = 1,rel_heights=c(.05, 1,.05))

ggsave("../results/plot6.pdf", width=6, height=6)
