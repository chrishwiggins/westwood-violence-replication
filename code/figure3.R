#######################
# Figure 3 (study 1)
#######################

#### Justified

plots2adata <- study1 %>%
	group_by(alignment, passed, justified) %>%
	summarise(n = n()) %>%
	mutate(freq = n / sum(n)) %>%
	ungroup() %>%
	complete(alignment,passed,justified,
			 fill = list(n = 0, freq = 0))

plots2adata$passed <- as.factor(plots2adata$passed)

plots2adata$justified <- as.factor(plots2adata$justified)
plots2adata$justified <- recode_factor(plots2adata$justified, "0" = "No", "1" = "Yes")

plot2a <- ggplot(data=plots2adata, aes(x=alignment, y=freq, fill=justified)) +
	theme_bw() +
	geom_bar(stat="identity", position = 'dodge') +
	ylim(0,1)+
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank(),
		  axis.title.x=element_blank(),
		  legend.margin=margin(0,0,0,0),
		  legend.box.margin=margin(t=0)) +
	theme(legend.position="bottom") +
	theme(axis.text.x = element_text(angle =40, vjust = 1, hjust=1)) +
	ylab("Proportion") +
	ggtitle("Driver is Justified by Response") +
	guides(fill=guide_legend("")) +
	geom_hline(yintercept = .5, linetype="dashed") + 
	guides(fill=guide_legend("",override.aes = list(size=2))) + 
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
	facet_grid(.~passed)


#### support

plots2bdata <- study1 %>%
	group_by(alignment,  passed, supportactions) %>%
	summarise(n = n()) %>%
	mutate(freq = n / sum(n)) %>%
	ungroup() %>%
	complete(alignment,passed,supportactions,
			 fill = list(n = 0, freq = 0))

plots2bdata$passed <- as.factor(plots2bdata$passed)
plots2bdata$supportactions <- as.factor(plots2bdata$supportactions)
plots2bdata$supportactions <- recode_factor(plots2bdata$supportactions, "1" = "Strongly\nOppose", "2" = "Oppose", "3" = "Neither Oppose\nNor Support", "4" = "Support", "5" = "Strongly\nSupport")

plot2b <- ggplot(data=plots2bdata, aes(x=alignment, y=freq, fill=supportactions)) +
	theme_bw() +
	geom_bar(stat="identity", position = 'dodge') +
	ylim(0,1)+
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank(),
		  axis.title.x=element_blank(),
		  legend.margin=margin(0,0,0,0),
		  legend.box.margin=margin(l=-30,t=0),
		  legend.justification='left') +
	theme(legend.position="bottom") +
	theme(axis.text.x = element_text(angle =40, vjust = 1, hjust=1)) +
	ylab("Proportion") +
	ggtitle("Support for Driver by Response") +
	guides(fill=guide_legend("")) +
	geom_hline(yintercept = .2, linetype="dashed") +
	guides(fill=guide_legend("",override.aes = list(size=2))) + 
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
	facet_grid(.~passed)

#### Charged

plots2cdata <- study1 %>%
	group_by(alignment, passed, charged) %>%
	summarise(n = n()) %>%
	mutate(freq = n / sum(n)) %>%
	ungroup() %>%
	complete(alignment,passed,charged,
			 fill = list(n = 0, freq = 0))

plots2cdata$passed <- as.factor(plots2cdata$passed)
plots2cdata$charged <- as.factor(plots2cdata$charged)
plots2cdata$charged <- recode_factor(plots2cdata$charged, "0" = "No", "1" = "Yes")

plot2c <- ggplot(data=plots2cdata, aes(x=alignment, y=freq, fill=charged)) +
	theme_bw() +
	geom_bar(stat="identity", position = 'dodge') +
	ylim(0,1)+
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank(),
		  axis.title.x=element_blank(),
		  legend.margin=margin(0,0,0,0),
		  legend.box.margin=margin(t=0)) +
	theme(legend.position="bottom") +
	theme(axis.text.x = element_text(angle =40, vjust = 1, hjust=1)) +
	ylab("Proportion") +
	ggtitle("Driver Should be Charged by Response") +
	guides(fill=guide_legend("")) +
	geom_hline(yintercept = .5, linetype="dashed") +
	guides(fill=guide_legend("",override.aes = list(size=2))) + 
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	scale_fill_manual(values=c( "#E69F00", "#56B4E9")) +
	facet_grid(.~passed)


plot1top <- plot_grid(plot2a,plot2b,plot2c, label_size = 12, ncol=1, labels = c("A","B","C"))
title <- ggdraw() + draw_label("Distributions of Responses Among\nEngaged and Disengaged Respondents", fontface='bold')


plot1top <- plot_grid(title, plot1top, ncol=1, rel_heights=c(0.05, 1)) 
ggsave("../results/plot3.pdf", width=5, height=12)
