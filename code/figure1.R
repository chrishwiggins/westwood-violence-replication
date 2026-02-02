#######################
# Figure 1 (Histogram)
######################

kme <- read_csv("../data/priorestimates.csv")

# compute highest support in our data

prop.table(table(support = study1$supportactions[study1$passed=="Engaged Respondent" & study1$alignment == "In-Party Driver"] >3))
prop.table(table(study2$supportactions[study2$passed=="Engaged Respondent" & study2$alignment == "In-Party Shooter"] >3))
prop.table(table(study3$supportactions[study3$passed=="Engaged Respondent" & study3$alignment == " In-Party Shooter"] >3))

mean(kme$PartisansSupport,na.rm=T)
median(kme$PartisansSupport,na.rm=T)

mean(c(.126506, .02150538, 0.02994792))
median(c(.126506, .02150538, 0.02994792))


18.85/2.99

all <- ggplot(data=kme[!is.na(kme$PartisansSupport),], aes(PartisansSupport)) + 
	geom_histogram(bins=30, aes(y=..density..)) + 
	scale_x_continuous(breaks=seq(0,50, by =10)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="none") +
	geom_vline(xintercept=c(12.6), linetype="solid", size=2, color ="#D55E00") + 
	geom_vline(xintercept=c(2.2), linetype="solid", size=2, color="#0072B2") + 
	
	xlab("Percent of Americans Supporting Violence") +
	ylab("Density")+ 
	coord_cartesian(xlim = c(0, 50), ylim = c(0,.3))

##### Democrats

prop.table(table(support = study1$supportactions[study1$alignment == "In-Party Driver" & study1$pid == "Democrat" & study1$passed == "Engaged Respondent"] >3))

prop.table(table(support =study2$supportactions[study2$alignment == "In-Party Shooter" & study2$pid == "Democrat" & study2$passed == "Engaged Respondent"] >3))

prop.table(table(study3$supportactions[study3$alignment == " In-Party Shooter" & study3$pid3 == "Democrat" & study3$passed == "Engaged Respondent"] >3))

dem <- ggplot(data=kme[!is.na(kme$DemocratSupport),], aes(DemocratSupport)) + 
	geom_histogram(bins=30, aes(y=..density..)) + 
	scale_x_continuous(breaks=seq(0,50, by =10)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="none") +
	geom_vline(xintercept=c(12.2), linetype="solid", size=2, color ="#D55E00") + 
	geom_vline(xintercept=c(3.2), linetype="solid", size=2, color="#0072B2") + 
	
	xlab("Percent of Democrats Supporting Violence") +
	ylab("Density")+ 
	coord_cartesian(xlim = c(0, 50), ylim = c(0,.3))

#### Republicans

prop.table(table(support = study1$supportactions[study1$alignment == "In-Party Driver" & study1$pid == "Republican" & study1$passed == "Engaged Respondent"] >3))

prop.table(table(support =study2$supportactions[study2$alignment == "In-Party Shooter" & study2$pid == "Republican" & study2$passed == "Engaged Respondent"] >3))

prop.table(table(study3$supportactions[study3$alignment == " In-Party Shooter" & study3$pid3 == "Republican" & study3$passed == "Engaged Respondent"] >3))

rep <- ggplot(data=kme[!is.na(kme$RepublicanSupport),], aes(RepublicanSupport)) + 
	geom_histogram(bins=30, aes(y=..density..)) + 
	scale_x_continuous(breaks=seq(0,50, by =10)) +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  axis.title.x = element_text(size = 12, vjust = -.1),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank()) +
	theme(legend.position="none") +
	geom_vline(xintercept=c(13.2), linetype="solid", size=2, color ="#D55E00") + 
	geom_vline(xintercept=c(1.1), linetype="solid", size=2, color="#0072B2") + 
	xlab("Percent of Republicans Supporting Violence") +
	ylab("Density") + 
	coord_cartesian(xlim = c(0, 50), ylim = c(0,.3))

title <- ggdraw() + draw_label("Distribution of All Kalmoe-Mason Derived\nPercentages of Support for Political\n Violence From the Media", fontface='bold')             


(hist <- plot_grid(title,all, rep, dem, label_size = 12, ncol=1,rel_heights=c(.4,1,1,1), labels = c("","A","B","C")))

ggsave("../results/plot_hist.pdf", width=6, height=6)


