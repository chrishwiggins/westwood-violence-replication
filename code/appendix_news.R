newsCoverage2016_2021 <- read_csv("../data/newsCoverage2016-2021.csv") 

newsCoverage2016_2021$Date <- as.Date(newsCoverage2016_2021$Date, "%m/%d/%y")

monthyear <-newsCoverage2016_2021 %>%
	group_by(month = format(Date, "%B"), year = format(Date, "%Y")) %>%
	summarise(total = n())

monthyear$month <- as.factor(monthyear$month)
monthyear$month <- fct_relevel(monthyear$month, "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

monthyear  %>%
	ggplot(aes(x = month, y = total)) +
	geom_bar(stat = "identity", fill = "darkorchid4") +
	theme(axis.text.y = element_text(size = 10),
		  axis.text.x = element_text(size = 10),
		  panel.grid.major = element_blank(),
		  panel.grid.minor = element_blank(),
		  axis.title.x=element_blank(),
		  legend.margin=margin(0,0,0,0),
		  legend.box.margin=margin(l=-30,t=0),
		  legend.justification='left') +
	theme(strip.background =element_rect(fill="Black"))+
	theme(strip.text = element_text(colour = 'white')) +
	theme(axis.text.x = element_text(angle =40, vjust = 1, hjust=1)) +
	labs(title = "News Coverage of American Political Violence",
		 subtitle = "(January 2016 - August 2021)",
		 y = "Number of News Stories",
		 x = "Month") +	facet_wrap(~ year, ncol = 2) 

ggsave("../results/news.pdf", width=6, height=6)