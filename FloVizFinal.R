## A new plotting of F. Nightingale's Coxcomb diagram of deaths of British soldiers
## by cause in the Crimean War

## Install packages and load library

install.packages("HistData")
install.packages("tidyverse")

library(HistData)
library(tidyverse)

## Get Nightingale Data - Taken from HistData documentation

require(reshape)
Night1<- Nightingale[,c(1,8:10)]
melted <- melt(Night1, "Date")
names(melted) <- c("Date", "Cause", "Deaths")
melted$Cause <- sub("\\.rate", "", melted$Cause)
melted$Regime <- ordered( rep(c(rep('Before', 12), rep('After', 12)), 3),
                          levels=c('Before', 'After'))
Night1 <- melted

# subsets, to facilitate separate plotting
Night1 <- subset(Night, Date < as.Date("1855-04-01"))
Night2 <- subset(Night, Date >= as.Date("1855-04-01"))
# sort according to Deaths in decreasing order, so counts are not obscured [thx: Monique Graf]
Night1 <- Night1[order(Night1$Deaths, decreasing=TRUE),]
Night2 <- Night2[order(Night2$Deaths, decreasing=TRUE),]
# merge the two sorted files
Night <- rbind(Night1, Night2)

## Create facet labels

facetlab <- c("Before Sanitation", "After Sanitation")
names(facetlab) <- c("Before", "After")

## Plot - Stacked

ggplot(Night, aes(fill=Cause, y=Deaths, x=Date)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual("legend", values = c("Disease" = "#0072B2", "Other" = "#000000", "Wounds" = "#EE442F"))+
  facet_wrap(.~Regime, scales = "free_x", labeller = labeller(Regime = facetlab))+
  theme_classic()+
  theme(strip.background = element_blank())+
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  labs(title = "A New Plot of 'Diagram of the Causes of Mortality in the Army in the East' by Florence Nightingale",
       subtitle = "Deaths of British soldiers in the Crimean War by cause (stacked) - Before and After Sanitation Improvements",
       caption = "Original Data by F. Nightingale Source: {HistData} CRAN package | Plot by @WillBall12",
       x = "Date",
       y = "Deaths")

## Plot - Facet Grid

ggplot(Night, aes(fill=Cause, y=Deaths, x=Date)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual("legend", values = c("Disease" = "#0072B2", "Other" = "#000000", "Wounds" = "#EE442F"))+
  facet_grid(Cause ~ Regime, scales = "free_x", labeller = labeller(Regime = facetlab))+
  theme_classic()+
  theme(strip.background = element_blank())+
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  labs(title = "A New Plot of 'Diagram of the Causes of Mortality in the Army in the East' by Florence Nightingale",
       subtitle = "Deaths of British soldiers in the Crimean War by cause (stacked) - Before and After Sanitation Improvements",
       caption = "Original Data by F. Nightingale Source: {HistData} CRAN package | Plot by @WillBall12",
       x = "Date",
       y = "Deaths")