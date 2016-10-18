library(ggplot2)
library(ggthemes)

HM_Corr <- read.csv("~/Desktop/Career/HM_Corr.csv")
Overdoses <- read.csv("~/Desktop/Career/Overdoses.csv")
Overdoses <- Overdoses[,c(1,2,5,8,14,17)]



Overdoses_melt <- melt(Overdoses,id.vars = "Years")

summary(Overdoses_melt)
str(Overdoses_melt)
Overdoses_melt$value <- gsub(",","",Overdoses_melt$value)
Overdoses_melt$color <- "Other Lethal Drugs"
Overdoses_melt$color[which(Overdoses_melt$variable=='Heroin' & Overdoses_melt$Years == c(2010, 2011, 2012, 2013, 2014))] <- "Heroin"
Overdoses_melt$variable <- gsub("Opioid.Analgesics..","Opioids",Overdoses_melt$variable)
Overdoses_melt$variable <- gsub("Prescription.Drugs","Prescriptions Drugs",Overdoses_melt$variable)

Overdoses_melt$color <- 0
Overdoses_melt$color[which(Overdoses_melt$variable=='Heroin'  & Overdoses_melt$Years == c(2010, 2011, 2012, 2013, 2014))] <- Overdoses_melt$value[which(Overdoses_melt$variable=='Heroin' & Overdoses_melt$Years == c(2010, 2011, 2012, 2013, 2014))]

class(Overdoses_melt$Years)
Overdoses_melt$value <- as.numeric(Overdoses_melt$value)
Overdoses_melt$color <- as.numeric(Overdoses_melt$color)
Overdoses_melt <- Overdoses_melt[which(Overdoses_melt$variable %in% c("Heroin","Benzodiazepines","Cocaine")),]

ggplot(Overdoses_melt,aes(Years,value, fill=variable)) +
  geom_line(aes(color=color))+
  labs(x="Year") +
  ggtitle("Drug Overdoses by Year")+
  ylab("Number of Overdoses in the US") +
  theme_fivethirtyeight_mod() +
  theme(legend.title=element_blank())  +
  geom_text(data = subset(Overdoses_melt,Years=='2014'),aes(label=variable),vjust=-1) +
  scale_x_continuous(breaks=c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)) +
  scale_color_continuous(low = "#979797", high = "#FC576B") +
  theme(legend.position="none") +
  labs(x="Year") +
  annotate("rect", ymin = 0, ymax = 15000,xmin=2010,xmax = 2015, alpha = .1) +
  annotate("text", x= 2012.5, y = 14000, label="Mass Decriminalization and\nLegalization of Cannabis", fontface=4, color='#4A4A4A')
  
  
HM_Corr$Marijuana_Arrests <- gsub(",","",HM_Corr$Marijuana_Arrests)
HM_Corr$Marijuana_Arrests <- as.numeric(HM_Corr$Marijuana_Arrests)
HM_Corr$Heroin_Overdoses <- gsub(",","",HM_Corr$Heroin_Overatopdoses)
HM_Corr$Heroin_Overdoses <- as.numeric(HM_Corr$Heroin_Overdoses)



HM_Melt <- melt(HM_Corr,id.vars = "Year")
HM_Melt$value <- gsub(",","",HM_Melt$value)
HM_Melt$value <- as.numeric(HM_Melt$value)

options(scipen=999)

HM_Corr$color <- "Before 2010"
HM_Corr$color[which(HM_Corr$Year>2009)] <- "After 2010"

ggplot(HM_Corr, aes(Heroin_Overdoses, Marijuana_Arrests, fill=Year)) +
  geom_point(aes(size=Year,color=color)) + 
  geom_text(data=subset(HM_Corr,Year > 2009),aes(label=Year),hjust=1.3, fontface=2, alpha=.7) +
  xlab("Heroin Overdoses") +
  ylab("Cannibus Seizures (kgs)") +
  ggtitle("Declining Cannabis Seizures, Increasing Heroin Overdoses in the US")+
  theme_fivethirtyeight_mod() +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  scale_colour_manual(values = c("#FC576B","#979797")) +
  annotate("segment", x=3000,xend = 10500, y=725000,yend = 74255, linetype="dashed") +
  annotate("text", x=7300, y=550000, label ="R2: 0.97\nHO = 11253.33 - 0.01 Arrests",fontface=3,color="#353535")

library(googleVis)
dea_statistics <- read.csv("~/Documents/dea_statistics.csv")

dea_statistics$Year <- as.numeric(dea_statistics$Year)
dea_statistics <- dea_statistics[,c(1:5)]
dea_melt <- melt(dea_statistics, id.vars = "Year")
dea_melt$color <- "No"
dea_melt$color[which(dea_melt$variable=="Marijuana_kgs")] <- "MJ"
dea_melt$color[which(dea_melt$variable=="Heroin_kgs")] <- "H"

dea_melt$variable <- gsub(c("Methamphetamine_kgs"),c("Methamphetamine"),dea_melt$variable)

ggplot(dea_melt,aes(Year,value)) +
  geom_line(aes(color=color))+
  facet_wrap(~variable, scales = "free_y") +
  ggtitle("DEA Drug Seizures by Year (kgs)")+
  theme_fivethirtyeight_mod() +
  scale_color_manual(values = c("#FC576B","#91CC4C","#979797")) +
  ylab("Sizes of Seizures") +
  theme(legend.title=element_blank()) +
  annotate("rect", ymin=-Inf, ymax=Inf, xmin=2010,xmax = 2015, alpha = .1) +
  theme(legend.position="none")
  

plot(gvisMotionChart(dea_statistics, idvar="Year", timevar ="Year",xvar = "Heroin_kgs", yvar = "Marijuana_kgs"))
gvisMotionChart()



theme_fivethirtyeight_mod <- function (base_size = 12, base_family = "sans") {
    (theme_foundation(base_size = base_size, base_family = base_family) + 
       theme(line = element_line(colour = "black"), rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"], linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]), 
             axis.text = element_text(color = 'black'), axis.ticks = element_blank(), axis.line = element_blank(), 
             legend.title = element_blank(), legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
             legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", 
             panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]), 
             panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"), 
             plot.margin = unit(c(1, 1, 1, 1), "lines"), strip.background = element_rect()))
}


ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
  
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  # Draw it
  grid.draw(g)
  
}

library(ggplot2)
library(grid)
library(dplyr)

#' Create some data to play with. Two time series with the same timestamp.
df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2), series1 = rnorm(8761), series2 = rnorm(8761, 100))

#' Create the two plots.
plot1 <- df %>%
  select(DateTime, series1) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
  ylab("Red dots / m") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot2 <- df %>%
  select(DateTime, series2) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
  ylab("Blue drops / L") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))



fit <- lm(Heroin_Overdoses ~ Marijuana_Arrests, data=subset(HM_Corr,Year>2007))
summary(fit)
##Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 36.90833    2.19080  16.847  < 2e-16 ***
## cyl         -2.26469    0.57589  -3.933  0.00048 ***
## hp          -0.01912    0.01500  -1.275  0.21253 


plot(mpg ~ cyl, data = mtcars, xlab = "Cylinders", ylab = "Miles per gallon")
abline(coef(fit)[1:2])

## rounded coefficients for better output
cf <- round(coef(fit), 2) 

## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("HO = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Arrests ")

## printing of the equation
mtext(eq, 3, line=-2)

