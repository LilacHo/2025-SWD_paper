# R Version 4.2.1

library(ggplot2)
library(dplyr)
library(ggpubr)

DD <- read.csv("DD 2021-2023.csv",na.strings = c("","NA"),header = TRUE)
str(DD)

DD$Year <- as.factor(DD$Year)

Combinedd <- function(x){
  1/(1+exp(8.0939868-0.0059552*x))
}

NJ2021d <- function(x){
  1/(1+exp(8.2064131-0.0066676*x))
}

WL2021d <- function(x){
  1/(1+exp(13.6566289-0.0089623*x))
}

NJ2022d <- function(x){
  1/(1+exp(10.117933-0.007811*x))
}

WL2022d <- function(x){
  1/(1+exp(10.7856383-0.0075591*x))
}

NJ2023d <- function(x){
  1/(1+exp(7.173633-0.006567*x))
}

WL2023d <- function(x){
  1/(1+exp(10.1641188-0.0074258*x))
}

NJCombined <- function(x){
  1/(1+exp(7.7709758-0.0064388*x))
}

WLCombined <- function(x){
  1/(1+exp(11.3422991-0.0077449*x))
}

## Figure 1###############
f1A <- ggplot(data=DD,aes(x = CDD_PRISM, y = proportion)) +
  # add points
  geom_point(aes(shape = Year, fill = Location), size =4) + 
  scale_shape_manual(values = c(22,24,21), guide="none") +
  scale_fill_manual(values=c("white","gray35"), guide="none") +
  # add lines
  stat_function(fun = NJ2021d, size =1.5, aes(colour = "2021 NJ"), linetype = "dotdash")+
  stat_function(fun = WL2021d, size =1.5, aes(colour = "2021 WL"), linetype = "longdash")+
  stat_function(fun = NJ2022d, size =1.5, aes(colour = "2022 NJ"), linetype = "dotdash")+
  stat_function(fun = WL2022d, size =1.5, aes(colour = "2022 WL"), linetype = "longdash")+
  stat_function(fun = NJ2023d, size =1.5, aes(colour = "2023 NJ"), linetype = "dotdash")+
  stat_function(fun = WL2023d, size =1.5, aes(colour = "2023 WL"), linetype = "longdash")+
  # stat_function(fun = Combinedd, size =2, aes(colour = "Combined")) +
  scale_colour_manual("Year Location", values = c("gray45","gray45","gray20","gray20", "gray70", "gray70"),
                      breaks = c('2021 NJ','2021 WL', '2022 NJ', '2022 WL', '2023 NJ', '2023 WL')) +
  ylab("Cumulative proportion of trap catch") + 
  #xlab("Cumulative Degree Day") +
  xlim(500,2000) + 
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        #plot.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, face = "bold"),
        # Remove legend
        legend.position = "none") +
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray10") + 
  geom_hline(yintercept=0.25, linetype="dashed", color = "gray10") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "gray10") +
  annotate('text',label="A",x=500,y=1,size=6, fontface = "bold") +
  #annotate("segment", x = 530, xend = 600, y = 0.95, yend = 0.95, colour = "gray25", size = 2) +
  #annotate('text',label="Combined",x=750,y=0.95,size=4) +
  annotate('text',label="NJ",x=600,y=0.875,size=4) + 
  annotate("point",x=600,y=0.825, size =4, shape=22) +
  annotate("point",x=600,y=0.775, size =4, shape=24) +
  annotate("point",x=600,y=0.725, size =4, shape=21)+
  annotate('text',label="WL",x=800,y=0.875,size=4) + 
  annotate("point",x=800,y=0.825, size =4, shape=22, fill = "gray35") +
  annotate("point",x=800,y=0.775, size =4, shape=24, fill = "gray35") +
  annotate("point",x=800,y=0.725, size =4, shape=21, fill = "gray35") + 
  annotate('text',label="2021",x=1000,y=0.825,size=4, color = "gray45")+ 
  annotate('text',label="2022",x=1000,y=0.775,size=4, color = "gray20")+ 
  annotate('text',label="2023",x=1000,y=0.725,size=4, color = "gray70")+
  geom_rect(aes(xmin=500,xmax=1100,ymin=0.675,ymax=0.915),fill="transparent",colour="black") # # rectangle


f1B <- ggplot(data=DD,aes(x = CDD_PRISM , y = proportion)) +
  # Change the theme style
  theme_bw() +  # The theme is applied earlier, allowing us to modify it without overriding.
  # Add points
  geom_point(aes(shape = Year, fill = Location), size =4) + 
  scale_shape_manual(values = c(22,24,21), guide="none") +
  scale_fill_manual(values=c("white","gray35"), guide="none") +
  # Add line (Combined model)
  stat_function(fun = NJCombined, size =1.5, linetype = "dotdash", colour = "gray50")+
  stat_function(fun = WLCombined, size =1.5, linetype = "longdash", colour = "gray50")+
  stat_function(fun = Combinedd, size = 2,colour = "gray20") +
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray10") + 
  geom_hline(yintercept=0.25, linetype="dashed", color = "gray10") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "gray10") +
  annotate('text',label="B",x=500,y=1,size=6, fontface = "bold") +
  annotate("segment", x = 550, xend = 750, y = 0.875, yend = 0.875, colour = "gray50", size = 1, linetype = "dotdash") +
  annotate("segment", x = 550, xend = 750, y = 0.825, yend = 0.825, colour = "gray50", size = 1, linetype = "longdash") +
  annotate("segment", x = 550, xend = 750, y = 0.775, yend = 0.775, colour = "gray20", size = 1) +
  annotate('text',label="NJ Combined",x=950,y=0.875,size=4) +
  annotate('text',label="WL Combined",x=950,y=0.825,size=4) +
  annotate('text',label="Combined",x=950,y=0.775,size=4) +
  geom_rect(aes(xmin=500,xmax=1150,ymin=0.725,ymax=0.915),fill="transparent",colour="black") + # # rectangle
  # Adjust the axes
  xlim(500,2000) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12)) +
  # Add x and y titles
  #xlab("Cumulative Degree Day") +
  ylab("Cumulative proportion of trap catch") 
  

  
f1 <- ggarrange(f1A, f1B, ncol = 1, nrow = 2)

annotate_figure(f1, 
                #left = text_grob("Cumulative proportion of trap catch",  face = "bold", size=15, rot = 90),
                bottom = text_grob("Cumulative degree days, base 7.2 °C",  face = "bold", size=16))
# Export: 520 (W) * 896 (H)


## Figure 2###############
abiotic <- read.csv("Abiotic factor 2021+2022_Summary.csv", na.strings = c("","NA"), header=TRUE)
str(abiotic)

# Plot
abiotic$Variety <- as.factor(abiotic$Variety)
levels(abiotic$Variety) <- list(Bluecrop="C",Blueray="R",Elliott="E")
abiotic$Year <- as.factor(abiotic$Year)

lb1 <- paste("R^2 == 0.70")
ggplot(data= abiotic, aes(x=Adult_Trap,y=Infested_rate))+
  geom_point(aes(shape = Variety, color=Year), size =4)+
  geom_smooth(method="lm", colour="gray35", se=FALSE) +
  scale_color_manual(values=c("gray61","gray10"))+ 
  ylab(expression("Rate of infestation by " *italic(D.~suzukii))) + 
  xlab(expression("Total number of "*italic(D.~suzukii)*" captured in traps"))+
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = "bold"),
        #plot.title = element_text(size = 20),
        axis.text=element_text(size=15),
        legend.position = c(0.85,0.25),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill = "transparent"))+
  geom_rect(aes(xmin=360,xmax=470,ymin=0,ymax=0.45),fill="transparent",colour="black")+ # rectangle
  annotate('text',label=lb1,x=80,y=0.9,size=6,parse=TRUE)
# Export: 706 (W) * 609 (H)


## Figure 3###############
infestation <- read.csv("Infestation 2021+2022.csv", na.strings = c("","NA"), header=TRUE)
str(infestation)

# makes Variety a categorical factor
infestation$Variety <-as.factor(infestation$Variety) 

# Plot
levels(infestation$Variety) <- list(Bluecrop="C",Blueray="R",Elliott="E")

y2021 <- infestation %>%
  filter(Year == "2021")

y2022 <- infestation %>%
  filter(Year == "2022")


p2021 <- ggplot(data = y2021, aes(x=Variety,y=Average_scars))+
  geom_bar(stat="summary",color = 'black',aes(fill=Variety),width=0.4)+
  scale_fill_manual(values=c("white","white","gray")) +
  geom_errorbar(stat="summary",fun.data="mean_se",width=0.1)+
  #ylab("Average egg scars/berry") + xlab("Variety") +
  theme_classic() + labs(title = "(A) 2021", face = "bold") +
  annotate("text", x=1, y=3.9, label="a", size = 10)+
  annotate("text", x=2, y=6.6, label="b", size = 10)+
  annotate("text", x=3, y=5.5, label="ab", size = 10) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=18),
        legend.position="none",
        axis.title.x=element_blank(), axis.title.y=element_blank())

p2022 <- ggplot(data = y2022, aes(x=Variety,y=Average_scars))+
  geom_bar(stat="summary",color = 'black',aes(fill=Variety),width=0.4)+
  scale_fill_manual(values=c("white","white","gray")) +
  geom_errorbar(stat="summary",fun.data="mean_se",width=0.1)+
  #ylab("Average egg scars/berry") + xlab("Variety") +
  theme_classic() + labs(title = "(B) 2022", face = "bold") +
  annotate("text", x=1, y=0.3, label="a", size = 10)+
  annotate("text", x=2, y=0.4, label="a", size = 10)+
  annotate("text", x=3, y=1, label="b", size = 10) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=18),
        legend.position="none",
        axis.title.x=element_blank(), axis.title.y=element_blank())

p <- ggplot(data = infestation, aes(x=Variety,y=Average_scars))+
  geom_bar(stat="summary",color = 'black',aes(fill=Variety),width=0.4)+
  scale_fill_manual(values=c("white","white","gray")) +
  geom_errorbar(stat="summary",fun.data="mean_se",width=0.1)+
  #ylab("Average egg scars/berry") + xlab("Variety") +
  theme_classic() + labs(title = "(C) Combined", face = "bold") +
  annotate("text", x=1, y=1.2, label="a", size = 10)+
  annotate("text", x=2, y=1.9, label="ab", size = 10)+
  annotate("text", x=3, y=2.3, label="b", size = 10) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=18),
        legend.position="none",
        axis.title.x=element_blank(), axis.title.y=element_blank())

f3 <- ggarrange(p2021,p2022,p,
                ncol = 3, nrow = 1)

annotate_figure(f3,  bottom = text_grob("Blueberry Variety", face = "bold", size=20), 
                left = text_grob("Average egg scars/berry",  face = "bold", size=20, rot = 90))
# Export: 1200 (W) * 448 (H)
