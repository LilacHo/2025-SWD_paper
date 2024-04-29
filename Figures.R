library(ggplot2)
library(tidyverse)

DD <- read.csv("DD 2021+2022.csv",na.strings = c("","NA"),header = TRUE)
str(DD)

DD$Year <- as.factor(DD$Year)

Combinedd <- function(x){
  1/(1+exp(7.283721-0.004784*x))
}

WL2021d <- function(x){
  1/(1+exp(11.59789-0.00741*x))
}

NJ2021d <- function(x){
  1/(1+exp(7.959260-0.006555*x))
}

WL2022d <- function(x){
  1/(1+exp(10.21779-0.00664*x))
}

NJ2022d <- function(x){
  1/(1+exp(11.59777-0.00817*x))
 }

NB2022d <- function(x){
  1/(1+exp(16.03475-0.00870*x))
}

## Figure 3
f3 <- ggplot(data=DD, aes(x = CDD, y = proportion)) +
  stat_function(fun = NJ2021d, size =1.5, aes(colour = "2021 NJ"), linetype = "longdash")+
  stat_function(fun = WL2021d, size =1.5, aes(colour = "2021 WL"), linetype = "longdash")+
  stat_function(fun = NJ2022d, size =1.5, aes(colour = "2022 NJ"), linetype = "longdash")+
  stat_function(fun = WL2022d, size =1.5, aes(colour = "2022 WL"), linetype = "longdash")+
  stat_function(fun = NB2022d, size =1.5, aes(colour = "2022 NB"), linetype = "longdash") + 
  stat_function(fun = Combinedd, size =2, aes(colour = "Combined")) +
  scale_colour_manual("Year Location", values = c("#785ef0","#648fff","#ffb000","#fe6100","#dc267f", "gray25"),
                      breaks = c('2021 NJ','2021 WL', '2022 NJ', '2022 WL', '2022 NB','Combined')) +
  ylab("Cumulative proportion of trap catch") + 
  xlab("Cumulative Degree Day") +
  #labs(title = "(B)", face = "bold") +
  xlim(500,2500) + 
  theme_bw()+
  theme(axis.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=18),
        #legend.position = c(0.18,0.79),
        legend.position = "bottom",
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill = "transparent")) +
  geom_point(aes(shape = Location,fill=Year), size =4) + 
  scale_shape_manual(values = c(21,22,24), guide="none") +
  #scale_shape_manual(values = c(16,15,17), guide="none") +
  scale_fill_manual(values=c("white","gray15"), guide="none") +
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray35") + 
  geom_hline(yintercept=0.25, linetype="dashed", color = "gray35") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "gray35") +
  annotate("segment", x = 530, xend = 600, y = 0.95, yend = 0.95, colour = "gray25", size = 2) +
  annotate('text',label="Combined",x=750,y=0.95,size=4) +
  annotate('text',label="2021",x=600,y=0.85,size=4) + 
  annotate("point",x=600,y=0.80, size =4, shape=22) +
  annotate("point",x=600,y=0.75, size =4, shape=24) +
  #annotate("point",x=155,y=0.45, size =4, shape=21)+
  annotate('text',label="2022",x=750,y=0.85,size=4) + 
  annotate("point",x=750,y=0.80, size =4, shape=22, fill = "gray15") +
  annotate("point",x=750,y=0.75, size =4, shape=24, fill = "gray15") +
  annotate("point",x=750,y=0.70, size =4, shape=21, fill = "gray15") + 
  annotate('text',label="NJ",x=900,y=0.80,size=4)+ 
  annotate('text',label="WL",x=900,y=0.75,size=4)+ 
  annotate('text',label="NB",x=900,y=0.70,size=4)+
  geom_rect(aes(xmin=500,xmax=1000,ymin=0.625,ymax=1),fill="transparent",colour="black") # # rectangle


## Figure 5
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

f5 <- ggarrange(p2021,p2022,p,
          ncol = 3, nrow = 1)

annotate_figure(f5,  bottom = text_grob("Variety", face = "bold", size=20), 
                left = text_grob("Average egg scars/berry",  face = "bold", size=20, rot = 90))

