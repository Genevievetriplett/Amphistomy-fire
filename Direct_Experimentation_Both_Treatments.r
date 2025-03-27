library(readxl)
library (ggplot2)
library(tidyr)
library(tidyverse)
library(cowplot)
combined.dat<-as.data.frame(read_csv("C:/Users/spunk/Downloads/Summary stats of the direct exp stomatal densities.csv"))

#sabal etonia subset

sabeto.combined.dat <- subset(combined.dat, species == "sabeto")

Comnined.sabeto.bar<-ggplot(sabeto.combined.dat, aes(x=treatment, y=stdn.mean, fill=leaf.number))+ 
  geom_bar( position="dodge",stat="identity")+
  geom_errorbar(aes(x=treatment, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se),position = position_dodge(0.9), width=0.15)+
  labs(x="Treatment",y=expression ("Stomatal Density per mm"^2))+theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+geom_text(aes(x = 1, y = 1200, label = "*"), size = 8) +scale_fill_manual(values=c("#666666",  "#CCCCCC", "#666666",  "#CCCCCC"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf"))+ coord_cartesian(ylim=c(700,1250))+
  theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size=10))



#serenoa repens subset
serrep.combined.dat <- subset(combined.dat, species == "serrep")

Comnined.serrep.bar<-ggplot(serrep.combined.dat, aes(x=treatment, y=stdn.mean, fill=leaf.number))+ 
  geom_bar( position="dodge",stat="identity")+
  geom_errorbar(aes(x=treatment, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se),position = position_dodge(0.9), width=0.15)+ theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+ theme(axis.title.y = element_blank(), plot.title = element_blank())+
  labs(x="Treatment",
       title=" S. repens ")+scale_fill_manual(values=c("#666666",  "#CCCCCC", "#666666",  "#CCCCCC"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf"))+  coord_cartesian(ylim=c(700,1250))+ theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size=10))

#theme(axis.title.x = element_blank())

################stomatal ratio

#load stomatal ratio dataset
sr.combined.dat<-as.data.frame(read_csv("C:/Users/spunk/Downloads/Summary stats of the direct exp stomatal ratios.csv"))


#sabal etonia subset
sr.sabeto.combined.dat <- subset(sr.combined.dat, species == "sabeto")

sr.Comnined.sabeto.bar<-ggplot(sr.sabeto.combined.dat, aes(x=treatment, y=str.mean, fill = leaf.number))+ 
  geom_bar(position="dodge",stat="identity")+
  geom_errorbar(aes(x=treatment, ymin=str.mean-str.se, ymax=str.mean+str.se), position = position_dodge(0.9), width=0.15)+
  labs(x="Treatment",y="Stomatal ratio (upper/total)",
       title=" S. etonia ")+ggtitle(expression(italic("Sabal etonia")))+theme(plot.title = element_text(size = 20,hjust = 0.5))+geom_text(aes(x = 2, y = 0.535, label = "+"), size = 6)+theme(legend.position = "none") +theme(axis.title.x = element_blank()) +scale_fill_manual(values=c("#666666",  "#CCCCCC", "#666666",  "#CCCCCC"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf"))+
  coord_cartesian(ylim=c(0.4,0.6))+theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size=10), )

#serenoa repens subset
sr.serrep.combined.dat <- subset(sr.combined.dat, species == "serrep")

sr.serrep.combined.dat$time_leaf <- c("before", "regrowth", "before", "regrowth")

sr.Comnined.serrep.bar<-ggplot(sr.serrep.combined.dat, aes(x=treatment, y=str.mean, fill = time_leaf))+ 
  geom_bar( position="dodge",stat="identity")+
  geom_errorbar(aes(x=treatment, ymin=str.mean-str.se, ymax=str.mean+str.se),position = position_dodge(0.9),  width=0.15)+
  labs(x="Treatment",y="Stomatal ratio (upper/total)",
       title=" S. repens ")+ggtitle(expression(italic("Serenoa repens")))+theme(plot.title = element_text(size = 20,hjust = 0.5))+theme(axis.title.x = element_blank(), axis.title.y = element_blank())+geom_text(aes(x = 2, y = 0.52, label = "+"), size = 6)+scale_fill_manual(values=c("#666666",  "#CCCCCC", "#666666",  "#CCCCCC"),labels = c("Before treatment", "After treatment","type3", "type4"))+ theme(legend.position = c(0.75, 0.835), legend.key.size = unit(0.5, "cm"), legend.text = element_text(size=10))+guides(fill = guide_legend(title = element_blank()))+ coord_cartesian(ylim=c(0.4,0.6))+theme(axis.text.x = element_text(size = 11), axis.text.y = element_text(size=10))

#+ guides(fill = guide_legend(labels = c("Before", "After")))
plot_grid(sr.Comnined.sabeto.bar,sr.Comnined.serrep.bar,Comnined.sabeto.bar,Comnined.serrep.bar, labels="AUTO")