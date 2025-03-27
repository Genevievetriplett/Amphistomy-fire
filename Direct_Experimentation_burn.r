library(readxl)
library (ggplot2)
library(tidyr)
library(tidyverse)
library(lme4)
library(car)

#load in my data
DE.dat.updated<-as.data.frame(read_excel("C:/Users/spunk/Downloads/Data from the direct experiment.xlsx"))

#filter to work with only sabal etonia and burn treatment
sabeto.DE.dat <- subset(DE.dat.updated, species == "sabeto")
sabeto.De.dat.burn <- subset(sabeto.DE.dat, treatment == "burn" )

#take the average of the multiple replicates 
DE.indiv.avg<-with(sabeto.De.dat.burn, aggregate(list(upper.count.leaf0=upper.count.leaf0, lower.count.leaf0=lower.count.leaf0, upper.count.leaf2=upper.count.leaf2, lower.count.leaf2=lower.count.leaf2), list(treatment=treatment, species=species, individual=individual), mean, na.rm=T))
dim(DE.indiv.avg)
head(DE.indiv.avg)

#convert counts to density using image area

DE.indiv.avg$upper.den.leaf0 <- DE.indiv.avg$upper.count.leaf0/0.1644
DE.indiv.avg$upper.den.leaf2 <- DE.indiv.avg$upper.count.leaf2/0.1644
DE.indiv.avg$lower.den.leaf0 <- DE.indiv.avg$lower.count.leaf0/0.1644
DE.indiv.avg$lower.den.leaf2 <- DE.indiv.avg$lower.count.leaf2/0.1644

#calculate stomatal density for each one

DE.indiv.avg$leaf0.stdn <- DE.indiv.avg$upper.den.leaf0 + DE.indiv.avg$lower.den.leaf0
DE.indiv.avg$leaf2.stdn <- DE.indiv.avg$upper.den.leaf2 + DE.indiv.avg$lower.den.leaf2
print(DE.indiv.avg)

#give them individual IDs rather than the numbering system
prevspost_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")


##### take the averages for the prefire and post fire leaves

prevspost_gather$leaf.number <- c(0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2)
sabeto_postfire<-with(prevspost_gather, aggregate(list(stdn=stdn), list(leaf.number=leaf.number),mean,na.rm=T))

#statistics
sabeto_postfire2 <- prevspost_gather %>% group_by(leaf.number) %>% summarise(
  stdn.mean = mean(stdn, na.rm=T),
  stdn.sd = sd(stdn, na.rm=T),
  stdn.n = length(stdn[!is.na(stdn)]),
  stdn.se = stdn.sd/sqrt(stdn.n)
)

#add for bar graph labelling purposes
sabeto_postfire2$leaf.number[sabeto_postfire2$leaf.number==0] <- "leaf 0"
sabeto_postfire2$leaf.number[sabeto_postfire2$leaf.number==2] <- "leaf 2"

#bar graph as example but not the one in my manuscript, go to the both treatments code for that

sabeto_postfire.sd.bar<-ggplot(sabeto_postfire2, aes(x=leaf.number, y=stdn.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal density",
       title="Stomatal Density of S. etonia before and after fire")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Treatment"))+
  ylim(0,1500)
sabeto_postfire.sd.bar

#These are the statistics I reported
t.test(stdn~leaf_no, data=prevspost_gather)



############ repeat for serrep

serrep.DE.dat<-as.data.frame(read_excel("C:/Users/spunk/Downloads/Data from the direct experiment.xlsx"))

serrep.DE.dat <- subset(DE.dat.updated, species == "serrep")
serrep.De.dat.burn <- subset(serrep.DE.dat, treatment == "burn" )

dim(serrep.De.dat.burn)
DE.indiv.avg.serrep<-with(serrep.De.dat.burn, aggregate(list(upper.count.leaf0=upper.count.leaf0, lower.count.leaf0=lower.count.leaf0, upper.count.leaf2=upper.count.leaf2, lower.count.leaf2=lower.count.leaf2), list(treatment=treatment, species=species, individual=individual), mean, na.rm=T))
dim(DE.indiv.avg.serrep)
head(DE.indiv.avg.serrep)

DE.indiv.avg.serrep$upper.den.leaf0 <- DE.indiv.avg.serrep$upper.count.leaf0/0.1644

DE.indiv.avg.serrep$upper.den.leaf2 <- DE.indiv.avg.serrep$upper.count.leaf2/0.1644
DE.indiv.avg.serrep$lower.den.leaf0 <- DE.indiv.avg.serrep$lower.count.leaf0/0.1644

DE.indiv.avg.serrep$lower.den.leaf2 <- DE.indiv.avg.serrep$lower.count.leaf2/0.1644

#calculate stomatal density for each one

DE.indiv.avg.serrep$leaf0.stdn <- DE.indiv.avg.serrep$upper.den.leaf0 + DE.indiv.avg.serrep$lower.den.leaf0

DE.indiv.avg.serrep$leaf2.stdn <- DE.indiv.avg.serrep$upper.den.leaf2 + DE.indiv.avg.serrep$lower.den.leaf2
print(DE.indiv.avg.serrep)

prevspost_gather_serrep$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")

# filter out 'na's
serrep_subset <- subset(prevspost_gather_serrep, !(ind_ID %in% c("Ind2", "Ind3", "Ind5")))
serrep_subset_bargraph_prevspost<-ggplot(serrep_subset, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

serrep_subset$leaf.number <- c(0,0,0,0,0,0,0,2,2,2,2,2,2,2)

serrep_postfire2 <- serrep_subset %>% group_by(leaf.number) %>% summarise(
  stdn.mean = mean(stdn, na.rm=T),
  stdn.sd = sd(stdn, na.rm=T),
  stdn.n = length(stdn[!is.na(stdn)]),
  stdn.se = stdn.sd/sqrt(stdn.n)
)
serrep_postfire2$leaf.number[serrep_postfire2$leaf.number==0] <- "leaf 0"
serrep_postfire2$leaf.number[serrep_postfire2$leaf.number==2] <- "leaf 2"

serrep_postfire.sd.bar<-ggplot(serrep_postfire2, aes(x=leaf.number, y=stdn.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal density",
       title="S. Density of S. repens before and after fire")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Treatment"))+
  ylim(0,1500)

print(serrep_postfire.sd.bar)


#####these are my statistics I reported

t.test(stdn~leaf_no, data=serrep_subset)


############linear models##############

#sabeto
m.sabeto.sd <-lmer(stdn~leaf.number+(1|individual),data=prevspost_gather)
summary(m.sabeto.sd)
Anova(m.sabeto.sd)
hist(resid(m.sabeto.sd))

#serrep
m.serrep.sd <-lmer(stdn~leaf.number+(1|individual),data=serrep_subset)
summary(m.serrep.sd)
Anova(m.serrep.sd)
hist(resid(m.serrep.sd))


#############################stomatal ratio############################################

DE.indiv.avg$leaf0.str <- DE.indiv.avg$upper.den.leaf0 / DE.indiv.avg$leaf0.stdn
DE.indiv.avg$leaf2.str <- DE.indiv.avg$upper.den.leaf2 / DE.indiv.avg$leaf2.stdn
head(DE.indiv.avg) 
dim(DE.indiv.avg)


bar.sr.sabeto_gather<-gather(DE.indiv.avg, key ="leaf_no", value="str", 13:15)

bar.sr.sabeto_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")



sr.prevspost_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")
sr.sabeto_bargraph_prevspost<-ggplot(sr.prevspost_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(sr.sabeto_bargraph_prevspost)

# filter out na's
#sr.Sabeto_subset <- subset(sr.prevspost_gather, !(ind_ID %in% c("Ind3", "Ind4", "Ind6", "Ind7")))
sr.sabeto_subset_bargraph_prevspost<-ggplot(sr.prevspost_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

###error bars and t-test
sr.prevspost_gather$leaf.number <- c(0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2)

sr.Sabeto_postfire2 <- sr.prevspost_gather %>% group_by(leaf.number) %>% summarise(
  str.mean = mean(str, na.rm=T),
  str.sd = sd(str, na.rm=T),
  str.n = length(str[!is.na(str)]),
  str.se = str.sd/sqrt(str.n)
)
sr.Sabeto_postfire2$leaf.number[sr.Sabeto_postfire2$leaf.number==0] <- "leaf 0"
sr.Sabeto_postfire2$leaf.number[sr.Sabeto_postfire2$leaf.number==2] <- "leaf 2"

sr.Sabeto_postfire.sd.bar<-ggplot(sr.Sabeto_postfire2, aes(x=leaf.number, y=str.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=str.mean-str.se, ymax=str.mean+str.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal ratio",
       title="Stomatal ratio of S. etonia before and after fire")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Treatment"))+
  ylim(0,1)

 #these are my statistics
t.test(str~leaf_no, data=sr.prevspost_gather)

####### serrep

DE.indiv.avg.serrep$leaf0.str <- DE.indiv.avg.serrep$upper.den.leaf0 / DE.indiv.avg.serrep$leaf0.stdn
DE.indiv.avg.serrep$leaf2.str <- DE.indiv.avg.serrep$upper.den.leaf2 / DE.indiv.avg.serrep$leaf2.stdn
print(DE.indiv.avg.serrep)

#make a barplot

bar.sr.serrep_gather<-gather(DE.indiv.avg.serrep, key ="leaf_no", value="str", 11:13)

bar.sr.serrep_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")

sr.serrep_bargraph<-ggplot(bar.sr.serrep_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

sr.prevspost_gather.serrep$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind7","Ind8","Ind9","Ind10")
sr.serrep_bargraph_prevspost<-ggplot(sr.prevspost_gather.serrep, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(sr.serrep_bargraph_prevspost)

# filter out na's
sr.Serrep_subset <- subset(sr.prevspost_gather.serrep, !(ind_ID %in% c("Ind2", "Ind3", "Ind5")))
sr.serrep_subset_bargraph_prevspost<-ggplot(sr.Serrep_subset, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

###error bars and t-test

sr.Serrep_subset$leaf.number <- c(0,0,0,0,0,0,0,2,2,2,2,2,2,2)

sr.serrep_postfire2 <- sr.Serrep_subset %>% group_by(leaf.number) %>% summarise(
  str.mean = mean(str, na.rm=T),
  str.sd = sd(str, na.rm=T),
  str.n = length(str[!is.na(str)]),
  str.se = str.sd/sqrt(str.n)
)
sr.serrep_postfire2$leaf.number[sr.serrep_postfire2$leaf.number==0] <- "leaf 0"
sr.serrep_postfire2$leaf.number[sr.serrep_postfire2$leaf.number==2] <- "leaf 2"

sr.serrep_postfire.sd.bar<-ggplot(sr.serrep_postfire2, aes(x=leaf.number, y=str.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=str.mean-str.se, ymax=str.mean+str.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal ratio",
       title="Stomatal ratio of S. repens before and after fire")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before fire", "After fire"))+ guides(fill = guide_legend(title = "Treatment"))+
  ylim(0,1)



#these are my statistics
t.test(str~leaf_no, data=sr.Serrep_subset)


##################linear model****************

#sabeto
sr.m.sabeto.sd <-lmer(str~leaf.number+(1|individual),data=sr.prevspost_gather)
summary(sr.m.sabeto.sd)
Anova(sr.m.sabeto.sd)
hist(resid(sr.m.sabeto.sd))

#serrep
sr.m.serrep.sd <-lmer(str~leaf.number+(1|individual),data=sr.Serrep_subset)
summary(sr.m.serrep.sd)
Anova(sr.m.serrep.sd)
hist(resid(sr.m.serrep.sd))


