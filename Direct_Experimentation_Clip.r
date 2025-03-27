library(readxl)
library (ggplot2)
library(tidyr)
library(tidyverse)
DEC.dat.updated<-as.data.frame(read_excel("C:/Users/spunk/Downloads/Data from the direct experiment.xlsx"))

sabeto.DEC.dat <- subset(DEC.dat.updated, species == "sabeto")
sabeto.DEC.dat.burn <- subset(sabeto.DEC.dat, treatment == "clip" )

sabeto.DEC.dat.burn$upper.count.leaf2<- as.numeric(as.character(sabeto.DEC.dat.burn$upper.count.leaf2))
sabeto.DEC.dat.burn$lower.count.leaf2<- as.numeric(as.character(sabeto.DEC.dat.burn$lower.count.leaf2))

DEC.indiv.avg<-with(sabeto.DEC.dat.burn , aggregate(list(upper.count.leaf0=upper.count.leaf0, lower.count.leaf0=lower.count.leaf0, upper.count.leaf2=upper.count.leaf2, lower.count.leaf2=lower.count.leaf2), list(treatment=treatment, species=species, individual=individual), mean, na.rm=T))
dim(DEC.indiv.avg)
head(DEC.indiv.avg)

#convert counts to density

DEC.indiv.avg$upper.den.leaf0 <- DEC.indiv.avg$upper.count.leaf0/0.1644
DEC.indiv.avg$upper.den.leaf2 <- DEC.indiv.avg$upper.count.leaf2/0.1644
DEC.indiv.avg$lower.den.leaf0 <- DEC.indiv.avg$lower.count.leaf0/0.1644
DEC.indiv.avg$lower.den.leaf2 <- DEC.indiv.avg$lower.count.leaf2/0.1644

#calculate stomatal density for each one

DEC.indiv.avg$leaf0.stdn <- DEC.indiv.avg$upper.den.leaf0 + DEC.indiv.avg$lower.den.leaf0
DEC.indiv.avg$leaf2.stdn <- DEC.indiv.avg$upper.den.leaf2 + DEC.indiv.avg$lower.den.leaf2
print(DEC.indiv.avg)


#make a barplot

bar.DEC.sabeto_gather<-gather(DEC.indiv.avg, key ="leaf_no", value="stdn", 10:12)

bar.DEC.sabeto_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")

C.sabeto_bargraph<-ggplot(bar.DEC.sabeto_gather, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

print (DEC.indiv.avg)
C.prevspost_sabeto<-DEC.indiv.avg[ , c('individual', 'leaf0.stdn', 'leaf2.stdn')]
print(C.prevspost_sabeto)
C.prevspost_gather<-gather(C.prevspost_sabeto, key ="leaf_no", value="stdn", 2:3)

C.prevspost_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")
C.sabeto_bargraph_prevspost<-ggplot(C.prevspost_gather, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(C.sabeto_bargraph_prevspost)

# filter out na's
C.Sabeto_subset <- subset(C.prevspost_gather, !(ind_ID %in% c("Ind2","Ind6")))
C.sabeto_subset_bargraph_prevspost<-ggplot(C.Sabeto_subset, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")


### take averages 

C.Sabeto_subset$leaf.number <- c(0,0,0,0,2,2,2,2)
c.sabeto_postfire<-with(C.Sabeto_subset, aggregate(list(stdn=stdn), list(leaf.number=leaf.number),mean,na.rm=T))

c.sabeto_postfire2 <- C.Sabeto_subset %>% group_by(leaf.number) %>% summarise(
  stdn.mean = mean(stdn, na.rm=T),
  stdn.sd = sd(stdn, na.rm=T),
  stdn.n = length(stdn[!is.na(stdn)]),
  stdn.se = stdn.sd/sqrt(stdn.n)
)
c.sabeto_postfire2$leaf.number[c.sabeto_postfire2$leaf.number==0] <- "leaf 0"
c.sabeto_postfire2$leaf.number[c.sabeto_postfire2$leaf.number==2] <- "leaf 2"

c.sabeto_postfire.sd.bar<-ggplot(c.sabeto_postfire2, aes(x=leaf.number, y=stdn.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal density",
       title="Stomatal Density of S. etonia before and after clip")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf"))

#this is the averaged bar graph
c.sabeto_postfire.sd.bar

t.test(stdn~leaf_no, data=C.Sabeto_subset)

############ repeat for serrep


serrep.DEC.dat <- subset(DEC.dat.updated, species == "serrep")
serrep.DEC.dat.burn <- subset(serrep.DEC.dat, treatment == "clip" )

serrep.DEC.dat.burn$upper.count.leaf2<- as.numeric(as.character(serrep.DEC.dat.burn$upper.count.leaf2))
serrep.DEC.dat.burn$lower.count.leaf2<- as.numeric(as.character(serrep.DEC.dat.burn$lower.count.leaf2))

dim(c.serrep.DE.dat)
c.DE.indiv.avg.serrep<-with(serrep.DEC.dat.burn, aggregate(list(upper.count.leaf0=upper.count.leaf0, lower.count.leaf0=lower.count.leaf0, upper.count.leaf2=upper.count.leaf2, lower.count.leaf2=lower.count.leaf2), list(treatment=treatment, species=species, individual=individual), mean, na.rm=T))
dim(c.DE.indiv.avg.serrep)
head(c.DE.indiv.avg.serrep)

c.DE.indiv.avg.serrep$upper.den.leaf0 <- c.DE.indiv.avg.serrep$upper.count.leaf0/0.1644
c.DE.indiv.avg.serrep$upper.den.leaf2 <- c.DE.indiv.avg.serrep$upper.count.leaf2/0.1644
c.DE.indiv.avg.serrep$lower.den.leaf0 <- c.DE.indiv.avg.serrep$lower.count.leaf0/0.1644
c.DE.indiv.avg.serrep$lower.den.leaf2 <- c.DE.indiv.avg.serrep$lower.count.leaf2/0.1644

#calculate stomatal density for each one

c.DE.indiv.avg.serrep$leaf0.stdn <- c.DE.indiv.avg.serrep$upper.den.leaf0 + c.DE.indiv.avg.serrep$lower.den.leaf0
c.DE.indiv.avg.serrep$leaf2.stdn <- c.DE.indiv.avg.serrep$upper.den.leaf2 + c.DE.indiv.avg.serrep$lower.den.leaf2
print(c.DE.indiv.avg.serrep)


#make a barplot

c.bar.DE.serrep_gather<-gather(c.DE.indiv.avg.serrep, key ="leaf_no", value="stdn", 10:11)

c.DE.indiv.avg.serrep$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")

c.serrep_bargraph<-ggplot(c.DE.indiv.avg.serrep, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

print (c.DE.indiv.avg.serrep)
c.prevspost_serrep<-c.DE.indiv.avg.serrep[ , c('individual', 'leaf0.stdn', 'leaf2.stdn')]
print(c.prevspost_serrep)
c.prevspost_gather_serrep<-gather(c.prevspost_serrep, key ="leaf_no", value="stdn", 2:3)

c.prevspost_gather_serrep$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")
c.serrep_bargraph_prevspost<-ggplot(c.prevspost_gather_serrep, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(c.serrep_bargraph_prevspost)

# filter out na's
c.serrep_subset <- subset(c.prevspost_gather_serrep, !(ind_ID %in% c("Ind6","Ind7")))
c.serrep_subset_bargraph_prevspost<-ggplot(c.serrep_subset, aes(x=ind_ID, y=stdn)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

c.serrep_subset$leaf.number <- c(0,0,0,0,0,2,2,2,2,2)

c.serrep_postfire2 <- c.serrep_subset %>% group_by(leaf.number) %>% summarise(
  stdn.mean = mean(stdn, na.rm=T),
  stdn.sd = sd(stdn, na.rm=T),
  stdn.n = length(stdn[!is.na(stdn)]),
  stdn.se = stdn.sd/sqrt(stdn.n)
)
c.serrep_postfire2$leaf.number[c.serrep_postfire2$leaf.number==0] <- "leaf 0"
c.serrep_postfire2$leaf.number[c.serrep_postfire2$leaf.number==2] <- "leaf 2"

c.serrep_postfire.sd.bar<-ggplot(c.serrep_postfire2, aes(x=leaf.number, y=stdn.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=stdn.mean-stdn.se, ymax=stdn.mean+stdn.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal density",
       title="Stomatal Density of S. repens before and after clip")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf Number"))

print(c.serrep_postfire.sd.bar)

t.test(stdn~leaf_no, data=c.serrep_subset)

############linear models##############

#sabeto
c.m.sabeto.sd <-lmer(stdn~leaf.number+(1|individual),data=C.Sabeto_subset)
summary(c.m.sabeto.sd)
Anova(c.m.sabeto.sd)
hist(resid(c.m.sabeto.sd))

#serrep
c.m.serrep.sd <-lmer(stdn~leaf.number+(1|individual),data=c.serrep_subset)
summary(c.m.serrep.sd)
Anova(c.m.serrep.sd)
hist(resid(c.m.serrep.sd))

#############################stomatal ratio############################################

DEC.indiv.avg$leaf0.str <- DEC.indiv.avg$upper.den.leaf0 / DEC.indiv.avg$leaf0.stdn
DEC.indiv.avg$leaf2.str <- DEC.indiv.avg$upper.den.leaf2 / DEC.indiv.avg$leaf2.stdn
head(DEC.indiv.avg)

#make a barplot

c.bar.sr.sabeto_gather<-gather(DEC.indiv.avg, key ="leaf_no", value="str", 13:15)

c.bar.sr.sabeto_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")

c.sr.sabeto_bargraph<-ggplot(c.bar.sr.sabeto_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

print (DEC.indiv.avg)
c.sr.prevspost_sabeto<-DEC.indiv.avg[ , c('individual', 'leaf0.str', 'leaf2.str')]
print(c.sr.prevspost_sabeto)
c.sr.prevspost_gather<-gather(c.sr.prevspost_sabeto, key ="leaf_no", value="str", 2:3)

c.sr.prevspost_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")
c.sr.sabeto_bargraph_prevspost<-ggplot(c.sr.prevspost_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(c.sr.sabeto_bargraph_prevspost)

# filter out na's
c.sr.Sabeto_subset <- subset(c.sr.prevspost_gather, !(ind_ID %in% c("Ind2", "Ind6")))
c.sr.sabeto_subset_bargraph_prevspost<-ggplot(c.sr.Sabeto_subset, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

#take average first

c.sr.Sabeto_subset$leaf.number <- c(0,0,0,0,2,2,2,2)

c.sr.Sabeto_postfire2 <- c.sr.Sabeto_subset %>% group_by(leaf.number) %>% summarise(
  str.mean = mean(str, na.rm=T),
  str.sd = sd(str, na.rm=T),
  str.n = length(str[!is.na(str)]),
  str.se = str.sd/sqrt(str.n)
)
c.sr.Sabeto_postfire2$leaf.number[c.sr.Sabeto_postfire2$leaf.number==0] <- "leaf 0"
c.sr.Sabeto_postfire2$leaf.number[c.sr.Sabeto_postfire2$leaf.number==2] <- "leaf 2"

c.sr.Sabeto_postfire.sd.bar<-ggplot(c.sr.Sabeto_postfire2, aes(x=leaf.number, y=str.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=str.mean-str.se, ymax=str.mean+str.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal ratio",
       title="Stomatal ratio of S. etonia before and after clip")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf Number"))

#final bar graph
print(c.sr.Sabeto_postfire.sd.bar)

t.test(str~leaf_no, data=c.sr.Sabeto_subset)


####### serrep

c.DE.indiv.avg.serrep$leaf0.str <- c.DE.indiv.avg.serrep$upper.den.leaf0 / c.DE.indiv.avg.serrep$leaf0.stdn
c.DE.indiv.avg.serrep$leaf2.str <- c.DE.indiv.avg.serrep$upper.den.leaf2 / c.DE.indiv.avg.serrep$leaf2.stdn
print(c.DE.indiv.avg.serrep)

#make a barplot
c.bar.sr.serrep_gather<-gather(c.DE.indiv.avg.serrep, key ="leaf_no", value="str", 13:15)

c.bar.sr.serrep_gather$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")

c.sr.serrep_bargraph<-ggplot(c.bar.sr.serrep_gather, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

#filter out leaf 1
print (c.DE.indiv.avg.serrep)
c.sr.prevspost_serrep<-c.DE.indiv.avg.serrep[ , c('individual', 'leaf0.str', 'leaf2.str')]
print(c.sr.prevspost_serrep)
c.sr.prevspost_gather.serrep<-gather(c.sr.prevspost_serrep, key ="leaf_no", value="str", 2:3)

c.sr.prevspost_gather.serrep$ind_ID<-c("Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind_1","Ind2","Ind3","Ind4","Ind5","Ind6")
c.sr.serrep_bargraph_prevspost<-ggplot(c.sr.prevspost_gather.serrep, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")
print(c.sr.serrep_bargraph_prevspost)

# filter out na's
c.sr.Serrep_subset <- subset(c.sr.prevspost_gather.serrep, !(ind_ID %in% c("Ind6", "Ind7")))
c.sr.serrep_subset_bargraph_prevspost<-ggplot(c.sr.Serrep_subset, aes(x=ind_ID, y=str)) +geom_bar(aes( fill=leaf_no), position="dodge",stat="identity")

###take averages
c.sr.Serrep_subset$leaf.number <- c(0,0,0,0,0,2,2,2,2,2)

c.sr.serrep_postfire2 <- c.sr.Serrep_subset %>% group_by(leaf.number) %>% summarise(
  str.mean = mean(str, na.rm=T),
  str.sd = sd(str, na.rm=T),
  str.n = length(str[!is.na(str)]),
  str.se = str.sd/sqrt(str.n)
)
c.sr.serrep_postfire2$leaf.number[c.sr.serrep_postfire2$leaf.number==0] <- "leaf 0"
c.sr.serrep_postfire2$leaf.number[c.sr.serrep_postfire2$leaf.number==2] <- "leaf 1"

c.sr.serrep_postfire.sd.bar<-ggplot(c.sr.serrep_postfire2, aes(x=leaf.number, y=str.mean))+ 
  geom_bar(aes(fill=leaf.number), position="dodge",stat="identity")+
  geom_errorbar(aes(x=leaf.number, ymin=str.mean-str.se, ymax=str.mean+str.se), width=0.2)+
  labs(x="Leaf relation to fire",y="stomatal ratio",
       title="Stomatal ratio of S. repens before and after clip")+scale_fill_manual(values=c("#CCCCCC",  "#666666"),labels = c("Before", "After"))+ guides(fill = guide_legend(title = "Leaf Number"))

t.test(str~leaf_no, data=c.sr.Serrep_subset)

##################linear model****************

plot(c.sr.Serrep_subset$leaf.number, c.sr.Serrep_subset$str)
#sabeto
c.sr.m.sabeto.sd <-lmer(str~leaf.number+(1|individual),data=c.sr.Sabeto_subset)
summary(c.sr.m.sabeto.sd)
Anova(c.sr.m.sabeto.sd)
hist(resid(c.sr.m.sabeto.sd))

#serrep
c.sr.m.serrep.sd <-lmer(str~leaf.number+(1|individual),data=c.sr.Serrep_subset)
summary(sr.m.serrep.sd)
Anova(sr.m.serrep.sd)
hist(resid(sr.m.serrep.sd))