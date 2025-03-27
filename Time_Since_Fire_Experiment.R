library(readxl)
library(lme4)
library(car)
library(mgcv)
library(readxl)
library (ggplot2)
library(tidyr)
library(tidyverse)
library(cowplot)
library(scales)
TSF.dat<-as.data.frame(read_csv("C:/Users/spunk/Downloads/Raw data from the Time Since Fire experiment.csv"))
head(TSF.dat)
dim(TSF.dat)

#calculate Stomatal Density
TSF.dat$upper.sd<-TSF.dat$upper.count/0.1644
TSF.dat$lower.sd<-TSF.dat$lower.count/0.1644
TSF.dat$tot.sd<-TSF.dat$lower.sd+TSF.dat$upper.sd
head(TSF.dat)
dim(TSF.dat)


#Convert to wide form
TSF.dat.wide<-reshape(TSF.dat[c("years.since.fire","unit","species","individual","leaf.age","replicate","upper.count", "lower.count", "tot.sd")],idvar=c("species","individual","replicate","unit"),v.names =c("upper.count","lower.count"),timevar="leaf.age",direction="wide")
head(TSF.dat.wide)


#calculate the total SD for each replicate
TSF.dat.wide$new.tot<-TSF.dat.wide$upper.count.new+TSF.dat.wide$lower.count.new
TSF.dat.wide$old.tot<-TSF.dat.wide$upper.count.old+TSF.dat.wide$lower.count.old
head(TSF.dat.wide)
dim(TSF.dat.wide)

#convert SD count to SD real
TSF.dat.wide$new.tot.num<-TSF.dat.wide$new.tot/0.1644
TSF.dat.wide$old.tot.num<-TSF.dat.wide$old.tot/0.1644
head(TSF.dat.wide)

#average the two replicates for each individual 
TSF.indiv.avg<-with(TSF.dat.wide, aggregate(list(new.tot=new.tot, old.tot=old.tot, tot.sd=tot.sd, upper.count.new=upper.count.new, lower.count.new=lower.count.new, new.tot.num=new.tot.num, old.tot.num= old.tot.num ), list(years.since.fire=years.since.fire, species=species, unit=unit, individual=individual), mean, na.rm=T))
head(TSF.indiv.avg)
dim(TSF.indiv.avg)

#subtract new indiv avg from old indiv avg 
TSF.indiv.avg$sd.indiv.diff<-TSF.indiv.avg$old.tot.num-TSF.indiv.avg$new.tot.num
TSF.indiv.avg$sd.indiv.diff.n <-TSF.indiv.avg$sd.indiv.diff*-1
head(TSF.indiv.avg)

#split into serrep and sabeto
sabeto.indiv.avg <- subset(TSF.indiv.avg, species == "sabeto")
dim(sabeto.indiv.avg)
head(sabeto.indiv.avg)
serrep.indiv.avg <- subset(TSF.indiv.avg, species == "serrep")
head((serrep.indiv.avg))
dim(serrep.indiv.avg)


#take the avg for each site 
serrep_unit_avg<-with(serrep.indiv.avg, aggregate(list(years.since.fire=years.since.fire,sd.indiv.diff.n=sd.indiv.diff.n), list(unit=unit),mean,na.rm=T))
print(serrep_unit_avg)
sabeto_unit_avg<-with(sabeto.indiv.avg, aggregate(list(years.since.fire=years.since.fire,sd.indiv.diff.n=sd.indiv.diff.n), list(unit=unit),mean,na.rm=T))
print(serrep_unit_avg)
dim(sabeto_unit_avg)
print(sabeto_unit_avg)

################### sd indiv diff stats
serrep.indiv.diff.stat<- serrep.indiv.avg %>% group_by(unit) %>% summarise(
  stdn.mean.tsf.serrep.diff = mean(sd.indiv.diff.n, na.rm=T),
  stdn.sd.tsf.serrep.diff = sd(sd.indiv.diff.n, na.rm=T),
  stdn.n.tsf.serrep.diff = length(sd.indiv.diff.n[!is.na(sd.indiv.diff.n)]),
  stdn.se.tsf.serrep.diff = stdn.sd.tsf.serrep.diff/sqrt(stdn.n.tsf.serrep.diff)
)

sabeto.indiv.diff.stat<- sabeto.indiv.avg %>% group_by(unit) %>% summarise(
  stdn.mean.tsf.sabeto.diff = mean(sd.indiv.diff.n, na.rm=T),
  stdn.sd.tsf.sabeto.diff = sd(sd.indiv.diff.n, na.rm=T),
  stdn.n.tsf.sabeto.diff = length(sd.indiv.diff.n[!is.na(sd.indiv.diff.n)]),
  stdn.se.tsf.sabeto.diff = stdn.sd.tsf.sabeto.diff/sqrt(stdn.n.tsf.sabeto.diff)
)


#######some of the panels in figure 4


#manually add jitter

serrep_unit_avg$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )

sabeto_unit_avg$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )


"serrep_stomatal.ratio.diff.item" <- serrep_unit_avg$sd.indiv.diff.n

serrep_sd_diff <- ggplot(serrep_unit_avg, aes(x = manualjitter, y = sd.indiv.diff.n)) +
  geom_point(color = "black", size = 1.8) +  # Equivalent to pch=19 and col="#330033"
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=serrep_stomatal.ratio.diff.item-serrep.indiv.diff.stat$stdn.se.tsf.serrep.diff, ymax=serrep_stomatal.ratio.diff.item+serrep.indiv.diff.stat$stdn.se.tsf.serrep.diff), width=0.05)+ # Log-transform x-axis (log="x" in base R)
  ylim(-250, 250) +  # Set y-axis limits
  labs(
    x = "Time-since-fire (yr)",
    y = element_blank(),
    title = element_blank()
  ) +
  theme_classic()+geom_hline(yintercept=0)+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))


"sabeto_stomatal.ratio.diff.item" <- sabeto_unit_avg$sd.indiv.diff.n

sabeto_sd_diff <- ggplot(sabeto_unit_avg, aes(x = manualjitter, y = sd.indiv.diff.n)) +
  geom_point(color = "black", size = 1.8) +  # Equivalent to pch=19 and col="#330033"
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=sabeto_stomatal.ratio.diff.item-sabeto.indiv.diff.stat$stdn.se.tsf.sabeto.diff, ymax=sabeto_stomatal.ratio.diff.item+sabeto.indiv.diff.stat$stdn.se.tsf.sabeto.diff), width=0.05)+ # Log-transform x-axis (log="x" in base R)
  ylim(-250, 250) +  # Set y-axis limits
  labs(
    x = "Time-since-fire (yr)",
    y = "Diff. in Stomatal Density",
    title = element_blank()
  ) +
  theme_classic()+geom_hline(yintercept=0)+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))


#statistics
m.sd.sabeto <-lmer(sd.indiv.diff~years.since.fire+(1|unit),data=TSF.indiv.avg[TSF.indiv.avg$species=="sabeto",])
summary(m.sd.sabeto)
Anova(m.sd.sabeto)
hist(resid(m.sd.sabeto))

m.sd.serrep <-lmer(sd.indiv.diff~years.since.fire+(1|unit),data=TSF.indiv.avg[TSF.indiv.avg$species=="serrep",])
summary(m.sd.serrep)
Anova(m.sd.serrep)
hist(resid(m.sd.serrep))



##########calculate stomatal ratio (= Upper/ upper+lower)



TSF.dat.wide$stomatal.ratio.new<-TSF.dat.wide$upper.count.new/TSF.dat.wide$new.tot *100
TSF.dat.wide$stomatal.ratio.old<-TSF.dat.wide$upper.count.old/TSF.dat.wide$old.tot *100

TSF.dat.wide$stomatal.ratio.new.dec<-TSF.dat.wide$upper.count.new/TSF.dat.wide$new.tot 
TSF.dat.wide$stomatal.ratio.old.dec<-TSF.dat.wide$upper.count.old/TSF.dat.wide$old.tot 

head(TSF.dat.wide)

#take the average per individual
TSF.str.indiv.avg<-with(TSF.dat.wide, aggregate(list(stomatal.ratio.new=stomatal.ratio.new, stomatal.ratio.old=stomatal.ratio.old, upper.count.new=upper.count.new, lower.count.new=lower.count.new), list(years.since.fire=years.since.fire, species=species, unit=unit, individual=individual), mean, na.rm=T))
head(TSF.str.indiv.avg)
dim(TSF.str.indiv.avg)

#subtract new indiv avg from old indiv avg 
TSF.str.indiv.avg$str.indiv.diff<-TSF.str.indiv.avg$stomatal.ratio.old-TSF.str.indiv.avg$stomatal.ratio.new
TSF.str.indiv.avg$str.indiv.diff.n<-TSF.str.indiv.avg$str.indiv.diff*-1

head(TSF.str.indiv.avg)

#split into serrep and sabeto
sabeto.str.indiv.avg <- subset(TSF.str.indiv.avg, species == "sabeto")
head(sabeto.str.indiv.avg)
serrep.str.indiv.avg <- subset(TSF.str.indiv.avg, species == "serrep")
head((serrep.str.indiv.avg))

# take the avg for each site
serrep_unit_str_avg<-with(serrep.str.indiv.avg, aggregate(list(years.since.fire=years.since.fire,str.indiv.diff.n=str.indiv.diff.n), list(unit=unit),mean,na.rm=T))

print(serrep_unit_str_avg)
sabeto_unit_str_avg<-with(sabeto.str.indiv.avg, aggregate(list(years.since.fire=years.since.fire,str.indiv.diff.n=str.indiv.diff.n), list(unit=unit),mean,na.rm=T))
dim(sabeto_unit_str_avg)
print(sabeto_unit_str_avg)

#now graph sd.indiv.diff vs years.since.fire for each sp

#stats
serrep.indiv.diff.stat.str<- serrep.str.indiv.avg %>% group_by(unit) %>% summarise(
  str.mean.tsf.serrep.diff = mean(str.indiv.diff.n, na.rm=T),
  str.sd.tsf.serrep.diff = sd(str.indiv.diff.n, na.rm=T),
  str.n.tsf.serrep.diff = length(str.indiv.diff.n[!is.na(str.indiv.diff.n)]),
  str.se.tsf.serrep.diff = str.sd.tsf.serrep.diff/sqrt(str.n.tsf.serrep.diff)
)

sabeto.indiv.diff.stat.str<- sabeto.str.indiv.avg %>% group_by(unit) %>% summarise(
  str.mean.tsf.sabeto.diff = mean(str.indiv.diff.n, na.rm=T),
  str.sd.tsf.sabeto.diff = sd(str.indiv.diff.n, na.rm=T),
  str.n.tsf.sabeto.diff = length(str.indiv.diff.n[!is.na(str.indiv.diff.n)]),
  str.se.tsf.sabeto.diff = str.sd.tsf.sabeto.diff/sqrt(str.n.tsf.sabeto.diff)
)


###make serrep stomatal ratio indiv diff figure
#manually add jitter

serrep_unit_str_avg$manualjitter <- c(14.00, 55.00, 0.27, 0.21, 2.15, 1.85, 1.10, 33.00, 1.00, 3.50, 0.90, 0.25 )

sabeto_unit_str_avg$manualjitter <- c(14.00, 55.00, 0.27, 0.21, 2.15, 1.85, 1.10, 33.00, 1.00, 3.50, 0.90, 0.25 )



"serrep_stomatal.den.diff.item" <- serrep_unit_str_avg$str.indiv.diff.n

serrep_str_diff <- ggplot(serrep_unit_str_avg, aes(x = manualjitter, y = str.indiv.diff.n)) +
  geom_point(color = "black", size = 1.8) +  # Equivalent to pch=19 and col="#330033"
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=serrep_stomatal.den.diff.item-serrep.indiv.diff.stat.str$str.se.tsf.serrep.diff, ymax=serrep_stomatal.den.diff.item+serrep.indiv.diff.stat.str$str.se.tsf.serrep.diff), width=0.05)+ 
  ylim(-2.5, 4) +  
  labs(
    x = element_blank(),
    y = element_blank(),
    title = expression(italic("Serenoa repens"))
  ) +
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+geom_hline(yintercept=0)+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

###make sabeto stomatal ratio indiv diff figure

"sabeto_stomatal.den.diff.item" <- sabeto_unit_str_avg$str.indiv.diff.n

sabeto_str_diff <- ggplot(sabeto_unit_str_avg, aes(x = manualjitter, y = str.indiv.diff.n)) +
  geom_point(color = "black", size = 1.8) +  # Equivalent to pch=19 and col="#330033"
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=sabeto_stomatal.den.diff.item-sabeto.indiv.diff.stat.str$str.se.tsf.sabeto.diff, ymax=sabeto_stomatal.den.diff.item+sabeto.indiv.diff.stat.str$str.se.tsf.sabeto.diff), width=0.05)+ # Log-transform x-axis (log="x" in base R)
  ylim(-2.5, 4) +  # Set y-axis limits
  labs(
    x = element_blank(),
    y = "% Diff. in Stomatal Ratio",
    title = expression(italic("Sabal etonia"))
  ) +
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+geom_hline(yintercept=0)+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

####This is my figure four, 4 panel figure###
plot_grid(sabeto_str_diff,serrep_str_diff ,sabeto_sd_diff ,serrep_sd_diff, labels="AUTO")


#anaysis

#m<-lmer(str.indiv.diff~years.since.fire+(1|unit),data=TSF.str.indiv.avg[TSF.str.indiv.avg$species=="sabeto",])

#m<-lmer(stomatal.ratio.new ~ years.since.fire+(1|unit), data=serrep.str.indiv.avg)
summary(m)
Anova(m)


#############################new leaf across sites###################################

serrep_unit_avg.sd<-with(serrep.indiv.avg, aggregate(list(years.since.fire=years.since.fire,sd.indiv.diff=sd.indiv.diff, new.tot.num=new.tot.num), list(unit=unit),mean,na.rm=T))
print(serrep_unit_avg.sd)

sabeto_unit_avg.sd<-with(sabeto.indiv.avg, aggregate(list(years.since.fire=years.since.fire,sd.indiv.diff=sd.indiv.diff, new.tot.num=new.tot.num), list(unit=unit),mean,na.rm=T))

#stats
serrep.new.leaf.stat<- serrep.indiv.avg %>% group_by(unit) %>% summarise(
  stdn.mean.tsf.serrep = mean(new.tot.num, na.rm=T),
  stdn.sd.tsf.serrep = sd(new.tot.num, na.rm=T),
  stdn.n.tsf.serrep = length(new.tot.num[!is.na(new.tot.num)]),
  stdn.se.tsf.serrep = stdn.sd.tsf.serrep/sqrt(stdn.n.tsf.serrep)
)

sabeto.new.leaf.stat<- sabeto.indiv.avg %>% group_by(unit) %>% summarise(
  stdn.mean.tsf.sabeto = mean(new.tot.num, na.rm=T),
  stdn.sd.tsf.sabeto = sd(new.tot.num, na.rm=T),
  stdn.n.tsf.sabeto = length(new.tot.num[!is.na(new.tot.num)]),
  stdn.se.tsf.sabeto = stdn.sd.tsf.sabeto/sqrt(stdn.n.tsf.sabeto)
)

#manually add jitter

serrep_unit_avg.sd$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )

sabeto_unit_avg.sd$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )

serrep.new.leaf.stat<- serrep.indiv.avg %>% group_by(unit) %>% summarise(
  stdn.mean.tsf.serrep = mean(new.tot.num, na.rm=T),
  stdn.sd.tsf.serrep = sd(new.tot.num, na.rm=T),
  stdn.n.tsf.serrep = length(new.tot.num[!is.na(new.tot.num)]),
  stdn.se.tsf.serrep = stdn.sd.tsf.serrep/sqrt(stdn.n.tsf.serrep)
)

#make serrep density new leaf figure 

serrep_density_new <- ggplot(serrep_unit_avg.sd, aes(x = manualjitter, y = new.tot.num)) +
  geom_point(color = "#330033", size = 1.8) + 
  geom_errorbar(aes(x=manualjitter, ymin=serrep_unit_avg.sd$new.tot.num-serrep.new.leaf.stat$stdn.se.tsf.serrep, ymax=serrep_unit_avg.sd$new.tot.num+serrep.new.leaf.stat$stdn.se.tsf.serrep),  width=0.05)+
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  # Set x-axis to logarithmic scale
  labs(x = "Time-since-fire (yr)", 
       y = element_blank(), 
       title = element_blank()) +
  ylim(800, 1300) +  # Set y-axis limits
  theme_classic() +theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

#make sabeto density new leaf figure 

Sabeto_density_new <- ggplot(sabeto_unit_avg.sd, aes(x = manualjitter, y = new.tot.num)) +
  geom_point(color = "#330033", size = 1.8) + 
  geom_errorbar(aes(x=manualjitter, ymin=sabeto_unit_avg.sd$new.tot.num-sabeto.new.leaf.stat$stdn.se.tsf.sabeto, ymax=sabeto_unit_avg.sd$new.tot.num+sabeto.new.leaf.stat$stdn.se.tsf.sabeto),  width=0.05)+
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  # Set x-axis to logarithmic scale
  labs(x = "Time-since-fire (yr)", 
       y=expression ("Stomatal Density per mm"^2),
       title = element_blank()) +
  ylim(800, 1300) + theme(panel.border = element_rect(color = "black",fill = NA, linewidth = 1))+ # Set y-axis limits
  theme_classic()  +theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))


#statistics
m.sd.sabeto.NEWS <-lmer(new.tot.num~years.since.fire+(1|unit),data=TSF.indiv.avg[TSF.indiv.avg$species=="sabeto",])
summary(m.sd.sabeto.NEWS)
Anova(m.sd.sabeto.NEWS)
hist(resid(m.sd.sabeto.NEWS))


m.sd.sabeto.NEWS.early <-lmer(new.tot.num~years.since.fire+(1|unit),data=early.sd.indiv.avg[TSF.indiv.avg$species=="sabeto",])
summary(m.sd.sabeto.NEWS.early)
Anova(m.sd.sabeto.NEWS.early)
hist(resid(m.sd.sabeto.NEWS.early))

m.sd.serrep.NEWS <-lmer(new.tot.num~years.since.fire+(1|unit),data=TSF.indiv.avg[TSF.indiv.avg$species=="serrep",])
summary(m.sd.serrep.NEWS)
Anova(m.sd.serrep.NEWS)
hist(resid(m.sd.serrep.NEWS))

##### stomatal ratio

serrep_unit_avg.str<-with(serrep.str.indiv.avg, aggregate(list(years.since.fire=years.since.fire, stomatal.ratio.new=stomatal.ratio.new), list(unit=unit),mean,na.rm=T))
print(serrep_unit_avg.str)

sabeto_unit_avg.str<-with(sabeto.str.indiv.avg, aggregate(list(years.since.fire=years.since.fire, stomatal.ratio.new=stomatal.ratio.new), list(unit=unit),mean,na.rm=T))

TSF.dat.wide

#stats
serrep.new.leaf.stat.str<- serrep.str.indiv.avg %>% group_by(unit) %>% summarise(
  str.mean.tsf.serrep = mean(stomatal.ratio.new, na.rm=T),
  str.sd.tsf.serrep = sd(stomatal.ratio.new, na.rm=T),
  str.n.tsf.serrep = length(stomatal.ratio.new[!is.na(stomatal.ratio.new)]),
  str.se.tsf.serrep = str.sd.tsf.serrep/sqrt(str.n.tsf.serrep)
)

sabeto.new.leaf.stat.str<- sabeto.str.indiv.avg %>% group_by(unit) %>% summarise(
  str.mean.tsf.sabeto = mean(stomatal.ratio.new, na.rm=T),
  str.sd.tsf.sabeto = sd(stomatal.ratio.new, na.rm=T),
  str.n.tsf.sabeto = length(stomatal.ratio.new[!is.na(stomatal.ratio.new)]),
  str.se.tsf.sabeto = str.sd.tsf.sabeto/sqrt(str.n.tsf.sabeto)
)


#manually add jitter

serrep_unit_avg.str$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )

sabeto_unit_avg.str$manualjitter <- c(13.00, 50.00, 0.27, 0.21, 2.15, 1.85, 1.10, 30.00, 1.00, 3.00, 0.90, 0.25 )

#manual jitter
"serrep_stomatal.ratio.new.item" <- serrep_unit_avg.str$stomatal.ratio.new

Serrep_ratio_new <-ggplot(serrep_unit_avg.str, aes(x = manualjitter, y = stomatal.ratio.new)) +
  geom_point(color = "#330033", size = 1.8) +  # Plot points in dark purple
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=serrep_stomatal.ratio.new.item-serrep.new.leaf.stat.str$str.se.tsf.serrep, ymax=serrep_stomatal.ratio.new.item+serrep.new.leaf.stat.str$str.se.tsf.serrep),  width=0.05)+ # Logarithmic scale for x-axis
  labs(x = element_blank(), 
       y = element_blank()) +ggtitle(expression(italic("Serenoa repens"))) +
  ylim(45, 55) +  # Set y-axis limits
  theme_classic() + theme(plot.title = element_text(hjust=0.5))+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

"sabeto_stomatal.ratio.new.item" <- sabeto_unit_avg.str$stomatal.ratio.new

Sabeto_ratio_new<-ggplot(sabeto_unit_avg.str, aes(x = manualjitter, y = stomatal.ratio.new)) +
  geom_point(color = "#330033", size = 1.8) +  # Plot points in dark purple
  scale_x_log10(breaks = c(0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 50.0)) +  geom_errorbar(aes(x=manualjitter, ymin=sabeto_stomatal.ratio.new.item-sabeto.new.leaf.stat.str$str.se.tsf.sabeto, ymax=sabeto_stomatal.ratio.new.item+sabeto.new.leaf.stat.str$str.se.tsf.sabeto),  width=0.05)+ # Logarithmic scale for x-axis
  labs(x = element_blank(), 
       y = "Stomatal ratio") +ggtitle(expression(italic("Sabal etonia"))) +theme(plot.title = element_text(size = 20,hjust = 0.5))+
  ylim(45, 55) +  # Set y-axis limits
  theme_classic()+ theme(plot.title = element_text(hjust=0.5))+theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size=10))

###This is my four panel figure
plot_grid(Sabeto_ratio_new,Serrep_ratio_new,Sabeto_density_new ,serrep_density_new, labels="AUTO")

#########linear Model#######
m.sr.sabeto <-lmer(str.indiv.diff~years.since.fire+(1|unit),data=TSF.str.indiv.avg[TSF.indiv.avg$species=="sabeto",])
summary(m.sr.sabeto)
Anova(m.sr.sabeto)
hist(resid(m.sr.sabeto))



m.sr.serrep <-lmer(str.indiv.diff~years.since.fire+(1|unit),data=TSF.str.indiv.avg[TSF.indiv.avg$species=="serrep",])
summary(m.sr.serrep)
Anova(m.sr.serrep)
hist(resid(m.sr.serrep))
