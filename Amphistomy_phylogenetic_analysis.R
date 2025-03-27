library(pez)
library(picante)
library(readxl)
library(tidyverse)
library(ggtree)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(janitor)
library(phylolm)
library(sensiPhy)

###### Remake the tree again for the analysis

tree.pre <- read.tree("C:/Users/spunk/Downloads/Vascular_Plants_rooted.dated.tre") # From Zanne, Amy E., et al. "Three keys to the radiation of angiosperms into freezing environments." Nature 506.7486 (2014): 89-92.
tree.pre$tip.label <- tolower(tree.pre$tip.label) # Make everything lowercase so they all match

#this is how to search the tree
tree.pre$tip.label[substr(tree.pre$tip.label,1,4)=="liat"] 

species.labels.semifinal<-as.data.frame(read_excel("C:/Users/spunk/Downloads/Phyllogenetic_comparative_1and2only (1).xlsx", sheet = "Sheet2"))


species.labels.semifinal$Species.lower<-as.character(species.labels.semifinal$`R analog`)
species.labels.semifinal$species.real<-as.character(paste(species.labels.semifinal$Genus,species.labels.semifinal$species,sep="_"))
species.labels.semifinal$species.labs<-as.character(species.labels.semifinal$species.real)

#species <- tolower(species.labels$tip.species) # Make everything lowercase so they all match
species.labels.semifinal[species.labels.semifinal$Species.lower %in% tree.pre$tip.label,]

genera<-sapply(strsplit(tree.pre$tip.label,"_"),"[[",1)
my.genera<-tolower(unique(sapply(strsplit(species.labels.semifinal$Species.lower,"_"),"[[",1)))
my.genera[my.genera %in% genera]
my.genera[!my.genera %in% genera]

species.labels.semifinal<-species.labels.semifinal[!is.na(species.labels.semifinal$`R analog`),]

tree <- congeneric.merge(tree.pre, unique(species.labels.semifinal$Species.lower)) # Assumes genus_species names; you can change this with an argument
tree <- drop.tip(tree, setdiff(tree$tip.label, species.labels.semifinal$Species.lower)) # Drop everything you don't need

species.labels.semifinal$tips<-factor(species.labels.semifinal$`R analog`,levels=tree$tip.label)
species.labels.semifinal<-species.labels.semifinal[order(species.labels.semifinal$tips),]

species.labels.semifinal$amp.col<-NA
species.labels.semifinal$amp.col[species.labels.semifinal$amphistomy_presence==0]<-"red"
species.labels.semifinal$amp.col[species.labels.semifinal$amphistomy_presence==1]<-"blue"


plot(tree,type="phylogram")
cophenetic.phylo(tree)
plot(tree)
col.tree<-tree
col.tree$tip.label<-species.labels.semifinal$species.real


plot(tree,cex = .66,tip.color = species.labels.semifinal$amp.col)

legend('topleft',fill=c("blue","red"),legend=c("Amphistomy","No Amphistomy"),bty='n')

box()

#### format df for analysis

just_traits <- species.labels.semifinal[c(15, 3, 11)]
fire.comm <- t(just_traits)

fire.comm.clean <- fire.comm %>%
  row_to_names(row_number = 1)

view(fire.comm.clean)
dim(fire.comm.clean)
t(fire.comm.clean)


#Tree = "col.tree"
#data = "fire.comm.cleam"

################begin pagels 1994 model analysis
class(fire.comm.clean)
fire.comm.df<-as.data.frame(fire.comm.clean)
tall.fire.comm<- t(fire.comm.df)
tall.fire.comm.df<-as.data.frame(tall.fire.comm)



a_p<-setNames(tall.fire.comm.df$amphistomy_presence,
                        rownames(tall.fire.comm.df))
rvs<-setNames(tall.fire.comm.df$`resprout vs seed`,
             rownames(tall.fire.comm.df))

#fit amphistomy and fire
amphifire.pagel<-fitPagel(col.tree,a_p,rvs)
fit.amphi<-fitPagel(col.tree,a_p,rvs,dep.var="x")
fit.amphi

fit.fire<-fitPagel(col.tree,a_p,rvs,dep.var="y")
fit.fire

#This is where I got my reported numbers from
anova(amphifire.pagel)
plot(amphifire.pagel,lwd.by.rate=TRUE)
