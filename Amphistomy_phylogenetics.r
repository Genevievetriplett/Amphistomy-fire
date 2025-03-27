
library(pez)
library(picante)
library(readxl)
library(tidyverse)
library(ggtree)


tree.pre <- read.tree("C:/Users/spunk/Downloads/Vascular_Plants_rooted.dated.tre") # From Zanne, Amy E., et al. "Three keys to the radiation of angiosperms into freezing environments." Nature 506.7486 (2014): 89-92.
tree.pre$tip.label <- tolower(tree.pre$tip.label) # Make everything lowercase so they all match

#this is how to search the tree ex. Liatris genus
tree.pre$tip.label[substr(tree.pre$tip.label,1,4)=="liat"] 

#in new column, use the given tip label or closest genus in an "r analog" column

#read in my amphistomy survey dataframe
species.labels.semifinal<-as.data.frame(read_excel("C:/Users/spunk/Downloads/amphistomy.final (4).xlsx"))

#formatting, replacing r analog name with the real ones I have in my survey
species.labels.semifinal$Species.lower<-as.character(species.labels.semifinal$`R analog`)
species.labels.semifinal$species.real<-as.character(paste(species.labels.semifinal$Genus,species.labels.semifinal$species,sep="_"))
species.labels.semifinal$species.labs<-as.character(species.labels.semifinal$species.real)

#species <- tolower(species.labels$tip.species) # Make everything lowercase so they all match
species.labels.semifinal[species.labels.semifinal$Species.lower %in% tree.pre$tip.label,]

genera<-sapply(strsplit(tree.pre$tip.label,"_"),"[[",1)
my.genera<-tolower(unique(sapply(strsplit(species.labels.semifinal$Species.lower,"_"),"[[",1)))
my.genera[my.genera %in% genera]
my.genera[!my.genera %in% genera]

#remove any NAs
species.labels.semifinal<-species.labels.semifinal[!is.na(species.labels.semifinal$`R analog`),]

tree <- congeneric.merge(tree.pre, unique(species.labels.semifinal$Species.lower)) # Assumes genus_species names; you can change this with an argument
tree <- drop.tip(tree, setdiff(tree$tip.label, species.labels.semifinal$Species.lower)) # Drop everything you don't need

species.labels.semifinal$tips<-factor(species.labels.semifinal$`R analog`,levels=tree$tip.label)
species.labels.semifinal<-species.labels.semifinal[order(species.labels.semifinal$tips),]


species.labels.semifinal$amp.col<-NA
species.labels.semifinal$amp.col[species.labels.semifinal$amphistomy_presence==0]<-"red"
species.labels.semifinal$amp.col[species.labels.semifinal$amphistomy_presence==1]<-"blue"

#check if that worked
plot(tree,type="phylogram")
cophenetic.phylo(tree)

#Phylogeny - color coded by specialization
col.tree<-tree
col.tree$tip.label<-species.labels.semifinal$species.real


plot(col.tree,cex = .45,tip.color = species.labels.semifinal$amp.col)

legend('topleft',fill=c("blue","red"),legend=c("Amphistomy","No Amphistomy"),bty='n')

box()


#####################Use phylosignal() to look for effect of K on the data

phylosignal(species.labels.semifinal$amphistomy_presence[!is.na(species.labels.semifinal$`R analog`)],phy=tree)
dim(species.labels.semifinal)


#####################proportions

#read in data
amphistomy.survey<-as.data.frame(read_excel("C:/Users/spunk/Downloads/amphistomy.final.xlsx",sheet = "sp_dat"))
view(amphistomy.survey)
dim(amphistomy.survey)


#proportion based on native-ness
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$native), margin=2)
N.native <- xtabs(~ amphistomy_presence + native, data = amphistomy.survey)
N.native
chisq.test(N.native)

#proportion based on woodyness
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$woody), margin=2)
N.growthform <- xtabs(~ amphistomy_presence + woody, data = amphistomy.survey)
N.growthform

amphistomy_presence_woodysubset <- subset(amphistomy.survey, woody !=1)
N.growthform.nointermediate <- xtabs(~ amphistomy_presence + woody, data = amphistomy_presence_woodysubset)
chisq.test(N.growthform.nointermediate)

#proportion based on leaf pubescence
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$leaf_pubescence), margin=2)
N.leaf.pub <- xtabs(~ amphistomy_presence + leaf_pubescence, data = amphistomy.survey)
N.leaf.pub

amphistomy_presence_pubsubset <- subset(amphistomy.survey, leaf_pubescence !=1)
N.pub.nointermediate <- xtabs(~ amphistomy_presence + leaf_pubescence, data = amphistomy_presence_pubsubset)
chisq.test(N.pub.nointermediate)


#proportion based on leaf angle
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$leaf_angle), margin=2)
N.leaf.angle <- xtabs(~ amphistomy_presence + leaf_angle, data = amphistomy.survey)
N.leaf.angle

amphistomy_presence_anglesubset <- subset(amphistomy.survey, leaf_angle !=1)
N.angle.nointermediate <- xtabs(~ amphistomy_presence + leaf_angle, data = amphistomy_presence_anglesubset)
chisq.test(N.angle.nointermediate)

#proportion based on leaf waxyness
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$leaf_waxyness), margin=2)
N.leaf.wax <- xtabs(~ amphistomy_presence + leaf_waxyness, data = amphistomy.survey)
N.leaf.wax

#proportion based on fire recovery method
prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$`resprout vs seed`), margin=2)
N.recovery <- xtabs(~ amphistomy_presence + `resprout vs seed`, data = amphistomy.survey)
N.recovery
chisq.test(N.recovery)
### This table is a bit messy, create new matrix where 1,2 counts for both reseed and resprout (plants will be duplicated)

recovery.matrix <- data.frame ("amphistomy-presence" = c(0, 1), "resprout"= c(28, 21), "reseed"= c(4,18))
chisq.test(recovery.matrix)

#frequency based on family

prop.table(table(amphistomy.survey$amphistomy_presence, amphistomy.survey$family), margin=2)

