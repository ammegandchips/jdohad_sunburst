#load packages
require(sunburstR)
require(RColorBrewer)

#load data
dat <- read.csv("\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\dat.csv", stringsAsFactors=FALSE, na.strings="") #PC
dat <- read.csv("/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/dat.csv",stringsAsFactors=FALSE, na.strings="") #Mac

#check all 532 studies are included
c(1:532)[which((1:532 %in% dat$Study.ID)=="FALSE")]

#clean and simplify exposure target data
dat$Exposure.target[which(dat$Exposure.target=="mother"|dat$Exposure.target=="Mother"|dat$Exposure.target=="Mother ")]<-"Mother"
dat$Exposure.target[which(dat$Exposure.target=="Offspring"|dat$Exposure.target=="offspring")]<-"Offspring"
dat$Exposure.target[grep(dat$Exposure.target,pattern="parent")]<-"(Great) grandparents"
dat$Timing.of.exposure[which(dat$Timing.of.exposure=="pregnancy"|dat$Timing.of.exposure=="Pregnancy")]<-"Pregnancy"
dat$Timing.of.exposure[which(dat$Timing.of.exposure == "Pre-pregnancy")] <- "Prepregnancy"

#select only "included" studies
df <- dat[which(dat$Included=="Y"|dat$Included=="y"),]
length(unique(df$Study.ID)) #325 studies

#prepare for sunburst function
prepare.for.sunburst <-function(df.x){
ring1 <- as.data.frame.matrix(table(df.x$Study.ID,paste(df.x$Exposure.target)))
ring1 <- apply(ring1,1,function(x) paste(colnames(ring1)[which(x!=0)],collapse=" & "))
ring1[ring1=="Mother"] <-"Mother only"
ring1[ring1=="Father"] <-"Father only"
ring1[ring1=="Offspring"] <-"Offspring only"
ring1[ring1=="(Great) grandparents"] <-"(Great) grandparents only"
ring0 <- rep("No maternal or paternal exposures",length(ring1))
ring0[grep(ring1,pattern="Mother")] <-"Maternal exposure (no paternal)"
ring0[grep(ring1,pattern="Father only")] <-"Paternal exposure (no maternal)"
ring0[paste(grepl(ring1,pattern="Father"),grepl(ring1,pattern="Mother"))=="TRUE TRUE"] <-"Both maternal and paternal exposures"
ring2 <- as.data.frame.matrix(table(df.x$Study.ID,paste(df.x$Exposure.target,df.x$Timing.of.exposure)))
ring2 <- apply(ring2,1,function(x) paste(colnames(ring2)[which(x!=0)],collapse=" & "))
ring3 <- as.data.frame.matrix(table(df.x$Study.ID,paste(df.x$Type.of.exposure.1)))
ring3 <- apply(ring3,1,function(x) paste(colnames(ring3)[which(x!=0)],collapse=" & "))
ring4 <- as.data.frame.matrix(table(df.x$Study.ID,paste(df.x$Type.of.exposure.2)))
ring4$"NA" <- NULL
ring4 <- apply(ring4,1,function(x) paste(colnames(ring4)[which(x!=0)],collapse=" & "))
ring5 <- as.data.frame.matrix(table(df.x$Study.ID,paste(df.x$Type.of.exposure.3)))
ring5$"NA" <- NULL
ring5 <- apply(ring5,1,function(x) paste(colnames(ring5)[which(x!=0)],collapse=" & "))

X<-cbind(ring0,ring1,ring2,ring3,ring4,ring5) 
Y<-apply(X,1,unique)
Y <- lapply(Y, function(x) paste(x[!is.na(x)], collapse = "-"))
Y <-as.data.frame(table(unlist(Y)))
list(X,Y)
}

#prepare dataframes for animals, humans, both * treating "pregnancy (birth)" exposures as maternal or offspring
df.mother <- df
df.mother$Timing.of.exposure[which(df.mother$Timing.of.exposure == "Pregnancy (birth)")] <-"Pregnancy"
df.offspring <- df
df.offspring$Exposure.target[which(df.offspring$Timing.of.exposure == "Pregnancy (birth)")] <-"Offspring"
df.offspring$Timing.of.exposure[which(df.offspring$Timing.of.exposure == "Pregnancy (birth)")] <-"Pregnancy"

df.animals.mother <- df.mother[which(df.mother$Species.studied!="Humans"),]
df.humans.mother <- df.mother[which(df.mother$Species.studied=="Humans"),]
df.animals.offspring <- df.offspring[which(df.offspring$Species.studied!="Humans"),]
df.humans.offspring <- df.offspring[which(df.offspring$Species.studied=="Humans"),]

#turn dataframes into format for sunburst
Both.mother<- prepare.for.sunburst(df.mother)
Animals.mother<- prepare.for.sunburst(df.animals.mother)
Humans.mother<- prepare.for.sunburst(df.humans.mother)
Both.offspring<- prepare.for.sunburst(df.offspring)
Animals.offspring<- prepare.for.sunburst(df.animals.offspring)
Humans.offspring<- prepare.for.sunburst(df.humans.offspring)

#create vector of colours (VALUES) matched to sections on the chart (DOMAINS)
DOMAINS <- unique(c(Both.mother[[1]],Both.offspring[[1]]))
VALUES <- rep("#a09992",length(DOMAINS))
VALUES[grep("No maternal or paternal exposures|Mother|Father|Mother Pregnancy|Father Pregnancy|Prepregnancy|Offspring|enduring|Postnatal|grandparents",DOMAINS)] <- "#5F5953"
VALUES[grep("Maternal exposure (no paternal)",DOMAINS)] <- "#F6AC42"
VALUES[grep("Mother only",DOMAINS)] <- "#f78e49"
VALUES[grep("Mother Pregnancy",DOMAINS)] <- "#F96A51"
VALUES[grep("Paternal exposure (no maternal)",DOMAINS)] <- "#43a38c"
VALUES[grep("Father only",DOMAINS)] <- "#70C5B1"
VALUES[grep("Both maternal and paternal exposures",DOMAINS)] <- "#94a76b"
VALUES[grep("Father & Mother",DOMAINS)] <- "#94a76b"

#create vector of all branches in the right order (i.e. maternal > both > paternal > neither)
X.all<-rbind(Both.mother[[2]][order(Both.mother[[2]]$Freq,decreasing=TRUE),],Both.offspring[[2]][order(Both.offspring[[2]]$Freq,decreasing=TRUE),])
All.combinations <- data.frame(combination=as.character(unique(X.all$Var1)))
All.combinations$order1 <- 1:nrow(All.combinations)
All.combinations$order2 <- rep(5,nrow(All.combinations))
All.combinations$order2[grepl(All.combinations$combination,pattern="Any maternal exposure")]<-1
All.combinations$order2[grepl(All.combinations$combination,pattern="Both maternal and paternal exposures")]<-2
All.combinations$order2[grepl(All.combinations$combination,pattern="Any paternal exposure")]<-3
All.combinations$order2[grepl(All.combinations$combination,pattern="No maternal or paternal exposures")]<-4
All.combinations$order3 <- 2
All.combinations$order3[grep(All.combinations$combination,pattern="Mother Pregnancy")]<-1

All.combinations <-All.combinations[order(All.combinations$order2, All.combinations$order3),]
All.combinations <- All.combinations$combination

#create sunbursts
Both.offspringS <- sunburst(na.omit(Both.offspring[[2]][match(All.combinations,Both.offspring[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")
Both.motherS <- sunburst(na.omit(Both.mother[[2]][match(All.combinations,Both.mother[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")
Humans.offspringS <- sunburst(na.omit(Humans.offspring[[2]][match(All.combinations,Humans.offspring[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")
Humans.motherS <- sunburst(na.omit(Humans.mother[[2]][match(All.combinations,Humans.mother[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")
Animals.offspringS <- sunburst(na.omit(Animals.offspring[[2]][match(All.combinations,Animals.offspring[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")
Animals.motherS <- sunburst(na.omit(Animals.mother[[2]][match(All.combinations,Animals.mother[[2]]$Var1),]),colors=list(range=VALUES,domain=DOMAINS),legend=FALSE,explanation = "function(d) { return ( Math.round((d.value / this * 100) * 100) / 100 ) + '%<br>' + d.value + ' of ' + this }")

#save sunbursts (PC)
saveRDS(Both.motherS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Both.mother.rds")
saveRDS(Animals.motherS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Animals.mother.rds")
saveRDS(Humans.motherS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Humans.mother.rds")
saveRDS(Both.offspringS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Both.offspring.rds")
saveRDS(Animals.offspringS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Animals.offspring.rds")
saveRDS(Humans.offspringS,"\\\\ads.bris.ac.uk\\filestore\\MyFiles\\Staff17\\gs8094\\Documents\\work\\ieu\\present\\pat_effects_commentary\\JDOHAD\\sunburst\\Humans.offspring.rds")

#save sunbursts (Mac)
saveRDS(Both.motherS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Both.mother.rds")
saveRDS(Animals.motherS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Animals.mother.rds")
saveRDS(Humans.motherS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Humans.mother.rds")
saveRDS(Both.offspringS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Both.offspring.rds")
saveRDS(Animals.offspringS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Animals.offspring.rds")
saveRDS(Humans.offspringS,"/Volumes/filestore/Documents/work/ieu/present/pat_effects_commentary/JDOHAD/sunburst/Humans.offspring.rds")
