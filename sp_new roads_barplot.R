##Import species index data for colors---------

index_sp<- read.csv("~/final index_species.csv", sep="\t")
#colors for index by the value
col.indx<- colorRampPalette(list('darkgreen','forestgreen','chartreuse4','darkolivegreen3','yellow','orange','red','darkred','indianred4','lightpink4'))(1001)
color.indx2<-data.frame(value=seq(0,10,by=0.01), color.name=col.indx)

index_sp$index_cat_10<-format(round(index_sp$index_cat_10, 1), nsmall = 1)
index_sp$index_cat_10<- as.factor(index_sp$index_cat_10)
color.indx2$value<- as.factor(color.indx2$value)
index_sp2<- merge(index_sp[,c(1,14)], color.indx2, by.x="index_cat_10", by.y="value", all.x=T)

### Add species distribution data -----------
species<- read.csv("~/spider5.csv")

#join with index value
index_sp3<- merge(index_sp2, species[,c(1,3)], by="binomial", all.x=T)


## Brazil -------

#Import data
br<- read.csv("~/BRAMA_Length_new_rd_each_spp_20201005.csv")
  
#sum different types of road by species
br2<-data.frame(aggregate(br$length, by=list(species=br$binomial), FUN=sum))

#Add total lenght of the new road project, and calculate teh % that will cross species distribution
br2$tot_length<- 36456.43
br2$perc <- round(br2$x/br2$tot_length*100,1)

#merge with colors dataframe
br3<- merge(br2, index_sp3, by.x="species", by.y="binomial", all.x=T)

#Calculate proportion of distribution range affected by roads
br3$dist_road<- br3$x / br3$Distribution_km2 *100

#bar plot
library(ggplot2)

#Barplot km of range affected by new roads
pdf("~/sp_new roads_BR.pdf", 10,5)
ggplot(data=br3, aes(x=reorder(species,-x), y=x))+ 
  geom_bar(stat="identity", width=0.5, fill=br3$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,38000))+
  scale_y_continuous(breaks=seq(0,38000,6000))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14,face='italic'))+
  ylab("Road length (km)") + xlab("")+
  coord_flip()
dev.off()

#Barplor proportion on total range occupied by new roads
pdf("~/sp_new roads_BR_v2.pdf", 10,5)
ggplot(data=br3, aes(x=reorder(species,-dist_road), y=dist_road))+ 
  geom_bar(stat="identity", width=0.5, fill=br3$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,0.5))+
  scale_y_continuous(breaks=seq(0,0.5,0.05))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14,face='italic'))+
  ylab("Proportion of distribution range affected by new roads") + xlab("")+
  coord_flip()
dev.off()

#The two barplots together, remove y label from the second plot
library(magrittr)
library(ggplot2)
g1<- ggplot(data=br3, aes(x=reorder(species,-dist_road), y=x, label=paste0(perc,"%")))+ 
  geom_bar(stat="identity", width=0.5, fill=br3$color.name)+ 
  theme_classic()+ 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,-1,3.84,30), "mm")) +
  geom_text(aes(y = x+4500),angle = 360, hjust = -.05, size = 4.5)+
  ylab("FRL (km)") + 
  scale_y_reverse(breaks=seq(0,40000,6000))+ scale_x_discrete(position="top")+
  coord_flip()

g2<-ggplot(data=br3, aes(x=reorder(species,-dist_road), y=dist_road, label=species))+ 
  geom_bar(stat="identity", width=0.5, fill=br3$color.name)+
  theme_classic()+ 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,30,1,-1), "mm")) +
  geom_text(aes(y = dist_road),angle = 360, hjust = -.05, size = 5.3, fontface="italic")+
  ylab(bquote("FRL relative to species range"~(km/km^2)))+
  scale_y_continuous(breaks=seq(0,0.5,0.05))+
  coord_flip(clip = "off")

#Now join plots
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))

pdf("~/sp_new roads_BR_twobar.pdf", 15,5)
grid.arrange(gg1,gg2,ncol=2)
dev.off()

## Africa ------------

#Import data
af<- read.csv("~/AF_Length_new_rd_each_spp_20201005.csv")

#Add total lenght of the new road project, and calculate teh % that will cross species distribution
af$tot_length<- 57122.69
af$perc <- round(af$length/af$tot_length*100,1)

#merge with colors dataframe
af2<- merge(af, index_sp3, by="binomial", all.x=T)

#Calculate proportion of distribution range affected by roads
af2$dist_road<- af2$length / af2$Distribution_km2 *100

#Barplot km of range affected by new roads
pdf("~/sp_new roads_AF.pdf", 10,5)
ggplot(data=af2, aes(x=reorder(binomial,-length), y=length))+ 
  geom_bar(stat="identity", width=0.8, fill=af2$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,42000))+
  scale_y_continuous(breaks=seq(0,42000,6000))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14, face='italic'))+
  ylab("Road length (km)") + xlab("")+
  coord_flip()
dev.off()

#Barplor proportion on total range occupied by new roads
pdf("~/sp_new roads_AF_v2.pdf", 10,5)
ggplot(data=af2, aes(x=reorder(binomial,-dist_road), y=dist_road))+ 
  geom_bar(stat="identity", width=0.8, fill=af2$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,0.5))+
  scale_y_continuous(breaks=seq(0,0.5,0.05))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14,face='italic'))+
  ylab("Proportion of distribution range affected by new roads") + xlab("")+
  coord_flip()
dev.off()

#The two barplots together, remove y label from the second plot
library(magrittr)
library(ggplot2)
g1<- ggplot(data=af2, aes(x=reorder(binomial,-dist_road), y=length, label=paste0(perc,"%")))+ 
  geom_bar(stat="identity", width=0.8, fill=af2$color.name)+ 
  theme_classic()+ 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,-1,3.84,40), "mm")) +
  geom_text(aes(y = length+5500),angle = 360, hjust = -.05, size = 4.5)+
  ylab("FRL (km)") + 
  scale_y_reverse(breaks=seq(0,42000,6000))+ scale_x_discrete(position="top")+
  coord_flip()

g2<- ggplot(data=af2, aes(x=reorder(binomial,-dist_road), y=dist_road, label=binomial))+ 
  geom_bar(stat="identity", width=0.8, fill=af2$color.name)+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,0.5,0.05))+
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,40,1,-1), "mm")) +
  geom_text(aes(y = dist_road),angle = 360, hjust = -.05, size = 5.3, fontface="italic")+
  ylab(bquote("FRL relative to species range"~(km/km^2)))+
  coord_flip(clip = "off")

#Now join plots
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))

pdf("~/sp_new roads_AF_twobar.pdf", 15,5)
grid.arrange(gg1,gg2,ncol=2)
dev.off()


## Nepal----------

#Import data
np<- read.csv("~/NP_Length_new_rd_each_spp_20201005.csv")

#Add total lenght of the new road project, and calculate teh % that will cross species distribution
np$tot_length<- 974.5949
np$perc <- round(np$length/np$tot_length*100,1)

#merge with colors dataframe
np2<- merge(np, index_sp3, by="binomial", all.x=T)

#Calculate proportion of distribution range affected by roads
np2$dist_road<- np2$length / np2$Distribution_km2 *100

#Barplot km of range affected by new roads
pdf("~/sp_new roads_NP.pdf", 10,5)
ggplot(data=np2, aes(x=reorder(binomial,-length), y=length))+ 
  geom_bar(stat="identity", width=0.8, fill=np2$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,1000))+
  scale_y_continuous(breaks=seq(0,1000,200))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14, face='italic'))+
  ylab("Road length (km)") + xlab("")+  
  coord_flip()
dev.off()

#Barplor proportion on total range occupied by new roads
pdf("~/sp_new roads_NP_v2.pdf", 10,5)
ggplot(data=np2, aes(x=reorder(binomial,-dist_road), y=dist_road))+ 
  geom_bar(stat="identity", width=0.8, fill=np2$color.name)+
  theme_classic()+ coord_cartesian(ylim=c(0,0.5))+
  scale_y_continuous(breaks=seq(0,0.5,0.01))+
  theme(axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.text.y = element_text(size=14,face='italic'))+
  ylab("Proportion of distribution range affected by new roads") + xlab("")+
  coord_flip()
dev.off()

#The two barplots together, remove y label from the second plot
library(magrittr)
library(ggplot2)
g1<- ggplot(data=np2, aes(x=reorder(binomial,-dist_road), y=length, label=paste0(perc,"%")))+ 
  geom_bar(stat="identity", width=0.8, fill=np2$color.name)+ 
  theme_classic()+ 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,-1,3.84,40), "mm")) +
  geom_text(aes(y = length+120),angle = 360, hjust = -.05, size = 4.5)+
  ylab("FRL (km)") + 
  scale_y_reverse(breaks=seq(0,1000,200))+ scale_x_discrete(position="top")+
  coord_flip()

g2<- ggplot(data=np2, aes(x=reorder(binomial,-dist_road), y=dist_road, label=binomial))+ 
  geom_bar(stat="identity", width=0.8, fill=np2$color.name)+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,0.5,0.01))+
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size=15), axis.title=element_text(size=15),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,40,1,-1), "mm")) +
  geom_text(aes(y = dist_road),angle = 360, hjust = -.05, size = 5.3, fontface="italic")+
  ylab(bquote("FRL relative to species range"~(km/km^2)))+
  coord_flip(clip = "off")

#Now join plots
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))

pdf("~/sp_new roads_NP_twobar.pdf", 15,5)
grid.arrange(gg1,gg2,ncol=2)
dev.off()
