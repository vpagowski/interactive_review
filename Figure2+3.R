#Re-create Figure 2 (static chord diagrams) and histograms (split by mechanism type, top row Fig. 3)

setwd("/Users/veronicapagowski/Desktop/interactive_app/interactive_review")
#Load libraries
library(tidyverse)
library(circlize)
library(ggridges)
library(ggplot2)
library(plyr)

df<- read.csv("articles_03_24_all.csv", header = TRUE)
df<-df %>% 
  filter(str_detect(Category, pattern = "nogap",negate=TRUE))

############## BIG SPATIAL SCALES ONLY ##############
dfb<-df %>% 
  filter(str_detect(Category, pattern = "Bg,"))

#Get spatial gap matrix
spatial<-dfb %>% 
  filter(str_detect(Category, pattern = "Spatial,") & !str_detect(Category, pattern = "Genetic,"))
out_spatial <- crossprod(table(subset(stack(setNames(lapply(strsplit(spatial$Category, 
                                                                 "[][]|,\\s*"), trimws), spatial$Id))[2:1], nzchar(values))))
#association with itself doesn't count
diag(out_spatial) <- 0

#Set an order to display categories
custom_order1<-c("Historical","Oceanography","Habitat","Invasive","Effort","Larval_duration","Behavior","Temperature","Selection_incompatibility","Competition","Oxygen","Salinity","Climate_change","Misc","Unknown","Deep_sea","Community","Genetic","Spatial")

#Filter categories that exist in this subset
custom_order_a <- intersect(custom_order1,colnames(out_spatial))

#Arrange 
out_spatial <- out_spatial[custom_order_a, custom_order_a]

#get genetic gap matrix
genetic<-dfb %>% 
  filter(str_detect(Category, pattern = "Genetic,") & !str_detect(Category, pattern = "Spatial,"))
out_gen <- crossprod(table(subset(stack(setNames(lapply(strsplit(genetic$Category, 
                                                                 "[][]|,\\s*"), trimws), genetic$Id))[2:1], nzchar(values))))
diag(out_gen) <- 0

#Arrange 
custom_order_b <- intersect(custom_order1,colnames(out_gen))
out_gen <- out_gen[custom_order_b, custom_order_b]

#get both gap matrix
both <- dfb %>%
  filter(str_detect(Category, pattern = "Genetic,") & str_detect(Category, pattern = "Spatial,"))
out_both <- crossprod(table(subset(stack(setNames(lapply(strsplit(both$Category, 
                                                                 "[][]|,\\s*"), trimws), both$Id))[2:1], nzchar(values))))
diag(out_both) <- 0

#Arrange 
custom_order_c <- intersect(custom_order1,colnames(out_both))
out_both <- out_both[custom_order_c, custom_order_c]



out_spatial[upper.tri(out_spatial)] <- NA
out_gen[upper.tri(out_gen)] <- NA
out_both[upper.tri(out_both)] <- NA


############## SMALL SPATIAL SCALES ONLY ##############
#Some repetition here, left for ease fo running separately
custom_order2<-c("Historical","Oceanography","Habitat","Invasive","Effort","Larval_duration","Behavior","Temperature","Selection_incompatibility","Competition","Oxygen","Salinity","Climate_change","Misc","Unknown","Deep_sea","Community","Genetic","Spatial")

dfs<-df %>% 
  filter(str_detect(Category, pattern = "Sm,"))

#Get spatial gap matrix
spatial1<-dfs %>% 
  filter(str_detect(Category, pattern = "Spatial,") & !str_detect(Category, pattern = "Genetic,"))
out_spatial1 <- crossprod(table(subset(stack(setNames(lapply(strsplit(spatial1$Category, 
                                                                     "[][]|,\\s*"), trimws), spatial1$Id))[2:1], nzchar(values))))
diag(out_spatial1) <- 0

#Arrange 
custom_order_a1<- intersect(custom_order2,colnames(out_spatial1))
out_spatial1 <- out_spatial1[custom_order_a1, custom_order_a1]

#get genetic gap matrix
genetic1<-dfs %>% 
  filter(str_detect(Category, pattern = "Genetic,") & !str_detect(Category, pattern = "Spatial,"))
out_gen1 <- crossprod(table(subset(stack(setNames(lapply(strsplit(genetic1$Category, 
                                                                 "[][]|,\\s*"), trimws), genetic1$Id))[2:1], nzchar(values))))
diag(out_gen1) <- 0

#Arrange 
custom_order_b1<- intersect(custom_order2,colnames(out_gen1))
out_gen1 <- out_gen1[custom_order_b1, custom_order_b1]

#get both gap matrix
both1 <- dfs %>%
  filter(str_detect(Category, pattern = "Genetic,") & str_detect(Category, pattern = "Spatial,"))
out_both1 <- crossprod(table(subset(stack(setNames(lapply(strsplit(both1$Category, 
                                                                  "[][]|,\\s*"), trimws), both1$Id))[2:1], nzchar(values))))
diag(out_both1) <- 0

#Arrange 
custom_order_c1<- intersect(custom_order2,colnames(out_both1))
out_both1 <- out_both1[custom_order_c1, custom_order_c1]


#Make plots
#add groups for spacing
group <- c(Historical = "A", Oceanography = "A", Habitat = "A", Invasive = "A", Effort = "A", Larval_duration = "A",
           Behavior = "A", Temperature = "A", Selection_incompatibility = "A", Competition = "A",Oxygen ="A",Salinity = "A",Climate_change ="A", Deep_sea="A", Misc = "A",Unknown = "A" ,Genetic = "B", Community = "B", Spatial = "B")

#We are interested in one way associations, make matrix a triagle (ie don't repeat the same values in a square matrix)
out_spatial1[upper.tri(out_spatial1)] <- NA
out_gen1[upper.tri(out_gen1)] <- NA
out_both1[upper.tri(out_both1)] <- NA

circos.clear()


#Assign colors
cols2 <- c(Spatial = "gray25",Genetic = "darkblue",Community = "darkmagenta",Historical = "plum1",Oceanography = "olivedrab2",
           Habitat = "violetred",Invasive = "brown4",Effort = "#F0E442",Larval_duration = "royalblue3",Behavior = "indianred3",Temperature = "#009E73",
           Selection_incompatibility = "lightsalmon",Competition = "#6EE2FF",Oxygen ="maroon", Salinity ="brown2",Climate_change = "chocolate1",Deep_sea = "darkgreen",Misc ="yellow4", Unknown = "tan")

#Big Gaps
par(mfrow = c(2,3))
chordDiagram(out_spatial,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10) 
title("Spatial",cex.main = 1.8)
#Add tick marks every 10 papers and make it pretty
for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}
chordDiagram(out_gen,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10)
title("Genetic",cex.main = 1.8)
for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}
chordDiagram(out_both,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10)
for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}
title("Both",cex.main = 1.8)

#Small gaps
#Repeat everything for small-scales
p<-chordDiagram(out_spatial1,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10) 
title("Spatial",cex.main = 1.8)
for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}
chordDiagram(out_gen1,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10)
title("Genetic",cex.main = 1.8)
for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}

chordDiagram(out_both1,grid.col = cols2,annotationTrack = "grid",group=group, big.gap = 10)
title("Both",cex.main = 1.8)

for(si in get.all.sector.index()) {
  circos.axis(h = 'top',
              major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280),
              sector.index = si,
              track.index = 1,
              labels = FALSE,
              minor.tick = FALSE,
              lwd = 0.5,
              labels.pos.adjust = TRUE)
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
}



#Legends are weird with circlize, make it separately and export for figure making
par(mfrow = c(1,3),mar = c(5,0,0,0))
plot.new()
legend("bottom", legend = c("Spatial","Genetic","Community", "Historical process","Oceanography","Habitat","Invasion","Effort", 
                            "Larval duration", "Behavior", "Temperature", "Selection/incompatibility","Competition", "Oxygen","Salinity","Climate change","Deep sea","Other","Unknown"), fill = cols2)



###########HISTOGRAMS###################
#Top part of Figure 3 (Histograms by date)
#A little clunky, repeat the same process for dfb to make big-scale plots

#Disjunction types
S<-dfs %>% 
  filter(str_detect(Category, pattern = "Spatial,"))
S$Category="S"
G<-dfs %>% 
  filter(str_detect(Category, pattern = "Genetic,"))
G$Category="G"
C<-dfs %>% 
  filter(str_detect(Category, pattern = "Community,"))
C$Category="C"

##Mechanisms##
V<-dfs %>% 
  filter(str_detect(Category, pattern = "Historical,"))
V$Category="V"
O<-dfs %>% 
  filter(str_detect(Category, pattern = "Oceanography,"))
O$Category="O"
H<-dfs %>% 
  filter(str_detect(Category, pattern = "Habitat,"))
H$Category="H"
I<-dfs %>% 
  filter(str_detect(Category, pattern = "Invasive,"))
I$Category="I"
E<-dfs %>% 
  filter(str_detect(Category, pattern = "Effort,"))
E$Category="E"
L<-dfs %>% 
  filter(str_detect(Category, pattern = "Larval_duration,"))
L$Category="L"
B<-dfs %>% 
  filter(str_detect(Category, pattern = "Behavior,"))
B$Category="B"
T<-dfs %>% 
  filter(str_detect(Category, pattern = "Temperature,"))
T$Category="T"
Sx<-dfs %>% 
  filter(str_detect(Category, pattern = "Selection_incompatibility,"))
Sx$Category="Sx"
F<-dfs %>% 
  filter(str_detect(Category, pattern = "Competition,"))
F$Category="F"
Ox<-dfs %>% 
  filter(str_detect(Category, pattern = "Oxygen,"))
Ox$Category="Ox"
Sal<-dfs %>% 
  filter(str_detect(Category, pattern = "Salinity,"))
Sal$Category="Sal"
W<-dfs %>% 
  filter(str_detect(Category, pattern = "Climate_change,"))
W$Category="W"
D<-dfs %>% 
  filter(str_detect(Category, pattern = "Deep_sea,"))
D$Category="D"

all<-rbind(V,O,H,I,E,L,B,T,Sx,F,Ox,Sal,W,D)


cols <- c("historical process" = "plum1", oceanography = "olivedrab2",  habitat= "violetred",invasion = "brown4", effort = "#F0E442", "larval dur." = "royalblue3",
           behavior = "indianred3", temperature = "#009E73",selection = "lightsalmon",competition = "#6EE2FF",oxygen="maroon", salinity="brown2",
           warming = "chocolate1","deep sea" = "darkgreen")

custom_order<-c("V","O","H","I","E","L","B","T","Sx","F","Ox","Sal","W","D")
# basic example
all %>%
  arrange(match(Category, custom_order))
all$Category <- factor(all$Category, levels = custom_order)
all$Category2<-mapvalues(all$Category, from = custom_order, to = c("historical process", "oceanography", "habitat","invasion", "effort", "larval dur.",
                                                                   "behavior","temperature","selection","competition","oxygen","salinity","warming","deep sea"))
# Make histograms
all %>% 
  ggplot(aes(x = year))+
  geom_histogram(aes(fill = Category2),  
                 position = position_stack(), 
                 alpha = 1) +
  facet_wrap(~Category,ncol = 1)+ 
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(),
        strip.text.x = element_blank())+ 
  scale_fill_manual(values = cols[all$Category2]) + 
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020))



