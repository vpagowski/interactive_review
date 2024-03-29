setwd("~/Desktop/Review")
ref_data <- read.csv("rayyan_references_included_chords2.csv", header = TRUE)

###Associations plots
library(tidyverse)
df<- read.csv("articles.csv", header = TRUE)
df<-df %>% 
  filter(str_detect(Category, pattern = "nogap",negate=TRUE))

############## SMALL SPATIAL SCALES ONLY ##############
dfs<-df %>% 
  filter(str_detect(Category, pattern = "J"))
#Spatial = S, Community = C, Genetic = G
# Small-scale = J, Big = K

#Get spatial gap matrix
spatial1<-dfs %>% 
  filter(str_detect(Category, pattern = "S") & !str_detect(Category, pattern = "G"))
out_spatial1 <- crossprod(table(subset(stack(setNames(lapply(strsplit(spatial1$Category, 
                                                                      "[][]|,\\s*"), trimws), spatial1$Id))[2:1], nzchar(values))))
diag(out_spatial1) <- 0
#remove U and M category (misc. and unknown)
out_spatial1 <- out_spatial1[, !colnames(out_spatial1) %in% c("U","M")] 
out_spatial1 <- out_spatial1[!rownames(out_spatial1) %in% c("U","M"),]

#Arrange 
custom_order<-c("V","O","H","I","E","B","T","C","S")
out_spatial1 <- out_spatial1[custom_order, custom_order]
cols <- c(B = "indianred3", C = "darkmagenta", E = "#F0E442",
          "F" = "#6EE2FF", G = "darkblue",
          H = "violetred", I = "brown4",
          L = "royalblue3",M = "yellow4",
          O = "olivedrab2",S = "gray25",
          Sx = "lightsalmon","T" = "#009E73",
          U = "tan",V = "plum1",W = "chocolate1")

library(circlize)

#Make plots
circos.clear()
#add groups for spacing
group <- c(V = "A", O = "A", H = "A", I = "A", E = "A", L = "A",
           B = "A", "T" = "A", Sx = "A", "F" = "A",W="A",G = "B", C = "B",S = "B")

out_spatial1[upper.tri(out_spatial1)] <- NA

p<-chordDiagram(out_spatial1,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10) 
title("Spatial")

#cols2 <- c("plum1","olivedrab2","violetred","borwn4","royalblue3","indianred3","#009E73","lightsalmon","#6EE2FF","chocolate1","gray25","darkblue","darkmagenta","green")


#Make interactive
library(chorddiag)
library(htmlwidgets)
library(gdata)

cols <- c(B = "indianred3", C = "darkmagenta", E = "#F0E442",
          "F" = "#6EE2FF", G = "darkblue",
          H = "violetred", I = "brown4",
          L = "royalblue3",M = "yellow4",
          O = "olivedrab2",S = "gray25",
          Sx = "lightsalmon","T" = "#009E73",
          U = "tan",V = "plum1",W = "chocolate1")


#add white columns that are blank to spatially separate diagram
cols2 <- c("#FFBBFF","#B3EE3A","#D02090","#8B2323", "#F0E442","#CD5555", "#009E73","white","white","white","white","white","white","#8B008B","#404040","white","white","white","white","white","white")
upperTriangle(out_spatial1) = lowerTriangle(out_spatial1, byrow=TRUE)
#p <- chorddiag(out_spatial,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10)

ad <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
out_spatial2 = cbind(out_spatial1[,c(1:7)],ad,ad,ad,ad,ad,ad,out_spatial1[,c(8:9)],ad,ad,ad,ad,ad,ad)
out_spatial2 = rbind(out_spatial2[c(1:7),],ad,ad,ad,ad,ad,ad,out_spatial2[c(8:9),],ad,ad,ad,ad,ad,ad)

p <- chorddiag(out_spatial2, groupColors = cols2, showGroupnames = FALSE,groupPadding = 2,showTooltips = FALSE,tickInterval=10,ticklabelFontsize=0)
p


setwd("/Users/veronicapagowski/Desktop/interactive_app/interactive_review")
saveWidget(p, "chord_test.html", selfcontained = TRUE, knitrOptions = list())

