library(tidyverse)
library(circlize)

setwd("~/Desktop/Review")
ref_data <- read.csv("rayyan_references_included_chords2.csv", header = TRUE)
df<- read.csv("articles.csv", header = TRUE)
df<-df %>% 
  filter(str_detect(Category, pattern = "nogap",negate=TRUE))

############## BIG SPATIAL SCALES ONLY ##############
dfb<-df %>% 
  filter(str_detect(Category, pattern = "K"))
#Spatial = S, Community = C, Genetic = G
# Small-scale = J, Big = K

#Get spatial gap matrix
spatial<-dfb %>% 
  filter(str_detect(Category, pattern = "S"))
out_spatial <- crossprod(table(subset(stack(setNames(lapply(strsplit(spatial$Category, 
                                                                     "[][]|,\\s*"), trimws), spatial$Id))[2:1], nzchar(values))))
diag(out_spatial) <- 0
#remove U and M category (misc. and unknown
out_spatial <- out_spatial[, !colnames(out_spatial) %in% c("U","M")] 
out_spatial <- out_spatial[!rownames(out_spatial) %in% c("U","M"),]

#Arrange 
custom_order<-c("V","O","H","I","E","L","B","T","Sx","F","W","G","C","S")
out_spatial <- out_spatial[custom_order, custom_order]

#get genetic gap matrix
genetic<-dfb %>% 
  filter(str_detect(Category, pattern = "G"))
out_gen <- crossprod(table(subset(stack(setNames(lapply(strsplit(genetic$Category, 
                                                                 "[][]|,\\s*"), trimws), genetic$Id))[2:1], nzchar(values))))
diag(out_gen) <- 0

#remove U and M category (misc. and unknown
out_gen <- out_gen[, !colnames(out_gen) %in% c("U","M")] 
out_gen <- out_gen[!rownames(out_gen) %in% c("U","M"),]

#Arrange 
custom_order<-c("V","O","H","I","E","L","B","T","Sx","F","W","S","G")
out_gen <- out_gen[custom_order, custom_order]

#get both gap matrix
both <- dfb %>%
  filter(str_detect(Category, pattern = "G") & str_detect(Category, pattern = "S"))
out_both <- crossprod(table(subset(stack(setNames(lapply(strsplit(both$Category, 
                                                                  "[][]|,\\s*"), trimws), both$Id))[2:1], nzchar(values))))
diag(out_both) <- 0

#remove U and M category (misc. and unknown
out_both <- out_both[, !colnames(out_both) %in% c("U","M")] 
out_both <- out_both[!rownames(out_both) %in% c("U","M"),]

#Arrange 
custom_order<-c("V","O","H","I","E","L","B","T","Sx","F","W","S","G")
out_both <- out_both[custom_order, custom_order]


#Make plots

cols <- c(B = "indianred3", C = "darkmagenta", E = "#F0E442",
          "F" = "#6EE2FF", G = "darkblue",
          H = "violetred", I = "brown4",
          L = "royalblue3",M = "yellow4",
          O = "olivedrab2",S = "gray25",
          Sx = "lightsalmon","T" = "#009E73",
          U = "tan",V = "plum1",W = "chocolate1")

#add groups for spacing
group <- c(V = "A", O = "A", H = "A", I = "A", E = "A", L = "A",
           B = "A", "T" = "A", Sx = "A", "F" = "A",W="A",G = "B", C = "B",S = "B")

out_spatial[upper.tri(out_spatial)] <- NA
out_gen[upper.tri(out_gen)] <- NA
out_both[upper.tri(out_both)] <- NA


#Big Gaps
circos.clear()
par(mfrow = c(2,3))
chordDiagram(out_spatial,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10) 
title("Spatial")

circos.clear()
chordDiagram(out_gen,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10)
title("Genetic")

circos.clear()
chordDiagram(out_both,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10)
title("Both")

#Make interactive
library(chorddiag)
#p <- chorddiag(out_spatial,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10)
p <- chorddiag(out_spatial, groupColors = cols, groupnamePadding = 20)
p

