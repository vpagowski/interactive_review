test_markdown
================
Veronica
2024-03-08

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
setwd("~/Desktop/Review")
#Make descision tree
ref_data <- read.csv("rayyan_references_included_chords2.csv", header = TRUE)

###Associations plots
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ─────────────────── tidyverse 2.0.0.9000 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
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
#remove U and M category (misc. and unknown
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
```

    ## ========================================
    ## circlize version 0.4.16
    ## CRAN page: https://cran.r-project.org/package=circlize
    ## Github page: https://github.com/jokergoo/circlize
    ## Documentation: https://jokergoo.github.io/circlize_book/book/
    ## 
    ## If you use it in published research, please cite:
    ## Gu, Z. circlize implements and enhances circular visualization
    ##   in R. Bioinformatics 2014.
    ## 
    ## This message can be suppressed by:
    ##   suppressPackageStartupMessages(library(circlize))
    ## ========================================

``` r
#Make plots
circos.clear()
#add groups for spacing
group <- c(V = "A", O = "A", H = "A", I = "A", E = "A", L = "A",
           B = "A", "T" = "A", Sx = "A", "F" = "A",W="A",G = "B", C = "B",S = "B")

out_spatial1[upper.tri(out_spatial1)] <- NA

p<-chordDiagram(out_spatial1,grid.col = cols,annotationTrack = "grid",group=group, big.gap = 10) 
title("Spatial")
```

![](test_markdown_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#cols2 <- c("plum1","olivedrab2","violetred","borwn4","royalblue3","indianred3","#009E73","lightsalmon","#6EE2FF","chocolate1","gray25","darkblue","darkmagenta","green")


#Make interactive
library(chorddiag)
library(htmlwidgets)
library(gdata)
```

    ## 
    ## Attaching package: 'gdata'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     combine, first, last, starts_with
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     keep
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     starts_with
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     nobs
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     object.size
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     startsWith

``` r
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
```

    ## Warning in cbind(out_spatial1[, c(1:7)], ad, ad, ad, ad, ad, ad, out_spatial1[,
    ## : number of rows of result is not a multiple of vector length (arg 2)

``` r
out_spatial2 = rbind(out_spatial2[c(1:7),],ad,ad,ad,ad,ad,ad,out_spatial2[c(8:9),],ad,ad,ad,ad,ad,ad)
```

    ## Warning in rbind(out_spatial2[c(1:7), ], ad, ad, ad, ad, ad, ad,
    ## out_spatial2[c(8:9), : number of columns of result is not a multiple of vector
    ## length (arg 2)

## Including Plots

You can also embed plots, for example:

``` r
#p <- chorddiag(out_spatial2, groupColors = cols2, showGroupnames = FALSE,groupPadding = 2,showTooltips = #FALSE,tickInterval=10,ticklabelFontsize=0)
#p
#setwd("/Users/veronicapagowski/Desktop/interactive_app/interactive_review")
#saveWidget(p, "chord_test.html", selfcontained = TRUE, knitrOptions = list())

library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
            geom_bar(position = "dodge")
ggplotly(p)
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-511426b0697a1074142b" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-511426b0697a1074142b">{"x":{"data":[{"orientation":"v","width":[0.1125,0.1125,0.1125,0.1125,0.112500000000001],"base":[0,0,0,0,0],"x":[0.60625,1.60625,2.60625,3.60625,4.60625],"y":[210,96,84,205,146],"text":["count:  210<br />cut: Fair<br />clarity: I1","count:   96<br />cut: Good<br />clarity: I1","count:   84<br />cut: Very Good<br />clarity: I1","count:  205<br />cut: Premium<br />clarity: I1","count:  146<br />cut: Ideal<br />clarity: I1"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(68,1,84,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"I1","legendgroup":"I1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.1125,0.112500000000001],"base":[0,0,0,0,0],"x":[0.71875,1.71875,2.71875,3.71875,4.71875],"y":[466,1081,2100,2949,2598],"text":["count:  466<br />cut: Fair<br />clarity: SI2","count: 1081<br />cut: Good<br />clarity: SI2","count: 2100<br />cut: Very Good<br />clarity: SI2","count: 2949<br />cut: Premium<br />clarity: SI2","count: 2598<br />cut: Ideal<br />clarity: SI2"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(70,51,126,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"SI2","legendgroup":"SI2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.1125,0.112500000000001],"base":[0,0,0,0,0],"x":[0.83125,1.83125,2.83125,3.83125,4.83125],"y":[408,1560,3240,3575,4282],"text":["count:  408<br />cut: Fair<br />clarity: SI1","count: 1560<br />cut: Good<br />clarity: SI1","count: 3240<br />cut: Very Good<br />clarity: SI1","count: 3575<br />cut: Premium<br />clarity: SI1","count: 4282<br />cut: Ideal<br />clarity: SI1"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(54,92,141,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"SI1","legendgroup":"SI1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.1125,0.112500000000001],"base":[0,0,0,0,0],"x":[0.94375,1.94375,2.94375,3.94375,4.94375],"y":[261,978,2591,3357,5071],"text":["count:  261<br />cut: Fair<br />clarity: VS2","count:  978<br />cut: Good<br />clarity: VS2","count: 2591<br />cut: Very Good<br />clarity: VS2","count: 3357<br />cut: Premium<br />clarity: VS2","count: 5071<br />cut: Ideal<br />clarity: VS2"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(39,127,142,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"VS2","legendgroup":"VS2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.112500000000001,0.112500000000001],"base":[0,0,0,0,0],"x":[1.05625,2.05625,3.05625,4.05625,5.05625],"y":[170,648,1775,1989,3589],"text":["count:  170<br />cut: Fair<br />clarity: VS1","count:  648<br />cut: Good<br />clarity: VS1","count: 1775<br />cut: Very Good<br />clarity: VS1","count: 1989<br />cut: Premium<br />clarity: VS1","count: 3589<br />cut: Ideal<br />clarity: VS1"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(31,161,135,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"VS1","legendgroup":"VS1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.112500000000001,0.112500000000001],"base":[0,0,0,0,0],"x":[1.16875,2.16875,3.16875,4.16875,5.16875],"y":[69,286,1235,870,2606],"text":["count:   69<br />cut: Fair<br />clarity: VVS2","count:  286<br />cut: Good<br />clarity: VVS2","count: 1235<br />cut: Very Good<br />clarity: VVS2","count:  870<br />cut: Premium<br />clarity: VVS2","count: 2606<br />cut: Ideal<br />clarity: VVS2"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(74,193,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"VVS2","legendgroup":"VVS2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.112500000000001,0.112500000000001],"base":[0,0,0,0,0],"x":[1.28125,2.28125,3.28125,4.28125,5.28125],"y":[17,186,789,616,2047],"text":["count:   17<br />cut: Fair<br />clarity: VVS1","count:  186<br />cut: Good<br />clarity: VVS1","count:  789<br />cut: Very Good<br />clarity: VVS1","count:  616<br />cut: Premium<br />clarity: VVS1","count: 2047<br />cut: Ideal<br />clarity: VVS1"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(159,218,58,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"VVS1","legendgroup":"VVS1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.1125,0.1125,0.1125,0.112500000000001,0.112500000000001],"base":[0,0,0,0,0],"x":[1.39375,2.39375,3.39375,4.39375,5.39375],"y":[9,71,268,230,1212],"text":["count:    9<br />cut: Fair<br />clarity: IF","count:   71<br />cut: Good<br />clarity: IF","count:  268<br />cut: Very Good<br />clarity: IF","count:  230<br />cut: Premium<br />clarity: IF","count: 1212<br />cut: Ideal<br />clarity: IF"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(253,231,37,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"IF","legendgroup":"IF","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Fair","Good","Very Good","Premium","Ideal"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Fair","Good","Very Good","Premium","Ideal"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"cut","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-253.55,5324.55],"tickmode":"array","ticktext":["0","1000","2000","3000","4000","5000"],"tickvals":[0,1000,2000,3000,4000,5000],"categoryorder":"array","categoryarray":["0","1000","2000","3000","4000","5000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"clarity","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"286479c122a9":{"x":{},"fill":{},"type":"bar"}},"cur_data":"286479c122a9","visdat":{"286479c122a9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
