#Plot Histograms from Figure 3 (number of mechanisms, found in bottom row)
#Also plot supplemental figure 2

#Read in modified csv file while contains all the labels except taxa are removed
#GSpatial category (test for genetic gap and don't find it) is also removed
#Also remove Genetic, Spatial, and Community categories

setwd("/Users/veronicapagowski/Desktop/interactive_app/interactive_review")

df<- read.csv("articles_03_24_histograms.csv", header = TRUE)
df<-df %>% 
  filter(str_detect(Category, pattern = "nogap",negate=TRUE))
df<-df %>% 
  filter(str_detect(Category, pattern = "Deep_sea,",negate=TRUE))


#Split into year groups
df$yr_grouping <- cut(df$year, seq(1980, 2025, 5),dig.lab = 5)
df$yr_grouping<-gsub("\\[|\\]", "", df$yr_grouping)
df$yr_grouping<-gsub("[()]", "", df$yr_grouping)

#Count categories (and remove 1 - the small or large label, each paper includes one)
df$Number<-str_count(df$Category, ',')
df$Number <- df$Number-1

############## BIG SPATIAL SCALES ONLY ##############
dfb<-df %>% 
  filter(str_detect(Category, pattern = "Bg,"))
############## SMALL SPATIAL SCALES ONLY ##############
dfs<-df %>% 
  filter(str_detect(Category, pattern = "Sm,"))

#Plot number of mechanisms implicated (all gaps, big and small)
library(stringr)

# Remove really old studies outside main categories and format to show summary with means + deviations
msd <- df %>%                           # Get mean & standard deviation by group
  drop_na(yr_grouping) %>%
  group_by(yr_grouping) %>%
  summarise_at(vars(Number),
               list(mean = mean,
                    sd = sd)) %>% 
  as.data.frame()
msd       


#Plot supplemental figure
ggplot(msd,                               
       aes(x = yr_grouping,
           y = mean)) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line())+ 
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(angle = 45,hjust = .5, vjust = .5)) + 
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd)) +
  geom_point()


##do this for small gaps
#Plot number of mechanisms implicated (all gaps, big and small)
library(stringr)

dfs$yr_grouping <- cut(dfs$year, seq(1980, 2025, 5),dig.lab = 5)

dfs$Number<-str_count(dfs$Category, ',')
dfs$Number <- dfs$Number-1


dfb$yr_grouping <- cut(dfb$year, seq(1980, 2025, 5),dig.lab = 5)

dfb$Number<-str_count(dfb$Category, ',')
dfb$Number <- dfb$Number-1


msds <- df %>%                           # Get mean & standard deviation by group
  drop_na(yr_grouping) %>%
  group_by(yr_grouping) %>%
  summarise_at(vars(Number),
               list(mean = mean,
                    sd = sd)) %>% 
  as.data.frame()

#Plot Supp fig2
ggplot(msds,                               
       aes(x = yr_grouping,
           y = mean)) + 
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd)) +
  geom_point()


# Plot figure 3 histograms (small scales)
dfs %>% 
  ggplot(aes(x = Number))+ geom_histogram(bins=6,fill="darkseagreen",color="black",alpha=0.7) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(),
        strip.text.x = element_blank())+ 
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  scale_x_continuous(breaks = round(seq(min(dfs$Number), max(dfs$Number), by = 1),1))


# Plot figure 3 histograms (big scales)
dfb %>% 
  ggplot(aes(x = Number))+ geom_histogram(bins=5,fill="darkseagreen",color="black",alpha=0.7) +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(),
        strip.text.x = element_blank())+ 
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  scale_x_continuous(breaks = round(seq(min(dfb$Number), max(dfb$Number), by = 1),1))


