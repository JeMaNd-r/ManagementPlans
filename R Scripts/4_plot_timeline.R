#- - - - - - - - - - - - - - - - -#
# Text mining of management plans #
#     author: Romy Zeiss          #
#                                 #
#     4. plot timeline EU         #
#- - - - - - - - - - - - - - - - -#

# Set working directories
folder.wd; work.wd; figu.wd
setwd(work.wd); getwd()

# Load packages
library(tidyverse) 
#library(ggplot2)  #is included in tidyverse
library(wordcloud) #as the name says: to create word clouds
library(wordcloud2)
#library(scales)   #for plot time-line policies
library(lubridate) #to transform date columns in one column

#- - - - - - - - - - - - - - - - - - - - - - -
## Load data ####

# Load term document matrices
#setwd(folder.wd)
#files <- list.files(pattern = "df_all.csv$")
#files #contains names of pdf files

# Load EU policies with year
setwd(work.wd)
policy <- read.csv("../Timeline_EUpolicies.csv", sep=",")
colnames(policy) <- c("Year", colnames(policy)[-1])
head(policy)

#- - - - - - - - - - - - - - - - - - - - - - -
## Create a time-line of policies based on published date ####
# see https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

## first: prepare data

# define date column
policy$month <- match(policy$Month,month.name) #get numbers for month names
policy[policy$Month=="",]$month <- 12  #assign missing months as December
policy[is.na(policy$Date),]$Date <- 28 #assign missing days

## Define date format
policy$date <- paste0(policy$Year, "-", policy$month,"-", policy$Date)
policy$date <- as.Date(policy$date, format='%Y-%m-%d')

# replace NAs (before "in prep.") by date in 2022
policy[is.na(policy$date),]$date <- as.Date("2022-01-01", format='%Y-%m-%d')
policy <- policy[with(policy, order(date)),] #sort dates in increasing order
policy$year <- as.numeric(stringr::str_sub(policy$date, 1, 4))
head(policy)

# remove ordinances
policy <- policy[policy$Type!="ordinance",]

# remove irrelevant lines (i.e. associations, publications, research, development)
policy <- policy[policy$Type!="association",]
policy <- policy[policy$Type!="publication",]
policy <- policy[policy$Type!="research",]
policy <- policy[policy$Type!="development",]

# order the levels in columns
policy$Level <- factor(policy$Level, levels = c("Germany", "EU", "UN", "international"))

# estimate number of policies per year, level and type
freq <- data.frame(year=unique(policy$year), sum=NA, event=NA, association=NA, 
                   legislation=NA, funding=NA, international=NA, EU=NA, Germany=NA)
for(yr in freq$year){
  # estimate number of policies per year:
  temp.policy <- policy[policy$year==yr,]
  freq[freq$year==yr,"sum"] <- nrow(temp.policy)
  
  #c(table(temp.policy$Relevance))
  
  # calculate nrow per type (event, association etc.)
  freq[freq$year==yr,"event"] <- nrow(temp.policy[temp.policy$Type=="event",])
  freq[freq$year==yr,"association"] <- nrow(temp.policy[temp.policy$Type=="association",])
  freq[freq$year==yr,"legislation"] <- nrow(temp.policy[temp.policy$Type=="legislation",])
  freq[freq$year==yr,"funding"] <- nrow(temp.policy[temp.policy$Type=="funding",])
  freq[freq$year==yr,"awareness"] <- nrow(temp.policy[temp.policy$Type=="awareness raising",])
  freq[freq$year==yr,"research"] <- nrow(temp.policy[temp.policy$Type=="research",])
  freq[freq$year==yr,"data system"] <- nrow(temp.policy[temp.policy$Type=="data system",])
  
  # estimate number of policies per level (international, EU, Germany)
  freq[freq$year==yr,"international"] <- nrow(temp.policy[temp.policy$Level=="international",])
  freq[freq$year==yr,"UN"] <- nrow(temp.policy[temp.policy$Level=="UN",])
  freq[freq$year==yr,"EU"] <- nrow(temp.policy[temp.policy$Level=="EU",])
  freq[freq$year==yr,"Germany"] <- nrow(temp.policy[temp.policy$Level=="Germany",])
  
  # estimate number of policies per relevance (climate, soil, transformations)
  freq[freq$year==yr,"biodiv"] <- nrow(temp.policy[temp.policy$Relevance=="biodiversity",])
  freq[freq$year==yr,"soil"] <- nrow(temp.policy[temp.policy$Relevance=="soil",])
  freq[freq$year==yr,"agri"] <- nrow(temp.policy[temp.policy$Relevance=="agriculture",])
  freq[freq$year==yr,"transformation"] <- nrow(temp.policy[temp.policy$Relevance=="transformation",])
  freq[freq$year==yr,"env"] <- nrow(temp.policy[temp.policy$Relevance=="environment",])
  freq[freq$year==yr,"sustain"] <- nrow(temp.policy[temp.policy$Relevance=="sustainability",])
  freq[freq$year==yr,"climate"] <- nrow(temp.policy[temp.policy$Relevance=="climate",])
  freq[freq$year==yr,"soil.div"] <- nrow(temp.policy[temp.policy$Relevance=="soil diversity",])
  freq[freq$year==yr,"develop"] <- nrow(temp.policy[temp.policy$Relevance=="development",])
  freq[freq$year==yr,"reversely"] <- nrow(temp.policy[temp.policy$Relevance=="reversely",])
}
head(freq)

## Save frequency table (per year etc.)
setwd(work.wd)
#write.csv(freq, "../Policies_per_year.csv")
freq <- read.csv("../Policies_per_year.csv"); freq <- freq[,-1]

#- - - - - - - - - - - - - - - - - - - - - - -
## Plot number of policies over time ####
# first, get from wide format into long format
freq2 <- gather(freq[,c("year", "Germany", "EU", "UN","international")],
                Level, number, Germany:international, factor_key=TRUE)
freq2$Level <- factor(freq2$Level, levels = c("international","UN", "EU","Germany"))

## A line graph
# setwd(figu.wd); pdf("Policies_per_year_lines.pdf")
ggplot(data=freq, aes(x=year))+
  geom_line(aes(y=Germany, col="Germany"))+  #to do: change colors
  geom_line(aes(y=EU, col="EU"))
# # warning: line connects points -> 0 is not shown as zero...
# dev.off()

# setwd(figu.wd); pdf("Policies_per_year_lines.pdf")
ggplot(data=freq2[freq2$year>=2005,], aes(col=Level, x=year, y=number))+
  geom_line()+
  facet_grid(Level~.)
# dev.off()

## As bar graph
#setwd(figu.wd); pdf("Policies_per_year_barplot.pdf")
ggplot(freq2[freq2$year>=2005,], aes(fill=Level, x=year, y=number)) +
  geom_bar(position="stack", stat="identity")+
  theme(legend.position = "")+
  scale_fill_manual(values=c("#C77CFF","#00BFC4","#7CAE00","#F8766D"))+
  facet_grid(Level~.)
dev.off()

## As dot plot graph (i.e. stacked points)
#setwd(figu.wd); pdf("Policies_per_year_dotplot_level.pdf")
ggplot(policy, aes(col=Level, fill=Level, x=year)) + 
  geom_dotplot(method="histodot", binwidth=1, stackdir="up", 
               stackgroups=T, dotsize = 1)+
  ylim(0,25)
  # use specific colors for levels
  #scale_fill_manual(values=c("chocolate2", "blue4", "cornflowerblue", "darkolivegreen4"))+
  #scale_color_manual(values=c("chocolate2", "blue4", "cornflowerblue", "darkolivegreen4"))
dev.off()

## combine EU, UN and international as one column "international" ####
policy <- policy[!is.na(policy$Level), ]
policy$Level2 <- "Germany"
policy[policy$Level!="Germany",]$Level2 <- "international"

leveling <- c("reversely", "development", "soil diversity", "sustainability", 
              "climate", "environment", "agriculture", "biodiversity", 
              "transformation", "soil")
policy$Relevance <- factor(policy$Relevance, 
                           levels= leveling)

#setwd(figu.wd); pdf("Policy_relevance_levels_barplot.pdf")
ggplot(policy, aes(fill=Level, y=Relevance))+
  geom_bar() +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom")
dev.off()

ggplot(policy, aes(fill=Type, y=Relevance))+
  geom_bar()

## Dot plot for types (i.e. stacked points)
#setwd(figu.wd); pdf("Policies_per_year_dotplot_type.pdf")
ggplot(policy, aes(col=Type, fill=Type, x=year)) + 
  geom_dotplot(method="histodot", binwidth=1, stackdir="up", stackgroups=T)+
  facet_grid(Level2~.)
# to do: change colors, change order of types (legislation first)
dev.off()

# Dot plot for relevance (i.e. stacked points)
#setwd(figu.wd); pdf("Policies_per_year_dotplot_relevance.pdf")
ggplot(policy, aes(col=Relevance, fill=Relevance, x=year)) + 
  geom_dotplot(method="histodot", binwidth=2, stackdir="up", stackgroups=T)
# to do: change colors
dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -
## now we will make a pretty time-line with labels ####

# re-define levels (EU, international, Germany)
unique(policy$Level)
policy$direction = -1
policy[policy$Level!="Germany",]$direction <- 1
policy$Level <- as.factor(policy$Level)
head(policy)

# to avoid overlap of text displayed in plot:
#positions <- c(0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75)

#policy <- policy[order(policy$date),]

df1 <- policy[policy$Level2=="Germany",]
df2 <- policy[policy$Level2!="Germany",]

line_pos1 <- data.frame("date"=unique(df1[df1$Label!="",]$date),
                       "position"=seq(from=0.25,by=0.5, length.out=length(unique(df1[df1$Label!="",]$date))))
line_pos2 <- data.frame("date"=unique(df2[df2$Label!="",]$date),
                       "position"=seq(to=0.25, by=-0.5, length.out=length(unique(df2[df2$Label!="",]$date))))

line_pos <- rbind(line_pos1, line_pos2)

# line_pos <- data.frame("date"=unique(policy[policy$Label!="",]$date),
#               "position"=rep(positions, length.out=length(unique(policy[policy$Label!="",]$date))))

df <- merge(x=policy, y=line_pos, by="date")#, all.x = TRUE)
df <- df[with(df, order(date, Relevance)), ]
head(df)

# also, if there are multiple objects within one month:
text_offset <- 0.15

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$year_count <- ave(df$year==df$year, df$year, FUN=cumsum)

# define text positions for labels (only those with abbreviation)
df.2 <- df[df$Label!="",]
df.2$text_position <- ((df.2$year_count * text_offset) + df.2$position) * df.2$direction
df <- merge(df, df.2[,c("text_position", "Policies")], by="Policies", all.x=T)

## Define groups of different objects (e.g. events, associations, funding)
unique(df$Relevance)
df$Type <- factor(df$Relevance, levels=unique(df$Relevance), ordered=TRUE)

#df$line_position <- df$year_count * df$direction
head(df)

#- - - - - - - - - - - - - - - - - - - - - - -
## PLOT ####
#df <- df[df$year>="1960",]
setwd(figu.wd)

#pdf("Policy_timeline2.pdf")
ggplot(df[df$Label!="",],aes(x=date,y=0, col=Relevance, 
                          label=Label))+
  
  scale_color_manual(values=c("purple", "orange", "darkolivegreen3","dodgerblue", "darkgreen",
                              "burlywood4", "darkolivegreen3", "burlywood3"))+
  theme_classic()+
  
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, color = "black", size=0.3)+
  
  # Plot vertical segment lines for labels
  geom_segment(aes(y=text_position,yend=0,xend=date), 
               color='black', size=0.2)+
  
  # Plot scatter points at zero and date
  geom_point(aes(y=0), size=2)+
  
  # Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   #axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "left")+
  
  # Show labels for important policy
  geom_text(aes(y=text_position,label=Label), size=4)

dev.off()

#- - - - - - - - - - - - - - - - - - - - - - -

