######## Kaggle Challenge #######
##### The data set can be used to explore how 
#age/height/weight tendencies have changed over time 
#due to changes in game philosophy and player development
#strategies. Also, it could be interesting to see 
#how geographically diverse the NBA is and 
#how oversees talents have influenced it. A longitudinal study on players' career arches can also be performed.


# Load data 
setwd("~/Desktop/Kaggle")
nba.df = read.csv('all_seasons.csv',stringsAsFactors = F, header = T)

# Check missing values 
miss = function(x){sum(is.na(x))}
print(apply(nba.df, 2, miss))

# packages 
install.packages('tidyverse')
library(tidyverse)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages('tidyr')
library(tidyr)
install.packages('gganimate')
library(gganimate)
install.packages('fmsb') 
library(fmsb)


### Data analysis 
nba.df$X = NULL


# Player's Age 
age = nba.df %>%
  group_by(season)%>%
  summarise(avg.age = mean(age))%>%
  arrange(desc(avg.age))%>%
  ungroup()

age %>%
  ggplot(aes(x=season, y=avg.age,group = 1)) +
  geom_line(color="blue") +
  geom_point(shape=21, color="black", fill="dark blue", size=6) +
  theme_ipsum()+theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  xlab('NBA Season')+ylab('Average Age')+
  ggtitle('NBA Players Age Over Time')

# Player's stats 
height = nba.df %>%
  group_by(season)%>%
  summarise(avg.height = mean(player_height))%>%
  arrange(desc(avg.height))%>%
  ungroup()

height %>%
  ggplot(aes(x=season, y=avg.height,group = 1)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum()+theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  xlab('NBA Season')+ylab('Average Height')+
  ggtitle('NBA Players Height Over Time')


# Weight 
Weight = nba.df %>%
  group_by(season)%>%
  summarise(avg.weight = mean(player_weight))%>%
  arrange(desc(avg.weight))%>%
  ungroup()

Weight %>%
  ggplot(aes(x=season, y=avg.weight,group = 1)) +
  geom_line(color="red") +
  geom_point(shape=21, color="black", fill=" dark red", size=6) +
  theme_ipsum()+theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  xlab('NBA Season')+ylab('Average Weight')+
  ggtitle('NBA Players Weight Over Time')

## Hypothesis: oversees talents have influenced NBA a lot. 
Oversea.player = nba.df %>%
  filter(country != 'USA')%>%
  group_by(season)%>%
  summarise(total.player = n())

Oversea.player %>%
  ggplot(aes(x = season,y=total.player,group = 1))+
  geom_line(color = ' dark orange') + geom_point(color = 'yellow')+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1,color = 'orange'))+
  theme(axis.text.y = element_text(color = 'orange'))+
  xlab('NBA Season')+ylab('Total Players')+
  ggtitle('The Trend of Overseas Player')+
  theme(plot.title = element_text(size = 15,face = 'bold',color = 'orange'))+
  theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  legend.background = element_rect(fill = "#f5f5f2", color = NA))



# Player's information 
Kevin = nba.df %>%
  filter(player_name == 'Kevin Durant')%>%
  arrange(desc(pts))

Kevin.data <- data.frame(Pts=c(40, 0, 32),
                         Reb=c(40, 0, 7.4),
                         Ast=c(40, 0, 5.5),
                         Net_rating=c(40, 0, 8.0),
                         Oreb_pct=c(40, 0, 0.022),
                         Dreb_pct=c(40, 0, 0.184),
                         usg_pct=c(40, 0, 0.327),
                         ts_pct=c(40, 0, 0.635),
                         ast_pct=c(40, 0, 0.259))
# The default radar chart 
radarchart(Kevin.data)

radarchart(Kevin.data, axistype=1, 
#custom polygon
pcol=rgb(0.2,0.3,0.3,0.9) , pfcol=rgb(0.2,0.3,0.3,0.5) , plwd=4 , 
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.8,
#custom labels
vlcex=0.8)

###  Kobe Bryant
Kobe = nba.df %>%
  filter(player_name == 'Kobe Bryant')%>%
  filter(season == '2010-11')
arrange(desc(pts))
# Draw the Chart 
Kobe.data <- data.frame(Pts=c(40, 0, 35.4),
                         Reb=c(40, 0, 5.3),
                         Ast=c(40, 0, 4.5),
                         Net_rating=c(40, 0, 4.7),
                         Oreb_pct=c(40, 0, 0.026),
                         Dreb_pct=c(40, 0, 0.127),
                         usg_pct=c(40, 0, 0.348),
                         ts_pct=c(40, 0, 0.559),
                         ast_pct=c(40, 0, 0.228))
radarchart(Kobe.data)
radarchart(Kobe.data, axistype=1, 
           pcol=rgb(0.5,0.1,0.8,0.9) , pfcol=rgb(0.5,0.1,0.8,0.9) , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,40,10), cglwd=0.8,
           vlcex=0.8)

# Lebron James 
Lebron = nba.df %>%
  filter(player_name == 'LeBron James')%>%
  arrange(desc(pts))
Lebron.data <- data.frame(Pts=c(40, 0, 27.5),
                        Reb=c(40, 0, 8.6),
                        Ast=c(40, 0, 9.1),
                        Net_rating=c(40, 0, 1.6),
                        Oreb_pct=c(40, 0, 0.033),
                        Dreb_pct=c(40, 0, 0.201),
                        usg_pct=c(40, 0, 0.310),
                        ts_pct=c(40, 0, 0.621),
                        ast_pct=c(40, 0, 0.432))
radarchart(Lebron.data)
radarchart(Lebron.data, axistype=1, 
           pcol=rgb(0.5,0.7,0.3,0.9) , pfcol=rgb(0.5,0.7,0.3,0.9) , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,40,10), cglwd=0.8,
           vlcex=0.8)

  