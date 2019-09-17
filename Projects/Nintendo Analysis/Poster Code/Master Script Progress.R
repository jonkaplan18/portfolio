library(tidyverse)
library(rlang)
library(rockchalk) #summarize is maksed from dplyr. How to change?
library(ggthemr)
#install.packages("data.table")
library(data.table) #transpose, between, first, last, :=,
library(dplyr)
library(ggpubr)
library(plyr)

getwd()
setwd("C:\\Users\\13302\\Desktop\\IST 719 Viz\\IST 719 Project")

base_df <- read.csv("CleanVG.csv")

levels(base_df$Platform)

Comb_levels <- base_df

Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs =c("3DS", "DS", "GBA", "GC", "N64", "NES", "SNES","Wii","WiiU", "GB"), newLabel = "Nintendo")

levels(Comb_levels$Platform)

table(base_df$Genre)

#Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs =c("3DS", "DS", "GBA", "GC", "N64", "NES", "SNES","Wii","WiiU"), newLabel = "Nintendo")


#play station 

#  PS, PS2, PS3, PS4, PSP, PSV, 

Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs =c("PS", "PS2", "PS3", "PS4", "PSP", "PSV"), newLabel = "PlayStation")

# Xbox

# X360, XB, XOne

Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs =c("X360", "XB", "XOne"), newLabel = "Xbox")
levels(Comb_levels$Platform)


# Sega

# DC, GEN, GG, SAT, SCD
Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs = c("DC", "GEN", "GG", "SAT", "SCD"), newLabel = "Sega")

levels(Comb_levels$Platform)

# Other
Comb_levels$Platform <- combineLevels(Comb_levels$Platform, levs = c("PCFX", "TG16", "WS", "2600", "3DO", "NG"), newLabel = "Other")


base_df1 <- na.omit(base_df)

base_df1$Year_of_Release <- as.Date(base_df1$Year_of_Release, "%Y")

base_df1$Year_of_Release <- format(as.Date(base_df1$Year_of_Release, format="%d/%m/%Y"),"%Y")



base_df1$Year_of_Release <- as.numeric(base_df1$Year_of_Release)

base_df1 <- base_df1 %>% filter(Year_of_Release <= 2016)

base_df1$Year_of_Release <- as.character(base_df1$Year_of_Release)

base_df1$Year_of_Release <- as.Date(base_df1$Year_of_Release, "%Y")

base_df1$Year_of_Release <- format(as.Date(base_df1$Year_of_Release, format="%d/%m/%Y"),"%Y")

base_df <- base_df[base_df$Rating != "AO",]
base_df <- base_df[base_df$Rating != "RP",]
base_df <- base_df[base_df$Rating != "K-A",]
base_df <- base_df[base_df$Rating != "",]
base_df <- base_df[base_df$Rating != "EC",]
base_df$Rating <- droplevels(base_df$Rating)

levels(base_df$Rating)
base_df1 <- base_df1[base_df1$Rating != "AO",]
base_df1 <- base_df1[base_df1$Rating != "RP",]
base_df1 <- base_df1[base_df1$Rating != "K-A",]
base_df1 <- base_df1[base_df1$Rating != "",]
base_df1$Rating <- droplevels(base_df1$Rating)

table(base_df1$Rating)

# proper ratings


# 1st = base blue, 2nd = base red, 3rd = base green, 4th = base yellow, 5th = 1st blue square, 6th = lighter red, 7th = green light 8th = light yellow
mario_colours <- c('#5311A6', '#DC0442', '#60D904', '#F4E005', '#310666', '#870027', '#3A8500', '#968900')
# you have to add a colour at the start of your palette for outlining boxes, we'll use a grey:
mario_with_grey <- c("#555555", mario_colours)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
mario_theme <- define_palette(
  swatch = mario_with_grey, # colours for plotting points and bars
  gradient = c(lower = mario_colours[1L], upper = mario_colours[2L]), #upper and lower colours for continuous colours
  background = "#EEEEEE" #defining a grey-ish background 
)

# set the theme for your figures:

ggthemr(mario_theme)

unique(Comb_levels$Platform)

#summarize is masked.... how to unmask
#You can unload the package which has masked functions and then reload it. It will regain precedence in the searchpath:
#regain mask precedence

unloadNamespace("tidyverse")
library(tidyverse)


Comb_levels %>%  select(Platform, Year_of_Release, Global_Sales) %>% group_by(Platform, )



ggplot(Comb_levels, aes(x = Year_of_Release, y = Global_Sales, color = Platform)) + geom_point()

# triple colon will set to hidden functions
#zoo:::.onLoad


to_line_chart <- Comb_levels %>% select(Platform, Year_of_Release, Global_Sales) %>%  group_by(Platform, Year_of_Release) %>%  dplyr:::summarize(Year_Sale = sum(Global_Sales))


to_line_chart <- to_line_chart %>%  spread(1,3)


to_line_chart <- to_line_chart[-c(27,62,  63, 88, 106, 139),]

write.csv(to_line_chart, file = "global with all data.csv")

to_line_chart <- base_df %>% select(Platform, Year_of_Release, JP_Sales) %>%  group_by(Platform, Year_of_Release) %>%  dplyr:::summarize(Year_Sale = sum(JP_Sales))

to_line_chart <- to_line_chart[-c(27,62,  63, 88, 106, 139),]

to_line_chart <- to_line_chart %>%  spread(1,3)

write.csv(to_line_chart, file = "JP with all data.csv")


ggplot(to_line_chart, aes(x = Year_of_Release, y = Year_Sale, color = Platform)) + geom_line(aes(group = Platform))
geom_line(aes(group = Subject)
# each group consists of only one observation. Do you need to adjust the group aesthetic?

ggplot(to_line_chart, aes(x = Year_of_Release, y = Year_Sale, color = Platform)) + geom_line(aes(group = Platform)) + ggtitle("Sales by Console by Year")+ theme(axis.text.x = element_text(angle= 90))


infogram_spread <- to_line_chart %>%  spread(1,3)

write.csv(infogram_spread, file = "infogram_spread.csv", row.names = FALSE)


write.csv(to_line_chart, file = "to_line_chart.csv",row.names=FALSE)

?spread


NA_line_chart <- Comb_levels %>% select(Platform, Year_of_Release, NA_Sales) %>%  group_by(Platform, Year_of_Release) %>%  dplyr:::summarize(Year_Sale = sum(NA_Sales))

NA_line_chart <- NA_line_chart[-c(27,62,  63, 88, 106, 139),]

NA_spread <- NA_line_chart %>%  spread(1,3)



write.csv(NA_spread, file = "NA_spread.csv", row.names=FALSE)




JP_line_chart <- Comb_levels %>% select(Platform, Year_of_Release, JP_Sales) %>%  group_by(Platform, Year_of_Release) %>%  dplyr:::summarize(Year_Sale = sum(JP_Sales))

JP_line_chart <- JP_line_chart[-c(27,62, 63, 88, 106, 139),]

JP_spread <- JP_line_chart %>%  spread(1,3)
write.csv(JP_spread, file = "JP_spread.csv", row.names = FALSE)



#some kind of market share analysis



base_85 <- base_df %>% filter(Year_of_Release == 1985)


base_NA <- na.omit(base_df)

NA_platformer <- base_NA %>%  filter(Genre == "Platform")

not_nintendo <- NA_platformer %>%  filter(Publisher != "Nintendo")

mean(not_nintendo$Critic_Score) # 68.87


nintendo_plat <- NA_platformer %>%  filter(Publisher == "Nintendo")

mean(nintendo_plat$Critic_Score) # 79.7





?top_n # wt = variable for ordering


Comb_levels %>%  group_by(Platform) %>%  top_n(10, wt = Global_Sales)


#need to put % of total profit column

base_df1 %>%  mutate(Total_Market_Share = 100*(Global_Sales/sum(Global_Sales)))

total_market <- Comb_levels %>%  mutate(Total_Market_Share = 100*(Global_Sales/sum(Global_Sales))) %>%  group_by((Platform)) %>% top_n(10, wt = Total_Market_Share)



sum(Comb_levels$Global_Sales)

# 8920.3


# 82.53/8920.3
colnames(total_market)

total_market_csv <- total_market[, c(2,17)]

write.csv(total_market_csv, file = "top_ten_market.csv" ,row.names = FALSE)




write.csv(MyData, file = "MyData.csv",row.names=FALSE)






ggplot(total_market, aes(x = Platform, y = Total_Market_Share)) + geom_col()


#g + geom_bar(aes(fill = drv))


total_market


gtally <- base_df %>%  group_by(Platform, Genre) %>%  add_tally()




write.csv(Comb_levels, file="Combined levels.csv", row.names= FALSE)




PlatformCounts <- Comb_levels %>%  select(Platform, Genre) %>%  group_by(Genre, Platform) %>%  add_tally()

PlatformCounts <- distinct(PlatformCounts)


base_df1 %>%  select(Name, Global_Sales, Platform, Genre) %>%  filter(Genre == "Platform", Platform == "PC") %>%  mutate(tots = sum(Global_Sales))





Comb_levels %>%  select(Name, Genre, Platform, Global_Sales) %>% filter(Platform == "Nintendo", Genre == "Shooter") %>% arrange(desc(Global_Sales))





#T, E, M, E10+

base_df1$Rating <- factor(base_df1$Rating, levels=c("T","E","M","E10+"))

ggplot(base_df1) +  geom_histogram(stat = "count",aes(x = Rating, fill = Rating)) + coord_flip()

#E10+ M E T

base_df1$Rating <- factor(base_df1$Rating, levels=c("E10+","M","E","T"))


ggplot(base_df) +  geom_histogram(stat = "count",aes(x = Rating, fill = Rating)) + coord_flip() + ggtitle("Count of games by ESRB Rating") + xlab("ESRB Rating") + ylab("Count of Games")

ggplot(base_df) +  geom_histogram(stat = "count",aes(x = Genre)) + coord_flip() + ggtitle("Count of games by Genre") + xlab("Genre Rating") + ylab("Count of Games")


scale_fill_manual(values=c("#F4B2BB","#95BB50","#F6BE73","#79747B","#9C94CD","#3B83AA", "#DC4B43")
                  
                  
plat <- base_df %>%  select(Genre) %>%  filter(Genre == "Platform")

dim(plat)                  
                  
ggplot(base_df) +  geom_histogram(stat = "count",aes(x = Genre)) + coord_flip() + ggtitle("Count of games by Genre") + xlab("Genre Rating") + ylab("Count of Games")





non_Nintendo <- Comb_levels[Comb_levels$Platform != "Nintendo",]

non_Nintendo$Genre <- droplevels(non_Nintendo$Genre)

Nintendo_only <- Comb_levels[Comb_levels$Platform == "Nintendo",]

ggplot(non_Nintendo) + geom_histogram(stat = "count", aes(x = Genre))

unique(base_df$Genre)

table(non_Nintendo$Genre)
                  

table(non_Nintendo$Genre)

#Action
#Sports
#SHooter
#Role-Playing
#Racing
#Adventure
#Misc
#Fighting
#Strategy
#Simulation
#Platform
#Puzzle



non_Nintendo$Genre <- factor(non_Nintendo$Genre, levels=c("Puzzle", "Platform", "Simulation", "Strategy", "Fighting", "Misc", "Adventure", "Racing", "Role-Playing", "Shooter", "Sports", "Action"))


ggplot(non_Nintendo) + geom_histogram(stat = "count", aes(x = Genre)) + coord_flip() + ggtitle("Distrubtion of Non-Nintendo Video Game Genres") + geom_text(aes(label=freq)), vjust =1.6, color = "white", size=3.5)



ggplot(Nintendo_only) + geom_histogram(stat = "count", aes(x = Genre))

table(Nintendo_only$Genre)





p <- ggplot(data = genrecount, aes(x=x, y=freq)) + geom_bar(stat = "identity") + geom_text(aes(label = freq), vjust=1.6, color="white", size = 3.5) + coord_flip()



genrecount <- da

genrecounts <- plyr::count(non_Nintendo$Genre)

p <- ggplot(genrecounts, aes(x, freq)) + geom_bar(stat="identity", position = "dodge") +geom_text(aes(label=freq, family = ("helvetica")), vjust= 0,hjust= -.25, color = "black", position = position_dodge(width=0.9), size = 3.5) + coord_flip() +ylab("Count of Games") +xlab("Genres") + ggtitle("Distribution of non-Nintendo Game Genres")
p

?geom_text

p
vignette("ggplot2-specs")

#Action
#Misc
#Sports
#

count(Nintendo_only$Genre)


non_Nintendo$Genre <- factor(non_Nintendo$Genre, levels=c("Puzzle", "Platform", "Simulation", "Strategy", "Fighting", "Misc", "Adventure", "Racing", "Role-Playing", "Shooter", "Sports", "Action"))

genrecounts <- plyr::count(non_Nintendo$Genre)

Nintendo_only$Genre <- factor(Nintendo_only$Genre, levels=c("Strategy", "Fighting", "Shooter", "Racing", "Puzzle", "Adventure", "Simulation", "Platform", "Role-Playing", "Sports", "Misc", "Action"))

Nintendo_Genres <- plyr::count(Nintendo_only$Genre)


mario_colours <- c('#5311A6', '#DC0442', '#60D904', '#F4E005', '#310666', '#870027', '#3A8500', '#968900','#6933AD','#E63567',  '#80E234', '#FFEE3A')
# you have to add a colour at the start of your palette for outlining boxes, we'll use a grey:
mario_with_grey <- c("#555555", mario_colours)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
mario_theme <- define_palette(
  swatch = mario_with_grey, # colours for plotting points and bars
  gradient = c(lower = mario_colours[1L], upper = mario_colours[2L]), #upper and lower colours for continuous colours
  background = "#EEEEEE" #defining a grey-ish background 
)


ggthemr(mario_theme)


library(tidyverse)






ggplot(genrecounts, aes(x, freq)) + geom_bar(stat="identity", position = "dodge", fill = '#310666') +geom_text(aes(label=freq, family = ("helvetica")), vjust= 0,hjust= -.05, color = "black", position = position_dodge(width=0.9), size = 3.5) + coord_flip() +ylab("Count of Games") +xlab("Genres") + ggtitle("Distribution of non-Nintendo Game Genres")
p

'#310666', '#870027'

Nintendo_Genres

ggplot(Nintendo_only, aes(x = Genre, y = plyr::count)) + geom_bar(stat = "count", position = "dodge") + coord_flip() + geom_text(aes(label = y))


ggplot(Nintendo_only, aes(x = Genre,)) + geom_bar(fill = '#870027') + geom_text(stat = 'count', aes(label = ..count..), vjust= 0,hjust= -.05, color = "black", position = position_dodge(width=0.9), size = 3.5)+ coord_flip() +ylab("Count of Games") +xlab("Genres") + ggtitle("Distribution of Nintendo Game Genres")


not_nintendo <- Comb_levels %>%  filter(Platform != "Nintendo")


ggplot(not_nintendo, aes(x = Genre,)) + geom_bar(fill = '#310666') + geom_text(stat = 'count', aes(label = ..count..), vjust= 0,hjust= -.05, color = "black", position = position_dodge(width=0.9), size = 3.5)+ coord_flip() +ylab("Count of Games") +xlab("Genres") + ggtitle("Distribution of non-Nintendo Game Genres")


not_nintendo$Genre <- factor(not_nintendo$Genre, levels =  c("Action", "Sports", "Shooter", "Role-Playing", "Racing", "Adventure", "Misc", "Fighting", "Strategy", "Simulation", "Platform", "Puzzle"))

not_nintendo$Genre <- factor(not_nintendo$Genre, levels =  c("Puzzle", "Platform", "Simulation", "Strategy", "Fighting", "Misc", "Adventure", "Racing", "Role-Playing", "Shooter", "Sports", "Action"))

?geom_text

?geom_label



Comb_levels %>%  select(Genre, Global_Sales) %>%  group_by(Genre) %>%  summarize(total = sum(Global_Sales))





tapply(Comb_levels$Global_Sales, Comb_levels$Genre, FUN=sum)

tapply(non_Nintendo$Global_Sales, non_Nintendo$Genre, FUN=sum)




tapply(Comb_levels$JP_Sales, Comb_levels$Platform, FUN=sum)




tapply(Comb_levels$Global_Sales, Comb_levels$Platform, FUN=sum)




tapply(Comb_levels$EU_Sales, Comb_levels$Platform, FUN=sum)

tapply(Comb_levels$Other_Sales, Comb_levels$Platform, FUN=sum)

tapply(Comb_levels$NA_Sales, Comb_levels$Platform, FUN=sum)


Plats <- Comb_levels %>%  filter(Genre == "Platform") %>%  group_by(Year_of_Release)



Plats$Year_of_Release

years <- c(1980:1984)
table(Plats$Year_of_Release)
Plats %>%  filter(Year_of_Release %in% years) %>%  summarize(sumSales = sum(Global_Sales))

# 19.58

years <- c(1985)

Plats %>%  filter(Year_of_Release %in% years) %>%  summarize(sumSales = sum(Global_Sales))


as.Date(Plats$Year_of_Release, "%Y")

Plats$Year_of_Release <- format(as.Date(Plats$Year_of_Release, format="%d/%m/%Y"),"%Y")



Plats$Year_of_Release <- as.numeric(Plats$Year_of_Release)

Plats$Year_of_Release <- as.numeric(Plats$Year_of_Release)

Plats %>%  filter(Year_of_Release < 1985)



height1 <- c(19.58, 43.17)
names <- c("Platformers before 1985", "Super Mario Bros. 1985")
SuperMario85 <- data.frame(height1, names)

?barplot

barplot(SuperMario85$height1, names.arg = SuperMario85$names, col = c("#5311A6", "#DC0442"))


SuperMario85$names <- as.factor(SuperMario85$names)


ggplot(SuperMario85, aes(x = names, fill = names)) + geom_bar(stat='identity', aes(y = height1)) + scale_fill_manual(values=c("#5311A6","#DC0442")) + labs(title = "Mario Revolutionized the Platformer Genre", subtitle = "Past sales to 2016, Super Mario Bros. has earned more than all other Platformers before it") + xlab("Games") + ylab("Sales in Millions $")



Comb_levels %>%  filter(Platform == "Nintendo", Year_of_Release == 2009) %>%  arrange(desc(Global_Sales))



# 2007-

# group top ten games and then show how they revolutionize

#

top_6 <- Comb_levels %>%  top_n(6, wt = Global_Sales)

ggplot(top_6, aes(x= Name, y = Global_Sales)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90))+ coord_flip()

top_6$Name <- as.character(top_6$Name)
top_6$Name <- as.factor(top_6$Name)
top_6$Name <- as.factor(top_6$Name, levels=c("Tetris", "Wii Sports Resort", "Pokemon Red/Pokemon Blue", "Mario Kart Wii", "Super Mario Bros.", "Wii Sports")) 

top_6$Name <- as.factor(top_6$Name, levels= c("Tetris", "Wii Sports Resort", "Pokemon Red/Pokemon Blue", "Mario Kart Wii", "Super Mario Bros.", "Wii Sports"))


 Comb_levels %>%  filter(Platform == "Nintendo")  %>%  select(Year_of_Release, Critic_Score, User_Score) %>% group_by(Year_of_Release, Critic_Score, User_Score) %>%  summarize(Mean_Critic = mean(Critic_Score, na.rm=TRUE), Mean_User = mean(User_Score, na.rm=TRUE))

 
 
 
 
 
 
 


str(Comb_levels)

# put user score into numeric

Comb_levels$User_Score <- as.numeric(Comb_levels$User_Score)

Ninten <- Comb_levels %>%  filter(Platform == "Nintendo")

typeof(Comb_levels$Year_of_Release)

Comb_levels$Year_of_Release <- as.factor(Comb_levels$Year_of_Release)

User_Critic <-Ninten %>%  group_by(Year_of_Release) %>% dplyr:::summarize(Mean_Critic = mean(Critic_Score, na.rm=TRUE), Mean_User = mean(User_Score, na.rm=TRUE))



ASdev <- Comb_levels %>%  filter(Developer == "Nintendo") %>%  group_by(Year_of_Release) %>%  dplyr:::summarize(DevMean = mean(Critic_Score, na.rm=TRUE))

write.csv(ASdev, file = "Nin as developer.csv", row.names = FALSE)


Nintendo <- Comb_levels %>%  filter(Platform == "Nintendo")

Dev <- Nintendo %>%  filter(Developer == "Nintendo") %>%  group_by(Year_of_Release) %>%  dplyr::summarise(DevCrit = mean(Critic_Score, na.rm=TRUE), User_Score = mean(User_Score, na.rm=TRUE))

write.csv(Dev, file = "Nin developr.csv", row.names = FALSE)






colnames(Nintendo)


notDev <- Comb_levels %>%  filter(Developer != "Nintendo", Platform == "Nintendo") %>%  
  group_by(Year_of_Release) %>% dplyr:::summarize(NotDev = mean(Critic_Score, na.rm=TRUE), User_Score = mean(User_Score, na.rm=TRUE))

asDev <- Comb_levels %>%  filter(Developer == "Nintendo") %>%  
  group_by(Year_of_Release) %>% dplyr:::summarize(Critic = mean(Critic_Score, na.rm=TRUE), User_Score = mean(User_Score, na.rm=TRUE))




write.csv(notDev, file = "Nin not developer.csv", row.names = FALSE)

write.csv(asDev, file = "Nin is developer.csv", row.names = FALSE)

notDev <- Comb_levels %>%  filter(Developer != "Nintendo", Platform == "Nintendo") %>%  
  group_by(Year_of_Release) %>% dply:::summarize(NotDev = mean(Critic_Score, na.rm=TRUE), User_Score = mean(User_Score, na.rm=TRUE))


#Comb_levels$Year_of_Release <- as.character(Comb_levels$Year_of_Release)

#Comb_levels$Year_of_Release <- as.Date(Comb_levels$Year_of_Release, "%Y")

#Comb_levels$Year_of_Release <- format(as.Date(Comb_levels$Year_of_Release, format="%d/%m/%Y"),"%Y")

#unique(Comb_levels$Year_of_Release)




Comb_levels %>%  filter(Platform == "Nintendo") %>% group_by(Platform) %>% summarize(gobsum = sum(Global_Sales))


Comb_levels %>%  filter(Genre == "Sports", Year_of_Release == 2006) %>% group_by(Platform) %>% summarize(gobsum = sum(Global_Sales))

# 135.83

Comb_levels %>%  filter(Genre == "Sports", Year_of_Release == 2009) %>% group_by(Platform) %>% summarize(gobsum = sum(Global_Sales))


# 137.33

SportsMoney <- c(135.83, 82.3, 137.33, 32.77)
Game <- c("All games", "Wii Sports", "All Games", "Wii Sports Resort")
sportsdf <- data.frame(SportsMoney, Game)



ggplot(sportsdf, aes(x = Game, fill = Game)) + geom_bar(stat='identity', aes(y = SportsMoney)) + scale_fill_manual(values=c("#5311A6","#5311A6", "#DC0442", "#DC0442")) + labs(title = "Wii Sports was highly innovative for its time", subtitle = "Detecting movement in three dimensions, the Wii Sports games were a fresh take on Sports Games.", caption =  'Wii Sports made up 60% of sports genre sales from 2006, Wii Sports Resort 24% from 2009') + xlab("Games") + ylab("Sales in Millions $")


library(stringr)

Mario <- Comb_levels %>% select(Name, Global_Sales) %>%  new_id = str_extract(id, "[^_]+$"))



#mario
mariogames <- Comb_levels %>% mutate(Is_Mario = str_extract(Name, "Mario") =='Mario' )



PlatFormers <- Comb_levels %>%  filter(Genre == 'Platform')
# make new column is mario in the name? boolean. Compare both....
######################################################
# mutate new column
PlatFormers1 <- PlatFormers
PlatFormers1 <-  PlatFormers %>%  mutate(Is_Mario = str_extract(Name, "Mario") =='Mario' )
PlatFormers1[PlatFormers1$Is_Mario == NA,] <- FALSE
PlatFormers1$Is_Mario[is.na(PlatFormers1$Is_Mario)] <- "FALSE"
is.factor(PlatFormers1$Is_Mario)
PlatFormers1$Is_Mario <- as.factor(PlatFormers1$Is_Mario)
PlatFormers1$Year_of_Release <- as.numeric(PlatFormers1$Year_of_Release)
PlatFormers2 <- PlatFormers1 %>%  filter(Year_of_Release >= 1985)
ggplot(PlatFormers1, aes(x = Year_of_Release, y = Global_Sales, fill = Is_Mario)) + geom_col()
ggplot(PlatFormers2, aes(x = Year_of_Release, y = Global_Sales)) + geom_line()


mariogames %>%  summarize(sum(Global_Sales)) #828.08





detach("package:plyr", unload=TRUE)
detach("package:kutils", unload=TRUE)
