library(tidyverse)

#import in all the raw data
NBA_totals <- read.csv("nba_total.csv")
NBA_advanced <- read.csv("nba_advanced.csv")
NBA_PerGame <- read.csv("nba_per_game.csv")
Player_Names <- read.csv("players_names.csv")
Hustle_stats <- read.csv("hustle_stats.csv")
NBA_Stats_2018 <- merge(NBA_advanced,NBA_PerGame, by = "Player")

#filter players who were traded
NBA_Stats_2018_Temp <- 
  NBA_Stats_2018 %>%
  filter(Tm.x == "TOT" & Tm.y == "TOT")
names <-
  NBA_Stats_2018_Temp %>%
  select(Player)

#identify duplicates as a result of trades
dups <- (as.vector(names$Player))
NBA_Stats_2018$Player %in% dups 


#get rid of duplicates
test <-
  NBA_Stats_2018 %>% 
  filter((Player %in% dups) == F)

final <- rbind(test,NBA_Stats_2018_Temp)

final <- final %>%
  arrange(Player)

#Sort out the data that is unneeded
data_BR <- 
  final %>%
  select(Player,G.x,MP.y,STL,BLK,DRB,PF,DWS,DBPM) %>%
  filter(G.x >= 35 & MP.y > 12)

#change column names
colnames(data_BR)[2] <- "GP"
colnames(data_BR)[3] <- "MIN"
view(data_BR)

#Combine stats.nba.com data with basketball-reference data
test <- cbind(data_BR, Player_Names)
data_BR <- 
  test %>%
  select(Name,GP,MIN,STL,BLK,DRB,PF,DWS,DBPM,PF) %>%
  arrange(Name)
colnames(data_BR)[1] <- "PLAYER"
Hustle_stats <- 
  Hustle_stats %>%
  filter(GP >= 35 & MIN > 12)
Overall <- merge(Hustle_stats,data_BR, by = "PLAYER")

#Rename columns
colnames(Overall)[4] <- "GP"
colnames(Overall)[5] <- "MP"
colnames(Overall)[6] <- "DFL"
colnames(Overall)[7] <- "DLBR"
colnames(Overall)[8] <- "CHRG"
colnames(Overall)[9] <- "C2PS"
colnames(Overall)[10] <- "C3PS"

Overall <- 
  Overall %>%
  select(PLAYER, GP, MP, STL, CHRG, BLK, DFL, DRB, 
         DLBR, C3PS, C2PS, DWS, DBPM, PF)

#Define standardize function
standardize <- function(x){
   n_mean <- mean(x, na.rm = TRUE)
   n_sd <- sd(x, na.rm = TRUE)
   return( (x - n_mean)/n_sd )
}

#Z-values of all the statistics
Stan_Data <- 
  Overall %>%
  mutate(STL = standardize(STL)) %>%
  mutate(CHRG = standardize(CHRG)) %>%
  mutate(BLK = standardize(BLK)) %>%
  mutate(DFL = standardize(DFL)) %>%
  mutate(DRB = standardize(DRB)) %>%
  mutate(DLBR = standardize(DLBR)) %>%
  mutate(C3PS = standardize(C3PS)) %>%
  mutate(C2PS = standardize(C2PS)) %>%
  mutate(DWS = standardize(DWS)) %>%
  mutate(DBPM = standardize(DBPM)) %>%
  mutate(PF = standardize(PF)) %>%
  arrange(PLAYER)

#make all values positive
SCOTTIE_DATA <- 
  Stan_Data %>%
  mutate(STL = STL + 1.7087406) %>%
  mutate(CHRG = CHRG + 0.6334989) %>%
  mutate(BLK = BLK + 1.1227788) %>%
  mutate(DFL = DFL + 1.6143699) %>%
  mutate(DRB = DRB + 1.4248575) %>%
  mutate(DLBR = DLBR + 1.7715299) %>%
  mutate(C3PS = C3PS + 2.0578999) %>%
  mutate(C2PS = C2PS + 1.2945471) %>%
  mutate(DWS = DWS + 2.0011923) %>%
  mutate(DBPM = DBPM + 2.2954378) %>%
  mutate(PF = PF + 2.2511336) %>%
  arrange(PLAYER)

#SCOTTIE formula
SCOTTIE_DATA <- 
  SCOTTIE_DATA %>%
  mutate(SCOTTIE = 0.28641649713* (0.1855417*STL + 0.1069588*CHRG 
                    + 0.1287841*BLK + 0.1478784*DFL + 0.1151842*DRB 
                    + 0.0868331*DLBR + 0.0808669*C3PS + 0.0412567*C2PS
                    - 0.1962163*PF + 0.0919539*DBPM + 0.1002965*DWS))

#SCOTTIE TOP 10
TOP_10 <- 
  SCOTTIE_DATA %>%
  filter(PLAYER %in% c("Anthony Davis",
                       "Paul George",
                       "Giannis Antetokounmpo",
                       "Draymond Green",
                       "Kawhi Leonard",
                       "Rudy Gobert",
                       "James Harden",
                       "Jimmy Butler",
                       "Marcus Smart",
                       "Jrue Holiday",
                       "Brook Lopez",
                       "Ersan Ilyasova"
  )) 
#SCOTTIE STATS TOP 10
SCOTTIE_top <- 
  TOP_10 %>%
  mutate(SCOTTIE_STL = 0.28641649713* (0.1855417*STL),
         SCOTTIE_CHRG = 0.28641649713*(0.1069588*CHRG),
         SCOTTIE_BLK = 0.28641649713*(0.1287841*BLK),
         SCOTTIE_DFL = 0.28641649713*(0.1478784*DFL),
         SCOTTIE_DRB = 0.28641649713*(0.1151842*DRB),
         SCOTTIE_DLBR = 0.28641649713*(0.0868331*DLBR),
         SCOTTIE_C3PS = 0.28641649713*(0.0808669*C3PS),
         SCOTTIE_C2PS = 0.28641649713*(0.0412567*C2PS),
         SCOTTIE_PF = -0.28641649713*(0.1962163*PF),
         SCOTTIE_DBPM = 0.28641649713*(0.0919539*DBPM),
         SCOTTIE_DWS = 0.28641649713*(0.1002965*DWS),
         SCOTTIE = 0.28641649713* 
           (0.1855417*STL + 0.1069588*CHRG 
           + 0.1287841*BLK + 0.1478784*DFL + 0.1151842*DRB 
           + 0.0868331*DLBR + 0.0808669*C3PS + 0.0412567*C2PS
           - 0.1962163*PF + 0.0919539*DBPM + 0.1002965*DWS)) 
SCOTTIE_top <- SCOTTIE_top %>%
  gather(Categories, valname, -PLAYER) %>%
  spread(PLAYER, valname)

SCOTTIE_top <- SCOTTIE_top %>% mutate(
  stat_grp = case_when(
    Categories %in% c("SCOTTIE_STL", "SCOTTIE_DFL", "SCOTTIE_C3PS", "SCOTTIE_DLBR") ~ "PERIMETER",
    Categories %in% c("SCOTTIE_BLK","SCOTTIE_CHRG","SCOTTIE_DRB", "SCOTTIE_C2PS") ~ "POST",
    Categories %in% c("SCOTTIE_DWS", "SCOTTIE_DBPM") ~ "ADVANCED",
    Categories %in% c("SCOTTIE_PF") ~ "FOULS",
    TRUE ~ "OTHER"
  ))


SCOTTIE_top <- SCOTTIE_top %>%
  filter(stat_grp != "OTHER") %>%
  arrange(stat_grp)


ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Paul George', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Anthony Davis', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Jimmy Butler', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Giannis Antetokounmpo', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Jrue Holiday', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Rudy Gobert', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Kawhi Leonard', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Draymond Green', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'James Harden', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Marcus Smart', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Ersan Ilyasova', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_top) + 
  geom_bar(aes(x=Categories, y=SCOTTIE_top$'Brook Lopez', fill = stat_grp), stat='identity' ) +
  ylab('RAW VALUE CONTRIBUTED') + 
  labs(fill = "STAT GROUP") +
  scale_fill_manual(values=c("#2B2E4A", "#E84545", "#53354A", "#903749"))

ggplot(data = SCOTTIE_DATA) + 
  geom_point(mapping = aes(x = GP, y = SCOTTIE, color = MP)) +
  scale_color_distiller(palette = "BuPu", direction = 1)

#Perimeter/Post/Advanced Breakdowns
SCOTTIE_DATA <-
  SCOTTIE_DATA %>%
  mutate(Per_SCOTTIE = 0.44404299011*(0.1855417*STL + 0.1478784*DFL
         + 0.0868331*DLBR + 0.0808669*C3PS - 0.1962163*PF + 0.396121384),
         Post_SCOTTIE =  0.66356335779*(0.1069588*CHRG + 0.1287841*BLK
         + 0.1151842*DRB + 0.0412567*C2PS),
         Advanced_Data = 0.94302613314*(0.0919539*DBPM + 0.1002965*DWS))

#Set up the ranking
Scottie_Ranking <- 
  SCOTTIE_DATA %>%
  select(PLAYER, SCOTTIE, Per_SCOTTIE, Post_SCOTTIE, Advanced_Data) %>%
  arrange(desc(SCOTTIE))

#Make it look nice
Scottie_Ranking <-
  Scottie_Ranking %>%
  mutate(SCOTTIE = round(SCOTTIE, digits = 3)) %>%
  mutate(Per_SCOTTIE = round(Per_SCOTTIE, digits = 3)) %>%
  mutate(Post_SCOTTIE = round(Post_SCOTTIE, digits = 3)) %>%
  mutate(Advanced_Data = round(Advanced_Data, digits = 3))

#Make SCOTTIE Scores out of 100
Scottie_Ranking <-
  Scottie_Ranking %>%
  mutate(SCOTTIE = SCOTTIE * 100) %>%
  mutate(Per_SCOTTIE = Per_SCOTTIE * 100) %>%
  mutate(Post_SCOTTIE = Post_SCOTTIE * 100) %>%
  mutate(Advanced_Data = Advanced_Data * 100)

#Graphs
ggplot(data = SCOTTIE_DATA) + 
  geom_point(mapping = aes(x=Per_SCOTTIE, y= Post_SCOTTIE))

ggplot(data = SCOTTIE_DATA) + 
  geom_point(mapping = aes(x=Advanced_Data, y= Post_SCOTTIE))

ggplot(data = SCOTTIE_DATA) + 
  geom_point(mapping = aes(x=Per_SCOTTIE, y= Advanced_Data))
  
