source("global_config.R")
install.packages("Lahman")
install.packages("pitchRx")

library(tidyverse)
library(Lahman)
library(baseballr)
library(ggrepel)

crcblue <- "#2905a1"


Batting %>%
  filter(playerID %in% c("ruthba01", "bondsba01", "aaronha01", "rodrial01", "pujolal01")) %>%
  left_join(Master, by = "playerID") %>%
  group_by(playerID) %>%
  mutate(Age = yearID - birthYear, 
         Player = paste(nameFirst, nameLast),
         cum_HR = cumsum(HR)) %>%
  ggplot(aes(x = Age, y = cum_HR)) +
  geom_line(aes(linetype = Player)) + 
  scale_y_continuous("Career Home Runs")



view(Batting)

Master %>%
  head(1)

library(xtable)

Master %>%
  head(1) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  rename(`field name` = rowname, value = `1`) %>%
  xtable::xtable(align = "lrl", label = "tab:Master",
                 caption = "First row of the \\cmd{Master.csv} file.") %>%
  print(include.rownames = FALSE, caption.placement = "top")


Batting %>%
  filter(playerID == "brocklo01", yearID == 1964) %>%
  head(2) %>%
  as_tibble()

Batting %>%
  filter(playerID == "ruthba01") %>%
  xtable::xtable(label = "tab:batting",
                 caption = "Batting statistics for Babe Ruth, taken from the \\cmd{Batting} table.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        floating.environment = "sidewaystable", size = "\\small\\addtolength{\\tabcolsep}{-3pt}")

library(xtable)
Pitching %>%
  filter(playerID == "ruthba01") %>%
  select(-SH, -SF, -GIDP) %>%
  xtable::xtable(label = "tab:pitching",
                 caption = "Pitching statistics for Babe Ruth, taken from the \\cmd{Pitching} table. A few extra columns not reported here are available.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        floating.environment = "sidewaystable", size = "\\small\\addtolength{\\tabcolsep}{-4pt}")

Fielding %>%
  filter(playerID == "ruthba01", POS == "OF") %>%
  mutate(RF = (PO + A) / G) %>%
  pull(RF)


library(xtable)
Fielding %>%
  filter(playerID == "ruthba01") %>%
  select(-PB, -WP, -SB, -CS, -ZR) %>%
  xtable::xtable(label = "tab:fielding",
                 caption = "Fielding statistics for Babe Ruth, taken from the \\cmd{Fielding} table. Columns featuring statistics relevant only to catchers are not reported.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        size = "\\small\\addtolength{\\tabcolsep}{-4pt}")

#scrape 2017 statcast data
s1 <- scrape_statcast_savant_batter_all("2017-04-02", 
                                        "2017-04-08")
s2 <- scrape_statcast_savant_batter_all("2017-04-09", 
                                        "2017-04-15")
s3 <- scrape_statcast_savant_batter_all("2017-04-16", 
                                        "2017-04-22")
s4 <- scrape_statcast_savant_batter_all("2017-04-23", 
                                        "2017-04-29")
s5 <- scrape_statcast_savant_batter_all("2017-04-30", 
                                        "2017-05-06")
s6 <- scrape_statcast_savant_batter_all("2017-05-07", 
                                        "2017-05-13")
s7 <- scrape_statcast_savant_batter_all("2017-05-14", 
                                        "2017-05-20")
s8 <- scrape_statcast_savant_batter_all("2017-05-21", 
                                        "2017-05-27")
s9 <- scrape_statcast_savant_batter_all("2017-05-28", 
                                        "2017-06-03")
s10 <- scrape_statcast_savant_batter_all("2017-06-04", 
                                         "2017-06-10")
s11 <- scrape_statcast_savant_batter_all("2017-06-11", 
                                         "2017-06-17")
s12 <- scrape_statcast_savant_batter_all("2017-06-18", 
                                         "2017-06-24")
s13 <- scrape_statcast_savant_batter_all("2017-06-25", 
                                         "2017-07-01")
s14 <- scrape_statcast_savant_batter_all("2017-07-02", 
                                         "2017-07-08")
s15 <- scrape_statcast_savant_batter_all("2017-07-09", 
                                         "2017-07-15")
s16 <- scrape_statcast_savant_batter_all("2017-07-16", 
                                         "2017-07-22")
s17 <- scrape_statcast_savant_batter_all("2017-07-23", 
                                         "2017-07-29")
s18 <- scrape_statcast_savant_batter_all("2017-07-30", 
                                         "2017-08-05")
s19 <- scrape_statcast_savant_batter_all("2017-08-06", 
                                         "2017-08-12")
s20 <- scrape_statcast_savant_batter_all("2017-08-13", 
                                         "2017-08-19")
s21 <- scrape_statcast_savant_batter_all("2017-08-20", 
                                         "2017-08-26")
s22 <- scrape_statcast_savant_batter_all("2017-08-27", 
                                         "2017-09-02")
s23 <- scrape_statcast_savant_batter_all("2017-09-03", 
                                         "2017-09-09")
s24 <- scrape_statcast_savant_batter_all("2017-09-10", 
                                         "2017-09-16")
s25 <- scrape_statcast_savant_batter_all("2017-09-17", 
                                         "2017-09-23")
s26 <- scrape_statcast_savant_batter_all("2017-09-24", 
                                         "2017-09-30")
s27 <- scrape_statcast_savant_batter_all("2017-10-01", 
                                         "2017-11-01")
sc1 <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
             s11, s12, s13, s14)
sc2 <- rbind(s15, s16, s17, s18, s19, s20, s21,
             s22, s23, s24, s25, s26, s27)
sc_all <- rbind(sc1, sc2)

write_csv(sc_all, "statcast2017.csv")

remove(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)

remove(s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, sc1, sc2)

#making decades
teams %>% mutate(decade = yearID + yearID %% 10)

#trying to figure ave # of HR by decade
teams %>% group_by(decade) %>% 
  summarise(stdHR = 162*(sum(HR)/sum(G))) %>% 
  ungroup()

#giancarlo stanton's hr's in 2017
statcast2017 <- read_csv("statcast2017.csv")

#player_name is last, first
stanton_hr <- statcast2017 %>% 
  filter(player_name == "Stanton, Giancarlo", events == "home_run") %>%
  mutate(is_fb = pitch_type %in% c("FF", "FT", "SI", "FC"))

plate_width <- 17 + 2 * (9/pi)

k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) + 
  geom_rect(xmin = -(plate_width/2)/12, 
            xmax = (plate_width/2)/12, 
            ymin = 1.5, 
            ymax = 3.6, color = "blue", fill = "lightgray", 
            linetype = 2, alpha = 0.01) + 
  coord_equal() + 
  scale_x_continuous("Horizontal location (ft.)", 
                     limits = c(-1.5, 1.5)) + 
  scale_y_continuous("Vertical location (ft.)", 
                     limits = c(1, 4))

k_zone_plot %+% stanton_hr +
  aes(color = is_fb) + 
  geom_point() + 
  scale_color_manual("Type", values = c("blue", "gray60"),
                     labels = c("off-speed", "fastball")
  )


#trying to figure ave # of HR and SO by decade 
HRandSO <- teams %>% group_by(decade) %>%
summarise(stdHR = 162*(sum(HR, na.rm = TRUE)/sum(G, na.rm = TRUE)), 
          SOs = 162*sum(SO, na.rm = TRUE)/sum(G, na.rm = TRUE)) %>%
ungroup()

ggplot(HRandSO, aes(x=SOs, y = stdHR)) + geom_point()
ggplot(teams, aes(x=SO, y = HR)) + geom_point()


#range statistic for Babe Ruth as outfielder 
Ruth_range <- Ruth %>% filter(POS == "OF") %>% 
  group_by(yearID) %>% 
  summarise(range = (sum(PO)+sum(A))/(sum(G)), games = sum(G))

#compare ave run per game between AL and NL after DH introduced in 1973
rpg <- teams %>% filter(yearID>1972) %>% group_by(lgID, yearID) %>% 
  summarise(rpg = mean(R/G)) %>% ungroup()
rpgp <- ggplot(rpg, aes(x = yearID, y = rpg, group = lgID, color = lgID)) + geom_line()

rpgp

#percent of complete games 1900 to 1909 compared to 2000 to 2009 - hmm, doesn't match book answer
Pitching %>% 
  filter((yearID >= 1900 & yearID <= 1909)|(yearID >=2000 & yearID<=2009)) %>% 
  group_by(yearID) %>% 
  summarise(pct1900 = sum(CG)/sum(G)) %>% 
  ungroup()

#% Complete games for 2000 to 2009
Pitching %>% 
  filter(yearID >= 1900 & yearID <= 1909) %>% 
  summarise(Gs= sum(G), CGs = sum(CG), pct = CGs / Gs)

#HRs by month in 2017
statcast2017 %>% 
  filter(events == "home_run") %>% 
  mutate(month = substr(game_date, 6, 7)) %>% 
  group_by(month) %>% 
  summarise(HRs = n())

#looking at just HRs in 2017
hrs <- statcast2017 %>% 
  filter(events == "home_run") %>% 
  select(pitch_type, player_name, balls, strikes, 
         outs_when_up, inning, launch_speed, launch_angle, 
         if_fielding_alignment, of_fielding_alignment, delta_home_win_exp)

#making tables
with(hrs, table(balls, strikes))

#Ch 2 
#looking at Warren Spahn
spahn <- read_csv("baseball_R/data/spahn.csv")

spahn %>% summarize(Min = min(ERA),
                    Q1 = quantile(ERA, .25),
                    Med = median(ERA),
                    Q3 = quantile(ERA, .75),
                    Max = max(ERA))

#when did Spahn have his lowest ERA?
spahn %>% filter(ERA == min(ERA)) %>% select(Age, ERA)

#add FIP to spahn
spahn %>% mutate(FIP = (13*HR + 3*BB - 2*SO)/IP) -> spahn

#arrange by FIP and show 10 years
spahn %>% 
  arrange(FIP) %>% 
  select(Year, Age, W, L, ERA, FIP) %>% 
  slice(1:10)

#filter to just 2 teams
spahn %>% filter(Tm == "BSN" | Tm == "MLN") -> spahn1

#compare between teams
spahn1 %>% 
  group_by(Tm) %>% 
  summarise(mean_W.L = mean(W.L, na.rm = TRUE),
            mean_ERA = mean(ERA),
            mean_WHIP = mean(WHIP),
            mean_FIP = mean(FIP)
  )

#create vector from vectors
W <- spahn$W
L <- spahn$L
100*W/(W+L) -> Win.Pct

Year <- 1946:1966  #sequence of consecutive integers
Age <- Year - 1921

plot(Age, Win.Pct)

#pull particular values from Year based on logical conditions
Year[(W > 20) & (Win.Pct > 60)]


#making a dataframe
Year2 <- 2008:2017
NL <- c("PHI", "PHI", "SFN", "SLN", "SFN", "SLN", "SFN", "NYN", "CHN", "LAN")
AL <- c("TBA", "NYA", "TEX", "TEX", "DET", "BOS", "KCA", "KCA", "CLE", "HOU")
Winner <- c("NL", "AL", "NL", "NL", "NL", "AL", "NL", "AL", "NL", "AL")
N_Games <- c(5,6,5,7,4,7,7,5,7,7)

WS_results <- tibble(Year = Year2, NL_Team = NL, AL_Team = AL, N_Games = N_Games, Winner = Winner)

#where does NY show up
grep("NY", c(AL, NL), value = TRUE)

#num of WS winners by league
WS_results %>% group_by(Winner) %>% summarise(N = n()) -> WS

#graph of results - geom_col graphs each frequency value in a column
ggplot(WS, aes(x=Winner, y = N)) + geom_col()

#looking at factors
WS_results %>% group_by(NL_Team) %>% summarise(N = n())

#redefine NL_team from character to factor, and by division  -- if re-run above command, it will be in order by division
WS_results <- WS_results %>% 
  mutate(NL_Team = factor(NL_Team, 
                          levels = c("NYN", "PHI", "CHN", "SLN", "LAN", "SFN")))

#not quite sure what all the output from this structure function does
str(WS_results$NL_Team)

#lists
world_series <- list(Winner = Winner, Number.Games = N_Games, 
                     Seasons = "2008 to 2017")
world_series$Number.Games #shows values in this variable
world_series[[2]] #shows value of 2nd component in list
pluck(world_series, "Number.Games")  #another way
world_series["Number.Games"] #another another way

#get a vector from a dataframs
WS_results$N_Games #returns the values 
pull(WS_results, N_Games) #does the same thing


#playoff from Lahman (seriespost) filtered to WS graphed 
ws <- filter(SeriesPost, yearID >= 1903, 
             round == "WS", wins + losses < 8)
ggplot(ws, aes(x = wins + losses))+ 
  geom_bar(fill='blue') + 
  labs(x = "Number of games", y = "Frequency")


Batting %>%
  filter(playerID == "mantlmi01") -> MM

MM <- MM %>% mutate(Age = yearID - 1931)

#function to calculate HR rates by age and AB
hr_rates <- function(age, hr, ab) {
  rates <- round(100*hr / ab, 1)
  list(x = age, y = rates)
}

#apply function to Mickey Mantle
hr_rates(MM$Age, MM$HR, MM$AB)

plot(hr_rates(MM$Age, MM$HR, MM$AB))

#batting in the 1960s
Batting %>% 
  filter(yearID >=1960, yearID<=1969) -> Batting_60

#sum of HRs by player
Batting_60 %>% 
  group_by(playerID) %>% summarise(HR = sum(HR)) -> hr_60

#arrange descending
hr_60 %>% 
  arrange(desc(HR)) -> hr_60

#could do above in a single pipeline
Batting %>% filter(yearID>=2010, yearID<=2019) %>% 
  group_by(playerID) %>% summarise(HR = sum(HR)) %>% 
  arrange(desc(HR)) -> hr_20

#function to find HR leader 
hr_leader <- function(data) {
  data %>% group_by(playerID) %>% summarise(HR = sum(HR)) %>% 
    arrange(desc(HR)) %>% head(1)
}

Batting %>% 
  mutate(decade = 10*floor(yearID/10)) %>% 
  split(pull(.,decade)) %>% 
  map_df(hr_leader, .id = "decade")

#hrs and so for players with over 5000 career AB
Batting %>% group_by(playerID) %>% 
  summarize(tAB = sum(AB, na.rm = TRUE),
            tHR = sum(HR, na.rm = TRUE),
            tSO = sum(SO, na.rm = TRUE)) -> long_careers
Batting_5000 <- filter(long_careers, tAB >= 5000)


#graph hr/ab vs so/ab with trendline
ggplot(Batting_5000, aes(x = tHR/tAB, y = tSO/tAB))+geom_point() + geom_smooth()


#ch 2 exercises
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)
SB.attempt <- SB + CS
Success.Rate <- SB / SB.attempt
SB.Game <- SB / G
ggplot(data.frame(Success.Rate, SB.Game),aes(x = SB.Game, y = Success.Rate))+geom_point()

outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", "Walk", "Out", "Single")
table(outcomes)
f.outcomes <- factor(outcomes,
                     levels = c("Out", "Walk", "Single", "Double"))

career.pitching <- Pitching %>% 
  group_by(playerID) %>% 
  summarise(SO = sum(SO, na.rm = TRUE),
            BB = sum(BB, na.rm = TRUE), 
            IPouts = sum(IPouts, na.rm = TRUE),
            midyear = median(yearID, na.rm = TRUE))

career.pitching %>% filter(IPouts >= 10000) -> career.10000

ggplot(career.10000, aes(x = midyear, y = SO / BB)) + geom_point()

#Chapter 3
hof <- read_csv("baseball_R/data/hofbatting.csv")

hof <- hof %>% mutate(MidCareer = (From + To)/2,
                      Era = cut(MidCareer,
                                breaks = c(1800, 1900, 1919, 1941,
                                           1960, 1976, 1993, 2050),
                                labels = c("19th Century", "Dead Ball",
                                           "Lively Ball", "Integration",
                                           "Expansion", "Free Agency", 
                                           "Long Ball")))
hof_eras <- summarise(group_by(hof, Era), N = n())

#bar chart
ggplot(hof, aes(x = Era)) + geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency")+
  ggtitle("Era of the Nonpitching Hall of Famers")
#could also do ggplot(hof_eras, aes(x = Era, y = N)) + geom_col()

#same, but with points and using hof_eras
ggplot(hof_eras, aes(x = Era, y = N)) + geom_point() +
  xlab("Baseball Era") +
  ylab("Frequency")+
  ggtitle("Era of the Nonpitching Hall of Famers")+
  coord_flip()

#this will make a pdf of 2 graphs - 
pdf("graphs.pdf")
ggplot(hof, aes(x=Era)) + geom_bar()
ggplot(hof_eras, aes(x = Era, y = N)) + geom_point()
dev.off()


#numeric graphs
#one-dimensional scatterplot
ggplot(hof, aes(x = OPS, y = 1)) + geom_jitter(height = 0.6) +
  ylim(-1, 3) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  coord_fixed(ratio = 0.05)

#histogram
ggplot(hof, aes(x = OPS)) + 
  geom_histogram(breaks = seq(0.4, 1.2, by = 0.1),
                 color = "blue", fill = "white")

#scatterplot use ggrepel to avoid overlapping labels - might need library
ggplot(hof, aes(x = MidCareer, y = OPS)) +
  geom_point() + 
  geom_smooth() +
  geom_text_repel(data = filter(hof, OPS > 1.05 | OPS < 0.5), 
                  aes(MidCareer, OPS, label = Player))


p <- ggplot(hof, aes(x = OBP, y = SLG)) + geom_point() +
  xlim(0.25, 0.50) + ylim(0.28, 0.75) + 
  xlab("On Base Percentage") + ylab("Slugging Percentage")

#use scale_x_continuous to do both
p <- ggplot(hof, aes(x = OBP, y = SLG)) + geom_point() +
  scale_x_continuous("On Base Percentage", limits = c(0.25, 0.50)) +
  scale_y_continuous("Slugging Percentage", limits = c(0.28, 0.75))

#add tiers for OPS = OBP + SLG and text
p <- ggplot(hof, aes(x = OBP, y = SLG)) + geom_point() +
  scale_x_continuous("On Base Percentage", limits = c(0.25, 0.50)) +
  scale_y_continuous("Slugging Percentage", limits = c(0.28, 0.75)) +
  geom_abline(slope = -1, intercept = seq(0.7, 1, by = 0.1)) +
  annotate("text",
           x = rep(.27, 4), y = c(.42, .52, .62, .72),
           label = paste("OPS = ", 
                         c(0.7, 0.8, 0.9, 1.0)))

#or could make a dataframe of what you want for labels
ops_labels <- tibble(
  OBP = rep(0.3, 4), 
  SLG = seq(0.4, 0.7, by = 0.1), 
  label = paste("OPS = ", OBP + SLG)
)

#and use the dataframe with the geom_text function
p <- ggplot(hof, aes(x = OBP, y = SLG)) + geom_point() +
  scale_x_continuous("On Base Percentage", limits = c(0.25, 0.50)) +
  scale_y_continuous("Slugging Percentage", limits = c(0.28, 0.75)) +
  geom_abline(slope = -1, intercept = seq(0.7, 1, by = 0.1)) +
  geom_text(data = ops_labels, hjust = "right", 
            aes(label = label))

p + geom_text_repel(data = filter(hof, OBP + SLG > 1), 
                    aes(OBP, SLG, label = Player))

#numeric and factor variable graphs
hof <- mutate(hof, hr_rate = HR / AB)

ggplot(hof, aes(x = hr_rate, y = Era)) + 
  geom_jitter(height = 0.1)

#boxplots of same
ggplot(hof, aes(x = hr_rate, y = Era)) + geom_boxplot()

#function to get info for any player in Master data and fix age
get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name, " "))
  People %>% 
    filter(nameFirst == Names[1],
           nameLast == Names[2]) %>% 
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>% 
    select(playerID, Player, birthyear)
}

#get info and make dataframe - can use this for any player
PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez")
                        )


Batting %>% 
  inner_join(PlayerInfo, by = "playerID") %>% 
  mutate(Age = yearID - birthyear) %>% 
  select(Player, Age, HR) %>% 
  group_by(Player) %>% 
  mutate(CHR = cumsum(HR)) -> HRdata

#HR by age
ggplot(HRdata, aes(x = Age, y = CHR, linetype = Player, 
                   color = Player)) + geom_line()

#McGuire Sosa race
sosa_id <- People %>% 
  filter(nameFirst == "Sammy", nameLast == "Sosa") %>% 
  pull(retroID)

mac_id <- People %>% 
  filter(nameFirst == "Mark", nameLast == "McGwire") %>% 
  pull(retroID)

all1998 <- read_csv("baseball_R/data/all1998.csv", 
                  col_names = pull(fields, Header))

hr_race <- all1998 %>% filter(BAT_ID %in% c(sosa_id, mac_id))
remove(all1998)

library(lubridate)

cum_hr <- function(d) {
  d %>% 
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) %>% 
    arrange(Date) %>% 
    mutate(HR = ifelse(EVENT_CD == 23, 1, 0),
           cumHR = cumsum(HR)) %>% 
    select(Date, cumHR)
}

hr_ytd <- hr_race %>% 
  split(pull(., BAT_ID)) %>% 
  map_df(cum_hr, .id = "BAT_ID") %>% 
  inner_join(People, by = c("BAT_ID" = "retroID"))

ggplot(hr_ytd, aes(x = Date, y = cumHR, 
                   linetype = nameLast, color = nameLast)) + 
  geom_line() + 
  geom_hline(yintercept = 62, color = crcblue) +
  annotate("text", ymd("1998 -04-15"), 65,
           label = "62", color = crcblue) +
  ylab("Home runs in the Season") +
  ggtitle("McGwire vs Sosa 1998")

#ch 3 exercises
hofpitching <- hofpitching %>% 
  mutate(BF.group = cut(BF, 
                        c(0, 10000, 15000, 20000, 30000),
                        labels= c("Less than 10000", "10000 to 15000", 
                                  "15000 to 20000", "more than 20000")))

#frequency table of BF.group
hofpitching %>% group_by(BF.group) %>% summarise(N = n())

#bar graph of BF.group
ggplot(hofpitching, aes(x = BF.group)) + geom_bar()

#in the 20000+ group
hofpitching %>% filter(BF.group == "more than 20000") %>% count()

#another plot of BF.group
ggplot(hofpitching, aes(x = BF, y = BF.group)) + geom_boxplot()

#or
hofpitching %>% group_by(BF.group) %>% summarise(N = n()) -> S
ggplot(S, aes(x = BF.group, y = N)) + geom_col()
ggplot(S, aes(x = BF.group, y = N)) + geom_point() + 
  ylim(0, 30) + coord_flip()

#WAR for HOF pitchers
ggplot(hofpitching, aes(x = WAR)) + geom_histogram()

#identify the two high outliers
library(ggrepel)

ggplot(hofpitching, aes(x = WAR)) + geom_boxplot() + 
  geom_text_repel(data = filter(hofpitching, WAR > 125), 
                aes(WAR, 0, label = `...2`))

#War per season
hofpitching <- hofpitching %>% 
  mutate(WAR.season = WAR / Yrs)

#one d scatterplot by bf.group
ggplot(hofpitching, aes(x=WAR.season, y = BF.group)) + geom_jitter(height = 0.1)

#add boxplots - first geom is "lowest", next is on top
ggplot(hofpitching, aes(x=WAR.season, y = BF.group)) + 
  geom_boxplot() + 
  geom_jitter(height = 0.1) +
  geom_text_repel(data = filter(hofpitching, WAR.season > 7), 
                  aes(WAR.season, BF.group, label = `...2`))
