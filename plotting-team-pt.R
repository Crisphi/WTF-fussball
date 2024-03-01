library(tidyverse)
library(wordcloud2)
library(ggwordcloud)
library(psych)
library(GGally)
library(Hmisc)
library(ggcorrplot)
library(ggalt)
library(readxl)
library(scales)
library(zipfR)

load("./weltfussball_liveticker/df_BL_1718_bis_2223")

df_BL_1718_bis_2223["Home_PT"] <- NA
df_BL_1718_bis_2223["Away_PT"] <- NA

df_BL_1718_bis_2223 <- df_BL_1718_bis_2223 %>%
  mutate_at(vars(Season), factor) %>%
  mutate(Home_PT = case_when(
                              Result_Home > Result_Away ~ 3,
                              Result_Home == Result_Away ~ 1,
                              Result_Home < Result_Away ~ 0
                             ))%>%  
  mutate(Away_PT = case_when(
                              Result_Away > Result_Home ~ 3,
                              Result_Away == Result_Home ~ 1,
                              Result_Away < Result_Home ~ 0
                            ))

Seasons <- levels(df_BL_1718_bis_2223$Season)
Teams <- levels(df_BL_1718_bis_2223$Team1)

columns <- c("Team", "Season", "Points_Home", "Points_Away", "Points_Sum")
pts_per_team_and_season <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(pts_per_team_and_season) <- columns
head(pts_per_team_and_season)

for(s in Seasons){
  filtered <- df_BL_1718_bis_2223%>%
    filter(Season == s)
  for(t in Teams){
    filtered_home <- filtered%>%
      filter(Team1 == t)
    filtered_away <- filtered%>%
      filter(Team2 == t)
    pt_home <- sum(filtered_home$Home_PT)
    pt_away <- sum(filtered_away$Away_PT)
    pt_sum <- pt_home+pt_away
    new_row <- c(Team = t, Season = s, Points_Home = pt_home, Points_Away = pt_away, Points_Sum = pt_sum)
    pts_per_team_and_season <- rbind(pts_per_team_and_season,new_row)
  }
}

colnames(pts_per_team_and_season) <- columns
head(pts_per_team_and_season)


pts_per_team_and_season %>%
    group_by(Team) %>%
    ggplot(aes(x =Season, y= Points_Sum, colour = Team, group= Team)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
    facet_wrap(vars(Team))

dateipfad <- "./plots/team-pt_plot.png"
#ggsave(dateipfad, width = 10, height = 6, units = "in")


pts_per_team_and_season %>%
  group_by(Team) %>%
  ggplot(aes(x =Season, y= Points_Sum, colour = Team, group= Team)) +
  geom_line() +
  geom_point() +
  #coord_cartesian(ylim = c(16, 85)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) 

dateipfad <- "./plots/team-pt-all_plot.png"
#ggsave(dateipfad, width = 10, height = 6, units = "in")
