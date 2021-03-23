library(tidyverse)
library(ggsoccer)
library(understatr)
library(grid)
library(extrafont)
library(cowplot)

loadfonts(device = "win")

team <- get_team_players_stats("Monaco", 2020) #enter team name within quotations and season required

df <- c(team$player_id)

df

shotx <- get_player_shots(df[1])

shotx

for (i in 2:length(df)) {
  shoti = get_player_shots(df[i])
  shotx = rbind(shotx,shoti)
}

teamshots <- subset(shotx,shotx$year == 2020)

player_name<-"Wissam Ben Yedder"

teamshots <- rename(teamshots, Result = result)
teamshots <- rename(teamshots, Situation = situation)

head(teamshots)
view(playershotsA)

playershots <- teamshots %>% 
  filter(player=={player_name},
         !Situation=="Penalty")

playershotsA <- teamshots %>% 
  filter(player_assisted=={player_name})

xgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960","#FCDC5F", "#F5B94D",
              "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000") 

p1 <- playershots %>%  # Player xG shot map with body part details
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = "#011515",
                 limits = FALSE)+
  
  scale_color_manual(
    values = c("Condition 1" = "#7C0000", "Condition 2" = "black"), guide = F) +
  
  geom_point(aes(fill = xG, shape = shotType,
                 stroke = if_else(Result == "Goal", 0.7, 0.4), color = if_else(Result == "Goal", "Condition 1", "Condition 2")),
             alpha = 0.8, size = 2) +
  
  scale_fill_gradientn(colours = xgcolors, limit = c(0,1), name = "Expected Goals Value") +
  
  scale_shape_manual(values = c("Head" = 21, "RightFoot" = 23, "LeftFoot" = 24, "OtherBodyPart" = 25),
                     labels(c("Head", "Right Foot", "Left Foot", "Other"))) +
  
  guides(shape = guide_legend(override.aes = list(size = 3, fill = "#C0C5C5", colour = "black"))) +
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  labs(subtitle = "Body Part") +  
  
  theme_pitch() +
  
  theme(plot.background = element_rect(fill = "#011515", color = "#011515"),
        panel.background = element_rect(fill = "#011515", color = "#011515"),
        
        legend.background = element_rect(fill = "#011515"),
        legend.key = element_rect(fill = "#011515"),
        legend.text.align = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.width = unit(0.4, 'cm'), #change legend key height
        legend.key.height = unit(0.4, 'cm'),
        legend.position = "left",
        legend.box.margin=margin(0, 10, 0, 0),
        
        plot.subtitle = element_text(colour = "#C0C5C5", family = "Malgun Gothic Semilight", size = 13, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#C0C5C5", family = "Malgun Gothic", size = 8))

p1

situ <- c("OpenPlay" = "#0043D8", "FromCorner" = "#00D829",  "DirectFreekick" = "#D89500", "SetPiece" = "#D800AF")

p2 <- playershots %>%             # Shot situation Details
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = "#011515",
                 limits = FALSE)+
  
  scale_colour_manual(values = c(situ), aesthetics = c("fill"), guide = guide_legend(override.aes = list(size = 3) )) +
  
  scale_color_manual(
    values = c("Goal" = "#7C0000", "No Goal" = "black")) +
  
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  
  geom_point(aes(fill = Situation, color = if_else(Result == "Goal", "Goal", "No Goal"),
                 stroke = if_else(Result == "Goal", 0.7, 0.4)), shape = 21, alpha = 0.75, size = 2) +
  
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  guides(size = FALSE) +
  
  labs(subtitle = "Shot Situation") +
  
  theme_pitch() +
  
  theme(plot.background = element_rect(fill = "#011515", color = "#011515"),
        panel.background = element_rect(fill = "#011515", color = "#011515"),
        legend.background = element_rect(fill = "#011515"),
        legend.key = element_rect(fill = "#011515"),
        legend.key.size = unit(0.2, 'cm'), #change legend key size
        legend.key.width = unit(0.2,'cm'),
        legend.position = "right",
        legend.box.margin=margin(0, 0, 0, -20),
        
        plot.subtitle = element_text(colour = "#C0C5C5", family = "Malgun Gothic Semilight", size = 13, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#C0C5C5", family = "Malgun Gothic", size = 8))

p2

shotcolor <- c("#011515", "#011515", "#043131", "#0F5454", "#228888", "#CC7E6D", "#DF4E2E","#E61616", "#FD0303")

p3 <- playershots %>%                 # Shot Heat map
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#2D4B4B",
                 fill   = "#011515",
                 limits = FALSE)+
  
  geom_point(shape = 19, size = 0, colour = "#011515") +
  
  stat_density2d(geom="raster", aes(fill=..density..), alpha = 0.5, 
                 contour=FALSE) +
  
  scale_fill_gradientn(colours = shotcolor) +
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  guides(fill = FALSE) +
  
  labs(subtitle = "Shot Density") +
  
  theme_pitch() +
  
  theme(plot.background = element_rect(fill = "#011515", color = "#011515"),
        panel.background = element_rect(fill = "#011515", color = "#011515"),
        legend.background = element_rect(fill = "#011515"),
        legend.key = element_rect(fill = "#011515"),
        legend.key.size = unit(0.2, 'cm'), #change legend key size
        legend.key.width = unit(0.2,'cm'),
        legend.position = "right",
        legend.box.margin=margin(0, 0, 0, -20),
        
        plot.subtitle = element_text(colour = "#C0C5C5", family = "Malgun Gothic Semilight", size = 13, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#C0C5C5", family = "Malgun Gothic", size = 8))
p3

p4 <- playershotsA %>%               # Shot Assists Map
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = "#011515",
                 limits = FALSE)+
  
  scale_color_manual(
    values = c("Condition 1" = "#7C0000", "Condition 2" = "black"), guide = F) +
  
  geom_point(aes(fill = xG, shape = Situation,
                 stroke = if_else(Result == "Goal", 0.7, 0.4), color = if_else(Result == "Goal", "Condition 1", "Condition 2")),
             alpha = 0.8, size = 2) +
  
  scale_fill_gradientn(colours = xgcolors, limit = c(0,1), name = "Expected Goals Value") +
  
  scale_shape_manual(values = c("FromCorner" = 21, "OpenPlay" = 23, "SetPiece" = 24), name ="") +
  
  guides(shape = guide_legend(override.aes = list(size = 3, fill = "#C0C5C5", colour = "black"))) +
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  labs(subtitle = "Shots Assisted") +  
  
  theme_pitch() +
  
  theme(plot.background = element_rect(fill = "#011515", color = "#011515"),
        panel.background = element_rect(fill = "#011515", color = "#011515"),
        
        legend.background = element_rect(fill = "#011515"),
        legend.key = element_rect(fill = "#011515"),
        legend.text.align = unit(0.2, 'cm'),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.width = unit(0.4, 'cm'), #change legend key height
        legend.key.height = unit(0.4, 'cm'),
        legend.position = "right",
        legend.box.margin=margin(0, 0, 0, -20),
        
        
        plot.subtitle = element_text(colour = "#C0C5C5", family = "Malgun Gothic Semilight", size = 13, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#C0C5C5", family = "Malgun Gothic", size = 8))

p4

ggdraw() +             # Compiling all plots into one
  draw_plot(p1, x = 0., y = .53, width = .5, height = .4) +
  draw_plot(p2, x = 0.45, y = .53, width = .5, height = .4) +
  draw_plot(p3, x = 0.043, y = 0.15, width = .5, height = .4) +
  draw_plot(p4, x = 0.45, y = 0.15, width = .5, height = .4) +
  
  draw_plot_label(label = ("Wissam Ben Yedder - AS Monaco - 20/21"), size = 12,
                  x = (0.5), y = (1), hjust = 0.5, family = "Malgun Gothic Semilight", colour = "#C0C5C5") +
  draw_plot_label(label = ("Shots and Shots Assisted Map - Excluding Penalties"), size = 8,
                  x = (0.5), y = (0.965), hjust = 0.5, family = "Malgun Gothic Semilight", colour = "#C0C5C5") +
  draw_plot_label(label = c("Twitter: @atom2r", "Data from Understat.com", "23/2/2021"), size = 8,
                  x = c(0.9, 0.9, 0.9), y = c(1, 0.98, 0.96), hjust = 0.5, family = "Malgun Gothic Semilight", colour = "#C0C5C5")

ggsave("Shot BenYed.png", bg = "#011515", width = 8, height = 6)