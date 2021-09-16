library(tidyverse)
library(ggsoccer)
library(understatr)
library(grid)
library(extrafont)
library(cowplot)
library(flextable)
library(webshot)
library(gdtools)
library(Cairo)
library(formattable)
library(officer)

loadfonts(device = "win")

font <- "Oxygen"
fon2 <- "Oxygen Light"

bgcol <- "#011515"

player_name <- "Mason Mount"

team_name <- "Chelsea" # Name as on understat.com

season <- "20/21"

T_date <- "16/9/2021"

team <- get_team_players_stats({team_name}, 2020) #enter team name within quotations and season required

df <- c(team$player_id)

df

shotx <- get_player_shots(df[1])

shotx

for (i in 2:length(df)) {
  shoti = get_player_shots(df[i])
  shotx = rbind(shotx,shoti)
}

teamshots <- subset(shotx,shotx$year == 2020)

head(teamshots)

teamshots <- teamshots %>%
  mutate(situation = replace(situation, situation == "FromCorner", "Corner"),
         situation = replace(situation, situation == "DirectFreekick", "Freekick"),
         shotType = replace(shotType, shotType == "RightFoot", "Right-Foot"),
         shotType = replace(shotType, shotType == "LeftFoot", "Left-Foot"),
         shotType = replace(shotType, shotType == "OtherBodyPart", "Other"))

playershots <- teamshots %>% 
  filter(player=={player_name},
         !situation == "Penalty")

playershotsA <- teamshots %>% 
  filter(player_assisted=={player_name})

theme_shotmapOG <- function(){ 
  
  font <- "Oxygen"   #assign font family up front
  bgcol <- "#011515"
  
  theme(
    
    plot.background = element_rect(fill = bgcol, color = bgcol),
    panel.background = element_rect(fill = bgcol, color = bgcol),
    
    legend.background = element_rect(fill = bgcol),
    legend.key = element_rect(fill = bgcol),
    
    plot.subtitle = element_text(colour = "#C0C5C5", family = font, size = 10, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(colour = "#C0C5C5", family = font, size = 7.5))
}

# Plot 1 - Shot Location

xgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960","#FCDC5F", "#F5B94D",
              "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000") 

p1 <- playershots %>% 
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = bgcol,
                 limits = FALSE)+
  
  scale_color_manual(
    values = c("Condition 1" = "#7C0000", "Condition 2" = "black"), guide = F) +
  
  geom_point(aes(fill = xG, shape = shotType,
                 stroke = if_else(result == "Goal", 0.7, 0.4), color = if_else(result == "Goal", "Condition 1", "Condition 2")),
             alpha = 0.7, size = 2) +
  
  scale_fill_gradientn(colours = xgcolors, limit = c(0,1), name = "Expected Goals Value") +
  
  scale_shape_manual(values = c("Head" = 21, "Right-Foot" = 23, "Left-Foot" = 24, "Other" = 25)) +
  
  guides(shape = guide_legend(override.aes = list(size = 3, fill = "#C0C5C5", colour = "black"))) +
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  labs(subtitle = "Chance Quality - Body Part") +  
  
  theme(legend.text.align = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.width = unit(0.4, 'cm'), #change legend key height
        legend.key.height = unit(0.4, 'cm'),
        legend.position = "left",
        legend.box.margin=margin(0, 10, 0, 0)) +
  
  theme_pitch() +
  
  theme_shotmapOG()

# Plot 2 - Shot Situation

situ <- c("OpenPlay" = "#0043D8", "Corner" = "#00D829",  "Freekick" = "#D89500", "SetPiece" = "#D800AF")

p2 <- playershots %>%
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = bgcol,
                 limits = FALSE)+
  
  scale_colour_manual(values = c(situ), aesthetics = c("fill"), labels = c("Corner", "Freekick", "OpenPlay", "SetPiece"),
                      guide = guide_legend(override.aes = list(size = 3))) +
  
  scale_color_manual(
    values = c("Goal" = "#7C0000", "No Goal" = "black")) +
  
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  
  guides(colour = guide_legend(override.aes = list(fill = "#C0C5C5", size = 2, stroke = 1.5))) +
  
  geom_point(aes(fill = situation, color = if_else(result == "Goal", "Goal", "No Goal"),
                 stroke = if_else(result == "Goal", 0.7, 0.4)), shape = 21, alpha = 0.7, size = 2) +
  
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  guides(size = FALSE) +
  
  labs(subtitle = "Shot Situation") +
  
  theme(legend.key.size = unit(0.2, 'cm'), #change legend key size
        legend.key.width = unit(0.2,'cm'),
        legend.position = "right",
        legend.box.margin=margin(0, 0, 0, -20)) +
  
  theme_pitch() +
  
  theme_shotmapOG()

#Plot 3 - Shot Density

shotcolor <- c("#011515", "#011515", "#043131", "#0F5454", "#228888", "#CC7E6D", "#DF4E2E","#E61616", "#FD0303")

p3 <- playershots %>%
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#2D4B4B",
                 fill   = bgcol,
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
  
  theme_shotmapOG()

# Plot 4 - Shot Assists

p4 <- playershotsA %>% 
  mutate(X = X*100,
         Y = Y*100) %>% 
  
  ggplot(aes(x = X, y = 100-Y))+
  
  annotate_pitch(colour = "#213232",
                 fill   = bgcol,
                 limits = FALSE)+
  
  scale_color_manual(
    values = c("Condition 1" = "#7C0000", "Condition 2" = "black"), guide = F) +
  
  geom_point(aes(fill = xG, shape = situation,
                 stroke = if_else(result == "Goal", 0.7, 0.4), color = if_else(result == "Goal", "Condition 1", "Condition 2")),
             alpha = 0.7, size = 2) +
  
  scale_fill_gradientn(colours = xgcolors, limit = c(0,1), name = "Expected Goals Value") +
  
  scale_shape_manual(values = c("Corner" = 21, "OpenPlay" = 23, "SetPiece" = 24)) +
  
  guides(shape = guide_legend(override.aes = list(size = 3, fill = "#C0C5C5", colour = "black"))) +
  
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  
  labs(subtitle = "Shots Assisted") +
  
  theme(legend.text.align = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.width = unit(0.4, 'cm'), #change legend key height
        legend.key.height = unit(0.4, 'cm'),
        legend.position = "right",
        legend.box.margin=margin(0, 0, 0, -20)) +
  
  theme_pitch() +
  
  theme_shotmapOG()

# Plot 5 - Table Formation

# Creating variable inputs for the Table

# 1 - Npg
gm <- playershots %>%
  filter(result == "Goal")                       # Non penalty goals

# 2 - Npxg
ExG <- formattable(sum(playershots[, 'xG']), digits = 1, format = "f")                  # Total Npxg

#3 - % of Team shots
team_s <- nrow(teamshots)
player_s <- nrow(playershots)                    # % of Team shots
P_ts <- formattable(player_s/team_s *100, digits = 0, format= "f")

#4 - % of Team Npxg
T_xG <- teamshots %>%
  filter(!situation == "Penalty")
T_xG <- sum(T_xG[, 'xG'])                        # % of Team npxG
P_TNPxG <- formattable(ExG/T_xG *100, digits = 0, format = "f")

#5 -% Openplay shots created 
op <- playershotsA %>%
  filter(situation == "OpenPlay")
nop <- playershotsA %>%
  filter(!situation == "OpenPlay")                                         # % of shots created from open play
pop <- as.numeric(nrow(op) + nrow(nop)) 
openplay <- formattable(nrow(op)/pop *100, digits = 0, format = "f")

#6 - Preffered foot
Rf <- playershots %>%
  filter(shotType == "Right-Foot")
Lf <- playershots %>%
  filter(shotType == "Left-Foot")
Tf <- as.numeric(nrow(Rf) + nrow(Lf))            # Preferred Foot
PRF <- formattable(nrow(Rf)/Tf*100, digits = 0, format = "f")
PLF <- formattable(nrow(Lf)/Tf*100, digits = 0, format = "f")
if(PRF > PLF){
  Pf <- noquote(paste0("Right Foot ", PRF, "%"))
} else {
  Pf <- noquote(paste0("Left Foot", PLF, "%"))
}

#7 - Total Assists
TA <- playershotsA %>%
  filter(result == "Goal")                       # Total Assists

#8 - Expected Assists
TxA <- formattable(sum(playershotsA[, 'xG']), digits = 1, format = "f")                 # Expected Assists

ft <- tibble(Npg = nrow(gm),
             NpxG = ExG,
             '+ve/-ve Shot Performance' = Npg - NpxG,
             "% of Team's Shots" = P_ts,
             "% of Team's NpxG" = P_TNPxG,
             '% of Shots Created from OpenPlay' = openplay,
             'Preferred Foot' = Pf,
             Assists = nrow(TA),
             xA = TxA)

set_flextable_defaults(
  font.family = font,
  font.size = 12,
  font.color = "#C0C5C5",
  table.layout = "fixed",
  digits = 1)            

tb <- ft %>% flextable()

tb <- bg(tb, bg = bgcol, part = "all")

tb <- align(tb, align = "center", part = "all")

big_border = fp_border(color = "#213232", width = 1.5)

tb <- border_outer(tb, part="all", border = big_border)

std_border = fp_border(color="#213232", width = 1.5)

tb <- border_inner_v(tb, border = std_border)

tb <- padding(tb, padding = 5, part = "header") %>% autofit() %>% as_raster()

p5 <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(tb), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

ggdraw() +
  draw_plot(p1, x = 0., y = .53, width = .5, height = .4) +
  draw_plot(p2, x = 0.45, y = .53, width = .5, height = .4) +
  draw_plot(p3, x = 0.043, y = 0.15, width = .5, height = .4) +
  draw_plot(p4, x = 0.45, y = 0.15, width = .5, height = .4) +
  draw_plot(p5, x = 0.1, y = -0.022, width = .8, height = .2, hjust = 0) +
  draw_plot_label(label = paste0(player_name, " - ",  team_name, " - ", season), size = 12,
                  x = (0.5), y = (1), hjust = 0.5, family = fon2, colour = "#C0C5C5") +
  draw_plot_label(label = ("Shots and Shots Assisted Map - Excluding Penalties"), size = 8,
                  x = (0.5), y = (0.965), hjust = 0.5, family = fon2, colour = "#C0C5C5") +
  draw_plot_label(label = paste0(c("Twitter: @atom2r", "Data from Understat.com", T_date)), size = 7,
                  x = c(0.9, 0.9, 0.9), y = c(1, 0.98, 0.96), hjust = 0.5, family = fon2, colour = "#C0C5C5")

ggsave("Shot MM.png", device = "png", type = "cairo", bg = bgcol, width = 8, height = 6)
