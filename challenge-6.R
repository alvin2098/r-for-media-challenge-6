### --- R Challenge 6, Alvin Aziz, 29.04.2021 --- ###

### --- Load libraries --- ###
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(janitor)
library(ggrepel)


### --- Load dataframes --- ###
bundestag_2019 <- readRDS("data/bundestag_2019.rds")

nebeneinkuenfte <- 
  read.csv("data/data-ROsix.csv", encoding = "UTF-8") %>% 
  clean_names() %>% 
  mutate(
    name = str_split(name, ", ") %>% 
      map_chr(~rev(.) %>% 
                paste(collapse = " ")),
    nebentaetigkeiten = nebentatigkeiten == "ja"
  ) %>% 
  select(-nebentatigkeiten, -partei)


### --- Join both dataframes --- ###
bundestag_2019 <- bundestag_2019 %>% 
  left_join(nebeneinkuenfte)


### --- Define political party colors --- ###
partei_farben = list( # create list so it can be used multiple times
  "CDU" = "black",
  "SPD" = "#E30013",
  "CSU" = "black",
  "FDP" = "#FFD700",
  "Linke" = "#BD3075",
  "Grüne" = "#19A229",
  "AfD" = "#009FE1",
  "fraktionslos" = "grey"
)


### --- Plotting with ggplot --- ###
ggplot(bundestag_2019, aes(x = lebensdaten, 
                           y = land, 
                           color = fraktion,
                           size = mindest_einkunfte_in_euro / 1000000)) +
  scale_color_manual(values = partei_farben) +
  geom_point(alpha = 0.5, position = "jitter") + # jitter to randomly vary location of points
  geom_label_repel(
    show.legend = F,
    data=subset(bundestag_2019, mindest_einkunfte_in_euro > 450000),
    aes(label = name),
    size = 3,
    segment.color = "white",
    fill = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Nebeneinkünfte der Bundestagsabgeordneten",
    x = "Geburtsjahr",
    y = "",
    size = "Mindestnebeneinkünfte \nin Millionen Euro", 
    color = "Fraktion"
  )
