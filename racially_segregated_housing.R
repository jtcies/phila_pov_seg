set.seed(7)

library('acs')
library('rgdal')
library('sp')
library('leaflet')
library('maptools')
library('tidyverse')
library('geojsonio')
library("spdplyr")
library('stringr')
library('htmlwidgets')

source('jtc_theme.R')

tract <- read_csv('ACS_15_5YR_S0601_with_ann.csv')

tract <- tract[-1,]

tract_shape <- readOGR(dsn = 'cb_2016_42_tract_500k')

tracts_json <- readOGR("Census_tracts_2010.geojson", "OGRGeoJSON")

#--------- Creating Functions --------

idSeg <- function(x) {
  a <- apply(x, 1, max)
  b <- if_else(a >= .8, 1, 0)
  return(b)
}

#---------- Cleaning / calcuating -----------
# renaming race columns for easier calcuating

tract <- dplyr::rename(tract, 
                total = HC01_EST_VC01,
                est.white = HC01_EST_VC20,
                est.black = HC01_EST_VC21,
                est.american.indian = HC01_EST_VC22,
                est.asian = HC01_EST_VC23,
                est.pi = HC01_EST_VC24,
                est.other = HC01_EST_VC25,
                est.hispanic = HC01_EST_VC28,
                pov = HC01_EST_VC67)

est <- c('est.black', 'est.white', 'est.american.indian', 'est.asian',
         'est.pi', 'est.other', 'est.hispanic')

# changing those columns to numeric

for(i in c(4, 108, 116, 124, 132, 140, 148, 164, 388)) {
  tract[,i] <- lapply(tract[,i], function(x) as.numeric(x))
}

# fixing them to be percentages

tract <- tract %>%
  mutate(est.white = est.white / 100, 
          est.black = est.black / 100,
          est.american.indian = est.american.indian / 100,
          est.asian = est.asian / 100,
          est.pi = est.pi / 100,
          est.other = est.other / 100,
          est.hispanic = est.hispanic / 100,
          pov = pov / 100)

# determining which tracts are segrated (greater than 80% one race)

tract$seg <- idSeg(tract[,est])

tract$seg[tract$total < 100] <- NA

# ---------- Analysis and plotting ----------

seg_bar <- ggplot(filter(tract, !is.na(seg)), aes(x = factor(seg))) + 
  geom_bar() + 
  jtc


# which race is plurality of resdients by tract

tract$seg.plu <- colnames(tract[,est])[max.col(tract[,est],
                                               ties.method = "first")]

tract$seg.plu <- factor(tract$seg.plu, levels = c('est.black',
                                                  'est.white', 
                                                  'est.hispanic',
                                                  'est.asian'))

tract$max <- apply(tract[,est], 1, max)

tract <- tract %>%
  mutate(seg.race = if_else(seg == 1, as.character(seg.plu), 'not segregated'))


seg_bar_race <- ggplot(filter(tract, !is.na(seg)), aes(x = factor(seg),
                                                       fill = seg.plu)) + 
  geom_bar() + 
  jtc


seg_dot <- ggplot(filter(tract, !is.na(seg)), aes(x = max,
                                                  fill = seg.plu)) + 
  geom_dotplot(method = 'histodot', stackgroups = TRUE, dotsize = .5,
               binwidth = .025) +
  geom_vline(xintercept = .7875, linetype = 'longdash') + 
  scale_fill_discrete(labels = c('black', 'white', 'Hispanic', 'Asian'),
                      name = str_wrap('race of plurarlity of residents 
                                      in each census tract', 20)) + 
  scale_y_continuous(name = 'count of census tracts', breaks = NULL) +
  xlab('pct of tract with same race as plurality of residents') + 
  annotate("text", x = .9, y = .95, label = 'segregated') +
  annotate("text", x = .5, y = .95, label = 'not segregated') +
  jtc

ggsave("seg_dot_plot.png", width = 10, height = 6.18, units = "in")
  

# creating a leaflet map

race_data <- select(tract, GEO.id2, 4, 108, 116, 124, 132, 140, 148, 164,
                    pov, seg, seg.plu, seg.race, max)

tracts_json <- left_join(tracts_json, race_data, by = c("GEOID10" = "GEO.id2"))

tracts_json$seg.race[tracts_json$seg.race == 'est.black'] <- '>80% Black'
tracts_json$seg.race[tracts_json$seg.race == 'est.hispanic'] <- '>80% Hispanic'
tracts_json$seg.race[tracts_json$seg.race == 'est.white'] <- '>80% White'

seg_color <- colorFactor(c("#6b7a8f", "#F7C331", "#f7882f", "#dcc7aa"),
                         domain = tracts_json$seg.race,
                         na.color = 'white')

labels <- sprintf("<strong>Tract %s </strong> <br> 
                    <p> 
                    Total Pop: %g <br/>
                    Pct black: %g <br/>
                    Pct white: %g <br/> 
                    Pct Hispanic: %g <br/> 
                    Pct Asian: %g <br/>
                    Pct American Indian: %g <br/> 
                    Pct Pacific Islander: %g <br/> 
                    Pct other: %g <br/>
                    Pct Blw Pov: %g
                    </p>",
    tracts_json$NAME10, tracts_json$total,
    tracts_json$est.black * 100, tracts_json$est.white * 100,
    tracts_json$est.hispanic * 100, tracts_json$est.asian * 100,
    tracts_json$est.american.indian * 100, tracts_json$est.pi * 100,
    tracts_json$est.other * 100, tracts_json$pov * 100) %>% 
  lapply(htmltools::HTML)

map <- leaflet(tracts_json, width = '100%') %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~seg_color(seg.race),
    weight = .5,
    opacity = 1,
    color = "#b5b9c2",
    fillOpacity = .7,
    highlight = highlightOptions(
      weight = 3,
      color = "#4f4f4f",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels) %>%
    addLegend(pal = seg_color, values = ~seg.race,
              title = "Legend", na.label = "NA (total pop < 100)")

saveWidget(map, "seg_pov_leaflet.html")

# plotting max plurality vs poverty rate

pov_lm_white <- lm(formula = pov ~ est.white, 
                   data = race_data)

pov_lm_black <- lm(formula = pov ~ est.black,
                   data = race_data)

pov_lm_hispanic <- lm(formula = pov ~ est.hispanic, 
                      data = race_data)

pov_lm_asian  <- lm(formula = pov ~ est.asian, 
                                      data = race_data)

race_long <- race_data %>%
  select(-seg, -seg.plu, -seg.race, -total, -max, -est.pi,
         -est.american.indian, -est.other) %>%
  gather(race, est, c(2:5), -pov)

race_long$race <- str_replace(race_long$race,'est.', '')

race_long$race[race_long$race == 'hispanic'] <- 'Hispanic'
race_long$race[race_long$race == 'asian'] <- 'Asian'

race_long$race <- factor(race_long$race, levels = c('black', 'white',
                                                    'Hispanic', 'Asian'))

# creating annotations with r2 for facet_wrap plot

white_ann <- data.frame(est = .8, pov = 1, 
                        race = factor('white', levels = c('black', 'white',
                                      'Hispanic', 'Asian')))
black_ann <- data.frame(est = .8, pov = 1, 
                        race = factor('black', levels = c('black', 'white',
                                                          'Hispanic', 'Asian')))
hispanic_ann <- data.frame(est = .8, pov = .2, 
           race = factor('Hispanic', levels = c('black', 'white',
                                             'Hispanic', 'Asian')))
asian_ann <- data.frame(est = .8, pov = .2, 
           race = factor('Asian', levels = c('black', 'white',
                                             'Hispanic', 'Asian')))

seg_pov <- ggplot(race_long, aes(x = est, y = pov)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_continuous(name = 'pct of residents') + 
  scale_y_continuous(name = 'pct below poverty line') +
  facet_wrap(~race) +
  theme(strip.text=element_text(vjust=.1)) +
  geom_text(data = black_ann, label = 'r2=.19') +
  geom_text(data = white_ann, label = 'r2=.33') +
  geom_text(data = hispanic_ann, label = 'r2=.16') +
  geom_text(data = asian_ann, label = 'r2=.01') +
  jtc

ggsave("seg_pov_plot.png", width = 10, height = 6.18, units = "in")

# calculate % of residents living in segregated tracts

pct_seg <- weighted.mean(race_data$seg, w = race_data$total, na.rm = TRUE)


