# VISUALIZATION

# inspired by @karim_douieb

# load packages
library(tidyverse)


# set strings as factors to false
options(stringsAsFactors = FALSE)


#########################
# MAPPING

library(sf) # processing spatial vector data
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
library(rgdal) # read shapefile


# map country and coordinate data

## shapefile
result_shp <- readOGR("C:/Users/User/Documents/GIS/GE15/GE15_parlimen_result_pt.shp",
                  stringsAsFactors = F)

#DOSM
msia_parl_shp <- read_sf("C:/Users/User/Documents/GIS/GE15/electoral_0_parlimen.geojson")

crs(result_shp)

crs(msia_parl_shp)



# layout map (readOGR)

map <- ggplot() + 
  geom_polygon(data = msia_parl_shp, aes(x = long, y = lat, group = group),  #readOGR
               fill = "grey", alpha = 0.3, colour = "white") +
  geom_point(data = as.data.frame(result_shp), aes(x = Long, y = Lat),
             color = 'red', size = 1, alpha = 0.5) +
  coord_map(xlim = c(99, 120), ylim = c(0.5, 8)) +
  theme_void()

map


# layout map (read_sf)

map <- ggplot() + 
  geom_sf(data = msia_parl_shp) + #read_sf
  geom_point(data = as.data.frame(result_shp), aes(x = Long, y = Lat),
              color = 'red', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(99, 120), ylim = c(0.5, 8)) +
  theme_void()

map


#########################
# POLYGON MAP


###import data
GE_data = read.csv("C:/Users/User/Documents/GIS/GE15/results_parlimen_ge15_combine.csv", 
                   header = T, sep = ",")



### set format
str(GE_data)

### join data to shp
GE_data_shp <- msia_parl_shp %>% 
  full_join(GE_data, by = "parlimen") %>% 
  mutate(votes_ratio = (votes/min(votes)*0.1)) %>%   #create ratio for size of dorling cartogram
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))



### reformat field
GE_data_shp$votes <- as.numeric(as.character(GE_data_shp$votes))


### add field ID for animation sequence
#result_shp$ID <- 1:nrow(result_shp) #by parlimen
#GE_data_shp$rank <- rank(GE_data_shp$lon, ties.method = "first") #by longitude


# CARTOGRAM
# using dorling (non overlapping circles)

library(cartogram)

class(GE_data_shp)
crs(GE_data_shp)

GE_data_shp2 <- st_as_sf(GE_data_shp)
GE_data_shp2 <-st_transform(GE_data_shp2, crs = 32648)  #need projected coordinate system, eg WGS 84 / UTM zone 48N

crs(GE_data_shp2)

GE_data_dorling <- cartogram_dorling(GE_data_shp2, k = 0.1, weight = "votes_ratio") #adjust k for size


### remove rank column from data so no confusion in animating layers with same field
GE_data_shp3 <- subset(GE_data_shp, select = -result)




### party colors
### https://en.wikipedia.org/wiki/2022_Malaysian_general_election

col_party <- c(
  "PERIKATAN NASIONAL (PN)"   = "#006699",
  "PAKATAN HARAPAN (PH)"   = "#ca413e",
  "PARTI ISLAM SE MALAYSIA (PAS)"    = "#006699",
  "BARISAN NASIONAL (BN)"= "#000080",
  "IKATAN DEMOKRATIK MALAYSIA (MUDA)"   = "#ca413e",
  "BEBAS (BEBAS)"   = "#cccccc",
  "PARTI KESEJAHTERAAN DEMOKRATIK MASYARAKAT (KDM)"    = "#cccccc",
  "PARTI WARISAN SABAH (WARISAN)"= "#a4e5fc",
  "PARTI GABUNGAN RAKYAT SABAH (GRS)"   = "#66CDAA", #blue to light green
  "PARTI TINDAKAN DEMOKRATIK (DAP)"   = "#ca413e",
  "GABUNGAN PARTI SARAWAK (GPS)"   = "#ba5dc7", #pink to purple
  "PARTI BANGSA MALAYSIA (PBM)"   = "#cccccc",
  "NA"    = "#cccccc") 



# LABEL PARTIES
## subset
label_party <- GE_data %>% 
  group_by(party) %>% 
  summarise(max_votes = max(votes)) %>% 
  left_join(GE_data_shp2, 
            by = c("max_votes"="votes", "party" = "party")) %>% 
  dplyr::select(party, max_votes, lon, lat) 


###import data
write.csv(label_party, "C:/Users/User/Documents/GIS/GE15/GE15_label.csv",
          row.names = F)
label_party = read.csv("C:/Users/User/Documents/GIS/GE15/GE15_label.csv", 
                   header = T, sep = ",")



# FONT

library(extrafont)

## import font
font_import(pattern="Roboto", prompt=FALSE)
## set 'sans' font as Roboto
windowsFonts(sans="Roboto Light") 
## load fonts
loadfonts(device="win")
loadfonts(device="postscript")


## mapping

library(ggrepel)

map_polygon <- ggplot() + 
  geom_sf(data = GE_data_shp3, 
          mapping = aes(fill = as.factor(party), group = as.factor(party)), 
          color = "white", linewidth = 0.05, #alpha = 0.2,
          show.legend = FALSE) +
  geom_sf(data = GE_data_dorling, mapping = aes(fill = party), 
          color = NA, #alpha = 0.5,
          show.legend = FALSE) +
  scale_fill_manual(name = "Party", values = col_party) +
  coord_sf(xlim = c(99, 120), ylim = c(0.5, 8)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "none") +
  annotate("text", x = 109, y = 6.5, size = 5,
           label = "Malaysia 15th General Election\nResults") +
  geom_text_repel(data = label_party, aes(x = lon, y = lat, label = party),
                  #min.segment.length = 0, seed = 42, box.padding = 0.5,
                  size = 2, color = "grey30") +
  labs(#title = paste0("Malaysia 15th General Elections\nResults"), 
       caption = "Data source: SPR, DOSM")


map_polygon


### print last plot to file
ggsave(paste0("GE15_map-dorling1.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")



## mapping choropleth


map_choro <- ggplot() + 
  geom_sf(data = GE_data_shp3, 
          mapping = aes(fill = as.factor(party), group = as.factor(party)), 
          color = "white", alpha = 1, show.legend = FALSE) +
  scale_fill_manual(name = "Party", values = col_party) +
  coord_sf(xlim = c(100, 119), ylim = c(1.2, 7.1)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 5, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "none") +
  annotate("text", x = 109, y = 6.5, size = 4,
           label = "Malaysia 15th General Election") +
  labs(#title = paste0("Malaysia 15th General Election\nResults"), 
       caption = "Twitter: ycgoh___")


map_choro

### print last plot to file
ggsave(paste0("GE15_map_static2_choro.jpg"), dpi = 300,
       width = 1500, height = 550, units = "px")


## mapping cartogram

GE_data_dorling2 <- cartogram_dorling(GE_data_shp2, k = 0.1, weight = "votes_ratio") #dunno why k=0.1 works

map_carto <- ggplot() + 
  geom_sf(data = GE_data_dorling3, 
          mapping = aes(fill = as.factor(party), group = as.factor(party)), 
          color = NA, alpha = 1,
          show.legend = FALSE) +
  scale_fill_manual(name = "Party", values = col_party) +
  coord_sf(xlim = c(100, 119), ylim = c(1.2, 7.1)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 5, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "none") +
  annotate("text", x = 109, y = 6.5, size = 4,
           label = "Malaysia 15th General Election") +
  labs(#title = paste0("Malaysia 15th General Election\nResults"), 
    caption = "Twitter: ycgoh___")


map_carto


### print last plot to file
ggsave(paste0("GE15_map_static2_carto.jpg"), dpi = 300,
       width = 1500, height = 550, units = "px")



#########################

# ANIMATION CARTOGRAM

#http://r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram

library(tweenr)
library(gganimate)
library(sfheaders)
library(transformr)


crs(GE_data_dorling2)
crs(GE_data_shp3)


### change projection to WGS84 (original UTM) so both spatial object have same proj
class(GE_data_dorling2)
GE_data_dorling3 <- st_transform(GE_data_dorling2, 
                                 crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
crs(GE_data_dorling3)

class(GE_data_shp3)
class(GE_data_dorling3)


## Give an id to every row (every parlimen)
GE_data_dorling3$parl_no <- seq(1, nrow(GE_data_dorling3))
GE_data_shp3$parl_no <- seq(1, nrow(GE_data_shp3))


## Transform these 2 objects in dataframe, plotable with ggplot2
GE_data_dorling3_df <- sf_to_df(GE_data_dorling3) 
GE_data_shp3_df <- sf_to_df(GE_data_shp3)



## join fields to connect df with data (parlimen)
GE_data_dorling3_df2 <- left_join(GE_data_dorling3_df, 
                                 GE_data_dorling3 %>% dplyr::select(c(24, 30)), 
                                 by = c("polygon_id" = "parl_no")) 
GE_data_shp3_df2 <- left_join(GE_data_shp3_df, 
                             GE_data_shp3 %>% dplyr::select(c(24, 29)), 
                                 by = c("multipolygon_id" = "parl_no")) 


## set time
GE_data_shp3_df2$time <- 1
GE_data_dorling3_df2$time <- 30


## remove geometry (for tweenr to work somehow), multipolygon_id for GE_data_shp3
GE_data_shp3_df2 <- subset(GE_data_shp3_df2, select = -c(polygon_id, geometry))
GE_data_dorling3_df2 <- subset(GE_data_dorling3_df2, select = -geometry)
GE_data_shp3_df2 <- GE_data_shp3_df2 %>% 
  rename(polygon_id = multipolygon_id)

### cannot use geom_sf if no geometry


## Set transformation type + time
#GE_data_df$ease <- "cubic-in-out"


## Calculate the transition between these 2 objects
dtw <- tween_polygon(GE_data_shp3_df2, GE_data_dorling3_df2, 
                     'cubic-in-out', 30, polygon_id, match = T) %>% 
  keep_state(10)


## check a few frame
ggplot() + 
  geom_polygon(data = GE_data_shp3_df2, 
               aes(fill = polygon_id, x = x, y = y, group = polygon_id), size = 0, alpha = 0.9
  )
ggplot() + 
  geom_polygon(data = dtw  %>% filter(.frame == 2), 
               aes(fill = party, x = x, y = y, group = polygon_id), size=0, alpha=0.9
  )




# Plot animated map - testing

map_anim_try1 <- ggplot() + 
  geom_polygon(data = dtw,  #%>% filter(.frame == 30), #try out frame by frame
          mapping = aes(fill = party, group = polygon_id, 
                        x = x, y = y), 
          color = "white", alpha = 1, size = 0.05,
          show.legend = FALSE) +
  scale_fill_manual(name = "Party", values = col_party) +
  coord_sf(xlim = c(100, 119), ylim = c(1.2, 7.1)) +
  theme_void() +
  theme(plot.title = element_text(family = "Times New Roman", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0.1),
        legend.position = "none") +
  labs(title = paste0("GE15 Results"), 
       caption = "Inspired by @karim_douieb\nData source: DOSM, SPR (@Thevesh)")


# Final map

map_anim_try1 <- ggplot() + 
  geom_polygon(data = dtw,  #%>% filter(.frame == 30), #try out frame by frame
               mapping = aes(fill = party, group = polygon_id, 
                             x = x, y = y), 
               color = "white", alpha = 1, linewidth = 0.05,
               show.legend = FALSE) +
  scale_fill_manual(name = "Party", values = col_party) +
  coord_sf(xlim = c(100, 119), ylim = c(1.2, 7.1)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 5, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "none") +
  annotate("text", x = 109, y = 6.5, size = 4,
           label = "Malaysia 15th General Election") +
  labs(#title = paste0("Malaysia 15th General Election\nResults"), 
    caption = "Twitter: ycgoh___")


map_anim_try1



# Make the animation


### mapping animation

map_anim <- map_anim_try1 +
  #shadow_mark() +
  transition_manual(frames = .frame)


gganimate::animate(map_anim, fps = 30, res = 300, #nframes = num_parl, 
                   start_pause = 10, end_pause = 10,
                   width = 1500, height = 550
                   )

anim_save("GE15_map_anim2.gif")




