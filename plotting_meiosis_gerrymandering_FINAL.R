## This file maps congressional districts in Texas and creates a visual
## representation of what a new district-mapping procedure might look like
## (based on re-drawing districts each election year based on population density)

library(tidycensus)
library(dplyr)
library(ggplot2)
library(beepr)


home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Meiosis and Gerrymandering'

setwd(home_dir)

## save texas border for later
texas_border= map_data('state', region = 'Texas')

######################################################
##
## Plot existing congressional district boundaries for TX
##
######################################################

# get data
congress <- get_acs(
  state = "TX",
  geography = "congressional district",
  # geography = "county",
  # geography = "block group",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2020
)

# plot
congress %>%
  ggplot() + 
  geom_sf(color = "black",fill="white") + 
  theme_void()
# save
ggsave("./Source_Files_Figures/congressional_districts.png",height=4,width=4,units="in")

######################################################
##
## Plot population density by census tract in Texas,
## using a dot density plot
##
######################################################

orange <- get_acs(
  state = "TX",
  geography = "tract",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2020
)

beep()
head(orange)

# get a sense of 1/36th Texas's population
round(sum(orange$estimate)/36,-3)

# plot tracts by population for fun
orange %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option="magma",direction=-1) +
  theme_void()

# ggsave("census_block_group.png",height=4,width=4,units="in")
# ggsave("census_tract.png",height=4,width=4,units="in")
# ggsave("county.png",height=4,width=4,units="in")

# Convert data to dots
dots <- as_dot_density(
  orange,
  value = "estimate",
  values_per_dot = 100
)

# plot
ggplot() +
  geom_sf(data = dots,
          # aes(color = variable),
          size = 0.01,
          alpha=0.2,
          color="black") +
  geom_polygon( data=texas_border, 
                aes(x=long, y=lat,group=group),
                colour="black", fill=NA )+
  theme_void()

beep()

# save
ggsave("./Source_Files_Figures/tract_dots.png",height=4,width=4,units="in")

######################################################
##
## Create visual representation for what it would look like to
## arbitrariliy scatter dots throughout texas
##
######################################################

colors<-c("#3ca598",
          "#3567af",
          "#7a62bc")

for (i in 1:3) {
  ## generate dots
  dots<- as_dot_density(
    congress,
    value = "estimate",
    values_per_dot = 700000)
  
  # count them
  print(nrow(dots))
  
  ## plot
  ggplot() +
    geom_sf(data = dots,
            # aes(color = variable),
            size = 2,
            color=colors[i],
            alpha=0.75) +
    geom_polygon( data=texas_border, 
                  aes(x=long, y=lat,group=group),
                  colour="black", fill=NA )+
    theme_void()
  
  ggsave(paste0("./Source_Files_Figures/",
                "tract_dots_",i,".png", sep=""),
                height=2,width=2,units="in")
  
}



