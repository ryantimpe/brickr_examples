#Day 10, Model 1 ----

library(brickr)

#Use `bricks_from_coords()` to convert coordinates into a 3D LEGO model
# PRO: Much easier to program models with tidyverse functions
# CON: More difficult to visualize when creating the model

#A 1x1 red brick
tibble::tibble(
  x = 1, y = 1, z = 1, color = "Bright red"
) %>% 
  bricks_from_coords() %>% 
  build_bricks()

#bricks_from_coords() requires 4 columns to build a model
# x: The location of bricks left-to-right, values > 0
# y: The location of bricks front-to-back, values > 0
# z: The location of bricks bottom-to-top, values > 0
# color: Name of color to use. See build_colors()

rgl::snapshot3d("output/day10_model1.png")
rgl::clear3d()

#Day 10, Model 2 ----

#Use tidyr::crossing() to create a row for each combination of input data
# Create bricks 4 studs wide, 2 studs deep, 5 levels high
tidyr::crossing(
  x = 1:4,
  y = 1:2,
  z = 1:5
) %>% 
  #Each brick on each Z gets a different color
  dplyr::mutate(
    color = lego_colors$Color[z]
  ) -> stacked_bricks

stacked_bricks %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::snapshot3d("output/day10_model2.png")
rgl::rgl.clear()

#Day 11, Model 1 ----

#Ducktales models!

#Start with a 1x1 Donald duck
# No need to include X and Y yet since they are constant
# Remember: "b" is brick (3-units high) & "p" is plate (1-unit)
# Each mid_level has 3 units: 0, 1, 2
tibble::tribble(
  ~z, ~mid_level,  ~piece_type, ~color,
  1, 0, "b", "Bright orange", #Orange brick feet
  2, 0, "b", "White", #White legs
  3, 0, "b", "Bright blue", #Begin blue shirt...
  4, 0, "p", "Bright blue", #... add 2 plates for height
  4, 1, "p", "Bright blue",
  4, 2, "p", "Bright orange", #beak
  5, 0, "b", "White", #head
  6, 0, "p", "White",
  6, 1, "p", "Black", #Cap brim
  6, 2, "p", "Bright blue" #Cap 
) -> skinny_donald

#Use tidyr::expand() to add X and Y, while duplicating each.
# The 1x1 Donald becomes a 2x2 Donald
# use nesting() to ensure we don't duplicate over all the other columns
skinny_donald %>% 
  tidyr::expand(x=1:2, y=1:2, 
                  tidyr::nesting(z, mid_level, piece_type, color)
                ) -> donald

#Render Donald in 3D!
donald %>% 
  bricks_from_coords() %>% 
  build_bricks(
    background_color = "#fcedcc" 
  )

rgl::snapshot3d("output/day11_model1.png")
rgl::rgl.clear()

#Use functional programming to make the 3 nephews!
# Define the tibble as with Donald, but use the variable shirt_color
# Include the ... in the function to ensure it works with {purrr} pmap()
make_nephew_duck <- function(shirt_color, x_offset=0, ...){
  tibble::tribble(
    ~z, ~mid_level,  ~piece_type, ~color,
    1, 0, "p", "Bright orange", #Orange plate feet x2
    1, 1, "p", "Bright orange", 
    1, 2, "p", "White",  #White plate legs x2
    2, 0, "p", "White",
    2, 1, "b", shirt_color, #Specific shirt color brick
    3, 1, "p", "Bright orange", #Orange beak plate
    3, 2, "b", "White", #White brick head
    4, 2, "p", shirt_color, #Plate for a cap to match the shirt x2
    5, 0, "p", shirt_color
  ) %>% 
    #Make the 1x1 model a 2x2 model
    tidyr::expand(x=1:2, y=1:2, 
                  tidyr::nesting(z, mid_level, piece_type, color)) %>% 
    #Increment the X value so the ducks don't overlap
    dplyr::mutate(x = x + x_offset)
}

#Define the 3 nephews
# Only need the shirt color and x-coord offset
tibble::tribble(
  ~shirt_color, ~x_offset,
  "Dark green", 3, #Louie
  "Bright red", 6, #Huey
  "Bright blue", 9 #Dewey
) %>% 
  #Run the function for each nephew & stack the resulting tibbles
  purrr::pmap_dfr(make_nephew_duck) %>% 
  #Bring back Donald for the fun of it
  dplyr::bind_rows(donald) %>% 
  #Render the model in 3D
  bricks_from_coords() %>% 
  build_bricks(
    background_color = "#fcedcc" 
  )

rgl::snapshot3d("output/day11_model2.png")
rgl::rgl.clear()

#Day 12, Model 1 ----
library(brickr)
library(dplyr)
library(tidyr)

#Define a model of Mario in English
c("1 plate Dark brown",
  "1 brick Earth blue",
  "1 plate Earth blue",
  "1 brick Bright red",
  "1 plate Light nougat",
  "1 plate Reddish brown",
  "2 plate Light nougat",
  "2 plate Bright red") -> mario

#Write a function to parse that English model
coords_from_words <- function(brick_words, x_offset = 0, y_offset = 0, z_offset=7){
  brick_words %>% 
    tibble::tibble(bricks=.) %>% 
    #Split the words into 3 groups
    mutate(bricks = purrr::map(bricks, 
                          ~stringr::str_split(.x, " ", n=3) %>% 
                            purrr::flatten() %>% 
                            setNames(c("count", "piece_type", "color")))) %>%
    #Unnest as columns
    unnest_wider(bricks) %>% 
    #Duplicate any repeated bricks
    uncount(as.numeric(count)) %>% 
    mutate(piece_type = substr(piece_type, 1 ,1)) %>% 
    #Complicated part is developing Z (3-height) and mid_level (1-height)
    mutate(
      this_height = ifelse(piece_type == "b", 3, 1),
      #Total height up until now
      cum_height = cumsum(this_height),
      #We really care about the previous height, not this height
      prev_height = lag(cum_height, default = 0),
      #Now convert that to z and mid_level %in% 0, 1, 2
      z = prev_height %/% 3 + 1,
      mid_level = prev_height %% 3 
    ) %>% 
    mutate(z = z + z_offset) %>% 
    select(z, mid_level, piece_type, color) %>%
    mutate(x=1, y=1) %>% 
    expand(x=1:2 + x_offset,
           y=1:2 + y_offset,
           nesting(z, mid_level, piece_type, color))
}
 
mario %>%
  coords_from_words() %>% 
  bricks_from_coords() %>% 
  build_bricks(background_color = "skyblue")

rgl::snapshot3d("output/day12_model1.png")
rgl::rgl.clear()

#Day 12, Model 2 ----

#Reuse our function for all Super Mario characters!
c("1 plate Dark brown",
  "1 brick Earth blue",
  "2 plate Earth blue",
  "1 brick Bright green",
  "1 plate Bright green",
  "1 plate Light nougat",
  "1 plate Reddish brown",
  "2 plate Light nougat",
  "2 plate Bright green") -> luigi

c("1 plate Vibrant coral",
  "2 brick Light purple",
  "1 plate Vibrant coral",
  "1 brick Light purple",
  "1 brick Light nougat",
  "2 plate Cool yellow",
  "1 plate Flame yellowish orange") -> peach

c("1 plate Bright yellow",
  "1 plate Bright orange",
  "2 brick Bright green",
  "1 plate Bright green",
  "1 plate White",
  "1 plate Bright green") -> yoshi

c("1 plate Reddish brown",
  "1 plate White",
  "1 plate Bright blue",
  "2 plate Light nougat",
  "1 plate White", 
  "1 plate Bright red",
  "1 plate White") -> toad


bind_rows(mario %>% coords_from_words(),
          luigi %>% coords_from_words(3),
          peach %>% coords_from_words(6),
          yoshi %>% coords_from_words(9),
          toad  %>% coords_from_words(12)
          ) %>% 
  bricks_from_coords() %>% 
  build_bricks(background_color = "skyblue")

rgl::snapshot3d("output/day12_model2.png")
rgl::rgl.clear()


#Day 13, Model 1 ----

library(brickr)

#Write a function to make a circle of bricks
circle <- function(radius, xc, yc, color="White",
                   z=2, piece_type="p", mid_level=0){
  tidyr::crossing(
    x=1:(radius*2),
    y=1:(radius*2),
    z=z, 
    color=color,
    mid_level=mid_level,
    piece_type=piece_type
  ) %>% 
    dplyr::mutate(
      dist = ((x-median(x))^2 + (y-median(y))^2)^(1/2)
    ) %>% 
    dplyr::filter(dist <= radius) %>% 
    dplyr::mutate(
      x=x+(xc-floor(median(x))), y=y+(yc-floor(median(y)))
    )
}

#Render normal selfie using bricks_from_mosaic()
png::readPNG("boring_selfie.png") %>% 
  image_to_mosaic(
    brightness = 1.1
  ) %>% 
  #Only make it 3D with 3 plates
  bricks_from_mosaic(mosaic_height = 3) %>% 
  build_bricks(
    background_color = "#fcedcc"
  )

#Now some circles on top of it!
# This uses bricks_from_coords()
# Don't close the rgl window after rendering the normal selfie
# Successive calls to build_bricks() will add bricks to the same scene
dplyr::bind_rows(
  circle(3, 18, 26, "White"),
  circle(3, 33, 26, "White"),
  circle(2, 19, 26, "Dark azur", mid_level = 1),
  circle(2, 34, 26, "Dark azur", mid_level = 1)
  ) %>% 
  bricks_from_coords() %>% 
  build_bricks(
    background_color = "#fcedcc"
  )

# rgl::snapshot3d("output/day13_model2.png")


#Day 14, Model 1 ----
library(brickr)

mario_nes <- jpeg::readJPEG("mario.jpg")
#Get the dimensions of the image so we can retain the proportions
dim(mario_nes) #539 high, 623 high
mosaic_dims <- round(c(623, 529)/10) #Every 10x10 pixels is 1 brick

#Convert the image to a mosaic. 
# Save it so we can use it later
mario_nes %>% 
  image_to_mosaic(mosaic_dims, 
                  #Change the color matching algorithm.
                  # euclidean matched the blue background better
                  method = "euclidean",
                  #Limit to a smaller palette to better match the retro colors
                  color_palette = c("universal", "generic")) -> mario_mosaic

#Render it as a plot
mario_mosaic %>% 
  build_mosaic()

ggplot2::ggsave("output/day14_model1.png", device = "png")

#Day 14, Model 2 ----
library(dplyr)
library(tidyr)

#Cheat & grab an intermediate mosaic object
# 'Img_lego' is a data frame of all 1x1 lego bricks
# We want to change the flat mosaic to a vertical mosaic...
# ... without the background
mario_mosaic$Img_lego %>%
  #Remove all shades of blue/lavendar (the background)
  filter(!stringr::str_detect(Lego_name, "blue|azur|laven")) %>% 
  # Remove the fuzzy edges around the blocks
  filter(!(y %in% c(32, 26))) %>% 
  #Swap Y for Z, change pieces to bricks
  mutate(z=y, y=10, color = Lego_name, piece_type="b") %>%
  select(x, y, z, color, piece_type) %>% 
  # Duplicate the base & [?] blocks
  bind_rows(
      filter(., z %in% c(1:6, 26:31)) %>%
      expand(y=4:10, nesting(x, z, color, piece_type))
  ) %>% 
  # Also make Mario pop out a bit
  bind_rows(
    filter(., z %in% c(15:25)) %>%
      expand(y=5:8, nesting(x, z, color, piece_type))
  ) %>% 
  #Remove object at the furthest Y so fully offset from background
  filter( !(y %in% 9:10 & z %in% c(15:25, 26:31))) -> mario_3d


#Render in 3D
mario_3d %>%
  bricks_from_coords() %>% 
  build_bricks(background_color = "skyblue")

rgl::snapshot3d("output/day14_model2.png")

