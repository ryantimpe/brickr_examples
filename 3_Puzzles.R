# Day 8, Model 1 ----
library(brickr)
library(dplyr)

#Begin with 1 turtle, Raphael
# He wears a red mask

#Rather than define all 4 bricks per Level, just define 1
tibble::tribble(
  ~Level, ~mid_level, ~x1,
  "A", 0, 7, #Green
  "B", 0, 7,
  "C", 0, 14, #Orange
  "D", 0, 14,
  "E", 0, 7, 
  "E", 1, 7,
  "E", 2, 3, #Red
  "F", 0, 7
) -> skinny_raphael

#Turn the 1-brick models into 4-brick models
skinny_raphael %>%
  tidyr::uncount(2) %>% #Duplicate every row
  mutate(x2 = x1) -> #Duplicate the value column
  raphael

#Some of the bricks should actually be plates.
# Turn bricks on Levels E and F into plates
raphael %>% 
  mutate_at(vars(x1, x2), 
            ~as.character(ifelse(Level %in% c("E", "F"), "p", "b"))) ->
  raphael_bricks 

#Render it in 3D!
raphael %>% 
  bricks_from_table(
    piece_matrix = raphael_bricks
  ) %>% 
  build_bricks(
    background_color = "#fcedcc" #Match the background color in the puzzle
  )

rgl::snapshot3d("output/day8_model1.png")
rgl::rgl.clear()

# Day 8, Model 2 ----

#Use {purrr} to duplicate the turtles & change the mask color
# Change Rapheal's red for each turtle
c(
  4, #Bright blue for Leonardo,
  29, #Flame yellowish organge for Michelangelo 
  22  #Medium lavendar for Donatello
) %>% 
  #map_dfc will bind columns
  purrr::map_dfc(
    ~raphael %>% 
      #Might be an easier way, but save this new color as a column...
      #... to use in the next formula inside mutate_at()
      mutate(new_col = .x) %>% 
      mutate_at(vars(x1, x2),
                ~ifelse(.==3, new_col, .)) %>% 
      #Create any empty column to leave a space between turtles
      mutate(x3=0) %>% 
      #Only keep the value columns
      select(x3, x1, x2)
  ) %>% 
  #Bind this new tibble to the right of the Raphael table
  bind_cols(raphael, .) ->
  four_turtles

#Split between bricks & plates, as before
four_turtles %>% 
  mutate_at(vars(starts_with("x")), 
            ~as.character(ifelse(Level %in% c("E", "F"), "p", "b"))) ->
  four_turtle_bricks 

#Render in 3D
four_turtles %>% 
  bricks_from_table(
    piece_matrix = four_turtle_bricks
  ) %>% 
  build_bricks(
    background_color = "#fcedcc" 
  )

rgl::snapshot3d("output/day8_model2.png")
rgl::rgl.clear()

#Day 9, Model 1 ----
#Simpsons brick puzzles

#Start with simple 1-brick models instead of 4-brick
# 5 Simpsons characters, one in each X column
tibble::tribble(
  ~Level, ~mid_level, ~x1, ~x2, ~x3, ~x4, ~x5,
  "A", 0, 13, 15, 13, 14, 30,
  "B", 0, 13, 15, 13, 14, 30,
  "B", 1,  0,  0,  3,  0, 30,
  "B", 2,  0,  0,  3,  0, 31,
  "C", 0,  1, 15,  0, 14,  0,
  "C", 1,  0,  0,  0, 31,  0,
  "C", 2,  0,  0, 31, 31,  0,
  "D", 0,  1, 15, 31,  0,  0,
  "E", 0, 31, 31,  0,  0,  0,
  "E", 1,  0, 31,  0,  0,  0,
  "E", 2,  0, 31,  0,  0,  0,
  "F", 0, 31,  0,  0,  0,  0,
  "F", 2,  0,  4,  0,  0,  0,
  "G", 2,  0,  4,  0,  0,  0,
  "H", 2,  0,  4,  0,  0,  0,
  "I", 2,  0,  4,  0,  0,  0,
) -> simpson_colors

#Manually produce a piece table in the same shape
# No real pattern to bricks vs plates, so can't use a formula
tibble::tribble(
  ~Level, ~mid_level, ~x1, ~x2, ~x3, ~x4, ~x5,
  "A", 0,"b","b","b","b","b",
  "B", 0,"b","b","p","b","p",
  "B", 1, NA, NA,"p", NA,"p",
  "B", 2, NA, NA,"b", NA,"b",
  "C", 0,"b","b", NA,"p", NA,
  "C", 1, NA, NA, NA,"p", NA,
  "C", 2, NA, NA,"p","b", NA,
  "D", 0,"b","b","b", NA, NA,
  "E", 0,"b","p", NA, NA, NA,
  "E", 1, NA,"p", NA, NA, NA,
  "E", 2, NA,"b", NA, NA, NA,
  "F", 0,"b", NA, NA, NA, NA,
  "F", 2, NA,"b", NA, NA, NA,
  "G", 2, NA,"b", NA, NA, NA,
  "H", 2, NA,"b", NA, NA, NA,
  "I", 2, NA,"b", NA, NA, NA,
) -> simpson_bricks

#Function to convert 1-brick models to 4-brick
one_brick_to_four <- function(dat){
  dat %>% 
    #For each Simpsons character, made 2 new columns...
    # ... Duplicate the bricks, name them with _1
    # ... Add an empty column after each character, named with _2
    mutate_at(vars(starts_with("x")), 
              list("1" = ~.x, "2" = ~NA)) %>% 
    #Duplicate each row (y-values)
    tidyr::uncount(2) %>% 
    #Order columns... Level, mid_level,...
    # Then alphabetical to group the Simpsons characters correctly
    select(Level, mid_level, sort(tidyselect::peek_vars()))
}

#Convert both the color & piece tables to full tables
list(colors = simpson_colors, bricks = simpson_bricks) %>% 
  purrr::map(one_brick_to_four) ->
  simpsons_full

#Render them in 3D
simpsons_full[["colors"]] %>% 
  bricks_from_table(
    piece_matrix = simpsons_full[["bricks"]]
  ) %>% 
  build_bricks(
    background_color = "#fcedcc" 
  )

rgl::snapshot3d("output/day9_model1.png")
