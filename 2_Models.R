#Day 1, Model 1 -----

library(brickr)

#Each value in a data frame corresponds to a 1x1 brick
# 'Level' tells brickr how high to place the brick
# "x" values are the columns after 'Level'
# "y" values are implicit and correspond with the data frame rows
# Numeric values can default between 1 & 54. Each number is a different color!

data.frame(
  Level = "A",
  x1 = 4 
) -> this_is_a_brick

this_is_a_brick %>% 
  # Turn it into a brickr object
  bricks_from_table() %>% 
  # Render it!
  build_bricks()

rgl::snapshot3d("output/day3_model1.png")

# Day 1, Model 2 ----
library(brickr)

#Make a bigger model
# 7 bricks (numeric values)...
# ...spanning 2 Levels, 2 x-values (columns), 2 y-values (rows)
data.frame(
  Level = c("A", "A", "B", "B"),
  x1    = c(5,    5,   4,    4),
  x2    = c(5,    5,   4,    0) #0 means no brick!
) -> more_bricks

# Levels can be named anything (in alphanumeric order)
# ...but letters makes them easy to differentiate from bricks

more_bricks %>% 
  # Turn it into a brickr object
  bricks_from_table() %>% 
  # Render it!
  build_bricks()

# Don't worry, there are ways in brickr to make this easier!

rgl::snapshot3d("output/day3_model2.png")


# Day 2, Model 1 ----

# tibble::tribble() creates a data frame in a more intuitive way... row-wise!
# As close to WYSIWYG as you'll get in R script

# Each colum name begins with a ~ and does not need to be in quotes
tibble::tribble(
  ~Level, ~x1, ~x2, ~x3, ~x4, ~x5,
  "A",   3,   3,   3,   3,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   3,   3,   3,   3,
) %>% 
  bricks_from_table() %>% 
  build_bricks()

# Using tibble::tribble(), you can more easily the 3D LEGO model you create
#brickr automatically collects single adjacent 1x1 bricks into larger bricks

rgl::snapshot3d("output/day4_model1.png")

# Day 2, Model 2 ----

#Add more rows with increasing Levels to make a house!
# This stacks your bricks!

tibble::tribble(
  ~Level, ~x1, ~x2, ~x3, ~x4, ~x5,
  "A",   3,   3,   3,   3,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   3,   0,   3,   3,
  #---
  "B",   3,   3,   3,   3,   3,
  "B",   3,   0,   0,   0,   3,
  "B",   3,   0,   0,   0,   3,
  "B",   3,   3,   3,   3,   3,
  #---
  "C",   8,   8,   8,   8,   8,
  "C",   3,   3,   3,   3,   3,
  "C",   3,   3,   3,   3,   3,
  "C",   8,   8,   8,   8,   8,
  #---
  "D",   0,   0,   0,   0,   0,
  "D",   8,   8,   8,   8,   8,
  "D",   8,   8,   8,   8,   8,
  "D",   0,   0,   0,   0,   0
) -> my_simple_house

my_simple_house %>% 
  bricks_from_table() %>% 
  build_bricks()

#Wow! That's kinda easy and intuitive... maybe...

rgl::snapshot3d("output/day4_model2.png")

# Day 3, Model 1 ----

#Start with the same house has yesterday...
tibble::tribble(
  ~Level, ~x1, ~x2, ~x3, ~x4, ~x5,
  "A",   3,   3,   3,   3,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   0,   0,   0,   3,
  "A",   3,   3,   0,   3,   3,
  #---
  "B",   3,   3,   3,   3,   3,
  "B",   3,   0,   0,   0,   3,
  "B",   3,   0,   0,   0,   3,
  "B",   3,   3,   3,   3,   3,
  #---
  "C",   8,   8,   8,   8,   8,
  "C",   3,   3,   3,   3,   3,
  "C",   3,   3,   3,   3,   3,
  "C",   8,   8,   8,   8,   8,
  #---
  "D",   0,   0,   0,   0,   0,
  "D",   8,   8,   8,   8,   8,
  "D",   8,   8,   8,   8,   8,
  "D",   0,   0,   0,   0,   0
) -> my_simple_house

#Re-assign the colors
tibble::tribble(
  ~".value", ~Color,
  3, "Light purple",
  8, "Earth blue"
) -> repaint_the_house

#Render the house!
#Use the color_guide argument to explicitly assign colors.
my_simple_house %>% 
  bricks_from_table(
    color_guide = repaint_the_house
  ) %>% 
  build_bricks()

rgl::snapshot3d("output/day5_model1.png")

# Day 3, Model 2 ----

#Wait, what are those color names?

# Use build_colors() to see them all!
build_colors()

# Set the argument .names_only = TRUE to print a list of them to the console
# This makes it easy to copy/paste
build_colors(TRUE)

# See all the detail in the lego_colors data frame
View(lego_colors)

ggplot2::ggsave("output/day5_model2.png", device = "png", width = 7, height = 5)

# Day 6, Model 1 ----

#What about other brick shapes?
# brickr can do that, too!

#Use tribble() to make a simple table out of bricks
# 8 = Reddish brown, 11 = Nougat
tibble::tribble(
  ~Level, ~x1, ~x2, ~x3, ~X4,
  "A",   8,   0,   0,   8,
  "A",   0,   0,   0,   0,
  "A",   0,   0,   0,   0,
  "A",   8,   0,   0,   8,
  #---
  "B",   8,   0,   0,   8,
  "B",   0,   0,   0,   0,
  "B",   0,   0,   0,   0,
  "B",   8,   0,   0,   8,
  #---
  "C",  11,  11,  11,  11,
  "C",  11,  11,  11,  11,
  "C",  11,  11,  11,  11,
  "C",  11,  11,  11,  11
) -> simple_table

#Render it in 3D
simple_table %>% 
  bricks_from_table() %>% 
  build_bricks()

rgl::snapshot3d("output/day6_model1.png")
rgl::rgl.clear()

# Day 6, Model 2 ----

# Use the piece_matrix argument to tell brickr which shapes to use
# This argument expects a table identical in shape to the original table,
# ... but with piece ID letters instead of color ID
library(dplyr)

#dplyr 0.8 using mutate_*() & vars()
# Convert numeric columns to characters,
# Then replace those values with Piece IDs
simple_table %>% 
  mutate_if(is.numeric, as.character) %>% 
  mutate_at(vars(starts_with("x")), 
            ~case_when(
              . ==  8 ~ "c1", #Reddish brown legs become cylinders
              . == 11 ~ "p",  #Nought table top becomes a flatter plate
              TRUE ~ . #Everything else, as-is
            )) -> simple_table_bricks

#dplyr 1.0 using across()
simple_table %>% 
  mutate(across(is.numeric, as.character)) %>% 
  mutate(across(starts_with("x"), 
                ~case_when(
                  . ==  8 ~ "c1", #Reddish brown legs become cylinders
                  . == 11 ~ "p",  #Nought table top becomes a flatter plate
                  TRUE ~ . #Everything else, as-is
                ))) -> simple_table_bricks

#Build table with the new piece table
simple_table %>% 
  bricks_from_table(
    piece_matrix = simple_table_bricks
  ) %>% 
  build_bricks()

#Check out brickr.org for more information about pieces
# http://brickr.org/articles/models-piece-type.html

rgl::snapshot3d("output/day6_model2.png")
rgl::rgl.clear()

# Day 7, Model 1 ----

#Bricks, and therefore Levels, are 3-units tall
# Plates are only 1-unit
# Add a `mid_level` column to your model to stack pieces with different heights

#Same Level repeated, but increasing mid_level
tibble::tribble(
  ~Level, ~mid_level, ~x1, ~x2, 
  "A", 0,   3,   3,  
  "A", 0,   3,   3,
  #---
  "A", 1,   4,   4, 
  "A", 1,   4,   4,  
  #---
  "A", 2,   5,   5,
  "A", 2,   5,   5
) -> stacked_colors

#Render the 3D model
stacked_colors %>% 
  bricks_from_table(
    #Duplice stacked_colors, making all values "p" for plate
    piece_matrix = stacked_colors %>% 
      mutate_at(vars(starts_with("x")), ~"p")
  ) %>% 
  build_bricks()

rgl::snapshot3d("output/day7_model1.png")
rgl::rgl.clear()

# Day 7, Model 2 ----

#Another house model. This one is tinier.
tibble::tribble(
  ~Level, ~mid_level, ~x1, ~x2, 
  "A", 0,  24,  24,  
  "A", 0,  24,  24,
  #---
  "B", 0,  24,  24, 
  "B", 0,  24,  24,  
  #---
  "B", 1,   8,   8,
  "B", 1,   8,   8
) -> tiny_house_colors

tibble::tribble(
  ~Level, ~mid_level, ~x1, ~x2, 
  "A", 0,  "b",  "b",  
  "A", 0,  "b",  "b",
  #---
  "B", 0,  "p",  "p", 
  "B", 0,  "p",  "p",  
  #---
  "B", 1,   "w4",   "w2",
  "B", 1,   "w4",   "w2"
) -> tiny_house_bricks

# The build_bricks() function takes arguments to customize the look of your model
tiny_house_colors %>% 
  bricks_from_table(
    piece_matrix = tiny_house_bricks
  ) %>% 
  build_bricks(
    rgl_lit = FALSE, # Turn off lighting for flat color
    outline_bricks = TRUE, #Outline bricks for a cartoon-y look
    background_color = "#FFFF99" #Yellow background is more fun than white
  )

rgl::snapshot3d("output/day7_model2.png")
rgl::rgl.clear()

