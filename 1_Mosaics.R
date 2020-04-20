# Day 1, Mosaic 1 ----

# install.packages("brickr")
library(brickr)

#Upload an awesome selfie (or any photo!)
awesome_selfie <- png::readPNG("awesome_selfie.png")

awesome_selfie %>% 
  #Create a mosaic object
  image_to_mosaic() %>% 
  #Plot it!
  build_mosaic()

ggplot2::ggsave("output/day1_mosaic1.png", device="png")

#brickr Pro-Tips

# 1. Use a close-up photo of a face or object. Mosaics are low-res & only 42 colors!
# 2. Minimize noise in the background. Use www.remove.bg to remove & replace the bg of your image.
# 3. Play around with the brightness, size, & color_palette arguments
# 4. See more examples at www.brickr.org

# Day 1, Mosaic 2 ----

#Customize it!

jpeg::readJPEG("awesome_selfie2.jpg") %>% #This selfie has a white background
  image_to_mosaic(
    60, #Make it bigger! (Default is 48x48 bricks)
    color_palette = "bw", #Make it black & white
    brightness = 1.1, #Make it brighter
    contrast = 1.2, #Up the contrast
    use_bricks = c("4x6", "2x4", "1x2", "1x1") #Choose the bricks to use
  ) %>% 
  build_mosaic("Awesome Black & White Selfie") #Add a title

ggplot2::ggsave("output/day1_mosaic2.png", device="png")

# Day 2, Mosaic 3 ----

# install.packages("brickr")
library(brickr)

#Upload an awesome selfie (or any photo!)
awesome_selfie <- png::readPNG("awesome_selfie.png")

#Make it a 3D mosaic
awesome_selfie %>% 
  #Create a mosaic object
  image_to_mosaic() %>% 
  #Convert it to 3D bricks
  bricks_from_mosaic() %>% 
  #Render the 3D model
  build_bricks()

# Day 2, Mosaic 4 ----

# install.packages("brickr")
library(brickr)

#Upload an awesome selfie (or any photo!)
awesome_selfie <- png::readPNG("awesome_selfie.png")

#Make it pop art
#4 mix-ups of the rgb channels... not the default c(1, 2, 3)
list(c(2, 3, 1), c(3, 2, 1), 
     c(1, 3, 2), c(3, 1, 2)) %>% 
  purrr::map(
   ~ awesome_selfie %>% 
      image_to_mosaic(32, #Make it smaller
                      brightness = 1.2, #and brighter
                      warhol = .x #And pass it the new rgb values
                      ) %>% 
      build_mosaic()
  ) %>% 
  #Use {patchwork} to arrange a list of ggplots!
  patchwork::wrap_plots()

ggplot2::ggsave("output/day2_mosaic4.png", device="png", width = 8, height=6)

# Day 3, Mosaic 1 ----

# install.packages("brickr")
library(brickr)

#Create a mosaic
awesome_mosaic <- png::readPNG("awesome_selfie.png") %>% 
  image_to_mosaic() 

#Plot it!
awesome_mosaic %>% 
  build_mosaic()

#Generate instructions
awesome_mosaic %>% 
  build_instructions()

#Print your pieces
awesome_mosaic %>% 
  build_pieces()


ggplot2::ggsave("output/day3_mosaic1.png", device="png")




jpeg::readJPEG("family.jpg") %>% 
  image_to_mosaic(
    img_size = c(120, 90),
    dithering = F
  ) %>% 
  build_mosaic()

ggplot2::ggsave("output/day3_mosaic5_nodither.png", device = "png", width = 16, height=12)


