# Housekeeping -----------------------------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse") else library("tidyverse")
if (!require("magick")) install.packages("magick") else library("magick")
if (!require("fs")) install.packages("fs") else library("fs")
if (!require("bmp")) install.packages("bmp") else library("bmp")



# Load single image ------------------------------------------------------------

filepath <- file.path("C:","Users","fabio","Documents","GitHub","chest-x-rays",
                 "files","01 original", "train", "NORMAL", "IM-0115-0001.jpeg")

image <- image_read(filepath)

image %>% str

image %>% image_attributes()
image %>% image_get_artifact()
image %>% image_scale("%25")
image %>% image_charcoal() %>% image_scale("%25")
image %>% image_negate() %>% image_scale("%25")
image %>% image_implode(factor = 1) %>% image_scale("%25")

kernel <- matrix(rep(-1,9),nrow=3)
kernel[2,2] <- 8
kernel <- kernel + 1/9

image %>% image_convolve(kernel) %>% image_scale("%25")

image %>% image_convolve("Sobel") %>% image_negate() %>% image_scale("%25")

kernel <- matrix(0,nrow=3,ncol=3)
kernel[2,c(1,3)] <- .25
kernel[c(1,3),2] <- .25

image %>% as.raster() %>% as.matrix() %>% magrittr::extract(1:10,1:10) 
str_remove("#") %>% strtoi(base = 16L)

image %>% as.matrix()

image %>% image_canny("0x10+40%+60%") %>% image_scale("%25")
image %>% image_convolve("Prewitt") %>% image_negate() %>% image_scale("25%")
image %>% image_convolve("Sobel") %>% image_negate() %>% image_scale("25%")
image %>% image_convolve("LoG:0,2") %>% image_scale("25%")
image %>% image_edge(radius = 3) %>% image_contrast() %>% image_negate() %>% image_scale("30%")
image %>% image_contrast(sharpen = 1) %>% image_scale("30%")
image %>% image_scale("30%")


# Transform image to matrix ----------------------------------------------------

image.matrix <-
  image %>% 
  as.raster() %>% 
  as.matrix() %>% 
  str_sub(2,3) %>% 
  strtoi(base = 16L) %>% 
  matrix(nrow = 1858, ncol = 2090, byrow = FALSE)

image.matrix.reduced <-
  image.matrix[1:1858 %% 10 == 0, 1:2090 %% 10 == 0]

image.matrix.reduced[image.matrix.reduced < 30] <- 255
image.matrix.reduced[image.matrix.reduced > 180] <- 255
image.matrix.reduced[(image.matrix.reduced > 120) & (image.matrix.reduced < 150)] <- 255

image.matrix.reduced %>% image(col = gray.colors(256, start = 1, end = 0))

image.matrix %>% hist()





image.bmp <- image_convert(image, format = "bmp")


image.matrix <-
  image %>% 
  as.raster() %>% 
  as.matrix() %>% 
  str_sub(2,3) %>% 
  strtoi(base = 16L) %>% 
  matrix(nrow = 1858, ncol = 2090, byrow = TRUE)

image.matrix %>% image
