#set working directory
setwd("/home/andrey/Documents/R/ImageAnalysis")

# Install from CRAN
library(imager)

imagePath1 <- "landscape.jpg"
imagePath2 <- "processor.jpg"

jpegValueScale = 255
histogramStep = 10

customShow <- function(img, title){
  plot(img, main = title)
}

#img_path <- "processor.jpg"
#image_num <- 1

img_convertion <- function(image_num, img_path) {
  img_color <- load.image(img_path)
  customShow(img_color, sprintf("Image %d: color", image_num))
  img_gray <- grayscale(img_color)
  customShow(img_gray, sprintf("Image %d: grayscale", image_num))
  
  # Histogram
  img_gray_scaled <- jpegValueScale*img_gray[]
  histogram_res <- 
    hist(img_gray_scaled, main=sprintf("Grayscale histogram of image %d", image_num), 
         xlab = "", xlim = c(0, jpegValueScale),
         breaks = seq(0, jpegValueScale + histogramStep, by=histogramStep))
  
  list(gray = img_gray_scaled, hist = histogram_res)
}

main_calc <- function(image_num, histogram_res) {
  #hist_x <- histogram_res$breaks
  hist_y <- histogram_res$counts
  
  message("Image ", image_num, " mean = ", mean(hist_y))
  message("Image ", image_num, " square derivation = ", sd(hist_y))
  message("Image ", image_num, " median = ", median(hist_y))
  hist_y.t <- table(hist_y)
  message("Image ", image_num, " mode = ", sort(unique(hist_y))[which.max(hist_y.t)])
}

conv1 <- img_convertion(1, imagePath1)
main_calc(1, conv1$hist)
conv2 <- img_convertion(2, imagePath2)
main_calc(2, conv2$hist)

message("Histogram correlation = ", cor(conv1$hist$counts, conv2$hist$counts))
message("Grayscale image correlation = ", cor(conv1$gray, conv2$gray))

chi_test <- function(dots) {
  interval <- dots$hist$breaks
  len <- length(interval)
  interval[1] <- (-Inf)
  interval[len] <- (+Inf)
  p_for_dots <- pnorm(interval, mean=mean(dots$gray), sd=sd(dots$gray))
  p_for_dots <- (p_for_dots[2:len] - p_for_dots[1:(len - 1)])
  
  chisq.test(dots$hist$counts, p = p_for_dots)
}

chi1 <- chi_test(conv1)
message("Chi square for image 1: ", as.numeric(chi1$statistic), "; p = ", chi1$p.value)
chi2 <- chi_test(conv2)
message("Chi square for image 2: ", as.numeric(chi2$statistic), "; p = ", chi2$p.value)