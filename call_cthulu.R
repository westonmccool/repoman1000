
#' Function to call cthulu
#'
#' @return
#' @export
#'
#' @examples
call_cthulu <- function() {
  
  # https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG
  
  img <- png::readPNG("cthulu.png")
  
  if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
    
    plot(1:2, 
         type='n',
         frame.plot = FALSE,
         xlab = "",
         ylab = "",
         axes = FALSE)
    
    if (names(dev.cur()) == "windows") {
      # windows device doesn't support semi-transparency so we'll need
      # to flatten the image
      transparent <- img[,,4] == 0
      img <- as.raster(img[,,1:3])
      img[transparent] <- NA
      
      # interpolate must be FALSE on Windows, otherwise R will
      # try to interpolate transparency and fail
      rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
      
    } else {
      # any reasonable device will be fine using alpha
      rasterImage(img, 1.2, 1.27, 1.8, 1.73)
    }
  }
  
  lapply(1:1e6, function(x) print("cthulu"))

}
