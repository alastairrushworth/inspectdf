#' @importFrom grDevices hcl

user_colours <- function(n, i){
  if(i == 0){
    colour_vector <- gg_color_hue(n)
  } else {
    palette_list <- list(
      cbfriendly = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
      `80s` = c("#ff48c4", "#2bd1fc", "#f3ea5f", "#c04df9", "#ff3f3f"),
      rainbow = c("#ed2b2c", "#f27630", "#f9d84b", "#31a06b",	"#2b55f7"),
      mario = c("#fed1b0", "#ee1c25", "#0065b3", "#ffffff", "#894c2f"), 
      pokemon = c("#b3ffa9", "#ffa9b3",	"#a9b3ff", "#fdff98", "#dfc189")
    )
    x <- colorRampPalette(palette_list[[i]])
    colour_vector <- x(n)
  }
  colour_vector
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



