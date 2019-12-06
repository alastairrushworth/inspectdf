#' @importFrom grDevices hcl

print_palette_pairs <- function(i){
  palette_list <- list(
    gg_default = gg_color_hue(5)[c(3, 1)],
    cbfriendly = c("#E69F00", "#56B4E9"),
    `80s` = c("#2bd1fc", "#ff3f3f"),
    rainbow = c("#e70000", "#ff8c00", "#ffef00", "#00811f",	"#0044ff", "#760089")[c(3, 1)],
    mario = c("#fed1b0", "#ee1c25", "#0065b3", "#ffffff", "#894c2f")[c(1, 3)], 
    pokemon = c("#b3ffa9", "#ffa9b3",	"#a9b3ff", "#fdff98", "#dfc189")[c(2, 3)]
  )
  return(palette_list[[i + 1]])
}

user_colours <- function(n, i){
  if(i == 0){
    colour_vector <- gg_color_hue(n)
  } else {
    palette_list <- list(
      cbfriendly = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
      `80s` = c("#ff48c4", "#2bd1fc", "#f3ea5f", "#c04df9", "#ff3f3f"),
      rainbow = c("#e70000", "#ff8c00", "#ffef00", "#00811f",	"#0044ff", "#760089"),
      mario = c("#fed1b0", "#ee1c25", "#0065b3", "#ffffff", "#894c2f"), 
      pokemon = c("#b3ffa9", "#ffa9b3",	"#a9b3ff", "#fdff98", "#dfc189")
    )
    if(i == 3){
      colour_vector <- rep(palette_list[[i]], length.out = n)
    } else {
      x <- colorRampPalette(palette_list[[i]])
      colour_vector <- x(n)
    }

  }
  colour_vector
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length.out = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

get_best_pair <- function(i){
  if(i == 0){
    out <- user_colours(3, i)[c(1, 3)]
  } else {
    out <- user_colours(9, i)[c(1 + (i == 1), 4)]
  }
  return(out)
}


