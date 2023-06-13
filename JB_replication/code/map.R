rm(list = ls())

libs <- c("httr", "tidyverse", "sf")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

# 1. GET RIVERS DATA
#---------

get_data <- function() {
  
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip"
  
  res <- httr::GET(url,
               write_disk("data/GIS/as_rivers.zip", overwrite = T),
             progress())
  unzip("data/as_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_as_shp", 
                          pattern="*.shp", full.names=T)
  
  
  return(filenames)
}

filenames <- get_data()

# Convert to multilinestring
load_rivers <- function() {
  list_riv <- lapply(filenames, sf::st_read)
  as_riv <- list_riv[[1]] |>
    sf::st_cast("MULTILINESTRING")
  
  return(as_riv)
}

as_riv <- load_rivers()

#Set width
get_river_width <- function() {
  as_riv_width <- as_riv |>
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 2 ~ 1.2,
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) |>
    sf::st_as_sf()
  
  return(as_riv_width)
}

as_riv_width <- get_river_width()

#Bounding box of India
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function() {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(67, 97, 97, 67, 67),
      c(5, 5, 37, 37, 5)
    ))),
    crs = crsLONGLAT
  )
  
  new_prj <- sf::st_transform(bbox, crs = 4087)
  bb <- sf::st_bbox(new_prj)
  
  return(bb)
}

bbox <- get_bounding_box()


# Get map
get_river_map <- function() {
  p <-
    ggplot() +
    geom_sf(
      data = as_riv_width,
      aes(
        color = factor(ORD_FLOW), size = width,
        alpha = factor(ORD_FLOW)
      )
    ) +
    coord_sf(
      crs = 4087,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    labs(
      y = "", subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "#08306b", "#08519c", "#2171b5",
        "#4292c6", "#6baed6", "#9ecae1",
        "#c6dbef", "#deebf7", "#deebf7"
      )
    ) +
    scale_size(range = c(0, .3)) +
    scale_alpha_manual(values = c(
      "2" = 1, "3" = 1, "4" = 1, "5" = .7, "6" = .6,
      "7" = .4, "8" = .3, "9" = .2, "10" = .1
    )) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 40, color = "#2171b5", hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 10, color = "grey60", hjust = 0.5, vjust = 10
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  return(p)
}

p1 <- get_river_map()

ggsave(
  filename = "data/Images/asian_rivers.png",
  width = 8.5, height = 7, dpi = 600,
  device = "png", bg = "white", p1
)


