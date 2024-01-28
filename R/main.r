# 1. PACKAGES

install.packages("remotes")
remotes::install_github(
    "ropensci/osmdata"
)

libs <- c(
    "sf",
    "terra",
    "tidyverse",
    "osmdata",
    "maptiles",
    "classInt",
    "tidyterra"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs],
        dependencies = T
    )
}

invisible(
    lapply(
        libs,
        library,
        character.only = T
    )
)

# 2. CITY BOUNDARIES
#----------------------

place_name <- "Paris, France"

city_border <- osmdata::getbb(
    place_name = place_name,
    format_out = "sf_polygon",
    limit = 1,
    featuretype = "settlement"
) |>
sf::st_set_crs(4326)

plot(
    sf::st_geometry(
        city_border
    )
)

# 3. DATA
#--------

options(timeout = 999)

urls <- c(
    "https://data.humdata.org/dataset/da0f4294-57ea-473d-98b8-315f1135f793/resource/2cddeeda-3263-4c23-973d-9b931b9c3066/download/fra_men_geotiff.zip",
    "https://data.humdata.org/dataset/da0f4294-57ea-473d-98b8-315f1135f793/resource/fc23467b-63ca-4013-8100-98cb68fc9336/download/fra_women_geotiff.zip")

for(url in urls){
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

main_dir <- getwd()

zip_files <- list.files(
    path = main_dir,
    pattern = ".zip",
    full.names = T
)

lapply(
    zip_files,
    unzip
)

# 4. LOAD RASTER FILE
#--------------------

raster_files <- list.files(
    path = main_dir,
    pattern = ".tif$",
    full.names = T
)

raster_list <- lapply(
    raster_files,
    terra::rast
)

# 5. CROP DATA
#-------------

city_rasters <- lapply(
    raster_list,
    function(x){
        terra::crop(
            x,
            terra::vect(
                city_border
            ),
            snap = "in",
            mask = T
        )
    }
)

# 6. CALCULATE HUMAN SEX RATIO
#-----------------------------

sex_ratio <- (
    (
        100 * city_rasters[[1]]
    ) / city_rasters[[2]]
)

terra::plot(sex_ratio)

# 7. STREET LAYER
#----------------

city_bbox <- sf::st_bbox(
    city_border
) |>
sf::st_as_sfc(
    crs = 4326
)

layer <- maptiles::get_tiles(
    city_bbox,
    provider = "CartoDB.Positron",
    zoom = 12,
    crop = T,
    project = F
)

terra::plot(layer)
maptiles::plot_tiles(layer)

# 8. REPROJECT RASTER
#--------------------

sex_ratio_reproj <- terra::project(
    sex_ratio,
    terra::crs(layer)
)

# 9. RASTER TO DATAFRAME
#-----------------------

sex_ratio_df <- sex_ratio_reproj |>
    as.data.frame(xy = T)

head(sex_ratio_df)
names(sex_ratio_df)[3] <- "ratio"

# 10. BREAKS
#-----------

min_val <- min(sex_ratio_df$ratio)
max_val <- max(sex_ratio_df$ratio)
limits <- c(min_val, max_val)

breaks <- classInt::classIntervals(
    var = sex_ratio_df$ratio,
    n = 6,
    style = "equal"
)$brks

# 11. MAP
#--------

p <- ggplot(data = sex_ratio_df) +
tidyterra::geom_spatraster_rgb(
    data = layer
) +
geom_tile(
    aes(
        x = x,
        y = y,
        fill = ratio
    ),
    na.rm = T
) +
scale_fill_gradient2(
    name = "",
    low = "#2686A0",
    mid = "#EDEAC2",
    high = "#A36B2B",
    midpoint = 100,
    limits = limits,
    breaks = round(breaks, 0),
    na.value = "white",
    guide = "colourbar"
) +
guides(
    fill = guide_colorbar(
        direction = "vertical",
        label.position = "right",
        label.hjust = 0,
        nrow = 1,
        drop = F
    )
) +
theme_void() +
theme(
    legend.position = c(.1, .85),
    legend.text = element_text(
        size = 12, color = "grey10"
    ),
    plot.margin = unit(
        c(
            t = 0, r = 0,
            b = 0, l = 0
        ), "lines"
    )
)

w <- ncol(sex_ratio_reproj)
h <- nrow(sex_ratio_reproj)

ggsave(
    "paris-sex-balance.png",
    p,
    width = w * 5,
    height = h * 5,
    units = "px",
    bg = "white"
)
