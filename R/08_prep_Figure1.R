world_dem <- terra::rast("./Data/DEM/mn30_grd/w001001.adf")
RE_extent <- sf::st_read("./Data/extent/roman_empire_ad_117.gpkg")
RE_extent2 <- sf::st_union(sf::st_make_valid(RE_extent))
RE_extent2 <- sfheaders::sf_remove_holes(obj = RE_extent2)
sf::st_write(RE_extent2, "./Data/extent/roman_empire_ad_117_combined.gpkg", append = FALSE)

world_dem2 <- terra::crop(world_dem, sf::st_buffer(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(RE_extent))), dist = 100))
world_dem2[world_dem2 <= 0] <- NA 
terra::writeRaster(x = world_dem2, filename = "./Data/DEM/RE_dem.tif", overwrite = TRUE)

r <- terra::rast("./Data/DEM/OS_50m_Wales.tif")
r_extent <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(r)))
r_extent <- sf::st_transform(r_extent, crs = sf::st_crs(world_dem))

sf::st_write(r_extent, "./Data/extent/roman_wales_extent.gpkg", append = FALSE)
