#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Plot a map
#'
#' This function plots a map
#'
#' @param osm_object OSM object to plot
#' @param color Color theme applied to the plot
#' @param scaling scaling applied to the plot 
#' @return NULL
#' @export
plot_map = function(osm_object, color, scaling) {
  
  scale_factor <- mean(scaling[1],scaling[2])
  
  ggplot2::ggplot(osm_object$x$osm_lines) +
    {if(!is.null(osm_object$x.water$osm_lines)) ggplot2::geom_sf(data =osm_object$x.water$osm_lines, fill = color$water, color= color$water, size = 1)} +
    #{if(!is.null(x.water$osm_multipolygons)) geom_sf_pattern(data =x.water$osm_multipolygons, pattern="circle", 
    #                                                         pattern_density = 0.05, pattern_spacing = 0.005, pattern_angle = 0,pattern_color = NA,pattern_fill="black", pattern_alpha=1,fill = color$water, color =color$water)} +
    #{if(!is.null(x.water$osm_polygons)) geom_sf_pattern(data =x.water$osm_polygons, pattern="circle", pattern_fill="black", pattern_color = NA,pattern_alpha=0.1, fill = color$water, color =color$water)} +
    {if(!is.null(osm_object$x.water$osm_multipolygons)) ggplot2::geom_sf(data =osm_object$x.water$osm_multipolygons, fill = color$water, color =color$water)} +
    {if(!is.null(osm_object$x.water$osm_polygons)) ggplot2::geom_sf(data =osm_object$x.water$osm_polygons, fill = color$water, color =color$water)} +
    {if(!is.null(osm_object$x.sea$osm_polygons)) ggplot2::geom_sf(data =osm_object$x.sea$osm_polygons, fill = color$water, color =color$water)} +
    {if(!is.null(osm_object$x.sea$osm_multipolygons)) ggplot2::geom_sf(data =osm_object$x.sea$osm_multipolygons, fill = color$water, color =color$water)} +
    
    
    ggplot2::geom_sf(data =osm_object$x.green$osm_multipolygons, fill = color$green, color=NA , linewidth=0.05) + # "#2F3737"
    ggplot2::geom_sf(data =osm_object$x.green$osm_polygons, fill = color$green, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.beach$osm_multipolygons, fill = color$beach, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.parking$osm_multipolygons, fill = color$parking, color= NA, linewidth=0.05) +
    
    
    {if(!is.null(osm_object$x.railway$osm_lines))
      ggplot2::geom_sf(data =osm_object$x.railway$osm_lines %>% dplyr::filter(if (!is.null(osm_object$x.railway$osm_lines$tunnel)) is.na(osm_object$x.railway$osm_lines$tunnel) else rep(TRUE,dim(osm_object$x.railway$osm_lines)[1])),
                       linetype = "11",
                       color=color$railway, linewidth = 0.5)} +
    
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "motorway") , color=color$street, linewidth=6*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "primary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "secondary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "tertiary") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "unclassified") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "residential") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "pedestrian") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "service") , color=color$street, linewidth=1*scale_factor) +
    #geom_sf(data =x$osm_lines %>% filter(highway == "footway") , color=color$street, linewidth=0.5/color$scale) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "living_street") , color=color$street, linewidth=1*scale_factor) +
    
    ggplot2::geom_sf(data =osm_object$x1$osm_multipolygons, ggplot2::aes(fill = colors),show.legend = F, color= NA, linewidth = 0*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x1$osm_polygons, ggplot2::aes(fill = colors), show.legend = F,color= NA, linewidth =0*scale_factor)+ # "#2F3737"
    ggplot2::scale_fill_manual(values=color$palette_building)+
    
    #labs(caption=center[3])+
    ggplot2::theme_void()+
    
    ggplot2::theme(#axis.title.x = element_text(hjust=0.5, size=rel(2), color="orange", face = "bold", family = "Roboto"),
      plot.caption = ggplot2::element_text(hjust=0.5, size=ggplot2::rel(2/4), color=color$street, face="bold",family = "Roboto"),
      panel.border = ggplot2::element_rect(colour = NA, fill=NA, linewidth = 3/scale_factor),#color$street 
      panel.background = ggplot2::element_rect(fill=color$background),
      plot.title = ggplot2::element_text(size=scale_factor*40*2.845276,family = "Poppins",face = "bold",hjust = 1,colour = "#292e28"),
      plot.subtitle = ggplot2::element_text(size=scale_factor*10*2.845276,family = "Poppins",face = "bold",hjust = 1,colour = "#292e28"),
      
      
      plot.margin = ggplot2::margin(t = 100*scaling[1], r = 80*scaling[2], b = 80*scaling[1], l = 80*scaling[2], unit = "mm"))+
    
    ggplot2::labs(title=stringr::str_to_lower(center[3]))+
    
    ggplot2::coord_sf(xlim = c(osm_object$my_bbox[1]+(osm_object$my_bbox[3]-osm_object$my_bbox[1])/20,
                               osm_object$my_bbox[3]-(osm_object$my_bbox[3]-osm_object$my_bbox[1])/20),
                      ylim = c(osm_object$my_bbox[2]+(osm_object$my_bbox[4]-osm_object$my_bbox[2])/20, 
                               osm_object$my_bbox[4]-(osm_object$my_bbox[4]-osm_object$my_bbox[2])/20))
}


#' Create color theme
#'
#' This function creates a theme
#'
#' @param palette The color palette
#' @return A color palette for plot_map()
#' @export
get_theme = function(palette) {
  color <- c()
  
  color$name <- palette
  
  if (palette == "macau") {
    # MACAU
    color$palette_building = c("#FFC857","#E9724C","#C5283D")
    color$water <- "#a8e1e6"
    color$green <- "#8BB174"
    color$beach <- "#FCE19C"
    color$parking <- "#F2F4CB"
    color$street <- "#475657"
    color$background <- "#F2F4CB"
    color$railway <- "#475657"
    
  }
  
  if (palette == "barcelona") {
    color$background <- "#F2F4CB"
    color$water <- "#a8e1e6"
    color$green <- "#8BB174"
    color$streets <- "#2F3737"
    color$palette_building <- c("#433633","#433633", "#FF5E5B")
    color$beach <- "#FCE19C"
    color$parking <- "#F2F4CB"
    color$railway <- "#1b1b1b"
    
  }
  
  # NIGHT
  if (palette == "night") {
    color$palette_building = c("#855988","#6B4984", "#483475")
    color$water <- "#192058"
    color$green <- "#2B2F77"
    color$railway <- "#1b1b1b"
    
    color$beach <- "#2F2352"
    color$parking <- "#2F2352"
    color$street <- "#1C1F31"
    color$background <- "#060A2E"
  }
  
  # IMHOF
  if (palette == "imhof") {
    color$palette_building = c("#423a40","#473F45", "#4F464D") 
    color$railway <- "#A12B3F"
    color$green <- "#cad8b5"
    color$water <- "#2d796f" #"#97B2B9"
    color$background <- "#97B2B9" # "#D3CCAF" #"#192058"
    color$street <- "#97B2B9" # "#B4D3DB"
    color$beach <- "#FCE19C"
    color$parking <- "#F2F4CB"
  }
  
  # BW
  if (palette == "bw") {
    color$palette_building = c("#FFFFFF","#FFFFFF", "#FFFFFF")
    color$railway <- "#292e28"
    color$green <- "#FFFFFF"
    color$water <- "#292e28" #"#97B2B9"
    color$background <- "#ffffff" # "#D3CCAF" #"#192058"
    color$street <- "#292e28"
    color$beach <- "#ffffff"
    color$parking <- "#Ffffff"
  }
  
  if (palette == "bwinv") {
    color$palette_building = c("#292e28","#292e28", "#292e28")
    #color$palette_building = c(NA,NA, NA)
    color$railway <- "#faf5eb"#FFFFFF"
    color$green <- "#faf5eb"
    color$water <- "#faf5eb" #"#97B2B9"
    color$background <- "#292e28" # "#D3CCAF" #"#192058"
    color$street <- "#faf5eb"
    color$beach <- "#faf5eb"
    color$parking <- "#faf5eb"
  }
  
  # ABC
  if (palette == "abc") {
    color$palette_building = c("#ED6012","#046D69", "#383922")
    color$railway <- "#1b1b1b"
    color$green <- "#7C6236"
    color$water <- "#1b1b1b" #"#97B2B9"
    color$background <- "#E5DBA9" # "#D3CCAF" #"#192058"
    color$street <- "#E5DBA9"
    color$beach <- "#E5DBA9"
    color$parking <- "#E5DBA9"
  }
  
  if (palette == "bauhaus") {
    color$palette_building = c("#b92c35","#b92c35", "#b92c35")
    color$railway <- "#1b1b1b"
    color$green <- "#f1b719"
    color$water <- "#25628a" #"#97B2B9"
    color$background <- "#f8ede3" # "#D3CCAF" #"#192058"
    color$street <- "#f8ede3"
    color$beach <- "#E5DBA9"
    color$parking <- "#E5DBA9"
  }
  
  
  if (palette == "ottowagner") {
    color$palette_building = c("#E9E6D7","#F8F4E9", "#E9E6D7")
    color$railway <- "#1b1b1b"
    color$green <- "#fc6257"
    color$water <- "#f4bf70" #"#97B2B9"
    color$background <- "#f5f2ee" # "#D3CCAF" #"#192058"
    color$street <- "#A4A8A4"
    color$beach <- "#D1B488"
    color$parking <- "#D1B488"
  }
  
  if (palette == "corbusier") {
    color$palette_building = c("#ff643d","#ff643d", "#ff643d")
    color$railway <- "#1b1b1b"
    color$green <- "#e8f7d1"
    color$water <- "#a5c1cf" #"#97B2B9"
    color$background <- "#e8f7d1" # "#D3CCAF" #"#192058"
    color$street <- "#0f0f0e"
    color$beach <- "#e8f7d1"
    color$parking <- "#e8f7d1"
  }
  
  if (palette == "wesanderson") {
    color$palette_building = c("#dea286","#dea286", "#dea286")
    color$railway <- "#1b1b1b"
    color$green <- "#e8f7d1"
    color$water <- "#a5c1cf" #"#97B2B9"
    color$background <- "#e8f7d1" # "#D3CCAF" #"#192058"
    color$street <- "#0f0f0e"
    color$beach <- "#e8f7d1"
    color$parking <- "#e8f7d1"
  }
  
  if (palette == "berlin") {
    color$palette_building = c("#ff643d","#80321F", "#E65A37")
    color$railway <- "#a38263"
    color$green <- "#d2d8b3"
    color$water <- "#a1bcae" #"#97B2B9"
    color$background <- "#fff5d9"
    color$street <- "#fff5d9"
    color$beach <- "#fff5d9"
    color$parking <- "#fff5d9"
  }
  
  if (palette == "dreamy") {
    color$palette_building = c("#cdb4db","#ffc8dd", "#ffc8dd")
    color$railway <- "#ffafcc"
    color$green <- "#ffafcc"
    color$water <- "#a2d2ff" #"#97B2B9"
    color$background <- "#bde0fe"
    color$street <- "#bde0fe"
    color$beach <- "#bde0fe"
    color$parking <- "#bde0fe"
  }
  
  return(color)
}



#' Calculate rectengular border given the coordinates
#'
#' This function calculates correct borders
#'
#' @param lat Latitude WSG84
#' @param lon Latitude WSG84
#' @param offlat offset Latitude
#' @param offlon offset Longitude
#' @return The border of the map in Coordinates
#' @export
get_border <- function(lat,lon,offlat,offlon) {
  # Earth's radius, sphere
  R = 6378137
  
  # offsets in meters
  dn = offlat
  de = offlon
  
  # Coordinate offsets in radians
  dLat = dn/R
  dLon = de/(R*cos(3.1415*lat/180))
  
  # Offset, decimal degrees
  ymax = lat + dLat * 180/3.1415
  xmax = lon + dLon * 180/3.1415 
  ymin = lat - dLat * 180/3.1415
  xmin = lon - dLon * 180/3.1415 
  
  return(c(xmin,ymin,xmax,ymax))
}


#' Retrieve OSM data
#'
#' This function retrieves OSM data
#'
#' @param center Center coordinate
#' @param y_distance Y distance in meters
#' @param x_distance X distance in meters
#' @return The shapefiles of buildings, streets, ..
#' @export
get_osmdata <- function(center, y_distance, x_distance) {
  place <- get_border(as.numeric(center[1]),as.numeric(center[2]),y_distance,x_distance)
  coords_bbox <- as.numeric(stringr::str_split(osmdata::opq(bbox = place)$bbox,",")[[1]])
  coords_bbox[3]-coords_bbox[1]
  coords_bbox[4]-coords_bbox[2]
  
  
  my_bbox <- sf::st_bbox(c(xmin=coords_bbox[2],xmax=coords_bbox[4],ymin=coords_bbox[1],ymax=coords_bbox[3]), crs=sf::st_crs(4326))
  
  q <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary", "unclassified", "residential","living_street","footway", "pedestrian")) 
  
  q1 <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_feature("building")
  
  q.water <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_feature("water")
  
  q.sea <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_features(c(
      "\"natural\"=\"water\"",
      "\"natural\"=\"bay\""))
  
  q.green <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_features (features = c (
      "\"landuse\"=\"forest\"",
      "\"landuse\"=\"grass\"",
      "\"landuse\"=\"orchard\"",
      "\"leisure\"=\"park\"",
      "\"natural\"=\"island\"",
      "\"natural\"=\"wood\""
    ))
  
  q.beach <- osmdata::opq(bbox = place) %>%
    osmdata::add_osm_feature("natural", c("beach")) 
  
  q.parking <- osmdata::opq (bbox=place) %>%
    osmdata::add_osm_features (features = c (
      "\"amenity\"=\"parking\"",
      "\"highway\"=\"pedestrian\"",
      "\"man_made\"=\"pier\""
    ))
  
  q.railway <- osmdata::opq (bbox=place) %>%
    osmdata::add_osm_features (features = c (
      "\"railway\"=\"rail\""
    ))
  
  
  cat("Getting data, be patient with requests failing..\n")
  
  osm <- c()
  
  osm$name <- center[3]
  
  cat("Streets..\n")
  osm$x <- q %>% osmdata::osmdata_sf()
  
  cat("Buildings..\n")
  osm$x1 <- q1 %>% osmdata::osmdata_sf()
  osm$x1$osm_polygons <- osm$x1$osm_polygons %>% {if (!is.null(osm$x1$osm_polygons$tunnel)) filter( is.na(tunnel)) else .}
  osm$x1$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_polygons)[1], replace = T)
  osm$x1$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_multipolygons)[1], replace = T)
  #x1$osm_polygons  <- x1$osm_polygons %>% filter(!grepl("Charles de Gaulle", name))
  
  cat("Water..\n")
  osm$x.water <- q.water %>% osmdata::osmdata_sf()
  osm$x.sea <- q.sea %>% osmdata::osmdata_sf()
  
  cat("Green..\n")
  osm$x.green <- q.green %>% osmdata::osmdata_sf() 
  
  cat("Beach, parking..\n")
  osm$x.beach <- q.beach %>% osmdata::osmdata_sf() 
  osm$x.parking <- q.parking %>% osmdata::osmdata_sf()
  osm$x.railway <- q.railway %>% osmdata::osmdata_sf()
  
  osm$my_bbox <- my_bbox
  return(osm)
}


#' Get scaling for a paper format
#'
#' Retrieve correct scaling value based on size A1 = default
#'
#' @param format Paper format
#' @return The scaling.
#' @export
get_scaling <- function(format) {
  if (!format %in% c("A1","A2","A3","A4","A5","small_poster","medium_poster","large_poster") )
    stop("Format not recognized. Try: A1,A2,A3,A4,A5,medium_poster")
  
  scaling <- c()
  
  
  if(format == "A1") scaling = c(1,1)
  if(format == "A2") scaling = c(1/sqrt(2), 1/sqrt(2))
  if(format == "A3") scaling = c(1/2, 1/2)
  if(format == "A4") scaling = c(1/(sqrt(2)*2), 1/(sqrt(2)*2))
  if(format == "A5") scaling = c(1/4, 1/4)
  
  # 11 x 17in
  if(format == "small_poster") scaling = c(1/(841/432),1/(594/279))
  
  # 18 x 24in
  if(format == "medium_poster") scaling = c(1/(841/609.6),1/(594/457.2))
  
  # 24 x 36in
  if(format == "large_poster") scaling = c(1/(841/914.4),1/(594/609.6))
  
  # 11 x 17in
  if(format == "small_poster") scaling = c(1/(841/432),1/(594/279))
  
  return (scaling)
}


#' Save map
#'
#' Save map on disk
#'
#' @param p ggplot object
#' @param filename output filename
#' @param scaling scaling
#' @return NULL
#' @export
save_map <- function(p, outdir, osm_object, scaling, color, format) {
  ggplot2::ggsave(plot=p,filename=paste0(outdir,"/",osm_object$name,"_",format,"_",color$name,".pdf"), 
                  device = cairo_pdf, 
                  width = 594*scaling[2],
                  height = 841*scaling[1], 
                  units = "mm")
}
