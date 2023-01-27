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
#' @param circle If TRUE draw circle plot
#' @return NULL
#' @export
plot_map = function(osm, color, scaling, circle) {
  
  # copy osm_object
  osm_object = osm
  
  use_bcircle <- circle
  
  # scaling factor of output format for adjusting outer margins
  scale_factor <- mean(scaling[1],scaling[2])
  
  # my_bcircle as background
  my_bcircle <- c()
  
  
  options(warn=-1)
  sf::sf_use_s2(F)
  # water
  osm_object$x.water$osm_lines1 <- osm_object$x.water$osm_lines
  osm_object$x.water$osm_polygons1 <- osm_object$x.water$osm_polygons
  osm_object$x.water$osm_multipolygons1 <- osm_object$x.water$osm_multipolygons
  osm_object$x.sea$osm_polygons1 <- osm_object$x.sea$osm_polygons
  osm_object$x.sea$osm_multipolygons1 <- osm_object$x.sea$osm_multipolygons
  
  osm_object$water <- dplyr::bind_rows(list(osm_object$x.water$osm_lines1 %>% sf::st_make_valid(),
                                            osm_object$x.water$osm_polygons1  %>% sf::st_make_valid(),
                                     osm_object$x.water$osm_multipolygons1 %>% sf::st_make_valid(),
                                     osm_object$x.sea$osm_multipolygons1 %>% sf::st_make_valid(),
                                     osm_object$x.sea$osm_polygons1 %>% sf::st_make_valid()))
  osm_object$water.dis  <- sf::st_union(osm_object$water)
  
  # buidlings

  osm_object$buildings <- list( osm_object$x1$osm_polygons, osm_object$x1$osm_multipolygons)
  osm_object$buildings <- osm_object$buildings[!sapply(osm_object$buildings,is.null)]
  osm_object$buildings.dis <- NULL
  if (length(osm_object$buildings) > 1) {
    osm_object$buildings.dis <- dplyr::bind_rows(osm_object$buildings)
  } else {
    osm_object$buildings.dis <- osm_object$buildings[[1]]
  }

  #if(!is.null(osm_object$x1$osm_polygons)) osm_object$x1$osm_polygons <-  osm_object$x1$osm_polygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  
  #if(!is.null(osm_object$x1$osm_multipolygons)) osm_object$x1$osm_multipolygons <- osm_object$x1$osm_multipolygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  
  
  
  # circle 
  if (use_bcircle) {
    
    my_bcircle <- get_circle(osm_object$center,osm_object$y_distance,osm_object$x_distance)
    
    # decrease margin for circle
    scaling <- scaling/sqrt(2)
    
      osm_object$my_bbox <- sf::st_bbox(my_bcircle)
      osm_object$my_bbox[c(1,2)] <- osm_object$my_bbox[c(1,2)]-(osm_object$my_bbox[c(3,4)]-osm_object$my_bbox[c(1,2)])*0.02
      osm_object$my_bbox[c(3,4)] <- osm_object$my_bbox[c(3,4)]+(osm_object$my_bbox[c(3,4)]-osm_object$my_bbox[c(1,2)])*0.02

      # streets
      if(!is.null(osm_object$x$osm_lines)) osm_object$x$osm_lines <- osm_object$x$osm_lines %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  
     
      # buildings
      if(!is.null(osm_object$buildings.dis)) osm_object$buildings.dis <- osm_object$buildings.dis %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )
      
      # for some magical reason, one needs to create a copy for water polygons to make it work. No idea why. # TODO
      #if(!is.null(osm_object$x.water$osm_lines)) osm_object$x.water$osm_lines1 <- osm_object$x.water$osm_lines %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  %>%  sf::st_make_valid() 
      #if(!is.null(osm_object$x.water$osm_multipolygons)) osm_object$x.water$osm_multipolygons1 <- osm_object$x.water$osm_multipolygons %>% sf::st_intersection(., my_bcircle ) 
      #if(!is.null(osm_object$x.water$osm_polygons)) osm_object$x.water$osm_polygon1 <-  osm_object$x.water$osm_polygons %>% sf::st_intersection(., my_bcircle ) 
      
      #if(!is.null(osm_object$x.sea$osm_multipolygons)) osm_object$x.sea$osm_multipolygons1 <-  osm_object$x.sea$osm_multipolygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  
      #if(!is.null(osm_object$x.sea$osm_polygons)) osm_object$x.sea$osm_polygons1 <-  osm_object$x.sea$osm_polygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle ) 
      
      if(!is.null(osm_object$water.dis)) osm_object$water.dis <- osm_object$water.dis %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  %>%  sf::st_make_valid() 
      
      if(!is.null(osm_object$x.green$osm_multipolygons)) osm_object$x.green$osm_multipolygons <-  osm_object$x.green$osm_multipolygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )  
      if(!is.null(osm_object$x.green$osm_polygons)) osm_object$x.green$osm_polygons <-  osm_object$x.green$osm_polygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle )
      
      if(!is.null(osm_object$x.beach$osm_multipolygons)) osm_object$x.beach$osm_multipolygons <-  osm_object$x.beach$osm_multipolygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle ) 
      if(!is.null(osm_object$x.parking$osm_multipolygons)) osm_object$x.parking$osm_multipolygons <-  osm_object$x.parking$osm_multipolygons %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle ) 
      
      if(!is.null(osm_object$x.railway$osm_lines))  osm_object$x.railway$osm_lines <- osm_object$x.railway$osm_lines %>%  sf::st_make_valid() %>% sf::st_intersection(., my_bcircle ) 
    
  }
  
  # patterns for hatching
  df.point1 <- data.frame( x = seq(osm_object$my_bbox$xmin,osm_object$my_bbox$xmax,(osm_object$my_bbox$xmax-osm_object$my_bbox$xmin)/300),id = 1)
  df.point2 <-  data.frame( y = seq(osm_object$my_bbox$ymin,osm_object$my_bbox$ymax,(osm_object$my_bbox$ymax-osm_object$my_bbox$ymin)/300), id = 1)                       
  df.point <- dplyr::full_join(df.point1, df.point2, by = "id") %>%
    dplyr::select(-id)
  df.point <- df.point %>% sf::st_as_sf(coords = c(1,2))
  sf::st_crs(df.point) <- 4326
  
  #df.point[sf::st_intersects(df.point,osm_object$x.water$osm_multipolygons1) %>% lengths > 0,] %>% 
  df.point.gg <- df.point[sf::st_intersects(df.point,osm_object$water.dis) %>% lengths > 0,] 
  
  # create a list of ggobjects for water
  gg.water <- list(ggplot2::geom_sf(data =osm_object$x.water$osm_lines1, fill = color$water, color= color$water, size = 1),
                   ggplot2::geom_sf(data =osm_object$x.water$osm_multipolygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.water$osm_polygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.sea$osm_polygons1, fill = color$water, color =color$water),
                   ggplot2::geom_sf(data =osm_object$x.sea$osm_multipolygons1, fill = color$water, color =color$water))
  
  p <- ggplot2::ggplot() +
    # add background
    {if(use_bcircle) ggplot2::geom_sf(data=my_bcircle, fill=color$background,color=NA) else ggplot2::geom_sf(data=sf::st_as_sfc(osm_object$my_bbox), fill=color$background,color=NA)} +
    
    
    ### add layers on top
    # water
    # BEWARE: when using {if} the original osm_object is used (important, when cropped!)
    # a list containing NULL will simply ignore NULL entries
    ggplot2::geom_sf(data =osm_object$water.dis, fill = color$water, color= color$water) +

    # water hatched
    {if(!is.null(osm_object$x.dis) && color$hatched == TRUE)  ggplot2::geom_sf( data=df.point.gg, shape=18,fill="black", size = 0.3, alpha=0.1 )}+
    #{if(!is.null(osm_object$x.water$osm_multipolygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.water$osm_multipolygons1) %>% lengths > 0,], shape=18,fill="black", size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.water$osm_polygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.water$osm_polygons1) %>% lengths > 0,], shape=18, fill="black",size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.sea$osm_multipolygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.sea$osm_multipolygons1) %>% lengths > 0,], shape=18,fill="black", size = 0.3, alpha=0.1 )} +
    #{if(!is.null(osm_object$x.sea$osm_polygons1) && color$hatched == TRUE) ggplot2::geom_sf( data=df.point[sf::st_intersects(df.point,osm_object$x.sea$osm_polygons1) %>% lengths > 0,], shape=18, fill="black",size = 0.3, alpha=0.1 )} +
    
    # green, beach & parking
    ggplot2::geom_sf(data =osm_object$x.green$osm_multipolygons, fill = color$green, color=NA , linewidth=0.05) + # "#2F3737"
    ggplot2::geom_sf(data =osm_object$x.green$osm_polygons, fill = color$green, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.beach$osm_multipolygons, fill = color$beach, color= NA, linewidth=0.05) +
    ggplot2::geom_sf(data =osm_object$x.parking$osm_multipolygons, fill = color$parking, color= NA, linewidth=0.05) +
  
    # railway
    {if(!is.null(osm_object$x.railway$osm_lines))
      ggplot2::geom_sf(data =osm_object$x.railway$osm_lines %>% dplyr::filter(if (!is.null(osm_object$x.railway$osm_lines$tunnel)) is.na(osm_object$x.railway$osm_lines$tunnel) else rep(TRUE,dim(osm_object$x.railway$osm_lines)[1])),
                       linetype = "11",
                       color=color$railway, linewidth = 0.5)} +
    
    # streets
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "motorway") , color=color$street, linewidth=6*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "primary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "secondary") , color=color$street, linewidth=4*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "tertiary") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "unclassified") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "residential") , color=color$street, linewidth=3*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "pedestrian") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "service") , color=color$street, linewidth=1*scale_factor) +
    ggplot2::geom_sf(data =osm_object$x$osm_lines %>% dplyr::filter(highway == "living_street") , color=color$street, linewidth=1*scale_factor) +

    # buildings
    #ggplot2::geom_sf(data =osm_object$x1$osm_multipolygons, ggplot2::aes(fill = colors),show.legend = F, color= NA, linewidth = 0*scale_factor)+
    #ggplot2::geom_sf(data =osm_object$x1$osm_polygons, ggplot2::aes(fill = colors), show.legend = F,color= NA, linewidth =0*scale_factor)+
    ggplot2::geom_sf(data = osm_object$buildings.dis, ggplot2::aes(fill = osm_object$buildings.dis$colors), show.legend = F,color= NA, linewidth =0*scale_factor)+
    ggplot2::scale_fill_manual(values=color$palette_building)+

    # remove axes
    ggplot2::theme_void()+
    
    # theme text and background
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(hjust=1, size=scale_factor*5*2.845276, color="#292e28", family = color$font),
      panel.border = ggplot2::element_rect(colour = NA, fill=NA),#color$street 
      panel.background = ggplot2::element_rect(fill=NA, color=NA),
      plot.title = ggplot2::element_text(size=scale_factor*40*2.845276,family = color$font ,face = "bold",hjust = 1,colour = "#292e28"),
      plot.caption = ggplot2::element_text(size=scale_factor*30*2.845276,family = color$font ,face = "bold",hjust = 0.5,vjust=1,colour = "#292e28"),
      plot.subtitle = ggplot2::element_text(size=scale_factor*10*2.845276,family = color$font ,face = "bold",hjust = 1,colour = "#292e28"),
      plot.margin = ggplot2::margin(t = 100*scaling[1], r = 80*scaling[2], b = 80*scaling[1], l = 80*scaling[2], unit = "mm")
      #plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "mm")
      )+
    
    # add text
    {if(use_bcircle) ggplot2::labs( caption = stringr::str_to_lower(paste0("\n\n",center[3]))) else ggplot2::labs( title=stringr::str_to_lower(center[3])) } +
    # ggplot2::labs(x = stringr::str_to_lower("artmaps::"))+
    
    # set coordinates 
    ggplot2::coord_sf(xlim = c(osm_object$my_bbox[1]+(osm_object$my_bbox[3]-osm_object$my_bbox[1])/20,
                               osm_object$my_bbox[3]-(osm_object$my_bbox[3]-osm_object$my_bbox[1])/20),
                      ylim = c(osm_object$my_bbox[2]+(osm_object$my_bbox[4]-osm_object$my_bbox[2])/20, 
                               osm_object$my_bbox[4]-(osm_object$my_bbox[4]-osm_object$my_bbox[2])/20))
  
    options(warn=0)
    return(p)
  
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
  
  cat("Creating street network..\n")
  osm$x <- q %>% osmdata::osmdata_sf() 
  osm$x$osm_lines <- osm$x$osm_lines %>% 
    dplyr::mutate(length = as.numeric(sf::st_length(.))) %>%
    dplyr::filter(length >= quantile(length,0.25)) 
  
  cat("Construct buildings..\n")
  osm$x1 <- q1 %>% osmdata::osmdata_sf()
  osm$x1$osm_polygons <- osm$x1$osm_polygons %>%
    {if (!is.null(osm$x1$osm_polygons$tunnel)) dplyr::filter( osm$x1$osm_polygons, is.na(tunnel) == TRUE) else .}
  
  if(!is.null(osm$x1$osm_polygons)) osm$x1$osm_polygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_polygons)[1], replace = T)
  if(!is.null(osm$x1$osm_multipolygons)) osm$x1$osm_multipolygons$colors <- sample(as.factor(c(1,2,3)) ,dim(osm$x1$osm_multipolygons)[1], replace = T)
  
  cat("Fill in water..\n")
  osm$x.water <- q.water %>% osmdata::osmdata_sf()
  osm$x.sea <- q.sea %>% osmdata::osmdata_sf()
  
  cat("Plant trees..\n")
  osm$x.green <- q.green %>% osmdata::osmdata_sf() 
  
  cat("Beach, parking..\n")
  osm$x.beach <- q.beach %>% osmdata::osmdata_sf() 
  osm$x.parking <- q.parking %>% osmdata::osmdata_sf()
  osm$x.railway <- q.railway %>% osmdata::osmdata_sf()

  osm$my_bbox <- my_bbox
  osm$y_distance <- y_distance
  osm$x_distance <- x_distance
  osm$center <- center
  return(osm)
}

#' Generate bounding circle
#'
#' This function generates a bounding circle
#'
#' @param center Center coordinate
#' @param y_distance Y distance in meters
#' @param x_distance X distance in meters
#' @return The circle
#' @export
get_circle <- function(center,y_distance,x_distance) {
  return(tibble::tibble(lat = as.numeric(center[1]),  long = as.numeric(center[2])) %>% 
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    sf::st_transform(crs=7801) %>%
    sf::st_buffer(dist = min(y_distance,x_distance)) %>% 
    sf::st_transform(crs = 4326))
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
#' @param outdir output_filename
#' @param osm_object osm_object
#' @param scaling scaling
#' @param color color
#' @param format format
#' @return NULL
#' @export
save_map <- function(p, outdir, osm_object, scaling, color, format) {
  ggplot2::ggsave(plot=p,filename=paste0(outdir,"/",osm_object$name,"_",format,"_",color$name,".pdf"), 
                  device = grDevices::cairo_pdf, 
                  width = 594*scaling[2],
                  height = 841*scaling[1], 
                  units = "mm")
}




#' Create color theme
#'
#' This function creates a theme
#'
#' @param palette The color palette
#' @return A color palette for plot_map()
#' @export
get_theme = function(palette, font) {
  color <- c()
  
  color$font <- font
  
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
    color$hatched <- TRUE
    
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
    color$hatched <- TRUE
    
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- FALSE
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
    color$hatched <- TRUE
  }
  
  if (palette == "dreamy") {
    color$palette_building = c("#cdb4db","#ffc8dd", "#ffc8dd")
    color$railway <- "#ffafcc"
    color$green <- "#ffafcc"
    color$water <- "#a2d2ff" 
    color$background <- "#bde0fe"
    color$street <- "#bde0fe"
    color$beach <- "#bde0fe"
    color$parking <- "#bde0fe"
    color$hatched <- FALSE
  }
  
  return(color)
}


