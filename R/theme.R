#' Create a poster-themed theme
#'
#' This function generates a ggplot2 theme that resembles a poster style. It is designed to be used with ggplot2 plots to provide a clean and bold aesthetic suitable for poster visuals.
#'
#' @param font The font family to be used for text elements in the plot.
#'             The default font is set to "Poppins".
#' @return A ggplot2 theme object that can be added to ggplot2 plotting calls.
#' @export
theme_postcard <- function(font = "Poppins") {

  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
           ggplot2::theme(
             axis.title.x = ggplot2::element_text(hjust=1, size=scale_factor*5*.pt, color="#292e28", family = font),
             panel.border = ggplot2::element_rect(colour = NA, fill=NA),
             panel.background = ggplot2::element_rect(fill=NA, color=NA),
             plot.title = ggplot2::element_text(size=scale_factor*40*.pt,family = font ,face = "bold",hjust = 1,colour = "#292e28",margin=ggplot2::margin(0,0,30*scale_factor,0)),
             plot.caption = ggplot2::element_text(size=scale_factor*8*.pt,family = font ,face = "bold",hjust = 0.5,vjust=1,colour = "#cccccc"),
             plot.subtitle = ggplot2::element_text(size=scale_factor*10*.pt,family = font ,face = "bold",hjust = 1,colour = "#292e28"),
             plot.margin = ggplot2::margin(t = 100*scale_factor, r = 80*scale_factor, b = 80*scale_factor, l = 80*scale_factor, unit = "mm")
           ))
}

#' Postcard theme with font Poppins
#'
#' Short
#'
#' @return The theme
#' @export
theme_postcard_poppins <- function() {
  return(theme_postcard(font = "Poppins"))
}

#' Postcard theme
#'
#' Description of the function
#'
#' @return The theme
#' @export
theme_postcard_anton <- function() {
  return(theme_postcard(font = "Anton"))
}


theme_infomap <- function(font = "Poppins") {
  if (!(font %in% c("Poppins","Anton","Cinzel","Barlow"))) {
    stop(cli::cli_abort("Font not found."))
  }

  #scale_factor <- mean(scaling[1],scaling[2])
  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
           ggplot2::theme(
             panel.border = ggplot2::element_rect(colour = NA, fill=NA),
             panel.background = ggplot2::element_rect(fill=NA, color=NA),
             plot.subtitle = ggplot2::element_text(size=scale_factor*10*.pt,family = font,face = "bold",hjust = 1,colour = "#292e28"),
             plot.title = ggplot2::element_text(size = scale_factor*40*.pt,hjust = 0,face = "bold",family=font,colour = "#292e28"),
             plot.margin = ggplot2::margin(t = 40*scale_factor, r = 10*scale_factor, b = 40*scale_factor, l = 10*scale_factor, unit = "mm"),
             plot.caption = ggplot2::element_text(face="plain",size=scale_factor*8*.pt, hjust = 1,family=font,color="#292e28"),
             legend.position = "top",
             legend.title = ggplot2::element_text(size = 10*scale_factor*.pt, family=font,face = "bold", color = "#292e28"),
             legend.text = ggplot2::element_text(size = 10*scale_factor*.pt, family = font, color= "#292e28"),
             legend.justification = c("right","top"),
             legend.margin = ggplot2::margin(-18*scale_factor, 0, 0, 0, unit = "mm"),
             legend.key.spacing = unit(0,"mm")
           ))
}

#' Infomap theme with font Anton
#'
#' Description of the function
#'
#' @param scale_factor The scale factor for output
#' @return The theme
#' @export
theme_infomap_anton <- function() {
  return(theme_infomap(font = "Anton"))
}

#' Infomap theme with font Anton
#'
#' Description of the function
#'
#' @param scale_factor The scale factor for output
#' @return The theme
#' @export
theme_infomap_poppins <- function() {
  return(theme_infomap(font = "Poppins"))
}

#' Infomap theme with font Barlow
#'
#' Description of the function
#'
#' @return The theme
#' @export
theme_infomap_barlow<- function() {
  return(theme_infomap(font = "Barlow"))
}
