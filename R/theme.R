#' Create a poster-themed theme
#'
#' This function generates a ggplot2 theme that resembles a poster style. It is designed to be used with ggplot2 plots to provide a clean and bold aesthetic suitable for poster visuals.
#'
#' @param font The font family to be used for text elements in the plot.
#'             The default font is set to "Poppins".
#' @return A ggplot2 theme object that can be added to ggplot2 plotting calls.
#' @export
theme_poster <- function(font = "Poppins") {
  #if (!(font %in% c("Poppins","Anton","Cinzel","Barlow"))) {
  #  stop(cli::cli_abort("Font not found."))
  #}

  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
           ggplot2::theme(
             axis.title.x = ggplot2::element_text(hjust=1, size=scale_factor*5*2.845276, color="#292e28", family = font),
             panel.border = ggplot2::element_rect(colour = NA, fill=NA),
             panel.background = ggplot2::element_rect(fill=NA, color=NA),
             plot.title = ggplot2::element_text(size=scale_factor*40*2.845276,family = font ,face = "bold",hjust = 1,colour = "#292e28",margin=ggplot2::margin(0,0,30*scale_factor,0)),
             plot.caption = ggplot2::element_text(size=scale_factor*8*2.845276,family = font ,face = "bold",hjust = 0.5,vjust=1,colour = "#cccccc"),
             plot.subtitle = ggplot2::element_text(size=scale_factor*10*2.845276,family = font ,face = "bold",hjust = 1,colour = "#292e28"),
             plot.margin = ggplot2::margin(t = 100*scale_factor, r = 80*scale_factor, b = 80*scale_factor, l = 80*scale_factor, unit = "mm")
           ))
}

#' Apply poster Theme with Poppins Font
#'
#' This function applies the poster theme using the 'Poppins' font.
#'
#' @return A ggplot2 theme object with the poster theme settings applied and 'Poppins' font set.
#'
#' @examples
#' \dontrun{
#' # Apply the poster theme with Poppins font to a ggplot object
#' ggplot_object + theme_poster_poppins()
#' }
#' @export
theme_poster_poppins <- function() {
  return(theme_poster(font = "Poppins"))
}

#' Apply poster Theme with Anton Font
#'
#' This function applies the poster theme using the 'Anton' font.
#'
#' @return A ggplot2 theme object with the poster theme settings applied and 'Anton' font set.
#'
#' @examples
#' \dontrun{
#' # Apply the poster theme with Anton font to a ggplot object
#' ggplot_object + theme_poster_anton()
#' }
#' @export
theme_poster_anton <- function() {
  return(theme_poster(font = "Anton"))
}

#' Apply Info Map Theme with Specified Font
#'
#' This function applies a custom theme for information maps, allowing the user to specify a font from a predefined list. It sets various ggplot2 theme elements such as the title, subtitle, caption, and legend to use the specified font and adjusts their appearance based on a scale factor.
#'
#' @param font A character string specifying the font to use for the theme elements. The default is "Poppins". Only "Poppins", "Anton", "Cinzel", and "Barlow" are valid options.
#'
#' @return A ggplot2 theme object with the information map theme settings applied.
#'
#' @examples
#' \dontrun{
#' # Apply the info map theme with the default Poppins font to a ggplot object
#' ggplot_object + theme_infomap()
#'
#' # Apply the info map theme with the Anton font
#' ggplot_object + theme_infomap(font = "Anton")
#' }
#'
#' @export
theme_infomap <- function(font = "Poppins") {
  #if (!(font %in% c("Poppins","Anton","Cinzel","Barlow"))) {
  #  stop(cli::cli_abort("Font not found."))
  #}

  scale_factor <- cartographr_env$scale_factor

  return(ggplot2::theme_void()+
           ggplot2::theme(
             panel.border = ggplot2::element_rect(colour = NA, fill=NA),
             panel.background = ggplot2::element_rect(fill=NA, color=NA),
             plot.subtitle = ggplot2::element_text(size=scale_factor*20*2.845276,family = font,face = "bold",hjust = -1,colour = "#292e28"),
             plot.title = ggplot2::element_text(size = scale_factor*40*2.845276,hjust = 0,face = "bold",family=font,colour = "#292e28"),
             plot.margin = ggplot2::margin(t = 40*scale_factor, r = 10*scale_factor, b = 40*scale_factor, l = 10*scale_factor, unit = "mm"),
             plot.caption = ggplot2::element_text(face="plain",size=scale_factor*8*2.845276, hjust = 1,family=font,color="#292e28"),
             legend.position = "top",
             legend.title = ggplot2::element_text(size = 10*scale_factor*2.845276, family=font,face = "bold", color = "#292e28"),
             legend.text = ggplot2::element_text(size = 10*scale_factor*2.845276, family = font, color= "#292e28"),
             legend.justification = c("right","top"),
             legend.margin = ggplot2::margin(-18*scale_factor, 0, 0, 0, unit = "mm"),
             legend.key.spacing = grid::unit(0,"mm")
           ))
}

#' Apply infomap Theme with Anton Font
#'
#' This function applies the infomap theme using the 'Anton' font.
#'
#' @return A ggplot2 theme object with the infomap theme settings applied and 'Anton' font set.
#'
#' @examples
#' \dontrun{
#' # Apply the infomap theme with Anton font to a ggplot object
#' ggplot_object + theme_poster_anton()
#' }
#' @export
theme_infomap_anton <- function() {
  return(theme_infomap(font = "Anton"))
}

#' Apply infomap Theme with Poppins Font
#'
#' This function applies the infomap theme using the 'Poppins' font.
#'
#' @return A ggplot2 theme object with the infomap theme settings applied and 'Poppins' font set.
#'
#' @examples
#' \dontrun{
#' # Apply the infomap theme with Poppins font to a ggplot object
#' ggplot_object + theme_poster_poppins()
#' }
#' @export
theme_infomap_poppins <- function() {
  return(theme_infomap(font = "Poppins"))
}

#' Apply infomap Theme with Barlow Font
#'
#' This function applies the infomap theme using the 'Barlow' font.
#'
#' @return A ggplot2 theme object with the infomap theme settings applied and 'Barlow' font set.
#'
#' @examples
#' \dontrun{
#' # Apply the infomap theme with Barlow font to a ggplot object
#' ggplot_object + theme_poster_barlow()
#' }
#' @export
theme_infomap_barlow <- function() {
  return(theme_infomap(font = "Barlow"))
}
