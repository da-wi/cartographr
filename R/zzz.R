.onLoad <- function(libname, pkgname) {
  # to show a startup message
  packageStartupMessage("Loading Google Fonts")
  sysfonts::font_add("Poppins",
                     regular = system.file(package = "mypostcard", "fonts","Poppins","Poppins-Bold.ttf", mustWork = T))
                     #regular = system.file("fonts", "Poppins", "Poppins-Regular.ttf", package = "mypostcard"))
                     #bold = paste0(system.file(package="mypostcard"),"/fonts/Poppins/Poppins-Bold.ttf"))
  showtext::showtext_auto() 
}