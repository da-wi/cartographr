.onAttach <- function(libname, pkgname) {
  # to show a startup message
  packageStartupMessage("Loading fonts...")
  sysfonts::font_add("Poppins",
                     regular = system.file(package = "cartographr", "fonts","Poppins","Poppins-Regular.ttf", mustWork = T),
                     bold = system.file(package = "cartographr", "fonts","Poppins","Poppins-Bold.ttf", mustWork = T))
  sysfonts::font_add("Anton",
                     regular = system.file(package = "cartographr", "fonts","Anton","Anton-Regular.ttf", mustWork = T))
  sysfonts::font_add("Cinzel",
                     regular = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-Regular.ttf", mustWork = T),
                     bold = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-Regular.ttf", mustWork = T) )
  showtext::showtext_auto()
  packageStartupMessage("Package ready.")

}
