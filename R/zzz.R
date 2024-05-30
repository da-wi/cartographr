.onAttach <- function(libname, pkgname) {
  sysfonts::font_add("Poppins",
                     regular = system.file(package = "cartographr", "fonts","Poppins","Poppins-Regular.ttf", mustWork = T),
                     bold = system.file(package = "cartographr", "fonts","Poppins","Poppins-Bold.ttf", mustWork = T))
  sysfonts::font_add("Anton",
                     regular = system.file(package = "cartographr", "fonts","Anton","Anton-Regular.ttf", mustWork = T))
  sysfonts::font_add("Cinzel",
                     regular = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-Regular.ttf", mustWork = T),
                     bold = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-Regular.ttf", mustWork = T) )
  sysfonts::font_add("Barlow",
                     regular = system.file(package = "cartographr", "fonts","Barlow","Barlow-Regular.ttf", mustWork = T),
                     bold = system.file(package = "cartographr", "fonts","Barlow","Barlow-Medium.ttf", mustWork = T))
  showtext::showtext_auto()
  cli::cli_alert_success(paste0(crayon::blue("cartographr")," ready."), class = "packageStartupMessage",wrap = F)
}
