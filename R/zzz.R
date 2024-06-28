.onAttach <- function(libname, pkgname) {
  sysfonts::font_add("Poppins",
                     regular = system.file(package = "cartographr", "fonts","Poppins","Poppins-Regular.ttf", mustWork = TRUE),
                     bold = system.file(package = "cartographr", "fonts","Poppins","Poppins-Bold.ttf", mustWork = TRUE))
  sysfonts::font_add("Anton",
                     regular = system.file(package = "cartographr", "fonts","Anton","Anton-Regular.ttf", mustWork = TRUE))
  sysfonts::font_add("Cinzel",
                     regular = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-Regular.ttf", mustWork = TRUE),
                     bold = system.file(package = "cartographr", "fonts","Cinzel","Cinzel-SemiBold.ttf", mustWork = TRUE) )
  sysfonts::font_add("Barlow",
                     regular = system.file(package = "cartographr", "fonts","Barlow","Barlow-Regular.ttf", mustWork = TRUE),
                     bold = system.file(package = "cartographr", "fonts","Barlow","Barlow-Medium.ttf", mustWork = TRUE))
  showtext::showtext_auto()
  cli::cli_inform(paste0(crayon::blue("cartographr")," ready."), class = "packageStartupMessage",wrap = FALSE)
}
