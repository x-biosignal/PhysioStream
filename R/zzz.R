.onUnload <- function(libpath) {
  library.dynam.unload("PhysioStream", libpath)
}
