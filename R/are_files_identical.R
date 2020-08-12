are_files_identical <- function(x, y){
  fx <- readBin(file(x, "rb"), "raw", n = file.size(x))
  fy <- readBin(file(y, "rb"), "raw", n = file.size(y))
  identical(fx, fy)
  identical(fx, fx)
}
