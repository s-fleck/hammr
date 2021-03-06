#' Embed plot in html document
#'
#' Generates a png file from a plot and encodes it as a base64 string
#' encode it as a base64 string, and wraps that string in an html `<img>` tag.
#'
#' @param x a function that plots something or a ggplot object
#' @param img Logical. If `TRUE` result will be wrapped in an img tag
#' @param ... passed on to [shiny::plotPNG()]
#'
#' @return if img is `TRUE` a shiny.tag object, else a character scalar.
#' @export
#' @md
#' @importFrom graphics plot
#'
#' @examples
#'
#' \dontrun{
#'
#' embed_plot(function() plot(cars), width = 200, height = 200)
#'
#'
#' p <- ggplot(
#'   cars,
#'   aes(x = speed, y = dist)
#' ) +
#'   geom_bar(stat = "identity")
#'
#' embed_plot(p, width = 200, height = 200)
#' }
#'
embed_plot <- function(x, img, ...){
  assert_namespace("knitr")
  assert_namespace("htmltools")
  UseMethod("embed_plot")
}



#' @export
embed_plot.function <- function(x, img = TRUE, ...){
  assert_namespace("shiny")
  tf <- paste0(tempfile(), ".png")
  shiny::plotPNG(filename = tf, func = x, ...)
  res <- knitr::image_uri(tf)

  if(isTRUE(img)){
    return(htmltools::tags$img(src = res))
  } else {
    return(res)
  }
}



#' @export
embed_plot.ggplot <- function(x, img = TRUE, ...){
  assert_namespace("ggplot2")
  tf <- paste0(tempfile(), ".png")

  func = function() plot(x)

  embed_plot(func, img = img, ...)
}
