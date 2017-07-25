#' Test for knitr output format
#'
#' @return a logical scalar.
#' @export
#'
#' @seealso
#'   \url{https://stackoverflow.com/questions/35144130/in-knitr-how-can-i-test-for-if-the-output-will-be-pdf-or-word}
#'   \url{https://stackoverflow.com/questions/30374492/evaluate-a-chunk-based-on-the-output-format-of-knitr}
#'
#' @examples
#' \dontrun{
#'
#' # Example Rmarkdown code:
#'
#' ```{r eval = is_pdf_output()}
#'   "Hi, I'm a PDF!"
#' ```
#'
#' #' ```{r eval = is_html_output()}
#'   "Hi, I'm a HTML!"
#' ```
#'
#' }
is_html_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="html"
}



#' @rdname is_html_output
is_pdf_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="latex"
}



#' @rdname is_html_output
is_docx_output = function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to") =="docx"
}
