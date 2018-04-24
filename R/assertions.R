#' Assert that package namesapce is available
#'
#' @param x `character` vector
#'
#' @return `TRUE` or throws an error on fail.
#' @export
#'
#' @examples
#' assert_namespace(c("hammr", "assertthat"))
#'
#' \dontrun{
#'   assert_namespace(c("hammr", "assertthat", "fnarglasfhaklf"))
#' }
#'
assert_namespace <- function(x){
  assert_that(all(
    vapply(x, requireNamespace, FALSE, quietly = TRUE)
  ))
}
