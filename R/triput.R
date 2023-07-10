#' Like `dput` for Those Confined to the Tidyverse
#'
#' Creates a `dput`-like pasteable format that can be used to create small tables.
#'
#' @param indf The input `data.frame`.
#' @param indents The number of spaces to indent each line of output. Defaults
#' to `4`.

#' @author Ananda Mahto. Name courtesy of [Frank](https://stackoverflow.com/users/1191259/frank).
#' @references <http://stackoverflow.com/q/42839626/1270695>
#' @examples
#'
#' \dontrun{
#' short_iris <- head(iris)
#' mc_tribble(short_iris)
#' }
#'
#' @export
triput <- function(x, indent = 2, pad = TRUE) {
  file = ""
  stopifnot(is.data.frame(x))
  cols <- lapply(x, function(col) {
    switch(
      class(col),
      factor =,
      character = paste0("\"", col, "\""),
      logical =,
      numeric =,
      integer = col,
      Date = paste0('as.Date("', col, '")'),
      POSIXct = paste0("as.POSIXct", col, ")"),
      list = unlist(lapply(col, dput_to_var)),
      AsIs = unlist(lapply(col, dput_to_var))
    )
  })

  header <- sprintf("~%s", names(x))
  body   <- as.matrix(as.data.frame(cols))

  table <- rbind(t(header), body)


  if (pad){
    for (i in seq_len(ncol(table))){
      table[, i] <- pad_left(table[, i])
    }
  }

  table[, 1] <- paste(paste(rep(" ", indent), collapse = ""), table[, 1])

  table_formatted <-
    paste(apply(table, 1, paste, collapse = ", "), collapse = ",\n")

  res <- paste0("tribble(\n", table_formatted, ")")

  if (is.character(file)) {
    if (nzchar(file)) {
      file <- file(file, "wt")
      on.exit(close(file))
    } else {
      file <- stdout()
    }
  }

  writeLines(res, file)
  invisible(x)
}



dput_to_var <- function(x) {
  con <- textConnection("out", "w", local = TRUE)
  on.exit(close(con))
  dput(x, con)
  paste(out, collapse = "")
}
#
# triput <- function(x, indent = 4) {
#   name <- as.character(substitute(x))
#   name <- name[length(name)]
#
#   col_names <- paste0("~", names(x), collapse = ", ")
#
#
#   browser()
#
#   deparseInternal <- function(x){
#     if (is.factor(x)){
#       x <- as.character(x)
#     }
#     deparse(x)
#   }
#
#   values <- lapply(x, deparseInternal)
#
#   res <- paste0(
#     paste(rep(" ", indent), collapse = ""),
#     values)
#
#   header <- paste("~", names(x))
#
#   obj <- paste(name, " <- tribble(\n", paste(res, collapse = ",\n"), ")", sep = "")
#
#   cat(obj)
#   invisible(obj)
# }
