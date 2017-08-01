#' Add margin row
#'
#' @param dat
#' @param numeric
#' @param integer
#' @param character
#' @param factor
#' @param POSIXt
#' @param default
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
df_add_margin_row <- function(
  dat,
  numeric = sum,
  integer = sum,
  character = "",
  factor = "",
  POSIXt = sum,
  default = NA,
  ...
){
  rbind(dat, get_margin_row(dat))
}





get_margin_row <- function(
  dat,
  numeric = sum,
  integer = sum,
  character = "",
  factor = "",
  POSIXt = sum,
  default = NA,
  ...
){
  funs <- c(
    list(
      numeric = numeric,
      integer = integer,
      character = character,
      factor = factor,
      POSIXt = sum
    ),
    list(...)
  )

  res <- vector('list', length(dat))

  for(i in seq_along(dat)){
    cls <- class(dat[[i]])[[1]]

    fun <- funs[[cls]]

    if(is.null(fun)){
      fun <- default
    }

    if (is.function(fun)) {
      res[[i]] <- fun(dat[[i]])
    } else if (is.scalar(fun)) {
      res[[i]] <- fun
    }
  }

  as.data.frame(setNames(res, names(dat)))
}
