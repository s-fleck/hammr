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
  na_rm = FALSE,
  ...
){
  dplyr::bind_rows(dat, get_margin_row(dat, na_rm = na_rm))
}





get_margin_row <- function(
  dat,
  numeric = sum,
  integer = sum,
  character = "",
  factor = "",
  POSIXt = sum,
  default = NA,
  na_rm = FALSE,
  ...
){
  funs <- list(
    numeric = numeric,
    integer = integer,
    character = character,
    factor = factor,
    POSIXt = sum
  )

  res <- vector('list', length(dat))

  for(i in seq_along(dat)){
    cls <- class(dat[[i]])[[1]]

    fun <- funs[[cls]]
    col <- dat[[i]]

    if(na_rm){
      col <- na.omit(col)
    }

    if(is.null(fun)){
      fun <- default
    }

    if (is.function(fun)) {
      res[[i]] <- fun(col)
    } else if (is.scalar(fun)) {
      res[[i]] <- fun
    }

    if(is.factor(dat[[i]])){
      res[[i]] <- as.factor(res[[i]])
    }

  }

  setNames(res, names(dat))
}
