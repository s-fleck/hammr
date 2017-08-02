#' Add a margin row (summary) to a data.frame
#'
#' @param dat
#' @param sum_name
#' @param sum_class
#' @param default
#' @param na_rm
#'
#' @return
#' @export
#'
#' @examples
df_add_margin_row <- function(
  .dat,
  ...,
  .sum_class = list(
    numeric = sum,
    integer = sum,
    character = "",
    factor = "",
    POSIXt = sum
  ),
  .default = NA,
  .na_rm = FALSE
){
  dplyr::bind_rows(
    .dat,
    get_margin_row(
      .dat,
      sum_name = list(...),
      sum_class = .sum_class,
      na_rm = .na_rm
    )
  )
}




get_margin_row <- function(
  dat,
  sum_name = list(),
  sum_class = list(
    numeric = sum,
    integer = sum,
    character = "",
    factor = "",
    POSIXt = sum
  ),
  default = NA,
  na_rm = FALSE
){
  # Preconditions
  assert_that(inherits(sum_name), "list")
  assert_that(inherits(sum_class), "list")
  assert_that(all(
    purrr::map_lgl(
      sum_name,
      function(x) is.function(x) || rlang::is_scalar_atomic(x))
  ))

  assert_that(all(
    purrr::map_lgl(
      sum_class,
      function(x) is.function(x) || rlang::is_scalar_atomic(x))
  ))



  res <- vector('list', length(dat))

  for(i in seq_along(dat)){
    col            <- dat[[i]]
    col_name       <- names(dat)[[i]]
    col_cls        <- class(dat[[i]])[[1]]
    if(na_rm)  col <- na.omit(col)


    if (col_name %in% names(sum_name)) {
      fun <- as_summerizer(x = sum_name[[col_name]])

    } else if (col_cls  %in% names(sum_class)) {
      fun <- as_summerizer(x = sum_class[[col_cls]])

    } else {
      fun <- as_summerizer(default)
    }


    res[[i]] <- fun(col)

    if(is.factor(dat[[i]])){
      res[[i]] <- as.factor(res[[i]])
    }
  }


  setNames(res, names(dat))
}




# utils -------------------------------------------------------------------

as_summerizer <- function(x){
  if(is.null(x)){
    stop()
  }

  if(!is.function(x)){
    return(function(...) x)
  }

  x
}
