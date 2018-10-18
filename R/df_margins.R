#' Add a margin row (summary) to a data.frame
#'
#' If you want to have a specific value for a specific column in the summary
#' row, you can do so by specifying this in the dot paramters (`...`). You can
#' also specify a function that returns a scalar instead. See examples.
#'
#' In the same manner you can also modify the summary functions to be used for
#' column classes. Again, you can also specify scalars instead.
#' The default is `base::sum()` for `numeric` and `integer`, empty strings for
#' `character` and `factor`, and `NA` for `Date` and `POSIXt` (Datetime).
#'
#' The summary row is added to the source `data.frame` via [dplyr::bind_rows()],
#' and therefor the dplyr coercion rules apply.
#' See `vignette("two-table", package = "dplyr")`.
#'
#'
#' @param .dat a `data.frame`
#' @param ... Pairs of column names and scalars, or functions that
#'   return scalars. These values will be substituted into the margin row,
#'   and override the results of the summary functions defined in .sum_class.
#'   See examples.
#' @param .sum_class Pairs of column classes and scalars, or functions that
#'   return scalars. These functions will be applied to each column that matches
#'   the specified class.
#' @param .default A sclar of a function that returns a scalar. Default value
#'   to use if column name or class could not be matched to any names in `...`
#'   or `.sum_class`.
#' @param .na.rm `logical`. Should missing values (including `NaN`) be removed?
#'
#' @return a `data.frame`
#' @export
#'
#' @family data.frame tools
#' @seealso [get_margin_row()]
#'
#' @examples
#'
#' df_add_margin_row(iris, Species = "Average")
#'
#' df_add_margin_row(iris, .sum_class = list(
#'   numeric = mean,
#'   factor = hammr::most_frequent
#' ))
#'
#'
df_add_margin_row <- function(
  .dat,
  ...,
  .sum_class = list(
    numeric = sum,
    integer = sum,
    character = "",
    factor = "",
    POSIXt = NA,
    Date = NA
  ),
  .default = NA,
  .na.rm = FALSE
){
  assert_namespace("dplyr")

  dplyr::bind_rows(
    .dat,
    get_margin_row(
      .dat,
      sum_name = list(...),
      sum_class = .sum_class,
      na.rm = .na.rm
    )
  )
}




#' Get a margin row (summary) of a data.frame
#'
#' This function powers `df_add_margin_row()`. The syntax is the same, except
#' that instead of the dot paramters (`...`) the summary functions for column
#' names are passed in as a `list` like sum_class (which makes it a bit safer
#' to programm with).
#'
#' @param dat a `data.frame`
#' @param sum_name A list of column names and scalars or functions that
#'   return scalars. These values will be substituted into the margin row,
#'   and override the results of the summary functions defined in .sum_class.
#'   See examples.
#' @param sum_class Pairs of column classes and scalars or functions that
#'   return scalars. These functions will be applied to each column that matches
#'   the specified class.
#' @param default A sclar of a function that returns a scalar. Default value
#'   to use if column name or class could not be matched to any names in `...`
#'   or `.sum_class`.
#' @param na.rm `logical`. Should missing values (including `NaN`) be removed?
#'
#' @return A list of length `ncol(dat)`.
#' @seealso [df_add_margin_row()]
#' @export
#'
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
  na.rm = FALSE
){
  # Preconditions
  assert_that(inherits(sum_name, "list"))
  assert_that(inherits(sum_class, "list"))
  assert_that(all(
    vapply(
      sum_name,
      function(x) {is.function(x) || is_scalar_atomic(x)},
      logical(1)
    )
  ))

  assert_that(all(names(sum_name) %in% names(dat)))

  assert_that(all(
    vapply(
      sum_class,
      function(x) {is.function(x) || is_scalar_atomic(x)},
      logical(1)
    )
  ))


  res <- vector('list', length(dat))

  for(i in seq_along(dat)){
    col            <- dat[[i]]
    col_name       <- names(dat)[[i]]
    col_cls        <- class(dat[[i]])[[1]]
    if(na.rm)  col <- stats::na.omit(col)


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

    assert_that(is.scalar(res[[i]]))
  }


  stats::setNames(res, names(dat))
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
