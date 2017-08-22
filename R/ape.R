#' Percentage Error Metrics
#'
#' Simple and useful prediction performance metrics:
#' * PE: Percentage Error
#' * MPE: Mean Percentage Error
#' * APE: Absolute Percentage Error
#' * MAPE: Mean Absolute Percentage Error
#'
#' @param true Numeric. True baseline values
#' @param pred Numeric. Predicted values to assess
#' @param na.rm Logical. Should `NA`s be removed prior to computation?
#' @param w An integer vector vector of the same length as `true` and
#'   `pred`. If `NULL` no weighting takes place.
#'
#' @return
#'
#'    `pe()` returns a numeric vector
#'
#'   `mpe()` and `weighted_mpe()` return a numeric scalar
#'
#'   `ape()` returns a postive numeric vector
#'
#'   `mape()` and `weighted_mape()` return a postive numeric scalar
#'
#'
#' @export
#'
weighted_mape <- function(
  true,
  pred,
  w = NULL,
  na.rm = TRUE
){
  # Preconditions
  assert_that(is.numeric(true))
  assert_that(is.numeric(pred))
  assert_that(is.null(w) || isit::is_equal_length(true, w))

  if (na.rm) {
    nas <- purrr::reduce(
      list(
        which(is.na(true)),
        which(is.na(pred)),
        suppressWarnings(which(is.na(w)))
      ),
      union
    )

    if (!is_empty(nas)) {
      true <- true[-nas]
      pred <- pred[-nas]

      if (!is.null(w)){
        w    <- w[-nas]
      }
    }
  }

  if (!is.null(w)) {
    true <- rep(true, w)
    pred <- rep(pred, w)
  }

  mean(ape(true, pred))
}




#' @rdname weighted_mape
#' @export
mape <- function(true, pred, na.rm = TRUE){
  if (na.rm) {
    nas <- union(is.na(true), which(is.na(pred)))

    if (!is_empty(nas)) {
      true <- true[-nas]
      pred <- pred[-nas]
    }
  }

  mean(ape(true, pred))
}




#' @rdname weighted_mape
#' @export
mpe <- function(true, pred, na.rm = TRUE){
  if (na.rm) {
    nas <- union(is.na(true), which(is.na(pred)))

    if (!is_empty(nas)) {
      true <- true[-nas]
      pred <- pred[-nas]
    }
  }

  mean(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
weighted_mpe <- function(true, pred, w = NULL, na.rm = TRUE){

  if(!is.null(w)){
    true <- rep(true, w)
    pred <- rep(pred, w)
  }

  mean(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
ape <- function(true, pred){
  abs(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
pe <- function(true, pred){
  assert_that(isit::is_equal_length(true, pred))
  (pred / true - 1) * 100
}
