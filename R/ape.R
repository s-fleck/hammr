#' Percentage Error Metrics
#'
#' Simple and useful prediction performance metrics:
#' * PE: Percentage Error
#' * MPE: Mean Percentage Error
#' * APE: Absolute Percentage Error
#' * MAPE: Mean Absolute Percentage Error
#'
#' @param true
#' @param pred
#' @param weights
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
#' @examples
weighted_mape <- function(true, pred, w = 1, na_rm = TRUE){

  if(na_rm){
    nas <- purrr::reduce(
      list(which(is.na(true)), which(is.na(pred)), which(is.na(w))),
      union
    )

    if(!is_empty(nas)){
      true <- true[-nas]
      pred <- pred[-nas]
      w    <- w[-nas]
    }
  }


  true <- rep(true, w)
  pred <- rep(pred, w)
  mean(ape(true, pred))
}




#' @rdname weighted_mape
#' @export
mape <- function(true, pred, na_rm = TRUE){
  if(na_rm){
    nas <- union(is.na(true), which(is.na(pred)))

    if(!is_empty(nas)){
      true <- true[-nas]
      pred <- pred[-nas]
    }
  }

  mean(ape(true, pred))
}




#' @rdname weighted_mape
#' @export
mpe <- function(true, pred, na_rm = TRUE){
  if(na_rm){
    nas <- union(is.na(true), which(is.na(pred)))

    if(!is_empty(nas)){
      true <- true[-nas]
      pred <- pred[-nas]
    }
  }

  mean(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
weighted_mpe <- function(true, pred, weights = 1, na.rm = TRUE){
  true <- rep(true, weights)
  pred <- rep(pred, weights)
  mean(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
ape = function(true, pred){
  abs(pe(true, pred))
}




#' @rdname weighted_mape
#' @export
pe = function(true, pred){
  (pred/true - 1) * 100
}
