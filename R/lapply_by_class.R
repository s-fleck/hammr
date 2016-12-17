lapply_by_class <- function(x, fun, classes, ...){

  inner_fun <- function(inner_x, ...){
    if(any(class(inner_x) %in% classes)){
      return(fun(inner_x, ...))
    } else {
      return(inner_x)
    }
  }

  lapply(x, inner_fun, ...)
}
