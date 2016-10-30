#' Find position of a short vector inside a longer vector
#'
#' @export
match_sequence_to_vector <- function(a, b){
  Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x) {
    y[[x]][x:(length(a) - length(b) + x)]
  })) == length(b)
}

match_vector <- function(a, b){
  warning('Deprecated. use match_sequence_to_vector instead.')
  match_sequence_to_vector(a, b)
}
