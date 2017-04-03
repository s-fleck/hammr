#' Find position of a short vector inside a longer vector
#'
#' \code{vec_match_seq} returns a vector of the positions of the matches of
#' a sequence inside a longer vector. The index coresponds to the where the
#' first element of the sequence matches the vector.
#'
#' `%seq_in%` returns a logical vector of the matching described above
#'
#' @param sequence sequence to be matched
#' @param target   target vector for sequence matching
#'
#' @family vector tools
#'
#' @rdname vec_match_seq
#' @export
vec_match_seq <- function(sequence, target){
  which(sequence %seq_in% target)
}


#' @rdname vec_match_seq
#' @export
`%seq_in%` <- function(sequence, target){
  group_matches <- function(x, matches, sequence, target){
    sub <- x:(length(sequence) + x - 1)
    matches[sub]
  }

  add_elements <- function(...){
    tmp <- list(...)[[1]]
    all(purrr::map_lgl(seq_along(tmp), function(x) tmp[[x]][[x]]))
  }


  matches    <- purrr::map(target, equal_or_both_na, sequence)
  iterations <- 1:(length(target) - length(sequence) + 1)
  matches    <- purrr::map(iterations, group_matches,
                           matches, sequence, target)

  res <- purrr::at_depth(matches, 1, add_elements)

  fill <- (length(res) + 1):length(target)
  res[fill] <- FALSE
  unlist(res)
}
