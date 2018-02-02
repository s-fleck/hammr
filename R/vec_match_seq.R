#' Find position of a short vector inside a longer vector
#'
#' \code{vec_match_seq} returns a vector of the positions of the matches of
#' a sequence inside a longer vector. The index coresponds to the where the
#' first element of the sequence matches the vector. Please note that `NA`
#' matches `NA` to be consistent with [match()]
#'
#' @param sequence sequence to be matched
#' @param target   target vector for sequence matching
#'
#' @return vec_match_seq returns the indicies of the first elements of the
#'   matches between sequence and target. vec_match_seq_lgl returns a logical
#'   vector with the same length as `sequence`
#'
#' @family vector tools
#'
#' @rdname vec_match_seq
#' @export
#'
#' @examples
#'
#' t <- c(1, 2, 3, 4, 3, 2, 1)
#' s <- c(3, 4)
#'
#' vec_match_seq(s, t)
#' vec_match_seq_lgl(s, t)
#' s %seq_in% t
#'
vec_match_seq <- function(sequence, target){
  which(vec_match_seq_internal(sequence, target))
}




#' @rdname vec_match_seq
#' @export
vec_match_seq_lgl <- function(sequence, target){
  res  <- vec_match_seq_internal(sequence, target)
  to_fill <- seq.int((length(res) + 1L), length(target))
  res[to_fill] <- FALSE
  res
}




#' @rdname vec_match_seq
#' @export
`%seq_in%` <- function(sequence, target){
  any(vec_match_seq_internal(sequence, target))
}




# utils -------------------------------------------------------------------

vec_match_seq_internal <- function(sequence, target){
  group_matches <- function(x, matches, sequence, target){
    sub <- seq.int(x, (length(sequence) + x - 1L))
    matches[sub]
  }

  add_elements <- function(...){
    tmp <- list(...)[[1]]
    all(purrr::map_lgl(seq_along(tmp), function(x) tmp[[x]][[x]]))
  }

  matches    <- purrr::map(target, equal_or_na, sequence)
  iterations <- seq_len(length(target) - length(sequence) + 1)
  matches    <- purrr::map(
    iterations, group_matches, matches, sequence, target
  )
  res <- purrr::modify_depth(matches, 1, add_elements)
  unlist(res)
}
