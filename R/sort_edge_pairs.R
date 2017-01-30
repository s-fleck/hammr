#' Sort a edge pairs into a path
#'
#' Sorts a finite number of directed edge-pairs defined by
#' previous, current and next node.
#'
#' \code{\link{sort_edge_pairs.default}} and \code{\link{sort_edge_pairs.data.table}}
#'
#'
#' @importFrom data.table data.table
#'
#' @return a sorted data.table consisting of the columns p, c and n (and .id if allow_partial = TRUE)
#' @export
sort_edge_pairs <- function(x, ..., allow_partial) UseMethod('sort_edge_pairs')


#' Sort a edge pairs into a path
#'
#' Sorts a finite number of directed edge-pairs defined by
#' previous, current and next node.
#'
#' @param p revious node
#' @param c current node (node connecting both edges)
#' @param n next node
#' @param allow_partial If allow partial is set to FALSE, an unsortable data set
#'  (eg not all nodes are connected) will throw an error. If set to TRUE, the
#'  nodes are sorted as well as possible. An additonal column .id is added to
#'  the results data.table to indicate each sorted group.
#'
#' @importFrom data.table data.table
#'
#' @return a sorted data.table consisting of the columns p, c and n (and .id if allow_partial = TRUE)
#' @export
sort_edge_pairs.default <- function(x, c, n, allow_partial = FALSE){
  p <- x

  # Check inputs ----
    assert_that(identical(length(p), length(c)))
    assert_that(identical(length(p), length(n)))

    if(!allow_partial){
      assert_that(is_potentially_sortable(p, c, n))
    }


  # Setup variables ----
    dat      <- data.table::data.table(p, c, n)
    nrow_dat <- nrow(dat)                            # Required for early return
    sorted   <- list()
    i        <- 1


  # Logic ----

    # Split data.table into list of sorted blocks.
    while(nrow(dat) > 0){
      cur <- dat[ 1]
      dat <- dat[-1]

      sel <- dat$c == cur$p   & dat$n == cur$c & !is.na(cur$p) & !is.na(dat$n)
      prv <- dat[ sel]
      dat <- dat[!sel]

      sel <- dat$c == cur$n   & dat$p == cur$c & !is.na(cur$n) & !is.na(dat$p)
      nxt <- dat[ sel]
      dat <- dat[!sel]


      # Trigger early return for trivial cases (covers >99% of all cases for SGV transport chains) ----

        # Everything sorted correclty
        if(nrow_dat %identical% 2L & any(
            nrow(nxt) %identical% 1L,
            nrow(prv) %identical% 1L)){

            res <- rbindlist(list(prv, cur, nxt))

            if(allow_partial){
              res[, .id := 1L]
            }

            return(res)
        }

        if(nrow_dat %identical% 3 & all(
          nrow(nxt) %identical% 1L,
          nrow(prv) %identical% 1L)){

          res <- rbindlist(list(prv, cur, nxt))
          if(allow_partial){
            res[, .id := 1L]
          }

          return(res)
        }

        # n = 2 and not sortable
        if(nrow_dat %identical% 2L & all(
          nrow(nxt) %identical% 0L,
          nrow(prv) %identical% 0L)){
             res <- rbindlist(list(cur, dat), idcol = ifelse(allow_partial, TRUE, NULL))
             return(res)
        }


      # Continue if no early exit was possible ----

      sorted[[i]] <- rbind(prv, cur, nxt)
      if(!is_sorted_edge_pairs(sorted[[i]]) |
         (!nrow(cur) %identical% 1L) |
         nrow(prv) > 1  |
         nrow(nxt) > 1) {
        stop(cannot_be_sorted_error())
      }

      i = i+1
    }





  # Sort the blocks
    nblocks <- length(sorted)
    j       <- 1

    while(length(sorted) > j){
      cur      <- sorted[[j]]
      failsave <- cur


      # Pop previous element
        prv_id <- get_prv(sorted, cur)

        if(length(prv_id) > 0){
          prv              <- sorted[[prv_id]]
          sorted[[prv_id]] <- NULL
        } else {
          prv <- data.table::data.table()
        }

      # Pop next element
        nxt_id <- get_nxt(sorted, cur)

        if(length(nxt_id) > 0){
          nxt              <- sorted[[nxt_id]]
          sorted[[nxt_id]] <- NULL
        } else {
          nxt <- data.table::data.table()
        }

      # Attach previous and current elements to front and back of sorted elements
        sorted[[j]] <- data.table::rbindlist(list(prv, cur, nxt))


      # If no previous or next nodes were found, but the whole list has not
      # yet been sorted: Fail
        if(length(sorted) > 1 & identical(sorted[[j]], failsave)){
          if(allow_partial){
            j = j+1
          } else {
            stop(cannot_be_sorted_error())
          }
        }
    }


  if(allow_partial %identical% TRUE){
    res <- data.table::rbindlist(sorted, idcol = TRUE)
    ok  <- res[, .(ok = assert_that(is_sorted_edge_pairs(p, c, n))), by = '.id']
    assert_that(all(ok))
  } else {
    res <- data.table::rbindlist(sorted)
    assert_that(is_sorted_edge_pairs(res))
  }


  return(res)
}


#' Sort a data.frame where rows contain from-current-to relationships
#'
#' If the data frame row ids do not form a consecutive path, an error is thrown
#'
#' @param x a data frame
#' @param p_name name of variable containing id of previous row
#' @param c_name name of variable containing id of current row
#' @param n_name name of variable conttaining id of next row
#' @export
sort_edge_pairs.data.table <- function(x, p_name = 'p', c_name = 'c', n_name = 'n', allow_partial = FALSE){
  dat <- x

  if(identical(nrow(dat), 1L)) {
    if(allow_partial){
      dat <- data.table::copy(dat)
      dat$.id <- 1L
    }

    return(dat)
  }

  dat <- data.table::copy(as.data.table(dat))

  p <- dat[[p_name]]
  c <- dat[[c_name]]
  n <- dat[[n_name]]

  sorted  <- sort_edge_pairs(p, c, n, allow_partial = allow_partial)
  order   <- match(paste(sorted$p, sorted$c, sorted$n), paste(dat[[p_name]], dat[[c_name]], dat[[n_name]]))
  res     <- dat[order]

  if(allow_partial){res$.id <- sorted$.id}

  return(res)
}


#' Sort a data.frame where rows contain from-current-to relationships
#'
#' If the data frame row ids do not form a consecutive path, an error is thrown
#'
#' @param dat a data frame
#' @param p_name name of variable containing id of previous row
#' @param c_name name of variable containing id of current row
#' @param n_name name of variable conttaining id of next row
#' @export
sort_edge_pairs.data.frame <- function(x, p_name = 'p', c_name = 'c', n_name = 'n', allow_partial = FALSE){
  dat <- data.table::as.data.table(x)
  res <- sort_edge_pairs(dat, p_name = p_name, c_name = c_name, n_name = n_name, allow_partial = allow_partial)
  res <- as.data.frame(res)
  return(res)
}


# sort edge pairs - helpers ----


  #' Is potentially sortable
  #'
  #' FALSE gurantees that not all edge pairs are sortable (but TRUE does
  #' not gurantee that they are)
  #'
  #' @return TRUE if there is a chance that the edge pairs could be sorted
  #' @export
  is_potentially_sortable <-  function(p, c, n){
      no_current_node_na <-  identical(sum(is.na(c)), 0L)

      na_p <- sum(is.na(p))
      na_n <- sum(is.na(n))

      max_one_parentless_node       <-  na_p <= 1
      max_one_childless_node        <-  na_n <= 1

      # Each 'previous' node, except the first, must also be a 'current' node
      # Each 'next' node, except the last, must have been a 'current' node
      current_and_previous_nodes_ok <-  sum(p %in% c) == length(p) - 1
      current_and_next_nodes_ok     <-  sum(n %in% c) == length(p) - 1


      res <- no_current_node_na            &
             max_one_parentless_node       &
             max_one_childless_node        &
             current_and_previous_nodes_ok &
             current_and_next_nodes_ok

      return(res)
  }


  get_prv <- function(dat, cur){
    dat %assert_class% 'list'
    cur %assert_class% 'data.table'
    assert_that(all('data.table' == sapply(dat, class)[1, ]))

    which(
      unlist(lapply(dat, function(x) x[nrow(x)]$c == cur[1]$p)) &
        unlist(lapply(dat, function(x) x[nrow(x)]$n == cur[1]$c))
    )
  }


  get_nxt <- function(dat, cur){
    dat %assert_class% 'list'
    cur %assert_class% 'data.table'
    assert_that(all('data.table' == sapply(dat, class)[1, ]))

    which(
      unlist(lapply(dat, function(x) x[1]$c == cur[nrow(cur)]$n)) &
        unlist(lapply(dat, function(x) x[1]$p == cur[nrow(cur)]$c))
    )
  }


# Utils ----

#' @export
is_sorted_edge_pairs <- function(p, c = NULL, n = NULL){
  if(is.null(c) && is.null(n)){

    assert_that(is.data.table(p))
    assert_that(names(p) %identical% c('p', 'c', 'n'))

    dat <- p
  } else {
    dat         <- data.table::data.table(p, c, n)
  }

  is_sorted   <- TRUE

  assert_that(nrow(dat) > 0)

  if(nrow(dat) %identical% 1L) return(TRUE)

  for(i in 2:nrow(dat)){
    is_sorted <- is_sorted &
      dat[i]$p  %identical% dat[i-1]$c  &
      dat[i]$c  %identical% dat[i-1]$n

    if(!is_sorted) return(FALSE)
  }

  return(is_sorted)
}

#' @export
is_sortable_edge_pairs <- function(p, c, n){
  tt  <- try(sort_edge_pairs(x=p, c=c, n=n), silent = TRUE)
  res <- ifelse(is(tt,"try-error"), FALSE, TRUE)

  return(res)
}
