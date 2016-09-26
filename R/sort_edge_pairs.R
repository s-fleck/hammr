#' Sort a data.frame where rows contain from-current-to relationships
#'
#' If the data frame row ids do not form a consecutive path, an error is thrown
#'
#' @param dat a data frame
#' @param p_name name of variable containing id of previous row
#' @param c_name name of variable containing id of current row
#' @param n_name name of variable conttaining id of next row
#' @export
sort_edge_pair_df <- function(dat, p_name = 'p', c_name = 'c', n_name = 'n'){
  dat <- data.table::copy(as.data.table(dat))

  p <- dat[[p_name]]
  c <- dat[[c_name]]
  n <- dat[[n_name]]

  sorted <- sort_edge_pairs(p, c, n)

  order <- match(paste(sorted$p, sorted$c, sorted$n), paste(dat[[p_name]], dat[[c_name]], dat[[n_name]]))

  dat <- dat[order]
}


#' Sort a edge pairs into a path
#'
#' Sorts a finite number of directed edge-pairs bedfined by the input
#' previous, current and next node. If the input cannot be sorted into
#' a connected path, an error will be thrown.
#'
#' @param p revious node
#' @param c current node (node connecting both edges)
#' @param n next node
#'
#' @import data.table
#'
#' @return a sorted data.table
#' @export
sort_edge_pairs <- function(p, c, n){

  # Check inputs ----
    assert_that(identical(length(p), length(c)))
    assert_that(identical(length(p), length(n)))
    assert_that(identical(sum(is.na(c)), 0L))

    na_p <- sum(is.na(p))
    na_n <- sum(is.na(n))


  # Check if inputs can be sorted ----
    max_one_parentless_node       <-  na_p <= 1
    max_one_childless_node        <-  na_n <= 1

    # Each 'previous' node, except the first, must also be a 'current' node
    # Each 'next' node, except the last, must have been a 'current' node
    current_and_previous_nodes_ok <-  sum(p %in% c) == length(p) - 1
    current_and_next_nodes_ok     <-  sum(n %in% c) == length(p) - 1


    if(!(max_one_parentless_node &
         max_one_childless_node &
         current_and_previous_nodes_ok &
         current_and_next_nodes_ok)){
      stop(cannot_be_sorted_error())
    }


  # Setup variables ----
    dat    <- data.table::data.table(p, c, n)
    sorted <- list()
    i      <- 1


  # Logic ----

  # Split data.table into list of sorted blocks.
  while(nrow(dat) > 0){
    cur <- dat[ 1]
    dat <- dat[-1]

    prv <- dat[ (c == cur$p & !is.na(cur$p))]
    dat <- dat[!(c == cur$p & !is.na(cur$p))]

    nxt <- dat[ (c == cur$n & !is.na(cur$n))]
    dat <- dat[!(c == cur$n & !is.na(cur$n))]

    sorted[[i]] <- rbind(prv, cur, nxt)
    if(!is_node_sorted(sorted[[i]])){
      stop(cannot_be_sorted_error())
    }

    i = i+1
  }


  # Sort the blocks
  failsave <- 0

  while(length(sorted) > 1){
    failsave <- failsave+1
    cur <- sorted[[1]]

    prv_id <- get_prv(sorted, cur)

    if(length(prv_id) > 0){
      prv              <- sorted[[prv_id]]
      sorted[[prv_id]] <- NULL
    } else {
      prv <- data.table::data.table()
    }


    nxt_id <- get_nxt(sorted, cur)

    if(length(nxt_id) > 0){
      nxt              <- sorted[[nxt_id]]
      sorted[[nxt_id]] <- NULL
    } else {
      nxt <- data.table::data.table()
    }

    sorted[[1]] <- data.table::rbindlist(list(prv, cur, nxt))


    if(failsave > 1000){
      stop('Sorting algrotihm did not terminate. Input is likely corrup.')
    }
  }

  res <- sorted[[1]]


  # Check if everything went right, only first and last node can have no parent / child
    ok <- sum(is.na(res[1]))         < 2
    ok <- ok & sum(is.na(res[nrow(res)])) < 2

    if(nrow(res) > 2){
      sub <- res[2:(nrow(res)-1)]
      ok  <- ok & sum(is.na(sub)) %identical% 0L
    }

    assert_that(ok == TRUE)


  assert_that(is_node_sorted(res))
  return(res)
}


#' @export
is_sortable_edge_pairs <- function(p, c, n){
  tt  <- try(sort_edge_pairs(p=p, c=c, n=n), silent = TRUE)
  res <- ifelse(is(tt,"try-error"), FALSE, TRUE)

  return(res)
}


# Utils ----
is_node_sorted <- function(p, c = NULL, n = NULL){
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

