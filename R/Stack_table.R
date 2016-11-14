stack_table <- function(dat1, dat2, rem_ext = NULL){
  dat1 %assert_class% 'data.frame'
  dat2 %assert_class% 'data.frame'
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))

  dat1 <- as.data.table(copy(dat1))
  dat2 <- as.data.table(copy(dat2))

  if(!is.null(rem_ext)){
    setnames(dat1, gsub(rem_ext, '', names(dat1)))
    setnames(dat2, gsub(rem_ext, '', names(dat2)))
  }

  assert_that(identical(sort(names(dat1)), sort(names(dat2))))
  dat2 <- dplyr::select_(dat2, .dots = names(dat1))

  res <- list(dat1, dat2)
  class(res) <- c('Stack_table', 'list')
  return(res)
}

print_tex.Stack_table <- function(dat,
                                  method = 'row',
                                  insert_empty_row = TRUE,
                                  format_fun_tab1 = identity,
                                  format_fun_tab2 = function(x) paste0('(', x, ')'),
                                  format_num_tab1 = list(big.mark = '~', digits = 0, format = 'f'),
                                  format_num_tab2 = list(digits = 2, format = 'f')){

  method %assert_class% 'character'
  insert_empty_row %assert_class% 'logical'
  assert_that(is.scalar(method))
  assert_that(is.scalar(insert_empty_row))

  tab1     <-  dat[[1]] %>%
    df_format(num_format = format_num_tab1, format_fun = format_fun_tab1)


  tab2     <- dat[[2]] %>%
    df_format(num_format = format_num_tab2, format_fun = format_fun_tab2)


  res <- switch(method,
         'row' = stack_rows_tex(tab1, tab2, insert_empty_row = FALSE),
         'col' = stack_cols_tex(tab1, tab2))

  return(res)
}


stack_cols_tex <- function(tab1, tab2, insert_empty_row) {

  res <- foreach(i = 1:nrow(tab1), .combine = rbind) %do% {
    r <- paste(tab1[i,], tab2[i,], sep = ' ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(tab1)

    return(r)
  }
}


stack_rows_tex <- function(tab1, tab2, insert_empty_row) {
  empty_row <- rep('', length(tab1)) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    data.table::setnames(names(tab1))

  res <- foreach(i = 1:nrow(tab1), .combine = rbind) %do% {
    r <- paste(tab1[i,], tab2[i,], sep = ' \\newline ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(tab1)


    if (insert_empty_row && i != nrow(tab1)) {
      r <- rbind(r, empty_row)
    }

    return(r)
  }
}


as.data.frame.Stack_table <- function(dat, method = 'row'){
  dat    %assert_class% 'stack_table'
  method %assert_class% 'character'
  assert_that(is.scalar(method))
  if(method %in% c('c', 'column')) method <- 'col'
  if(method %in% c('r'))           method <- 'row'

}


stack_rows <- function(dat,
                       as_character = TRUE,
                       format_args1 = NULL,
                       format_args2 = NULL){
  dat %assert_class% 'Stack_table'

  rows <- foreach(i = 1:nrow(dat[[1]]), .combine = c) %do% {
    list(dplyr::slice(dat[[1]], i),
          dplyr::slice(dat[[2]], i)
    )
  }

  data.table::rbindlist(rows)
}


# df_stack <- function(dat1, dat2, method = 'r'){
#
#   # preconditions ----
#     dat1   %assert_class% 'data.frame'
#     dat2   %assert_class% 'data.frame'
#
#
#     output %assert_class% 'character'
#     assert_that(is.scalar(output))
#
#     # parse input parameters
#
#
#       if(output %in% c('df'))            output <- 'data.frame'
#       if(output %in% c('xls', 'excell')) output <- 'xlsx'
#       if(output %in% c('tex'))           output <- 'latex'
#
#   assert_that(method %in% c('row', 'col'))
#   assert_that(output %in% c('data.frame', 'xlsx', 'latex'))
#
# }

save_as.Stack_table <- function(dat, outfile, format){

}
