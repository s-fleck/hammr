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


as.data.frame.Stack_table <- function(dat, method = 'r'){
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
