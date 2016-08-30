#' Format table for exporting
#'
#' @param line1
#' @param line2
#' @param insert_empty_line
#' @param remove_ext
#' @param as_character
#'
#' @export
stat_table <- function(line1, line2, insert_empty_line = TRUE, remove_ext = NULL, as_character = TRUE){
  if(!is.null(remove_ext)){
    names(line1) <- gsub(remove_ext, '', names(line1))
    names(line2) <- gsub(remove_ext, '', names(line2))
  }


  assert_that(identical(sort(names(line1)), sort(names(line2))))
  assert_that(identical(nrow(line1), nrow(line2)))


  if(insert_empty_line) {
    if(!as_character)
      warning('Columns will be coerced to character if insert_empty_line = TRUE')
    as_character <- TRUE
  }


  if(as_character){
    line1 <- apply(line1, 2, as.character) %>% as.data.frame(stringsAsFactors = FALSE)
    line2 <- apply(line2, 2, as.character) %>% as.data.frame(stringsAsFactors = FALSE)
  }


  res <- foreach(i = 1:nrow(line1), .combine = rbind) %do% {
    r <- rbind(line1[i, ], line2[i, ])

    if(insert_empty_line && i != nrow(line1)){
      el <- rep('', length(line1)) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE)
      names(el) <- names(line1)
      r <- rbind(r, el)
    }

    return(r)
  }

  return(res)
}



print_xtable <- function(tab, digits = 0, ...) {
  alignment <- paste0('ll', paste(rep('X', ncol(tab)-1), collapse = ''))

  xtable(tab, align = alignment, digits = 0, ...) %>%
    print(format.args = list(big.mark = "~"),
          include.rownames=FALSE,
          floating = FALSE,
          tabular.environment = 'tabularx',
          booktabs = TRUE,
          sanitize.text.function = identity,
          width = '\\textwidth',
          digits = digits)
}


removetex <- function(x){
  x <- stringr::str_replace(x, '\\\\newline', '\n')
  x <- stringr::str_replace(x, '~', ' ')
}
