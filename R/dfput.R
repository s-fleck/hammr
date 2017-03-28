#' Print code to recreate a data.frame
#'
#' Usefull for creating examples, somewhat cleaner output than dput
#'
#' @param factors
#'   `"full"`: preserve all factor levels, but longest output string,
#'   `"simple"`: keep only levels in the data,
#'   `"character"`: convert factor to character
#' @param dat a data.frame
#' @md
#' @return dat (invisibly)
#' @export
dfput <- function(dat, factors = 'simple'){
  assert_that(is.data.frame(dat))
  assert_that(purrr::is_scalar_character(factors))
  assert_that(factors %in% c('character', 'simple', 'full'))


  lines <- character()

  for(i in seq_along(dat)){
    if(!is.factor(dat[[i]])){
      value <- capture.output(dput(dat[[i]]))

    } else {
      if (factors %identical% 'character'){
        value <- capture.output(dput(as.character(dat[[i]]))) %>%
          paste(collapse = '')
      } else if (factors %identical% 'simple') {
        tmp <- capture.output(dput(as.character(dat[[i]]))) %>%
          paste(collapse = '')
        value <- sprintf('factor(%s)', tmp)
      } else if (factors %identical% 'full'){
        value <- capture.output(dput(dat[[i]])) %>%
          paste(collapse = '')
      } else {
        stop('Something went wrong')
      }
    }

    lines[[i]] <- sprintf('%s = %s,', names(dat)[[i]], value)
  }


  lines <- strwrap(lines, indent = 2, exdent = 4, width = 60)

  lines <- c(
    'data.frame(',
    lines,
    '  stringsAsFactors = FALSE',
    ')'
  )

  cat(paste(lines, collapse = '\n'))

  invisible(dat)
}
