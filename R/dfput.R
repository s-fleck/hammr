#' Print code to recreate a data.frame
#'
#' Usefull for creating data.frames in code examples, somewhat cleaner output
#' than dput
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
  assert_that(rlang::is_scalar_character(factors))
  assert_that(factors %in% c('character', 'simple', 'full'))


  lines <- character()

  for(i in seq_along(dat)){
    if(!is.factor(dat[[i]])){
      value <- utils::capture.output(dput(dat[[i]]))

    } else {
      if (factors %identical% 'character'){
        value <- utils::capture.output(dput(as.character(dat[[i]]))) %>%
          paste(collapse = '')
      } else if (factors %identical% 'simple') {
        tmp <- utils::capture.output(dput(as.character(dat[[i]]))) %>%
          paste(collapse = '')
        value <- sprintf('factor(%s)', tmp)
      } else if (factors %identical% 'full'){
        value <- utils::capture.output(dput(dat[[i]])) %>%
          paste(collapse = '')
      } else {
        stop('Something went wrong')
      }
    }

    value <- paste(value, collapse = "")
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
