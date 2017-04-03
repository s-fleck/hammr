lint_this <- function(...){
  if (!requireNamespace('lintr')){
    stop('Package "lintr" is not available.')
  } else if (!requireNamespace('rstudioapi')){
    stop('Package "rstudioapi" is not available.')
  }

  fname <- rstudioapi::getActiveDocumentContext()$path

  if (identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  print(lint(fname, ...))
}


lint_this2 <- function(...){
  if (!requireNamespace('lintr')){
    stop('Package "lintr" is not available.')
  } else if (!requireNamespace('rstudioapi')){
    stop('Package "rstudioapi" is not available.')
  }

  sel_linters <- lintr:::settings$linters[
    !names(lintr:::settings$linters) %in% c('spaces_left_parentheses_linter',
                                            'single_quotes_linter')]

  fname <- rstudioapi::getActiveDocumentContext()$path

  if (identical(fname, '')){
    fname <-  rstudioapi::getSourceEditorContext()$path
  }

  print(lint(fname, linters = sel_linters, ...))
}
