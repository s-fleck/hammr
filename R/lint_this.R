lint_this <- function(...){

  if (require(lintr)){
    fname <- rstudioapi::getActiveDocumentContext()$path
    if (identical(fname, '')){
      fname <-  rstudioapi::getSourceEditorContext()$path
    }
    lint(fname, ...)
  } else {
    warning('Package "lintr" is not available.')
  }
}


lint_this2 <- function(...){
  sel_linters <- lintr:::settings$linters[
    !names(lintr:::settings$linters) %in% c('spaces_left_parentheses_linter',
                                            'single_quotes_linter')]

  if (require(lintr)){
    fname <- rstudioapi::getActiveDocumentContext()$path
    if (identical(fname, '')){
      fname <-  rstudioapi::getSourceEditorContext()$path
    }
    print(lint(fname, linters = sel_linters, ...))
  } else {
    warning('Package "lintr" is not available.')
  }

}
