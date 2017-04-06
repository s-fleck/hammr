#' Complement a data.frame based on vectors
#'
#' Adds empty rows to a \code{data.frame} based values in an id column or a
#' combination of id columns. The intended use of this function is to ensure
#' that \code{data.frames} that result from aggreating larger \code{data.frames}
#' contain the same groups.
#'
#' @param dat a \code{data.frame}
#' @param complement a \code{list} of the form \code{list(col1 = c("value1",
#'   "value2", ...), col2 = c(1, 2, ...)), ...}. The names of \code{complement}
#'   must correspond to column names of \code{dat}, and all list elements of
#'   \code{complement} must have the same length.
#' @param fill Value to fill newly created rows with.
#'
#'
#' @return a \code{data.frame} containing all rows from \code{dat} and extra
#'   rows for all values of \code{complement}
#'
#' @family data.frame tools
#' @export
#'
#' @examples
#'
#' df1 <- data.frame(
#'   g1 = c("a", "b", "c"),
#'   g2 = c("A", "A", "C"),
#'   da = 1:3
#' )
#'
#' comp = list(
#'   g1 = c("a", "d", "c"),
#'   g2 = c("A", "B", "B")
#' )
#'
#' df2 <- data.frame(
#'   g1 = c("a", "d", "c"),
#'   g2 = c("A", "B", "B"),
#'   da = 7:9
#' )
#'
#'
#' df_complement(
#'   df1,
#'   list(
#'     g1 = df2$g1,
#'     g2 = df2$g2
#'   )
#' )
#'
#' #    g1 g2 da
#' # 1:  a  A  1
#' # 2:  b  A  2
#' # 3:  c  C  3
#' # 4:  d  B NA
#' # 5:  c  B NA
#'
#' df_complement(
#'   df2,
#'   list(
#'     g1 = df1$g1,
#'     g2 = df1$g2
#'   )
#' )
#'
#' #    g1 g2 da
#' # 1:  a  A  7
#' # 2:  d  B  8
#' # 3:  c  B  9
#' # 4:  b  A NA
#' # 5:  c  C NA
#'
df_complement <- function(
  dat,
  complement,
  fill = NA
){
  # Pre-conditions
    assert_that(hammr::all_unique(names(dat), silent = TRUE))
    assert_that(hammr::all_unique(names(complement), silent = TRUE))

    assert_that(is.list(complement))
    assert_that(all(names(complement) %in% names(dat)))

    assert_that(suppressWarnings(hammr::all_identical(
      lapply(complement, length))
    ))

    assert_that(length(complement) > 0)


  # Setup
    dd     <- as.data.table(data.table::copy(dat))

    col    <- names(complement)
    notcol <- names(dd)[!names(dd) %in% col]
    newcol <- dd[1]
    newcol[, (notcol) := fill]

    extract_values <- function(comp, i){
      as.character(comp[[i]])
    }


  # Logic
    for(i_comp in seq_along(complement[[1]])){
      values <- purrr::map(complement, extract_values, i_comp)

      for(i_row in seq_len(nrow(dd))){
        is_match <- identical(
          values,
          lapply(as.list(dd[i_row, (col), with = FALSE]), as.character)
        )

        if (is_match) {
          break
        } else if (identical(i_row, nrow(dd))) {
          newcol[, (col) := values]
          dd <- rbind(
            dd,
            newcol
          )
        }
      }
    }


  if(data.table::is.data.table(dat)){
    return(dd)
  } else {
    return(as.data.frame(dd))
  }
}




#' Complement a data.frame based on a different data.frame
#'
#' @param df1 a data.frame
#' @param df2 a data.frame
#' @param complement_cols columns to base the complementation on
#' @param fill value to fill newly created rows with
#'
#' @export
df_complement2 <- function(
  df1,
  df2,
  complement_cols,
  fill = NA
){
  # Pre-conditions
    assert_that(hammr::all_unique(names(df1), silent = TRUE))
    assert_that(hammr::all_unique(names(df2), silent = TRUE))
    assert_that(
      is.character(complement_cols) &&
      all_unique(complement_cols, silent = TRUE)   &&
      length(complement_cols) > 0
    )

  # add missing rows
    c1 <- df2 %>%
      dplyr::select_(.dots = complement_cols) %>%
      as.list()

    c2 <- df1 %>%
      dplyr::select_(.dots = complement_cols) %>%
      as.list()

    res <- list(
      df1 = df_complement(df1, complement = c1),
      df2 = df_complement(df2, complement = c2)
    )


  # Add missing cols
    missing_cols_df1 <- setdiff(names(res$df2), names(res$df1))
    missing_cols_df2 <- setdiff(names(res$df1), names(res$df2))

    for(col in missing_cols_df1){
      res$df1[[col]] <- fill
    }

    for(col in missing_cols_df2){
     res$df2[[col]] <- fill
    }


  # If factor levels are not identical, convert column to character
    for(el in complement_cols){
      if(!identical(levels(res$df1[[el]]), res$df2[[el]])){
        res$df1[[el]] <- as.character(res$df1[[el]])
        res$df2[[el]] <- as.character(res$df2[[el]])
      }
    }



  # Sort outgoing data.frames
    data.table::setcolorder(res$df2, names(res$df1))


  # Post-conditions
    rescc1 <- res$df1 %>%
      dplyr::select_(.dots = complement_cols)
    rescc2 <- res$df1 %>%
      dplyr::select_(.dots = complement_cols)

    assert_that(rescc1 %identical% unique(rescc1))
    assert_that(rescc2 %identical% unique(rescc2))


    assert_that(identical(
      nrow(res$df1), nrow(res$df2)
    ))

    assert_that(identical(
      names(res$df1), names(res$df2)
    ))

  unname(res)
}
