#' Complement a data.frame
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
#' @return a \code{data.frame} containing all rows from \code{dat} and extra
#'   rows for all values of \code{complement}
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
#'     g1 = df$g1,
#'     g2 = df$g2
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
  complement
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
    newcol[, (notcol) := NA]

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

  return(dd)
}
