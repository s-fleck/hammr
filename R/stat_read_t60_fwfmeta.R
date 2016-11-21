#' Title
#'
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
read_t60_fwfmeta <- function(infile){
  # 2016
  suppressWarnings(
    meta <- readr::read_fwf(infile,
                            col_positions = readr::fwf_positions(
                              start = c(10, 12, 33, 37, 40),
                              end   = c(11, 18, 35, 39, 999),
                              c('level', 'name', 'type', 'width', 'drop')
                            )) %>%
      hammr::df_trimws() %>%
      dplyr::mutate(name = tolower(name)) %>%
      as.data.table()
  )

  # für fixed decimal felder ist angebene feldlänge nicht die physische feldlänge
  meta[type == 'DF', width := as.integer(ceiling(width/2+0.5))]

  for (i in 1:nrow(meta)) {
    if (meta[i]$level %identical% 2L) {
      n2 <- meta[i]$name
    }

    if (meta[i]$level %identical% 3L) {
      meta[i, name := paste(n2, capwords(name), sep = '')]
    }
  }

  meta <- meta[!is.na(width)]

  readr::fwf_widths(meta$width, meta$name)
}
