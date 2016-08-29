labels_verka <- c('6' = 'Inland',
                  '1' = 'Empfang',
                  '2' = 'Versand',
                  '3' = 'Transit',
                  '7' = 'Ausland'
)

#' @export
labeller_verka <- ggplot2::as_labeller(labels_verka)

#' @export
label_verka <- function(x, as_factor = TRUE){
  res <- unlist(labeller_verka(x))

  if(as_factor) {
    res <- factor(res, levels = labels_verka)
  }
}


labels_nstrk <- c(
  '0' = 'Land- und forstwirtschaftl. Erzeugnisse',
  '1' = "Andere Nahrungs- und Futtermittel",
  '2' = 'Feste mineralische Brennstoffe',
  '3' = "Erdöl, Mineralölerzeugnisse",
  '4' = "Erze und Metallabfälle",
  '5' = 'Eisen, Stahl und NE-Metalle',
  '6' = 'Steine und Erden und Baustoffe',
  '7' = 'Düngemittel',
  '8' = 'Chemische Erzeugnisse',
  '9' = 'Fahrzeuge, Maschinen, sonst. Waren'
)



#' @export
labeller_nstrk <- ggplot2::as_labeller(labels_nstrk)

#' @export
label_nstrk <- function(x, as_factor = TRUE){
  x <- strtrim(x, 1)
  res <- unlist(labeller_nstrk(x))

  if(as_factor) {
    res <- factor(res, levels = labels_nstrk)
  }
}


labels_reg1 <- c(
  '1' = "Burgenland",
  '2' = "Kärnten",
  '3' = "Niederösterreich",
  '4' = "Oberösterreich",
  '5' = "Salzburg",
  '6' = "Steiermark",
  '7' = "Tirol",
  '8' = "Vorarlberg",
  '9' = "Wien"
)


#' @export
labeller_reg1 <- ggplot2::as_labeller(labels_reg1)

#' @export
label_reg1 <- function(x){
  x <- strtrim(x, 1)
  unlist(labeller_reg1(x))
}


