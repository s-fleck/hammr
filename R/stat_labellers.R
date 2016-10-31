# Various helper functions to help assinging text labels to variables used
# by various STAT packages. Should probably go into a lookup database


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

Encoding(labels_nstrk) <- 'UTF-8'


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


labels_nstrk_s <- c(
  '0' = 'Land- u forstw. Erzeugnisse',
  '1' = "Nahrungs- u Futtermittel",
  '2' = 'Feste Brennstoffe',
  '3' = "Erdölerzeugnisse",
  '4' = "Erze und Metallabfälle",
  '5' = 'Eisen, Stahl und NE-Metalle',
  '6' = 'Steine, Erden und Baustoffe',
  '7' = 'Düngemittel',
  '8' = 'Chemische Erzeugnisse',
  '9' = 'Sonstige Waren'
)

Encoding(labels_nstrk_s) <- 'UTF-8'

#' @export
labeller_nstrk_s <- ggplot2::as_labeller(labels_nstrk_s)

#' @export
label_nstrk_s <- function(x, as_factor = TRUE){
  x <- strtrim(x, 1)
  res <- unlist(labeller_nstrk_s(x))

  if(as_factor) {
    res <- factor(res, levels = labels_nstrk_s)
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

Encoding(labels_reg1) <- 'UTF-8'


#' @export
labeller_reg1 <- ggplot2::as_labeller(labels_reg1)

#' @export
label_reg1 <- function(x){
  x <- strtrim(x, 1)
  unlist(labeller_reg1(x))
}



labels_reg1_s <- c(
  '1' = "Burgenl.",
  '2' = "Kärnten",
  '3' = "Niederö.",
  '4' = "Oberö.",
  '5' = "Salzb.",
  '6' = "Steierm.",
  '7' = "Tirol",
  '8' = "Vorarlb.",
  '9' = "Wien"
)

Encoding(labels_reg1_s) <- 'UTF-8'


#' @export
labeller_reg1_s <- ggplot2::as_labeller(labels_reg1_s)

#' @export
label_reg1_s <- function(x){
  x <- strtrim(x, 1)
  unlist(labeller_reg1_s(x))
}


labels_reg1_ss <- c(
  '1' = "B",
  '2' = "K",
  '3' = "NÖ",
  '4' = "OÖ",
  '5' = "S",
  '6' = "ST",
  '7' = "T",
  '8' = "V",
  '9' = "W"
)

Encoding(labels_reg1_ss) <- 'UTF-8'


#' @export
labeller_reg1_ss <- ggplot2::as_labeller(labels_reg1_ss)

#' @export
label_reg1_ss <- function(x){
  x <- strtrim(x, 1)
  unlist(labeller_reg1_ss(x))
}

