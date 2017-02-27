# Dirty -------------------------------------------------------------------

dirty_reload <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/hammr'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvtool'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/tatool'))
}


reload_gvtool <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvtool'))
}

reload_tatool <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/tatool'))
}

reload_hammr <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/hammr'))
}

reload_gvroad <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvroad'))
}

reload_gvrail <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvrail'))
}


install_gv <- function(creds = NULL){
  if (is.null(creds)) {
    creds <- ui_credentials(
      'Please your bitbucket username and password')
  }

  requireNamespace("devtools")
  requireNamespace("httr")
  requireNamespace("RCurl")
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  repositories <- c(
    's_fleck/testthis',
    's_fleck/tatool',
    's_fleck/gvtool',
    's_fleck/gvroad',
    's_fleck/gvrail'
  )

  for(repository in repositories){
    devtools::install_bitbucket(
      repository,
      username = creds$user,
      password = creds$pw,
      upgrade = FALSE
    )
  }
}
