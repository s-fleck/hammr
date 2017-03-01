# Dirty -------------------------------------------------------------------

dirty_reload <- function(){
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/hammr'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/gvtool'))
  try(devtools::load_all('P:/Verkehr/Projekte/Fleck/R/tatool'))
}


reinstall_helper <- function(x){
  basepath <- file.path('P:', 'Verkehr', 'Projekte', 'Fleck', 'R')

  if(!dir.exists(basepath)){
    basepath <- file.path('~', 'proj', 'R')
  }

  try(devtools::install(file.path(basepath, x)))
}


reinstall_gvroad <- function(){
  reinstall_helper('gvroad')
}


reinstall_tatool <- function(){
  reinstall_helper('tatool')
}

reinstall_gvtool <- function(){
  reinstall_helper('gvtool')
}


reload_helper <- function(x){
  basepath <- file.path('P:', 'Verkehr', 'Projekte', 'Fleck', 'R')

  if(!dir.exists(basepath)){
    basepath <- file.path('~', 'proj', 'R')
  }

  try(devtools::load_all(file.path(basepath, x)))
}

reload_gvtool <- function(){
  reload_helper('gvtool')
}

reload_tatool <- function(){
  reload_helper('tatool')
}

reload_hammr <- function(){
  reload_helper('hammr')
}

reload_gvroad <- function(){
  reload_helper('gvroad')
}

reload_gvrail <- function(){
  reload_helper('gvrail')
}

reload_gvout <- function(){
  reload_helper('gvout')
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
