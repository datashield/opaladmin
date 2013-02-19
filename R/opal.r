#-------------------------------------------------------------------------------
# Copyright (c) 2013 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Install package if not already available.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param repos Character vector, the base URLs of the repositories to use.
#' @export
oadmin.install_package <- function(opal, pkg, repos=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){oadmin.install_package(o, pkg, repos)})
  } else {
    if (!oadmin.installed_package(opal, pkg)) {
      # default repos
      defaultrepos <- c("http://cran.obiba.org/stable","http://cran.rstudio.com")
      # append user provided ones
      repostr <- paste('"', append(defaultrepos, repos),'"',collapse=',',sep='')
      cmd <- paste('install.packages("', pkg, '", repos=c(getOption("repos"),', repostr ,'), dependencies=TRUE)', sep='')
      resp <- opal.execute(opal, cmd, FALSE)
    }
  }
}

#' Remove package permanently.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
oadmin.remove_package <- function(opal, pkg) {
  resp <- opal.execute(opal, paste('remove.packages("', pkg, '")', sep=''), FALSE)
}

#' Check if a package is installed.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
oadmin.installed_package <- function(opal, pkg) {
  opal.execute(opal, paste('require("', pkg, '", character.only=TRUE)', sep=''), FALSE)
}

#' Get package description from Opal(s).
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
oadmin.package_description <- function(opal, pkg) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.package_description(o, pkg)})
  } else {
    inst <- opal.execute(opal, paste('installed.packages(fields=c("Aggregate","Assign"))', sep=''), FALSE)
    desc <- NULL
    for (i in 1:nrow(inst)) {
      if(inst[i]==pkg) { 
        desc <- strsplit(inst[i,],"\n")
        break
      }
    }
    return(desc)
  }
}

#' Install devtools package if not already available.
#'
#' @param opal Opal object or list of opal objects.
#' @export
oadmin.install_devtools <- function(opal) {
  oadmin.install_package(opal,'devtools')
}

#' Check if devtools package is installed.
#'
#' @param opal Opal object or list of opal objects.
#' @export
oadmin.installed_devtools <- function(opal) {
  oadmin.installed_package(opal,'devtools')
}

#' Install a package from a source repository on GitHub. Makes sure devtools package is available.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param username GitHub user name.
#' @param ref Desired git reference. Could be a commit, tag, or branch name. Defaults to "master".
#' @param auth_user Your github username if you're attempting to install a package hosted in a private repository (and your username is different to username).
#' @param password Your github password
#' @export
oadmin.install_github <- function(opal, pkg , username=getOption("github.user"), ref="master", auth_user=NULL, password=NULL) {
  opal.install_devtools(opal)
  cmd <- paste('devtools::install_github("', pkg, '", username="', username, '", ref="', ref, '")', sep="")
  opal.execute(opal, cmd, FALSE)
}

#' Load package in the current session.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.load_package <- function(opal, pkg) {
  opal.execute(opal, paste('library("', pkg, '")', sep=''), TRUE)
}

#' Unload package from the current session.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.unload_package <- function(opal, pkg) {
  resp <- opal.execute(opal, paste('detach("package:', pkg, '", character.only=TRUE, unload=TRUE)', sep=''), TRUE)
}
