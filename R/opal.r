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
#' @export
opal.install_package <- function(opal, pkg) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.install_package(o, pkg)})
  } else {
    if (!opal.installed_package(opal, pkg)) {
      opal.execute(opal, paste('install.packages("', pkg, '", dependencies=TRUE)', sep=''), FALSE)
    }
  }
}

#' Remove package permanently.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.remove_package <- function(opal, pkg) {
  opal.execute(opal, paste('remove.packages("', pkg, '")', sep=''), FALSE)
}

#' Check if a package is installed.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.installed_package <- function(opal, pkg) {
  opal.execute(opal, paste('require("', pkg, '", character.only=TRUE)', sep=''))
}

#' Get package description from Opal(s).
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.package_description <- function(opal, pkg) {
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
opal.install_devtools <- function(opal) {
  opal.install_package(opal,'devtools')
}

#' Check if devtools package is installed.
#'
#' @param opal Opal object or list of opal objects.
#' @export
opal.installed_devtools <- function(opal) {
  opal.installed_package(opal,'devtools')
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
opal.install_github <- function(opal, pkg , username=getOption("github.user"), ref="master", auth_user=NULL, password=NULL) {
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
  opal.execute(opal, paste('detach("package:', pkg, '", character.only=TRUE, unload=TRUE)', sep=''), TRUE)
}
