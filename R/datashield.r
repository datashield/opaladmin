#-------------------------------------------------------------------------------
# Copyright (c) 2013 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Set a Datashield method in Opal(s).
#' 
#' @title Set Datashield Method
#' 
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it will be accessed by Datashield users.
#' @param func Function name.
#' @param path Path to the R file containing the script (mutually exclusive with func).
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @export
dsadmin.set_method <- function(opal, name, func=NULL, path=NULL, type="aggregate") {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.set_method(o, name, func=func, path=path, type=type)})
  } else {
    # build method dto
    if(is.null(func)) {
      # read script from file
      rscript <- paste(readLines(path),collapse="\\n")
      rscript <- gsub('\"','\\\\"', rscript)
      methodDto <- paste('{"name":"', name, '","DataShield.RScriptDataShieldMethodDto.method":{"script":"', rscript, '"}}', sep='')  
    } else {
      methodDto <- paste('{"name":"', name, '","DataShield.RFunctionDataShieldMethodDto.method":{"func":"', func, '"}}', sep='')
    }
    # TODO check if method exists: create or update
    dsadmin.rm_method(opal, name, type=type)
    opal:::.post(opal, "datashield", "env", type, "methods", body=methodDto, contentType="application/json");
  }
}

#' Remove a Datashield method from Opal(s).
#' 
#' @title Remove Datashield Method
#' 
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it is accessed by Datashield users.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @export
dsadmin.rm_method <- function(opal, name, type="aggregate") {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.rm_method(o, name, type=type)})
  } else {
    # ignore errors and returned value
    resp <- opal:::.delete(opal, "datashield", "env", type, "method", name, callback=function(r){})
  }
}

#' Get a Datashield method from Opal(s).
#' 
#' @title Get Datashield Method
#' 
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it is accessed by Datashield users.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @export
dsadmin.get_method <- function(opal, name, type="aggregate") {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.get_method(o, name, type=type)})
  } else {
    opal:::.get(opal, "datashield", "env", type, "method", name)
  }
}

#' Get Datashield methods from Opal(s).
#' 
#' @title Get Datashield Methods
#' 
#' @param opal Opal object or list of opal objects.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @export
dsadmin.get_methods <- function(opal, type="aggregate") {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.get_methods(o, type=type)})
  } else {
    opal:::.get(opal, "datashield", "env", type, "methods")
  }
}

#' Install a package from Datashield public source repository on GitHub.
#'
#' @param opal Opal object or list of opal objects. 
#' @param pkg Package name.
#' @param ref Desired git reference. Could be a commit, tag, or branch name. Defaults to "master".
#' @export
dsadmin.install_datashield <- function(opal, pkg, ref="master") {
  opal.install_github(opal, pkg, username="datashield", ref=ref)
}
