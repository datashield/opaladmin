% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.r
\name{oadmin.install_package}
\alias{oadmin.install_package}
\title{Install Package}
\usage{
oadmin.install_package(opal, pkg, repos = NULL)
}
\arguments{
\item{opal}{Opal object or list of opal objects.}

\item{pkg}{Package name.}

\item{repos}{Character vector, the base URLs of the repositories to use.}
}
\value{
TRUE if successfully installed
}
\description{
Install package if not already available in Opal(s). To install the latest version of a package, it has to be removed first.
}
