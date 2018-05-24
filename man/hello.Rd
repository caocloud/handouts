\name{check.packages}
\alias{check.packages}
\title{check.packages}
%% function for checking the presence of packages in local directory

\usage{
check.packages(x)

}
\description{
The main purpose of this function is to check if there is a required package in the local directory by providing the name of the package. if not, install the package and load the package to the workspace otherwise the function keeps the install step and load the package.

The function take the x and check in the library folder in local directory. If there is no package found in the library folder, it looks for the tar.gz file of that package in the rlib library to install that package.
}

\arguments{
  \item{x :}{A name of a packages or a list of packages to be check.
  }
}

\author{Van Cao and members in Biostatistics Group at OUCRU-VN.}

\examples{
check.packages("janitor")
}

