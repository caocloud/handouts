\name{mySummary.ae}
\alias{mySummary.ae}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ mySummary.ae
%%  ~~function to do ... ~~
}
\description{
This gives the summary of all adverse events by two treatment arms. Each represents thenumber of patients in each arm for each specific adverse event and number of total adverse events showed for each adverse event. This table output also gives the fisher exact test between the number of patients in these treatment arms.
}
\usage{
mySummary.ae(ae,arm=as.factor(ae$arm))
}

\arguments{
  \item{ae:}{the table of adverse event}

  \item{arm:}{Treatment arms as factor variables}
}

\note{
  This package requires dependencies from this two packages: tidyverse package version 1.2.1 and flextable package version 0.4.4.
}

\author{Van Cao and members in Biostatistics Group at OUCRU-VN.}

\examples{
data(ae_fix)
mySummary.ae(ae_fix,arm=as.factor(ae_fix$arm))
}
