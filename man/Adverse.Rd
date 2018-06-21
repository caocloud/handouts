\name{mySummary.ae}
\alias{mySummary.ae}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ mySummary.ae
%%  ~~function to do ... ~~
}
\description{
This gives the summary of all events by two treatment arms. Each represents the number of patients in each arm for each specific event and number of total events showed for each event. This table output also gives the fisher exact test between the number of patients in two treatment arms.
}
\usage{
mySummary.ae(ae_fix,arm=as.factor(ae_fix$arm),var=ae$AE,SUBJID="SUBJID",name="ae.name")
}

\arguments{
  \item{ae:}{the table of adverse event}

  \item{arm:}{Treatment arms as factor variables}
  \item{var:}{List of events wanted to be summarized}
  \item{SUBJID:}{Name of IDs in the data}
  \item{name:}{The name of events used in this summary table. It can be adverse event names or others like accident event or blood type...}
}

\note{
  This package requires dependencies from this two packages: tidyverse package version 1.2.1 and flextable package version 0.4.4.
}

\author{Van Cao and members in Biostatistics Group at OUCRU-VN.}

\examples{
data(ae_fix)
mySummary.ae(ae_fix,arm=as.factor(ae_fix$arm),var=ae$AE,SUBJID="SUBJID",name="ae.name")
}
