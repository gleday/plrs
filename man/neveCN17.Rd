\name{neveCN17}
\alias{neveCN17}
\docType{data}

\title{
  Copy number for chromosome 17.
}

\description{Preprocessed copy number data of Neve et al. (2006) for chromosome 17.}

\usage{
neveCN17
}

\format{An object of class \code{\link[CGHbase]{cghCall}}}

\source{
M. Neve et al. in Gray Lab at LBL. Neve2006: expression and CGH data on breast cancer cell lines. R package version 0.1.10.
}

\references{
	Neve, R.M. et al. (2006). A collection of breast cancer cell lines for the study of functionally distinct cancer subtypes. \emph{Cancer cell}, 10, 515-527.
}


\examples{
data(neveCN17)
dim(neveCN17)
head(fData(neveCN17))

}
