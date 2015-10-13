\name{neveGE17}
\alias{neveGE17}
\docType{data}

\title{
  mRNA expression for chromosome 17.
}

\description{Normalized gene expression data of Neve et al. (2006) for chromosome 17.}

\usage{
neveGE17
}

\format{An object of class \code{\link[Biobase]{ExpressionSet}}}

\source{
M. Neve et al. in Gray Lab at LBL. Neve2006: expression and CGH data on breast cancer cell lines. R package version 0.1.10.
}

\references{
	Neve, R.M. et al. (2006). A collection of breast cancer cell lines for the study of functionally distinct cancer subtypes. \emph{Cancer cell}, 10, 515-527.
}


\examples{
data(neveGE17)
dim(neveGE17)
head(fData(neveGE17))

}
