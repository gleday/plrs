This is a read-only mirror of the Bioconductor SVN repository. Package Homepage: http://bioconductor.org/packages/devel/bioc/html/plrs.html Bug Reports: https://support.bioconductor.org/p/new/post/?tag_val=plrs.

# plrs

This R package implements the method described in

Leday, G. G., van der Vaart, A. W., van Wieringen, W. N., and van de Wiel, M. A. (2013). [Modeling association between DNA copy number and gene expression with constrained piecewise linear regression splines](http://projecteuclid.org/euclid.aoas/1372338469). The Annals of Applied Statistics, 7(2), 823-845.

## Description

The package implements a flexible class of models to decipher how DNA copy number abnormalities in cancer cells alter the mRNA gene expression level. This class of models aims to reflect the biological mechanism operating between these two molecular levels and help identifying relevant markers.

The statistical framework implemented in plrs allows integrative analysis of DNA copy number and mRNA expression data, which incorporates segmented and called aCGH data. The combined use of segmented and called aCGH data improves model flexibility and interpretability. The form of the relationship is allowed to vary per gene and model interpretation is ameliorated with biologically motivated constraints on the parameters. The package implements methods for model selection, interval estimation and testing the strength of the association.

plrs can also be employed to the joint analysis of DNA copy number and microRNA expression.

## Installation

If you wish to install **plrs** from R (using package [devtools](https://cran.r-project.org/web/packages/devtools/index.html)):

```R
install.packages("devtools")
library(devtools)
install_github("gleday/plrs")
library(plrs)
```

**Remark:** For Mac OS X, it may be necessary to install a recent GNU Fortran compiler beforehand. See [here](http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/) for more explanations.

