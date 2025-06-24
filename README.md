**Note:** plrs is no longer on Bioconductor and is maintained only in this repository.

# plrs

Joint analysis of DNA copy number and mRNA expression data.

## Description

plrs implements a flexible class of models to decipher how DNA
copy number abnormalities in cancer cells alter the mRNA gene expression level.
This modeling framework aims to reflect the biological mechanism operating
between these two molecular levels and to help identifying relevant markers.

The method described in

Leday, G. G., van der Vaart, A. W., van Wieringen, W. N., and van de Wiel, M. A. (2013). [Modeling association between DNA copy number and gene expression with constrained piecewise linear regression splines](http://projecteuclid.org/euclid.aoas/1372338469). The Annals of Applied Statistics, 7(2), 823-845.

## Features

plrs:

- enables the integration of *array comparative genomic hybridization*
(aCGH) data -- both segmented and called -- with mRNA expression data.

- improves model flexibility and interpretability with the combined use of
segmented and called aCGH data, as well as with biologically motivated
constraints on the parameters.

- allows the form of the relationship to vary per gene.

- can also be used for the joint analysis of DNA copy number and
microRNA expression.

## Installation

If you wish to install plrs from R (using package [devtools](https://cran.r-project.org/web/packages/devtools/index.html)):

```R
install.packages("devtools")
library(devtools)
install_github("gleday/plrs")
library(plrs)
```

**Remark:** For Mac OS X, it may be necessary to install a recent GNU
Fortran compiler beforehand. See [here](http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/) for more explanations.

