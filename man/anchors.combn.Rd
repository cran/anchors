% Created :  2002-10-05
% Modified:  $Date: $
% Revision:  $Revision: $
% RCS-ID:    $Id:  $
\name{anchors.combn}
\alias{anchors.combn}
\title{Calculate known minimum or estimated entropy for survey vignettes}
\description{
  Calculate known minimum or estimated entropy for survey vignettes
}
\usage{
   anchors.combn(adata, fdata, type, options)
}
\arguments{
  \item{adata}{object of class anchors.data}
  \item{fdata}{data frame from which adata was built }
  \item{type}{either "B" or "C", specifying nonparametric method}
  \item{options}{object of class anchors.options}
}
\value{A list of class \code{anchors.combn}, containing elements
 \item{vign.rank}{a matrix with columns corresponding to the vignette
    selection, the estimated entropy (if covar is specified), the
  known entropy, the number of cases with intervals for that particular vignette
  selection, and 2J+1 number of categories for the given
  vignettes.}
 \item{N}{the number of observations after list-wise deletion}
 \item{self}{a character string containing the variable name of the
    self-response question}
 \item{vign}{a character vector containing the variable names of the
      vignette questions.  The first element of the vector corresponds
      to vignette 1, the second vignette 2, etc. }
  }
\note{
  Related materials and worked examples are available at
  http://wand.stanford.edu/anchors/
}
  
\author{Jonathan Wand \url{http://wand.stanford.edu} with Dan Hopkins
  and Olivia Lau}

\references{
  Wand, Jonathan; Gary King; and Olivia Lau. (2007) ``Anchors: Software for
  Anchoring Vignettes''. \emph{Journal of Statistical Software}.  Forthcoming.
  copy at http://wand.stanford.edu/research/anchors-jss.pdf

  Wand, Jonathan and Gary King. (2007)
  Anchoring Vignetttes in R: A (different kind of) Vignette
  copy at http://wand.stanford.edu/anchors/doc/anchors.pdf

  Gary King and Jonathan Wand.  "Comparing Incomparable Survey
  Responses: New Tools for Anchoring Vignettes,"
  Political Analysis, 15, 1 (Winter, 2007): Pp. 46-66,
  copy at http://gking.harvard.edu/files/abs/c-abs.shtml.

   }

\seealso{\code{\link{anchors}}}


\examples{

  ## ra <- anchors( fo, data, method="C", combn=TRUE)
  ## summary( ra , combn = "Minimum" )
  ## plot( ra, type="xy" , xy =  c("minimum","intervals") )

}

\keyword{htest}
