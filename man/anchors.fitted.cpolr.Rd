% Created :  2002-10-05
% Modified:  $Date: 2006/12/02 05:10:17 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: fitted.cpolr.Rd,v 1.4 2006/12/02 05:10:17 olau Exp $
\name{fitted.anchors.cpolr}
\alias{fitted.anchors.cpolr}
\title{Conditional and unconditional prediction for censored ordered probit}
\description{
  Conditional and unconditional prediction for censored ordered
  probit.  Unconditional prediction returns the fitted values (predicted
probabilities) from the \code{\link{cpolr}} object.  Conditional prediction
takes the observed range of the diff-corrected self-response output from
\code{\link{anchors}} and renormalizes the predicted
probabilities for each observation.}

\usage{
   \method{fitted}{anchors.cpolr}(object, average = FALSE, unconditional = FALSE, ...)
}
\arguments{
  \item{object}{anchors.cpolr object}
  \item{average}{a logical value.  See \code{values} below for more details.}
  \item{unconditional}{Set to TRUE if you submit an
    anchors.object AND want the unconditional probabilities returned.
    One case that you would submit a anchors.rank object is
  if you did subsetting for the anchors object but not for the cpolr
  object, and want the intersection of the two objects used for the
  unconditional probabilities.}
  \item{\dots}{required for S3, but any other options will be ignored.}
}
\value{If \code{average = FALSE}, a matrix of predicted probabilities
  with rows corresponding to observations, and columns corresponding to
  categories.

  If \code{average = TRUE}, the matrix of predicted probabilities
  (conditional or unconditional) is summarized to a vector (summed by categories,
  then renormalized to sum to 1).

  If \code{anchors} object has been specified, then each observation is
  renormalized to fall into the range of the diff-corrected
  self-response for that observation.  If there are no ties for a given
  observation, then that observation is a
  vector consisting of (k-1) zeros and 1 one.  If there are ties, then
  the predicted probabilities for that observation are renormalized to
  fall within the diff-corrected range.  

  If \code{anchors} object is omitted, identical to the matrix of predicted
  probabilities from the \code{\link{cpolr}} output.
  
}
\note{ 

  Related materials and worked examples are available at
  http://wand.stanford.edu/anchors/
  
  }
  
\author{Jonathan Wand \url{http://wand.stanford.edu}}

\references{
  Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied
     Statistics with S.}  4th edition.  Springer.

  Wand, Jonathan; Gary King; and Olivia Lau. (2007) ``Anchors: Software for
  Anchoring Vignettes''. \emph{Journal of Statistical Software}.  Forthcoming.
  copy at http://wand.stanford.edu/research/anchors-jss.pdf

  Wand, Jonathan and Gary King. (2007)
  Anchoring Vignetttes in R: A (different kind of) Vignette
  copy at http://wand.stanford.edu/anchors/doc/anchors.pdf

  Gary King and Jonathan Wand.  "Comparing Incomparable Survey
  Responses: New Tools for Anchoring Vignettes," Political Analysis, 15,
  1 (Winter, 2007): Pp. 46-66,
  copy at http://gking.harvard.edu/files/abs/c-abs.shtml.
    
}
\seealso{\code{\link{anchors}}, \code{\link{cpolr}}}
\examples{

## see examples in anchors

}

\keyword{models}
