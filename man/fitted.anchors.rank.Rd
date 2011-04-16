% Created :  2002-10-05
% Modified:  $Date: 2006/12/02 05:10:17 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: fitted.anchors.rank.Rd,v 1.4 2006/12/02 05:10:17 olau Exp $
\name{fitted.anchors.rank}
\alias{fitted.anchors.rank}
\title{Fitted values of non-parametric models}
\description{Fitted values of non-parametric models}
\usage{
   \method{fitted}{anchors.rank}(object, ..., ties = c("omit", "uniform", "cpolr",
                 "minentropy"), average = FALSE, unconditional = FALSE)
}
\arguments{
  \item{object}{object of class \code{\link{anchors.rank}}.}
  \item{\dots}{required for S3, but any other options will be ignored.}
  \item{ties}{method for allocating ties/interval valued cases to a
    scalar value}
  \item{average}{a logical value.  See \code{values} below for more details.}
  \item{unconditional}{Set to TRUE if you submit an
    anchors.object AND want the unconditional probabilities returned.
    One case that you would submit a anchors.rank object is
  if you did subsetting for the anchors object but not for the cpolr
  object, and want the intersection of the two objects used for the
  unconditional probabilities.}
}
\value{If \code{average = FALSE}, a matrix of predicted probabilities
  with rows corresponding to observations, and columns corresponding to
  categories.

  If \code{average = TRUE}, the matrix of predicted probabilities
  (conditional or unconditional) is summarized to a vector.
  
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

## Basic usage: see anchors

}

\keyword{models}
