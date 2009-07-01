% Created :  2002-10-05
% Modified:  $Date: $
% Revision:  $Revision: $
% RCS-ID:    $Id: $
\name{summary.anchors.rank}
\alias{summary.anchors.rank}
\title{Summary of non-parameteric anchors analysis}
\description{
  Summary of non-parameteric analysis of a set of vignettes and a self-assessment.
}
\usage{
 \method{summary}{anchors.rank}( object, ..., ties = c("omit", "uniform", "cpolr",
                 "minentropy"), combn = TRUE, digits = 3)
}
\arguments{
  \item{object}{ The object of class "anchors.rank", as returned by
    \code{\link{anchors}}}
  \item{...}{additional arguments affecting the summary produced.}
  \item{ties}{(optional) shows distribution of ranks by method for allocating ties/interval valued
    cases to a scalar value; can be one or more of the following:

     "uniform": allocates in equal proportions among scalar values

     "omit": drops cases with interval values

     "cpolr": uses censored ordered probit model to allocate ties

     "minentropy": allocates ties to produce minimum entropy

  }
  \item{combn}{print summary of combn if avaiable in anchors.rank object}
  \item{digits}{sig digits to print}
}
\note{
  Related materials and worked examples are available at
  http://wand.stanford.edu/anchors/
}
\author{Jonathan Wand \url{http://wand.stanford.edu}}
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

  King, Gary; Christopher J.L. Murray; Joshua A. Salomon; and Ajay
  Tandon. "Enhancing the Validity and Cross-cultural Comparability of
  Survey Research," American Political Science Review, Vol. 98, No. 1
  (February, 2004): 191-207,
  copy at http://gking.harvard.edu/files/abs/vign-abs.shtml

}
\seealso{\code{\link{anchors}}}
\examples{
## see anchors
}
\keyword{models}
