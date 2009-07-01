% Created :  2002-10-05
% Modified:  $Date: 2004/07/07 04:08:40 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: summary.chopit.Rd,v 1.4 2004/07/07 04:08:40 jwand Exp $
\name{summary.anchors.chopit}
\alias{summary.anchors.chopit}
\title{Summary of CHOPIT Analysis}
\description{
  Prints estimates and -log-likelihood from the out of CHOPIT model.
}
\usage{
 \method{summary}{anchors.chopit}( object, ..., digits = 4 )
}
\arguments{
  \item{object}{ The object of class "anchors.chopit", as returned by
    \code{\link{anchors}}}
  \item{...}{additional arguments affecting the summary produced}
  \item{digits}{Number of digits you would like to have printed, using
    \code{\link{round}} function.}
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
  
  King, Gary; Christopher J.L. Murray; Joshua A. Salomon; and Ajay
  Tandon. "Enhancing the Validity and Cross-cultural Comparability of
  Survey Research," American Political Science Review, Vol. 98, No. 1
  (February, 2004): 191-207,
  copy at http://gking.harvard.edu/files/abs/vign-abs.shtml
}

\seealso{\code{\link{anchors}},\code{\link{chopit}}}
\examples{
## see help(anchors)
}
\keyword{models}
