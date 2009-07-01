% Created :  2002-10-05
% Modified:  $Date: 2005/08/10 23:25:49 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: anchors.plot.rank.Rd,v 1.4 2005/08/10 23:25:49 jwand Exp $
\name{plot.anchors.combn}
\alias{plot.anchors.combn}
\title{Plot results from anchors( method='entropy') }
\description{Plot results from anchors( method='entropy') }
\usage{
  \method{plot}{anchors.combn}(x, ... , xy )
}
\arguments{
   \item{x}{anchors.combn object}
   \item{...}{additional options for plot}
   \item{xy}{name two columns from anchors.combn to plot (case
     insensitive, and you only need to specify first three letters of
     each name); must be a      vector of strings of length 2.}
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

}
\seealso{\code{\link{anchors}},\code{\link{anchors.combn}}}
\examples{
## see help(anchors.combn) and demo(anchors.plot) for examples
}
\keyword{hplot}
