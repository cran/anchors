% Created :  2002-10-05
% Modified:  $Date: 2005/08/10 23:25:49 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: anchors.plot.rank.Rd,v 1.4 2005/08/10 23:25:49 jwand Exp $
\name{plot.anchors.rank}
\alias{plot.anchors.rank}
\title{Plot components of anchors.rank objects}
\description{Plot components of anchors.rank objects.  Currently this
  only makes a scatter plot if anchors(...,combn=TRUE).  For plots of
  densities of ranks, see \code{barplot.anchors.rank} }
\usage{
  \method{plot}{anchors.rank}(x, ..., xy)
}
\arguments{
   \item{x}{anchors.rank object}
   \item{...}{for plot}
   \item{xy}{name two columns from anchors.combn to plot (case
     insensitive, and you only need to specify first three letters of
     each name); must be a vector of strings of length 2; see Details below.}
 }
 \details{

   Each observation in the plot is a subset of vignettes, represented by
   their index values as originally passed by the formula (see
   \code{anchors.order} for an example/description).  
   
   To define the axis of the scatter plot, the columns to choose from
   include:

   "estimated": estimated entropy from cplor model

   "minimum": minimum entropy from minimum.entropy calculation

   "interval": number of cases with interval (non-scalar) ranks

   "span" average span of ranks (including scalar cases)

   "max" maximum B or C value for the subset of vignettes

   Choose two of the above, e.g., xy=c("minimum","interval")
   
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
\seealso{
  \code{\link{anchors}},
  \code{\link{anchors.order}}
}
\examples{
## see help(anchors) and demo(anchors.plot) for examples
}
\keyword{hplot}
