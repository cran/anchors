% Created :  2002-10-05
% Modified:  $Date: 2005/08/10 23:25:49 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: anchors.barplot.order.Rd,v 1.4 2005/08/10 23:25:49 jwand Exp $
\name{barplot.anchors.order}
\alias{barplot.anchors.order}
\title{Barplot of frequency of vignette orderings}
\description{Barplot of frequency of vignette orderings}
\usage{
  \method{barplot}{anchors.order}(height, ..., top=20)
}
\arguments{
  \item{height}{anchors.order object}
  \item{...}{arguments for \code{barplot}}
  \item{top}{Show 'top' most common combinations}
}
\details{

   Each vignette is represented by the integer indexing it's place as
   passed to the function.  For example, let

   fo <- list(vign = cbind(vign1,vign2,vign3))

   then 1=vign1, 2=vign2, 3=vign3. In the case of an anchors.order
   object produced with the option ties="interval", tied variables are
   represented by being included in brackets.  

   z  <- anchors( fo, data, method="order",
                 options=anchors.options(ties="interval"))

  then a row with "\{1,3\},2" indicates that vign1 and vign3 are tied,
  and vign2 has a higher value than both of them.

} \note{
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
\seealso{\code{\link{anchors}},\code{\link{anchors.order}}}
\examples{
## see help(anchors.order)  and demo(anchors.plot) for examples
}
\keyword{hplot}
