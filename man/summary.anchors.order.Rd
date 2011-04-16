% Created :  2002-10-05
% Modified:  $Date: 2005/08/10 23:25:49 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: anchors.print.order.Rd,v 1.4 2005/08/10 23:25:49 jwand Exp $
\name{summary.anchors.order}
\alias{summary.anchors.order}
\title{Calculate frequency of vignette orderings}
\description{Calculate frequency of vignette orderings}
\usage{
  \method{summary}{anchors.order}(object, top, digits = getOption("digits"),verbose=FALSE, ...)
}
\arguments{
   \item{object}{anchors.order object}
   \item{top}{Show 'top' most common combinations}
   \item{digits}{digits to print}
   \item{verbose}{additional summaries}
   \item{...}{additional arguments}
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
\seealso{\code{\link{anchors}},\code{\link{anchors.order}}}
\examples{
## see example in help(anchors.order)
}
\keyword{hplot}
