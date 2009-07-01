% Created :  2002-10-05
% Modified:  $Date: 2005/08/10 23:25:49 $
% Revision:  $Revision: 1.4 $
% RCS-ID:    $Id: anchors.plot.barplot.Rd,v 1.4 2005/08/10 23:25:49 jwand Exp $
\name{barplot.anchors.rank}
\alias{barplot.anchors.rank}
\title{Barlot of distribution of non-parametric ranks}
\description{Barplot of distribution of non-parametric ranks}
\usage{
  \method{barplot}{anchors.rank}(height, ...,
                                 ties = c("uniform","minentropy","omit","cpolr"))
}
\arguments{
   \item{height}{anchors.rank object}
   \item{...}{
     may include additional anchors.rank objects that will plotted
     beside x, but these MUST be listed in consecutive order immediately
     following x.
     
     in addition, may list extra arguments for \code{barplot} function}

   \item{ties}{shows distribution of ranks by method for
     allocating ties/interval valued cases to a scalar value

     "uniform": allocates in equal proportions among scalar values

     "omit": drops cases with interval values

     "cpolr": uses censored ordered probit model to allocate ties

     "minentropy": allocates ties to produce minimum entropy
     
   }
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
\seealso{\code{\link{anchors}},\code{\link{plot.anchors.rank}}}
\examples{
## see demo(anchors.freedom3)  and demo(anchors.plot) for examples
}
\keyword{hplot}
