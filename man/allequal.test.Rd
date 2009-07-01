\name{allequal.test}
\alias{allequal.test}
\title{all.equal with expected outcome test} 
\description{
 Wrapper for all.equal, with ability to specify expected outcome. 
}
\usage{
allequal.test(target,current,expect=TRUE)
}
\arguments{
  \item{target}{R object}
  \item{current}{R object}
  \item{expect}{logical, expected result of all.equal(x,y)}
}
\value{stop() if all.equal(x,y) != expect, else returns expect }  
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

\examples{

a <- c(1:5)
b <- a+1

allequal.test(a,a,expect=TRUE)
allequal.test(a,b,expect=FALSE)
## these throw stop() but here we wrap in try() for example usage
try(allequal.test(a,a,expect=FALSE))
try(allequal.test(a,b,expect=TRUE))

}
\keyword{utilities}
