% Created :  2002-10-05
% Modified:  $Date: 2004/10/17 20:09:46 $
% Revision:  $Revision: 1.10 $
% RCS-ID:    $Id: chopit.Rd,v 1.10 2004/10/17 20:09:46 jwand Exp $
\name{anchors.options}
\alias{anchors.options}
\title{Set or query anchors() parameters}

\description{ 'anchors.options' can be used to set or query parameters passed to
  anchors functions.  Parameters can be set by specifying them as
  arguments to 'anchors.options' in 'tag = value' form, and will return an
  updated list of parameters.
}

\usage{
  anchors.options(...)
}

\arguments{
  \item{...}{arguments in 'tag = value' form.
    The tags must come from the anchors parameters described
    below.}

}
\section{anchors.options()}{

  Options specific to each method are described in their respective help
  pages. 

  Options generic to all functions are:
  
    \describe{  
      \item{verbose     }{ FALSE, if TRUE more detailed run-type printing}
      \item{silence     }{ FALSE, if TRUE turns off even some standard printing}
      \item{debug       }{ 0, if >0 show gory details for
	debugging. Caution: high numbers can produce very high volume output.} 
    }

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
  \code{\link{chopit}},
  \code{\link{anchors.data}},
  \code{\link{anchors.order}}
}

\examples{

data(mexchn)
fo <- list(self = xsayself ~ 1,
           vign = xsay3    ~ 1,
           tau  =           ~1 )
out1  <- chopit( fo, mexchn, options= anchors.options( vign.var = "hetero", normalize="hilo" ))
summary(out1)


}

\keyword{models}
