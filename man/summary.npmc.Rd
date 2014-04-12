\name{summary.npmc}
\alias{summary.npmc}
\title{Summarize multiple comparisons}
\usage{
\method{summary}{npmc}(object, ...)
}
\arguments{
  \item{object}{an object of class \code{npmc}.}

  \item{...}{additional parameters.}
}
\value{
the input object is returned silently.
}
\description{
\code{summary.npmc} summarizes pairwise group comparisons.
}
\details{
This function prints a summary of pairwise multiple
comparisons created by the \code{\link{npmc}} function.
}
\examples{
states <- data.frame(state.region, state.x77)
results <- npmc(Illiteracy ~ state.region, states)
summary(results)
}
\author{
Rob Kabacoff <rkabacoff@statmethods.net>
}

