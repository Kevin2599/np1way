\name{plot.npmc}
\alias{plot.npmc}
\title{Plot nonparametric multiple comparisons}
\usage{
\method{plot}{npmc}(x, prob = 0.05, ...)
}
\arguments{
  \item{x}{an object of class \code{npmc}.}

  \item{prob}{significance level.}

  \item{...}{additional arguments passed to the
  \code{\link{boxplot}} function.}
}
\description{
\code{plot.npmc} plots pairwise group comparisons.
}
\details{
This function plots pairwise multiple comparisons created
by the \code{\link{npmc}} function.
}
\examples{
states <- data.frame(state.region, state.x77)
results <- npmc(Illiteracy ~ state.region, states)
plot(results, col="red", main="Multiple Comparisons",
     xlab="US Region", ylab="Illiteracy Rates")
}
\author{
Rob Kabacoff <rkabacoff@statmethods.net>
}

