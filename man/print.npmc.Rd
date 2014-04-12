\name{print.npmc}
\alias{print.npmc}
\title{Print multiple comparisons}
\usage{
\method{print}{npmc}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{npmc}.}

  \item{...}{additional arguments passed to the function.}
}
\value{
the input object is returned silently.
}
\description{
\code{print.npmc} prints pairwise group comparisons.
}
\details{
This function prints pairwise multiple comparisons created
by the \code{\link{npmc}} function.
}
\examples{
states <- data.frame(state.region, state.x77)
results <- npmc(Illiteracy ~ state.region, states)
print(results)
}
\author{
Rob Kabacoff <rkabacoff@statmethods.net>
}

