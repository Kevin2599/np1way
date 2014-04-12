\name{npmc}
\alias{npmc}
\title{Nonparametric multiple comparisons}
\usage{
npmc(formula, data, exact = FALSE, sort = TRUE, method = c("holm",
  "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"))
}
\arguments{
  \item{formula}{an object of class formula, relating the
  dependent variable to the grouping variable.}

  \item{data}{a data frame containing the variables in the
  model.}

  \item{exact}{logical. If \code{TRUE}, calculate exact
  Wilcoxon tests.}

  \item{sort}{logical. If \code{TRUE}, sort groups by
  median dependent variable values.}

  \item{method}{method for correcting p-values for multiple
  comparisons.}
}
\value{
a list with 7 elements: \item{CALL}{function call}
\item{data}{data frame containg the depending and grouping
variable} \item{sumstats}{data frame with statistics by
group} \item{kw}{results of the Kruskal-Wallis test}
\item{method}{method used to adjust p-values}
\item{mc}{data frame containing the multiple comparisons}
\item{vnames}{variable names}
}
\description{
\code{npmc} computes nonparametric pairwise group
comparisons.
}
\details{
This function computes all pairwise group comparisons using
Wilcoxon Rank Sum tests. Exact tests can be requested when
there are no ties on the dependent variable. Probability
values are adjusted for mulitple comparisons using the
\code{\link{p.adjust}} function.
}
\examples{
states <- data.frame(state.region, state.x77)
results <- npmc(Illiteracy ~ state.region, states)
summary(results)
plot(results)
}
\author{
Rob Kabacoff <rkabacoff@statmethods.net>
}

