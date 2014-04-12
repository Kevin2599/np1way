#' @title Nonparametric multiple comparisons
#'
#' @description
#' \code{npmc} computes nonparametric pairwise group comparisons.
#'
#' @details
#' This function computes all pairwise group comparisons using 
#' Wilcoxon Rank Sum tests. Exact tests can be requested when
#' there are no ties on the dependent variable.
#' Probability values are adjusted for mulitple comparisons using the 
#' \code{\link{p.adjust}} function. 
#' 
#' @param formula an object of class formula, relating the dependent variable 
#' to the grouping variable.
#' @param data a data frame containing the variables in the model.
#' @param exact logical. If \code{TRUE}, calculate exact Wilcoxon tests.
#' @param sort logical. If \code{TRUE}, sort groups by median dependent variable values.
#' @param method method for correcting p-values for multiple comparisons.
#' @export
#' @return a list with 7 elements:
#' \item{CALL}{function call}
#' \item{data}{data frame containg the depending and grouping variable}
#' \item{sumstats}{data frame with statistics by group}
#' \item{kw}{results of the Kruskal-Wallis test}
#' \item{method}{method used to adjust p-values}
#' \item{mc}{data frame containing the multiple comparisons}
#' \item{vnames}{variable names} 
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' states <- data.frame(state.region, state.x77)
#' results <- npmc(Illiteracy ~ state.region, states)
#' summary(results)
#' plot(results)
npmc <- function(formula, data, exact=FALSE, sort=TRUE,               
                method=c("holm", "hochberg", "hommel", "bonferroni",      
                         "BH", "BY", "fdr", "none")){
  
  if (missing(formula) || length(formula) != 3L ||                 
        (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
    stop("'formula' is missing or incorrect")
    
  method <- match.arg(method)

  df <- model.frame(formula, data)                            
  y <- df[[1]]
  g <- as.factor(df[[2]])
  vnames <- names(df)
  
  if(sort) g <- reorder(g, y, FUN=median)                           
  groups <- levels(g)
  k <- nlevels(g)
  
  getstats <- function(x)(c(N = length(x), Median = median(x),      
                          MAD = mad(x)))
  sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  
  kw <- kruskal.test(formula, data)                             
  mc <- NULL
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      y1 <- y[g==groups[i]]
      y2 <- y[g==groups[j]] 
      test <- wilcox.test(y1, y2, exact=exact)
      r <- data.frame(Group.1=groups[i], Group.2=groups[j], 
                      W=test$statistic[[1]], p=test$p.value)
      # note the [[]] to return a single number
      mc <- rbind(mc, r)
    }
  }
  mc$p <- p.adjust(mc$p, method=method)
  
  
  data <- data.frame(y, g)                                    
  names(data) <- vnames
  results <- list(CALL = match.call(), 
                  data=data,
                  sumstats=sumstats, kw=kw, 
                  method=method, mc=mc, vnames=vnames)
  class(results) <- c("npmc", "list")
  return(results)
}

