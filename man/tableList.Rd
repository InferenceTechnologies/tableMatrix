% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableMatrix.R
\name{tableList}
\alias{tableList}
\title{tableList constructor}
\usage{
tableList(tabData, aidData = list(), dataType = NULL)
}
\arguments{
\item{tabData}{A data.frame or a data.table.}

\item{aidData}{Aid structures, generally in the form of a list.}

\item{dataType}{Named list. Contains names of groups of attributes, elements are attributes which
belong to the group.}
}
\value{
A \code{tableList} object
}
\description{
\code{tableList} constructor, creates tableList object from a data.frame or a data.table
and from aid data. The goal is to wrap data.table and any additional data structures together. 
\code{tableList} behaves like a data.table object. Groups of attributes can be stored together via 
\code{dataType}.
}
\examples{

data(chickwts)

# Bundle chickwts data.frame together with a linear model
TL <- tableList(chickwts, lm(weight~feed, chickwts))

# tableList behaves like a data.table  
mean(TL[feed=="casein", weight])

# Aid part of the tableList object carries the linear model
aid(TL)

# dataType support
TL <- tableList(chickwts, lm(weight~feed, chickwts), dataType=list(group1="weight", group2="feed"))

}
\seealso{
\code{\link{dim.tableList}}, \code{\link{dimnames.tableList}}
   \code{\link{rbind.tableList}}
}
