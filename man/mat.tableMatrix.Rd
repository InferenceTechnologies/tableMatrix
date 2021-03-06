% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableMatrix.R
\name{mat.tableMatrix}
\alias{mat.tableMatrix}
\alias{mat<-.tableMatrix}
\title{Get or set matrix attribute}
\usage{
\method{mat}{tableMatrix}(obj, matN = NULL, ...)

\method{mat}{tableMatrix}(obj) <- value
}
\arguments{
\item{obj}{\code{tableMatrix} object.}

\item{matN}{Integer. Matrix number in the matrix part list.}

\item{...}{Passed arguments.}

\item{value}{List of matrices.}
}
\value{
Full matrix part of \code{tableMatrix} or a matrix.
}
\description{
\code{tableMatrix} method to get or set matrix part attribute. Matrix part is a
list of matrices.
}
\examples{

data(images8By8)

# Create tableMatrix from images8By8 and images10By10
TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
list(4:ncol(images8By8),4:ncol(images10By10)))

# Full matrix part of TM
mat(TM)

# Matrix part of TM corresponding to matrix type 2
mat(TM, 2)

}
