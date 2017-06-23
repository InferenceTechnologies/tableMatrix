#
# Inference Technologies 
# http://inferencetech.com
#
# pkg tableMatrix
#
# Class tableList, tableMatrix
# 
# 0.81
# 

#
# Imports
#

#' @import data.table
#' @importFrom stats na.omit
NULL

# without this, there is a warning during check
# data.table variables
utils::globalVariables(c("J", "."))

#
# Constants
#

# tableMatrix names
tmName <- list(matN="tm.matN", matRow="tm.matRow", matCols="tm.matCols", matDim="tm.matDim", allRow="tm.allRow")


#
# Classes Constructors
#

# Wraps tableList components together.
tableListWrap <- function(tab=data.table(), aid=list(), objClass=NULL) {

	obj <- list()
	obj$tab <- tab
	obj$aid <- aid

	if (!is.null(objClass)) {
		class(obj) <- objClass
	} else {
		class(obj) <- "tableList"
	}

	return(obj)
}

#' tableList constructor
#' 
#' \code{tableList} constructor, creates tableList object from a data.frame or a data.table
#' and from aid data. The goal is to wrap data.table and any additional data structures together. 
#' \code{tableList} behaves like a data.table object.
#'
#' @param tabData A data.frame or a data.table.
#' @param aidData Aid structures, generally in the form of a list.
#' 
#' @return A \code{tableList} object
#' 
#' @seealso
#'    \code{\link{dim.tableList}}, \code{\link{dimnames.tableList}}
#'    \code{\link{rbind.tableList}} 
#' 
#' @examples
#' 
#' data(chickwts)
#' 
#' # Bundle chickwts data.frame together with a linear model
#' TL <- tableList(chickwts, lm(weight~feed, chickwts))
#'
#' # tableList behaves like a data.table  
#' mean(TL[feed=="casein", weight])
#'
#' # Aid part of the tableList object carries the linear model
#' aid(TL)
#' 
#' @export
tableList <- function(tabData, aidData=list()) {

	if (missing(tabData)) { return(tableListWrap()) }

	if (is.data.frame(tabData)) { tabData <- as.data.table(tabData) }
	if (!is.data.table(tabData)) { stop("tableList requires data.frame or data.table") }
	
	return(tableListWrap(tabData, aidData))
}

# Wraps tableMatrix components
tableMatrixWrap <- function(tab=data.table(), mat=list(), matDim=data.table(),
	aid=list(), setKey=FALSE, objClass=NULL) {
	
	obj <- list()
	obj$tab <- tab
	obj$mat <- mat
	obj$matDim <- matDim
	obj$aid <- aid

	if (setKey) {
		setkeyv(obj$tab, c(tmName$matN, tmName$matRow))
		setkeyv(obj$matDim, tmName$matN)
	}
	
	if (!is.null(objClass)) {
		class(obj) <- objClass
	} else {
		class(obj) <- c("tableMatrix", "tableList") 
	}

	return(obj)
}

#' tableMatrix constructor
#' 
#' @description \code{tableMatrix} constructor, creates tableMatrix object from a list of 
#' data.frames or data.tables. It is useful for datasets with the following 
#' structure: first set of columns of varying types is intented as meta data,
#' second set of columns of the same type is intended as main data. 
#' \code{tableMatrix} combines strengths of data.table (access via bracket 
#' to the meta data part) and matrix (main data). It also stores dimensions of main data,
#' thus allowing to combine rows of varying lengths into one object. As in \code{tableList},
#' \code{tableMatrix} can carry any additional aid data. 
#'
#' @param dataList Dataset(s) in the form of data.frame or data.table or list of data.frames or data.tables.
#' All datasets must have the same meta data columns, matrix parts can be different. 
#' @param tabCol Integer or character vector or list of these vectors. Specifies column 
#' names or indices of meta data part. For list input, names \code{j} or \code{r} can be used. 
#' List name \code{j} indicates that column names or indices are specified, name \code{r} indicates 
#' range of column names or indicies. By default or when a vector is provided, \code{j} is used.
#' @param matCol Integer or character vector or list of these vectors. Specifies column
#' names or indices of main data part. For list input, names \code{j} or \code{r} can be used
#' as in \code{tabCol}.
#' @param dims Numeric vector or list of vectors. Specifies dimensions for the \code{matDim} part.
#' By default with no dimension details given by user, main data is considered to be vectors
#' with one dimension.
#' @param dimNames Character vector. Specifies dimension names in \code{matDim} for each
#' element of \code{dims} parameter. If not specified these names are generated automatically.
#' @param aidData Aid structures generally in the form of a list.
#' 
#' @details \code{tableMatrix} is a S3 class that consists of 3 parts. 
#' \code{tab} - table part - is used for storing meta data, 
#' \code{mat} - matrix part - for storing main data and \code{matDim} - dimensions part - for 
#' dimensions of main data. 
#' \code{mat} is a list of matrices. \code{tab} is a data.table. In \code{tab} first column \code{tm.matN} 
#' is the matrix number in \code{mat}, second column \code{tm.matRow} is the row in the matrix. 
#' \code{matDim} is \code{data.table}. In \code{matDim} for each matrix number \code{tm.matN} dimensions
#' can be specified with user defined dimensions. 
#' Default print of \code{tableMatrix} is the print of the \code{tab} part without \code{tm.matN} and 
#' \code{tm.matRow} columns.
#' 
#' @return A \code{tableMatrix} object
#' 
#' @seealso
#'    \code{\link{getRowRepo.tableMatrix}}, \code{\link{getRowDim.tableMatrix}},
#'    \code{\link{merge.tableMatrix}}, \code{\link{rbind.tableMatrix}}
#' 
#' @examples
#'
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10)
#' images10By10DT <- data.table::as.data.table(images10By10)
#' 
#' # Generate tableMatrix from data.frame images8By8: use columns 1:3 as meta data and 
#' # columns 4:ncol(images8By8) as main data 
#' TM <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8))
#' matDim(TM) # show matDim
#' tab(TM) # show meta data part of tableMatrix
#' head(mat(TM)[[1]]) # show head of main data part
#' 
#' # Generate tableMatrix from data.frame images8By8: use columns "direction" and "dimY"
#' # as meta data and columns 4:ncol(images8By8) as main data 
#' tableMatrix(images8By8, c("direction","dimY"), 4:ncol(images8By8))
#' 
#' # User defined dimensions with default names
#' TM <- tableMatrix(images8By8, c("direction","dimX","dimY"), 4:ncol(images8By8), c(8,8))
#' matDim(TM)
#' 
#' # User defined dimensions with custom names
#' dims <- c(8,8)
#' names(dims) <- c("dimX", "dimY")
#' 
#' TM <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8), dims)
#' matDim(TM)
#' 
#' # tabCol and matCol list input with "j" option
#' # Column indices: first 3 columns in tab, rest in mat
#' tableMatrix(images8By8, list(j=1:3), list(j=4:ncol(images8By8)))
#' # Column names: columns "direction" and "dimY" in tab, 
#' # columns "pixel1" and "pixel2" in mat
#' tableMatrix(images8By8, list(j=c("direction","dimY")), list(j=c("pixel1","pixel2")))
#' 
#' # tabCol and matCol list input with "r" option
#' # Column indices: first 3 columns in tab, rest in mat
#' tableMatrix(images8By8, list(r=c(1,3)), list(r=c(4,ncol(images8By8))))
#' # Same with column names
#' tableMatrix(images8By8, list(r=c("direction","dimY")), list(r=c("pixel1","pixel100")))
#'
#' # data.table as the start dataset
#' tableMatrix(images10By10DT, 1:3, 4:ncol(images10By10DT))
#' 
#' # data.frame and data.table with different main data parts -> two matrices in mat.
#' # Elements in tabCol and matCol lists correspond to images8By8 and images10By10DT
#' # respectively
#' TM <- tableMatrix(list(images8By8, images10By10DT),
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(4:ncol(images8By8),4:ncol(images10By10DT)))
#' matDim(TM)
#' length(mat(TM)) # 2 matrices in mat
#' 
#' # User defined named dimensions
#' TM <- tableMatrix(list(images8By8, images10By10),
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))),list(c(8,8),c(10,10)),
#' dimNames =c("dimX", "dimY"))
#' matDim(TM)
#' 
#' # Same main data parts -> only one matrix in mat
#' TM <- tableMatrix(list(images8By8, images8By8), 
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(j=4:ncol(images8By8),4:ncol(images8By8)))
#' matDim(TM)
#' length(mat(TM)) # 1 matrix in mat
#' 
#' @export
tableMatrix <- function(dataList, tabCol, matCol, dims=NULL, dimNames=NULL, aidData=list()) {

	obj <- tableMatrixWrap()
	if (missing(dataList)) { return(obj) }

	dataList <- inlist(dataList, is.data.frame(dataList)||is.data.table(dataList))
	tabCol <- inlist(tabCol)
	matCol <- inlist(matCol)
	dims <- inlist(dims)

	for (i in 1:length(dataList)) {

		obji <- dataList[[i]]
		if (is.data.frame(obji)) { obji <- as.data.table(obji) }
		if (!is.data.table(obji)) { stop("tableMatrix requires list of data.frames or data.tables") }

		addTab <- obji[, colj(obji, geti(tabCol, i)), with=F]
		addTab[,c(tmName$matN, tmName$matRow):=list(1L, 1:nrow(addTab))]
		colShiftRef(addTab, c(tmName$matN, tmName$matRow), 1)

		addMat <- as.matrix(obji[, colj(obji, geti(matCol, i)), with=F])

		if (! is.null(dims)) {
			dimi <- geti(dims, i, T)
			#generate names for dims
			if (is.null(names(dimi)) && is.null(dimNames)) {
				dimiNames <- paste0(tmName$matDim, 1:length(dimi))
			} else {
				#names are taken from param
				if (is.null(names(dimi))) {
					if (length(dimNames)!=length(dimi)) { stop("dims and dimNames lengths do not match") }
				    dimiNames <- dimNames
				} else {
				    dimiNames <- names(dimi)
				}
			}

			#set matDim
			addMatDim <- setnames(data.table(1L), tmName$matN)
			addMatDim[,c(dimiNames) := as.list(dimi)]
		} else {
			addMatDim <- setnames(data.table(1L, ncol(addMat)), c(tmName$matN, tmName$matCols))
		}
		obj <- rbind(obj, tableMatrixWrap(addTab, list(addMat), addMatDim, setKey=T))
	}
	setkeyv(obj$tab, c(tmName$matN, tmName$matRow))
	setkeyv(obj$matDim, tmName$matN)
	return(obj)
}

#
# Generics
#

#' S3 tableMatrix generic to get or set table attribute
#' 
#' @param obj Object.
#' @param value data.table
#' @param ... Passed arguments.
#' 
#' @return data.table
#' 
#' @rdname tab
#' 
#' @export
tab <- function(obj,...) { UseMethod("tab") }
#' @rdname tab
#' 
#' @export
'tab<-' <- function(obj, value) { UseMethod("tab<-") }

#' S3 tableMatrix generic to get or set matrix attribute
#' 
#' @param obj Object.
#' @param value List of matrices.
#' @param ... Passed arguments.
#' 
#' @return List of matrices
#' 
#' @rdname mat
#' 
#' @export
mat <- function(obj,...) { UseMethod("mat") }
#' @rdname mat
#' 
#' @export
'mat<-' <- function(obj, value) { UseMethod("mat<-") }

#' S3 tableMatrix generic to get or set matDim attribute
#' 
#' @param obj Object.
#' @param ... Passed arguments.
#' @param value data.table
#' 
#' @return data.table
#' 
#' @rdname matDim
#' 
#' @export
matDim <- function(obj,...) { UseMethod("matDim") }
#' @rdname matDim
#' 
#' @export
'matDim<-' <- function(obj, value) { UseMethod("matDim<-") }

#' S3 tableMatrix generic to get or set aid attribute
#' 
#' @param obj Object.
#' @param value User defined
#' @param ... Passed arguments.
#'
#' @return User aid data.
#' 
#' @rdname aid
#' 
#' @export
aid <- function(obj,...) { UseMethod("aid") }
#' @rdname aid
#' 
#' @export
'aid<-' <- function(obj, value) { UseMethod("aid<-") }

#' S3 tableMatrix generic to get row repo for matrix attribute
#' 
#' @param ... Passed arguments.
#' 
#' @return numeric vector.
#'
#' @rdname getRowRepo
#' 
#' @export
getRowRepo <- function(...) { UseMethod("getRowRepo") }

#' S3 tableMatrix generic to get or set row from matrix attribute
#' 
#' @param ... Passed arguments.
#'
#' @return numeric vector.
#' 
#' @rdname getRow
#' 
#' @export
getRow <- function(...) { UseMethod("getRow") }
#' @rdname getRow
#' 
#' @export
setRow <- function(...) { UseMethod("setRow") }

#' S3 tableMatrix generic to get row dim
#' 
#' @param ... Passed arguments.
#' 
#' @return integer vector.
#'
#' @export
getRowDim <- function(...) { UseMethod("getRowDim") }

#
# Classes Generics Methods
#

#' Get or set table attribute
#'
#' \code{tableList} method to get or set table attribute.
#' 
#' @param obj \code{tableList} object.
#' @param value data.table
#' @param ... Passed arguments.
#'
#' @return Table part.
#' 
#' @rdname tab.tableList
#' 
#' @export
tab.tableList <- function(obj, ...) {	

	objTab <- obj$tab
	if (!truelength(objTab)) {
		setDT(objTab)
	}
	return(copy(objTab))
}

#' @rdname tab.tableList
#' 
#' @export
'tab<-.tableList' <- function(obj, value) {
	
	if (!is.data.table(value)) { stop("data.table object required") }
	obj$tab <- value
	return(obj) 
}

#' Get or set aid attribute
#' 
#' \code{tableList} method to get or set aid attribute.
#' 
#' @param obj \code{tableList} object.
#' @param value User defined. 
#' @param ... Passed arguments.
#'
#' @return Aid part.
#' 
#' @rdname aid.tableList
#' @export
aid.tableList <- function(obj, ...) {	return(obj$aid) }

#' @rdname aid.tableList
#' 
#' @export
'aid<-.tableList' <- function(obj, value) {
	obj$aid <- value
	return(obj) 
}

#' Get or set table attribute
#'
#' \code{tableMatrix} method to get or set table attribute of \code{tableMatrix} object.
#'
#' @param obj \code{tableMatrix} object.
#' @param matN Integer. Matrix number in \code{mat} list.
#' @param addRow Logical. If TRUE column \code{tm.allRow} with row indexes before subsetting
#' is added.
#' @param resetN Logical. Used when matN is specified. When FALSE
#' \code{tm.matN} of returned tab won't be reseted to 1. Default TRUE.
#' @param ... Passed arguments.
#' 
#' @return Full table part of \code{tableMatrix} or a subset of table part.
#'
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#' matDim(TM)
#' 
#' # Table part of TM
#' tab(TM)
#' 
#' # Table part of TM corresponding to matrix type 2
#' tab(TM, 2)
#' 
#' # Add row indexes
#' tab(TM, 2, TRUE)
#' 
#' @export
tab.tableMatrix <- function(obj, matN=NULL, addRow=FALSE, resetN=TRUE, ...) {	

	objTab <- obj$tab
	if (!truelength(objTab)) {
		setDT(objTab)
	}

	if (addRow) {
		objTab <- copy(objTab)
		objTab[,c(tmName$allRow):=.I]		
	}

	if (!is.null(matN)) {
		objTab <- objTab[.(matN)]
		if (resetN) {
			objTab[,c(tmName$matN):=1L]
			setkeyv(objTab, with(tmName, c(matN, matRow)))
		}
	} else {
		if (!addRow) {
			objTab <- copy(objTab)
		}
	}

	return(objTab) 
}

#' Get or set matrix attribute
#' 
#' \code{tableMatrix} method to get or set matrix part attribute. Matrix part is a
#' list of matrices.
#'
#' @param obj \code{tableMatrix} object.
#' @param matN Integer. Matrix number in the matrix part list.
#' @param value List of matrices.
#' @param ... Passed arguments.
#' 
#' @return Full matrix part of \code{tableMatrix} or a matrix.
#' 
#' @rdname mat.tableMatrix
#' 
#' @examples
#' 
#' data(images8By8)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#' 
#' # Full matrix part of TM
#' mat(TM)
#' 
#' # Matrix part of TM corresponding to matrix type 2
#' mat(TM, 2)
#' 
#' @export
mat.tableMatrix <- function(obj, matN=NULL, ...) {

	objMat <- obj$mat
	if (!is.null(matN)) {
		objMat <- objMat[[matN]]
	}
	return(objMat) 
}
#' @rdname mat.tableMatrix
#' 
#' @export
'mat<-.tableMatrix' <- function(obj, value) {
	
	if (!is.list(value)||!all(sapply(value, is.matrix))) { stop("list containing matrices required") }
	obj$mat <- value
	return(obj) 
}

#' Get or set matDim attribute
#'
#' \code{tableMatrix} method to get or set matDim attribute.
#' 
#' @param obj \code{tableMatrix} object.
#' @param matN Integer. Matrix number in the matrix part list.
#' @param resetN Logical. When FALSE \code{tm.matN} of returned \code{matDim} won't be 
#' reseted to 1.
#' @param ... Passed arguments.
#' @param value data.table
#'
#' @return Full dimensions part of \code{tableMatrix} or a subset of dimensions part.
#' 
#' @rdname matDim.tableMatrix
#' 
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(r=c(4,ncol(images8By8)),r=c(4,ncol(images10By10))))
#'
#' # Dimensions part of TM
#' matDim(TM)
#' 
#' # Dimensions part of TM corresponding to matrix type 2
#' matDim(TM, 2)
#'
#' @export
matDim.tableMatrix <- function(obj, matN=NULL, resetN=FALSE, ...) {	

	objMatDim <- obj$matDim
	if (!truelength(objMatDim)) {
		setDT(objMatDim)
	}
	if (!is.null(matN)) {
		objMatDim <- objMatDim[.(matN)]
		if (resetN) {
			objMatDim[,c(tmName$matN):=1L]
		}
	} else {
		objMatDim <- copy(objMatDim)
	}

	return(objMatDim) 
}
#' @rdname matDim.tableMatrix
#' @export
'matDim<-.tableMatrix' <- function(obj, value) {
	obj$matDim <- value
	return(obj) 
}

#' Get row repo
#' 
#' \code{tableMatrix} method to get row repo (vector of matN and matRow) for the matrix attribute
#' 
#' @param obj \code{tableMatrix} object.
#' @param i Integer. Row index in \code{tab}.
#' @param repo Numeric vector. Vector with 2 elements - matN and matRow.
#' @param ... Passed arguments.
#' 
#' @return Vector of matN and matRow.
#' 
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))))
#' tab(TM)
#' 
#' # Row 2 in tab(TM) corresponds to first matrix, second row 
#' getRowRepo(TM, 2)
#'
#' # Row 91 in tab(TM) corresponds to second matrix, first row 
#' getRowRepo(TM, 91)
#'
#' @export
getRowRepo.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	if (is.null(repo)) { return(c(obj$tab[[tmName$matN]][i], obj$tab[[tmName$matRow]][i])) }
	return(repo)
}

#' Get or set row from the matrix attribute
#' 
#' \code{tableMatrix} method to get or set a row from the matrix attribute.
#' 
#' @param obj \code{tableMatrix} object.
#' @param value Vector for setting value.
#' @param i Integer. Row index in \code{tab}.
#' @param repo Numeric vector. Vector with 2 elements - matN and matRow.
#' @param ... Passed arguments.
#'
#' @return Row from the matrix part.
#' 
#' @rdname getRow.tableMatrix
#' 
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#'
#' # Row 91 in tab(TM) corresponds to second matrix, first row 
#' row <- getRow(TM, 91)
#'
#' # Row 91 in tab(TM) corresponds to second matrix, first row 
#' getRow(TM, repo=c(2,1))
#'
#' # Change matrix row corresponding to row 91 in tab(TM) 
#' TM <- setRow(TM, rep(2,length(row)), 91)
#' getRow(TM, 91)
#' 
#' @export
getRow.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	return(obj$mat[[repo[1]]][repo[2],]) 
}
#' @rdname getRow.tableMatrix
#' 
#' @export
setRow.tableMatrix <- function(obj, value, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	obj$mat[[repo[1]]][repo[2],] <- value
	return(obj) 
}

#' Get row dimensions
#' 
#' \code{tableMatrix} method to get row dimensions from matDim atrribute.
#' 
#' @param obj \code{tableMatrix} object.
#' @param i Integer. Row index in \code{tab}.
#' @param repo Numeric vector. Vector with 2 elements - matN and matRow.
#' @param ... Passed arguments.
#' 
#' @return Dimensions corresponding to the row.
#' 
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableMatrix from images8By8 and images10By10
#' TM <- tableMatrix(list(images8By8, images10By10), 
#' list(r=c(1,3), r=c(1,3)),
#' list(r=c(4,ncol(images8By8)),r=c(4,ncol(images10By10))),list(c(8,8),c(10,10)),
#' dimNames =c("dimX", "dimY"))
#' matDim(TM)
#' tab(TM) 
#'
#' # Dimensions corresponding to row 1 in tab(TM)
#' getRowDim(TM, 1)
#'
#' # Dimensions corresponding to row 91 in tab(TM)
#' getRowDim(TM, 91)
#'
#' # Dimensions corresponding to row 1 in second matrix in mat(TM)
#' getRowDim(TM, repo=c(2,1))
#'
#' @export
getRowDim.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	return(as.integer(obj$matDim[.(repo[1]), -1, with=F])) 
}

#
# Standard Generics Methods
#

#' Bracket
#' 
#' \code{tableList} method, passes data.table bracket functionality to the table attribute.
#' Usage is the same as in data.table[] and data.table[] <-.
#' Assigning works only for \code{tab} part and only \code{i} and \code{j} from \code{data.table}
#' is used.
#' 
#' @param x \code{tableList} object.
#' @param i Same as \code{i} in \code{data.table}
#' @param j Same as \code{j} in \code{data.table}
#' @param value Value to be set.
#' @param ... Passed arguments.
#' 
#' @return \code{tableList} or vector.
#'
#' @examples
#' 
#' data(images8By8)
#'
#' # Create tableList from images8By8[,1:3]
#' TL <- tableList(images8By8[,1:3])
#' 
#' # Apply data.table bracket on a tableList object
#' TL[direction=="both"]
#'
#' 
#' TL[2,1] <- "aaa"
#' 
#' # setting row
#' TL[2,] <- list("aaa", 1000, 1000)
#' 
#' # setting column
#' TL[,2] <- 1
#'
#' @export
'[.tableList' <- function(x, ...) {

	## copy first, then bracket 1
	matchCall <- match.call()
	matchCall[[1]] <- quote(`[`)
	assign("brTableListTab",tab(x), envir=parent.frame())
	on.exit(rm("brTableListTab", envir=parent.frame()))
	matchCall[[2]] <- quote(brTableListTab)
	objTab <- eval.parent(matchCall)

	## copy first, then bracket 2, requires envir=parent.frame() parameter
	# objTab <- do.call('[',list(tab(x),...), quote=T, envir=envir)
	## copy after bracket
	# objTab <- copy(x$tab[...])
	
	if (is.null(nrow(objTab))) { return(objTab) }

	x$tab <- objTab
	return(x)
}

#' Bracket
#' @rdname sub-.tableList
#' @export
'[<-.tableList' <- function(x, i, j, value) {
	matchCall <- match.call()
	matchCall[[1]] <- quote(`[<-`)
	assign("brTableListTab",tab(x), envir=parent.frame())
	on.exit(rm("brTableListTab", envir=parent.frame()))
	matchCall[[2]] <- quote(brTableListTab)
	if (! missing(i)) {
		assign("brI",i, envir=parent.frame())
		on.exit(rm("brI", envir=parent.frame()))
		matchCall$i <- quote(brI)
	}
	if (! missing(j)) {
		assign("brJ",j, envir=parent.frame())
		on.exit(rm("brJ", envir=parent.frame()))
		matchCall$j <- quote(brJ)
	}
	assign("brValue",value, envir=parent.frame())
	on.exit(rm("brValue", envir=parent.frame()))
	matchCall$value <- quote(brValue)
	objTab <- eval.parent(matchCall)

	if (is.null(nrow(objTab))) { return(objTab) }

	x$tab <- objTab
	return(x)
}

#' Double bracket
#' 
#' \code{tableList} method, passes double bracket functionality to the table attribute.
#' 
#' @param x \code{tableList} object.
#' @param ... Passed arguments.
#' 
#' @return vector
#'
#' @export
'[[.tableList' <- function(x, ...) {

	return(x$tab[[...]])
}

#' Bracket
#' 
#' \code{tableMatrix} method, passes data.table bracket functionality to the table attribute.
#' Assigning works only for \code{tab} part and only \code{i} and \code{j} from \code{data.table}
#' is used. If \code{j} is numeric, it corresponds to first atribute after \code{tmName} names.
#' 
#' @param x \code{tableMatrix} object.
#' @param i Same as \code{i} in \code{data.table}
#' @param j Same as \code{j} in \code{data.table}
#' @param value Value to be set.
#' @param ... Passed arguments.
#'
#' @return \code{tableMatrix} or vector.
#'
#' @examples
#' 
#' data(images8By8)
#'
#' # Create tableMatrix from images8By8
#' TM <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8))
#'
#' 
#' TM[2,1] <- "aaa"
#'
#' TM[1,"dimX"] <- 1000
#' 
#' # setting row
#' TM[2,] <- list("aaa", 1000, 1000)
#' 
#' # setting column
#' TM[,2] <- 1
#'
#' @export
'[.tableMatrix' <- function(x, ...) {

	## copy first, then bracket 1
	matchCall <- match.call()
	matchCall[[1]] <- quote(`[`)
	assign("brTableMatrixTab",tab(x), envir=parent.frame())
	on.exit(rm("brTableMatrixTab", envir=parent.frame()))
	matchCall[[2]] <- quote(brTableMatrixTab)
	objTab <- eval.parent(matchCall)

	## copy first, then bracket 2, requires envir=parent.frame() parameter
	# objTab <- do.call('[',list(tab(x),...), quote=T, envir=envir)
	## copy after bracket
	# objTab <- copy(x$tab[...])

	if (is.null(nrow(objTab))) { return(objTab) }
	if (!nrow(objTab)) { 
		x$tab <- objTab
		x$mat <- list()
		x$matDim <- data.table()
		return(x)
	}

	mergeNA <- is.na(objTab[[tmName$matN]])
	if (sum(mergeNA)) { objTab <- objTab[!mergeNA] }

	objMat <- mat(x)

	uniqMatN <- sort(unique(objTab[[tmName$matN]]))

	matNIdx <- list()
	for (i in 1:length(uniqMatN)) {
		matN <- uniqMatN[i]
		matNIdx[[i]] <- which(objTab[[tmName$matN]]==matN)
		objMat[[matN]] <- objMat[[matN]][objTab[[tmName$matRow]][matNIdx[[i]]],,drop=F]
		objTab[matNIdx[[i]], c(tmName$matRow):=.I]
	}

	objMatDim <- matDim(x)[.(uniqMatN)]
	
	if (length(uniqMatN)!=length(objMat)) {
		tmMatSort <- list()
		for (i in 1:length(uniqMatN)) {
			matN <- uniqMatN[i]
			objTab[matNIdx[[i]],c(tmName$matN):=i]
			tmMatSort[[i]] <- objMat[[matN]]
			matDimMatNIdx <- which(objMatDim[[tmName$matN]]==matN)
			objMatDim[matDimMatNIdx, c(tmName$matN):=i]
		}
		objMat <- tmMatSort
	}

	x$tab <- objTab
	x$mat <- objMat
	x$matDim <- objMatDim
	setkeyv(x$tab, c(tmName$matN, tmName$matRow))
	setkeyv(x$matDim, tmName$matN)
	return(x)
}

#' Bracket
#' @rdname sub-.tableMatrix
#' @export
'[<-.tableMatrix' <- function(x, i, j, value) {
	matchCall <- match.call()
	if (missing(j)) {
		j<-setdiff(colnames(x$tab), tmName)
		matchCall$j <- j
		names(matchCall)[4:5] <- c("j", "value")
	}
	if (is.numeric(j)) j <- setdiff(colnames(x$tab), tmName)[j]

	matchCall[[1]] <- quote(`[<-`)
	assign("brTableListTab",tab(x), envir=parent.frame())
	on.exit(rm("brTableListTab", envir=parent.frame()))
	matchCall[[2]] <- quote(brTableListTab)
	if (! missing(i)) {
		assign("brI",i, envir=parent.frame())
		on.exit(rm("brI", envir=parent.frame()))
		matchCall$i <- quote(brI)
	}
	assign("brJ",j, envir=parent.frame())
	on.exit(rm("brJ", envir=parent.frame()))
	matchCall$j <- quote(brJ)
	assign("brValue",value, envir=parent.frame())
	on.exit(rm("brValue", envir=parent.frame()))
	matchCall$value <- quote(brValue)
	objTab <- eval.parent(matchCall)

	if (is.null(nrow(objTab))) { return(objTab) }

	x$tab <- objTab
	setkeyv(x$tab, c(tmName$matN, tmName$matRow))
	setkeyv(x$matDim, tmName$matN)
	return(x)
}

#' Dimensions
#' 
#' \code{tableList} method, returns dimensions of the table attribute.
#' 
#' @param x \code{tableList} object.
#' 
#' @return Dimensions of the table part.
#'
#' @export
dim.tableList <- function(x) {
	return(dim(x$tab))
}

#' Dimension names
#' 
#' \code{tableList} method, returns dimension names of the table attribute.
#' 
#' @param x \code{tableList} object.
#'
#' @return Column and row names of the table part.
#'
#' @export
dimnames.tableList <- function(x) {
	return(dimnames(x$tab))
}

#' Print
#' 
#' \code{tableList} method, prints table attribute.
#' 
#' @param x \code{tableList} object.
#' @param ... Passed arguments.
#' 
#' @export
print.tableList <- function(x, ...) {
	
	print(tab(x))
	return(invisible())
}

#' Print
#' 
#' \code{tableMatrix} method, prints table attribute without \code{tmName} columns. 
#' 
#' @param x \code{tableMatrix} object.
#' @param ... Passed arguments.
#' 
#' @export
print.tableMatrix <- function(x, ...) {

	objectNames <- setdiff(colnames(x$tab), tmName)
	print(tab(x)[,c(objectNames), with=F])
	return(invisible())
}

#' Merge
#' 
#' \code{tableMatrix} method, merges \code{tableMatrix} objects with data.table or 
#' data.frame.
#' 
#' @param x \code{tableMatrix} object
#' @param y data.table or data.frame object.
#' @param key Shared columns as merging key.
#' @param ... Passed arguments.
#' 
#' @return \code{tableMatrix} object.
#' 
#' @examples
#' 
#' data(images8By8)
#' 
#' # Create tableMatrix from images8By8
#' TM <- tableMatrix(images8By8, c("direction","dimX","dimY"), 4:ncol(images8By8), c(8,8))
#' 
#' # Merge tableMatrix object with a data.frame
#' merge(TM, data.frame(direction="down", flag=TRUE), key="direction")
#' 
#' @export
merge.tableMatrix <- function(x, y, key, ...) {

	if (is.data.frame(y)) { y <- as.data.table(y) }
	obj <- copy(x)
	dataObj <- copy(y)
	setkeyv(obj$tab,key)
	setkeyv(dataObj,key)
	return(obj[J(dataObj)])
}

#' Combine by rows
#' 
#' \code{tableList} method, binds rows of \code{tableList} objects.
#' 
#' @param ... \code{tableList} objects. 
#' @param use.names Passed to rbind.data.table.
#' @param fill Passed to rbind.data.table.
#' 
#' @return \code{tableList} object.
#'
#' @examples
#' 
#' data(images8By8)
#' data(images10By10)
#'
#' # Create tableList objects from images8By8[,1:3] and images10By10[,1:3]
#' TL1 <- tableList(images8By8[,1:3])
#' TL2 <- tableList(images10By10[,1:3])
#' 
#' # Bindind rows of two tableList objects
#' rbind(TL1, TL2)
#'
#' @export
rbind.tableList <- function(..., use.names=TRUE, fill=FALSE) {

	argsList <- list(...)
	
	emptyIdx <- integer()
	for (argsIdx in 1:length(argsList)) {
		if (!nrow(argsList[[argsIdx]])) { emptyIdx <- c(emptyIdx, argsIdx) }
	}
	argsList[emptyIdx] <- NULL

	if (length(argsList)==0) { return(tableListWrap()) }
	if (length(argsList)==1) { return(argsList[[1]]) }

	obj <- copy(argsList[[1]])

	for (argsIdx in 2:length(argsList)) {
		
		objAdd <- argsList[[argsIdx]]
		if (!is.tableList(objAdd)) { stop("tableList objects required") }

		obj$tab <- rbind(obj$tab, objAdd$tab, use.names=use.names, fill=fill)

	}	

	return(obj)
}

#' Combine by rows
#' 
#' \code{tableMatrix} method, binds rows of \code{tableMatrix} objects. If 
#' matrix parts have the same dimensions, it combines rows in table and matrix parts
#' using data.table and matrix rbind methods respectively. In case when dimensions differ
#' it combines rows in table part and adds new matrix to the matrix part.
#'
#' @param ... \code{tableMatrix} objects.
#' @param use.names Passed to rbind.data.table.
#' @param fill Passed to rbind.data.table.
#' 
#' @return \code{tableMatrix} object.
#' 
#' @examples 
#' 
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10) 
#' 
#' # Create tableMatrix objects from images8By8 and images10By10
#' TM1 <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8))
#' TM2 <- tableMatrix(images10By10, 1:3, 4:ncol(images10By10))
#'
#' # Combining tableMatrix objects with same dimensions
#' TM <- rbind(TM1, TM1)
#' # Combined table part
#' tab(TM)
#' # One matrix in the matrix part
#' length(mat(TM))
#' # One dimension type
#' matDim(TM)
#' 
#' # Combining tableMatrix objects with different dimensions
#' TM <- rbind(TM1, TM2)
#' # Combined table part
#' tab(TM)
#' # Two matrices in the matrix part
#' length(mat(TM))
#' # Two dimension types
#' matDim(TM)
#' 
#' @export
rbind.tableMatrix <- function(..., use.names=TRUE, fill=FALSE) {

	argsList <- list(...)

	emptyIdx <- integer()
	for (argsIdx in 1:length(argsList)) {
		if (!nrow(argsList[[argsIdx]])) { emptyIdx <- c(emptyIdx, argsIdx) }
	}
	argsList[emptyIdx] <- NULL

	if (length(argsList)==0) { return(tableMatrixWrap()) }
	if (length(argsList)==1) { return(argsList[[1]]) }

	obj <- copy(argsList[[1]])

	for (argsIdx in 2:length(argsList)) {

		objAdd <- argsList[[argsIdx]]
		if (!is.tableMatrix(objAdd)) { stop("tableMatrix objects required") }
		
		for (matN in 1:nrow(objAdd$matDim)) {

			addTab <- tab(objAdd, matN)
			addMat <- mat(objAdd, matN)
			addMatDim <- matDim(objAdd, matN)

			setkeyv(obj$matDim, setdiff(colnames(obj$matDim), tmName$matN))
			setkeyv(addMatDim, setdiff(colnames(addMatDim), tmName$matN))
			dimMatch <- obj$matDim[J(addMatDim[,-match(tmName$matN, colnames(addMatDim)),with=F])][[tmName$matN]]

			if (is.na(dimMatch)) {
				matNnext <- as.integer(max(obj$matDim[[tmName$matN]]) + 1)
				addTab[, c(tmName$matN):=matNnext]
				obj$tab <- rbind(obj$tab, addTab)
				obj$mat[[matNnext]] <- addMat
				addMatDim[,c(tmName$matN):=matNnext]
				obj$matDim <- rbind(obj$matDim, addMatDim)
			} else {
				dimMatch <- as.integer(dimMatch)
				addTab[,c(tmName$matN):=dimMatch]
				matNIdx <- which(obj$tab[[tmName$matN]]==dimMatch)
				matRowLast <- max(obj$tab[[tmName$matRow]][matNIdx])
				matRowNext <- matRowLast + addTab[[tmName$matRow]]
				addTab[,c(tmName$matRow):=matRowNext]
				obj$tab <- rbind(obj$tab, addTab, use.names=use.names, fill=fill)
				obj$mat[[dimMatch]] <- rbind(obj$mat[[dimMatch]], addMat)
			}
		}
	}

	setkeyv(obj$tab, c(tmName$matN, tmName$matRow))
	setkeyv(obj$matDim, tmName$matN)
	return(obj)
}

#' Copy
#' 
#' Copy of a \code{tableList} object.
#' 
#' @param obj \code{tableList} object.
#' 
#' @return \code{tableList} object.
#' 
#' @export
copy.tableList <- function(obj) {

	obj$tab <- tab(obj)
	return(obj)
}

#' Copy
#' 
#' Copy of a \code{tableMatrix} object. 
#' 
#' @param obj \code{tableMatrix} object.
#' 
#' @return \code{tableMatrix} object.
#' 
#' @export
copy.tableMatrix <- function(obj) {

	obj$tab <- tab(obj)
	obj$matDim <- matDim(obj)
	return(obj)
}

#
# Functions
#

#' tableList test
#' 
#' Tests if passed object is of class \code{tableList}
#' 
#' @param obj \code{tableList} object
#' 
#' @return logical vector.
#' 
#' @export
is.tableList <- function(obj) {

	if ("tableList"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' tableMatrix test
#' 
#' Tests if passed object is of class \code{tableMatrix}
#' 
#' @param obj \code{tableTable} object
#' 
#' @return logical vector.
#' 
#' @export
is.tableMatrix <- function(obj) {

	if ("tableMatrix"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' Images of distributions
#'
#' Dataset containing samples of multivariate normal distributions. 
#' Each row in the dataset represents a generated matrix. Dimensions are stored in
#' dimX and dimY columns. Directions "up" and "down" correspond to different 
#' covariance matrices. Direction "both" was created as a combination of "up" and "down". 
#'
#' @format Data frame with 90 rows and 67 variables:
#' \itemize{
#'   \item{direction direction of diagonal, factor "up", "down", "both"}
#'   \item{dimX dimension x of image, in pixels}
#'   \item{dimY dimension y of image, in pixels}
#'   \item{pixel1:pixel64 pixel of image, in number}
#' }
"images8By8"

#' Images of distributions
#'
#' Dataset containing samples of multivariate normal distributions. 
#' Each row in the dataset represents a generated matrix. Dimensions are stored in
#' dimX and dimY columns. Directions "up" and "down" correspond to different 
#' covariance matrices. Direction "both" was created as a combination of "up" and "down". 
#'
#' @format Data frame with 90 rows and 103 variables:
#' \itemize{
#'   \item{direction direction of diagonal, factor "up", "down", "both"}
#'   \item{dimX dimension x of image, in pixels}
#'   \item{dimY dimension y of image, in pixels}
#'   \item{pixel1:pixel100 pixel of image, in number}
#' }
"images10By10"