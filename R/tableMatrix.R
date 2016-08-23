#
# Inference Technologies
#
# Class tableList, tableMatrix
# 
# 0.70
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
tmName <- list(matN="tm.matN", matRow="tm.matRow", matCols="tm.matCols", allRow="tm.allRow")


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


#' S3 class tableList object
#' 
#' \code{tableList} constructor, creates tableList object from a data.frame or data.table
#' and list of aid data. It serves to wrap meta data and additional data structures together. It acts 
#' like data.table.
#'
#' @param tabData Data.frame or data.table. Descriptors of data.
#' @param aidData Aid List. Can be used to add more information (or structure)
#' @return A tableList object
#' @export
#' @seealso
#'    \code{\link{dim.tableList}}, \code{\link{dimnames.tableList}}
#'    \code{\link{rbind.tableList}} 
#' @examples
#' data(images8By8)
#' dim(images8By8)
#'
#' data(images10By10)
#' dim(images10By10)
#' images10By10AsTable <- data.table::as.data.table(images10By10)
#'
#' tabPart <- images8By8[,1:3]
#' tableList(tabPart, NULL)
#'
#' 
#' tabDataTable <- images10By10AsTable[, .(direction, dimX, dimY)]
#' tableList(tabDataTable, NULL)
#'
#' aidPart <- list("example1", "example2")
#' tableList(tabPart, aidPart)

tableList <- function(tabData, aidData) {

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

#' S3 class tableMatrix object
#' 
#' 
#' @description \code{tableMatrix} constructor, creates tableMatrix object from a list of 
#' data.frames or data.tables. It combines best of data.table (access via bracket 
#' to metadata part) and matrix. It stores dimensions of main part and effectively
#' use this information while using multiple datasets which can have different 
#' dimensions of main data. It also can store additional structures. It is
#' useful for datasets which have 
#' following condition: the main data could be stored as matrix and other 
#' columns as description.
#' @details \code{tableMatrix} is a S3 class which consists of 3 mandatory parts. 
#' \code{tab} (table part) part is used for storing descriptors of data (meta data), 
#' mat (matrix part) for storing main data and matDim - dimensions of matrix part. 
#' \code{mat} is list of matrices. \code{tab} is \code{data.table}. Its first column \code{tm.matN} 
#' tells index of matrix in \code{mat} to which is row connected to. Second column 
#' \code{tm.matRow} is index of row in this matrix. \code{matDim} is \code{data.table}. It
#' has one mandatory column \code{tm.Matn} which is index of matrix in \code{mat} to which 
#' is row connected to. Rest of columns describe dimensions of \code{mat} part.
#' Default print of \code{tableMatrix} is \code{tabData} without \code{tm.matN} and 
#' \code{tm.matRow} columns.
#'
#' @param dataList Data.frame, data.table or list of data.frame (data.table).
#' Data from which is \code{tableMatrix} created. All datasets must have the same meta data
#' columns, matrix parts can be different. 
#' @param tabCol Integer or character vector, list of vectors. Specifies columns 
#' names or indices of meta data part. Name \code{j} or \code{r} can be used when it is list. 
#' \code{j} option means that names or indices are listed, \code{r} option specifies 
#' from to names (indices). By default or when it is vector, \code{j} is used.
#' @param matCol Integer or character vector, list of vectors. Specifies columns
#' names or indices. Name \code{j} or \code{r} can be used. \code{j} option means that names 
#' or indices are listed, \code{r} option specifies from to names (indices). 
#' By default or when it is vector \code{j} is used.
#' @param dims Numeric vector, \code{list} of vectors. Specifies dimensions of matDim.
#' If no names of column atributes are given or dimNames is NULL, columns 
#' \code{dim}+number are generated.Default NULL. 
#' @param dimNames Character vector. Specifies names of columns in matDim part of object.
#' \code{data.table} with column names \code{tmName$matN} (index of matrix) and 
#' \code{tmName$matCols} (number of matrix part columns). Default NULL. 
#' @return A tableMatrix object
#' @export
#' @seealso
#'    \code{\link{getRowRepo.tableMatrix}}, \code{\link{getRowDim.tableMatrix}},
#'    \code{\link{merge.tableMatrix}}, \code{\link{rbind.tableMatrix}}
#' @examples
#'
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10)
#' images10By10AsTable <- data.table::as.data.table(images10By10)
#' 
#' basicTableMatrix <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8))
#' matDim(basicTableMatrix) # show matDim
#' tab(basicTableMatrix) # show descriptor part of tableMatrix
#' head(mat(basicTableMatrix)[[1]]) #show head of main part - matrix
#' tableMatrix(images8By8, c("direction","dimY"), 4:ncol(images8By8))
#' 
#' #Use of data.frame and vectors
#' tableMatrix(images8By8, c("direction","dimX","dimY"), 4:ncol(images8By8), c(8,8))
#' 
#' #User named dimensions
#' dims <- c(8,8)
#' names(dims) <- c("dimX", "dimY")
#' 
#' userDimsTM <- tableMatrix(images8By8, 1:3, 4:ncol(images8By8), dims)
#' matDim(userDimsTM)
#' 
#' # Use of "r" option and list
#' # first 3 columns in tab, rest in mat
#' tableMatrix(images8By8, list(r=c(1,3)), list(r=c(4,ncol(images8By8))))
#' # first 3 columns in tab, rest in mat
#' tableMatrix(images8By8, list(r=1:3), list(r=4:ncol(images8By8)))
#'
#' # Use with "j" option 
#' # first and third column is in tab, fourth and last in mat
#' tableMatrix(images8By8, list(j=c(1,3)), list(j=c(4,ncol(images8By8))))
#' # irst 3 columns in tab, rest in mat
#' tableMatrix(images8By8, list(j=1:3), list(j=4:ncol(images8By8)))
#' 
#' # Use of data.table
#' tableMatrix(images10By10AsTable, 1:3, 4:ncol(images10By10AsTable))
#' 
#' # Data.frame and data.table. Different matrix part - two matrices in mat
#' bothDefaultDims <- tableMatrix(list(images8By8, images10By10AsTable),
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(4:ncol(images8By8),4:ncol(images10By10AsTable)))
#' matDim(bothDefaultDims)
#' length(mat(bothDefaultDims)) # 2 matrices in mat
#' 
#' # Use of named dims
#' bothUserDims <- tableMatrix(list(images8By8, images10By10), list(r=c("direction","dimY"),
#' j=c("direction","dimX","dimY")), list(4:ncol(images8By8),4:ncol(images10By10)),
#' list(c(8,8), c(10,10)))
#' matDim(bothUserDims)
#' length(mat(bothUserDims))
#' 
#' 
#' tableMatrix(list(images8By8, images10By10),
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))),list(c(8,8),c(10,10)),
#' dimNames =c("dimX", "dimY"))
#'
#' 
#' # Same matrix part - only one matrix in mat
#' bothSameMatrixPart <- tableMatrix(list(images8By8, images8By8), 
#' list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(j=4:ncol(images8By8),4:ncol(images8By8)))
#' matDim(bothSameMatrixPart)
#' length(mat(bothSameMatrixPart)) # 1 matrix in mat
tableMatrix <- function(dataList, tabCol, matCol, dims=NULL, dimNames=NULL) {

	obj <- tableMatrixWrap()
	if (missing(dataList)) { return(obj) }

	if (is.data.frame(dataList)||is.data.table(dataList)) { dataList <- list(dataList) }

	if (! is.null(dims)) {
		if(! is.list(dims)) {
			dims <- list(dims)
		}

		if (length(dims) != length(dataList)) {
			stop("length of dims should be the same as number of dataLists")
		}
		
	}

	for (i in 1:length(dataList)) {

		obji <- dataList[[i]]
		if (is.data.frame(obji)) { obji <- as.data.table(obji) }
		if (!is.data.table(obji)) { stop("tableMatrix requires list of data.frames or data.tables") }

		if (! is.null(dims) && length(dims) != length(dataList)) {
			stop("length of dims should be the same as number of data.frames (data.tables)")
		}
		
		addTab <- obji[, colj(obji, geti(tabCol, i)), with=F]
		addTab[,c(tmName$matN, tmName$matRow):=list(1, 1:nrow(addTab))]
		colShiftRef(addTab, c(tmName$matN, tmName$matRow), 1)

		addMat <- as.matrix(obji[, colj(obji, geti(matCol, i)), with=F])


		if (! is.null(dims)) {
			dim <- dims[[i]]
			#generate names for dims
			if (is.null(names(dim)) && is.null(dimNames)) {
				names <- c(1:length(dim))
				names <- paste("dim", names, sep = "")
			} else {
				#names are taken from param
				if (is.null(names(dim))) {
				    names <- dimNames
				} else {
				    names <- names(dim)
				}
			}

			#set matDim
			addMatDim <- setnames(data.table(c(1L)), c(tmName$matN))
			addMatDim[,names := as.list(dim), with=F]
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
# documented parameters
# tab, mat, matDim and aid signature changed to obj,...
# tab.tableMatrix and so on parameters changed to obj, ...
# setters (and .tableMatrix) changed to (obj, value)
# to getRowRepo.tableMatrix, getRow.tM, setRow.tM, getRowDim.tM added par ... 

#' S3 tableMatrix generic to get or set table attribute
#' @param ... Further arguments passed to or from other methods.
#' @param obj Variable name
#' @param value \code{data.table}.
#' @rdname tab
#' @export
tab <- function(obj,...) { UseMethod("tab") }
#' @rdname tab
#' @export
'tab<-' <- function(obj, value) { UseMethod("tab<-") }

#' S3 tableMatrix generic to get or set matrix attribute
#' @param ... Further arguments passed to or from other methods.
#' @param obj Object to be used
#' @param value Matrix.
#' @rdname mat
#' @export
mat <- function(obj,...) { UseMethod("mat") }
#' @rdname mat
#' @export
'mat<-' <- function(obj,value) { UseMethod("mat<-") }

#' S3 tableMatrix generic to get or set matDim attribute
#' @param ... Further arguments passed to or from other methods.
#' @param obj Object to be used
#' @param value Matrix.
#' @rdname matDim
#' @export
matDim <- function(obj,...) { UseMethod("matDim") }
#' @rdname matDim
#' @export
'matDim<-' <- function(obj, value) { UseMethod("matDim<-") }

#' S3 tableMatrix generic to get row repo for matrix attribute
#' @param ... Arguments passed to or from other methods.
#' @rdname getRowRepo
#' @export
getRowRepo <- function(...) { UseMethod("getRowRepo") }

#' S3 tableMatrix generic to get or set row from matrix attribute
#' @param ... Arguments passed to or from other methods.
#' @rdname getRow
#' @export
getRow <- function(...) { UseMethod("getRow") }
#' @rdname getRow
#' @export
setRow <- function(...) { UseMethod("setRow") }

#' S3 tableMatrix generic to get row dim
#' @param ... Arguments passed to or from other methods.
#' @export
getRowDim <- function(...) { UseMethod("getRowDim") }

#' S3 tableMatrix generic to get or set aid attribute
#' @param ... Arguments passed to or from other methods.
#' @param obj Variable name
#' @param value \code{data.table}.
#' @rdname aid
#' @export
aid <- function(obj,...) { UseMethod("aid") }
#' @rdname aid
#' @export
'aid<-' <- function(obj,value) { UseMethod("aid<-") }

#
# Methods Functions
#


#
# Classes Generics Methods
#

#' Get or set table attribute
#'
#' \code{tableList} method to get or set table attribute
#' @param obj \code{dataList}.
#' @param value \code{data.table}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname tab.tableList
#' @export
tab.tableList <- function(obj, ...) {	

	objTab <- obj$tab
	if (!truelength(objTab)) {
		setDT(objTab)
	}
	return(copy(objTab))
}

#' @rdname tab.tableList
#' @export
'tab<-.tableList' <- function(obj, value) {
	
	if (!is.data.table(value)) { stop("data.table object required") }
	obj$tab <- value
	return(obj) 
}

#' Get or set aid attribute
#' 
#' \code{tableList} method to get or set aid attribute.
#' @param obj \code{tableList} object.
#' @param value List. 
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname aid.tableList
#' @export
aid.tableList <- function(obj, ...) {	return(obj$aid) }

#' @rdname aid.tableList
#' @export
'aid<-.tableList' <- function(obj, value) {
	obj$aid <- value
	return(obj) 
}

#' Get or set table attribute
#'
#' Function to get or set table attribute of \code{tableMatrix} object.
#'
#' @param obj \code{tableMatrix} object.
#' @param matN Integer. Index of list in \code{mat} part of \code{tableMatrix}. Default NULL.
#' @param addRow Logical. If specified, column \code{tm.allRow} with original row 
#' number is added.  Default FALSE.
#' @param resetN Logical. Used only when matN is specified. When FALSE
#' matN of returned tab won't be reseted to 1. Default TRUE.
#' @param ... Further arguments passed to or from other methods.
#' 
#' @return Table part of \code{tableMatrix}
#'
#' @examples
#' data(images8By8)
#' data(images10By10)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#' tm2 <- tableMatrix(images15By15, 1:3, 4:ncol(images15By15))
#' matDim(tm)
#' 
#' tab(tm)
#' 
#' #get tab only from where matN is 2
#' tab(tm,2)
#' 
#' #tm.allRow column is added
#' tab(tm,2, TRUE)
#' 
#' #matN stays the same
#' tab(tm,2, TRUE, FALSE)
#' 
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
#' \code{tableMatrix} method to get or set matrix attribute. Mat consists
#' of list of matrices.
#'
#' @param obj \code{tableMatrix} object.
#' @param value Matrix.
#' @param matN Integer. Index of list in mat part of \code{tableMatrix}.
#' If not NULL, only one part is returned. Default NULL.
#' @param ... Further arguments passed to or from other methods.
#' @return Mat part of \code{tableMatrix} or a matrix
#' @rdname mat.tableMatrix
#' 
#' @examples
#' data(images8By8)
#' data(images10By10)
#' data(images15By15)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#' tm2 <- tableMatrix(images15By15, 1:3, 4:ncol(images15By15))
#' mat(tm)
#' 
#' head(mat(tm, 2))
#' 
#' mat(tm) <- mat(tm2) 
#' @export
mat.tableMatrix <- function(obj, matN=NULL, ...) {

	objMat <- obj$mat
	if (!is.null(matN)) {
		objMat <- objMat[[matN]]
	}
	return(objMat) 
}
#' @rdname mat.tableMatrix
#' @export
'mat<-.tableMatrix' <- function(obj, value) {
	
	if (!is.list(value)||!all(sapply(value, is.matrix))) { stop("list containing matrices required") }
	obj$mat <- value
	return(obj) 
}
#' Get or set matDim attribute
#' 
#' \code{tableMatrix} method to get or set matDim attribute
#' @param obj \code{tableMatrix} object.
#' @param matN Integer. Index of list in mat part of \code{tableMatrix}. 
#' Default NULL. When NULL, all rows from matDim are returned.
#' @param resetN Logical. When TRUE, returned matDim has matN=1. Default FALSE.
#' @param ... Further arguments passed to or from other methods.
#' @param value Matrix.
#' @rdname matDim.tableMatrix
#' @examples
#' data(images8By8)
#' data(images10By10)
#' data(images15By15)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(r=c(4,ncol(images8By8)),r=c(4,ncol(images10By10))))
#' tm2 <- tableMatrix(images15By15, 1:3, 4:ncol(images15By15))
#'
#' matDim(tm)
#' 
#' matDim(tm, 2)
#'
#' matDim(tm, 2, TRUE)
#' 
#' matDim(tm) <- matDim(tm2) 
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

#' Method to get a row repo
#' 
#' \code{tableMatrix} method to get row repo (vector of matN and matRow) for the matrix attribute
#' @param obj \code{tableMatrix} object.
#' @param i Integer. Index of row in \code{tab}. Default NULL.
#' @param repo Numeric vector. Vector of 2 elements - matRow and matRow. Default NULL.
#' @param ... Further arguments passed to or from other methods.
#' @return vector of matN and matRow
#' @export
#' @examples
#' data(images8By8)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))))
#' tab(tm)
#' 
#' #1st matrix, 2nd row
#' getRowRepo(tm,2)
#'
#' #2nd matrix, 1st row  
#' getRowRepo(tm, 91)
#'
#' #Same result as previous
#' getRowRepo(tm, repo=c(2,1))
getRowRepo.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	# if (is.null(repo)) { return(as.integer(obj$tab[i,c(tmName$matN,tmName$matRow),with=F])) }
	if (is.null(repo)) { return(c(obj$tab[[tmName$matN]][i], obj$tab[[tmName$matRow]][i])) }
	return(repo)
}

#' Get or set row from the matrix attribute
#' 
#' \code{tableMatrix} method to get or set a row from the matrix attribute
#' @param obj \code{tableMatrix} object.
#' @param value Vector for setting value.
#' @param i Integer. Index of row in tab. Default NULL.
#' @param repo Numeric vector. Vector of 2 elements - matRow and matRow. Default NULL.
#' @param ... Further arguments passed to or from other methods.
#' @rdname getRow.tableMatrix
#' @examples
#' data(images8By8)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(1:3, 1:3),
#' list(4:ncol(images8By8),4:ncol(images10By10)))
#'
#' #Returns 1st row of 2nd matrix in mat
#' row <- getRow(tm, 91)
#'
#' getRow(tm, repo=c(2,1))
#'
#' tm2 <- setRow(tm, rep(2,length(row)),91)
#' getRow(tm2, 91)
#' 
#' @export
getRow.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	return(obj$mat[[repo[1]]][repo[2],]) 
}
#' @rdname getRow.tableMatrix
#' @export
setRow.tableMatrix <- function(obj, value, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	obj$mat[[repo[1]]][repo[2],] <- value
	return(obj) 
}

#' Method to get row dim
#' 
#' \code{tableMatrix} method to get row dim which is stored in matDim.
#' @param obj \code{tableMatrix} object.
#' @param i Integer. Index of row in tab. Default NULL.
#' @param repo Numeric vector. Vector of 2 elements - matRow and matRow. Default NULL.
#' @param ... Further arguments passed to or from other methods.
#' @return integer vector of dimensions
#' @export
#' @examples
#' data(images8By8)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), 
#' list(r=c(1,3), r=c(1,3)),
#' list(r=c(4,ncol(images8By8)),r=c(4,ncol(images10By10))),list(c(8,8),c(10,10)),
#' dimNames =c("dimX", "dimY"))
#' matDim(tm)
#' tab(tm) 
#'
#' getRowDim(tm,1)
#'
#' #2nd matrix in mat, 1st row
#' getRowDim(tm,91)
#'
#' #Same as previous 
#' getRowDim(tm,repo=c(2,1))
#'
getRowDim.tableMatrix <- function(obj, i=NULL, repo=NULL, ...) {	

	repo <- getRowRepo(obj, i, repo)
	return(as.integer(obj$matDim[.(repo[1]), -1, with=F])) 
}

#
# Standard Generics Methods
#

#' Bracket tableList
#' 
#' \code{tableList} method passes data.table parameters to the table attribute.
#' Usage is the same as data.table[].
#' @param x \code{tableList} object.
#' @param ... Sequence of parameters for \code{tableList}.
#' @export
#' @examples
#' data(images8By8)
#' dim(images8By8)
#'
#' tabPart <- images8By8[,1:3]
#' tl <- tableList(tabPart, NULL)
#' tl[direction=="both"]
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

#' Double bracket
#' 
#' tableList method passes data.table parameters to the table attribute
#' @param x \code{tableList} object.
#' @param ... Sequence of parameters for \code{tableList}.
#' @export
'[[.tableList' <- function(x, ...) {

	return(x$tab[[...]])
}

#' Bracket
#' 
#' S3 tableMatrix method to pass data.table parameters to the table attribute.
#' @export
#' @param x \code{tableMatrix}.
#' @param ... Sequence of parameters for \code{tableMatrix}.
#' @examples
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10) 
#' 
#' image8By8TM <- tableMatrix(images8By8, list(1:3), list(4:ncol(images8By8)))
#' image10By10TM <- tableMatrix(images10By10, list(r=c(1,3)), list(j=c(4:ncol(images10By10))))
#'
#' together <- rbind(image8By8TM , image10By10TM)
#' 
#' #where matN is 2
#' together[.(2)]
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
#' Dimensions
#' 
#' \code{tableList} method returns dimensions of the table attribute
#' @param x \code{tableList} object
#' @export
dim.tableList <- function(x) {
	return(dim(x$tab))
}

#' Dimension Names
#' 
#' \code{tableList} method, returns dimension names of the table attribute
#' @param x \code{tableList} object
#' @export
dimnames.tableList <- function(x) {
	return(dimnames(x$tab))
}

#' Print
#' 
#' \code{tableList} method, prints only tab data.
#' @param x \code{tableList} object
#' @param ... Further arguments passed to or from other methods.
#' @export
print.tableList <- function(x, ...) {
	
	print(tab(x))
	return(invisible())
}

#' Print
#' 
#' \code{tableMatrix} method to print. Only tab data without \code{tmName} vars 
#' are printed.
#' @param x \code{tableMatrix} object
#' @param ... Further arguments passed to or from other methods.
#' @export
print.tableMatrix <- function(x, ...) {

	objectNames <- setdiff(colnames(x$tab), tmName)
	print(tab(x)[,c(objectNames), with=F])
	return(invisible())
}

#' Merging tableMatrix
#' 
#' Merging \code{tableMatrix} with data.table or data.frame
#' @param x \code{tableMatrix} object
#' @param y Data.table or data.frame object.
#' @param key Vector of shared column names to be used as key.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @examples
#' data(images8By8)
#' dim(images8By8)
#' data(images10By10)
#' dim(images10By10)
#' 
#' tm <- tableMatrix(images8By8, c("direction","dimX","dimY"), 4:ncol(images8By8), c(8,8))
#' merge(tm, data.frame(direction="down", flag=TRUE), key="direction")
merge.tableMatrix <- function(x, y, key, ...) {

	if (is.data.frame(y)) { y <- as.data.table(y) }
	obj <- copy(x)
	dataObj <- copy(y)
	setkeyv(obj$tab,key)
	setkeyv(dataObj,key)
	return(obj[J(dataObj)])
}

#' Binding tableList
#' 
#' Binding rows of \code{tableList} objects.
#' @param ... List of \code{tableList}. Objects to be bind together. 
#' @param use.names Just as use.names in data.table. If \code{TRUE} items are bound
#' by matching column names. Default \code{TRUE}.
#' @param fill Just as fill in data.table. Used when tab parts are different. Fills
#' missing columns with NAs. If \code{TRUE}, \code{user.names} has to be \code{TRUE}.
#' Default \code{FALSE}.
#' @export
#' @examples
#' data(images8By8)
#' dim(images8By8)
#'
#' data(images10By10)
#' dim(images8By8)
#'
#' tl1 <- tableList(images8By8[,1:3], NULL)
#' tl2 <- tableList(images10By10[,1:3], NULL)
#' tl3 <- tableList(images8By8[,1:2], NULL)
#' 
#' rbind(tl1, tl2)
#'
#' #Different number of columns
#' rbind(tl1, tl3, fill=TRUE)
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
#' S3 method to bind tableMatrix objects by row
#' 
#' Takes a sequence of \code{tableMatrix} arugments and combines them by row. If 
#' matrix parts have same dimensions, it only adds rows in tab and mat part. 
#' Otherwise it will add new matrix to mat part of \code{tableMatrix}.
#'
#' @param ... List of \code{tableMatrix}. Objects to be bind together. Their tab
#' or mat part can be different.
#' @param use.names Just as use.names in data.table. If \code{TRUE} items are bound
#' by matching column names. Default \code{TRUE}.
#' @param fill Just as fill in data.table. Used when tab parts are different. Fills
#' missing columns with NAs. If \code{TRUE}, \code{user.names} has to be \code{TRUE}.
#' Default \code{FALSE}
#' 
#' @return A tableMatrix object
#' @export
#' @examples 
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10) 
#' 
#' image8By8TM <- tableMatrix(images8By8, list(1:3), list(4:ncol(images8By8)))
#' #Only first two columns are used for tab part
#' image8By8TM2 <- tableMatrix(images8By8, list(c(1,2)), list(4:ncol(images8By8)))
#' image10By10TM <- tableMatrix(images10By10, list(r=c(1,3)), list(j=c(4:ncol(images10By10))))
#'
#' #Different matrix part, another matrix created in mat
#' tm <- rbind(image8By8TM , image10By10TM)
#' length(mat(tm))
#' tab(tm)
#'
#' #Same mat and tab part
#' tm2 <- rbind(image8By8TM , image8By8TM)
#' length(mat(tm2))
#' tab(tm2)
#'
#' #Different number/names of columns of tabs, fill=TRUE 
#' rbind(image8By8TM,image8By8TM2, fill=TRUE)
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


#' Copying tableList
#' 
#' Copy of a tableList obj
#' @param obj \code{tableList} object
#' @export
copy.tableList <- function(obj) {

	obj$tab <- tab(obj)
	return(obj)
}

#' Copying tableMatrix
#' 
#' Copy of a tableMatrix object. 
#' @param obj \code{tableList} object
#' @export
copy.tableMatrix <- function(obj) {

	obj$tab <- tab(obj)
	obj$matDim <- matDim(obj)
	return(obj)
}

#
# Functions
#

#' tableList testing
#' 
#' Tests if passed obj is of class tableList
#' @param obj \code{tableList} object
#' @export
is.tableList <- function(obj) {

	if ("tableList"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' Method to test class of object
#' 
#' Tests if passed obj is of class tableMatrix
#' @param obj \code{tableTable} object
#' @export
is.tableMatrix <- function(obj) {

	if ("tableMatrix"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' Images of distributions
#'
#' A dataset containing samples of multivariate normal distributions. 
#' Each row in dataset represents a generated matrix. Dimensions are stored in
#' dimX and dimY. Direction "up" and "down" depends on used covariance matrix.
#' "Both" was created as combination of the previous. 
#'
#' @format A data frame with 90 rows and 67 variables:
#' \itemize{
#'   \item{direction direction of diagonal, factor "up", "down", "both"}
#'   \item{dimX dimension x of image, in pixels}
#'   \item{dimY dimension y of image, in pixels}
#'   \item{pixel1:pixel64 pixel of image, in number}
#' }
"images8By8"

#' Images of distributions
#'
#' A dataset containing samples of multivariate normal distributions. 
#' Each row in dataset represents a generated matrix. Dimensions are stored in
#' dimX and dimY. Direction "up" and "down" depends on used covariance matrix.
#' "Both" was created as combination of the previous. 
#'
#' @format A data frame with 90 rows and 103 variables:
#' \itemize{
#'   \item{direction direction of diagonal, factor "up", "down", "both"}
#'   \item{dimX dimension x of image, in pixels}
#'   \item{dimY dimension y of image, in pixels}
#'   \item{pixel1:pixel100 pixel of image, in number}
#' }
"images10By10"

#' Images of distributions
#'
#' A dataset containing samples of multivariate normal distributions. 
#' Each row in dataset represents a generated matrix. Dimensions are stored in
#' dimX and dimY. Direction "up" and "down" depends on used covariance matrix.
#' "Both" was created as combination of the previous. 
#'
#' @format A data frame with 90 rows and 228 variables:
#' \itemize{
#'   \item{direction direction of diagonal, factor "up", "down", "both"}
#'   \item{dimX dimension x of image, in pixels}
#'   \item{dimY dimension y of image, in pixels}
#'   \item{pixel1:pixel225 pixel of image, in number}
#' }
"images15By15"
