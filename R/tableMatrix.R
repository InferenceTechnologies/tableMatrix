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
NULL


#
# Constants
#

# tableMatrix names
tmName <- list(matN="tm.matN", matRow="tm.matRow", matCols="tm.matCols", allRow="tm.allRow")

#
# Classes Constructors
#

# Wraps tableList components
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


#' Creates tableList object
#' 
#' \code{tableList} constructor, creates tableList obj from a data.frame or data.table and from a aid list
#' @param tabData data.frame or data.table
#' @param aidData Aid list data
#' @return A tableList object
#' @export 
#' @examples
#' 
#' 
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
#' data.frames or data.tables. It combines features of data.table and matrix. 
#' The result is faster access to data. It is useful for datasets which have 
#' following condition: the main data could be stored as matrix and other 
#' columns are for description. 
#' @details \code{tableMatrix} is a S3 class which consists of 3 mandatory parts. 
#' Tab (table part) part is used for storing descriptors of data, mat (matrix part) 
#' for storing main data and matDim - dimensions of matrix part. 
#' Mat is list of matrices. Tab is \code{data.table}. Its first column, \code{tm.matN} 
#' tells index of matrix in mat to which is row connected to. Second column 
#' \code{tm.matRow} is index of row in this matrix. MatDim is \code{data.table}. It
#' has one mandatory column \code{tm.Matn} which is index of matrix in mat to which 
#' is row connected to.
#'
#' @param dataList Data.frame, data.table or list of data.frame (data.table).
#' Data from which is \code{tableMatrix} created. All datasets must have the same descriptor
#' columns, matrix parts can be different. 
#' @param tabCol Integer or character vector, list of vectors. Specifies columns 
#' names or indices of description part. Name \code{j} or \code{r} can be used when it is list. 
#' \code{j} option means that names or indices are listed, \code{r} option specifies 
#' from to names (indices). By default or if it is vector, \code{j} is used.
#' @param matCol Integer or character vector, list of vectors. Specifies columns
#' names or indices. Name \code{j} or \code{r} can be used. \code{j} option means that names 
#' or indices are listed, \code{r} option specifies from to names (indices). 
#' By default or if it is vector \code{j} is used.
#' @param dims Numeric vector, \code{list} of vectors. Specifies dimensions of matDim.
#' If no names of column atributes are given, colums \code{dim}+number are generated. By default is matDim 
#' \code{data.table} with column names \code{tmName$matN} (index of matrix) and 
#' \code{tmName$matCols} (number of matrix part columns) .
#' @return A tableMatrix object
#' @export
#' @examples
#' data(images8By8)
#' dim(images8By8)
#'
#' data(images10By10)
#' dim(images10By10)
#' images10By10AsTable <- as.data.table(images10By10) 
#'
#' basicTableMatrix <- tableMatrix(images8By8, c(1:3), c(4:ncol(images8By8)))
#' matDim(basicTableMatrix) # show matDim
#' tab(basicTableMatrix) # show descriptor part of tableMatrix
#' head(mat(basicTableMatrix)[[1]]) #show head of main part - matrix
#' tableMatrix(images8By8, c("direction","dimY"), c(4:ncol(images8By8)))
#' 
#' #Use one data.frame, vector to use as data descriptors and another vector to be used as matrix data, matDim is 8x8 
#' tableMatrix(images8By8, c("direction","dimX","dimY"), c(4:ncol(images8By8)), c(8,8))
#' tableMatrix(images8By8, c(1,2,3), c(4:ncol(images8By8)), c(8,8)) 
#' tableMatrix(images8By8, c(1:3), c(4:ncol(images8By8)), c(8,8))
#' tableMatrix(images8By8, 1:3, 4:ncol(images8By8), c(8,8))
#'
#' #Use named dims
#' dims <- c(8,8)
#' names(dims) <- c("dimX", "dimY")
#' namedDimsTM <- tableMatrix(images8By8, c(1:3), c(4:ncol(images8By8)), dims)
#' matDim(namedDimsTM)
#'
#' #Use one data.frame, list with one vector as data descriptors with "r" option, list with 1 vector as part for matrix "r" option 
#' # first three columns in tab, rest in mat
#' tableMatrix(images8By8, list(r=c(1,3)), list(r=c(4,ncol(images8By8)))) 
#' # first three columns in tab, rest in mat 
#' tableMatrix(images8By8, list(r=c(1:3)), list(r=c(4:ncol(images8By8)))) 
#'
#' #Use one data.frame, list with one vector as data descriptors with "j" option, list with 1 vector as part for matrix "r" option 
#' # first and third column is in tab, fourth and last in mat
#' tableMatrix(images8By8, list(j=c(1,3)), list(j=c(4,ncol(images8By8)))) 
#' # first three columns in tab, rest in mat 
#' tableMatrix(images8By8, list(j=c(1:3)), list(j=c(4:ncol(images8By8)))) 
#'
#' #Use data.table as data, first 3 columns as data descriptors, rest as part for matrix
#' tableMatrix(images10By10AsTable, c(1:3), (4:ncol(images10By10AsTable)))
#'
#' #Combination of data.frame and data.table. Each dataset has different matrix part so two matrices are created
#' both <- tableMatrix(list(images8By8, images10By10), list(r=c("direction","dimY"),
#' j=c("direction","dimX","dimY")), list(c(4:ncol(images8By8)),c(4:ncol(images10By10))),
#' list(c(8,8), c(10,10)))
#' matDim(both)
#' length(mat(both)) # number of matrix parts
#'
#' bothWithoutSpecDims <- tableMatrix(list(images8By8, images10By10), 
#'   list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#'   list(c(4:ncol(images8By8)),c(4:ncol(images10By10))))
#' matDim(bothWithoutSpecDims)
#' length(mat(bothWithoutSpecDims)) # number of matrix parts - 2
#'
#' #Combination of two data.frames which have the same matrix part. Only one matrix in mat is created.
#' bothSameMatrixPart <- tableMatrix(list(images8By8, images8By8), list(r=c("direction","dimY"), j=c("direction","dimX","dimY")),
#' list(j=c(4:ncol(images8By8)),c(4:ncol(images8By8))))
#' matDim(bothSameMatrixPart)
#' length(mat(bothSameMatrixPart)) # number of matrix parts - only 1
#'

tableMatrix <- function(dataList, tabCol, matCol, dims=NULL) {

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
			if (is.null(names(dim))) {
				names <- c(1:length(dim))
				names <- paste("dim", names, sep = "")
			} else {
				names <- names(dim)
			}

			#set matDim
			addMatDim <- setnames(data.table(c(1)), c(tmName$matN))
			addMatDim[,names := as.list(dim), with=F]
		} else {
			addMatDim <- setnames(data.table(1, ncol(addMat)), c(tmName$matN, tmName$matCols))
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

# S3 tableMatrix generic to get or set table attribute
#' @export
tab <- function(...) { UseMethod("tab") }
#' @export
'tab<-' <- function(...) { UseMethod("tab<-") }

# S3 tableMatrix generic to get or set matrix attribute
#' @export
mat <- function(...) { UseMethod("mat") }
#' @export
'mat<-' <- function(...) { UseMethod("mat<-") }

# S3 tableMatrix generic to get or set matDim attribute
#' @export
matDim <- function(...) { UseMethod("matDim") }
#' @export
'matDim<-' <- function(...) { UseMethod("matDim<-") }

# S3 tableMatrix generic to get row repo for matrix attribute
#' @export
getRowRepo <- function(...) { UseMethod("getRowRepo") }

# S3 tableMatrix generic to get or set row from matrix attribute
#' @export
getRow <- function(...) { UseMethod("getRow") }
#' @export
setRow <- function(...) { UseMethod("setRow") }

# S3 tableMatrix generic to get row dim
#' @export
getRowDim <- function(...) { UseMethod("getRowDim") }

# S3 tableMatrix generic to get or set aid attribute
#' @export
aid <- function(...) { UseMethod("aid") }
#' @export
'aid<-' <- function(...) { UseMethod("aid<-") }

#
# Methods Functions
#


#
# Classes Generics Methods
#

#' Get or set table attribute
#'
#' tableList method to get or set table attribute
#' @rdname tab.tableList
#' @export
tab.tableList <- function(obj) {	

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
#' tableList method to get or set aid attribute
#' @rdname aid.tableList
#' @export
aid.tableList <- function(obj) {	return(obj$aid) }
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
#' @param matN index of list in mat part of \code{tableMatrix}. Default NULL.
#' @param addRow Boolean. When specified, column \code{tm.allRow} with row 
#' number is added.  Default FALSE.
#' @param resetN Integer. Used only when matN is specified. When FALSE
#' matN of returned tab won't be reseted to 1. Default NULL.
#' @examples
#' data(images8By8)
#' data(images10By10)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(c(1:3), c(1:3)),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))))
#' tm2 <- tableMatrix(images15By15, 1:3, 4:ncol(images15By15))
#' matDim(tm)
#' 
#' tab(tm)
#' 
#' #get tab only from where matN is 1
#' tab(tm,2)
#' 
#' #tm.allRow column is added
#' tab(tm,2, TRUE)
#' 
#' #matN stays the same
#' tab(tm,2, TRUE, FALSE)
#' 
#' \dontrun{
#' tm[.(1)]$tab <- tab(tm2)
#' } 
#' 
#' @export
tab.tableMatrix <- function(obj, matN=NULL, addRow=FALSE, resetN=NULL) {	

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
		if (is.null(resetN)) { resetN <- TRUE }
		if (resetN) {
			objTab[,c(tmName$matN):=1]
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
#' @param matN index of list in mat part of \code{tableMatrix}.
#' When NULL, whole list of  Default NULL.
#' @rdname mat.tableMatrix
#' 
#' @examples
#' data(images8By8)
#' data(images10By10)
#' data(images10By10)
#'
#' tm <- tableMatrix(list(images8By8, images10By10), list(c(1:3), c(1:3)),
#' list(c(4:ncol(images8By8)),c(4:ncol(images10By10))))
#' tm2 <- tableMatrix(images15By15, 1:3, 4:ncol(images15By15))
#' mat(tm)
#' 
#' 
#' head(mat(tm, 2))
#' 
#' mat(tm) <- mat(tm2) 
#' @export
mat.tableMatrix <- function(obj, matN=NULL) {

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
#' tableMatrix method to get or set matDim attribute
#' @rdname matDim.tableMatrix
#' @export
matDim.tableMatrix <- function(obj, matN=NULL, resetN=FALSE) {	

	objMatDim <- obj$matDim
	if (!truelength(objMatDim)) {
		setDT(objMatDim)
	}
	if (!is.null(matN)) {
		objMatDim <- objMatDim[.(matN)]
		if (resetN) {
			objMatDim[,c(tmName$matN):=1]
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

#' Get a row repo
#' 
#' tableMatrix method to get row repo for the matrix attribute
#' @export
getRowRepo.tableMatrix <- function(obj, i=NULL, repo=NULL) {	

	# if (is.null(repo)) { return(as.integer(obj$tab[i,c(tmName$matN,tmName$matRow),with=F])) }
	if (is.null(repo)) { return(c(obj$tab[[tmName$matN]][i], obj$tab[[tmName$matRow]][i])) }
	return(repo)
}

#' Get or set row from the matrix attribute
#' 
#' tableMatrix method to get or set a row from the matrix attribute
#' @rdname getRow.tableMatrix
#' @export
getRow.tableMatrix <- function(obj, i=NULL, repo=NULL) {	

	repo <- getRowRepo(obj, i, repo)
	return(obj$mat[[repo[1]]][repo[2],]) 
}
#' @rdname getRow.tableMatrix
#' @export
setRow.tableMatrix <- function(obj, value, i=NULL, repo=NULL) {	

	repo <- getRowRepo(obj, i, repo)
	obj$mat[[repo[1]]][repo[2],] <- value
	return(obj) 
}

#' Get row dim
#' 
#' tableMatrix method to get row dim
#' @export
getRowDim.tableMatrix <- function(obj, i=NULL, repo=NULL) {	

	repo <- getRowRepo(obj, i, repo)
	return(as.integer(obj$matDim[.(repo[1]), -1, with=F])) 
}

#
# Standard Generics Methods
#

#' Bracket
#' 
#' tableList method passes data.table parameters to the table attribute
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

#' Double bracket
#' 
#' tableList method passes data.table parameters to the table attribute
#' @export
'[[.tableList' <- function(x, ...) {

	return(x$tab[[...]])
}

#' Bracket
#' 
#' S3 tableMatrix method to pass data.table parameters to the table attribute.
#' @export
#' @param x \code{table.matrix}.
#' @param ... Sequence of parameters for \code{table.matrix}.
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
#' tableList method returns dimensions of the table attribute
#' @export
dim.tableList <- function(obj) {
	return(dim(obj$tab))
}

#' Dimension Names
#' 
#' tableList method returns dimension names of the table attribute
#' @export
dimnames.tableList <- function(obj) {
	return(dimnames(obj$tab))
}

#' Print
#' 
#' tableList method prints tableList obj
#' @export
print.tableList <- function(obj) {
	
	print(tab(obj))
	return(invisible())
}

#' Print
#' 
#' tableMatrix method prints tableMatrix obj
#' @export
print.tableMatrix <- function(obj) {

	objectNames <- setdiff(colnames(obj$tab), tmName)
	print(tab(obj)[,c(objectNames), with=F])
	return(invisible())
}

#' Merging tableMatrix
#' 
#' Merging tableMatrix with data.table or data.frame
#' @export
merge.tableMatrix <- function(obj, dataObj, key) {

	if (is.data.frame(dataObj)) { dataObj <- as.data.table(dataObj) }
	obj <- copy(obj)
	dataObj <- copy(dataObj)
	setkeyv(obj$tab,key)
	setkeyv(dataObj,key)
	return(obj[J(dataObj)])
}

#' Binding tableList
#' 
#' Binding rows of tableList objects
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

#' S3 method to bind tableMatrix objects by row
#' 
#' Takes a sequence of \code{tableMatrix} arugments and combine them by row. If 
#' matrix parts have same dimensions, it only adds rows in tab and mat part. 
#' Otherwiseit will add new matrix to mat part of \code{tableMatrix}.
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
#' image8By8TM <- tableMatrix(images8By8, list(c(1:3)), list(c(4:ncol(images8By8))))
#' #Only first two columns are used for tab part
#' image8By8TM2 <- tableMatrix(images8By8, list(c(1,2)), list(c(4:ncol(images8By8))))
#' image10By10TM <- tableMatrix(images10By10, list(r=c(1,3)), list(j=c(4:ncol(images10By10))))
#'
#' #Different matrix part, another matrix is created in mat
#' together1 <- rbind(image8By8TM , image10By10TM)
#' length(mat(together1))
#' tab(together1)
#'
#' #Same mat and tab part
#' together2 <- rbind(image8By8TM , image8By8TM)
#' length(mat(together2))
#' tab(together2)
#'
#' #Different number/names of columns of tabs, fill=TRUE must be used
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
#' @export
copy.tableList <- function(obj) {

	obj$tab <- tab(obj)
	return(obj)
}

#' Copying tableMatrix
#' 
#' Copy of a tableMatrix obj
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
#' @export
is.tableList <- function(obj) {

	if ("tableList"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' tableMatrix testing
#' 
#' Tests if passed obj is of class tableMatrix
#' @export
is.tableMatrix <- function(obj) {

	if ("tableMatrix"%in%class(obj)) return(TRUE)
	return(FALSE)
}
