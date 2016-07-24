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
#' tableList constructor, creates tableList obj from a data.frame or data.table and from a aid list
#' @param tabData data.frame or data.table
#' @param aidData Aid list data
#' @return A tableList object
#' @export 
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
#' \code{tableMatrix} constructor, creates tableMatrix object from a list of data.frames or 
#' data.tables. It combines features of data.table and matrix. The result is faster access to 
#' data. It is useful for datasets which have following condition: the main data could be stored 
#' as matrix and other columns are for description. 
#'
#' @param dataList List of data.frames or data.tables
#' @param tabCol List of table attribute columns names or indices
#' @param matCol List of matrix attribute columns names or indices
#' @return A tableMatrix object
#' @export 
#' @examples 
#' data(images8By8)
#' dim(images8By8)
#' 
#' data(images10By10)
#' dim(images10By10)
#' images10By10AsTable <- as.data.table(images10by10) 
#'
#' #Use data.frame as data, first 3 columns as data descriptors, rest as part for matrix
#' tableMatrix(images8By8, list(r=c(1:3)), list(j=c(4:ncol(images8By8))))
#'
#' #Use data.table as data, first 3 columns as data descriptors, rest as part for matrix
#' tableMatrix(images10By10AsTable, list(r=c(1:3)), list(j=c(4:ncol(images10By10AsTable ))))
#'
#' #Use data.frame as data, first 2 columns as data descriptors with column names, 
#' #rest as part for matrix
#' tableMatrix(images8By8, list(r=c("shape","dimX","dimY")), list(j=c(4:ncol(images8By8))))
#'
#' \dontrun{
#' tableMatrix(images8By8, list(c("shape","dimX","dimY")), list(c(4:ncol(images8By8))))	
#' }

tableMatrix <- function(dataList, tabCol, matCol) {

	obj <- tableMatrixWrap()
	if (missing(dataList)) { return(obj) }

	if (is.data.frame(dataList)||is.data.table(dataList)) { dataList <- list(dataList) }

	for (i in 1:length(dataList)) {

		obji <- dataList[[i]]
		if (is.data.frame(obji)) { obji <- as.data.table(obji) }
		if (!is.data.table(obji)) { stop("tableMatrix requires list of data.frames or data.tables") }
		
		addTab <- obji[, colj(obji, geti(tabCol, i)), with=F]
		addTab[,c(tmName$matN, tmName$matRow):=list(1, 1:nrow(addTab))]
		colShiftRef(addTab, c(tmName$matN, tmName$matRow), 1)

		addMat <- as.matrix(obji[, colj(obji, geti(matCol, i)), with=F])
	
		addMatDim <- setnames(data.table(1, ncol(addMat)), c(tmName$matN, tmName$matCols))
		
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
#' tableMatrix method to get or set table attribute
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
#' tableMatrix method to get or set matrix attribute
#' @rdname mat.tableMatrix
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
	
	if (!is.list(value)||!all(sapply(l, is.matrix))) { stop("list containing matricies required") }
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
#' S3 tableMatrix method passes data.table parameters to the table attribute
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

#' Binding tableMatrix
#' 
#' Binding rows of tableMatrix objects
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
