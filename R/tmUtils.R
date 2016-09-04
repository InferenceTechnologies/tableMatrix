#
# Inference Technologies
#
# Utils
# 
# 0.23
# 


# Returns column index for both numeric and character input
# spceial value -1 for last column
# spceial character "last" for last column
# col can be list, item name specifies action:
# j  - column coordinates, numeric or character
# r  - column range, numeric or character
# rb - column range, numeric or character, border columns excluded
colj <- function(obj, col, do="j", sortj=FALSE, numj=TRUE) {

	# maybe not needed anymore?
	if (!is.list(col)) {
		col <- list(col)
		names(col) <- do
	}

	if (is.null(names(col))) {
		names(col) <- do
	}

	if (is.character(obj)) {
		colNames <- obj
	} else {
		colNames <- colnames(obj)
	}
	nCol <- length(colNames)

	coljV <- integer()
	for (i in 1:length(col)) {
		
		colItem <- col[[i]]
		colRange <- switch(names(col)[i], "r"=TRUE, "rb"=TRUE, FALSE)
		colBorder <- switch(names(col)[i], "rb"=FALSE, TRUE)

		if (is.character(colItem)) { 
			lastChar <- match("last", colItem)
			if (!is.na(lastChar)) { colItem[lastChar] <- colNames[nCol] }
			colItem <- na.omit(match(colItem, colNames))
			if (!length(colItem)) { next }
		}

		lastj <- match(-1, colItem)
		if (!is.na(lastj)) { colItem[lastj] <- nCol }

		if (colRange) { colItem <- min(colItem):max(colItem) } 
		if (!colBorder&&length(colItem)>2) { colItem <- colItem[-c(1, length(colItem))] }
		coljV <- union(coljV, colItem)
	}
	if (!length(coljV)) { coljV <- 1:nCol 
	} else if (sortj) {
		coljV <- sort(coljV)
	}
	if (!numj) { coljV <- colNames[coljV] }

	return(coljV)
}

# Moves columns
# special character "end" to move colSrc at the end
colShiftRef <- function(obj, colSrc, colDest, destInSrc="doNothing") {

	colSrc <- colj(obj, colSrc, sortj=F, numj=F)
	colNewOrder <- setdiff(colnames(obj), colSrc)

	if (is.numeric(colDest)) {
		if (colDest>ncol(obj)) { colDest <- "end" }
	}

	if (colDest!="end") {
		colDest <- colj(obj, colDest, sortj=F, numj=F)
		colDestj <- match(colDest, colNewOrder)
		if (is.na(colDestj)) { 
			if (destInSrc=="doNothing") { return(invisible()) } 
			colDestj <- destInSrc 
			if (colDestj>length(colNewOrder)) { colDestj <- -1 }
		}
	} else {
		colDestj <- -1
	}

	if (colDestj==1) {
		colNewOrder <- c(colSrc, colNewOrder)
	} else if (colDestj==-1) {
		colNewOrder <- c(colNewOrder, colSrc)
	} else {
		colNewOrder <- c(colNewOrder[1:(colDestj-1)], colSrc, colNewOrder[colDestj:length(colNewOrder)])
	}

	setcolorder(obj, colNewOrder)
	return(invisible())
}

# limits indexing to the length of the object
geti <- function(obj, i) {

	if (is.null(obj)) { return(NULL) }

	# case when obj is vector 
	if (! is.list(obj)) { obj <- list(obj) }
	return(obj[ifelse(i>length(obj), length(obj), i)])
}

# cat function
ncatn <- function(..., k1=0, k2=1, verbose=TRUE) {
	if (!verbose) { return(invisible()) }
	cat(rep("\n",k1))
	cat(...)
	cat(rep("\n",k2))
}

# print function
nprintn <- function(..., k1=0, k2=0, verbose=TRUE) {
	if (!verbose) { return(invisible()) }
	cat(rep("\n",k1))
	print(...)
	cat(rep("\n",k2))
}