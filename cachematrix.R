## This file has source for a pair of functions that caches the inverse of a matrix


##--------------Function Definitions Starte ----------------------##     


## This function creates a special "matrix" object that can cache its inverse.
## It is assumed that the matrix supplied is always invertible
## Essentially, this creates a wrapper around the actualMatrix that caches its inverse as well

makeCacheMatrix <- function(x = matrix()) {
        
        setMatrix <- function(y){
                inverse <<- NULL
                actualMatrix <<- y
        }
        
        setMatrix(x)
        getMatrix <- function(){actualMatrix}
        
        getInverse <- function(){inverse}
        setInverse <- function(inv){inverse <<- inv}
        
        list( getMatrix=getMatrix, getInverse=getInverse, setMatrix=setMatrix, setInverse=setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

## It is assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixWrapper <- x
        
        msg <- NULL
        invMatrix <- NULL
        
        if(isInverseMatrixCached(matrixWrapper)){
                invMatrix <- getInverseFromCache(matrixWrapper)
                msg <- "Inverse is Obtained From Cache."
        }else{
                msg <- "Inverse Not in Cache."
                invMatrix <- calculateInverseMatrix(matrixWrapper,...)
                cacheInverseMatrix(matrixWrapper, invMatrix)
        }
        
        message(msg)
        invMatrix
}

## This function accesses Cache Strategy and returns matrix's inverse 
getInverseFromCache <- function(matrixWrapper){
        return(matrixWrapper$getInverse())
}

## This function checks into the Cache Implementation to determine
## if the inverse of the matrix was cached or not.
isInverseMatrixCached <- function(matrixWrapper){
        invMatrix <- matrixWrapper$getInverse()
        if(is.null(invMatrix)){
                return(as.logical(0))
        }else{
                return(as.logical(1))
        }
}


## This function implements Caching Strategy
cacheInverseMatrix <- function(matrixWrapper, invMatrix){

        matrixWrapper$setInverse(invMatrix)
        invisible()
       
}
        
## This function implements strategy for calculating Inverse of a Matrix
calculateInverseMatrix <- function(wrapperMatrix1, ...){
        actualMatrix <- wrapperMatrix1$getMatrix()
        print(actualMatrix)
        return(solve(actualMatrix,...))
}

