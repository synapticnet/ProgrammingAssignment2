## makeCacheMatrix creates an object that holds a matrix and its inverse.
## cacheSolve returns the inverse of the matrix stored in a
## makecacheMatrix object.



## creates an object that holds a matrix and can store its inverse.
## get() retruns the original matrix. getInv() returns the inverse matrix
## if it has been calculated, if not returns null.  Set() stores a matrix and
## removes any stored inverse matrix.
makeCacheMatrix <- function(mtx = matrix()) {
    
    invMtx <- NULL
    set <- function(y){
        mtx <<- y
        invMtx <<- NULL
    }
    get <- function() mtx
    
    setInv <- function(inv) invMtx <<- inv
    getInv <- function() invMtx
    
    isCachedMatrix <- TRUE
    
    list(set = set, get = get, setInv = setInv, 
         getInv = getInv, isCachedMatrix = isCachedMatrix)
}


## Input: Takes a makeCachMatrix object
## output: returns the inverse of the matrix stored in the makeCachMatrix object.
##      If that object does not contain the inverse matrix it will calculate
##      the inverse, store it in the makeCachMatrix object, then return the result.
cacheSolve <- function(cachedMatrix, ...) {
    
    inv <- cachedMatrix$getInv()
    if(!is.null(inv)){
        return(inv)
    }
    
    mtx <- cachedMatrix$get()
    inv <- solve(mtx, ...)
    cachedMatrix$setInv(inv)
    inv
}
