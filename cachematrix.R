## 
## Put comments here that give an overall description of what your
## functions do
## 
## This function creates a invertible matrix to be used as 
## input of the cacheSolve() function
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## 
## This function return the inverse of the original matrix created by the 
## function makeCacheMatrix()
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting chached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        return(inv)
}
