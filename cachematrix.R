## Put comments here that give an overall description of what your
## functions do

## This script contains two functions to calculate the inverse matrix
## of a square invertible matrix and can cache the inverse matrix to
## reuse in later time if the given matrix doesn't change. 

## Write a short comment describing this function

## The makeCacheMatrix takes a square invertible matrix as input and
## can cache it's inverse. This function also creates a special "matrix"
## for the cacheSolve funtion.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

## The cacheSolve function calculates the inverse of the special "matrix"
## returned by the makeCacheMatrix function. If the matrix doesn't
## change and the inverse is calculate than this function can retrive
## the value from makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
