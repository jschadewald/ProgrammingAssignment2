## This file provides a new matrix object and a solver
## that, together, allow for caching the inverse of the matrix.
## It is almost exactly like the example for caching the mean
## of a vector. 
## A third function is provided as a "how to" guide.


## makeCacheMatrix assumes (per the assignment) that you give
##   it an invertible matrix. It returns a list object of
##   functions that let you get and set both the matrix 'x'
##   and the inverse of 'x'.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve expects its input 'x' to be a makeCacheMatrix object.
##   It returns the cached inverse of 'x' or it computes, caches,
##   and returns the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #compute the inverse
    x$setinverse(inv) #store the inverse in x
    inv
}


## Example showing use and purpose.

example1 <- function() {
    #First, create an invertible matrix.
    mat<-matrix(c(1,2,3,4),2,2)
    print(mat)
    #Then, wrap it with makeCacheMatrix
    cacheMat<-makeCacheMatrix(mat)
    #Next, compute the inverse with cacheSolve
    cacheSolve(cacheMat)
    #No need to store the value because it's cached.
    print(cacheSolve(cacheMat))
    #Let's make sure it's the inverse.
    #If it is, this will give the identity matrix:
    print(id<-mat %*% cacheSolve(cacheMat))
    #Thanks for your consideration!
}