## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function takes in input a matrix and allocates functions to get and set matrix
## or to get and set inverse. Set functions are exploit in memory allocation of matrix 
## or inverse of matrix in order to speed up matrix inversion procedure

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Function takes in input an item created with makeCacheMatrix and returns inverse of matrix
## If the inverse was previously calculated, function returns cached version, else function calculates
## inverse and then stores it for subsequent cached access 

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
