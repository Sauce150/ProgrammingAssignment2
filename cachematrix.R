## The following two functions are designed to allow the user to
## solve for the inverse of a matrix and cache (store) that value
## for later use.

## The makeCacheMatrix function allows the user to select one of
## four operations related to caching the value of a matrix
## and its inverse, which are:
##    1.  Set the value of the matrix.
##    2.  Get the value of the matrix.
##    3.  Set the inverse of the matrix.
##    4.  Get the inverse of the matrix.

## This function WILL NOT SOLVE for the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y                               ## Sets the value of the matrix
                inverse <<- NULL                      ## resets inverse to NULL
        }
        get <- function() x                           ## Gets the value of the matrix
        setinverse <- function(inv) inverse <<- inv   ## Sets the inverse of the matrix
        getinverse <- function() inverse              ## Gets the inverse of the matrix
        list(set = set,
             get = get,                               ## Creates and returns a list of
             setinverse = setinverse,                 ## the four functions defined above.
             getinverse = getinverse)

}


## The cacheSolve function will check to see if the inverse of the passed
## matrix has been cached.  If it finds the value, it will return the
## cached value.  If it does not find the value, it will calculate the
## inverse of the matrix, cache the value, and return the inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()                     ## looks for the inverse
        if(!is.null(inverse)) {                       ## if found, returns the message
                message("getting cached data")        ## and the inverse matrix
                return(inverse)
        }
        data <- x$get()                               ## if not found, gets the martix
        inverse <- solve(data, ...)                   ## solves for the inverse
        x$setinverse(inverse)                         ## caches the inverse value
        inverse                                       ## returns a matrix that is
}                                                     ## the inverse of 'x'

