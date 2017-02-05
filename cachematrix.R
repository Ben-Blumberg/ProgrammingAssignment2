## makeCacheMatrix creates an environment to store, retreive, and set a matrix x and its inverse i.
## cacheSolve retrieves the calculated inverse of a matrix from i in the makeCacheMatrix objct if 
## available, if not available the inverse is calculated and the cached value is set to it

## function creates an object which contains, as a list, functions for getting and setting the matrix
## and the inverse. The matrix and the inverse are stored within the objects environment 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function checks whether a value for inverse is currently stored in the makeCacheMatrix object
## which it returns if available. If not available it calculates the inverse, sets the inverse variable
## in the object environment, and returns the calculated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m<- x$get()
        i <- solve(m, ...)
        x$setInverse(i)
        i
}
