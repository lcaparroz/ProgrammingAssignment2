## Creates a special matrix object with the ability to cache its inverse.
## Arguments:
## - x: an R matrix value. Default: empty matrix.
## Return: a list with the following attributes:
## - get: function to retrieve the matrix.
## - set: function to assign the matrix. Resets the cached inverted matrix.
## - getInverse: function to retrieve the inverse of the matrix. Returns NULL if
##               the inverse of the matrix has not been assigned yet.
## - setInverse: function to assign the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize variables
    i <- NULL

    # Matrix getters
    get <- function() x
    getInverse <- function() i

    # Matrix setters
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    setInverse <- function(inverse) i <<- inverse

    # Return the matrix object
    list(get = get,
         getInverse = getInverse,
         set = set,
         setInverse = setInverse)
}

## Compute the inverse of a matrix. If the passed matrix object has the cached
## value of its inverse, returns it instead.
## Arguments:
## - x: a matrix object, created with the function `makeCacheMatrix`.
## Return: the inverse of the provided matrix.

cacheSolve <- function(x, ...) {
    # Retrieve cached inverse of the matrix
    i <- x$getInverse()

    if(is.null(i)) {
        # Compute inverse of the matrix and cache the value
        i <- solve(x$get(), ...)
        x$setInverse(i)
    } else {
        message("Getting cached inverse of the matrix")
    }

    ## Return the inverse of matrix 'x'
    i
}
