## makeCacheMatrix and cacheSolve improve performance of an algorithm that needs the inverse of the same matrix
## many times. cacheSolve calculates the inverse of an invertible matrix one time and then obtain the inverse
## from cache every time thereafter with the help of makeCacheMatrix, thus avoiding recalculating the inverse.

## makeCacheMatris is a function of functions that sets and gets a matrix as well as its inverse from cache.
## It assumes the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(theInverse) i <<- theInverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix. It assumes that the matrix is always invertible.
## If the inverse of the matrix has already been calculated and store in cache, it takes it
## from cache, otherwise it calculates it, stores it in cache and returns the inverse.
## It uses a function of function as input (makeCacheMatrix) to store set and get the data from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message('getting cached inverse matrix')
                return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
