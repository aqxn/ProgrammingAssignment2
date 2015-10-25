## the following pair of functions caches the inverse of a matrix.

## this function creates a special "matrix" object that can cache its inverse
## this is a list of functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of the special "matrix" created with
## the function above. It first checks if the inverse has already been
## calculated. If so, it "gets" the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data/matrix and
## sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}

