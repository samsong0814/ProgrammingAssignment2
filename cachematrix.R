## Below are a pair of functions that that stores a matrix and cache the inverse of a matrix.

## The following function makeCacheMatrix creates a special "Matrix",
## which is really a list containing a function to
## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function computes the inverse of the special "Matrix"
## created with the above function. If the inverse has already been calculated 
## then it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


