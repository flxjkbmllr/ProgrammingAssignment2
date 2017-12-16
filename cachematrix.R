## This second programming assignment is to write an R function that is able
## to cache potentially time-consuming computations.
## In our case, taking the inverse of a matrix is typically a fast operation.
## However, for a very large matrix, it may take long to compute the inverse,
## especially if it has to be computed repeatedly (e.g. in a loop).
## If the contents of a matrix are not changing, it may make sense to cache
## the value of the inverse so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i ## Return a matrix that is the inverse of 'x'
}

