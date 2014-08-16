##  A pair of functions that cache the inverse of a matrix.

##  makeCacheMatrix: creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  cacheSolve:computes the inverse of the matrix returned by makeCacheMatrix 
##  above. If the inverse has already been calculated and the matrix has not 
##  changed then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinv(i)
        i
}
