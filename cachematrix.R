
## This function creates a special “matrix” object that can cache its inverse.
## It uses an operator which can be used to assign the matrix value to an object 
## in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)  {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        matrx <- x$get()
        i <- solve(matrx, ...)
        x$setinverse(i)
        i
        
}