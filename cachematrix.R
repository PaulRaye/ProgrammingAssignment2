## A Matrix object that caches its inverse

## Creates the Matrix object with starting matrix
##  $set sets the matrix
##  $get returns the matrix
##  $setInverse sets the inverse of the matrix
##  $getInverse returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL # holds the cached inverse
    set <- function(newMatrix) {
        x <<- newMatrix
        cachedInverse <<- NULL # reset the inverse
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse )
}


## Checks to see if the inverse is cached. 
## If it isn't calculates the inverse and sets it
## Lastly returns the inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(is.null(inverse)) {
        message("recalculating inverse...")
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
    }
    inverse
}
