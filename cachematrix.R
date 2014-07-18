## @authot Shushan Chen,13:00 18/07/2014
## This program aims to reduce the cost of calculating the inverse of matrix
## by caching the data rather than computing repeatedly

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This fuction is used to compute the inverse of the special "matrix" returned by 
## above function "makeCacheMatrix". Since it caches the inverse, if the inverse has
## already been calculated, the cached result will be directly returned without 
## calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv < solve(x)
    x$setinv(inv)
    inv
}
