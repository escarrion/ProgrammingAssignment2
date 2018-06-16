## Put comments here that give an overall description of what your
## functions do

##make a special matrix object we can pass to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) inverse <<- solve
    getinv <- function() inverse
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv, determinant = determinant )
}


## Return the inverse of the object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)){
        message('getting cached data')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinv(inverse)
    inverse
}
