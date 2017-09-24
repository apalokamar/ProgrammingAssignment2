        ## Put comments here that give an overall description of what your
        ## functions do

        ## This function creates a special "matrix" object that can cache its 
        ## inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <<- function(solve) inv <<- solve ## creates object for function result
        getinverse <<- function() inv ## calculates matrix inverse and storages in an object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## prints a list with the 4 objects inside the function
}

        ## This function computes the inverse of the special "matrix" returned by
        ## makeCacheMatrix above. If the inverse has already been calculated (and
        ## the matrix has not changed), then the cachesolve should retrieve the
        ## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## checks if the matrix inverse has been already calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) ## retursn the inverse if it is available
        }
        data <- x$get()
        inv <- solve(data, ...) ## calculates matrix inverse if it is not available
        x$setinverse(inv)
        inv ## returns the printed matrix inverse
      ## Return a matrix that is the inverse of 'x'
}
