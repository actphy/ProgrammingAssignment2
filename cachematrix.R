## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) minv <<- inverse
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
               minv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(minv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(minv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        minv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(minv)
        
        return(minv)
}
