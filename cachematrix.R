## Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse. 

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
## "matrix" object is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then retrieve the inverse from the cache.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## If the inverse has not already been calculated computes the inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
