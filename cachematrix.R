## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(mtx = matrix()) {
        invmtx <- NULL
        set <- function(mtx.y) {
                mtx <<- mtx.y
                intmtx <<- NULL
        }
        get <- function() mtx
        setinvmtx <- function(intmtx.new) intmtx <<- intmtx.new
        getintmtx <- function() intmtx
        list(set = set, get = get,
             setintmtx = setintmtx,
             getintmtx = getintmtx)
}


## Write a short comment describing this function

# Compute the inverse of the special matrix returned by makeCacheMatrix above
cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
        intmtx <- mtx$getinmtx()
        # If the inverse has already been calculated (and the matrix not changed!),
        # then the cacheSolve should retrieve the inverse from the cache
        if(!is.null(intmtx)) {
                message("getting cached data")
                return(intmtx)
        }
        data <- mtx$get()
        intmtx <- solve(data, ...)
        mtx$setintmtx(intmtx)
        intmtx
}
