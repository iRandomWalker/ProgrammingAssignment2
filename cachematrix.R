## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(mtx = matrix()) {
        invmtx <- NULL
        # set matrix
        set <- function(mtx.y) {
                mtx <<- mtx.y # set a new matrix, mtx.y, to mtx in the parent environment with <<-.
                intmtx <<- NULL
        }
        # retrieve matrix
        get <- function() mtx
        # set inverse matrix; note the assignment operator, <<-.
        setinvmtx <- function(intmtx.new) intmtx <<- intmtx.new
        getintmtx <- function() intmtx
        # return a list of of set(), get(), setintmtx(), and getintmtx()
        list(set = set, get = get, 
             setintmtx = setintmtx,
             getintmtx = getintmtx)
}


## Write a short comment describing this function

# Compute the inverse of the special matrix returned by makeCacheMatrix above if the inverse matrix is null and/or the
# original matrix has changed
cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
        intmtx <- mtx$getinmtx()
        # If the inverse has already been calculated (and the matrix not changed!),
        # then the cacheSolve should retrieve the inverse from the cache
        if(!is.null(intmtx)) {
                message("getting cached data")
                return(intmtx)
        }
        # get matrix and store in data
        data <- mtx$get()
        # calculate the inverse matrix.
        intmtx <- solve(data, ...)
        # make a copy of the inverse matrix to mtx
        mtx$setintmtx(intmtx)
        # return the inverse matrix calculated by solve(), if the cache does not have it.
        intmtx
}
