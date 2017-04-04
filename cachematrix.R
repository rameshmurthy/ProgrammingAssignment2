## The solution has two functions one constructs a special matrix and other
## accepts the special matrix and returns the inverse of the matrix.
## It improves the performance of the invers of a matrix by caching the result.

## This function constructs a special matrix and returns the list of access methods to this special matrix
## It has setter and getter methods of the matrix and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL
    set <- function(y){
        x <<- y
        iMatrix <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inverseMatrix) iMatrix <<- inverseMatrix
    getinversematrix <- function() iMatrix
    list(set = set, get = get, setinversematrix = setinversematrix, 
         getinversematrix = getinversematrix)
}


## This method returns the inverse of a give special matrix. It uses solve method.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iMatrix <- x$getinversematrix()
    if(!is.null(iMatrix)) {
        message("getting cached data")
        return(iMatrix)
    }
    dataMatrix <- x$get()
    iMatrix <- solve(dataMatrix,...)
    x$setinversematrix(iMatrix)
    iMatrix
}
