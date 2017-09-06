## makeCacheMatrix includes four functions which set the value of the matrix, gets the value of the matrix,
## sets the inverse of the matrix, and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function(){
        x
    }
    setinverse <- function(solve) m <<- inverse
    getinverse <- function() m
    list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
