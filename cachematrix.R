## The functions makeCacheMatrix and cacheSolve can be used to compute and cache
## the inverse of a matrix 


##The makecacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
        invM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invM <<- inverse
        getinverse <- function() invM
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##With cacheSolve the inverse of the matrix returned by makeCacheMatrix is computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinverse()
        #check whether inverse of matrix has been already calculated
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinverse(invM)
        invM
}
