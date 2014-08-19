## 'makeCacheMatrix' and 'cacheSolve' Functions

## These two functions work together - briefly, the first function creates a 
## matrix and saves the information in a cache. The second function creates an
## inverse of the original matrix.

## 'makeCacheMatrix' Function
## The 'makeCacheMatrix' function has two elements, firstly, it creates a 
## matrix and saves the information, secondly, it contains a list of functions 
## allowing this information to be used by the 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## 'cacheSolve' Function
## The 'cacheSolve' function returns the inverse of the matrix created in the
## 'makeCacheMatrix' function. If the inverse of the matrix has already been
## requested, the message "getting cached data" will be displayed and the 
## cached information will be retrieved and returned.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
       

