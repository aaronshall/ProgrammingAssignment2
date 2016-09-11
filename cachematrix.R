## Peer Graded Assignment
## Programming Assignment 2: Lexical Scoping
##
## In this assignment we will write two functions, one
## that creates a special "matrix" object that can cache
## its inverse, makeCacheMatrix, and another that computes
## the inverse of the special "matrix" returned by
## makeCacheMatrix, cachesolve. If the inverse has already
## been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the
## cache.
##

makeCacheMatrix <- function(x = matrix()) {
    # makeCacheMatrix creates a matrix, which is a list
    # containing a function to
    # 1. set the matrix
    # 2. get the matrix
    # 3. set the inverse of the matrix
    # 4. get the inverse of the matrix
    
    # Initially set to NULL
    inv.matrix <- NULL
    
    # Set the matrix itself but not the inverse
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }
    
    # Get the matrix itself but not the inverse
    get <- function() x
    
    # Set the inverse
    setinverse <- function(inverse) inv.matrix <<- inverse
    
    # Get the inverse
    getinverse <- function() inv.matrix
    
    # Create list
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)	
}

##Calculate the inverse and cache the result

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv.matrix <- x$getinverse()
    
    # If ...
    if(!is.null(inv.matrix)) {
        # Return computed inverse		
        return(inv.matrix)
    }
    
    # If no ...
    # Get the matrix itself
    data <- x$get()
    
    # Find inverse
    inv.matrix <- solve(data, ...)
    
    # Cache this result
    x$setinverse(inv.matrix)
    
    # Return new result
    inv.matrix    
    # If the inverse has already been calculated, cacheSolve()
    # returns the cached inverse
}