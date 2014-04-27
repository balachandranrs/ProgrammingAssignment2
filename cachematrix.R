## Overall idea of the following modules is to avoid repeated computations
## The two functions store the inverse of the matrix object so that the inverse
## need not be computed every time.

## The 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y     #Scoping Assignment
        m <<- NULL  #Scoping m to NULL within 'set'
    }
    
    get <- function() x  #'get' function returns x
    
    setinverse <- function(inverse) m <<- inverse  
                    #'setinverse' is being used by 'cacheSolve'
    
    getinverse <- function() m
                    #'getinverse' returns the inverse value from pseudo 'cache'
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
                    #'makeCacheMatrix' returns a list of 4 functions
}


## 'cacheSolve uses the methods defined in the function 'makeCacheMatrix'
## and returns the inverse of a matrix

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    #check if matrix inverse already exist
    if(!is.null(m)) {
        message("Getting Cached Matrix...")
        return(m)
        #return from pseudo'cache' and exit function
    }
    
    #if inverse is not computed already, compute inverse
    data <- x$get()
    
    #solve function computes the inverse of matrix
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m               #returns the inverse of the matrix
}
