## Programming Assignment 2: create nonsingular matrix --> cache;
## compute inverse of cached matrix

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # sets matrix value, clears any prev matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # gets matrix value
        get <- function() x
        
        # sets and gets inverse of matrix 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        # creates list of all four
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        # if inverse matrix has already been computed, return it
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        
        # if inverse matrix has not been computed, 
        # compute and return it
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}