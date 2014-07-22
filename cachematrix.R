## Ciaran McCormick
## cachematrix.R contains two functions used to cache the result of a 
## matrix inversion.

## makeCacheMatrix contains 4 functions which set, get, setinverse and getinverse
## of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## set to NULL
        
        ## set the inversion matrix and let the original matrix be NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix m
        get <- function() x
        
        ## Called to set the global variable m to the inverse
        setinverse <- function(inverse) m <<- inverse
        
        ## Return the global matrix m
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## If m is not NULL then inverse matrix exists, return inverse
        if(!is.null(m)) {
                message('getting cached data')
                return(m)
        }
        
        ## else return data and solve matrix to get inverse
        data <- x$get()
        m <- solve(data,...)
        
        ## set the cache
        x$setinverse(m)
        
        ## return the inverse
        m
}



