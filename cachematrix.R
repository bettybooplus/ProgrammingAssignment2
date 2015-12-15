## The assignment was to write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrx <- NULL  # assign NULL to matrx. 
        #In R NULL is an special object with no mode. 
        
        set <- function(y) { #set the value of the matrix 
                x <<- y
                matrx <<- NULL 
        }
        get <- function() x # get the value of the matrix 
        setinverse <- function(inv) matrx <<- inv # set the value of the inverse of the matrix 
        getinverse <- function() matrx # get the value of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
        
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrx <- x$getinverse()
        if(!is.null(matrx)) { # retrieve the inverse of the matrix from the cache when it has already been calculated
                message("getting cached inverse matrix") 
                return(matrx)
        }
        data <- x$get()
        matrx <- solve(data, ...) #calculate the inverse of the matrix when is not in the cache
        x$setinverse(matrx)
        matrx                     
        
}
