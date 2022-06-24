## Together, these functions save time and space by allowing the user to avoid 
## calculating the inverse of a matrix repeatedly by caching the value.

## This function (makeCacheMatrix) creates a special matrix that is a list
## of functions. These functions set the value of the matrix, get the value of
## the matrix, set the inversion of matrix, and get the inversion of the matrix.
## It does NOT calculate the inversion itself. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function (cacheSolve) uses the data from the makeCacheMatrix function
## and computes the inversion. If the inversion has already been computed and
## stored, it will print the result from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
