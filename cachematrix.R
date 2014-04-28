## Below are two functions that are used to create a special object that stores a matrix
## and cache's its inverse assuming that the matrix supplied is always invertible.

## makeCacheMatrix creates a list containing functions to
## set the value of a matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) 
{
            inverse <- NULL
            ## set the value of a matrix
            set <- function(input) 
            {
                  mat <<- input
                  inverse <<- NULL
            }
            ## get the value of the matrix
            get <- function() 
                  mat
            ## set the value of the inverse of the matrix
            setinverse <- function(matrix)
                  inverse <<- matrix
            ## get the value of the inverse of the matrix
            getinverse <- function() 
                  inverse
            ## create a list of the four functions, the list is the return value
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## The following function calculates the inverse of the input matrix. 
## If the inverse has already been calculated it gets the inverse from the cache.
## Else, it calculates the inverse of the data and sets the value in the cache.

cacheSolve <- function(x, ...)
{
            inverse <- x$getinverse()
            ## If set inverse is not null, return cached data.
            if(!is.null(inverse)) 
            {
                  message("getting cached data")
                  return(inverse)
            }
            ## Inverse not previously computed.
            ## Compute inverse and cache it.
            data <- x$get()
            inverse <- solve(data, ...)
            x$setmean(inverse)
            inverse
}
