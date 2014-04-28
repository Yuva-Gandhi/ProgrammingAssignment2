## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) 
{
            inverse <- NULL
            mat <- mat
            
            set <- function(input) 
            {
                  mat <<- input
                  inverse <<- NULL
            }
            
            get <- function() 
                  mat
            
            setinverse <- function(matrix)
                  inverse <<- matrix
            
            getinverse <- function() 
                  inverse
            
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...)
{
            inverse <- x$getinverse()
            
            if(!is.null(inverse)) 
            {
                  message("getting cached data")
                  return(inverse)
            }
            
            data <- x$get()
            inverse <- solve(data, ...)
            x$setmean(inverse)
            inverse
}
