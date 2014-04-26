## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    # Initialise to NULL
    cachedInverse <- NULL
    
    # If you call get get the original matrixL
    Get <- function() x
    
    # if you call set, set the matrix and reinitialise the cached variable to Null
    # we could do a check to see if its the same matrix
    Set <- function(matrixToSet)
    {
        x <<- matrixToSet
        cachedInverse <<- NULL
    }
    
    # Sets the inverse and assigns the cache
    SetInverse <- function()
    {
      cachedInverse <<- solve(x)
    }
    
    # Gets the cached inverse
    GetInverse <- function() cachedInverse
    
    # Provides the method for the matrix
    list(Get = Get, Set= Set, SetInverse = SetInverse, GetInverse = GetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    val <- x$GetInverse()
    if(!is.null(val))
    {
          message("Getting the cached value")
          return(val)
    }
    
    ## Set the inverse matrix - optimsied from example
    x$SetInverse()
    
    ## get the value from the matrix
    inverseMatrix <- x$GetInverse()
    
    ## Return value
    inverseMatrix
}

cacheMatrix <- makeCacheMatrix(matrix(1:4,2,2,4))
cacheMatrix$Get()
inverseMatrix <- cacheSolve(cacheMatrix)
inverseMatrix
inverseMatrix <- cacheSolve(cacheMatrix)
inverseMatrix

### OutPut from R ###
#cacheMatrix <- makeCacheMatrix(matrix(1:4,2,2,4))
#> cacheMatrix$Get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> inverseMatrix <- cacheSolve(cacheMatrix)
#> inverseMatrix
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> inverseMatrix <- cacheSolve(cacheMatrix)
#Getting the cached value
#> inverseMatrix
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
