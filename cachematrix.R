## These functions can be used to cache inverses of matrix to avoid unnessary calculations
##
## Example - create the cache object
## m <- matrix( 1:4, 2, 2 )
## mCache <- makeCacheMatrix(m)
##
## Example usage for retrieving the inverse
## mCache$getInverse()
## OR
## cacheSolve( mCache )
##
## If you want to reuse same object with different matrix
## mCache$set( matrix( 1:9, 3, 3) )
#


## makeCacheMatrix function constructs a list object containing following functions 
## set( matrix ) - sets the matrix to be cached. Matrix needs to be invertible.
## get() - returns the original matrix
## getInverse() - returns cached inverse and calculates it if necessary

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  ## Sets the matrix and initializes variables
  set <- function(matrix) 
  {
    x <<- matrix
    cachedInverse <<- NULL
  }
  
  get <- function() 
  {
    x
  }
  
  getInverse <- function()
  {
    if( is.null(cachedInverse))
    {
      cachedInverse <<- solve( x )
    } 
    cachedInverse
  }
  
  list (set = set , get = get, getInverse = getInverse)
}


## cacheSolve calls the getInverse for the cached matrix
## and returns the cached value
cacheSolve <- function ( x = matrix(), ... )
{
  x$getInverse()
}