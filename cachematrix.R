## Put comments here that give an overall description of what your
## functions do

## Define a function makeCacheMatrix that takes a matrix m as input.

makeCacheMatrix <- function(m) 
	{
  	list(
    		set = function(mtrx) 
		{
      	m <<- mtrx
      	inv <<- NULL
    		},
    	get = function() m,
    	setInverse = function(inverse) inv <<- inverse,
    	getInverse = function() inv)
	}


## Define a function cacheSolve that takes a cacheMatrix and additional arguments (if any) as input.
		
cacheSolve <- function(cacheMatrix, ...) 
	{
  	inv <- cacheMatrix$getInverse()
  	if (!is.null(inv)) 
		{
    		message("getting cached data")
    		return(inv)
  		}
  	matrix <- cacheMatrix$get()
  	inv <- solve(matrix, ...)
  	cacheMatrix$setInverse(inv)
  	inv
	}
                
