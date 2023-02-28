## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
                
