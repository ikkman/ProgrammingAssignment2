## [NAME]:
## makeCacheMatrix
## [DESCRIPTION]:
## Create a special matrix holder that comes with inverse property to
## check if the inverse has been computed/cached before to save time
## [INPUTS]: 
## - x is a square invertible (assumption)
## [OUTPUT]: 
## - a list containing functions to retrieve/set the inverse of x

makeCacheMatrix <- function (x = matrix()) 
{
   inv <- NULL
   
   set <- function (y) 
   {
      x <<- y
      inv <<- NULL
   }
   
   get <- function () x
   
   setinverse <- function (inverse) inv <<- inverse
   
   getinverse <- function () inv
   
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## [NAME]:
## cacheSolve
## [DESCRIPTION]:
## Calculated the inverse of x if it has not computed/cached before or
## return the computed/cached inverse immediately 
## [INPUTS]: 
## - x is a special matrix/list created by makeCacheMatrix function
## [OUTPUT]: 
## - inverse matrix of x

cacheSolve <- function (x, ...) 
{
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   
   if(!is.null(i)) 
   {
      message("getting cached data")
      return(i)
   }
   
   data <- x$get()
   
   i <- solve(data, ...) ## b is missing so it calculate the inverse of a
   
   x$setinverse(i)
   
   i
}
