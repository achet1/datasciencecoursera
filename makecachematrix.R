
##MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) ##set the matrix
   {
   x <<- y
   inv <<- NULL
 }
 get <- function() x   ##get the matrix
 setinverse <- function(inverse) inv <<- inverse   ##set the inverse of the matrix
 getinverse <- function() inv                     ##get the inverse of the matrix
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}









## Cache solve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...)
  {
  ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if(!is.null(inv))
     {
         message("getting cached data")
         return(inv)
      }
   data <- x$get()
   inv <- solve(data) %*% data  ##calculating the inverse
   x$setInverse(inv)
   inv   ##returning the marix
 }
