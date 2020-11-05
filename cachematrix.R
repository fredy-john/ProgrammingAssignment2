## Put comments here that give an overall description of what your
## functions do

#Caches the value of inverse matrix, so that it isn't recalculated
#if the values haven't changed

## Write a short comment describing this function

# creates a special "vector", 
#which is really a list containing a function to :
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x=matrix()) 
{
  i <- NULL
  
  set <- function(y) 
  {
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


## Write a short comment describing this function

#Calculates and sets value for inverse matrix
#if it exists, displays cached value

cacheSolve <- function(x=matrix()) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  i <- solve(x$get())
  x$setinverse(i)
  i
}
