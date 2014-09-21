# Matrix inversion is a computationally costly operation.
# There may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly spcially when the matrix is immutable or slow
#changing in nature. 
#The following two functions are used to cache/retrieve the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the data of the matrix
# 2. get the data of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the cached inverse matrix
  invr <- NULL
  
  # Setter for the matrix 
  set <- function(y) 
  {
    x <<- y
    ## remove the cache of previously computed inverse matrix
    invr <<- NULL
  }
  
  # getter for the matrix 
  get <- function()
  {
    x
  }
  
  # setter for the inverse  matrix 
  setInverse <- function(invr_n)
  {
    invr <<- invr_n
  }
  
  # getter for the inverse  matrix 
  getInverse <- function()
  {
    invr
  }
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# The following function cacheSolve returns the inverse of the matrix. It 
# takes an matrix object created by makeCacheMatrix method defined previously.
# It first checks if the inverse has already been computed and return if 
# already computed and cached without doing any further computation.
# If cache is notpresent, it computes the inverse, sets the value in the cache 
# for future used and return the newly computed inverse.

cacheSolve <- function(x, ...) 
{
  invr <- x$getInverse()
  if(!is.null(invr)) ## inverse is already comuted and return it
  {
    message("getting cached data x")
  }else ##inverse is not computed , compute it and set it in the cache 
  {
    matrx <- x$get()
    invr <- solve(matrx)
    x$setInverse(invr)
  }
  #return the inverse 
  invr
}


# Test  run example
#> x = rbind(c(1, 2), c(3, 4))
#> m = makeCacheMatrix(x)
#> cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> cacheSolve(m)
#getting cached data x
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

