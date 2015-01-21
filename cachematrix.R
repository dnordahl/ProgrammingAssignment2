## The following two functions use lexical scoping to create a set of 
## cached values in a side environment to store the cached results of 
## matrix inversions.

## makeCacheMatrix() creates a wrapper around a matrix object which has an
## associated container to store a cached, inverse value of that matrix 
## in another persistent scope which will accompany matrix results returned by 
## makeCacheMatrix.  The inverse container is not populated until 
## computation of inversion is actually requested by a call to cacheScope.
## Subsequent calls to cacheScope will return the cached value until 
## the wrapper's set function is called again, in which case the container 
## is reset to NULL and the next call to cacheSolve will recompute the 
## inverse.  Since an element-by-element matrix comparison would be an 
## expensive means of change detection, a simple reference equality check 
## is used instead (So we will assume that mostly an API user will not  
## update a matrix unless they intend to change its values).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function is called on a matrix (and associated inverted matrix container) 
## returned by the makeCacheMatrix method.  If the matrix inverse has not been set and 
## stored in the container associated with this matrix (variable inverse), then it will 
## compute it, assign the result to variable inverse, and return its value.  
## Otherwise it will return the cached container's value (variable inverse)

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()  # (this will return NULL for the next line if makeCacheMatrix$set 
                             #  has changed the matrix since the last time the cacheSolve was called)
  if(!is.null(inverse)) {
    message("getting cached data")  # this lets us know a cached value was returned for debug purposes
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

## Test cases:

# > A <- makeCacheMatrix()
# > A$set(matrix(1:4,nrow=2,ncol=2))
# > A$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## Inverse is not calculated until requested:

# > A$getInverse()
# NULL

## Inverse is calculated and returned on first call to cacheSolve:

# > cacheSolve(A)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## This call to cacheSolve also updated the cached inverse result container value:

# > A$getInverse()
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

## A second call to cacheSolve on an unchanged A returns the cached result:

# > cacheSolve(A)
# getting cached matrix...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## a change to matrix A is made:

# > A$set(matrix(5:8,nrow=2,ncol=2))
# > A$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8

## The change was detected and the inverse was recalculated and returned:

# cacheSolve(A)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

