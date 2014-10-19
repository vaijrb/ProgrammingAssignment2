# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

   makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
           x <<- y
           inv <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
          message("Getting data from cache")
          return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}

### Sample Run ###
##Create the matrix

#> x = rbind(c(1, -1/4), c(-1/4, 1))
#> m = makeCacheMatrix(x)
#> m$get()
#      [,1]  [,2]
#[1,]  1.00 -0.25
#[2,] -0.25  1.00

## First run getting non cached data

#>cacheSolve(m)
#          [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667

## Second Run getting the data from the cache

#> cacheSolve(m)
#Getting data from cache
#          [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> 
