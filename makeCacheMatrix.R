#  makeCacheMatrix sets up variables to hold a matrix and its inverse and
#  creates a list containing the variable names containing the functions
##   1. Matrix
##      (A) set the value of the matrix before you can...
##      (B) get the value of the matrix
##   2. Inverse Matrix
##      (A) set the value of inverse of the matrix then...
##      (B) get the value of inverse of the matrix
##   3. Create variables for makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
## 1. Matrix (set then get)
      set  <- function(y) {
            x <<- y         # scoped to "makeCacheMatrix/set" ("~/set")
            xInv <<- NULL   # initialise xInv, ditto scoped
      }
      get <- function() x   # returns input matrix to x

## 2. Inverse Matrix (setInv then getInv)
      xInv <- NULL          # sets up a variable to hold the inverse of x
      setInvx <- function(inv) xInv <<- inv # inverse(x) scoped to "~/setInvx"
      getInvx <- function() xInv # return inverse(x) to getInv

## Create list of variables to use as: x <- makeCacheMatrix()
##   x$set()/x$get(): set/get matrix
##   x$setInvx()/x$getInvx(): set/get inverse matrix
      list(set=set, get=get, setInvx=setInvx, getInvx=getInvx)

}

cacheSolve <- function(x, ...) {
      m <- x$getInvx()
      if(!is.null(m)) {    # if m NOT NULL then...
            message("getting cached data.")
            return(m)      # ...get it. 
      }
      data <- x$get()      # if not, we do x$get to get the matrix object
      m <- solve(data)     # run solve() on data to invert it
      x$setInvx(m)          # we then set it to the object
      m
}


# Tests
# > source("makeCacheMatrix.R")
# > source("makeCacheMatrix.R")
# > x <- matrix(rnorm(25, 1, 10), 5, 5)
# > q <- makeCacheMatrix(x)
# > cacheSolve(q)
# [,1]        [,2]        [,3]        [,4]          [,5]
# [1,]  0.04269883 -0.04405254 -0.09770979  0.16398003  0.0820866492
# [2,] -0.02777188  0.02612352  0.02352776 -0.05708792 -0.0589620276
# [3,] -0.09470532  0.02840167  0.10968300 -0.14589467 -0.0421111855
# [4,] -0.01273353  0.00153627 -0.06084448 -0.03120416  0.0005488854
# [5,] -0.08558463 -0.04325922  0.09886826 -0.16121228 -0.0232776152
# > cacheSolve(q)
# getting cached data.
# [,1]        [,2]        [,3]        [,4]          [,5]
# [1,]  0.04269883 -0.04405254 -0.09770979  0.16398003  0.0820866492
# [2,] -0.02777188  0.02612352  0.02352776 -0.05708792 -0.0589620276
# [3,] -0.09470532  0.02840167  0.10968300 -0.14589467 -0.0421111855
# [4,] -0.01273353  0.00153627 -0.06084448 -0.03120416  0.0005488854
# [5,] -0.08558463 -0.04325922  0.09886826 -0.16121228 -0.0232776152
