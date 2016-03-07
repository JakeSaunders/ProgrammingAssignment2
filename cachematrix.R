## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      # This function is given a square matrix x and creates a special "matrix" object that can cache its inverse.
      
      inv <- NULL               # assigns inv the value NULL inside the local enviroment 
      set <- function(y){      # nameless func that:
            x <<- y           #     assigns x the values of y outside the local enviroment 
            inv <<- NULL      #     assigns inv the value NULL outside the local enviroment 
      }
      get <- function() x      # assigns nameless func to get the value of x
      setinv <- function(inverse) inv <<- inverse 
                              # assigns nameless func to set the inverse outside local enviroment
      getinv <- function() inv # assigns nameless func to get the value of inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
                              #returns list containing functions to set and get both the matrix and its inverse
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      # This function retrieve the inverse of a matrix from cache
      
      inv <- x$getinv()        # assigns inv locally from getinv() 
      
      if (!is.null(inv)){     # if the inv have been set to NULL cache exists 
            return(inv)             # return cached value for inv
      }
      
      mat.data = x$get()      # assigns mat.data locally from get()
      inv <- solve(mat.data, ...)
      # Take inv of mat.data and assigns to inv locally
      x$setinv(inv)           # Sets inv in cache
      return(inv)             # returns value of inv
}


# For testing functions
x = matrix(c(1,2,3,4), nrow=2, ncol=2) 
b <- makeCacheMatrix(x)
c <- cacheSolve(b)
c
