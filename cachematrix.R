## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              ##initialized to hold inverse matrix
  set <- function(y){             ##define the set func to assign new 
    x <<- y                       ##value of matrix in parent environment
    inv <<- NULL           ##if there is a new matrix inv_matrix evaluated to null
    
  }
  get <- function()x             ##define the get func, returns value of matrix arg
  
  set_inverse <- function(inverse) inv <<- inverse        ##assign value of inv_matrix in the parent environment 
  get_inverse <- function() inv                           ##get the value of inv_matrix where its called
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)      ## you need this in order to refer 
  ##to the functions with the $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$get_inverse()                  
  
  if(!is.null(inv)){                         ##if inverse matrix is not null print  
    message("getting cached data")           ##message and return the inverse matrix
    return(inv)
  }
  data <- x$get()              ##assign x$get to data
  inv <- solve(data, ...)       ##result of solve assigns to the inverse matrix (solve() is a func used to solve equations)
  x$set_inverse(inv)
  inv
}


