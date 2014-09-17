## The makeCacheMatrix will create a matrix, it will reset the inv (inverse value)
## to NULL whether it is called directly or through the $set function.  User can 
## set the values using the $set function, can retrieve the value of makeCacheMatrix
## by using the $get function as well as set and get the inverse value (inv) using
## the $setinv & $getinv functions.



## Comments are in line

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL                                 # reset inv to null 
  set <- function(y){                        # set function to set new matrix value
    x <<- y                                  # save the new matrix value as x
    inv <<- NULL                             # reset inv to null
  }
  get <- function() x                        # get the value of x (as a function)
  setinv <- function(inverse) inv<<- inverse # function to set an inverse matrix manually
  getinv <- function() inv                   # function to get the inverse matrix 
  
  list(set = set, get = get,                 # list the available functions
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function will call the $getinv function from above and store 
## this in inv, a check will be made whether the inv value is not null, if not  
## null, display message and return cached inv value.  If null then create 'data'
## value with $get function from above, solve(data) to create the inv value, then
## multiply this with the orignal matrix to create the inverse value.  This is then
## set using the $setinv function from aboive and inv is returned

## Comments are in line

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                         # call the getinv function from above and store in inv
  if(!is.null(inv)) {                       # if inv is not null then return the cached inv value
    message("getting cached data")          # display info message
    return(inv)                             # return the cached inv value
  }                                         # if no inv value cached...
  data <- x$get()                           # store the x value from above in data
  inv <- solve(data)                        # solve data to get inverse matrix
  inv <- inv %*% data                       # multiply inv by the original data to get inv value
  x$setinv(inv)                             # set inv in above function
  inv                                       # return inv value 
}
