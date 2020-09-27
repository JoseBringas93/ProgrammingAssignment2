## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                   #Initialize inv as NULL since it  is a new matrix
  set <- function(y) {
    x <<- y                                     #Set x value as recently created matrix (x being outside this definition, thus the <<-)
    inv <<- NULL                                
  }
  get <- function() x                           #When x$get is called, prints x value 
  setinverse <- function(solve) inv <<- solve   #When x$setinverse is called, set inv value to its argument
  getinverse <- function() inv                  #When x$getinverse is called, prints the inv value
  list(set = set, get = get,                    #Adding functions to a list
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                         #Gets the current inv value
  if(!is.null(inv)) {                           #If inv value is different than null(already calculated),
    message("getting cached data")              #it will tell you that a cached valued is being returned 
    return(inv)                                 #and print it, ending the routine.
  }
  data <- x$get()                               #If in value has not been calculated(if condition not being met), it will retreive the matrix
  inv <- solve(data, ...)                       #Calculates the inverse of the argument matrix if routine has not returned anything.
  x$setinverse(inv)                             #Sets the inv value to our recent calculated inverse
  inv                                           #Prints matrix inverse
}
