# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL #setting the inverse to zero as a placeholder
  set <- function(y) {
    x <<- y #sets x to y in this environment
    xinv <<- NULL #sets the inverse matrix to null as a placeholder
  } 
  
  get <- function() x # to get the matrix that has to be cached
  setInv <- function(inv) xinv <<- inv # sets xinv to the inversed matrix
  getInv <- function() xinv # gives the inverse
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv) #created a list where you cabn find the input (x) and the output (inverse of x)
}


# The second function calculates the inverse of the special "matrix" created with the first function. 
# If the inverse has already been calculated, the second function gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
  m <- x$getInv() # sets m to the inverse of x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } # the inverse is already known it's returned
  data <- x$get() #if not data is becoming the cached matrix
  m <- solve(data) # m is becoming the inverse of data ( and so the inverse of x)
  x$setInv(m) # set m as inverse of x
  m #return the inverse
}


