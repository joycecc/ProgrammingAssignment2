## makeCacheMatrix creates a list of 4 functions, set(),get(),setinv(),getinv(),
##cacheSolve() can call the functions created by makeCacheMatrix()

## Test Example: 
##    > x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##    > cacheSolve(x)
##    .. calculate inverse and output
##    > cacheSolve(x)
##    .. output inverse matrix from cache 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  ## set initial inverse as NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x   
  #get the initial matrix
  
  setinv <- function(invs) m <<- invs 
  #store the calculated inverse matrix into m, cacheSolve() will call this 
  ##function to store the inverse after computing the inverse of the initial matrix
  
  getinv <- function() m 
  ## if the inverse matrix is already computed , then getinv() will return the inverse,
  ##if not, then getinv() will return NULL. cacheSolve will call this function to see 
  ##if the the inverse exists already
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  ##the list of 4 functions that cacheSolve() will call
}


## cacheSolve mainly check if the inverse matrix is already computed. If not, then cacheSolve() compute
## the inverse matrix and store it back to x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ##get the inverse matrix(or NULL if the inverse hasn't been calculated) from x
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the inverse is already computed then just return cached data
  
  data <- x$get()
  ##if the inverse isn't in x then get the initial matrix from x
  
  m <- solve(data)
  ##calculate the inverse
  
  x$setinv(m)
  #store the calculated inverse matrix
  
  m ## return the calculated inverse matrix
}
