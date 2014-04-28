## Put comments here that give an overall description of what your
## functions do

## makes cached matrix which is list containing
## functions set/get the value of the matrix
##           setinverse/getinverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    
    x <<- y
    invm <<- NULL  
  }
  
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## returns the inverse matrix
## if possible uses cached value

cacheSolve <- function(x, ...) {
  
  invm <- x$getinverse()
  
  if (!is.null(invm)) {
    message("getting inverted cached matrix")
    return(invm)        	
  }
  
  data <- x$get()
  
  message("inverting matrix + caching it")
  invm <- solve(data)
  x$setinverse(invm)
  invm  
}


a <-makeCacheMatrix(matrix( c(5,2,3,4,5,6,7,8,9), nrow=3, ncol=3) )
b <-cacheSolve(a)
c <-cacheSolve(a)
d <-cacheSolve(a)