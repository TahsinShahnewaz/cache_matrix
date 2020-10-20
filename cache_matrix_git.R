getwd()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x    
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(t) {
  inv <- t$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- t$get()
  inv <- solve(data)
  t$setinverse(inv) #setting inverse in the cache
  inv #output
}


e<-matrix(1:4,nrow=2)
amatrix<-makeCacheMatrix(e)
amatrix$get()
amatrix$getinverse()
amatrix$set(matrix(5:8,2))
