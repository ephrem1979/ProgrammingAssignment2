MakeCacheMatrix <- function(x = matrix()){ ## Assigning result to the variable MakeCacheMatrix
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function()(inv)
  list(set = set, get =get, getInverse = getInverse,setInverse = setInverse)
}

cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)  ##expression toreturn the inverse
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$getInverse(inv)  ##Expression to get inverse from x
  inv
}
