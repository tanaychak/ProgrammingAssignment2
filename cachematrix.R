## makeCacheMatrix creates the functions that can cache the inverse of a matrix

## set sets the matrix, get gets the matrix, setinv sets inverse of the matrix, getinv gets inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv=NULL
  
  set = function(y){
    x<<-y
    inv<<-NULL
  }

  get = function() x
  setinv=function(inverse) inv<<-inverse
  getinv=function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## assuming the matrix is convertible, check if inverse is computed.
## if already computed, return from cache
## if not computed, calculate the inverse and set value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
