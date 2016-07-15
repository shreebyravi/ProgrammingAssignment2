
## Creating a function that sets and gets the matrix and the value of the matrix


## This is used to illustrate lexical scoping where caching variables are used to save time in computations
## x and inv variables are defined in set,setInverse (their 
## environment) and fetched from the same using get and getInverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##creation of the matrix and changing the inverse matrix value if original value is changed
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ##calculation of matrix inverse
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ##passing the value of the function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Used to fetch the cache of the matrix

cacheSolve <- function(x, ...) {
  ##retrieving th einverse of the matrix
  inv <- x$getInverse() 
  #if the matrix was already cached then the same is fetched
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##else the inverse of the matrix is computed
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
cachemat.txt
Open
Displaying cachemat.txt.
