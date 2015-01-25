## Pair of functions to calculate matrix inverse / retrieve previously cached matrix inverse




##Set, retrieve value of matrix, set value of matrix inverse
##Input: matrix (assumed invertible)
##Returns list with 4 strings referring to sub-functions 

makeCacheMatrix <- function(M = matrix())
{
  MI <- NULL
  set <- function(N)
  {
    M <<- N
    MI <<- NULL
  }
  get <- function() M
  setinverse <- function(MInverse) MI <<- MInverse
  getinverse <- function() MI
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check for existence of cached value of matrix inverse
## If inverse not found cached, calculate it using solve(), cache new value of inverse


cacheSolve <- function(M = matrix(), ...)
{
  MI <- M$getinverse()
  if (!is.null(MI))
  {
    message("getting cached data")
    return(MI)
  }
  M_input <- M$get()
  MI <- solve(M_input)
  M$setinverse(MI)
  MI
    
}

