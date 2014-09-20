## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: the function caches inverse of the matrix through setinverse()
## returns a list of available sub-functions.
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  #set <- function (mymatrix){
  #  x <<- mymatrix
  #  invx <<- NULL
  #}
  
  get <- function(){x}
  
  setinverse <- function (myinvmatrix){
    invx <<- myinvmatrix  #cache my inv data in invx
  }
  
  getinverse <- function(){
    return (invx)
  }
  
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
  
}


## cacheSolve looks up cache for the inverse of the matrix
## and calculates the inverse if cache is not found.  
## THe list of functions from MakeCacheMatrix goes in as a parameter.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse();  #lookup the cache
  
  if (!is.null(inv)){
    message("Retrieved from cache")
  }
  else{  
    #the data was not cached.  so compute inverse
    inv = solve(x$get());
    message("Cache not available. Calculated inverse")
    #cache the inverse for future lookup
    x$setinverse(inv)
  }
  
  return (inv)
}

# ## test script
# 
# mat_a <-rbind(c(1,4), c(2,5))
# mat_a
# mcm <- makeCacheMatrix(mat_a)
# cacheSolve(mcm)  #should compute
# cacheSolve(mcm)  #should use cache
# cacheSolve(mcm)  #should use cache
# mat_b=cacheSolve(mcm) #should use cache; mat_b initialized with inverse matrix
# mcm <- makeCacheMatrix(mat_b)
# cacheSolve(mcm)  #should compute
# cacheSolve(mcm)  #should use cache
# cacheSolve(mcm)  #should use cache


##sample output

# > mat_a <-rbind(c(1,4), c(2,5))
# > mat_a
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# > mcm <- makeCacheMatrix(mat_a)
# > cacheSolve(mcm)
# Cache not available. Calculated inverse
# [,1]       [,2]
# [1,] -1.6666667  1.3333333
# [2,]  0.6666667 -0.3333333
# > cacheSolve(mcm)
# Retrieved from cache
# [,1]       [,2]
# [1,] -1.6666667  1.3333333
# [2,]  0.6666667 -0.3333333
# > cacheSolve(mcm)
# Retrieved from cache
# [,1]       [,2]
# [1,] -1.6666667  1.3333333
# [2,]  0.6666667 -0.3333333
# > mat_b=cacheSolve(mcm)
# Retrieved from cache
# > mcm <- makeCacheMatrix(mat_b)
# > cacheSolve(mcm)
# Cache not available. Calculated inverse
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# > cacheSolve(mcm)
# Retrieved from cache
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# > cacheSolve(mcm)
# Retrieved from cache
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# > 