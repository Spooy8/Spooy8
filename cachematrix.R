## My function is getting and caching the inverse of a matrix.

## makeCasheMatrix is actually 2 function combined.
## the first one create a special matrix, and 
## the second one caches it inverse .

makeCacheMatrix <- function(x = matrix()) {
  makeinverse <- NULL
  set<- function(y){
    x<<-y
    makeinverse<<- NULL
  }
  
  get<-function()x
  setinverse<-function(inverse) makeinverse<<-inverse
  getinverse<- function() makeinverse
  list(set=set, get=get,setinverse=setinverse,getinverse+getinverse)
  
}


## this function check if the inverse was calculated before in the cache
## and return it ,if no it calculate it using solve() function. 

cacheSolve <- function(x, ...){
  makeinverse<- x$getinverse()
  if(!is.null(makeinverse)){
    print("getting cache data")
    return(makeinverse) 
  }
  
  data<-x$get()
  makeinverse<- solve(data, ...)
  x$setinverse(makeinverse)
  makeinverse
}
