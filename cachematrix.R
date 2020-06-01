## The function written below takes a matrix as input and
## set a matrix and get a matrix
## set matrix inverse and get matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){inv<<-inverse}
  getinverse<-function(){inv}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function written below calculates the inverse of the "matrix" created with the above function. 
## Firstly, It checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse value in the cache.


cacheSolve <- function(x, ...) {
       
  inv<-x$getinverse()
  if(!is.null(inv)){
    massage("getting chached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setinverse(inv)
  inv
  }
