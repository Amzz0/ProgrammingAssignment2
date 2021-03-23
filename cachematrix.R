## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## WE have 2 function MakeCacheMatrix,MakeCacheMatrix
##makechacematrix consists of get,set,stinv, getinv
##Library(MASS) is used to calculate inverse for non square as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                                  ##inverse as NULL
  set<-function(y){
                   x<<-y
                   inv<<-NULL
  }
  get<-function()x                            ##to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
                     inver<-ginv(x)
                     inver%*%x               ##to obtain inverse of a matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       geting = getinv)

}
## Write a short comment describing this function
## Use to get cache data

cacheSolve <- function(x, ...) {
  inv<-x$getinv()                                    ##have Cache data
  if(!is.null(inv)) {                                ##Check whether is null
                    message("getting chated data!")
                    return(inv)                      ##for inverse value
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv                                               ## Return a matrix that is the inverse of 'x'
}
