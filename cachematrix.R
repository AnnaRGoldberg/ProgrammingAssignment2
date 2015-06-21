## Put comments here that give an overall description of what your
## functions do
  ##These function create a special matrix object with caches its inverse. 
  ##Function makeCacheMatrix defines the value of the matrix, clears the old cache, 
  ##defines a function to get the inverse of the matrix and returns a list with,
  ##setinverse, getinverse, get and the matrix of y.

## create a special matrix object that can cache it's inverse
## Write a short comment describing this function

> makeCacheMatrix<-function(x=matrix()){
  + inv<<-NULL
  ##Deinfes the function to set the value of the matrix
  ##clears the old inverse from the cache
  + set<-function(y){
    + x<<-y ##set the value of the matrix
    + inv<<-NULL ##clear the cache
    + }
  + get<-function()x ##Define the function to get the value of the matrix
  + setinverse<-function(inverse)inv<<-inverse ##define the function to set the
  ##the inverse. 
  + getinverse<-function()inv ##define the function to get the inverse
  + list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  ## returns a list with the four functions listed above.
  + }




## Write a short comment describing this function

## this function returns the inverse of matrix (x).
## in this function, if the matrix has already been 
## calculated (and the matrix has not changed), then
## cachesolve retrieves the inverse matrix from the cache
## otherwise the function below calculates the inverse of 
## the matrix and sets the inverse of the matrix in the cache
## with the setinverse function


cachesolve<-function(x,...){
+ inv<-x$getinverse() ## this gets the cached valuue for the inverse (if it is there)
+ if(!is.null(inv)){ ##if the cache is empty, we puull it anyway
+ message("getting cached data")
+ return(inv)
+ }
## if the cache is empty, we need to calculate the inverse, cache it
## and show the inverse as the resule.
+ data<-x$get() ## get the value of the matrix
+ inv<-solve(data,...) ## calculate the inverse
+ x$setinverse(x) ## set the inveres in the cache
+ x ## show the inverse
+ }

