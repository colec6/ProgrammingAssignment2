## These functions are a part of the RProgramming module of the Data Science 
## Track from John's Hopkins university on Coursera They accept a square, 
## non-singular (i.e. invertible) matrix object and then calculate the inverse. 
## This inverse is then cached for retrival later if required.  It should be 
## called in the form x<-makeCacheMatrix(matrix(1:4,2,2)) and then 
## cachesolve(x).  If it is desired to alter the matrix x then this can be done 
## by using the function call x$set(new matrix), similarly with other functions 
## in the list declared in cacheMatrix (i.e. to view the value of the current
## matrix x x$get()).

## This functiuon accepts a square, non-singular matrix as input and creates a
## list containing several functions to set a new value for the matrix argument,
## det the matrix argument, set the cached inverse value for the matrix argument
## or get the cached inverse value for the matrix argument.
makeCacheMatrix<-function(x=matrix()){
  # on first run set the value of inv to null
  inv<-NULL
  # declare a function to allow changing the matrix 
  # setting x equal to the new matrix and inv equal to null again
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  # declare the function to retrieve the matrix stored in x
  get<-function() x
  # declare a function to store the computed inverse in m
  setinv<-function(solve) inv<<-solve
  # declare a function to retrieve the computed inverse from variable m
  getinv<-function() inv
  # create a list containing the functiuons declared above  
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
  
}


## this function retrieves the current value of inverse and checks to see if it
## is equal to null (i.e. the inverse has yet to be calculated).  If it is then
## the inverse is calculated and then cached for retrieval later, if the inverse
## is not equal to null then the cached inverse value is retrieved.
cacheSolve<-function(x){
  # get the current value of the inverse and assign to variable inv
  inv<- x$getinv()
  # test to see if inv==NULL
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # assign the matrix to the variable data using the get function
  data<-x$get()
  # Compute the inverse of the matrix and assign to variable inv
  # note the use of %*% to perform matrix multiplication
  inv<-solve(data) %*% data
  # set the value of inv as equal to the inverse
  x$setinv(inv)
  # return the inverse
  inv
}