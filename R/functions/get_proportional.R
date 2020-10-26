get_proportional <- function(x){
  x <- x/sum(x, na.rm = TRUE)
  return(x)
}
