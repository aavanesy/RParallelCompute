
## Example of parallel computing where input is not a list but a dataframe ----
library(purrr)

df <- data.frame(
  x = 1:10,
  y = 10:1)

myfun <- function(x, y){
  z = x * y
  return(z)
}

# returns a list
res <- pmap(df, myfun)

# unlists returns numeric values 
res2 = pmap_dbl(df, myfun)




