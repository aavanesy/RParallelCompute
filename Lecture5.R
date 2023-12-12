
# 5.Running Slow Codes in R

library(tictoc)

?tic
?toc

tic()
print("Do something...")
Sys.sleep(1)
toc()

# Most tutorials will show code with a timer ----
calc_slow <- function(x){
  Sys.sleep(1)
  # slow API or SQL call
  # very complex calculations (finance, pharma, stats, big data)
  return(x + 1)
}

## Simple Solution Using Loops ----
tic()
res <- c()
for(i in 1:10){
  print(i)
  res = c(res, calc_slow(i))
}
toc()
print(res_one)


# Using Base R and Apply Scripts ----
?apply
?lapply
?sapply
tic()
res_two <- sapply(1:10, calc_slow)
toc()

# Both codes are slow and inefficient
# R is built this way



