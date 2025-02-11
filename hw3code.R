
numbers <- c(6, 10, 14, 15, 21, 
             22, 26, 33, 34, 35, 
             38, 39, 46, 51, 55, 
             57, 58, 62, 65, 69, 
             75, 77, 82, 85, 86, 
             87, 91, 93, 94, 95)
factors_df <- data.frame(Number = numeric(),
                 factor_1 = numeric(), 
                 factor_2 = numeric(),
                 factor_3 = numeric (),
                 stringsAsFactors=FALSE) 

prime_factors_Loop <- function(x) {
  factors_for_x <- c()   # Vector that will store factors for the value of x
  i <- 2           # Start checking factors from two
  
  while (x >= i) {  # Continue looping while x is greater than or equal to i
    if (x %% i == 0) {   # If i is a factor of x 
    factors_for_x <- c(factors_for_x, i)  # Add i to the list of factors
      x <- x / i  # Divide x by i to remove that factor
    } else {
      i <- i + 1  # If i is not a factor, move to the next number
    }
  }
 return(factors_for_x) #returns the vector containing the prime factors for x
}
for(j in 1:length(numbers)){
  x = numbers[j]
  factors_for_x <- prime_factors_Loop(x)
  
new_row <- data.frame(Number = numbers[j],
                      factor_1 = factors_for_x[1],
                      factor_2 = factors_for_x[2],
                      factor_3 = factors_for_x[3],
                      stringsAsFactors = FALSE)

factors_df <- rbind(factors_df, new_row)
}