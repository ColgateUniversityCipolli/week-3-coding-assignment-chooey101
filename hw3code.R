
numbers <- c(6, 10, 14, 15, 21, #establishes a vector containing the numbers referenced within the problem
             22, 26, 33, 34, 35, 
             38, 39, 46, 51, 55, 
             57, 58, 62, 65, 69, 
             75, 77, 82, 85, 86, 
             87, 91, 93, 94, 95)
factors_df <- data.frame(Number = numeric(), #Creates an empty data frame where we will store every number's prime factors
                 factor_1 = numeric(), 
                 factor_2 = numeric(),
                 factor_3 = numeric (),
                 stringsAsFactors=FALSE) 

prime_factors_Loop <- function(x) { #establishes a function that when called will output a vector containing the factors of x
  factors_for_x <- c()   # Vector that will store factors for the value of x
  i <- 2           # Start checking factors beginning two
  
  while (x >= i) {  # Continue looping while x is greater than or equal to i
    if (x %% i == 0) {   # If i is a factor of x 
    factors_for_x <- c(factors_for_x, i)  # Add i to the list of factors
      x <- x / i  # Divide x by i to remove that factor
    } else {
      i <- i + 1  #if i is not a factor, move to the next number
    }
  }
 return(factors_for_x) #returns the vector containing the prime factors for x
}
for(j in 1:length(numbers)){ #loop that stores each number and their respective factors in the factors_df dataframe
  x = numbers[j]
  factors_for_x <- prime_factors_Loop(x)
new_row <- data.frame(Number = numbers[j],
                      factor_1 = factors_for_x[1],
                      factor_2 = factors_for_x[2],
                      factor_3 = factors_for_x[3],
                      stringsAsFactors = FALSE)

factors_df <- rbind(factors_df, new_row)  #data frame containing each number within the vector, along with their respective prime factors
}
#After examining data frame, 75 is the only number with more than two factors, and therefore is the incorrect number
factors_for_74 <- prime_factors_Loop(74) #Runs the loop to check factors for 74 instead, outputting a pair of two factors that are prime and unique
#After examining the dataframe, we've observed 75's position as 21, so we can now replace it's values with that of the correct number
factors_df$Number[21] <- 74 #replaces 75 with the correct number, 74
factors_df$factor_1[21] <- factors_for_74[1] #Replaces factor 1 for 74
factors_df$factor_2[21] <- factors_for_74[2] #Replaces factor 2 for 74
factors_df$factor_3[21] <- NA #Eliminates factor 3 as no numbers have more than two factors
View(factors_df) # Displays the factors_df dataframe which includes all correct numbers from the given list and their respective factors