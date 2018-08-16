#' Reads in the data in the file named filename
#' 
#' @param filename The name of the file of data
#' @return The data in filename, an Nx3 matrix.
dataLoad <- function(filename) {
  # Insert your code here
  dataframe <- read.table(filename, sep=" ")
  data <- data.matrix(dataframe)
  todelete <- rep(TRUE, nrow(data))
  for (row in 1:nrow(data)) {
    temp <- data[row, 1]
    growthrate <- data[row, 2]
    bacteria <- data[row, 3]
    if (temp < 10 || temp > 60) {
      print(paste("Error on line", row, "Temperature must be between 10 and 60, inclusive. The temperature on this line was", temp, sep=" "))
      todelete[row] <- FALSE
    }
    
    if (growthrate < 0) {
      print(paste("Error on line", row, "Growth rate must be a positive number. The growth rate on this line was", growthrate, sep=" "))
      todelete[row] <- FALSE
    }
    
    if (bacteria != 1 && bacteria != 2 && bacteria != 3 && bacteria != 4) {
      print(paste("Error on line", row, "Bacteria must be labeled 1,2,3,or 4. The bacteria on this line was", bacteria, sep=" "))
      todelete[row] <- FALSE
    }
  }
  return(data[todelete,])
}

dataStatistics <- function(data, statistic) {
  # Insert your code here
  return(result)
}

#' @description Displays two plots:
#'  1. "Number of bacteria": A bar plot of the number of
#'     Bacteria in the data
#'  2. "Growth rate by temperature": A plot with the
#'     Tempurature on the x-axis and the Growth rate on the
#'     y-axis. The x-axis must go from 10 to 60 degrees, and
#'     the y-axis must start from 0. The plot should contain
#'     a single axis with four graphs, one for each type of
#'     Bacteria. The different graphs must be distinguished
#'     using e.g. different colors, markers, or line styles
#'     
#' @param data An Nx3 matrix with columns Temperature, 
#'             Growth rate, and Bacteria
dataPlot <- function(data) {
  # Insert your code here
}

done <- FALSE
action <- ""
menu <- "1. Load data\n2. Filter data\n3. Display statistics\n4. Generate plots\n5.quit"
cat(menu)

while (!done) {
  action <- suppressWarnings(as.numeric(readline("Enter the number of which action you would like to perform:")))
  if (is.na(action)) {
    cat("Not a valid input. Please input a number from 1 to 5.\n")
  } else if (action == 1) {
    
  } else if (action == 2) {
    
  } else if (action == 3) {
    
  } else if (action == 4) {
    
  } else if (action == 5) {
    done <- TRUE
  } else {
    cat("Not a valid input. Please input a number from 1 to 5.\n")
  }
}

5