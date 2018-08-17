#' @author Megan Wilkinson
#' @description Reads in the data in the file named filename
#' 
#' @param filename The name of the file of data
#' @return The data in filename, an Nx3 matrix.
dataLoad <- function(filename) {
  # Insert your code here
  dataframe <- read.table(filename, sep=" ",header = FALSE)
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


#' @author Megan Wilkinson
#' @description The main program script
done <- FALSE
action <- ""
data <- NULL

# const variables
statistic.v <- c("’Mean Temperature", "Mean Growth rate", "Std Temperature", "Std Growth rate", "Rows", "Mean Cold Growth rate", "Mean Hot Growth rate")
statistics.menu <- "1. Mean (average) temperature\n2. Mean (average) growth rate\n3.Standard deviation of temperature\n4. Standard deviation of growth rate\n5. The total number of rows in the data\n6. Mean (average) growth rate when temperature is less than 20 degrees\n7. Mean (average) growth rate when temperatureis greater than 50 degrees"
menu <- "1. Load data\n2. Filter data\n3. Display statistics\n4. Generate plots\n5. quit"

cat(menu)

while (!done) {
  action <- suppressWarnings(as.numeric(readline("Enter the number of which action you would like to perform:")))
  if (is.na(action)) { # handle NaNs
    cat("Invalid input. Please input a number from 1 to 5.\n")
  } else if (action == 1) { # load data
    dataname <- readline("Enter the name of the data file:")
    if (file.exists(dataname)) {
      data <- dataLoad(dataname)
    } else {
      cat("No such file exists")
    }
  } else if (action == 2) { # filter data
    filterType <- suppressWarnings(as.numeric(readline("Enter the filter type you would like to apply (1. bacteria or 2. growthrate):")))
    if (is.na(action)) {
      cat("Invalid input. Please input 1(bacteria) or 2(growth rate)")
    } else if (filterType == 1) { 
      if (is.null(data)) {
        cat("Please input data.")
      } else {
        
      }
    } else if (filterType == 2) {
      if (is.null(data)) {
        cat("Please input data.")
      } else {
        
      }
    }
  } else if (action == 3) { # display statistics
    cat("Enter one of the following numbers to show a statistic:\n")
    cat(statistics.menu)
    statistic <- suppressWarnings(as.numeric(readline()))
    if (is.na(statistic) || statistic < 1 || statistic > 7) {
      cat("Invalid input. Please input a number 1 to 7")
    } else {
      if (is.null(data)) {
        cat("Please input data.")
      } else {
        dataStatistics(data, statistic.v[statistic])
      }
    } 
  } else if (action == 4) { # generate plots
    dataPlot(data)
  } else if (action == 5) { # quit
    done <- TRUE
  } else { # handle out of range numbers
    cat("Not a valid input. Please input a number from 1 to 5.\n")
  }
}
