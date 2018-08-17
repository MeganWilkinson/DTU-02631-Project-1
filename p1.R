#' @author Megan Wilkinson
#' @description Reads in the data in the file named filename
#' 
#' @param filename The name of the file of data
#' @return The data in filename, an Nx3 matrix.
dataLoad <- function(filename) {
  ## get data from space-seperated table and turn it into a matrix
  dataframe <- read.table(filename, sep=" ", header=FALSE)
  data <- data.matrix(dataframe)
  colnames(data) <- c("Temperature", "Growthrate", "Bacteria")

  todelete <- rep(TRUE, nrow(data)) # vector to keep track of invalid rows to remove
  
  ## find invalid rows
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

#' @author Megan Coleman
#' @description Calculates statistics based on a matrix of data
#' 
#' @param data, statistic describe the matrix data set and statistic sought
#' @return The calculated statistic
dataStatistics <- function(data, statistic) {
  # access the different columns of data as vectors
  temps     <- data[,1] # temperature data
  growth    <- data[,2] # growth rate data
  bacteria  <- data[,3] # bacteria    data
  
  if(statistic == "Mean Temperature")     # calculates vector mean with NA values
    result <- mean(temps, trim=0, na.rm=FALSE) 
  
  else if(statistic == "Mean Growth rate") # calculates vector mean with NA values
    result <- mean(growth, trim=0, na.rm=FALSE)
  
  else if(statistic == "Std Temperature")
    result <- d(temps, na.rm = FALSE)
  
  else if(statistic == "Std Growth rate") 
    result <- sd(growth, na.rm = FALSE)
  
  else if(statistic == "Rows")
    result <- nrow(data)
  
  else if(statistic == "Mean Cold Growth Rate")  # calculates vector mean withOUT NA values
    result <- mean(temps < 20 , trim=0, na.rm=TRUE)
  
  else if(statistic == "Mean Hot Growth rate")  # calculates vector mean withOUT NA values
    result <- mean(temps > 50 , trim=0, na.rm=TRUE)
  
  return(result)
}

#' @author Megan Coleman
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
  # Number of Bacteria Bar Plot
  num1 <- nrow(data[data[,3] == 1,,drop=FALSE])
  num2 <- nrow(data[data[,3] == 2,,drop=FALSE])
  num3 <- nrow(data[data[,3] == 3,,drop=FALSE])
  num4 <- nrow(data[data[,3] == 4,,drop=FALSE])
  counts <- c("Salmonella enterica"=num1, "Bacillus cereus"=num2, "Listeria"=num3, "Brochothrix thermosphacta"=num4)
  barplot(counts, xlab="Bacteria", ylab="Number of Bacteria")
  title("Number of Bacteria Bar Plot")
  
  # Growth Rate by Temp Line Chart
  colors <- rainbow(4)
  linetype <- c(1:4)
  plotchar <- seq(18, 22,1)
  b1 <- data[data[,3] == 1,,drop=FALSE]
  b1 <- b1[order(b1[,1]),]
  y1 <- b1[,2]
  x1 <- b1[,1]
  b2 <- data[data[,3] == 2,,drop=FALSE]
  b2 <- b2[order(b2[,1]),]
  y2 <- b2[,2]
  x2 <- b2[,1]
  b3 <- data[data[,3] == 3,,drop=FALSE]
  b3 <- b3[order(b3[,1]),]
  y3 <- b3[,2]
  x3 <- b3[,1]
  b4 <- data[data[,3] == 4,,drop=FALSE]
  b4 <- b4[order(b4[,1]),]
  y4 <- b4[,2]
  x4 <- b4[,1]
  # x and y axis range
  xrange <- c(10,60)
  yrange <- c(0,max(data[,2]))
  
  # the plot
  plot(x1,y1,type = "o", col = "red", xlab = "Temperature in Celcius", ylab = "Growth Rate",
       main = "Bacteria Growth", xlim=xrange, ylim = yrange)
  lines(x2,y2, type="o", col="blue")
  lines(x3,y3, type="o", col="green")
  lines(x4,y4, type="o", col="purple")
  legend(xrange[1], yrange[2], cex=0.8, col = colors, pch=plotchar, title="Bacteria", legend=c("Salmonella enterica", "Bacillus cereus", "Listeria", "Brochothrix thermosphacta"))

}


#' @author Megan Wilkinson
#' @description The main program script
done <- FALSE
action <- ""
data <- NULL

## const variables
statistic.v <- c("’Mean Temperature", "Mean Growth rate", "Std Temperature", "Std Growth rate", "Rows", "Mean Cold Growth rate", "Mean Hot Growth rate")
statistics.menu <- "1. Mean (average) temperature\n2. Mean (average) growth rate\n3.Standard deviation of temperature\n4. Standard deviation of growth rate\n5. The total number of rows in the data\n6. Mean (average) growth rate when temperature is less than 20 degrees\n7. Mean (average) growth rate when temperatureis greater than 50 degrees"
menu <- "1. Load data\n2. Filter data\n3. Display statistics\n4. Generate plots\n5. quit"

cat(menu) # print main menu

## loop until user ends the script
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
    if (is.null(data)) {
      cat("Please input data first.")
      next
    } else {
      filterType <- suppressWarnings(as.numeric(readline("Enter the filter type you would like to apply:\n1. Filter by Bacteria\n2. Filter by Growth Rate:")))
      if (is.na(action) || (filterType != 1 && filterType != 2)) {
        cat("Invalid input. Please input 1(bacteria) or 2(growth rate)")
      } else if (filterType == 1) { 
        bacteria <- suppressWarnings(as.numeric(readline("Enter which bacteria you would like to filter for(e.g. only show Listeria):\n1. Salmonella enterica\n2. Bacillus cereus\n3. Listeria\n4. Brochothrix thermosphacta")))
        data <- data[data[,3] == bacteria,,drop=FALSE]
      } else if (filterType == 2) {
        lowerbound <- suppressWarnings(as.double(readline("Enter the LOWERBOUND for growth rates you would like to filter for:")))
        upperbound <- suppressWarnings(as.double(readline("Enter the UPPERBOUND for growth rates you would like to filter for:")))
        data <- data[data[,2] >= lowerbound,,drop=FALSE]
        data <- data[data[,2] <= upperbound,,drop=FALSE]
      }
    }
  } else if (action == 3) { # display statistics
    if (is.null(data)) {
      cat("Please input data first.")
    } else {
      cat("Enter one of the following numbers to show a statistic:\n")
      cat(statistics.menu)
      statistic <- suppressWarnings(as.numeric(readline()))
      if (is.na(statistic) || statistic < 1 || statistic > 7) {
        cat("Invalid input. Please input a number 1 to 7")
      } else {
        dataStatistics(data, statistic.v[statistic])
      } 
    }
  } else if (action == 4) { # generate plots
    if (is.null(data)) {
      cat("Please input data first.")
    } else {
      dataPlot(data)
    }
  } else if (action == 5) { # quit
    done <- TRUE
  } else { # handle out of range numbers
    cat("Not a valid input. Please input a number from 1 to 5.\n")
  }
}
