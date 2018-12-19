data <- rnorm(2000, 1) # Big enough set of numbers.
mat.data <- matrix(data, ncol = 10) # Put it up together in a matrix.
indicesForNAs1 <- sample(1:200, 100, replace = FALSE) # First column will have 500 NA's
indicesForNAs2 <- sample(1:1750, 250, replace = FALSE) # 500 more NA's randomly present in the matrix.
indicesforNAs <- c(indicesForNAs1, indicesForNAs2)
mat.data[indicesforNAs] <- NA
df.data <- data.frame(mat.data)
names(df.data) <- paste("Variable",LETTERS[1:10])

missingValPlot <- function(df) {
ax1 <- names(df)[c(T,F)] # Creating the top axis
ax2 <- names(df)[c(F,T)] # Creating the bottom axis

image(1:ncol(df), 1:nrow(df), t(df), col = "indianred3", axes = FALSE,
      xlab = "", ylab = "") # Making a heatmap
rect(0.5, 0, ncol(df) + 0.5, nrow(df)) # Surrounding the map with a border
title("Plot for Missing Values", line = 2.5) # Setting title
axis(1, seq(1,length(names(df)),2), ax1, line = 0.05, cex.axis = 0.75) # Plotting axes
axis(3, seq(2,length(names(df)),2), ax2, line = -0.05, cex.axis = 0.75)
for (i in 1:9) {
  abline(v = i + 0.5, col = "white") # Creating white column separators
}
par(xpd = TRUE)
legend(-1,nrow(df) + 25,c("Available", "Missing"), fill = c("indianred3", "white"),
       cex = 0.65)
par(xpd = FALSE)
}

missingValPlot(df.data)