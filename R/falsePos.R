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

# Correlation Plot





A <- matrix(sample(100, 1000, replace = T), ncol = 10)
corr <- round(cor(A),3) # We use the Pearson correlation coefficient
corr.abs <- abs(corr)
colnames(corr.abs) <- LETTERS[1:ncol(corr.abs)]
correlationPlot <- function(corr) {
  
  f <- colorRamp(c("white", "indianred3")) # Creating a color gradient
  colors <- rgb(f(corr.abs)/255)
  ax <- colnames(corr) # Axes labels for the plot
  mat.length <- dim(corr)[1]
  par(xpd = TRUE) # To position the legend out of the plot
  symbols(x = rep_len(1:mat.length, mat.length**2), 
          y = rep(1:mat.length, each = mat.length), 
          circles = rep(1, mat.length**2), inches = 0.1, bg = colors, 
          xlab = "", ylab = "", main = "Correlation between variables",
          axes = F)
  axis(1, 1:mat.length, ax, las = 2, line = -1.5)
  axis(2, 1:mat.length, ax, las = 2, line = -1.5)
  legend(-2,12.5,c("No Correlation", "Perfect Correlation"), 
         pch = 21, pt.bg = c("white", "indianred3"), pt.cex = 2,
         cex = 0.8)
  par(xpd = FALSE) 
}

correlationPlot(corr.abs)


for (i in 1:10) {
  for (j in 1:10) {
    ifelse(corr.abs[i,j] < 0.25, corr.abs[i,j] <- 4 * corr.abs[i,j] , 1)
  }
}

correlationPlot(corr.abs)