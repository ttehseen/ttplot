A <- matrix(sample(100, 1000, replace = T), ncol = 10) # A matrix with random numbers
corr <- round(cor(A),3)  # Getting the correlation values
corr.abs <- abs(corr)
colnames(corr.abs) <- LETTERS[1:ncol(corr.abs)] # Assigning column names
rownames(corr.abs) <- LETTERS[1:ncol(corr.abs)] # and row names

corr.abs # Let's have a look

# The function
correlationPlot <- function(corr) {
  f <- colorRamp(c("white", "indianred3")) # Creating a color gradient
  colors <- rgb(f(corr.abs)/255)
  ax <- colnames(corr) # Axes labels for the plot
  mat.length <- dim(corr)[1]
  par(xpd = TRUE) # To position the legend out of the plot
  symbols(x = rep_len(1:mat.length, mat.length**2), 
          y = rep(1:mat.length, each = mat.length), 
          circles = rep(1, mat.length**2), inches = 0.175, bg = colors, 
          xlab = "", ylab = "", main = "Correlation between variables",
          axes = F)
  axis(1, 1:mat.length, ax, las = 2, line = -1.5)
  axis(2, 1:mat.length, ax, las = 2, line = -1.5)
  text(x = rep_len(1:mat.length, mat.length**2), 
       y = rep(1:mat.length, each = mat.length), 
       labels = corr, cex = 0.5)
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
