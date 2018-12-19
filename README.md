# ttplot

While working on a statistical studies I realized that while the R language is very robust with visualization- and definitely beats Python at it- it can be sometimes a bit cumbersome and difficult to finetune specialized graphics that come bundled with packages like ggplot2 or the many others you can find on CRAN/GitHub. Therefore, I sometimes prefer creating plotting functions from scratch that absolutely suit my needs. `ttplot` is my opensource package for these functions. Whenever I come across a need to create a graphic that I cannot find in standard packages like ggplot2, I code a function to do that. I will keep documenting and adding such functions to this package in the future. 

## Installation

Make sure you have devtools installed and loaded. You can do that by running `install.packages("devtools")` followed by `library(devtools)`. Next, you can install `ttplot` by
```r 
devtools::install_github(“ttehseen/bowlR”)
`library(ttplot)`
```

## Functions

Here are a few functions contained in the package:

### `missingValPlot()`

`missingValPlot()` is a very handy function that visualizes the NA's in a data frame. It takes a dataframe as an argument and, for each columns, colors the missing values with white and the available values with red in a chart. This gives you an overall view on the 'completeness' of the data frame. It also helps spot any glaring irregularities in the data- for instance, in the example below, column A has around 25% missing values. It also lets you compare within different columns and, with two plots, within separate dataframes.

Let us see a use case for the plot. Let's generate a dataframe of random numbers and replace some of these numbers with NA values. Then let's see how helpful the `head()` and `summary` functions are and whether our visualization helps our understanding of the data's completeness.

#### Simulating some data.
```R
data <- rnorm(2000, 1) # Big enough set of numbers.
mat.data <- matrix(data, ncol = 10) # Putting it up together in a matrix.
indicesForNAs1 <- sample(1:150, 100, replace = FALSE) # First column will have 500 NA's
indicesForNAs2 <- sample(1:1850, 250, replace = FALSE) # 500 more NA's randomly scattered in the matrix.
indicesforNAs <- c(indicesForNAs1, indicesForNAs2)
mat.data[indicesforNAs] <- NA # Assigning the NA's
df.data <- data.frame(mat.data) # Converting to a dataframe
names(df.data) <- paste("Variable",LETTERS[1:10]) # Assigning column names

head(df.data) # Let's see what the data looks like according to good old head
summary(df.data) # and summary functions
```
```
> head(df.data)
   Variable A Variable B Variable C Variable D  Variable E Variable F  Variable G Variable H
1  1.64802215  2.4335060 -0.1447880  1.3059162  1.78609281  1.7066910  0.51570199  3.0358122
2          NA  0.5264305  1.5610882 -0.2961850  1.86390605         NA  3.00806673  1.4414812
3  1.00052985         NA  1.4622146         NA  0.01786125  0.2043386  1.30614575  0.3765237
4 -0.05184347  2.2128741 -0.3006032  0.6406179  1.79527366  0.8716912  0.59883426  1.9758739
5          NA  1.5622239  0.4876409  0.2680735 -0.29057645         NA          NA  2.1894105
6  0.32468156         NA         NA  2.0838818          NA -0.1888339 -0.03342089  2.8410463
   Variable I   Variable J
1  2.53281576           NA
2          NA  0.650746110
3          NA           NA
4 -0.04209843  1.077944390
5  0.01292377 -0.008865424
6  1.29793244  0.358627911
> summary(df.data)
   Variable A        Variable B       Variable C        Variable D        Variable E     
 Min.   :-1.0851   Min.   :-1.559   Min.   :-1.7662   Min.   :-2.1081   Min.   :-1.8260  
 1st Qu.: 0.2245   1st Qu.: 0.452   1st Qu.: 0.2893   1st Qu.: 0.4246   1st Qu.: 0.4137  
 Median : 0.7864   Median : 1.009   Median : 1.0561   Median : 1.0178   Median : 1.3170  
 Mean   : 0.8365   Mean   : 1.066   Mean   : 1.0442   Mean   : 1.0550   Mean   : 1.1567  
 3rd Qu.: 1.4285   3rd Qu.: 1.757   3rd Qu.: 1.6826   3rd Qu.: 1.6177   3rd Qu.: 1.8306  
 Max.   : 3.0579   Max.   : 4.451   Max.   : 5.1757   Max.   : 3.5021   Max.   : 3.7195  
 NA's   :109       NA's   :24       NA's   :38        NA's   :29        NA's   :29       
   Variable F        Variable G        Variable H        Variable I        Variable J     
 Min.   :-1.4850   Min.   :-1.6648   Min.   :-2.1387   Min.   :-1.2580   Min.   :-1.1335  
 1st Qu.: 0.3259   1st Qu.: 0.2952   1st Qu.: 0.3250   1st Qu.: 0.3627   1st Qu.: 0.2354  
 Median : 1.0264   Median : 0.9873   Median : 0.8928   Median : 1.0033   Median : 0.9735  
 Mean   : 1.0077   Mean   : 1.0142   Mean   : 0.9596   Mean   : 1.0138   Mean   : 0.8922  
 3rd Qu.: 1.6439   3rd Qu.: 1.7042   3rd Qu.: 1.5698   3rd Qu.: 1.6428   3rd Qu.: 1.4692  
 Max.   : 3.7163   Max.   : 3.9517   Max.   : 3.2725   Max.   : 3.6735   Max.   : 3.3067  
 NA's   :28        NA's   :27        NA's   :20        NA's   :28        NA's   :7        
> 
```

Now we can infer from this that the NA values seems to be pretty randomly present throughout the data frame. `Variable A` and `Variable J` are a bit different because the former has more and the latter less number of NA's than the other columns. However, we don't know much about how the NAs are distributed within each column. Now this is important information because many ML/other functions in R drop an entire row if an NA value is present. The distribution of NA's can very much determine how much data we lose when using the aforementioned functions.

Now let's introduce our function.

#### Writing the function.

```R
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
```
No external packages are used and the plot will be easily customizeable. 

#### The plot
Let's run and see the results.

```R
missingValPlot(df.data)
```

We can see from the plot that the NA values for `Variable A` are significantly greater in number than the rest. It might be a good idea to drop this column in any predictive model. We can also see that for `Variable J`, the NA values are all present in the bottom half of the dataset. There might have been some error during data collection that corrupted values for that part, assuming that the data is arranged chronologically. For the remaining columns, the NA's are present randomly and sparsely. This is very good insight that we did not get from just looking at the summary. A graph like this can be very good for feature analysis before designing a model. It can inform your decision on which columns to drop for the model.

### correlationPlot()

Another useful plot function I wrote for the package is to visualize the correlation between variables. I find this plot especially useful when making decisions about which variables to select for a regression model. In regression, choosing two predictor variables that are strongly correlated, or collinear, can lead to a misleading model, and this should be avoided. Thus, a plot produced by `correlationPlot()` can inform that decision.

#### Simulating some data.

We can generate a matrix of random numbers with each column representing a different variable. We can then get the absolute value for the correlation between each set of variables and store that in a matrix- in this case called `abs.corr`.

```R
A <- matrix(sample(100, 1000, replace = T), ncol = 10) # A matrix with random numbers
corr <- round(cor(A),3)  # Getting the correlation values
corr.abs <- abs(corr)
colnames(corr.abs) <- LETTERS[1:ncol(corr.abs)] # Assigning column names
rownames(corr.abs) <- LETTERS[1:ncol(corr.abs)] # and row names

corr.abs # Let's have a look
```
```
> corr.abs
      A     B     C     D     E     F     G     H     I     J
A 1.000 0.062 0.009 0.185 0.091 0.200 0.091 0.022 0.038 0.048
B 0.062 1.000 0.046 0.161 0.005 0.002 0.102 0.079 0.088 0.019
C 0.009 0.046 1.000 0.078 0.048 0.092 0.012 0.161 0.109 0.021
D 0.185 0.161 0.078 1.000 0.009 0.106 0.023 0.012 0.136 0.066
E 0.091 0.005 0.048 0.009 1.000 0.024 0.059 0.084 0.125 0.002
F 0.200 0.002 0.092 0.106 0.024 1.000 0.065 0.088 0.052 0.002
G 0.091 0.102 0.012 0.023 0.059 0.065 1.000 0.139 0.015 0.079
H 0.022 0.079 0.161 0.012 0.084 0.088 0.139 1.000 0.045 0.071
I 0.038 0.088 0.109 0.136 0.125 0.052 0.015 0.045 1.000 0.091
J 0.048 0.019 0.021 0.066 0.002 0.002 0.079 0.071 0.091 1.000
```

That is a lot of numbers. At first glance it is very difficult to compare the correlations. Let's see if our plot can help us.

#### Writing the function.

```R
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
```
Again, everything is coded in base R so it should be relatively easy to manipulate my code and customize the plot for any specific needs.

#### The plot.

```R
correlationPlot(corr.abs)
```

We get a nice grid of circles where each circle is filled with a shade of red that corresponds to the correlation between the variables it is representing. The correlation is written inside the box as well. Now since we used completely random data, we expectedly, do not see much correlation between our variables. For the sake of visualization, let us manipulate our correlation matrix a bit and bump up the values to see what the plot would have looked like had our data not been completely random and uncorrelated.

```R
for (i in 1:10) {
  for (j in 1:10) {
    ifelse(corr.abs[i,j] < 0.25, corr.abs[i,j] <- 4 * corr.abs[i,j] , 1)
  }
}

correlationPlot(corr.abs)
```
The plot makes is much easier to visualize the correlation matrix!

![Batplot1](https://github.com/ttehseen/bowlR/blob/master/imgs/batPlot1.png)

I will update the package with more graphics functions as well as a few predictive ones using Machine Learning methods in R.
