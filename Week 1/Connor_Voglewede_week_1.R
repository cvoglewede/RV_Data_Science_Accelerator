
mtcars <- mtcars
myName <- "Connor Voglewede"
mtcarsColumns <- c(colnames(mtcars))
mtcarsSummary <- summary(mtcars)
dratValue <- mtcars$drat[mtcars$cyl==6 & mtcars$mpg>21]
topQsec <- head(mtcars[order(-mtcars$qsec),],6)


