library(xlsx)
library(plyr)

consoles <- read.xlsx("Console.xlsx", sheetIndex = 1, as.data.frame = T)
#colnames(consoles)
consoles$Volume = consoles$Width * consoles$Height * consoles$Length

isNumericIndex <- colwise(is.numeric)(consoles)
consoles[,colnames(consoles) != "Price" & isNumericIndex & colnames(consoles) != "Volume" & colnames(consoles) != "Weight"] = consoles[,colnames(consoles) != "Price" & isNumericIndex]/consoles$Price

std_func <- function(x) {
  if (is.factor(x)) as.character(x)
  else (x-mean(x))/(max(x)-min(x))
}

std_consoles <- colwise(std_func)(consoles)
colnames(std_consoles)[colnames(std_consoles) == "Type."] <- "Type"
std_consoles <- subset(std_consoles, select = -c(Width, Height, Length))

write.csv(std_consoles, file = "Console_Std.csv")
