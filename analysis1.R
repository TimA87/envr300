
# import libraries
library(openair)
library(ggplot2)
library(lubridate)
library(reshape2)
library(zoo)

# write function to fill in some na values
consecna <- function(x, n=2) {
  # function to identify elements with n or more consecutive NA values
  # n=2 is the default value, but can be changed when included in the funciton call
  y <- rle(is.na(x))
  y$values <- y$lengths > (n - 0.5) & y$values
  inverse.rle(y)
}

options(warn=-1)

dir <- "D:/school/envr 300"
setwd(dir)

# read in data frame from csv file
df <- read.csv("CO2 spreadsheet.csv",colClasses='character',header=T)
# make sure numbers are in numeric format
df <- as.data.frame(sapply(df, as.numeric))

# remove -99.99 values and replace with NA
df[df==-99.99] <- NA

# fix messed up dates
df$date <- as.POSIXlt(paste(df$Yr,df$Mn,"01",sep="-"), format="%Y-%m-%d")
df$date <- ymd(df$date)

# get running mean of CO2 values
row.names(df) <- NULL
df$num <- seq(1:length(df[,1])) 

file1 <- df[!consecna(df$CO2),]
number <- file1[is.na(file1$CO2),]
file1$CO2 <- na.approx(file1$CO2)
df$CO2[is.element(df$num,number$num)] <- file1$CO2[is.element(file1$num,number$num)]
file1  <- NULL
number <- NULL

df$rollCO2 <- rollapply(df$CO2,24,mean,fill=NA,align="center")
# create new data frame for plotting

df$avgCO2 <- na.approx(df$rollCO2,na.rm=F)

tdf <- df[!is.na(df$avgCO2),]
tdf$res <- tdf$avgCO2 - tdf$CO2

tdf$series <- seq(1,length(tdf$res))

summary(lm(tdf$avgCO2 ~ tdf$series))

newdf <- melt(df, id="date", measure.vars=c("CO2", "avgCO2"))

# ploet the CO2
ggplot(newdf, aes(x=date, y=value, color=variable)) +
  geom_line(size=1,alpha=0.9) +
  theme(plot.title=element_text(size=15, vjust=1.5)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(values=c("firebrick", "navyblue")) +
  theme(plot.title = element_text(color="firebrick", size=14, face="bold.italic")) +
  theme(panel.background = element_rect(fill = "grey92")) +
  theme(panel.grid.major = element_line(colour = "grey70")) +
  theme(panel.grid.minor = element_line(colour = "grey60")) +
  ggtitle("CO2 concentrations from 1958 to 2010") +
  xlab("Date") +
  ylab("CO2 (ppm)")

# plot residuals



# calculate trend significance
