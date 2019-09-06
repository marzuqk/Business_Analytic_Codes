#################################################################
# From R for Marketing Research and Analytics, Chapter 4
#################################################################

# Load the .csv data from the web
cust.df = read.csv("http://goo.gl/PmPkaG")
cust.df$cust.id = factor(cust.df$cust.id)

# Explore the data structure
str(cust.df)
head(cust.df)

#################################################################
# Exploring associations with a scatterplot
#################################################################

# Basic scatterplot
plot(x=cust.df$store.spend, y=cust.df$online.spend)

# Provide more information for the axes and chart title
plot(cust.df$store.spend, cust.df$online.spend, 
     main="Customers as of June 2017", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.7)

# Plotting on a log scale
plot(cust.df$store.spend+1, cust.df$online.spend+1, 
     main="Customers as of June 2017", 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)", 
     cex=0.7, log="xy")

# Scatterplot matrix
pairs(formula = ~ age + credit.score + email +
        distance.to.store + online.visits + online.trans + 
        online.spend + store.trans + store.spend,
      data=cust.df)
pairs(cust.df[ , c(2:10)]) # same command as the above

#################################################################
# Exploring associations with correlation coeffs and tests
#################################################################

# Correlation coefficient
cor(cust.df$online.visits, cust.df$online.spend)
cor.test(cust.df$online.visits, cust.df$online.spend)

# Correlation matrix
cor(cust.df[, c(2, 3, 5:10)])

# corrplot package
install.packages(c("corrplot", "gplots"))
library(corrplot)    # for correlation plot
library(gplots)      # for color interpolation
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))










