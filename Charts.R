###############################
# Creating Charts with ggplot #
###############################

# Prepared by Jay Simon
# Last updated on 11/24/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a short introduction to the syntax and some basic features of ggplot
# It uses the mtcars data set

# Packages required: ggplot2

# The following line uses the ggplot function to create a scatterplot with horsepower on the y-axis and displacement on the x-axis
ggplot(mtcars,aes(y=hp,x=disp)) + geom_point()

# Let's unpack that line a little.

# A "geom" is a geometric object that represents data
# There are many types.  Two other common ones are geom_line and geom_bar
# In this case, the geom_point function is what makes it a scatterplot

# The "aes" function adds an "aesthetic" to the chart.  An aesthetic describes visible properties of the chart, like colors, sizes, etc.
# Here we're using them for the plot itself, to define what the axes are
# You can also add them to geoms.  For example, we can color the points based on another variable:
ggplot(mtcars,aes(y=hp,x=disp)) + geom_point(aes(color = wt))

# We can also set the color scale:
ggplot(mtcars,aes(y=hp,x=disp)) + geom_point(aes(color = wt)) + scale_color_continuous(low = "Green", high = "Red")

# Plots can be stored as objects to make adding individual features easier.
# For example, the following two lines of code do the same thing as the previous line:
carplot <- ggplot(mtcars,aes(y=hp,x=disp)) + geom_point(aes(color = wt))
carplot + scale_color_continuous(low = "Green", high = "Red")

# ggplot has predefined "themes" for charts.  The following line applies the "classic" theme:
carplot + theme_classic()
# Some other common themes include bw, grey, light, and dark

# We can use a geom to add lines to plots
carplot + theme_classic() + geom_abline(slope = 0, intercept = 200,color = "Green")

# We can also add/change labels using the labs function
carplot + theme_classic() + labs(title="Relationship Between Horsepower and Displacement",
                                 x="Displacement", y="Horsepower")

# There are many other types of charts besides scatterplots
# For example, the following line creates a histogram
# A histogram requires only one axis.  (The y-axis is count.)
ggplot(mtcars,aes(x=hp)) + geom_histogram(bins=10) + theme_classic()

