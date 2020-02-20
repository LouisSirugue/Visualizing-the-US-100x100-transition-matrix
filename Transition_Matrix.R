################################################
# Visualizing the US 100x100 transition matrix #
################################################
# author: Louis Sirugue
# date: July 2019
################################################
rm(list = ls())

# This document aims to provide a visual representation of the US 100x100 
# transition matrix computed by Chetty et al. (2014) in the article "Where 
# is the land of opportunity? The geography of intergenerational mobility 
# in the United States". A 100x100 (resp. 5x5) transition matrix gives the 
# probability that a child is in centile (resp. quintile) m of the child 
# income distribution conditional on his parent being in centile (resp. 
# quintile) n of the parent income distribution. The following packages are 
# needed to propery go through the code.
list.of.packages <- c("ggplot2", "ggpubr", "haven")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)

# Let's start with 5x5 transition matrix, whose values are displayed in the 
# second Table of the paper. All values should be gathered in a single vector. 
# The following vector is broken such that each row corresponds to a given 
# child quintile and each column correspond to a given parent quintile.
Scale <- c(.337, .242, .178, .134, .109, 
           .280, .242, .198, .160, .119,
           .184, .217, .221, .209, .170,
           .123, .176, .220, .244, .236,
           .075, .123, .183, .254, .365)

# The quintiles of the children then write
child_quintile <- c()
for (i in 1:5){child_quintile <- c(child_quintile, rep(i, 5))}

# And those of the parents write
parent_quintile <- rep(seq(1, 5, 1), 5)

# The data frame should thus be shaped as follows.
quintiles <- data.frame(child_quintile, parent_quintile, Scale)
quintiles

# A heatmap of the transition matrix can then be plotted as:
ggplot(quintiles, aes(x = parent_quintile, y = child_quintile)) +
  geom_tile(aes(fill = Scale)) +
  scale_fill_gradient(low = "grey10", high = "steelblue") +
  scale_x_discrete(name = "Parent quintile", expand = c(0, 0), 
                   limits = seq(1, 5, 1))+
  scale_y_discrete(name = "Child quintile", expand = c(0, 0), 
                   limits = seq(1, 5, 1)) +
  theme(text = element_text(size = 14)) +
  ggtitle(label = "5x5 Transition Matrix")

# As the lighter the cell the higher the probability, the blue diagonal 
# is an indication of intergenerational persistence. 

# Let's now scale up to the 100x100 transition matrix. The data are available 
# in .dta and .csv format on Opportunity Insigths' website. It contains a 
# 100x100 transition matrix for the U.S. based on Chetty et al.'s core sample 
# (1980-82 birth cohorts) and baseline family income definitions.
data <- read_dta(file.choose())
data <- data.frame(data)

# The raw data should be reshaped as follows...
child_centile <- rep(data$kid_fam_bin, 100)
parent_centile <- rep(1, 96)
for(i in 2:100) {
  parent_centile <- c(parent_centile, rep(i, 96))
}
Scale <- data$par_frac_bin_1
for (cname in colnames(data)) {
  if (cname != "par_frac_bin_1" & cname != "kid_fam_bin") {
    Scale <- c(Scale, data[,cname])
  }
}
shaped_data <- data.frame(child_centile, parent_centile, Scale)

# ... and can then be plotted as
ggplot(shaped_data, aes(x = parent_centile, y = child_centile)) +
  geom_tile(aes(fill = Scale)) +
  scale_fill_gradient(low = "grey10", high = "steelblue") +
  scale_x_discrete(name = "Parent centile", expand = c(0, 0), 
                   limits = seq(1, 100, 10))+
  scale_y_discrete(name = "Child centile", expand = c(0, 0), 
                   limits = seq(1, 100, 10)) +
  theme(text = element_text(size = 14)) +
  ggtitle(label = "100x100 transition matrix")

# We can't see much on this graph because the matrix was not restricted 
# to children with positive income. Indeed, centile 1 corresponds to 
# negative child income and centile 4 corresponds to the mass of children 
# earning zero income, which is completely out of the range of the rest of 
# the matrix. We can rather focus on centiles 7-100 that are percentile 
# bins for children with positive income.
positive_inc <- subset(shaped_data, child_centile > 7 & parent_centile > 7)
ggplot(positive_inc, aes(x = parent_centile, y = child_centile)) +
  geom_tile(aes(fill = Scale)) +
  scale_fill_gradient(low = "grey10", high = "steelblue") +
  scale_x_discrete(name = "Parent centile", expand = c(0, 0), 
                   limits = seq(10, 90, 10))+
  scale_y_discrete(name = "Child centile", expand = c(0, 0), 
                   limits = seq(10, 90, 10)) +
  theme(text = element_text(size = 14)) +
  ggtitle(label = "Positive-income Transition Matrix")

# The result is not very interesting either, we can only distinguish a 
# few blue tiles at the very top right of the graph. Let's zoom on the 
# top10xtop10 matrix.
zoom <- subset(shaped_data, child_centile > 89 & parent_centile > 89)
ggplot(zoom, aes(x = parent_centile, y = child_centile)) +
  geom_tile(aes(fill = Scale)) +
  scale_fill_gradient(low = "grey10", high = "steelblue", 
                      limits = c(min(positive_inc$Scale), 
                                 max(positive_inc$Scale))) +
  scale_x_discrete(name = "Parent centile", expand = c(0, 0), 
                   limits = seq(90, 100, 1))+
  scale_y_discrete(name = "Child centile", expand = c(0, 0), 
                   limits = seq(90, 100, 1)) +
  theme(text = element_text(size = 14)) +
  ggtitle(label = "Top10xTop10 Transition Matrix")

# It is quite clear that a high intergenerational persistence at the 
# top of the income distribution prevents the graph from depicting any 
# clear pattern. To get a better sense of what happens on the major part 
# of the income distribution, the top of the data can be trimmed as follows.
trimmed_data <- subset(positive_inc, child_centile < 99 & parent_centile < 99)
ggplot(trimmed_data, aes(x = parent_centile, y = child_centile)) +
  geom_tile(aes(fill = Scale)) +
  scale_fill_gradient(low = "grey10", high = "steelblue") +
  scale_x_discrete(name = "Parent centile", expand = c(0, 0), 
                   limits = seq(10, 90, 10))+
  scale_y_discrete(name = "Child centile", expand = c(0, 0), 
                   limits = seq(10, 90, 10)) +
  theme(text = element_text(size = 14)) +
  ggtitle(label = "Transition Matrix")

# A diagonal seems to emerge between the bottom-left and the top-right corner, 
# but it is still a bit noisy. To identify a potential pattern, I mark the 
# n = {1, 5, 10 , 15} lightest cells of each column.
marked_data <- trimmed_data
for (i in 8:98) {
  marked_data$Scale[match(max(marked_data$Scale[marked_data$parent_centile == i], 
                              na.rm = TRUE), marked_data$Scale)] <- NA
}
lbound <- min(marked_data$Scale, na.rm = TRUE)
ubound <- max(marked_data$Scale, na.rm = TRUE)
TR.plot2 <- function(n) {
  marked_data <- trimmed_data
  iteration <- 0
  repeat {
    for (i in 8:98) {
      marked_data$Scale[match(max(marked_data$Scale[marked_data$parent_centile == i], 
                                  na.rm = TRUE), marked_data$Scale)] <- NA
    }
    iteration = iteration + 1
    if (iteration == n){
      break
    }
  }
  ggplot(marked_data, aes(x = parent_centile, y = child_centile)) +
    geom_tile(aes(fill = Scale)) +
    scale_fill_gradient(low = "grey10", high = "steelblue", na.value = "grey5", 
                        limits = c(lbound, ubound)) +
    scale_x_discrete(name = "Parent centile", expand = c(0, 0), 
                     limits = seq(10, 90, 10))+
    scale_y_discrete(name = "Child centile", expand = c(0, 0), 
                     limits = seq(10, 90, 10)) +
    theme(text = element_text(size = 14)) +
    ggtitle(label = paste(n, "marked tile per column", sep =" "))
}

ggarrange(TR.plot2(1), TR.plot2(5), TR.plot2(10), TR.plot2(15), ncol = 2, nrow = 2)

# This allows to distinguish a clear S-shape that is reminiscent to the nonparametric 
# relationship between the log child family income and the log parent family income 
# displayed by Figure I-B in the paper.
