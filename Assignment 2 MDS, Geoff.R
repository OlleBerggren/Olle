# Calculate distance 
nations.dist <- dist(Nations)

# Perform multidimensional scaling 
mds.nations <- cmdscale(nations.dist) 

# Change mds.state to a dataframe for use in ggplot
mds.nations_df <- data.frame(mds.nations)

# Plot the representation of the data in two dimensions 
ggplot(data = mds.nations_df, aes(x = X1, y = X2, label = rownames(mds.nations))) + 
  geom_text(alpha = 0.8, size = 3) 
which(is.na(politeness)==T) 

rownames(Nations) = colnames(Nations)

View(Nations)
(D <- dist(Nations))
nations1 <- isoMDS(D, k =2)
View(nations1)

x <- nations1$points[,1]
y <- nations1$points[,2]
lim <- range(c(x, y)) * 1.2

nations11 <- sim2diss(Nations, method = 9)  
nations11
nations1 <- mds(nations11, type = "ordinal")
nations1
names(nations1)
nations1$confdist
round(nations1$confdist, 2)
plot(nations1)
plot(nations1, plot.type = "Shepard")
plot(nations1, plot.type = "bubbleplot")
plot(nations1, plot.type = "stressplot")


plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = lim, ylim = lim, type = "n")
text(x, y, labels = labels(Nations), cex = 0.7)

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(nations1$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(nations1), cex = 0.6)
voting_sh <- Shepard(nations1[lower.tri(nations1)],
                     nations1$points)

plot(voting_sh, pch = ".", xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(voting_sh$x), 
     ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")
