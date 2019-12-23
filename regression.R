install.packages("keras")
library(keras)
top <- read.csv("top50.csv")
View(top)
scatter.smooth(x=top$Beats.Per.Minute, y=top$Popularity, main="beats~ popularity")
scatter.smooth(x=top$Energy, y=top$Popularity, main="energy~ popularity")
scatter.smooth(x=top$Danceability, y=top$Popularity, main="danceabilty~ popularity")
scatter.smooth(x=top$Loudness..dB.., y=top$Popularity, main="loudness~ popularity")
scatter.smooth(x=top$Liveness, y=top$Popularity, main="liveness~ popularity")
scatter.smooth(x=top$Valence., y=top$Popularity, main="valence~ popularity")
scatter.smooth(x=top$Length., y=top$Popularity, main="length~ popularity")
scatter.smooth(x=top$Acousticness.., y=top$Popularity, main="acoustic~ popularity")
scatter.smooth(x=top$Speechiness., y=top$Popularity, main="speechiness~ popularity")
##using boxplot to check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(top$Beats.Per.Minute, main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Beats.Per.Minute)$out))  # box plot for 'speed'
boxplot(top$Energy, main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Energy)$out))
boxplot(top$Danceability, main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Danceability)$out))
boxplot(top$Loudness..dB.., main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Loudness..dB..)$out))
boxplot(top$Liveness, main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Liveness)$out))
boxplot(top$Valence., main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Valence.)$out))
boxplot(top$Length., main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Length.)$out))
boxplot(top$Acousticness.., main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Acousticness..)$out))
boxplot(top$Speechiness., main="beats per minute", sub=paste("Outlier rows: ", boxplot.stats(top$Speechiness.)$out))
##check if the values are normally distributed
library(e1071)  # for skewness function
par(mfrow=c(1, 2))  # divide graph area in 2 columns
##beats per minute
plot(density(top$Beats.Per.Minute), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Beats.Per.Minute), 2)))  # density plot for 'speed'
polygon(density(top$Beats.Per.Minute), col="yellow")
plot(density(top$Energy), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Energy), 2)))  # density plot for 'speed'
polygon(density(top$Energy), col="yellow")
plot(density(top$Danceability), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Danceability), 2)))  # density plot for 'speed'
polygon(density(top$Danceability), col="yellow")
plot(density(top$Loudness..dB..), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Loudness..dB..), 2)))  # density plot for 'speed'
polygon(density(top$Loudness..dB..), col="yellow")
plot(density(top$Liveness), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Liveness), 2)))  # density plot for 'speed'
polygon(density(top$Liveness), col="yellow")
plot(density(top$Valence.), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Valence.), 2)))  # density plot for 'speed'
polygon(density(top$Valence.), col="yellow")
plot(density(top$Length.), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Length.), 2)))  # density plot for 'speed'
polygon(density(top$Length.), col="yellow")
plot(density(top$Acousticness..), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Acousticness..), 2)))  # density plot for 'speed'
polygon(density(top$Acousticness..), col="yellow")
plot(density(top$Speechiness.), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Speechiness.), 2)))  # density plot for 'speed'
polygon(density(top$Speechiness.), col="yellow")
plot(density(top$Popularity), main="Density Plot: beats per minute", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(top$Popularity), 2)))  # density plot for 'speed'
polygon(density(top$Popularity), col="yellow")
##check out the correlation
cor(top$Popularity, top$Valence.) 
cor(top$Popularity, top$Beats.Per.Minute)
cor(top$Popularity, top$Energy)
cor(top$Popularity, top$Danceability)
cor(top$Popularity, top$Loudness..dB..)
cor(top$Popularity, top$Liveness)
cor(top$Popularity, top$Length.)
cor(top$Popularity, top$Acousticness..)
cor(top$Popularity, top$Speechiness.)
##let us concentrate on valence ::
#building a regression model
linearMod <- lm(Popularity~Valence., data=top)  # build linear regression model on full data
print(linearMod)
#Linear regression diagnostics
summary(linearMod)
linearMod <- lm(Popularity~Beats.Per.Minute, data=top)  # build linear regression model on full data
print(linearMod)
#Linear regression diagnostics
summary(linearMod)
linearMod <- lm(Popularity~Energy, data=top)  # build linear regression model on full data
print(linearMod)
#Linear regression diagnostics
summary(linearMod)
linearMod <- lm(Popularity~Danceability, data=top)  # build linear regression model on full data
print(linearMod)
#Linear regression diagnostics
summary(linearMod)


#multiple regression
fit <- lm(top$Popularity~top$Beats.Per.Minute+top$Energy+top$Danceability+top$Loudness..dB..+top$Liveness+top$Valence.+top$Length.+top$Acousticness..+top$Speechiness.,data=top)
summary(fit)



