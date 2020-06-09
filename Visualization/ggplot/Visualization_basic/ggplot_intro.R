library(ggplot2)
ggplot(diamonds)  # if only the dataset is known.
ggplot(diamonds, aes(x=carat))  # if only X-axis is known. The Y-axis can be specified in respective geoms.
ggplot(diamonds, aes(x=carat, y=price))  # if both X and Y axes are fixed for all layers.
ggplot(diamonds, aes(x=carat, color=cut))  # Each category of the 'cut' variable will now have a distinct  color, once a geom is added.


#adding layer
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() # Adding scatterplot geom (layer1) and smoothing geom (layer2).

#different
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + geom_smooth(aes(x=carat, y=price, color=cut))

#one smooth line
library(ggplot2)
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=cut)) + geom_smooth()  # same but simpler

ggplot(diamonds, aes(x=carat, y=price, color=cut, shape=color)) + geom_point()

#labels
gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + labs(title="Scatterplot", x="Carat", y="Price")  # add axis lables and plot title.
print(gg)

gg1 <- gg + theme(plot.title=element_text(size=30, face="bold"), 
                  axis.text.x=element_text(size=15), 
                  axis.text.y=element_text(size=15),
                  axis.title.x=element_text(size=25),
                  axis.title.y=element_text(size=25)) + 
  scale_color_discrete(name="Cut of diamonds")  # add title and axis text, change legend title.
print(gg1)  # print the plot

#facet
gg1 + facet_wrap( ~ cut, ncol=3)
gg1 + facet_wrap(color ~ cut)
gg1 + facet_wrap(color ~ cut, scales="free")
gg1 + facet_grid(color ~ cut)   # In a grid


#timeseries
library(ggfortify)
autoplot(AirPassengers) + labs(title="AirPassengers")  # where AirPassengers is a 'ts' object


# Approach 1:
data(economics, package="ggplot2")  # init data
economics <- data.frame(economics)  # convert to dataframe
ggplot(economics) + geom_line(aes(x=date, y=pce, color="pcs")) + geom_line(aes(x=date, y=unemploy, col="unemploy")) + scale_color_discrete(name="Legend") + labs(title="Economics") # plot multiple time series using 'geom_line's

# Approach 2:
library(reshape2)
df <- melt(economics[, c("date", "pce", "unemploy")], id="date")
ggplot(df) + geom_line(aes(x=date, y=value, color=variable)) + labs(title="Economics")# plot multiple time series by melting

df <- melt(economics[, c("date", "pce", "unemploy", "psavert")], id="date")
ggplot(df) + geom_line(aes(x=date, y=value, color=variable))  + facet_wrap( ~ variable, scales="free")


#barplot
