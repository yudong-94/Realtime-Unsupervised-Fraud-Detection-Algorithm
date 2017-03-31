### overlap in top 1%

library(dplyr)
library(ggplot2)
library(stringr)

# load data
load("expert_data.Rda")

load("overlap_1percent_3days.rda")
load("overlap_top10_3days.rda")
overlap_3d = overlap
overlap_top10_3d = overlap_top10

load("overlap_1percent_7days.rda")
load("overlap_top10_7days.rda")
overlap_7d = overlap
overlap_top10_7d = overlap_top10

rm(overlap, overlap_top10)


# user written function for creating descriptive statistics
mystats <- function(y, na.omit=TRUE) {
  
  x <- y[!is.na(y)]
  n <- length(x)
  m <- mean(x)
  median <- median(x)
  s <- sd(x)
  
  return(c(number=n, mean=m, stdev=s,median = median))
}

vars <- c(names(overlap_3d)[13:28])

stats_3d <- t(data.frame(apply(overlap_3d[vars], 2, mystats)))
stats_7d <- t(data.frame(apply(overlap_7d[vars], 2, mystats)))
stats_all <- t(data.frame(apply(data[vars], 2, mystats)))

# combine all the stats
stats_sum <- cbind (stats_all, stats_3d, stats_7d)
# Writing Summary stats to external file
write.csv(stats_sum, file = "stats_sum.csv")


stats_sum2 <- data.frame(variables = rownames(stats_3d), rbind(stats_all, stats_3d, stats_7d), row.names = NULL)
stats_sum2[1:16, "data"] = "all records"
stats_sum2[17:32, "data"] = "top 1% with 3-days time window"
stats_sum2[33:48, "data"] = "top 1% with 7-days time window"

ggplot(data = stats_sum2, aes(x = variables, y = mean, fill = data)) +
    geom_bar(width = 0.6, position = position_dodge(width = 0.8), stat = "identity") +
    ylab("Variable mean value") +
    geom_text(aes(label = round(mean,3)), 
              hjust = 0, vjust = 0.5, size = 5,
              position = position_dodge(width=1)) +
    scale_fill_manual(values=c("darkblue", "blue", "lightblue")) +
    ggtitle("Average counts comparison") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)) +
    coord_flip()