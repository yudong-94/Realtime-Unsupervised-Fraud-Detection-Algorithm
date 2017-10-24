library(dplyr)
library(ggplot2)

### for 3-days time window
load("variables_3.Rda")

### for 7-days time window
load("variables_7.Rda")

####### deal with frivilous values in type1 variables

# SSN:
## frivilous value: 737610282
## involved columns: PERSON_SSN, PHONE_SSN, ADD_SSN, TIME_SSN

avg = mean(data[data$ssn != 737610282, "PERSON_SSN"])
data[data$ssn == 737610282, "PERSON_SSN"] = avg

avg = mean(data[data$ssn != 737610282, "PHONE_SSN"])
data[data$ssn == 737610282, "PHONE_SSN"] = avg

avg = mean(data[data$ssn != 737610282, "ADD_SSN"])
data[data$ssn == 737610282, "ADD_SSN"] = avg

avg = mean(data[data$ssn != 737610282, "TIME_SSN"])
data[data$ssn == 737610282, "TIME_SSN"] = avg

# PHONE:
## frivious value: 9105580920
## involved columns: PERSON_PHONE, SSN_PHONE, ADD_PHONE, TIME_PHONE

avg = mean(data[data$homephone != 9105580920, "PERSON_PHONE"])
data[data$homephone == 9105580920, "PERSON_PHONE"] = avg

avg = mean(data[data$homephone != 9105580920, "SSN_PHONE"])
data[data$homephone == 9105580920, "SSN_PHONE"] = avg
 
avg = mean(data[data$homephone != 9105580920, "ADD_PHONE"])
data[data$homephone == 9105580920, "ADD_PHONE"] = avg

avg = mean(data[data$homephone != 9105580920, "TIME_PHONE"])
data[data$homephone == 9105580920, "TIME_PHONE"] = avg

# ADD_ZIP
## frivilous value: 2602 AJTJ AVE 68138
## involved columns: PERSON_ADD, SSN_ADD, PHONE_ADD, TIME_ADD

avg = mean(data[data$ADD_ZIP != "2602 AJTJ AVE 68138", "PERSON_ADD"])
data[data$ADD_ZIP == "2602 AJTJ AVE 68138", "PERSON_ADD"] = avg

avg = mean(data[data$ADD_ZIP != "2602 AJTJ AVE 68138", "SSN_ADD"])
data[data$ADD_ZIP == "2602 AJTJ AVE 68138", "SSN_ADD"] = avg

avg = mean(data[data$ADD_ZIP != "2602 AJTJ AVE 68138", "PHONE_ADD"])
data[data$ADD_ZIP == "2602 AJTJ AVE 68138", "PHONE_ADD"] = avg

avg = mean(data[data$ADD_ZIP != "2602 AJTJ AVE 68138", "TIME_ADD_ZIP"])
data[data$ADD_ZIP == "2602 AJTJ AVE 68138", "TIME_ADD_ZIP"] = avg

# save(data, file = "expert_data.Rda")

# extract only the expert variables part and valid records
# 3-days time window:
expert_app = data[data$date>"2015-01-03", 13:28]
# 7-days time window
expert_app = data[data$date>"2015-01-07", 13:28]


##########################################################

### PCA

## Do the z-scale and PCA
pr.out=prcomp(expert_app,scale=TRUE)
# names(pr.out)
pr.out$center
# double-check
# apply(expert_app,2,mean)
pr.out$scale

## get the result of PCA
# pr.out$rotation
# pr.out$sdev
pr.var=pr.out$sdev^2
# pr.var
pve=pr.var/sum(pr.var)
pve
sum(pve[1:6])

## scree plot to understand the smallest number of principla componets required
## focus on the point at which the proportion of variance drops off

plot(pve,xlab='Principal Component',ylan='Proportion of Variance Explained',
     ylim=c(0,0.3),type='b',main='Scree Plot with 3-days Time Window')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained with 3-days Time Window", 
     ylim=c(0,1), type='b')

## PC1-6 are enough 

pca=pr.out$rotation 
pca=data.frame(pca)

## selecting only PC1 to PC6
pcaneed=pca %>%
    select(PC1:PC6)

## Transform the z scaled variables into PC space 
## by multiplying 2 matrix
finaldata=as.matrix(expert_app) %*% as.matrix(pcaneed)
finaldata=data.frame(finaldata)
save(finaldata, file = "finaldata.rda")


##########################################################
### calculating fraud score using heuristic algorithm

## calculate fraud score
mean = colMeans(finaldata)
cov = cov(finaldata)
ma_dist = mahalanobis(finaldata, mean, cov)
ma_dist = data.frame(cbind(849:dim(data)[1], ma_dist))
# if 7days: change 849 to 2015
colnames(ma_dist) = c("Record", "ma_dist")

summary(ma_dist$ma_dist)
quantile(ma_dist$ma_dist, 0.99)


## plot a histogram
ggplot(data = ma_dist, aes(x = ma_dist)) +
    geom_histogram(bins = 70) +
    coord_cartesian(xlim=c(0,70000)) +
    scale_y_log10() +
    ggtitle("Fraud Score of Heuristic Algorithm with 7-days Time Window") +
    xlab("Fraud Score") +
    ylab("count") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18))

# # how many scores above 2000
# count(filter(ma_dist, ma_dist > 2000))


## find the index of top 10% and 1% records
# cut_off = round(dim(ma_dist)[1] * 0.1)
# top10_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
# cut_off2 = round(dim(ma_dist)[1] * 0.01)
# top1_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off2]


##############################################################
### Autoencoder

library(h2o)
localH2O = h2o.init()

finaldata.hex = as.h2o(finaldata)

# autoencoder with 4 hidden layers
finaldata.dl = h2o.deeplearning(x = names(finaldata.hex), training_frame = finaldata.hex,
                                autoencoder = TRUE,
                                reproducible = F,
                                max_w2 = 10,
                                l1=1e-5,
                                hidden = c(400, 300, 200, 100))
## Reconstruct
finaldata.anon = h2o.anomaly(finaldata.dl, finaldata.hex, per_feature=TRUE)
err = as.data.frame(finaldata.anon)
# the "err" data frame contains all reconstruction errors of each record

#save(err, file = "reconstruct_error_1.rda")

## Plot the reconstructed Squared Error for the first 3 PCs
plot(err$reconstr_PC1.SE, main='Reconstruction Error - PC1')
plot(err$reconstr_PC2.SE, main='Reconstruction Error - PC2')
plot(err$reconstr_PC3.SE, main='Reconstruction Error - PC3')


### Autoencoder score

## Fraud score
auto_score = data.frame(rowSums(err))
auto_score = cbind(849:dim(data)[1], auto_score)
# if 7days: change 849 to 2015
colnames(auto_score) = c("Record", "auto_score")

summary(auto_score$auto_score)
quantile(ma_dist$ma_dist, 0.99)

## histogram of the scores
ggplot(data = auto_score, aes(x = auto_score)) +
    geom_histogram(bins = 50) +
    #coord_cartesian(xlim=c(0,1)) +
    scale_y_log10() +
    ggtitle("Fraud Score of Autoencoder with 7-days Time Window") +
    xlab("Fraud Score") +
    ylab("count") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18))


## get the index of the top 10% and 1% records
# top10_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]
# top1_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off2]

## compare fraud records of the two algorithms
fraud_compare <- function(percentage, cut_off = FALSE) {
    if (!cut_off) {
        cut_off = round(dim(ma_dist)[1] * percentage)
    }
    top_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
    top_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]
    count = 0
    for (i in top_index_auto) {
        if (i %in% top_index_ma) {
            count = count + 1
        }
    }
    print(count/cut_off*100)
    print(paste0(round(count/cut_off,2)*100, " percent of records matched"))
}

fraud_compare(0.01) #top 1% (1000 records): 100% matched
fraud_compare(0.001) #top 0.1% (100 records): 99% matched for 3-days, 98% for 7
fraud_compare(percentage = NULL, cut_off = 10) #top 10: 3 matched for 3-days, 1 for 7

## extract the top 1% records of both algorithm
cut_off = round(dim(ma_dist)[1] * 0.01)
top_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off] + 848
# if 7days: change 848 to 2014
top_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off] + 848
# if 7days: change 848 to 2014

overlap = data %>%
    filter(record.. %in% top_index_ma, record.. %in% top_index_auto)

save(overlap, file = "overlap_1percent.rda")

## extract the top 10 overlap records 
top_10 = NULL
for (i in top_index_auto) {
    if (i %in% top_index_ma) {
        top_10 = c(top_10, i)
    }
    if (length(top_10) == 10) {
        break
    }
}

overlap_top10 = data %>%
    filter(record.. %in% top_10)

save(overlap_top10, file = "overlap_top10.rda")
write.csv(overlap_top10, file = "overlap_top10_2.csv")

