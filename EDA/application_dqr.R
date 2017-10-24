#setwd("~/Desktop/DSO 562/HW5")
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#app = read.csv("applications 100k.csv")
#save(file = "applications.rda", data = app)
load("applications.rda")


# there are some obviously fake numbers in SSN, e.g. 999999999   (not this number in the dataset)
# also frivolous address, e.g. 123 main st.
# also frivolous data of birth (1907726 in this dataset), phone number
# when you consider the frequency of certain SSN/PHONE, remove

#record
## categorical

#date
## date
sum(is.na(app$date))
length(unique(app$date))
app$date = ymd(app$date)
app$month = month(app$date, label = TRUE)
app$day = day(app$date)
app$day = as.factor(app$day)

ggplot(data = app, aes(x = month)) +
    geom_bar() +
    ggtitle("Distribution of Application Months") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))

ggplot(data = app, aes(x = day)) +
    geom_bar() +
    ggtitle("Distribution of Application Days") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))

#ssn
## categorical
sum(is.na(app$ssn))
length(unique(app$ssn))

top_ssn = app %>%
    group_by(ssn) %>%
    summarise(count = n(), prop = n()/100000) %>%
    arrange(-count)
top_ssn$ssn = as.factor(top_ssn$ssn)

ggplot(data = top_ssn[2:11,], aes(x = reorder(ssn, - count), y = count)) +
    geom_bar(stat = "identity") +
    scale_y_log10() +
    xlab("SSN") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))

#firstname
## categorical
sum(is.na(app$firstname))
length(unique(app$firstname))

top_firstname = app %>%
    group_by(firstname) %>%
    summarise(count = n(), prop = n()/100000) %>%
    arrange(-count)
top_firstname$firstname = as.factor(top_firstname$firstname)

ggplot(data = top_firstname[1:10,], aes(x = reorder(firstname, - count), y = count)) +
    geom_bar(stat = "identity") +
    xlab("First Name") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))

#lastname
## categorical
sum(is.na(app$lastname))
length(unique(app$lastname))

top_lastname = app %>%
    group_by(lastname) %>%
    summarise(count = n(), prop = n()/100000) %>%
    arrange(-count)
top_lastname$lastname = as.factor(top_lastname$lastname)

ggplot(data = top_lastname[1:10,], aes(x = reorder(lastname, - count), y = count)) +
    geom_bar(stat = "identity") +
    xlab("Last Name") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))


#address
## text

#zip5
## categorical
sum(is.na(app$zip5))
length(unique(app$zip5))

top_zip5 = app %>%
    group_by(zip5) %>%
    summarise(count = n(), prop = n()/100000) %>%
    arrange(-count)
top_zip5$zip5 = as.factor(top_zip5$zip5)

ggplot(data = top_zip5[1:10,], aes(x = reorder(zip5, - count), y = count)) +
    geom_bar(stat = "identity") +
    xlab("Zip Code") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1))


#dob
## date

#homephone
## categorical