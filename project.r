getwd()

# set the working directory
setwd("/Volumes/Work/Data Science/R/Project")

# packages use -------------
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("RColorBrewer") # use to expand color palette with ggplot and RColorBrewer --

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)


# import and discover data -------------------
data <- read.csv("consumer_complaintss.csv")

head(data)
(summary(data))
# data.summary=data.frame (summary(data))
dim(data)            # dimensions of object
str(data)
glimpse(data) # same Str
names(data)
is.na(data)

# check the difference unique values by column-------
unique(data$tags)
unique(data$company_response_to_consumer)
unique(data$consumer_consent_provided)
unique(data$submitted_via)
unique(data$company_public_response)
unique(data$sub_product)
unique(data$sub_issue)
unique(data$state)

#unique(data$tags,data$timely_response)

# Calculate the numbers of Blank values each column ------
blank.count = (colSums(data==""))
#blank.count =data.frame(colSums(data==""))

"
change blanks values to NA values
data$sub_product[data$sub_product == ""] <- NA
"

# Order the company_public_response by type of order (descending) --> use dplyr ----
data %>%
  group_by(data$sub_product) %>%
  summarise(Response_Numbers= n()) %>%
  arrange(desc(Response_Numbers))


data %>%
  group_by(company_public_response, company) %>%
  summarise(Response_Numbers= n())%>%
  arrange(desc(Response_Numbers))

# No_of_complaints by states
data %>%
  group_by(data$state) %>%
  summarise(No_of_complaints= n()) %>%
  arrange(desc(No_of_complaints))


# vasualization --------------------
#  Numbers of consumer disputed --------

colors <- c(rep("red",1), rep("blue",1))

ggplot(data, aes(consumer_disputed.))  +
  geom_histogram(stat="count", fill=colors)

# consumer disputed by submitted via ------

ggplot(data, aes(x= consumer_disputed., fill= submitted_via)) +
  geom_bar() +
  labs(y = "submitted compliens ", title="consumer disputed by submitted via")

# Numbers of products by bar chart----------

ggplot(data, aes(x=product, fill = product)) + 
  geom_bar() + 
  ggtitle("Number of Complaints by product") + 
  xlab("product") + ylab("Number of Complaints")

# products by submitted via ------------------

ggplot(data, aes(x= product, fill= submitted_via)) +
  geom_bar() +
  labs(y = "submitted compliens ", title="products by submitted via")

# Top 10 companies has complaints --------------

data%>%
  group_by(data$company,data$state) %>%
  summarise(No_of_complaints= n()) %>%
  arrange(desc(No_of_complaints))


# The most states has complains -------------------

# expand color palette with ggplot and RColorBrewer --
colourCount = length(unique(data$state)) # use length of the unique values in the column
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data, aes(x= state))  +
  geom_histogram(stat="count", fill=getPalette(colourCount)) +
  coord_flip()+ 
  theme(legend.position="right") +
  labs(x= "states", y = " Numbers of complines", title="the most state has complines")

#-
ggplot(data, aes(x= state))  +
  geom_histogram(stat="count", fill=getPalette(colourCount)) +
  coord_flip()+ 
  theme(legend.position="right") +
  labs(x= "states", y = " Numbers of complines", title="the most states has complines")

# The timely response for complaints by most top 5 states --------------------
top5_states = subset(data, state =="CA" | state =="TX"|state =="NY"|state =="GA"|state =="FL")
top5_states %>%
  group_by(top5_states$timely_response) %>%
  summarise(timely_response= n()) %>%
  arrange(desc(timely_response))

ggplot(top5_states, aes(x=factor(1), fill=timely_response))+
  geom_bar(width = 1)+
  coord_polar("y")

# The most products that have complaints from the consumer by top5_states ---------
ggplot(top5_states, aes(x=factor(1), fill=product))+
  geom_bar(width = 1)+
  coord_polar("y")


#No_of_complaints per top 5 statws of product ----------
top5_states %>%
  group_by(top5_states$product) %>%
  summarise(No_of_complaints= n()) %>%
  arrange(desc(No_of_complaints))

# the most Submitted methods ---------

ggplot(top5_states, aes(x=factor(1), fill=submitted_via))+
  geom_bar(width = 1)+
  coord_polar("y")

top5_states %>%
  group_by(top5_states$submitted_via) %>%
  summarise(No_of_submitted_via= n()) %>%
  arrange(desc(No_of_submitted_via))

# add more with out sent-----------
ggplot(top5_states, mapping= aes(x=company, y=submitted_via))+
  geom_point()



