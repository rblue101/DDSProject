---
title: "brew case study"
author: "Joey Hernandez"
date: "2022-10-08"
output: html_document
---
Set up starting libraries and import the datasets.
```{r}
library(tidyverse)
library(ggplot2)
beer_data <-read.csv("C:/Users/Joey/Desktop/bs-dds/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Beers.csv")

brew_data <- read.csv("C:/Users/Joey/Desktop/bs-dds/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Breweries.csv")

 view(beer_data)

```

Quick glance at data
```{r}
# quick glance... beer data contains at least one repeat entry (#9)
dim(beer_data) # 2410 rows, 7 column 
sum(is.na(beer_data)) # 1067 total NA values...
sum(is.na(beer_data$ABV)) # 62
sum(is.na(beer_data$IBU)) # 1005 international bitterness units

# quick glance...brew data 
dim(brew_data) # 558 rows and 4 columns
view(brew_data)
sum(is.na(brew_data))# no NA values or any issues at first glance. 

```

Merging Data together
```{r}
# It looks like data can be merged with brewery ID - brew ID

# changed name of column to match the other dataset 
# head(beer_data) # Brewery_id
# head(brew_data) # Brew_ID
names(beer_data)[1] <- 'beer_name'
names(brew_data)[2] <- 'Brew_name'
names(brew_data)[1]<- 'Brewery_id'
casestudy <- full_join(beer_data, brew_data, by = "Brewery_id")
casestudy_na <- full_join(beer_data, brew_data, by = "Brewery_id")
# view(casestudy)

```
* Note that we created an NA version and a non NA version... this will be important later...


# Question 1 : How many breweries are present in each state? - No NA version 
```{r}
# how many breweries are present in each state?
# cool state plot?

#gets rid of the white space before the State abb. 
casestudy$State<-trimws(casestudy$State, "left")
casestudy <- na.omit(casestudy) # to omit NA values.


us_brew <- casestudy %>% group_by(State) %>%
  summarise(num_brew = n_distinct(Brew_name),
  mean_abv = mean(ABV), mean_ibu = mean(IBU)) %>%
  arrange(desc(num_brew))

names(us_brew)[1] <- 'state'
us_brew <- as.data.frame(us_brew)
# view(us_brew)

# can also make map of flavor profiles etc ... would be VERY interesting data. 
library(usmap)
plot_usmap(data=us_brew[,1:2],
           regions = "states",
           labels = TRUE,label_color = "white",
           values = "num_brew")+
  theme(legend.position = "right")

#for creating a table in RMD
knitr::kable(casestudy %>% group_by(State) %>%
        summarise(n_unique = n_distinct(Brew_name),
                  mean_abv = mean(ABV), mean_ibu = mean(IBU)) %>%
        arrange(desc(n_unique)))

```
# Question 1 - With NA values present - 

```{r}
# how many breweries are present in each state?
# cool state plot?
view(casestudy_na)
#gets rid of the white space before the State abb. 
casestudy_na$State<-trimws(casestudy_na$State, "left")


us_brew_na <- casestudy_na %>% group_by(State) %>%
  summarise(num_brew = n_distinct(Brew_name),
  mean_abv = mean(ABV), mean_ibu = mean(IBU)) %>%
  arrange(desc(num_brew))

names(us_brew_na)[1] <- 'state'
us_brew <- as.data.frame(us_brew_na)
# view(us_brew)

# can also make map of flavor profiles etc ... would be VERY interesting data. 
library(usmap)
plot_usmap(data=us_brew_na[,1:2],
           regions = "states",
           labels = TRUE,label_color = "white",
           values = "num_brew")+
  theme(legend.position = "right")

#for creating a table in RMD
knitr::kable(casestudy_na %>% group_by(State) %>%
        summarise(n_unique = n_distinct(Brew_name),
                  mean_abv = mean(ABV), mean_ibu = mean(IBU)) %>%
        arrange(desc(n_unique)))

```

# Question 3: Address the missing values in each column:

style has missing values (implicit):

CAN'd Aid:
Oskar Blues Brewery teamed up with CAN'd Aid Foundation to fill cans of drinking water
for residents in Southeast Texas. The missing value can be attributed to this item not 
having a style of alcohol since it is water. 

Crowler:
CROWLER® (CAN + growler) is a 32-ounce or 25-ounce CAN filled with fresh craft beer from the draft source. The missing values here are attributed to this not being an actaul drink.

OktoberFiesta:
This was a missing value that seems to have just been missed. It has been addressed.

Kilt Lifter Scottish:
The style for this beer was not successfully split, or there was a data entry error. 
It has been addressed. 

Special Relase: 
This beer seems to be retired, and thus has no information of relevance. It is undetermined if this is possibly a seasonal beer, or one saved for special events. It will remain on the list. 

IBU - ABV has NA (explicit):
The IBU and ABV columns list almost half of their values as "NA". It would be unwise to 
remove this data since it would misrepresent the presense of breweries and their offerings. It would also not be wise to impute the values of this data since the reasoning of the explict missing values is unkown but could be attributed to process of creation, laws of certain areas, and other unknown variables. 
```{r}
# enter corrections below for the above information
casestudy[854,'Style'] = "Scottish Ale"
casestudy[867, 'Style'] = 'Marzen'
```
Question 4 - Compute the median alcohol content and interantional bitterness unit for each state.


```{r}
#  MEDIAN DATA CREATION
median_values <- casestudy %>% group_by(State) %>%
  summarise(num_brew = n_distinct(Brew_name),
  median_abv = median(ABV), median_ibu = median(IBU)) %>%
  arrange(desc(num_brew))

median_values<- as.data.frame(median_values)

# CREATING THE ABV MEDIAN PLOT
abv_median <- median_values %>% ggplot(aes(x = reorder(State,desc(median_abv)), y = median_abv))+
  geom_bar(stat = 'identity')

abv_median

# CREATING THE IBU MEIDAN PLOT

ibu_median <- median_values %>% ggplot(aes(x = reorder(State,desc(median_ibu)), y = median_ibu))+
  geom_bar(stat = 'identity')

ibu_median

```
Question 5 - 
1) Which state has the maximum alcoholic (ABV) beer?
2) Which State has the most bitter (IBU) beer?
```{r}

```
Question 6 - 
Comment on the summary statistics and distribution of the ABV variable 
```{r}

```
Question 7 -
Is there an apparent relationship between the bitterness of the beer and it's alcoholic content? Draw a scatter plot. make your best judgment of a relationship and Explain your answer. 
```{r}


```

