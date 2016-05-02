# read the test and train test set
setwd("~/Dropbox/Data and Codebook")

#install.packages("readr")
library(readr)

##Libraries required
#Data work 
#install.packages("data.table")
require(data.table) #Working with large files

#install.packages("xlsx")
#library(xlsx)       #Loading and saving .xlsx files 

# install.packages("plyr")
require(plyr)   #Always load in this order plyr, dpply, lubridate - dpplyr overrides some of methods in plyr. 

#install.packages("dplyr")
require(dplyr) #Use require as it will give an error message if the package doesn't exist

#installed.packages("lubridate")
require(lubridate) #used for working with data information. 

#install.packages("reshape2")
require(reshape2)  #used for melting 


#Formating and printing 
#install.packages("devtools")
library(devtools)
#devtools::install_github("adletaw/captioner")   #Nice library for numbering and captioning tables in conjunction with knitr and pandoc
#devtools::install_github('Rapporter/pander')
require(pander)     #for creating nice output tables.
require(captioner)

#Set up the figure and table numbering
fig_nums<-captioner()
tab_nums<-captioner(prefix = "Table")

#Using pryr abbreviate how to call fig_nums function 
#install.packages("pryr")
require(pryr)
citefig<-pryr::partial(fig_nums,display="cite")
citetab<-pryr::partial(tab_nums,display="cite")

#Turn off caption.prefix as allow captioner to handle this. 
panderOptions('table.caption.prefix', '')
panderOptions('big.mark', ",")
panderOptions('keep.trailing.zeros', TRUE)

# read the table into R
#Load the data 
adwords <- data.table::fread("approved_adwords_v3.csv",header=TRUE, stringsAsFactors = TRUE) 
purchase <-data.table::fread("approved_data_purchase-v5.csv",header=TRUE, stringsAsFactors = TRUE) 
ga <- data.table::fread("approved_ga_data_v2.csv",header=TRUE,  stringsAsFactors = TRUE)


### Initial explaration

### adwords

dim(adwords) # 1451987      28
ad_uni_campaign = unique(adwords$campaign) # 1132
ad_uni_campaign_id = unique(adwords$campaign_id)
ad_uni_ad_group = unique(adwords$ad_group) # 9713
ad_uni_ad_group_id = unique(adwords$adgroup_id) # 9713
ad_uni_keyword = unique(adwords$keyword) # 97222
ad_uni_keystate = unique(adwords$keyword.state) # 3
ad_uni_match = unique(adwords$match.type) # 3

### purchase # 1699911    45

# agregate by campaign
camp_total = adwords %>% group_by(ad_campaign = adwords$campaign) %>%
  summarize(total=n()) %>% arrange(desc(total))

# aggregate by campain and ad_group
camp_by_adgroup = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group) %>%
  summarize(total=n()) %>% arrange(desc(ad_campaign), desc(ad_group), desc(total))

# aggregate by campain and ad_group
camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$keyword.state) %>%
  summarize(total=n()) %>% mutate(freq = total/sum(total))

camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$match.type) %>%
  summarize(total=n()) %>% mutate(freq = total/sum(total))


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

conv.rate2 = sub("%", "", adwords$conv.rate)
conv.rate2 = as.factor(conv.rate2)
conv.rate2 = as.numeric(levels(conv.rate2))[conv.rate2]
conv.rate2 = conv.rate2/100

camp_by_adgroup_kwstate = adwords %>% group_by(ad_campaign = adwords$campaign, ad_group=adwords$ad_group, camp_by_adgroup_kwstate = adwords$match.type, conversions = conv.rate2) %>%
  summarize(total=n(), sum = sum(conversions)) %>% mutate(freq = sum/total)


arrange(camp_by_adgroup_kwstate, desc(freq))

camp_by_adgroup_kwstate[camp_by_adgroup_kwstate$camp_by_adgroup_kwstate == "enabled",]

camp_by_adgroup

sum(camp_by_adgroup[,1] == "Columbus OH")
### ga  4225456 rows and 46

####
# compare the adwords and the ga
# adgroup_id and campaign_id

ga_campaign_id = unique(ga$campaign_id)
ga_ad_group_id = unique(ga$adgroup_id) # 9713

# install.packages("VennDiagram")
library(VennDiagram)

# GA campaign ID and AD campaign ID: 282 overlap, AD has 312 unique and GA has 12 unique
grid.newpage()
venn.plot1<-draw.pairwise.venn(
  length(ga_campaign_id),length(ad_uni_campaign_id),length(intersect(ga_campaign_id,ad_uni_campaign_id)),
  category = c("ga campaign id", "ad campaign id"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))
grid.draw(venn.plot1)


### Ad group ID, has 545 overlapping, 1820 unique ga group ID and 345 ad unique group id
grid.newpage()
venn.plot2<-draw.pairwise.venn(
  length(ga_ad_group_id),length(ad_uni_ad_group_id),length(intersect(ad_uni_ad_group_id,ga_ad_group_id)),
  category = c("ad group id", "ga group id"),
  lty = rep("blank",2),
  fill = c("light blue", "pink"),
  alpha = rep(0.5, 2),
  cat.pos = c(0,0),
  cat.dist = rep(0.025, 2))
grid.draw(venn.plot2)