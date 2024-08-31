library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(DT)
library(ggplot2)
library(shiny)
library(htmltools)

html <- read_html("https://en.wikipedia.org/wiki/Tourism_in_India")

table_data <- html %>%
  html_table()

# Foreign tourist arrivals in India (1997???20222).
table_1 <- table_data[[4]]

# Foreign exchange earnings from tourism in India (1997???2020)
table_2 <- table_data[[5]]

# Source countries for foreign tourist arrivals in India in 2019.
table_3 <- table_data[[6]]

# Share of top 10 states/UTs of India in number of foreign tourist visits in 2017.
table_4 <- table_data[[8]]

# Share of top 10 states/UTs of India in number of domestic tourist visits in 2017.
table_5 <- table_data[[9]]

table_1$`Arrivals (millions)` <- as.numeric(table_1$`Arrivals (millions)`)
mean(table_1$`Arrivals (millions)`) 

#################################################################################

data_img <- html %>%
  html_elements(".mw-file-element") %>%
  html_attr("src")

data_img <- data_img[29:110]
# data_img

data_place <- html %>% html_elements(".gallerytext") %>% html_text()
data_place <- data_place[1:82]
Place_to_Visit <- c()
for(i in 1:82)
{
  data_place[i] <- substring(data_place[i],first = 2)
  Place_to_Visit[i] <- gsub(pattern = "\n\n\t\t\t",replacement = "",x = data_place[i])
}

# we can also use this for collecting names
# html %>% html_elements(".gallerytext") %>% html_text2()

# Place_to_Visit

url_img <- c()
for(i in 1:82)
{
  
  url_img[i] <- paste0("https:",data_img[i])
}
# url_img
# a <- load.image(url_img[1])
# plot(a)


#data <- tibble(Place_to_Visit,url_img)
#view(data)  


d <- html %>% html_elements(".gallerytext a") %>% html_attr("href")
# d

link_n <- c(d[1],d[3],d[5],d[7],d[9],d[10],d[12],
            d[13],d[15],d[17],d[19],d[21],d[23],
            d[24],d[25],d[26],d[27],d[29],d[30])
link_s <- c(d[32],d[33],d[36],d[38],d[40],d[42],d[43],d[44],
            d[45],d[47],d[48],d[50],d[52],
            d[53],d[54])
link_c <- c(d[55],d[57],d[59],d[60],d[61],d[62],d[65],
            d[66],d[68],d[69],d[71],d[73],
            d[75])
link_w <- c(d[76],d[77],d[78],d[80],d[81],d[82],
            d[83],d[84],d[85],d[87],
            d[89],d[91],d[92],d[93],
            d[95],d[96])
link_e <- c(d[98],d[100],d[103],"Rainbow Waterfalls",
            d[105],d[108],d[109],d[110],d[111],
            d[112],d[113],d[114],"Sri_Mayapur_Chandrodaya_Mandir")
link_ne <- c(d[116],d[118],d[120],d[121],
             d[122],d[123])

description <- c(link_n,link_s,link_c,link_w,link_e,link_ne)
# description

url_place_description <- c()
for(i in 1:82)
{
  
  url_place_description[i] <- paste0("https://en.wikipedia.org/",description[i])
}
# url_place_description


final_table <- tibble(Place_to_Visit,url_img,url_place_description)

dis <- NULL
for (i in 1:82)
{
  dis[i] <- paste0("<a href='",final_table$url_place_description[i],"' target='_blank'>Details</a>")
}
dis

final_table <- tibble(final_table$Place_to_Visit,dis) 
colnames(final_table)[colnames(final_table) == "final_table$Place_to_Visit"] <- "Place_to_Visit"
colnames(final_table)[colnames(final_table) == "dis"] <- "discription_link"


# if you want to see the table realted the webpage the you can run the below code.
# for getting more information in (readable) table format
#  table <- c()
#  for(i in 1:84)
#  {

#    table[i] <- read_html(url_img_description[i]) %>% 
#    html_table()
#   }

####################################################################################

North_India <- tibble(Place_to_Visit[1:19],url_img[1:19],url_place_description[1:19])
colnames(North_India)[colnames(North_India) == "Place_to_Visit[1:19]"] <- "Data_Type"
colnames(North_India)[colnames(North_India) == "url_img[1:19]"] <- "Picture_URL"



South_India <- tibble(Place_to_Visit[20:34],url_img[20:34],url_place_description[20:34])
colnames(South_India)[colnames(South_India) == "Place_to_Visit[20:34]"] <- "Data_Type"
colnames(South_India)[colnames(South_India) == "url_img[20:34]"] <- "Picture_URL"


Central_India <- tibble(Place_to_Visit[35:47],url_img[35:47],url_place_description[35:47])
colnames(Central_India)[colnames(Central_India) == "Place_to_Visit[35:47]"] <- "Data_Type"
colnames(Central_India)[colnames(Central_India) == "url_img[35:47]"] <- "Picture_URL"


West_India <- tibble(Place_to_Visit[47:63],url_img[47:63],url_place_description[47:63])
colnames(West_India)[colnames(West_India) == "Place_to_Visit[47:63]"] <- "Data_Type"
colnames(West_India)[colnames(West_India) == "url_img[47:63]"] <- "Picture_URL"


East_India <- tibble(Place_to_Visit[63:76],url_img[63:76],url_place_description[63:76])
colnames(East_India)[colnames(East_India) == "Place_to_Visit[63:76]"] <- "Data_Type"
colnames(East_India)[colnames(East_India) == "url_img[63:76]"] <- "Picture_URL"


NorthEast_India <- tibble(Place_to_Visit[76:82],url_img[76:82],url_place_description[76:82])
colnames(NorthEast_India)[colnames(NorthEast_India) == "Place_to_Visit[76:82]"] <- "Data_Type"
colnames(NorthEast_India)[colnames(NorthEast_India) == "url_img[76:82]"] <- "Picture_URL"


###################################################################################

month <- read_html("https://www.tourism-of-india.com/travel-by-month.html")

#data1 <- html %>% html_elements(".mt-0") %>% html_text2()
#data1

data_month <- month %>% html_elements(".text") %>% html_text2()


paragraph <- c()
words <- c()
first_three_words <- c()
remaining_text <- c()
for(i in 1:length(data_month))
{
  paragraph[i] <- gsub("\n\n",x = data_month[i],replacement = " ")
  # Split the paragraph into words
  words <- unlist(strsplit(paragraph[i], " "))
  
  # Extract the first three words
  first_three_words[i] <- paste(words[1:1], collapse = " ")
  
  # Remaining paragraph
  remaining_text[i] <- paste(words[4:length(words)], collapse = " ")
  
}

Travel_by_Month_in_India <- tibble(first_three_words,remaining_text)
colnames(Travel_by_Month_in_India) <- c("Months","Description")
##################


url <- read_html("https://www.thrillophilia.com/motorcycle-tours-in-india")
data3 <- url %>% html_elements(".h3") %>% html_text2()
data3 <- data3[1:20]

data4 <- url %>% html_elements(".text-holder.read-more-wrap") %>% html_text2()

a <- c()
b <- c()
c <- c()
d <- c()
e <- c()

a <- strsplit(x = data4[1],split = "Route Map:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e1 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- e1

a <- strsplit(x = data4[2],split = "Route Map:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e2 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e2)



a <- strsplit(x = data4[3],split ="Common route taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e3 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e3)

a <- strsplit(x = data4[4],split ="Route Map:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e4 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e4)

a <- strsplit(x = data4[5],split ="Route Map:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e5 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e5)

a <- strsplit(x = data4[6],split ="Route Map:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e6 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e6)

a <- strsplit(x = data4[7],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e7 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e7)

a <- strsplit(x = data4[8],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e8 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e8)

a <- strsplit(x = data4[9],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e9 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e9)

a <- strsplit(x = data4[10],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e10 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e10)

a <- strsplit(x = data4[11],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e11 <-data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e11)

a <- strsplit(x = data4[12],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e12 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e12)

a <- strsplit(x = data4[13],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e13 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e13)

a <- strsplit(x = data4[14],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e14 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e14)

a <- strsplit(x = data4[15],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e15 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e15)

a <- strsplit(x = data4[16],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e16 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e16)

a <- strsplit(x = data4[17],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e17 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e17)

a <- strsplit(x = data4[18],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e18 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e18)

a <- strsplit(x = data4[19],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e19 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e19)

a <- strsplit(x = data4[20],split ="Common Route Taken:")
b <- strsplit(a[[1]][2],split = "Total Riding Distance:")
c <- strsplit(b[[1]][2],split = "Best Riding Season:")
d <- strsplit(c[[1]][2],split = "Also")
e20 <- data_frame(a[[1]][1],b[[1]][1],c[[1]][1],d[[1]][1])

my_tibble <- add_row(my_tibble,e20)

my_tibble <- my_tibble %>%
  mutate(data3) %>%
  select(data3, everything())

colnames(my_tibble)[colnames(my_tibble) == "data3"] <- "Tour"
colnames(my_tibble)[colnames(my_tibble) == "a[[1]][1]"] <- "Description"
colnames(my_tibble)[colnames(my_tibble) == "b[[1]][1]"] <- "Common Route Taken"
colnames(my_tibble)[colnames(my_tibble) == "c[[1]][1]"] <- "Total Riding Distance"
colnames(my_tibble)[colnames(my_tibble) == "d[[1]][1]"] <- "Best Riding Season"


###################################################################################
# Andhra_Pradesh

##### for url of photo...............

html_Andhra <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Andhra_Pradesh")

html_Andhra1 <- html_Andhra%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Andhra1 <- html_Andhra1[2:11]
url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Andhra1[i])
}


###### for name of photo..............

html_Andhra2 <- html_Andhra%>%html_elements(".mw-default-size")%>%html_text()

html_Andhra2[1]= "Highest Man Made Stone Nandi"

Place_to_Visit <- html_Andhra2[1:10]

Andhra_Pradesh <- data.frame(Place_to_Visit,url_img)

Andhra_Pradesh <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Andhra_Pradesh)[colnames(Andhra_Pradesh) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Andhra_Pradesh)[colnames(Andhra_Pradesh) == "url_img[1:10]"] <- "Picture_URL"


################################################################################# 

#  Bihar

##### for url of photo...............

html_Bihar <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Bihar")

html_Bihar1 <- html_Bihar%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Bihar1 <- html_Bihar1[1:10]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Bihar1[i])
}

###### for name of photo..............

html_Bihar2 <- html_Bihar%>%html_elements(".mw-default-size")%>%html_text()

html_Bihar2[5]= "Maha Bodhi Temple, Bodhgaya"

html_Bihar2[6]="Nalanda Mounds"

html_Bihar2[7]="Vikramshila"

html_Bihar2[8]="Old City Walls at Rajgir"

html_Bihar2[9]="Kesariya Stupa -kesariya- east champaran"

html_Bihar2[10]=" Ashokan Pillar Vaishali "

Place_to_Visit <- html_Bihar2

Bihar <- data.frame(Place_to_Visit,url_img)

Bihar <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Bihar)[colnames(Bihar) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Bihar)[colnames(Bihar) == "url_img[1:10]"] <- "Picture_URL"

###################################################################################

#  chattisgarh

##### for url of photo...............

html_chattisgarh <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Chhattisgarh")

html_chattisgarh1 <- html_chattisgarh%>%html_elements(".mw-file-element")%>%html_attr("src")

html_chattisgarh1 <- html_chattisgarh1[3:12]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_chattisgarh1[i])
}


###### for name of photo..............

html_chattisgarh2 <- html_chattisgarh%>%html_elements(".mw-default-size")%>%html_text()
html_chattisgarh2 <- c(html_chattisgarh2[1:9])
html_chattisgarh3 <- c("Chitrakot_waterfalls")
html_chattisgarh4 <- c(html_chattisgarh3,html_chattisgarh2)
html_chattisgarh4[7] <- "Baloda-bazar"

Place_to_Visit <- html_chattisgarh4[1:10]

chattisgarh <- data.frame(Place_to_Visit,url_img)

chattisgarh <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(chattisgarh)[colnames(chattisgarh) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(chattisgarh)[colnames(chattisgarh) == "url_img[1:10]"] <- "Picture_URL"

#################################################################################

# Gujarat

##### for url of photo...............

html_Gujarat <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Gujarat")

html_Gujarat1 <- html_Gujarat%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Gujarat1 <- html_Gujarat1[4:13]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Gujarat1[i])
}

###### for name of photo..............

html_Gujarat2 <- html_Gujarat%>%html_elements(".mw-default-size")%>%html_text()

Place_to_Visit <- html_Gujarat2[1:10]

Gujarat <- data.frame(Place_to_Visit,url_img)

Gujarat <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Gujarat)[colnames(Gujarat) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Gujarat)[colnames(Gujarat) == "url_img[1:10]"] <- "Picture_URL"

###############################################################################

# Uttar_pradesh

##### for url of photo...............

html_Uttar_pradesh <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Uttar_Pradesh")

html_Uttar_pradesh1 <- html_Uttar_pradesh%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Uttar_pradesh1[1] <- html_Uttar_pradesh1[30]

html_Uttar_pradesh1[5] <- html_Uttar_pradesh1[18]

html_Uttar_pradesh1[9] <- html_Uttar_pradesh1[29]

html_Uttar_pradesh1[3] <- html_Uttar_pradesh1[41]

html_Uttar_pradesh1[6] <- html_Uttar_pradesh1[25]

html_Uttar_pradesh1<- html_Uttar_pradesh1[1:10]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Uttar_pradesh1[i])
}

###### for name of photo..............
html_Uttar_pradesh <- c()
html_Uttar_pradesh[1] <- "Triveni Sangam, Allahabaad"
html_Uttar_pradesh[2] <- "Taj_Mahal, Agra"
html_Uttar_pradesh[3] <- "Fatehpur Sikri"
html_Uttar_pradesh[4] <- "Prem_mandir"
html_Uttar_pradesh[5] <- "Agra Fort"
html_Uttar_pradesh[6] <- "Naini_Bridge,Allahabaad"
html_Uttar_pradesh[7] <- "Varanasiganga"
html_Uttar_pradesh[8] <- "Kumbhmela"
html_Uttar_pradesh[9] <- "Anand_Bhawan"
html_Uttar_pradesh[10] <- "Ashtapad Hastinapur"

Place_to_Visit <- html_Uttar_pradesh

Uttar_pradesh <- data.frame(Place_to_Visit,url_img)

Uttar_pradesh <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Uttar_pradesh)[colnames(Uttar_pradesh) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Uttar_pradesh)[colnames(Uttar_pradesh) == "url_img[1:10]"] <- "Picture_URL"

###############################################################################

# Karnataka

##### for url of photo...............

html_Karnataka <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Karnataka")

html_Karnataka1 <- html_Karnataka%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Karnataka1 <- html_Karnataka1[70:79]
url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Karnataka1[i])
}

###### for name of photo..............

html_Karnataka2 <- html_Karnataka%>%html_elements(".mw-default-size")%>%html_text()
html_Karnataka2 <- html_Karnataka2[5:14]
Place_to_Visit <- html_Karnataka2
Karnataka <- data.frame(Place_to_Visit,url_img)

Karnataka <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Karnataka)[colnames(Karnataka) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Karnataka)[colnames(Karnataka) == "url_img[1:10]"] <- "Picture_URL"




##################################################################################

# Jharkhand  

##### for url of photo...............

html_Jharkhand  <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Jharkhand")

html_Jharkhand1 <- html_Jharkhand %>%html_elements(".mw-file-element")%>%html_attr("src")

html_Jharkhand1 <- html_Jharkhand1[-c(8)]

html_Jharkhand1 <- html_Jharkhand1[1:10]

url_img <- c()

for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Jharkhand1[i])
}

###### for name of photo..............

html_Jharkhand2 <- html_Jharkhand%>%html_elements(".mw-default-size")%>%html_text()

Place_to_Visit <- html_Jharkhand2[1:10]

Jharkhand <- data.frame(Place_to_Visit,url_img)

Jharkhand <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Jharkhand)[colnames(Jharkhand) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Jharkhand)[colnames(Jharkhand) == "url_img[1:10]"] <- "Picture_URL"


##################################################################################

# Goa  

##### for url of photo...............

html_Goa  <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Goa")

html_Goa1 <- html_Goa%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Goa1 <- html_Goa1[-c(9:12)]

html_Goa1 <- html_Goa1[1:10]
url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Goa1[i])
}

###### for name of photo..............

html_Goa2 <- html_Goa%>%html_elements(".thumb.tright")%>%html_text()

foo_Goa <- strsplit(html_Goa2, ",")

html_Goa3 <- foo_Goa[[1]][2:7]

html_Goa4 <- c("Goa_panoramio")

html_Goa2 <- c(html_Goa4,c( html_Goa3))

html_Goa2[8] <- "Baga Beach in North Goa"

html_Goa2[9] <- "Bogmalo beach in South Goa"

html_Goa2[10] <- "Glassy Tiger"

Place_to_Visit <- html_Goa2

Goa <- data.frame(Place_to_Visit,url_img)

Goa<- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Goa)[colnames(Goa) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Goa)[colnames(Goa) == "url_img[1:10]"] <- "Picture_URL"


################################################################################  

# Madhya_Pradesh

##### for url of photo...............

html_Madhya_Pradesh <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Madhya_Pradesh")

html_Madhya_Pradesh1 <- html_Madhya_Pradesh%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Madhya_Pradesh1 <- html_Madhya_Pradesh1[2:11]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Madhya_Pradesh1[i])
}

###### for name of photo..............

html_Madhya_Pradesh2 <- html_Madhya_Pradesh%>%html_elements(".mw-default-size")%>%html_text()

html_Madhya_Pradesh2[10] <- "Tal_ul_Masjid, Bhopal"

html_Madhya_Pradesh2 <- html_Madhya_Pradesh2[1:10] 
Place_to_Visit <-  html_Madhya_Pradesh2

Madhya_Pradesh <- data.frame(Place_to_Visit,url_img)

Madhya_Pradesh <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Madhya_Pradesh)[colnames(Madhya_Pradesh) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Madhya_Pradesh)[colnames(Madhya_Pradesh) == "url_img[1:10]"] <- "Picture_URL"


####################################################################################

# Assam

##### for url of photo...............

html_Assam <- read_html("https://en.wikipedia.org/wiki/Tourism_in_Assam")

html_Assam1 <- html_Assam%>%html_elements(".mw-file-element")%>%html_attr("src")

html_Assam1 <- html_Assam1[3:9]
url_img <- c()
for(i in 1:7)
{
  
  url_img[i] <- paste0("https:",html_Assam1[i])
}

###### for name of photo..............

html_Assam2 <- html_Assam%>%html_elements(".mw-halign-right")%>%html_text()

html_Assam2[4] <- "Kamakhya Temple"
html_Assam2[5] <- "Majuli island"
html_Assam2[6] <- "Nameri national Park"
html_Assam2[7] <- "Rang ghar pavilions"

Place_to_Visit <- html_Assam2

Assam <- data.frame(Place_to_Visit,url_img)

Assam <- tibble(Place_to_Visit[1:7],url_img[1:7])
colnames(Assam)[colnames(Assam) == "Place_to_Visit[1:7]"] <- "Data_Type"
colnames(Assam)[colnames(Assam) == "url_img[1:7]"] <- "Picture_URL"

####################################################################################
# 1.Tourism in West Bengal....//


html_West_Bengal<-read_html("https://en.wikipedia.org/wiki/Tourism_in_West_Bengal")

html_West_Bengal1 <- html_West_Bengal%>%html_elements(".mw-file-element")%>%html_attr("src")
html_West_Bengal1 <- html_West_Bengal1[1:10]
url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_West_Bengal1[i])
}

# for name of photo..............

html_West_Bengal2 <- html_West_Bengal%>%html_elements(".mw-halign-right")%>%html_text()
html_West_Bengal2[1]= "Darjeeling Himalayan Railway"
html_West_Bengal2[2]= "Howrah Bridge"
html_West_Bengal2[3]= "Acharya Jagadish Chandra Bose Indian Botanic Garden"
html_West_Bengal2[4]= "Morgan House Kalimpong"
html_West_Bengal2[5]= "Mandarmani Sea Beach"
html_West_Bengal2[6]= "Chintamani Kar Bird Sanactuary Kolkata"
html_West_Bengal2[7]= "Panthera tigris tigris"
html_West_Bengal2[8]= "Gorumara National Park Panorama"
html_West_Bengal2[9]= "baul singers"
html_West_Bengal2[10]= "Sunderban"
Place_to_Visit <- html_West_Bengal2[1:10]

West_bengal <- data.frame(Place_to_Visit,url_img)
West_bengal <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(West_bengal)[colnames(West_bengal) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(West_bengal)[colnames(West_bengal) == "url_img[1:10]"] <-"Picture_URL"



# 2.Tourism in Delhi.......

html_Delhi<-read_html("https://en.wikipedia.org/wiki/List_of_tourist_attractions_in_Delhi")
html_Delhi1 <- html_Delhi%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Delhi1 <- html_Delhi1[5:14]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Delhi1[i])
}


# for name of photo..............

html_Delhi2 <- html_Delhi%>%html_elements("mw-default-size")%>%html_text()
html_Delhi2[1]= "Rashtrapati_Bhavan"
html_Delhi2[2]= "Qutub minar"
html_Delhi2[3]= "Red Fort"
html_Delhi2[4]= "Akshardham Temple"
html_Delhi2[5]= "ISKCON Temple"
html_Delhi2[6]= "Digambar Jain Lal Mandir"
html_Delhi2[7]= "Gurudwara Bangla Sahib"
html_Delhi2[8]= "Jama Masjid"
html_Delhi2[9]= "Cathedral Church"
html_Delhi2[10]= "National War Memorial"
Place_to_Visit <- html_Delhi2[1:10]

Delhi <- data.frame(Place_to_Visit,url_img)

Delhi <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Delhi)[colnames(Delhi) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Delhi)[colnames(Delhi) == "url_img[1:10]"] <-"Picture_URL"


# 3.Tourism of Telangana

html_Telangana<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Telangana")
html_Telangana1 <- html_Telangana%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Telangana1 <- html_Telangana1[1:8]

url_img <- c()
for(i in 1:8)
{
  
  url_img[i] <- paste0("https:",html_Telangana1[i])
}


# for name of photo..............

html_Telangana2 <- html_Telangana%>%html_elements("mw-default-size")%>%html_text()
html_Telangana2[1]= "Charminar"
html_Telangana2[2]= "Warangal fort"
html_Telangana2[3]= "Bhongir Fort"
html_Telangana2[4]= "Jain temple warangal"
html_Telangana2[5]= "Bhadrachalam Temple"
html_Telangana2[6]= "Birla Temple"
html_Telangana2[7]= "Mecca Masjid"
html_Telangana2[8]= "Medak Cathedral"
Place_to_Visit <- html_Telangana2[1:8]

Telangana <- data.frame(Place_to_Visit,url_img)
Telangana <- data.frame(Place_to_Visit,url_img)
Telangana <- tibble(Place_to_Visit[1:8],url_img[1:8])
colnames(Telangana)[colnames(Telangana) == "Place_to_Visit[1:8]"] <- "Data_Type"
colnames(Telangana)[colnames(Telangana) == "url_img[1:8]"] <-"Picture_URL"


# 4.Tourism in Uttrakhand

html_Uttrakhand<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Uttarakhand")
html_Uttrakhand1 <- html_Uttrakhand%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Uttrakhand1 <- html_Uttrakhand1[1:10]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Uttrakhand1[i])
}

# for name of photo..............

html_Uttrakhand2 <- html_Uttrakhand%>%html_elements("mw-default-size")%>%html_text()
html_Uttrakhand2[1]= "Badrinath Valley"
html_Uttrakhand2[2]= "Kedarnath"
html_Uttrakhand2[3]= "Gangotri temple"
html_Uttrakhand2[4]= "Yamunotri temple and ashram"
html_Uttrakhand2[5]= "Kedarnath Temple"
html_Uttrakhand2[6]= "Tungnath temple"
html_Uttrakhand2[7]= "Rudranath temple"
html_Uttrakhand2[8]= "Madhyamaheshwar Temple"
html_Uttrakhand2[9]= "Kalpehswar"
html_Uttrakhand2[10]= "Nainital lake"
Place_to_Visit <- html_Uttrakhand2[1:10]
Uttrakhand <- data.frame(Place_to_Visit,url_img)
Uttrakhand <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Uttrakhand)[colnames(Uttrakhand) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Uttrakhand)[colnames(Uttrakhand) == "url_img[1:10]"] <-"Picture_URL"




# 5.Tourism in Tamilnadu

html_Tamilnadu<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Tamil_Nadu")
html_Tamilnadu1 <- html_Tamilnadu%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Tamilnadu1 <- html_Tamilnadu1[2:11]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Tamilnadu1[i])
}

# for name of photo..............

html_Tamilnadu2 <- html_Tamilnadu%>%html_elements("mw-default-size")%>%html_text()
html_Tamilnadu2[1]= "Marina Beach"
html_Tamilnadu2[2]= "Tamil Nadu Flag"
html_Tamilnadu2[3]= "south of Chenna"
html_Tamilnadu2[4]= "Madurai Meenakshi"
html_Tamilnadu2[5]= "Gangaikonda Cholapuram Temple Gopuram"
html_Tamilnadu2[6]= "Thiruvannamalai Annamalaiyar Temple"
html_Tamilnadu2[7]= "Hogenakkal Falls bathing area"
html_Tamilnadu2[8]= "Parisal Boating in Hogenakkal falls"
html_Tamilnadu2[9]= "Pichavaram Mangrove Forest"
html_Tamilnadu2[10]= "Amman Temple"
Place_to_Visit <- html_Tamilnadu2[1:10]

TamilNadu <- data.frame(Place_to_Visit,url_img)
TamilNadu <- data.frame(Place_to_Visit,url_img)
TamilNadu <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(TamilNadu)[colnames(TamilNadu) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(TamilNadu)[colnames(TamilNadu) == "url_img[1:10]"] <-"Picture_URL"



# 6.Tourism in Kerala

html_Kerala<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Kerala")
html_Kerala1 <- html_Kerala%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Kerala1 <- html_Kerala1[3:12]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Kerala1[i])
}
# for name of photo..............

html_Kerala2 <- html_Kerala%>%html_elements("mw-default-size")%>%html_text()
html_Kerala2[1]= "House Boat"
html_Kerala2[2]= "Light house in Ponnani"
html_Kerala2[3]= "Resort calicut kerala"
html_Kerala2[4]= "Kerala Houseboat"
html_Kerala2[5]= "Kottappura nileshwaram house"
html_Kerala2[6]= "Munnar Hillscape"
html_Kerala2[7]= "Sithar Kundu View Point"
html_Kerala2[8]= "Lightmatter lion tailed macaque"
html_Kerala2[9]= "The View of the Athirapally Falls during the onset of Monsoon"
html_Kerala2[10]= "Sea From Bakel"
Place_to_Visit <- html_Kerala2[1:10]
Kerala <- data.frame(Place_to_Visit,url_img)

Kerala <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Kerala)[colnames(Kerala) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Kerala)[colnames(Kerala) == "url_img[1:10]"] <-"Picture_URL"


# 7.Tourism in Rajasthan

html_Rajasthan<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Rajasthan")
html_Rajasthan1 <- html_Rajasthan%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Rajasthan1 <- html_Rajasthan1[2:11]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Rajasthan1[i])
}


# for name of photo..............

html_Rajasthan2 <- html_Rajasthan%>%html_elements("mw-default-size")%>%html_text()
html_Rajasthan2[1]= "Udaipur Lake Palace"
html_Rajasthan2[2]= "Hawa Mahal on a stormy afternoon"
html_Rajasthan2[3]= "JagMandir"
html_Rajasthan2[4]= "City Palace by lake Pichola"
html_Rajasthan2[5]= "Mehrangarh Fort"
html_Rajasthan2[6]= "AmberFort"
html_Rajasthan2[7]= "Amer Fort"
html_Rajasthan2[8]= "Jaisalmer forteresse"
html_Rajasthan2[9]= "Ancient Temples Jaisalmer"
html_Rajasthan2[10]= "Walls of Kumbhalgarh"  
Place_to_Visit <- html_Rajasthan2[1:10]

Rajasthan <- data.frame(Place_to_Visit,url_img)
Rajasthan <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Rajasthan)[colnames(Rajasthan) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Rajasthan)[colnames(Rajasthan) == "url_img[1:10]"] <-"Picture_URL"



# 8.Tourism in Odisha
html_Odisha<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Odisha")
html_Odisha1 <- html_Odisha%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Odisha1 <- html_Odisha1[3:12]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Odisha1[i])
}
# for name of photo..............


html_Odisha2 <- html_Odisha%>%html_elements("mw-default-size")%>%html_text()
html_Odisha2[1]= "Konarak Sun Temple"
html_Odisha2[2]= "Brahmeswar Temple Bhubaneswar"
html_Odisha2[3]= "Mukteswar temple"
html_Odisha2[4]= "Lingaraj temple"
html_Odisha2[5]= "Rajarani Temple"
html_Odisha2[6]= "Chilika Lake"
html_Odisha2[7]= "Barehipani Falls"
html_Odisha2[8]= "Badaghagara Kendujhar"
html_Odisha2[9]= "Khandhadhar Falls"
html_Odisha2[10]= "Sanaghagara Kendujhar"
Place_to_Visit <- html_Odisha2[1:10]
Odisha <- data.frame(Place_to_Visit,url_img)
Odisha <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Odisha)[colnames(Odisha) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Odisha)[colnames(Odisha) == "url_img[1:10]"] <-"Picture_URL"



# 9.Tourism in Maharastra
html_Maharastra<-read_html("https://en.wikipedia.org/wiki/Tourism_in_Maharashtra")
html_Maharastra1 <- html_Maharastra%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Maharastra1 <- html_Maharastra1[4:13]

url_img <- c()
for(i in 1:10)
{
  
  url_img[i] <- paste0("https:",html_Maharastra1[i])
}


# for name of photo..............


html_Maharastra2 <- html_Maharastra%>%html_elements("mw-default-size")%>%html_text()
html_Maharastra2[1]= "Sydney point panchghani"
html_Maharastra2[2]= "Pandharpur Aashad panoramio"
html_Maharastra2[3]= "Sri Hazur Sahib Nanded"
html_Maharastra2[4]= "Statue Of Ahimsa MangiTungi"
html_Maharastra2[5]= "Bahubali Kumbhoj"
html_Maharastra2[6]= "Kailasha temple at ellora"
html_Maharastra2[7]= "Elephanta Caves"
html_Maharastra2[8]= "Ajanta"
html_Maharastra2[9]= "Pataleshwar Caves temple"
html_Maharastra2[10]= "Bibika"
Place_to_Visit <- html_Maharastra2[1:10]
Maharastra <- data.frame(Place_to_Visit,url_img)
Maharastra <- data.frame(Place_to_Visit,url_img)
Maharastra <- tibble(Place_to_Visit[1:10],url_img[1:10])
colnames(Maharastra)[colnames(Maharastra) == "Place_to_Visit[1:10]"] <- "Data_Type"
colnames(Maharastra)[colnames(Maharastra) == "url_img[1:10]"] <-"Picture_URL"



# 10.Tourism in  Andaman and Nicobar
html_Andaman_and_Nicobar <- read_html("https://en.wikipedia.org/wiki/Tourism_in_the_Andaman_and_Nicobar_Islands")
html_Andaman_and_Nicobar1 <- html_Andaman_and_Nicobar%>%html_elements(".mw-file-element")%>%html_attr("src")
html_Andaman_and_Nicobar1 <- html_Andaman_and_Nicobar1[1:6] 

url_img <- c()
for(i in 1:6)
{
  
  url_img[i] <- paste0("https:",html_Andaman_and_Nicobar1[i])
}

# for photo name........//

html_Andaman_and_Nicobar2 <- html_Andaman_and_Nicobar%>%html_elements(".mw-default-size")%>%html_text() 
html_Andaman_and_Nicobar2[1]= "CellularJail Wings" 
html_Andaman_and_Nicobar2[2]= "Gallows on Viper Island" 
html_Andaman_and_Nicobar2[3]= "Havelock Island"
html_Andaman_and_Nicobar2[4]= "India Tourism Elephant"
html_Andaman_and_Nicobar2[5]= "Viper Island" 
html_Andaman_and_Nicobar2[6]= "Andaman ross"

Place_to_Visit <- html_Andaman_and_Nicobar2[1:6] 
Andaman_and_Nicobar <- data.frame(Place_to_Visit,url_img)
Andaman_and_Nicobar <- tibble(Place_to_Visit[1:6],url_img[1:6])
colnames(Andaman_and_Nicobar)[colnames(Andaman_and_Nicobar) == "Place_to_Visit[1:6]"] <- "Data_Type"
colnames(Andaman_and_Nicobar)[colnames(Andaman_and_Nicobar) == "url_img[1:6]"] <-"Picture_URL"


State_list <- c("Andhra_Pradesh",
                "Bihar",
                "chattisgarh",
                "Gujarat",
                "Uttar_pradesh",
                "Karnataka",
                "Jharkhand",
                "Goa",
                " Madhya_Pradesh",
                "Assam",
                "West_bengal",
                "Delhi",
                "Telangana",
                "Uttrakhand",
                "TamilNadu",
                "Kerala",
                "Rajasthan",
                "Odisha",
                "Maharastra",
                "Andaman_and_Nicobar",
                "Jammu_and_kashmir",
                "Meghalaya",
                "Sikkim",
                "Nagaland",
                "Tripura",
                "Mizoram",
                "Himachal_Pradesh",
                "Haryana",
                "Panjab",
                "Manipur",
                "Chandigarh",
                "Dadra_and_Nagar_Haveli",
                "Ladakh",
                "Lakshadeep",
                "Puducherry"
)

#####################################################################################

# Define your data tables (table_1, table_2, etc.) and table_list here
# Sample data for table2_list 
table2_list <- c("North_India",
                 "South_India",
                 "Central_India",
                 "West_India",
                 "East_India",
                 "NorthEast_India")


table_list <- c(
  "Foreign tourist arrivals in India (1997???2022).",
  "Foreign exchange earnings from tourism in India (1997???2020).",
  "Source countries for foreign tourist arrivals in India in 2019.",
  "Share of top 10 states/UTs of India in number of foreign tourist visits in 2017.",
  "Share of top 10 states/UTs of India in number of domestic tourist visits in 2017.",
  "Scrapped_data_table")
