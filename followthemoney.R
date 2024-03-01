library(ggplot2)
library(dplyr)
library(plotly)
library(sf)
library(mapview)
library(leafpop)

##load workspace
load("mapviewFTM.RDATA")

# file_path <- "C:/Users/zmarroquinmarroquin/OneDrive - Furman University/Desktop/FTMData/FollowTheMoneyDownload20240130_full.xlsx"

#Read file into the data frame
data <- read_excel(file_path, sheet=5)

#print the first few rows of the data frame
head(data)

## Creating the faceted bar chart
# ggplot(data, aes(x = Candidate, y = Amount, fill = Status_of_Candidate)) +
#   geom_bar(stat = "identity", position = "dodge")+
#   labs(title = "Bar Chart",
#        x = "Candidate",
#        y = "Amount Donated",
#        fill = "Election Status") +
#   facet_wrap(~Status_of_Candidate, scales= "free_x", ncol = 2)

##For this part to work you have to download and attach the dplyr package

#Clean the dataframe first
cleaned_df <- data %>%
  group_by(Candidate, Status_of_Candidate, General_Party, District,
           Broad_Sector, City, Zip) %>%
  summarise(TotalAmount = sum(Amount))

print(cleaned_df)

## For this part you need the plotly package! 

##Plotting clean data with interactivity
plot <- ggplot(top_candidates, aes(x = Candidate, y = top_candidate_amount, fill = Status_of_Candidate
                               # text = paste("Candidate: ", Candidate, "<br>Total Amount: $",
                               #              top_candidate_amount,
                               #              "<br>Outcome: ", Status_of_Candidate,
                               #              "<br>Party: ", General_Party, "<br>House: ",
                               #              District)
                               ))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Amount Donated to Candidates",
       x = "Candidate",
       y = "Total Amount Donated",
       fill = "Election Outcome")

plot

# plotly_plot <- ggplotly(plot, tooltip = "text") ##plotly_plot makes it interactive 
# 
# plotly_plot


##Choropleth map

##Read in shapefile 
house_districts_shp <- st_read("C:/Users/zmarroquinmarroquin/OneDrive - Furman University/Desktop/FTMData/sc_sldl_2011_to_2021")

##Filter Merged_data to exclude negative values
filtered_data <- cleaned_df[cleaned_df$TotalAmount >= 0, ]

## Group by District 
district_summarized_data <- filtered_data %>%
  group_by(District) %>%
  summarize(TotalAmount = sum(TotalAmount, na.rm = TRUE))

##Figure out the candidate with the highest donations for each district
top_candidates <- filtered_data %>%
  group_by(District) %>%
  slice(which.max(TotalAmount))

top_candidates <- top_candidates %>%
  rename(top_candidate_amount = TotalAmount) #renaming a column 

##Merge top_candidates and summarized_data to have a table for the popup data
merged_with_popup <- merge(top_candidates, district_summarized_data, by = "District")

##Merging my data with shapefile
Merged_data <-  merge(house_districts_shp, merged_with_popup, all.x = TRUE)

##Creating the choropleth map with no negative values and grouped by district
ggplot() +
  geom_sf(data = Merged_data, aes(fill = TotalAmount)) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue", 
    na.value = "grey50", 
    trans = "identity",
    limits = c(0, max(Merged_data$TotalAmount)),
    breaks = seq(0, max(Merged_data$TotalAmount), by = 75000),
    labels = scales::number_format(scale_factor = 1e-3, suffix = "K")
  ) +
  theme_minimal()

##Using Mapview package because It'll be easier to add popups this way 
mapview(Merged_data, zcol = c("TotalAmount", "Broad_Sector"),
        col.regions = colorRampPalette(c("lightblue", "darkblue")),
        legend = TRUE, 
        popup = popupTable(Merged_data,
                   zcol = c("District",
                            "TotalAmount",
                            "Candidate",
                            "top_candidate_amount",
                            "Broad_Sector"))) 


