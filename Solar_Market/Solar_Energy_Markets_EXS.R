# Exercise in finding the best way to invest in social energy
#####################################################
library(XML)
library(dplyr)

# Dataset 1 - latlong (shpfile download and added lat/long in GIS)
#source: http://www.arcgis.com/home/item.html?id=4e02a13f5ec6412bb56bd8d3dadd59dd
library(raster)
cities <- tbl_df(shapefile("Solar_Market//US_cities/US_cities.shp")) 
cities <- cities %>%  plyr::rename(c("ycoord"="LAT")) %>% plyr::rename(c("xcoord"="LONG")) %>% 
  filter(CLASS=="city") %>% dplyr::select(NAME, ST, LAT, LONG)

# Dataset 2 - Electricity Prices (web table)
library(RCurl)
data <- getURL("https://www.eia.gov/electricity/monthly/epm_table_grapher.cfm?t=epmt_5_6_a", ssl.verifypeer = FALSE)
perftable <- readHTMLTable(data[[1]], stringsAsFactors = T)
elctprice <- perftable[[2]]
elctprice <- tbl_df(elctprice[,c(1,10)])
colnames(elctprice) <- c("State", "Price")
# Add abbreviation of States:
us <- readHTMLTable("http://www.softschools.com/social_studies/state_abbreviations/",  
                    header=T, which=1,stringsAsFactors=F, trim = T, skip.rows = 3)
colnames(us) <- us[3,]
us <- tbl_df(us[4:nrow(us),])
elctprice <- elctprice %>% mutate(State=toupper(State)) %>%
  left_join(us) %>% dplyr::select(Abbreviation, Price) %>% plyr::rename(c("Abbreviation"="ST")) %>% na.omit()

### Whole DF
cities <- cities %>% left_join(elctprice)
cities$Price <- as.numeric(cities$Price)
cities <- cities %>% na.omit() %>% mutate(idx_SUN = scale(LAT), idx_SUN=idx_SUN+abs(min(idx_SUN)),
                            idx_PRICE = scale(Price), idx_PRICE=idx_PRICE+abs(min(idx_PRICE)),
                            Market_Potential_Index = idx_SUN+idx_PRICE)
Market_Potential <- quantile(cities$Market_Potential_Index)
for (i in 1:nrow(cities)){
if (cities$Market_Potential_Index[i]<=Market_Potential[2]){cities$Market_Potential[i]<-"High"} else{
  if(cities$Market_Potential_Index[i]>Market_Potential[2]&cities$Market_Potential_Index[i]<=Market_Potential[3]){ cities$Market_Potential[i]<-"Medium"} else{
    cities$Market_Potential[i]<-"Low"}}}
  
cities$Market_Potential_Index <- -cities$Market_Potential_Index+9
                                 
                               
# Plot a map
df <- cities
df <- df%>% plyr::rename(c("Market_Potential_Index"="Solar Market Potential index"))%>%
  plyr::rename(c("Market_Potential"="Market Potential Level")) %>% 
  plyr::rename(c("Price"="Electricity Price (Cents per KW)"))
map <- shapefile("Solar_Market//US_cities/US_cities.shp")
map <- merge(map, df)

map1 <- map[,c(1,2, 54, 57,58)]
map1 <- map1[na.omit(map1),]
library(mapview)
m <- mapview(map1, legend=T, zcol="Solar Market Potential index")
m


# Get ClimateAttitude
library(twitteR)
setup_twitter_oauth("", "", "", "")

cities <- cities[order(-cities$Market_Potential_Index),]
poten_cities <- cities[1:10,] # The cities with highest potential
poten_cities$LatLong <- paste0(poten_cities$LAT, ",", poten_cities$LONG, ", ", " 2mi")
date = as.character(Sys.Date()) # Today's date
poten_cities$Twitnum <- 0
for (i in 1:nrow(poten_cities)){
  tw = searchTwitter('#climatechange', n = 1e4, since = date, geocode=poten_cities$LatLong[i])
  poten_cities$Twitnum[i] <- length(tw)
  }
bupotencities <- poten_cities
poten_cities$Potential <- scale(poten_cities$Twitnum)
poten_cities <- poten_cities[order(-poten_cities$Potential),]
poten_cities
library(plotly)
poten_cities$NAME <- factor(poten_cities$NAME, levels = poten_cities[["NAME"]])
save(poten_cities, file = 'Solar_Market/potencities.rda')
p <- plot_ly(poten_cities, x=~poten_cities$NAME ,y=~poten_cities$Potential, type="bar", 
        marker = list(color = 'rgb(158,202,225)', range ("-1", "-0.5", "Mean", "0.5", "1"),
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Solar Market Potential - Twitter Analysis",
         xaxis = list(title = ""),
         yaxis = list(title = "#climatechange Tweets Relative to the Mean"))
       
      
p
