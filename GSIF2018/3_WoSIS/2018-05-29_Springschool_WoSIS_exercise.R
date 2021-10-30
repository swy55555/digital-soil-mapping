
# working directory
setwd("/home/user/Downloads/")

#download
download.file("ftp://public:public@ftp.isric.org/wosis_latest/wosis_latest_orgc.tsv", "wosis_latest_orgc.tsv")

# read
orgc = read.table("wosis_latest_orgc.tsv", sep="\t",quote = "",header=TRUE)

# look
dim(orgc)
colnames(orgc)
orgc[1:10, 1:8]

# plot
hist(log(orgc[,"orgc_value_avg"]))
plot(orgc$longitude, orgc$latitude)
library(leaflet)
mymap <- leaflet(orgc) %>% addProviderTiles(providers$Stamen.Terrain) %>% addMarkers(~longitude, ~latitude, popup = ~paste(as.character(upper_depth), as.character(lower_depth), as.character(orgc_value_avg), sep=" | "), label = ~paste(as.character(upper_depth), as.character(lower_depth), as.character(orgc_value_avg), sep=" | "), clusterOptions = markerClusterOptions())
mymap



