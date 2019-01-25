# clean out the 311 data and choose only City of Miami data
# match a tract number using lattitude and longitude of 311 data

# created by Hye Seon Yi
# reference: https://commercedataservice.github.io/tutorial_311_trees_p1/

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    print(paste("installing",x))
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  } else{
    print(paste(x,"found"))
  }
}

## These lines load the required packages
packages <- c("rgeos", "rgdal", "maptools", "dplyr", "reshape2", "ggplot2", 
              "ggmap", "rstan","StanHeaders","gridExtra", "raster",
              "sp", "RColorBrewer", "fmsb")

##Apply the pkgTest using lapply()
lapply(packages, pkgTest)

# set path to the right location
setwd("~/Desktop/Pray/Up to God/Data Mining/311/deadline/kmeans")

# read 311 data file for Miami-Dade County
reports_all <-read.csv("input/311_data.csv")
head(reports_all)

# Choose only City of Miami data only
reports_all <- reports_all[tolower(reports_all$City) == "city_of_miami",]

# clean the data
# change all of 311 report dataset
# change case owners to have only underlines in data
reports_all$Case.Owner <- gsub("-","_",reports_all$Case.Owner)
reports_all$category <- reports_all$Case.Owner

# find each category in 311 reports
if(is.factor(reports_all$Case.Owner)){
  category.cases <- levels(reports_all$Case.Owner)
} else {
  category.cases <- unique(reports_all$Case.Owner)
}

# change date for the corrrect format
reports_all$Date <- as.Date(reports_all$Ticket.Created.Date,format="%m/%d/%Y")

# We also remove reports with no location. These happen to be infrequent.
reports_all <- reports_all[!is.na(reports_all$Latitude) & !is.na(reports_all$Longitude),]

##################################################
# Load Tracts shapefile of Miami-Dade County
##################################################
tracts <-readOGR("../../source/input/Tracts_2010", "Tracts_2010")
tracts@data$id <-rownames(tracts@data)
tracts.points <-fortify(tracts, region="id")
tracts.df <-left_join(tracts.points, tracts@data, by = "id")

# We convert each report to a spatial points (sp) object.
reports.sp <- SpatialPoints(coord=reports_all[, c("Longitude","Latitude")],
                            proj4string=tracts@proj4string)

# We use the over function to find the tract for each report
reports_all <- cbind(reports_all,over(x=reports.sp,y=tracts))
reports_all <- reports_all[!is.na(reports_all$TRACTCE10),]

# select only 311 data and tract number
reports_all <- reports_all[1:29]

##################################################
# Load boundary shapefile of the City of Miami
##################################################
boundary <-readOGR("../../source/input/Municipal_Coastal_Boundary", "Municipal_Coastal_Boundary")

# find the boundary of Miami
miami_boundary <- boundary[tolower(boundary$NAME) == "miami", ]

miami_boundary@data$id <-rownames(miami_boundary@data)
miami_boundary.points <-fortify(miami_boundary, region="id")
miami_boundary.df <-left_join(miami_boundary.points, miami_boundary@data, by = "id")

##################################################
# Load boundary shapefile from Dr. Kim Lersch
##################################################
miami_shape<-readOGR("../../source/input/Shapefile", "Category_by_Tract")

miami_shape@data$id <-rownames(miami_shape@data)
miami_shape.points <-fortify(miami_shape, region="id")
miami_shape.df <-left_join(miami_shape.points, miami_shape@data, by = "id")

miami_shape.df <- miami_shape.df[,-c(12:122)]
colnames(miami_shape.df)[11] <- "TRACTCE10"

###########################################################
# combine 311 data with tract number and census information
###########################################################
census_title_labels <- c(tot_pop = "Total population",
                         med_income = "Median household income",
                         per_cap_income = "Per capita income", 
                         perc_under_poverty = "Percent of family below the poverty level",
                         perc_unemployment_rate = "Percent of unemployment rate", 
                         perc_white = "Percent of White",
                         perc_aa = "Percent of African American",
                         perc_hispanic = "Percent of Hispanic",
                         perc_other_race = "Percent of other race", 
                         perc_ed_high = "Percent of High school graduates",
                         perc_ed_college = "Percent of College graduates",
                         perc_ed_att_col_deg = "Percent of graduate or professional degree",
                         perc_other_ed = "Percent of other education",
                         perc_citizen = "Percent of US citizen",
                         perc_non_citizen = "Percent of non US citizen")

census_tract <- read.csv('input/acsvars_070718.csv')
names(census_tract)[4:18] <- names(census_title_labels)
names(census_tract)[3] <- "TRACTCE10"

#add leading zeroes so tract format from census matches 311 website
census_tract$TRACTCE10 <- sprintf('%06d',census_tract$TRACTCE10)

# choose FL state & Miami-Dade County
census_tract <- census_tract[census_tract$state == 12 & census_tract$county == 86,]

# choose reports with census tract numbers
reports_all <- semi_join(reports_all, census_tract, by = "TRACTCE10")

# Here we provide the dates for 8 major storms
# https://en.wikipedia.org/wiki/Category:Hurricanes_in_Florida 
# https://en.wikipedia.org/wiki/List_of_Florida_hurricanes_(2000%E2%80%93present)#2013 

storm.dates <- as.Date(c("2013-06-05", # Tropical Storm Andrea (2013) 
                         "2015-08-28", # Erika https://en.wikipedia.org/wiki/File:Tropical_Storm_Erika_2015_rainfall_graphic.gif 
                         "2016-06-05", # Tropical Storm Colin (2016)  
                         "2016-08-19", # Zika
                         "2016-09-01", # Hermine 
                         "2016-09-14", # Julia
                         "2016-10-07", # Mathew 
                         "2017-09-10"  # Irma
), #
format = "%Y-%m-%d")

# set labels for the categories
category_labels <- c(Animal_Services = "Animal Services", 
                     COM_Code_Enforcement = "Code Enforcement",
                     Communications_Department = "Communications", 
                     Public_Works_Construction_6_60 = "Construction",
                     Public_Works_Right_Of_Way_4_60 = "Public Right-of-way",
                     Public_Works_Road_And_Bridges_16_60 = "Road and Bridges", 
                     Public_Works_Traffic_Engineering_10_60 = "Traffic Engineering",
                     Public_Works_traffic_Signals_And_Signs_15_60 = "Traffic Signals/Signs",
                     RAAM_27_93 = "RAAM",
                     Regulatory_and_Economic_Resources = "Regulatory/Economic Resources")

# set short labels for the categories
category_short_labels <- c(Animal_Services = "Animal", 
                           COM_Code_Enforcement = "Code",
                           Communications_Department = "Communications", 
                           Public_Works_Construction_6_60 = "Construction",
                           Public_Works_Right_Of_Way_4_60 = "Right-of-way",
                           Public_Works_Road_And_Bridges_16_60 = "Road/Bridges", 
                           Public_Works_Traffic_Engineering_10_60 = "Engineering",
                           Public_Works_traffic_Signals_And_Signs_15_60 = "Signals/Signs",
                           RAAM_27_93 = "RAAM",
                           Regulatory_and_Economic_Resources = "Resources")

# set labels for the storm - only Irma
storm_labels <- c(Storm1 = "Andrea (2013)", 
                  Storm2 = "Erika (2015)",
                  Storm3 = "Colin (2016)", 
                  Storm4 = "Zika (2016)",
                  Storm5 = "Hermine (2016)",
                  Storm6 = "Julia (2016)",
                  Storm7 = "Mathew (2016)", 
                  Storm8 = "Irma (2017)")

# set storm name - only Irma
storm_names <- paste("Storm",1:8,sep="")

# set output path
output_dir <- "output_miami"
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

##################################################
# Plot counts of all 311 report dataset by date
##################################################
ggplot() +
  theme_bw() +
  geom_point(aes(x=Date,log(freq,base=10),alpha=log(freq,base=10)),data=plyr::count(reports_all, "Date")) +
  geom_vline(aes(xintercept = as.numeric(storm.dates)),color="red",alpha=.7,linetype=2,
             data = data.frame(storm.dates)) +
  xlim(as.Date("2013-01-01",format="%Y-%m-%d"),as.Date("2018-01-01",format="%Y-%m-%d")) +
  theme(legend.position="none") +
  labs(x="day",y=expression(log[10](311)),
       title = "The City of Miami 311 report counts by date")

ggsave(paste0(output_dir,"/311_all.png"),dpi = 500)

#We plot the tract data using ggplot2
#For other color palette options, see the ggplot documentation at 
#http://docs.ggplot2.org/current/scale_brewer.html

# prepare to drop the axes and ticks but leave the guides and legends
# We can't just throw down a theme_nothing()!
drop_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

##################################################
# Plot boundary and tract map of Miami
##################################################
# color map
ggplot() +
  geom_polygon(aes(long,lat,group=group,color="Tract"),
               data=miami_shape.df, 
               fill="white") +
  geom_polygon(aes(long,lat,group=group,color="Miami"),
               data=miami_boundary.df,fill="grey",alpha=0.5) +
  scale_color_discrete(name=NULL) +
  theme_bw() +
  coord_equal()

ggsave(paste0(output_dir, "/miami_tracts_boundary.png"),dpi = 500)

# find the numbers of 311 reports according to their tract number
df <- as.data.frame(table(reports_all[,c("TRACTCE10")]))
colnames(df) <- c("TRACTCE10","Reports")
df <- df[df$Reports > 1,]

# group tracts according to # of reports
df <- df %>%
  mutate(rp_grp = cut(Reports, 
                      as.integer(seq(min(Reports), max(Reports), length.out = 6)),
                      include.lowest = TRUE,
                      dig.lab = 5))
unique(df$rp_grp)
df$rp_grp 

btracts.df <- inner_join(miami_shape.df,df,by="TRACTCE10")

##################################################
# plot all 311 reports by tracts
##################################################
ggplot(btracts.df)+
  aes(x=long,y=lat,group=group,fill=rp_grp)+
  geom_polygon(color="grey",size=.1)+
  geom_polygon(aes(x=long,y=lat,group=group),data=miami_boundary.df,fill="white",
               color="black",alpha=0) +
  theme_bw() +
  drop_the_axes +
  coord_equal() +
  labs(title = "Total number of 311 reports of the City of Miami") +
  scale_fill_brewer(name = "# of 311 reports", palette = 7)

ggsave(paste0(output_dir, "/reports_all_map.png"),dpi = 500)

# find the numbers of 311 reports according to their tract number
df <- as.data.frame(table(reports_all[,c("TRACTCE10")]))
colnames(df) <- c("TRACTCE10","Reports")
df <- df[df$Reports > 1,]

# join with census data
df <- inner_join(df, census_tract, by = "TRACTCE10")

# choose only necessary fields
df <- df[, c(1:2,5)] %>% 
  mutate(rate = Reports/tot_pop) %>%
  arrange(desc(rate))

write.csv(df, 
          file = paste0(output_dir, "/311_per_population_by_tract.csv"),
          row.names = FALSE)

btracts.df <- inner_join(miami_shape.df,df,by="TRACTCE10")

##################################################
# plot all 311 reports per population by tracts
##################################################
ggplot(btracts.df)+
  aes(x=long,y=lat,group=group,fill=rate)+
  geom_polygon(color="grey",size=.1)+
  geom_polygon(aes(x=long,y=lat,group=group),data=miami_boundary.df,fill="white",
               color="grey",alpha=0) +
  theme_bw() +
  drop_the_axes +
  coord_equal() +
  labs(title = "311 reports per population of the City of Miami") +
  scale_fill_continuous(expression("Rate"),
                        low="yellow",high="red",na.value="white")+
  
  ggsave(paste0(output_dir, "/reports_per_population_map.png"),dpi = 500)


# preprocessing is done in 311.R
save.image("~/Desktop/Pray/Up to God/Data Mining/311/deadline/kmeans/miami.RData")

# process data to run kmeans algorithm
# count reports by tract #
df <- as.data.frame(table(reports_all[,c("TRACTCE10")]))
colnames(df) <- c("TRACTCE10","total")

# create 311 report signiature dataframe
flag <- "category"
# flag <- "issue_type"

# initialize
freq_df <- df

if (flag == "category"){
  # count reports by each category by tract #
  for(temp.c in category.cases){
    # temp.c <- "Animal_Services"
    print(temp.c)
    case_df <- as.data.frame(table(reports_all[reports_all$category==temp.c,c("TRACTCE10")]))
    if (nrow(case_df) != 0){
      colnames(case_df) <- c("TRACTCE10", temp.c)
      freq_df <- merge(x = freq_df, y = case_df, by = c("TRACTCE10"), all.x=TRUE)
    }
  }
} else if (flag == "issue_type"){
  # find each issue type in 311 reports
  issue_types <- reports_all %>% 
    group_by(Issue.Type) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    dplyr::select(Issue.Type)
  
  # count reports by each issue type by tract #
  for(temp.iss in issue_types$Issue.Type){
    # temp.iss <- "TRAFFIC STUDY REQUEST - SCHOOL RELATED - SIGN AROUND SCHOOL"
    print(temp.iss)
    issue_types_df <- as.data.frame(table(reports_all[reports_all$Issue.Type==temp.iss,c("TRACTCE10")]))
    if (nrow(issue_types_df) != 0){
      colnames(issue_types_df) <- c("TRACTCE10", temp.iss)
      freq_df <- merge(x = freq_df, y = issue_types_df, by = c("TRACTCE10"), all.x=TRUE)
    }
  }
}

freq_df[is.na(freq_df)] <- 0

# find # of columns (10 categories)
print(colnames(freq_df))

# normalize the result
# remove tract with no reports (only one row: tractce10 = 980100)
divided_df <- freq_df[freq_df$total > 1,]
divided_df[,c(-1,-2)] <- sweep(divided_df[,c(-1,-2)],1,FUN="/",divided_df$total)
kmdata <- divided_df[,c(-1,-2)]
# kmdata <- scale(kmdata)

# find within sum of squares (WSS) for each k
wss <- numeric(10)
for (k in 1:10) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=100)$withinss)

# plot wss graph
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

# save the plot
if (flag == "category") {
  # save the resulted plot in pdf
  dev.copy(png, paste0(output_dir, "/wss_kmeans_category.png"))
  dev.off()
} else if (flag == "issue_type"){
  # save the resulted plot in pdf
  dev.copy(png, paste0(output_dir, "/wss_kmeans_issue_types.png"))
  dev.off()
}

# run kmeans
set.seed(6)

# set value k for kmeans
k=6

output_dir <- paste0("output_cluster_",flag,k)
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

km <- kmeans(kmdata, centers=k, nstart=1000) # run kmeans function
km$size # find the sizes of k clusters

################################
# plotting centers of clusters
################################
if (flag == "category"){
  centers.vector <- t(as.table(km$centers))
  centers.percent <- sweep(centers.vector,1,FUN="*",100)
  
  colors <- brewer.pal(nrow(centers.vector), "Paired")
  par(xpd=TRUE, mar = c(5, 4, 4, 2) + 0.1 + c(0,0,0,3))
} else if (flag == "issue_type"){
  center.table <- as.table(km$centers)
  centers.vector <- t(center.table[,c(1:24)])
  centers.percent <- sweep(centers.vector,1,FUN="*",100) 
  
  # get enough colors for # of features
  getPalette = colorRampPalette(brewer.pal(10, "Paired"))
  colors <- getPalette(nrow(centers.vector))
  
  par(xpd=TRUE, mar = c(5, 4, 4, 2) + 0.1 + c(0,0,0,3))
}

# plot the 4 centers
barplot(centers.percent, 
        main=paste0("Centers for the ",k," clusters"),
        xlab="clusters",
        ylab="percent (%)",
        col=colors, 
        beside=TRUE
)

# save the resulted plot in png
if (flag == "category"){
  legend(x="topright",
         inset=c(-0.1,0), 
         legend=rownames(centers.vector),
         fill=colors,
         bty = "n",
         cex=0.9)
  
  dev.print(png, 
            paste0(output_dir, "/centers_clusters_category.png"), 
            width=1070,
            height=600)
} else if (flag == "issue_type"){
  legend(x="topright",
         inset=c(-0.2,-0.07), 
         #inset=c(-0.1,-0.14), 
         legend=rownames(centers.vector),
         fill=colors,
         bty = "n",
         cex=0.75)
  
  dev.print(png, 
            paste0(output_dir, "/centers_clusters_issue_type.png"),
            width=1070,
            height=600)
}

# reset
par(mar=c(5, 4, 4, 2) + 0.1) 

################################
# Prepare to map the cluster map
################################

# find clustering information to each tract
cluster.vector <- as.table(km$cluster)
divided_df$cluster <- cluster.vector

# add clustering information to tract.df
btracts.df <- inner_join(miami_shape.df,divided_df,by="TRACTCE10")

# plot a map with kmeans cluster information
ggplot(btracts.df)+
  ggtitle("(Miami-Dade County Tracts) Cluster distribution") +
  aes(x=long,y=lat,group=group,fill=as.factor(cluster))+
  geom_polygon(color="grey",size=.1)+
  geom_polygon(aes(x=long,y=lat,group=group),data=miami_boundary.df,fill="white",
               color="grey",alpha=0) +
  scale_fill_discrete(name="Clusters", breaks=c(1:k)) + 
  coord_equal()

# save the map
dev.print(png, 
          paste0(output_dir, "/cluster_map_", flag, ".png"),
          width=600,
          height=600)

#####################             
# plot a radar chart
##################### 

# calculate census mean for each cluster
btracts.df <- inner_join(divided_df,census_tract,by="TRACTCE10")
btracts.df <- as.data.frame(lapply(btracts.df, as.numeric))

cluster_census_mean <- btracts.df %>% 
  group_by(cluster) %>%
  summarise(median_income = mean(med_income),
            mean_income = mean(per_cap_income),
            mean_under_poverty = mean(perc_under_poverty),
            mean_unemployment = mean(perc_unemployment_rate),
            mean_white = mean(perc_white),
            mean_aa = mean(perc_aa),
            mean_hispanic = mean(perc_hispanic),
            mean_ed_high = mean(perc_ed_high),
            mean_ed_college = mean(perc_ed_college),
            mean_ed_att_col_deg = mean(perc_ed_att_col_deg),
            mean_citizen = mean(perc_citizen)) %>%
  arrange(cluster) 

colnames(cluster_census_mean) =
  c("cluster", #1
    "median income", #2
    "per income capita", #3
    "under poverty(%)", #4
    "unemployment(%)", #5
    "white population(%)", #6
    "african american(%)", #7
    "hispanic(%)", #8
    "high school graduates(%)", #9
    "college graduates(%)", #10
    "graduate or professional degree(%)", #11
    "citizen(%)") #12

# convert to matrix to find the maximum value
cluster_census_mean.m <- as.matrix(cluster_census_mean)
max_num <- max(cluster_census_mean.m[,4:12]) # calculate except median income
max_income <- max(cluster_census_mean.m[,2]) # max median income
adjust_value <- max_income/max_num
# remove "per income capita" and "graduate or professional degree"
cluster_census_mean.df <- cbind(cluster_census_mean[,2]/adjust_value)
cluster_census_mean.df <- cbind(cluster_census_mean[,c(4:10,12)], 
                                cluster_census_mean.df)

num <- ncol(cluster_census_mean.df)

# set row names
rownames(cluster_census_mean.df) <- paste0("cluster", 1:k)
cluster_census_mean.df <- rbind(rep(max_num,num) , rep(0,num) , cluster_census_mean.df)

# set colors for the radar chart
if(k == 2){
  colors_border=c(rgb(0,197,199,maxColorValue=255,alpha=255),
                  rgb(255,98,99,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(0,197,199,maxColorValue=255,alpha=76))
} else if(k == 3){
  colors_border=c(rgb(255,98,99,maxColorValue=255,alpha=255),
                  rgb(0,199,57,maxColorValue=255,alpha=255), 
                  rgb(95,148,255,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(0,199,57,maxColorValue=255,alpha=76), 
              rgb(95,148,255,maxColorValue=255,alpha=76))
} else if(k == 4){
  colors_border=c(rgb(255,98,99,maxColorValue=255,alpha=255),
                  rgb(104,183,0,maxColorValue=255,alpha=255), 
                  rgb(0,197,199,maxColorValue=255,alpha=255), 
                  rgb(216,92,255,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(104,183,0,maxColorValue=255,alpha=76), 
              rgb(0,197,199,maxColorValue=255,alpha=76), 
              rgb(216,92,255,maxColorValue=255,alpha=76))
} else if(k == 5){
  colors_border=c(rgb(255,98,99,maxColorValue=255,alpha=255),
                  rgb(158,171,0,maxColorValue=255,alpha=255), 
                  rgb(0,201,142,maxColorValue=255,alpha=255), 
                  rgb(0,175,250,maxColorValue=255,alpha=255),
                  rgb(251,51,244,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(158,171,0,maxColorValue=255,alpha=76), 
              rgb(0,201,142,maxColorValue=255,alpha=76), 
              rgb(0,175,250,maxColorValue=255,alpha=76),
              rgb(251,51,244,maxColorValue=255,alpha=76))
} else if(k == 6){
  colors_border=c(rgb(255,98,99,maxColorValue=255,alpha=255),
                  rgb(184,163,0,maxColorValue=255,alpha=255),
                  rgb(158,171,0,maxColorValue=255,alpha=255), 
                  rgb(0,201,142,maxColorValue=255,alpha=255), 
                  rgb(0,175,250,maxColorValue=255,alpha=255),
                  rgb(251,51,244,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(184,163,0,maxColorValue=255,alpha=76),
              rgb(158,171,0,maxColorValue=255,alpha=76), 
              rgb(0,201,142,maxColorValue=255,alpha=76), 
              rgb(0,175,250,maxColorValue=255,alpha=76),
              rgb(251,51,244,maxColorValue=255,alpha=76))
} else if(k == 7){
  colors_border=c(rgb(255,98,99,maxColorValue=255,alpha=255),
                  rgb(200,156,0,maxColorValue=255,alpha=255),
                  rgb(0,191,0,maxColorValue=255,alpha=255), 
                  rgb(0,202,151,maxColorValue=255,alpha=255), 
                  rgb(0,184,239,maxColorValue=255,alpha=255),
                  rgb(177,119,255,maxColorValue=255,alpha=255),
                  rgb(255,16,214,maxColorValue=255,alpha=255))
  
  colors_in=c(rgb(255,98,99,maxColorValue=255,alpha=76),
              rgb(200,156,0,maxColorValue=255,alpha=76),
              rgb(0,191,0,maxColorValue=255,alpha=76), 
              rgb(0,202,151,maxColorValue=255,alpha=76), 
              rgb(0,184,239,maxColorValue=255,alpha=76),
              rgb(177,119,255,maxColorValue=255,alpha=76),
              rgb(255,16,214,maxColorValue=255,alpha=76))
}

par(mar = rep(1.3, 4))

# plot the radar chart
radarchart(cluster_census_mean.df, # data
           pcol=colors_border, # color for plot data
           pfcol=colors_in, # color for filling
           plwd=1, # line width for plot data
           axistype=1, # type of axis; 1=center label
           cglcol="grey", # radar grid color
           cglty=1, # radar grid line type; 1=solid
           cglwd=0.5, # radar grid line width
           axislabcol="grey", 
           caxislabels=as.integer(seq(0,max_num,max_num/4)), # character label for center label
           plty=1, # plot line type
           vlcex=1.2) # label font size magnification

legend(x=1, y=1.3, 
       legend = rownames(cluster_census_mean.df[-c(1,2),]), 
       bty = "n", 
       pch=20, 
       col=colors_border, 
       text.col = colors_border, 
       cex=1.2, 
       pt.cex=3)

dev.print(png, 
          paste0(output_dir, "/radar_", flag, ".png"),
          width=700,
          height=600)

# reset
par(mar=c(5, 4, 4, 2) + 0.1)

