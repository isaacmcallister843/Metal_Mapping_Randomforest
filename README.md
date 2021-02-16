# Metal_Mapping_Randomforest

![Pb - RF](https://user-images.githubusercontent.com/78721353/108032572-8d14a100-6fe7-11eb-9be0-e332c216c414.png)
#### Figure 1: Randomforest Predictive Map (area of exceedance in grey) 

## Motivation 
Similar to the Metal_Mapping_GAM project, this project seeks to use the RandomForest ML model to create maps of metal concentrations. This approach has several advantages over the GAM approaches; it is much simpler, results are easy to read, the ranger package in R provides easy work with rasters as well. 

## Intellectual Property
Since I worked in a team to develop these products for large clients I do not have permission to distribute the raw data used in these projects. I have also renamed any identifying files and removed the location data from the showcased images. 

## Data and Modeling
The data was collected by external teams and was presented as a csv and after processing was in the following columns: 

|sample_id|Easting|Northing|Elevation|Slope|Aspect|metal_1|metal_2|...|
|---------|-------|--------|---------|-----|------|-------|-------|---|

In this case slope, aspect, and elevation were extracted using GIS tools. These values were given in a geo_tiff and I wrote a search program to retrieve them for each sample location. The random forest algorithm was applied using the ranger package. This package had several advantages, including the ability to add a weighting vector. After experimentation I found that applying less weight to observations further from the average easting and northing value created the best predictions.

The metals_mapping file involves much of my rough work, while the random_forest_map is the lightweight and fast approach.

## Features 
- High accuracy predictions
- Lightweight and simple modeling approach
- Map generation and raster output








