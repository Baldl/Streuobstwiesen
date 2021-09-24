#' @name 006_extraction.R
#' @description 
#' @docType function
#' @param pol sf object
#' @param rasterStack terra rast object
#' @param bufferSize default = -20
#' @param idColName string default = "ID"
#' @return 
#' 

rasterStack = terra::rast("D:/Natur40/ForestModellingHessen/ForestModellingRLP/data/001_raw_data/sentinel/summer/BOA/S2A2A_20190629_108__BOA_10.dat")
study_area = sf::read_sf(file.path(envrmt$study_area, "study_area.gpkg"))
study_area = sf::st_transform(study_area, crs(rasterStack))
rasterStack = terra::crop(rasterStack, study_area)
names(rasterStack) <- c("B01",  "B02","B03","B04","B05","B06","B07","B08","B09", "B11", "B12")
terra::writeRaster(rasterStack, file.path(envrmt$sentinel, "sentinel.tif"), overwrite = TRUE)

#-----------
rasterStack = raster::stack(file.path(envrmt$sentinel, "sentinel.tif"))
pol = sf::read_sf(file.path(envrmt$hlnug, "streuobst.gpkg"))
pol = sf::st_transform(pol, crs(rasterStack))
pol$OBJ_ID = 1:nrow(pol)
pol$JAHR <- NULL
pol$OBJ_NR <- 
pol$DWH_ID <- NULL
pol = na.omit(pol)
pol$ID = NULL

idColName = "OBJ_ID"

  
  # extract all polygons from raster stack
  result = lapply(seq(nrow(pol)), function(i){
    cur = pol[i,]
    ext <- raster::extent(cur)
    
      
      sen = raster::crop(rasterStack, ext)
      all = RStoolbox::spectralIndices(sen,
                                       redEdge1 = "B05",
                                       redEdge2 = "B06",
                                       redEdge3 = "B07",
                                       nir = "B08",
                                       swir2 = "B11", 
                                       swir3 = "B12",
                                       red = "B04",
                                       green = "B03", 
                                       blue = "B02")
      
      all = raster::stack(sen, all)
   
      
      df = raster::extract(all, cur, df = TRUE)
      df = df %>% dplyr::mutate(cur %>% select((!!sym(idColName))))
      df$ID <- NULL
      
      print(paste("Extracted Polygon", i, "of", nrow(pol)))
      return(df)
  }) # end lapply
  
  
  # backup save
  saveRDS(result, file.path(paste0(tempdir(), "extract_backup.RDS")))

  # foramting of extraction
  res = result[sapply(result, is.data.frame)]
  res = do.call(rbind, res)
  return(res)

extr_merge = merge(res,pol, by = "OBJ_ID")
saveRDS(extr_merge, file.path(envrmt$model_training_data, "extraction.RDS"))
