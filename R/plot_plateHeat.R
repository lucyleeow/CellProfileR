

# Plot a feature (column) as plate heatmap for each plate.
# Scale is uniform across all plates
############################################################

plot_plateHeat <- function(
  df,          # df of per well data
  column,      # approximate column name of feature of interest as string 
  # matching will be done with grep
  plate = 384  # number of wells in plate
){
  
  # check inputs
  assertthat::assert_that(is.character(column),
                          length(column) == 1,
                          msg = "Check that 'column' is single character")
  
  
  # grep for column name
  grep_pattern <- paste(column, "$", sep = "")
  column_full <- colnames(df)[grep(grep_pattern, colnames(df))]
  
  assertthat::assert_that(length(column_full) == 1,
                          msg = "Did not find single column when matching with 'column'. Try different 'column' string")
  
  
  # find med, max and min to set common gradient scale
  feature_vect <- df[ , get(column_full)]
  
  med <- median(feature_vect)
  max <- max(feature_vect)
  min <- min(feature_vect)
  
  
  # make plots
  plots <- df %>%
    group_by(Metadata_Barcode) %>%
    do(plots = raw_map(data = .[,column_full],
                       well = .$Metadata_WellID,
                       plate = plate) + 
         ggtitle(.$Metadata_Barcode[1]) +
         scale_fill_gradient2(low = "blue", mid = "white",
                              high = "red", midpoint = med,
                              limits = c(floor(min), ceiling(max)))
    )
  
  for (i in 1:length(unique(df$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}