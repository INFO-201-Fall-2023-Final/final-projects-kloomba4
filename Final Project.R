library(dplyr)

# Read datasets
df1 <- read.csv("year-ended-2018-ggerih-csv-2.csv")
df2 <- read.csv("adult-depression-lghc-indicator-2.csv")

#Check for duplicates
duplicated_rows_df1 <- df1[duplicated(df1$year) | duplicated(df1$year, fromLast = TRUE), ]
duplicated_rows_df2 <- df2[duplicated(df2$Year) | duplicated(df2$Year, fromLast = TRUE), ]


#Combine the dataframes
joined_df <- left_join(df1, df2, by = c("year" = "Year"))
joined_df <- left_join(df1, df2, by = c("year" = "Year"), relationship = "many-to-many")

joined_df <- joined_df[1:24000, ]

# Data Cleaning
summary(joined_df)

duplicated_rows <- joined_df[duplicated(joined_df), ]
if (nrow(duplicated_rows) > 0) {
  joined_df <- unique(joined_df)
}


# Create a new categorical variable
mean_emissions <- mean(joined_df$data_val, na.rm = TRUE)

joined_df$emission_category <- ifelse(joined_df$data_val > mean_emissions, "above average", "below average")


# Create a new continuous/numerical variable ("difference between upper and lower CL”)
joined_df <- mutate(joined_df,
                    CL_diff =  Upper.95..CL - Lower.95..CL
)


# Create a summarization data frame
common_variable <- "year"  

summary_df <- summarise(
  group_by(joined_df, .data[[common_variable]]),
  mean_emissions = mean(data_val, na.rm = TRUE),
  CL_diff = median(CL_diff, na.rm = TRUE),
  total_rows = n()
)

write.csv(joined_df, file = "joined_df.csv", row.names = FALSE)



