# ============================================================
# CS5803 Data Visualisation
# LSOA-Level Crime Profile Clustering
# ------------------------------------------------------------
# 1. Load dataset
# ------------------------------------------------------------

data_file <- "Final_DV_Dataset.csv"

if (!file.exists(data_file)) {
  stop(
    paste(
      "Final_DV_Dataset.csv not found.",
      "Place data_modelling_clustering.R and Final_DV_Dataset.csv in the same folder.",
      "Current working directory:",
      getwd()
    )
  )
}

crime_raw <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(crime_raw) <- trimws(names(crime_raw))

# ------------------------------------------------------------
# 2. Flexible column matching
# ------------------------------------------------------------

find_col <- function(possible_names, data_names) {
  match_name <- possible_names[possible_names %in% data_names]
  if (length(match_name) == 0) return(NA)
  match_name[1]
}

month_col <- find_col(c("Month_Label", "Month Label", "Month"), names(crime_raw))
force_col <- find_col(c("Police_Force", "Police Force", "Reported by"), names(crime_raw))
lsoa_col  <- find_col(c("LSOA name", "LSOA_Name", "LSOA.name"), names(crime_raw))
type_col  <- find_col(c("Crime type", "Crime_type", "Crime.type"), names(crime_raw))
group_col <- find_col(c("Crime_Group", "Crime Group", "Crime.Group"), names(crime_raw))
count_col <- find_col(c("Crime_Count", "Crime Count", "Crime.Count"), names(crime_raw))
lat_col   <- find_col(c("Latitude"), names(crime_raw))
lon_col   <- find_col(c("Longitude"), names(crime_raw))

missing <- c()

if (is.na(month_col)) missing <- c(missing, "Month_Label")
if (is.na(force_col)) missing <- c(missing, "Police_Force")
if (is.na(lsoa_col))  missing <- c(missing, "LSOA name")
if (is.na(type_col))  missing <- c(missing, "Crime type")
if (is.na(group_col)) missing <- c(missing, "Crime_Group")
if (is.na(count_col)) missing <- c(missing, "Crime_Count")
if (is.na(lat_col))   missing <- c(missing, "Latitude")
if (is.na(lon_col))   missing <- c(missing, "Longitude")

if (length(missing) > 0) {
  stop(paste("Missing required columns:", paste(missing, collapse = ", ")))
}

# ------------------------------------------------------------
# 3. Create clean internal dataset
# ------------------------------------------------------------

crime <- data.frame(
  Month_Label  = crime_raw[[month_col]],
  Police_Force = crime_raw[[force_col]],
  LSOA_Name    = crime_raw[[lsoa_col]],
  Crime_Type   = crime_raw[[type_col]],
  Crime_Group  = crime_raw[[group_col]],
  Crime_Count  = as.numeric(crime_raw[[count_col]]),
  Latitude     = as.numeric(crime_raw[[lat_col]]),
  Longitude    = as.numeric(crime_raw[[lon_col]]),
  stringsAsFactors = FALSE
)

crime <- crime[
  !is.na(crime$LSOA_Name) &
    !is.na(crime$Crime_Group) &
    !is.na(crime$Crime_Count) &
    !is.na(crime$Latitude) &
    !is.na(crime$Longitude),
]

if (nrow(crime) == 0) {
  stop("Dataset loaded, but no valid rows remain after cleaning.")
}

# ------------------------------------------------------------
# 4. Standardise crime group labels
# ------------------------------------------------------------

crime$Crime_Group <- trimws(crime$Crime_Group)

expected_groups <- c(
  "Acquisitive crime",
  "High harm crime",
  "Public order / ASB",
  "Other crime"
)

crime$Crime_Group[!(crime$Crime_Group %in% expected_groups)] <- "Other crime"

# ------------------------------------------------------------
# 5. Aggregate total crime by LSOA
# ------------------------------------------------------------

total_by_lsoa <- aggregate(
  crime$Crime_Count,
  by = list(LSOA_Name = crime$LSOA_Name),
  FUN = sum,
  na.rm = TRUE
)

names(total_by_lsoa)[2] <- "Total_Crime"

# ------------------------------------------------------------
# 6. Average location by LSOA
# ------------------------------------------------------------

location_by_lsoa <- aggregate(
  cbind(Latitude, Longitude) ~ LSOA_Name,
  data = crime,
  FUN = mean
)

# ------------------------------------------------------------
# 7. Crime group counts by LSOA
# ------------------------------------------------------------

group_counts <- aggregate(
  crime$Crime_Count,
  by = list(
    LSOA_Name = crime$LSOA_Name,
    Crime_Group = crime$Crime_Group
  ),
  FUN = sum,
  na.rm = TRUE
)

names(group_counts)[3] <- "Group_Count"

# Convert long group counts into wide table using base R
wide_counts <- reshape(
  group_counts,
  idvar = "LSOA_Name",
  timevar = "Crime_Group",
  direction = "wide"
)

# Clean column names
names(wide_counts) <- gsub("Group_Count\\.", "", names(wide_counts))
names(wide_counts) <- gsub(" ", "_", names(wide_counts))
names(wide_counts) <- gsub("/", "", names(wide_counts))

# Ensure all expected group columns exist
expected_wide_cols <- c(
  "Acquisitive_crime",
  "High_harm_crime",
  "Public_order__ASB",
  "Other_crime"
)

for (col in expected_wide_cols) {
  if (!(col %in% names(wide_counts))) {
    wide_counts[[col]] <- 0
  }
}

wide_counts[is.na(wide_counts)] <- 0

# ------------------------------------------------------------
# 8. Merge LSOA modelling table
# ------------------------------------------------------------

lsoa_model <- merge(total_by_lsoa, location_by_lsoa, by = "LSOA_Name", all.x = TRUE)
lsoa_model <- merge(lsoa_model, wide_counts, by = "LSOA_Name", all.x = TRUE)

lsoa_model[is.na(lsoa_model)] <- 0

# ------------------------------------------------------------
# 9. Calculate crime group shares
# ------------------------------------------------------------

safe_share <- function(part, total) {
  ifelse(total > 0, part / total, 0)
}

lsoa_model$Acquisitive_Share <- safe_share(
  lsoa_model$Acquisitive_crime,
  lsoa_model$Total_Crime
)

lsoa_model$High_Harm_Share <- safe_share(
  lsoa_model$High_harm_crime,
  lsoa_model$Total_Crime
)

lsoa_model$Public_Order_ASB_Share <- safe_share(
  lsoa_model$Public_order__ASB,
  lsoa_model$Total_Crime
)

lsoa_model$Other_Crime_Share <- safe_share(
  lsoa_model$Other_crime,
  lsoa_model$Total_Crime
)

# ------------------------------------------------------------
# 10. Prepare features for clustering
# ------------------------------------------------------------

cluster_features <- data.frame(
  Total_Crime = lsoa_model$Total_Crime,
  Acquisitive_Share = lsoa_model$Acquisitive_Share,
  High_Harm_Share = lsoa_model$High_Harm_Share,
  Public_Order_ASB_Share = lsoa_model$Public_Order_ASB_Share,
  Other_Crime_Share = lsoa_model$Other_Crime_Share,
  Latitude = lsoa_model$Latitude,
  Longitude = lsoa_model$Longitude
)

cluster_features <- cluster_features[
  complete.cases(cluster_features),
]

lsoa_model <- lsoa_model[
  complete.cases(data.frame(
    Total_Crime = lsoa_model$Total_Crime,
    Acquisitive_Share = lsoa_model$Acquisitive_Share,
    High_Harm_Share = lsoa_model$High_Harm_Share,
    Public_Order_ASB_Share = lsoa_model$Public_Order_ASB_Share,
    Other_Crime_Share = lsoa_model$Other_Crime_Share,
    Latitude = lsoa_model$Latitude,
    Longitude = lsoa_model$Longitude
  )),
]

if (nrow(cluster_features) < 4) {
  stop("Not enough LSOA areas available for 4-cluster k-means.")
}

cluster_features_scaled <- scale(cluster_features)

# ------------------------------------------------------------
# 11. Run k-means clustering
# ------------------------------------------------------------

set.seed(42)

k <- 4

kmeans_result <- kmeans(
  cluster_features_scaled,
  centers = k,
  nstart = 25,
  iter.max = 100
)

lsoa_model$Cluster <- kmeans_result$cluster

# ------------------------------------------------------------
# 12. Create cluster summaries
# ------------------------------------------------------------

cluster_summary <- aggregate(
  cbind(
    Total_Crime,
    Acquisitive_Share,
    High_Harm_Share,
    Public_Order_ASB_Share,
    Other_Crime_Share,
    Latitude,
    Longitude
  ) ~ Cluster,
  data = lsoa_model,
  FUN = mean
)

cluster_counts <- aggregate(
  lsoa_model$LSOA_Name,
  by = list(Cluster = lsoa_model$Cluster),
  FUN = length
)

names(cluster_counts)[2] <- "Number_of_LSOAs"

cluster_summary <- merge(cluster_summary, cluster_counts, by = "Cluster")

# ------------------------------------------------------------
# 13. Assign interpretable cluster labels
# ------------------------------------------------------------

cluster_summary$Cluster_Label <- paste("Cluster", cluster_summary$Cluster)

highest_volume_cluster <- cluster_summary$Cluster[
  which.max(cluster_summary$Total_Crime)
]

highest_acquisitive_cluster <- cluster_summary$Cluster[
  which.max(cluster_summary$Acquisitive_Share)
]

highest_high_harm_cluster <- cluster_summary$Cluster[
  which.max(cluster_summary$High_Harm_Share)
]

lowest_volume_cluster <- cluster_summary$Cluster[
  which.min(cluster_summary$Total_Crime)
]

cluster_summary$Cluster_Label[cluster_summary$Cluster == highest_volume_cluster] <-
  "Central high-volume hotspots"

cluster_summary$Cluster_Label[cluster_summary$Cluster == highest_acquisitive_cluster] <-
  "Acquisitive-dominant areas"

cluster_summary$Cluster_Label[cluster_summary$Cluster == highest_high_harm_cluster] <-
  "High-harm profile areas"

cluster_summary$Cluster_Label[cluster_summary$Cluster == lowest_volume_cluster] <-
  "Lower-volume outer areas"

# Avoid duplicate labels if the same cluster meets multiple conditions
duplicate_labels <- duplicated(cluster_summary$Cluster_Label)

if (any(duplicate_labels)) {
  cluster_summary$Cluster_Label <- paste(
    "Cluster",
    cluster_summary$Cluster,
    "- modelled crime profile"
  )
}

lsoa_model <- merge(
  lsoa_model,
  cluster_summary[, c("Cluster", "Cluster_Label")],
  by = "Cluster",
  all.x = TRUE
)

# ------------------------------------------------------------
# 14. Create final output
# ------------------------------------------------------------

output <- data.frame(
  LSOA_Name = lsoa_model$LSOA_Name,
  Total_Crime = round(lsoa_model$Total_Crime, 0),
  Latitude = lsoa_model$Latitude,
  Longitude = lsoa_model$Longitude,
  Acquisitive_Share = round(lsoa_model$Acquisitive_Share, 4),
  High_Harm_Share = round(lsoa_model$High_Harm_Share, 4),
  Public_Order_ASB_Share = round(lsoa_model$Public_Order_ASB_Share, 4),
  Other_Crime_Share = round(lsoa_model$Other_Crime_Share, 4),
  Cluster = lsoa_model$Cluster,
  Cluster_Label = lsoa_model$Cluster_Label,
  stringsAsFactors = FALSE
)

output <- output[order(output$Cluster, -output$Total_Crime), ]

write.csv(
  output,
  "LSOA_Cluster_Output.csv",
  row.names = FALSE
)

write.csv(
  cluster_summary,
  "LSOA_Cluster_Summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 15. Print useful summary for report interpretation
# ------------------------------------------------------------

cat("\nClustering complete.\n")
cat("Output file created: LSOA_Cluster_Output.csv\n")
cat("Cluster summary file created: LSOA_Cluster_Summary.csv\n\n")

cat("Number of LSOAs clustered:", nrow(output), "\n\n")

cat("Cluster summary:\n")
print(cluster_summary)

cat("\nSuggested Tableau setup:\n")
cat("1. Import LSOA_Cluster_Output.csv into Tableau.\n")
cat("2. Create a new worksheet called 'LSOA Crime Profile Clusters'.\n")
cat("3. Put Longitude on Columns and Latitude on Rows.\n")
cat("4. Use Circle marks.\n")
cat("5. Put Cluster_Label on Colour.\n")
cat("6. Put Total_Crime on Size.\n")
cat("7. Put LSOA_Name, Cluster_Label, Total_Crime and share variables on Tooltip.\n")
