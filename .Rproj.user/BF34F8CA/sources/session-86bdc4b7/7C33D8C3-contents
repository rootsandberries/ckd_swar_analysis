#1. Clean and prepare data for analysis

#Load libraries
library(CiteSource)
library(tidyverse)

#Set up----
#Import data
citation_files <- list.files(path = file.path("./data/bibfiles"), pattern = "\\.ris", full.names = TRUE)
citation_files

citations <- read_citations(citation_files,
                            cite_sources = c("CINAHL", "ACM", "IEEE", "Medline", "ScopusTIAB"),
                            cite_labels = c("search", "search", "search", "search", "search"),
                            tag_naming = "best_guess")

#Merge duplicates while creating a dataframe of potential duplicates for manual review
unique_citations <- dedup_citations(citations, manual = TRUE)

#Save the two dataframes separately
unique_df <- unique_citations$unique
manual_df <- unique_citations$manual

#Split record ID values into list in main dataframe
unique_df$SplitValues <- strsplit(as.character(unique_df$record_ids), ",\\s*")

#Look for matches in record ID columns in manual deduplication dataframe and indicate in new column if found
unique_df$MatchFound <- sapply(unique_df$SplitValues, function(x) any(x %in% manual_df$record_id1))
unique_df$MatchFound2 <- sapply(unique_df$SplitValues, function(x) any(x %in% manual_df$record_id2))

#Merge matching column into one
unique_df$MergedColumn <- unique_df$MatchFound | unique_df$MatchFound2

#Remove extra matching columns
unique_df <- select(unique_df, -MatchFound, -MatchFound2, -SplitValues)

#Subset only rows with match found and write to csv for manual duplicate ID
subset_df <- unique_df[unique_df$MergedColumn == TRUE, ] 
subset_df <- subset_df[order(subset_df$title, decreasing = TRUE), ]
write.csv(subset_df, "outputs/subset.csv", row.names = FALSE)

#Manually identify matches by marking matching numbers in MergedColumn column and NAs for non-matches

#Reimport subset dataframe with manual duplicates ID'd
subset_clean <- read.csv("data/subset_clean.csv")

#Set aside NAs from dup column 
na_rows_df <- subset_clean %>% filter(is.na(MergedColumn))

#Merge rows that match on dup column, keeping all values for 
#cite_source, cite_label, source and label)
grouped_df <- subset_clean %>%
  filter(!is.na(MergedColumn)) %>%
  group_by(MergedColumn) %>%
  summarize(across(.cols = -c(record_ids,cite_source, cite_label, source, label),
                   .fns = ~ if (all(is.na(.))) NA else first(na.omit(.)),
                   .names = "{.col}"),
            record_ids = paste(record_ids, collapse = ", "),  
            cite_source = paste(cite_source, collapse = ", "),
            cite_label = paste(cite_label, collapse = ", "),
            source = paste(source, collapse = ", "),
            label = paste(label, collapse = ", "),
            .groups = "drop") 

#Add back the NA values and remove dup column
combined_df <- bind_rows(grouped_df, na_rows_df) %>% 
  select(-MergedColumn)

#Remove Merged column from unique_df
unique_df <- select(unique_df, -MergedColumn)

#Combine new manually merged records to rest of records
unique_all_dedup <- rbind(unique_df, combined_df)

#Use unique_all_dedup with ckd_swar_dboverlap.R