#Set up----
#Import data
scopus_files <- list.files(path = file.path("./scopus_key_data"), pattern = "\\.ris", full.names = TRUE)
scopus_files 

sc_citations <- read_citations(scopus_files,
                            cite_sources = c("ScopusTIAB", "ScopusTIABKEY"),
                            cite_labels = c("search", "search"),
                            tag_naming = "best_guess")

#Merge duplicates
unique_sc_citations <- dedup_citations(sc_citations)

#Count number of unique and non-unique citations from different sources and labels 
n_unique_sc <- count_unique(unique_sc_citations)

#For each unique citation, determine which sources were present
source_comparison_sc <- compare_sources(unique_sc_citations, comp_type = "sources")

#Initial upload/post internal deduplication table creation
initial_counts_sc <- record_counts(unique_sc_citations, sc_citations, "cite_source")
record_counts_table(initial_counts_sc)

my_heatmap_sc <- plot_source_overlap_heatmap(source_comparison_sc)
my_heatmap_sc

unique_sckey <- n_unique_sc %>% 
  filter(cite_source=="ScopusTIABKEY", unique == TRUE)

export_ris(unique_sckey, filename = "scopus_key_unique.ris", source_field = "DB", label_field = "C5")
