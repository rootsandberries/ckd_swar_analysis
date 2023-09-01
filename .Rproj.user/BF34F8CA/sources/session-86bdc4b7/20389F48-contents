#2. Conduct database and source analysis

#Script to assess database overlap using CiteSource package
#This script continues from ckd_swar_dataclean.R using unique_all_dedup dataframe

#Load libraries
library(CiteSource)
library(kableExtra)
library(webshot)

#Count number of unique and non-unique citations from different sources and labels 
n_unique <- count_unique(unique_all_dedup)

#For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_all_dedup, comp_type = "sources")

#Initial upload/post internal deduplication table creation
initial_counts <- record_counts(unique_all_dedup, citations, "cite_source")
record_counts_table(initial_counts)

#Source overlap----
#Heat maps
pdf("plots/heatmap.pdf", width = 8, height = 6)  # Adjust width, height, and res as needed
plot_source_overlap_heatmap(source_comparison)
dev.off()

#For heatmap showing percentages instead 
pdf("plots/heatmap_percent.pdf", width = 8, height = 6)  # Adjust width, height, and res as needed
plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")
dev.off()

#For an upset plot to show database overlap
pdf("plots/upset.pdf", width = 8, height = 6)  # Adjust width, height, and res as needed
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
dev.off()


#Source content analysis----
#Top journals----
#Get unique records from each source and add bibliographic data
unique_ieee <- n_unique %>% 
  filter(cite_source=="IEEE", unique == TRUE) %>%
  inner_join(unique_all_dedup, by = "duplicate_id")

unique_cinahl <- n_unique %>% 
  filter(cite_source=="CINAHL", unique == TRUE) %>%
  inner_join(unique_all_dedup, by = "duplicate_id")

unique_acm <- n_unique %>% 
  filter(cite_source=="ACM", unique == TRUE) %>%
  inner_join(unique_all_dedup, by = "duplicate_id")

unique_medline <- n_unique %>% 
  filter(cite_source=="Medline", unique == TRUE) %>%
  inner_join(unique_all_dedup, by = "duplicate_id")

unique_scopus <- n_unique %>% 
  filter(cite_source=="ScopusTIAB", unique == TRUE) %>%
  inner_join(unique_all_dedup, by = "duplicate_id")

#Analyze journal titles for unique records
ieee_journals <- unique_ieee %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

cinahl_journals <- unique_cinahl %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

#Fix blank journal cells in ACM data
unique_acm$journal <- ifelse(unique_acm$journal == "", unique_acm$title, unique_acm$journal)

acm_journals <- unique_acm %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

medline_journals <- unique_medline %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

scopus_journals <- unique_scopus %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

#Use the knitr:kable function to print a nice looking table of the top 10 journals
options(kableExtra.auto_size = FALSE)  # Turn off automatic table width adjustment

kable(ieee_journals[1:5, ], table.attr = "style='width:30%;'") %>%  
  kable_styling(bootstrap_options = "striped") %>%
  add_header_above(c("IEEE Xplore" = 2)) %>% 
  save_kable("tables/ieee_table.png", zoom = 3)

kable(cinahl_journals[1:5, ], table.attr = "style='width:30%;'") %>%  
  kable_styling(bootstrap_options = "striped") %>%
  add_header_above(c("CINAHL" = 2)) %>% 
  save_kable("tables/cinahl_table.png", zoom = 3)

kable(acm_journals[1:5, ], table.attr = "style='width:30%;'") %>%  
  kable_styling(bootstrap_options = "striped") %>%
  add_header_above(c("ACM Digital Library" = 2)) %>% 
  save_kable("tables/acm_table.png", zoom = 3)

kable(medline_journals[1:5, ], table.attr = "style='width:30%;'") %>%  
  kable_styling(bootstrap_options = "striped") %>%
  add_header_above(c("Medline" = 2)) %>% 
  save_kable("tables/medline_table.png", zoom = 3)

kable(scopus_journals[1:5, ], table.attr = "style='width:30%;'") %>%  
  kable_styling(bootstrap_options = "striped") %>%
  add_header_above(c("Scopus" = 2)) %>% 
  save_kable("tables/scopus_table.png", zoom = 3)

#Publication date----

#Combine all unique record dataframes into a single dataframe. Note that we'll leave Criminal Justice Abstracts out since there is only one unique record.
all_unique <- bind_rows(unique_ieee, unique_cinahl, unique_acm, unique_medline, unique_scopus)

#Group by year and source, count and produced a faceted line graph
pdf("plots/pubyear.pdf", width = 8, height = 6)  # Adjust width, height, and res as needed
all_unique %>% group_by(cite_source.x, year) %>% 
  summarise(count = n()) %>%  
  ggplot(aes(year, count, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = seq(min(all_unique$year), max(all_unique$year), by = 5)) +
  facet_wrap(~ cite_source.x) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Publication year") + ylab("Unique records")
dev.off()

#Join into a single dataframe
#This will be used in the keyword co-occurence analysis
unique_all_bib <- rbind(unique_ieee, unique_cinahl, unique_acm, unique_medline, unique_scopus)


