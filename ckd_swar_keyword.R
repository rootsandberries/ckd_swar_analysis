#3. Conduct keyword co-occurence and cluster analysis

#Load libraries
library(litsearchr)
library(igraph)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)


#Create a one-column dataframe of combined titles and abstracts
#This uses the dataframe produced at the end of ckd_swar_dboverlap.R
unique_tiab <- unique_all_bib[, c("duplicate_id", "title","abstract")]
unique_tiab$combined <- paste(unique_tiab$title, unique_tiab$abstract, sep = " ")
unique_tiab <- unique_tiab[, c("duplicate_id","combined")]
unique_tiab <- as.data.frame(unique_tiab)

#Use litsearchr to extract terms and create keyword co-occurence network

#extract terms that are bigrams and occur at least three times
#Can play around with these settings
extracted_terms_pass1 <- litsearchr::extract_terms(text=unique_tiab[, "combined"], 
                                                   method="fakerake", 
                                                   min_freq=3, 
                                                   min_n=2)

#Write out extracted_terms for review and mark non-relevant terms
#Save text file of non-relevant terms to remove_bigrams.txt
writeLines(extracted_terms_pass1, "outputs/extracted_terms_pass1.txt")

#Use this additional list of stopwords from Luke Tudge tutorial:
#https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html
clinpsy_stopwords <- read_lines("data/clin_psy_stopwords.txt")

#Create final list of stopwords
all_stopwords <- c(get_stopwords("English"), clinpsy_stopwords)


#Extract final list of terms for network
extracted_terms <- extract_terms(
  text=unique_tiab[, "combined"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=all_stopwords)

#Use manually identified non-relevant terms from above and remove from extracted terms
bigrams_to_remove <- readLines("data/remove_bigrams.txt")
cleaned_terms <- setdiff(extracted_terms, bigrams_to_remove)

#Create object of terms from title/abstract in unique_tiab
docs <- paste(unique_tiab[, "combined"])

#Create matrix of the terms of interest (i.e. extracted_terms) based on docs
dfm <- create_dfm(elements=docs, features=cleaned_terms)

#Create graph object only including terms that appear in at least 3 records
#Can play around with this setting
g <- create_network(dfm, min_studies=3)

#Remove nodes with less than 5 edges
isolated <- which(degree(g)==5)
g2 <- delete.vertices(g, isolated)

#Set layout
layout_fr = layout_with_fr(g2)

#Plot network
plot(g2, edge.color = "lightgray", edge.width = 0.2, vertex.size = 2, vertex.label=NA, layout = layout_fr)

#Identify communities
my_clusters_1 <- cluster_louvain(g2, weights = NULL)

#Print number of communities and size of each
length(my_clusters_1)
sizes(my_clusters_1)

## Identify which communities have fewer than 20 nodes (play around with this parameter)
small <- which(table(my_clusters_1$membership) < 20)

## Keep only nodes in communities with greater than 20 members (play around with this)
keep <- V(g2)[!(my_clusters_1$membership %in% small)]

#Make graph object removing small communities based on g2 clusters
g3  <- induced_subgraph(g2, keep)


#Redo communities and layout
my_clusters_2 <- cluster_louvain(g3)
layout_fr_2 <- layout_fr[keep,]
length(my_clusters_2)
sizes(my_clusters_2)

#Set colors for nodes
color_palette <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#A6CEE3", "#E78AC3")
i <- V(g3)$name %in% names(membership(my_clusters_2))
V(g3)$color[i] <- color_palette[membership(my_clusters_2)]

#Plot network
pdf("plots/network_graph.pdf", width = 8, height = 6)  # Adjust width, height, and res as needed
plot(g3, 
     vertex.label = NA, 
     vertex.size=3, 
     edge.color = "lightgray", 
     edge.width = 0.2,
     layout=layout_fr_2,
     edge.curved = TRUE,
     mark.groups=NULL)
dev.off()

#Assign cluster groups to nodes
#cluster_grps <- membership(my_clusters_2)
cluster_grps_names <- cbind(V(g3)$name, my_clusters_2$membership)

#Count the number of times extracted terms occur in the whole dataset
phrase_counts <- sapply(extracted_terms, function(phrase) sum(str_count(docs, phrase)))
result_df <- data.frame(Phrase = names(phrase_counts), Count = as.vector(phrase_counts))

#Merge to cluster dataframe
word_freq_cluster <- merge(cluster_grps_names, result_df, by.x = "V1", by.y = "Phrase", all.x = TRUE)
word_freq_cluster_sub <- subset(word_freq_cluster, Count != 0)
write.csv(word_freq_cluster_sub, "outputs/word_freq_cluster.csv")

#Word cloud
png(filename="plots/wc1.png")
wc1 <- word_freq_cluster_sub %>% 
  filter(V2 == 1) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()

png(filename="plots/wc2.png")
wc2 <- word_freq_cluster_sub %>% 
  filter(V2 == 2) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()

png(filename="plots/wc3.png")
wc3 <- word_freq_cluster_sub %>% 
  filter(V2 == 3) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()

png(filename="plots/wc4.png")
wc4 <- word_freq_cluster_sub %>% 
  filter(V2 == 4) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()

png(filename="plots/wc5.png")
wc5 <- word_freq_cluster_sub %>% 
  filter(V2 == 5) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()

png(filename="plots/wc6.png")
wc6 <- word_freq_cluster_sub %>% 
  filter(V2 == 6) %>% 
  with(wordcloud(V1, Count, min.freq = 1,
                 #max.words=100, 
                 random.order=FALSE,
                 rot.per=0,
                 colors = brewer.pal(8, "Dark2")))
dev.off()


