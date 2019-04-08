library(stringr) # text cleaning and regular expressions
library(tidytext) # provides additional text mining functions
library(widyr) # widen, process, tidy data
library(dplyr) # data manipulation
library(tidyr) # data tidying
library(tibble) # data type
library(igraph) # graph library
library(tidygraph) # tidy framework for igraph
library(ggraph) # graph librar for igraph
library(ggpubr) # used for matrix graph layout

#################################################
############### Read in Book ####################
#################################################

# Read the text files
book_title = "Alice's Adventures in Wonderland"
chapters = 1:12
chapter_stem = "alice"
ext <-".txt"
folder<- "alice/"

book <- tibble()

# read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)
  
  chapter_text <- raw %>%
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
    unnest_tokens(word, text) %>% # tokenise the text
    filter(!word %in% stop_words$word) # remove stop words
  
  book <- rbind(book, words) # add rows to the book tibble
}







#################################################
############### Word Correlation ################
#################################################
word_cor <- book %>%
  group_by(word) %>%
  filter(n() >= 8) %>% # minimum number of word pairs to consider; determines the number of nodes; lower value -> more nodes
  pairwise_cor(item=word, feature=chapter) %>% 
  filter(!is.na(correlation), correlation >= 0.6) # minimum correlation; determines the number of edges


## correltion to high produces lots of indicidual, disconnect graphs and lots of clusters
# not with much meaning. too low filter fo rn produces clisters with large amount of nodes with little meaning






#################################################
############### Graph and Measurements ##########
#################################################
correlation_graph <- as_tbl_graph(word_cor, directed = F) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) # (hello,alice) same as (alice,hello)

original_graph <- correlation_graph %>% 
  activate(nodes) %>%
  mutate(centrality = centrality_degree(),
         diameter = graph_diameter(),
         radius = graph_radius(),
         eccentricity = node_eccentricity())

original_graph_measurements <- as_tibble(original_graph)

ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph",
       subtitle = paste("Diameter = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))


ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = eccentricity), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph",
       subtitle = paste("Diameter = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))










#################################################
############### Community Detection #############
#################################################

###################
##### Functions ###
###################
## seperates a community detected igraph into a list 
# of separate graphs based on community
get_community_graphs <- function(community_graph) {
  community_graph <- community_graph %>% 
    activate(nodes) # activate node table
  
  graph_tibble <- community_graph %>%
    as_tibble()
  max_community <- max(graph_tibble$community) # get max number of communities
  
  community_list = list()
  current_community <- 1
  while(current_community <= max_community) {
    # filter original network by current community and store partitioned network in a list
    community_list[[current_community]] <- community_graph %>% filter(community == current_community)
    current_community <- current_community + 1
  }
  community_list
}

# generates a ggraph for a network cluster in tidy_graph format
get_community_ggraph <- function(tidy_graph) {
  tidy_graph <- tidy_graph %>% 
    activate(nodes) %>%
    mutate(centrality = centrality_degree(),
           diameter = graph_diameter(),
           radius = graph_radius(),
           eccentricity = node_eccentricity())
  
  tidy_graph_measurements <- tidy_graph %>% as_tibble()
  
  community_ggplot <- ggraph(tidy_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
    geom_node_point(alpha = 0.25, size = 2) +
    geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "red", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
    theme_void() +
    scale_color_gradient(low = 'green', high = 'red') +
    labs(title = paste("Community = ", sample(tidy_graph_measurements$community, 1)),
         subtitle = paste("Diamater = ", sample(tidy_graph_measurements$diameter, 1),
                          " | Radius = ", sample(tidy_graph_measurements$radius, 1),
                          "| Nodes = ", length(tidy_graph_measurements$radius))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = 'bold'))
}

###################
### Fast-Greedy ###
###################
community.fg <- correlation_graph %>%
  activate(nodes) %>%
  mutate(community = group_fast_greedy())

community.fg_measurements <- community.fg %>%
  as_tibble()

# plot network colored by cluster
ggraph(community.fg, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(aes(color = factor(community)), alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = factor(community)), size = 4, repel = TRUE, segment.color = "red", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  labs(title = "Fast Greedy Community Detection",
       subtitle = paste("Communities = ", max(community.fg_measurements$community)),
       color = "Community") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

# remove small clusters
community.fg <- community.fg %>%
  group_by(community) %>%
  filter(n() > 5) %>%
  ungroup() 

# separate clustered graph into separate graphs for each cluster
community_graphs <- get_community_graphs(community.fg) # return tbl_graph for each community

# get ggplot for each cluster
community_ggplots <- lapply(community_graphs, get_community_ggraph)
ggarrange(plotlist = community_ggplots, ncol = 3, nrow = 3)




#######################
### Louvain ###########
#######################
community.lv <- correlation_graph %>%
  activate(nodes) %>%
  mutate(community = group_louvain())

community.lv_measurements <- community.lv %>%
  as_tibble()

# plot network colored by cluster
ggraph(community.lv, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(aes(color = factor(community)), alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = factor(community)), size = 4, repel = TRUE, segment.color = "red", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  labs(title = "Louvain Community Detection",
       subtitle = paste("Communities = ", max(community.lv_measurements$community)),
       color = "Community") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

# remove small clusters
community.lv <- community.lv %>%
  group_by(community) %>%
  filter(n() > 5) %>%
  ungroup() 

# separate clustered graph into separate graphs for each cluster
community_graphs <- get_community_graphs(community.lv) # return tbl_graph for each community

# get ggplot for each cluster
community_ggplots <- lapply(community_graphs, get_community_ggraph)
ggarrange(plotlist = community_ggplots, ncol = 3, nrow = 4)










###################################
############### Bigram ############
###################################
book <- tibble()

# read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)
  
  chapter_text <- raw %>%
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # tokenise the text
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% # remove stop words
    unite("bigram", c("word1", "word2"), sep = " ")
  
  book <- rbind(book, words) # add rows to the book tibble
}


#################################################
############### Bigram Correlation ##############
#################################################
bigram_cor <- book %>%
  group_by(bigram) %>%
  filter(n() >= 3) %>% # minimum number of word pairs to consider; determines the number of nodes; lower value -> more nodes
  pairwise_cor(item=bigram, feature=chapter) %>% 
  filter(!is.na(correlation), correlation >= 0.6) # minimum correlation; determines the number of edges


#################################################
############### Graph and Measurements ##########
#################################################
correlation_graph <- as_tbl_graph(bigram_cor, directed = F) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) # (hello,alice) same as (alice,hello)

original_graph <- correlation_graph %>% 
  activate(nodes) %>%
  mutate(centrality = centrality_degree(),
         diameter = graph_diameter(),
         radius = graph_radius(),
         eccentricity = node_eccentricity())

original_graph_measurements <- as_tibble(original_graph)

ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph - Bigrams",
       subtitle = paste("Diamater = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))











#################################################
############### Trigram Approach ################
#################################################
book <- tibble()

# read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)
  
  chapter_text <- raw %>%
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% # tokenise the text
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>% # remove stop words
    unite("trigram", c("word1", "word2", "word3"), sep = " ")
  
  book <- rbind(book, words) # add rows to the book tibble
}


#################################################
############### Trigram Correlation #############
#################################################
trigram_cor <- book %>%
  group_by(trigram) %>%
  filter(n() >= 2) %>% # minimum number of word pairs to consider; determines the number of nodes; lower value -> more nodes
  pairwise_cor(item=trigram, feature=chapter) %>% 
  filter(!is.na(correlation), correlation >= 0.6) # minimum correlation; determines the number of edges


#################################################
############### Graph and Measurements ##########
#################################################
correlation_graph <- as_tbl_graph(trigram_cor, directed = F) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) # (hello,alice) same as (alice,hello)

original_graph <- correlation_graph %>% 
  activate(nodes) %>%
  mutate(centrality = centrality_degree(),
         diameter = graph_diameter(),
         radius = graph_radius(),
         eccentricity = node_eccentricity())

original_graph_measurements <- as_tibble(original_graph)

ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph - Trigrams",
       subtitle = paste("Diamater = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))











#################################################
############### Read in Book ####################
#################################################

# Read the text files
book_title = "Alice's Adventures in Wonderland"
chapters = 1:6
chapter_stem = "alice"
ext <-".txt"
folder<- "alice/"

book <- tibble()

# read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)
  
  chapter_text <- raw %>%
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
    unnest_tokens(word, text) %>% # tokenise the text
    filter(!word %in% stop_words$word) # remove stop words
  
  book <- rbind(book, words) # add rows to the book tibble
  
}


#################################################
############### Word Correlation ######################
#################################################
word_cor <- book %>%
  group_by(word) %>%
  filter(n() >= 7) %>% # minimum number of word pairs to consider; determines the number of nodes; lower value -> more nodes
  pairwise_cor(item=word, feature=chapter) %>% 
  filter(!is.na(correlation), correlation >= 0.6) # minimum correlation; determines the number of edges

#################################################
############### Graph and Measurements ######################
#################################################
correlation_graph <- as_tbl_graph(word_cor, directed = F) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) # (hello,alice) same as (alice,hello)

original_graph <- correlation_graph %>% 
  activate(nodes) %>%
  mutate(centrality = centrality_degree(),
         diameter = graph_diameter(),
         radius = graph_radius(),
         eccentricity = node_eccentricity())

original_graph_measurements <- as_tibble(original_graph)

ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph",
       subtitle = paste("Diamater = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

#######################
### Louvain ###########
#######################
community.lv <- correlation_graph %>%
  activate(nodes) %>%
  mutate(community = group_louvain())

community.lv_measurements <- community.lv %>%
  as_tibble()

# plot network colored by cluster
ggraph(community.lv, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(aes(color = factor(community)), alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = factor(community)), size = 4, repel = TRUE, segment.color = "red", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  labs(title = "Louvain Community Detection",
       subtitle = paste("Communities = ", max(community.lv_measurements$community)),
       color = "Community") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

# separate clustered graph into separate graphs for each cluster
community_graphs <- get_community_graphs(community.lv) # return tbl_graph for each community

# get ggplot for each cluster
community_ggplots <- lapply(community_graphs, get_community_ggraph)
ggarrange(plotlist = community_ggplots, ncol = 3, nrow = 3)

















#################################################
############### Read in Book ######################
#################################################

# Read the text files
book_title = "Alice's Adventures in Wonderland"
chapters = 7:12
chapter_stem = "alice"
ext <-".txt"
folder<- "alice/"

book <- tibble()

# read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)
  
  chapter_text <- raw %>%
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
    unnest_tokens(word, text) %>% # tokenise the text
    filter(!word %in% stop_words$word) # remove stop words
  
  book <- rbind(book, words) # add rows to the book tibble
  
}


#################################################
############### Word Correlation ######################
#################################################
word_cor <- book %>%
  group_by(word) %>%
  filter(n() >= 7) %>% # minimum number of word pairs to consider; determines the number of nodes; lower value -> more nodes
  pairwise_cor(item=word, feature=chapter) %>% 
  filter(!is.na(correlation), correlation >= 0.6) # minimum correlation; determines the number of edges

#################################################
############### Graph and Measurements ######################
#################################################
correlation_graph <- as_tbl_graph(word_cor, directed = F) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) # (hello,alice) same as (alice,hello)

original_graph <- correlation_graph %>% 
  activate(nodes) %>%
  mutate(centrality = centrality_degree(),
         diameter = graph_diameter(),
         radius = graph_radius(),
         eccentricity = node_eccentricity())

original_graph_measurements <- as_tibble(original_graph)

ggraph(original_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = centrality), size = 4, repel = TRUE, segment.color = "white", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  scale_color_gradient(low = 'green', high = 'red') +
  labs(title = "Original Graph",
       subtitle = paste("Diamater = ", sample(original_graph_measurements$diameter, 1),
                        " | Radius = ", sample(original_graph_measurements$radius, 1))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

#######################
### Louvain ###########
#######################
community.lv <- correlation_graph %>%
  activate(nodes) %>%
  mutate(community = group_louvain())

community.lv_measurements <- community.lv %>%
  as_tibble()

# plot network colored by cluster
ggraph(community.lv, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.7), edge_width=0.5, show.legend = FALSE) +
  geom_node_point(aes(color = factor(community)), alpha = 0.25, size = 2) +
  geom_node_label(aes(label = name, color = factor(community)), size = 4, repel = TRUE, segment.color = "red", segment.alpha = 0.7, segment.size = 0.4) + # repel =TRUE stops labels overlapping; but also  attaches a segment line between node and text, which may be visually confusing if it overlaps with graph edges and nodes
  theme_void() +
  labs(title = "Louvain Community Detection",
       subtitle = paste("Communities = ", max(community.lv_measurements$community)),
       color = "Community") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))

# separate clustered graph into separate graphs for each cluster
community_graphs <- get_community_graphs(community.lv) # return tbl_graph for each community

# get ggplot for each cluster
community_ggplots <- lapply(community_graphs, get_community_ggraph)
ggarrange(plotlist = community_ggplots, ncol = 3, nrow = 3)