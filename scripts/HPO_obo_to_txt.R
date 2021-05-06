# script description ------------------------------------------------------

## Project: Ontologies
## Script: HPO_obo_to_txt.R
## Purpose: import Human Phenotype Ontology in obo format and convert it to
## to plain text files
## HPO notes: format-version: 1.2; hp/releases/2021-04-13

# install / load libraries ------------------------------------------------

if (!require("ontologyIndex")) install.packages("ontologyIndex")
library("ontologyIndex")

if (!require("dplyr")) install.packages("dplyr")
library("dplyr")

if (!require("tidyr")) install.packages("tidyr")
library("tidyr")

if (!require("Hmisc")) install.packages("Hmisc")
library("Hmisc")

# get ontology ------------------------------------------------------------

hpo <- get_ontology("http://purl.obolibrary.org/obo/hp.obo")

## hpo ids and descriptions

hpo_description <- data.frame(
  hpo_term = as.character(row.names(as.data.frame(hpo[[2]]))),
  hpo_description = as.character(as.data.frame(hpo[[2]])[, 1]), 
  stringsAsFactors = F)

## parents, children and all the ancetors for a given hpo term as a list

parents <- hpo[[3]]
children <-  hpo[[4]]
ancestors <- hpo[[5]]


# set of functions to convert these lists to data frames ------------------


## get parental nodes 

get_parent_nodes <- function(parents) {
  
  hpo_term <- list()
  hpo_parents <-list()
  
  for(i in 1:length(names(parents))){
    
    hpo_term [[i]] <- names(parents)[i]
    hpo_parents[[i]] <- paste(parents[[i]], collapse=",")
  }
  
  hpo_parents <- data.frame(
    hpo_term = do.call(rbind,hpo_term), 
    hpo_parents = do.call(rbind, hpo_parents), stringsAsFactors = F) %>%
    mutate(hpo_parents = strsplit(as.character(hpo_parents), ",")) %>%
    unnest(hpo_parents) %>%
    filter(hpo_term != hpo_parents)
  
  return(hpo_parents)
  
}


## get children nodes 

get_children_nodes <- function(children) {
  
  hpo_term <- list()
  hpo_children <- list()
  
  for(i in 1:length(names(children))){
    
    hpo_term [[i]] <- names(children)[i]
    hpo_children[[i]] <- paste(children[[i]], collapse=",")
  }
  
  hpo_children <-  data.frame(
    hpo_term = do.call(rbind, hpo_term),
    hpo_children = do.call(rbind, hpo_children), stringsAsFactors = F) %>%
    mutate(hpo_children = strsplit(as.character(hpo_children), ",")) %>%
    unnest(hpo_children) %>%
    filter(hpo_term != hpo_children)
  
  return(hpo_children)
}


## get all the ancestors/ top levels

get_ancestor_nodes <- function(ancestors) {
  
  hpo_term <- list()
  hpo_ancestors <- list()
  
  for(i in 1:length(names(ancestors))) {
    
    hpo_term [[i]] <- names(ancestors)[i]
    hpo_ancestors[[i]] <- paste(ancestors[[i]], collapse=",")
  }
  
  hpo_ancestors <- data.frame(
    hpo_term = do.call(rbind, hpo_term),
    hpo_ancestors = do.call(rbind, hpo_ancestors),stringsAsFactors = F) %>%
    mutate(hpo_ancestors = strsplit(as.character(hpo_ancestors), ",")) %>%
    unnest(hpo_ancestors) %>%
    filter(hpo_term != hpo_ancestors )
  
  return(hpo_ancestors)
}

## get top levels of the ontology (physiological systems)

hpo_toplevels <- get_parent_nodes(parents) %>%
  filter(hpo_parents == "HP:0000001") %>%
  left_join(hpo_description,by = "hpo_term") %>%
  select(hpo_term, hpo_description)

hpo_toplevels_phenotypic_abnormalities_only <- get_parent_nodes(parents) %>%
  filter(hpo_parents == "HP:0000118") %>%
  left_join(hpo_description,by = "hpo_term") %>%
  select(hpo_term,hpo_description)


# apply functions ---------------------------------------------------------


hpo_parental_nodes <- get_parent_nodes(parents)

hpo_children_nodes <- get_children_nodes(children)

hpo_ancestor_nodes <- get_ancestor_nodes(ancestors)



# export files ------------------------------------------------------------


hpo_dir <- "./data/"


files_to_export <- list(hpo_description, 
                        hpo_parental_nodes,
                        hpo_children_nodes,
                        hpo_ancestor_nodes,
                        hpo_toplevels,
                        hpo_toplevels_phenotypic_abnormalities_only)

names(files_to_export) <- Cs(hpo_description,
                             hpo_parental_nodes,
                             hpo_children_nodes,
                             hpo_ancestor_nodes,
                             hpo_toplevels,
                             hpo_toplevels_phenotypic_abnormalities_only)


for (i in 1:length(files_to_export)){
  
  write.table(files_to_export[[i]],
              paste0(hpo_dir, names(files_to_export)[i], ".txt"), 
              quote = F, sep = "\t", row.names = FALSE)
  
}


