# script description ------------------------------------------------------

## Project: Ontologies
## Script: MP_obo_to_txt.R
## Purpose: import Mammalian Phenotype Ontology in obo format and convert it
## to plain text files
## MP notes: format-version: 1.2; data-version: releases/2019-08-09


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

mp <- get_ontology("http://ontologies.berkeleybop.org/mp.obo")

## mp ids and descriptions

mp_description <- data.frame(
  mp_term = as.character(row.names(as.data.frame(mp[[2]]))),
  mp_description = as.character(as.data.frame(mp[[2]])[, 1]),
  stringsAsFactors = F)

## parents, children and all the ancestors for a given mp term as a list

parents <- mp[[3]]
children <- mp[[4]]
ancestors <- mp[[5]]


# set of functions to convert these lists to data frames ------------------

get_parent_nodes <- function(parents) {
  
  mp_term <- list()
  mp_parents <-list()
  
  for(i in 1:length(names(parents))) {
    
    mp_term [[i]] <- names(parents)[i]
    mp_parents[[i]] <- paste(parents[[i]], collapse = ",")
  }
  
  mp_parents <- data.frame(
    mp_term = do.call(rbind, mp_term),
    mp_parents = do.call(rbind, mp_parents), stringsAsFactors = F) %>%
    mutate(mp_parents = strsplit(as.character(mp_parents),",")) %>%
    unnest(mp_parents) %>%
    filter(mp_term != mp_parents)
  
  return(mp_parents)
  
}


## get children nodes 

get_children_nodes <- function(children){
  
  mp_term <- list()
  mp_children <- list()
  
  for(i in 1:length(names(children))) {
    
    mp_term [[i]] <- names(children)[i]
    mp_children[[i]] <- paste(children[[i]], collapse = ",")
  }

    mp_children <- data.frame(
      mp_term = do.call(rbind, mp_term), 
      mp_children = do.call(rbind, mp_children), stringsAsFactors = F) %>%
      mutate(mp_children = strsplit(as.character(mp_children), ",")) %>%
      unnest(mp_children) %>%
      filter(mp_term != mp_children)
  
  return(mp_children)
}


## get all the ancestors/ top levels 

get_ancestor_nodes <- function(ancestors) {
  
  mp_term <- list()
  mp_ancestors <- list()
  
  for(i in 1:length(names(ancestors))) {
    
    mp_term [[i]] <- names(ancestors)[i]
    mp_ancestors[[i]] <- paste(ancestors[[i]], collapse = ",")
  }
  
    mp_ancestors <- data.frame(
      mp_term = do.call(rbind, mp_term),
      mp_ancestors = do.call(rbind, mp_ancestors), stringsAsFactors = F) %>%
      mutate(mp_ancestors = strsplit(as.character(mp_ancestors), ",")) %>%
      unnest(mp_ancestors) %>%
      filter(mp_term != mp_ancestors)
  
  return(mp_ancestors)
}

## get top levels 

mp_toplevels <- get_parent_nodes(parents) %>%
  filter(mp_parents == "MP:0000001") %>%
  left_join(mp_description, by="mp_term") %>%
  select(mp_term, mp_description)


# apply functions ---------------------------------------------------------


mp_parental_nodes <- get_parent_nodes(parents)

mp_children_nodes <- get_children_nodes(children)

mp_ancestor_nodes <- get_ancestor_nodes(ancestors)



# export files ------------------------------------------------------------

mp_dir <-  "./data/"

files_to_export <- list(mp_description,
                        mp_parental_nodes,
                        mp_children_nodes,
                        mp_ancestor_nodes, 
                        mp_toplevels)

names(files_to_export) <- Cs(mp_description,
                             mp_parental_nodes,
                             mp_children_nodes,
                             mp_ancestor_nodes, 
                             mp_toplevels)


for (i in 1:length(files_to_export)){
  
  write.table(files_to_export[[i]], 
              paste0(mp_dir, names(files_to_export)[i],".txt"), 
              quote = F, sep = "\t", row.names = FALSE)
  
}


