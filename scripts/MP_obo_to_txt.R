############################################################################################
############################################################################################

## Project: Ontologies
## Script: MP_obo_to_txt.R
## Purpose: import Mammalian Phenotype Ontology in obo format and convert to plain text files
## Author: Pilar Cacheiro
## Date: 06/03/2019
## MP notes: format-version: 1.2; data-version: releases/2019-03-04

#############################################################################################
#############################################################################################

library(ontologyIndex);library(tidyr);library(dplyr);library(Hmisc)

##############################################################################################


mp <- get_ontology("http://ontologies.berkeleybop.org/mp.obo")

## hpo ids and descriptions

mp.description <- data.frame(mp.term=as.character(row.names(as.data.frame(mp[[2]]))),
                             mp.description=as.character(as.data.frame(mp[[2]])[,1]),stringsAsFactors = F)

## parents, children and all the ancestros for a given hpoO term as a list

parents <- mp[[3]]
children <- mp[[4]]
ancestors <- mp[[5]]


## set of functions to convert these lists to data frame format:


#### get parental nodes ########################################################################

get.parent.nodes <- function(parents){
  
  mp.term <- list()
  mp.parents <-list()
  
  for(i in 1:length(names(parents))){
    
    mp.term [[i]] <- names(parents)[i]
    mp.parents[[i]] <- paste(parents[[i]],collapse=",")
  }
  
  mp.parents <- data.frame(mp.term = do.call(rbind,mp.term),mp.parents = do.call(rbind,mp.parents),stringsAsFactors=F) %>%
    mutate(mp.parents = strsplit(as.character(mp.parents),",")) %>%
    unnest(mp.parents) %>%
    filter(mp.term != mp.parents)
  
  return(mp.parents)
  
}


#### get children nodes ##############################################################################

get.children.nodes <- function(children){
  
  mp.term <- list()
  mp.children <- list()
  
  for(i in 1:length(names(children))){
    
    mp.term [[i]] <- names(children)[i]
    mp.children[[i]] <- paste(children[[i]],collapse=",")
  }
  
  mp.children <-  data.frame(mp.term = do.call(rbind,mp.term),mp.children= do.call(rbind,mp.children),stringsAsFactors=F) %>%
    mutate(mp.children = strsplit(as.character(mp.children),",")) %>%
    unnest(mp.children) %>%
    filter(mp.term != mp.children)
  
  return(mp.children)
}


#### get all the ancestors/ top levels ##################################################################

get.ancestor.nodes <- function(ancestors){
  
  mp.term <- list()
  mp.ancestors <- list()
  
  for(i in 1:length(names(ancestors))){
    
    mp.term [[i]] <- names(ancestors)[i]
    mp.ancestors[[i]] <- paste(ancestors[[i]],collapse=",")
  }
  
  mp.ancestors <- data.frame(mp.term = do.call(rbind,mp.term),mp.ancestors = do.call(rbind,mp.ancestors),stringsAsFactors=F) %>%
    mutate(mp.ancestors = strsplit(as.character(mp.ancestors),",")) %>%
    unnest(mp.ancestors) %>%
    filter(mp.term != mp.ancestors )
  
  return(mp.ancestors)
}

### get top levels #########################################################################################

mp.toplevels <- get.parent.nodes(parents) %>%
  filter(mp.parents=="MP:0000001") %>%
  left_join(mp.description,by="mp.term") %>%
  select(mp.term,mp.description)

##############################################################################################################
##############################################################################################################
##############################################################################################################


mp.parental.nodes <- get.parent.nodes(parents)

mp.children.nodes <- get.children.nodes(children)

mp.ancestor.nodes <- get.ancestor.nodes(ancestors)


#############################################################################################################

## export files

mp.dir <- "../mp/"

files.to.export <- list(mp.description,mp.parental.nodes,mp.children.nodes,mp.ancestor.nodes, mp.toplevels)

names(files.to.export) <- Cs(mp.description,mp.parental.nodes,mp.children.nodes,mp.ancestor.nodes, mp.toplevels)


for (i in 1:length(files.to.export)){
  
  write.table(files.to.export[[i]],paste0(mp.dir,names(files.to.export)[i],".txt"), quote = F,sep="\t",row.names = FALSE)
  
}

#############################################################################################################
#############################################################################################################

