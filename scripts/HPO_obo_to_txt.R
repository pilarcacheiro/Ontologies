############################################################################################
#############################################################################################

## Project: Ontologies
## Script: HPO_obo_to_txt.R
## Purpose: import Human Phenotype Ontology in obo format and convert to plain text files
## Author: Pilar Cacheiro15/02/2019 
## Date: 15/02/2019
## HPO notes: format-version: 1.2; data-version: releases/2019-02-12

#############################################################################################
#############################################################################################

library(ontologyIndex);library(tidyr);library(dplyr);library(Hmisc)

##############################################################################################


hpo <- get_ontology("http://purl.obolibrary.org/obo/hp.obo")

## hpo ids and descriptions

hpo.description <- data.frame(hpo.term=as.character(row.names(as.data.frame(hpo[[2]]))),
                             hpo.description=as.character(as.data.frame(hpo[[2]])[,1]),stringsAsFactors = F)

## parents, children and all the ancestros for a given hpoO term as a list

parents <- hpo[[3]]
children <-  hpo[[4]]
ancestors <- hpo[[5]]


## set of functions to convert these lists to data frame format:


#### get parental nodes ########################################################################

get.parent.nodes <- function(parents){
  
  hpo.term <- list()
  hpo.parents <-list()
  
  for(i in 1:length(names(parents))){
    
    hpo.term [[i]] <- names(parents)[i]
    hpo.parents[[i]] <- paste(parents[[i]],collapse=",")
  }
  
  hpo.parents <- data.frame(hpo.term = do.call(rbind,hpo.term),hpo.parents = do.call(rbind,hpo.parents),stringsAsFactors=F) %>%
    mutate(hpo.parents = strsplit(as.character(hpo.parents),",")) %>%
    unnest(hpo.parents) %>%
    filter(hpo.term != hpo.parents)
  
  return(hpo.parents)
  
}


#### get children nodes ##############################################################################

get.children.nodes <- function(children){
  
  hpo.term <- list()
  hpo.children <- list()
  
  for(i in 1:length(names(children))){
    
    hpo.term [[i]] <- names(children)[i]
    hpo.children[[i]] <- paste(children[[i]],collapse=",")
  }
  
  hpo.children <-  data.frame(hpo.term = do.call(rbind,hpo.term),hpo.children= do.call(rbind,hpo.children),stringsAsFactors=F) %>%
    mutate(hpo.children = strsplit(as.character(hpo.children),",")) %>%
    unnest(hpo.children) %>%
    filter(hpo.term != hpo.children)
  
  return(hpo.children)
}


#### get all the ancestors/ top levels ##################################################################

get.ancestor.nodes <- function(ancestors){
  
  hpo.term <- list()
  hpo.ancestors <- list()
  
  for(i in 1:length(names(ancestors))){
    
    hpo.term [[i]] <- names(ancestors)[i]
    hpo.ancestors[[i]] <- paste(ancestors[[i]],collapse=",")
  }
  
  hpo.ancestors <- data.frame(hpo.term = do.call(rbind,hpo.term),hpo.ancestors = do.call(rbind,hpo.ancestors),stringsAsFactors=F) %>%
    mutate(hpo.ancestors = strsplit(as.character(hpo.ancestors),",")) %>%
    unnest(hpo.ancestors) %>%
    filter(hpo.term != hpo.ancestors )
  
  return(hpo.ancestors)
}

### get top levels #########################################################################################

hpo.toplevels <- get.parent.nodes(parents) %>%
  filter(hpo.parents=="HP:0000001") %>%
  left_join(hpo.description,by="hpo.term") %>%
  select(hpo.term,hpo.description)

hpo.toplevels.phenotypic.abnormalities.only <- get.parent.nodes(parents) %>%
  filter(hpo.parents=="HP:0000118") %>%
  left_join(hpo.description,by="hpo.term") %>%
  select(hpo.term,hpo.description)

##############################################################################################################
##############################################################################################################
##############################################################################################################


hpo.parental.nodes <- get.parent.nodes(parents)

hpo.children.nodes <- get.children.nodes(children)

hpo.ancestor.nodes <- get.ancestor.nodes(ancestors)


#############################################################################################################

## export files

hpo.dir <- "../hpo/"

files.to.export <- list(hpo.description,hpo.parental.nodes,hpo.children.nodes,hpo.ancestor.nodes,
                       hpo.toplevels,hpo.toplevels.phenotypic.abnormalities.only)

names(files.to.export) <- Cs(hpo.description,hpo.parental.nodes,hpo.children.nodes,hpo.ancestor.nodes,
                            hpo.toplevels,hpo.toplevels.phenotypic.abnormalities.only)


for (i in 1:length(files.to.export)){
  
  write.table(files.to.export[[i]],paste0(hpo.dir,names(files.to.export)[i],".txt"), quote = F,sep="\t",row.names = FALSE)
  
}

#############################################################################################################
#############################################################################################################

