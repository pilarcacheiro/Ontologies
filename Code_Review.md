### Description

This repository contains two scripts HPO_obo_to_txt.R and MP_obo_to_txt.R
to import phenotype ontologies from obo format 
(https://owlcollab.github.io/oboformat/doc/GO.format.obo-1_4.html)
to data frames in order to make it easy to work with these annotations in R.
The 2 scripts are nearly identical, one uses the Human Phenotype Ontology (HPO)
and the other one the Mammalian Phenotype Ontology (MP). The scripts import
the ontologies directly from different online repositories.

### What kind of feedback would I like?

* I use these scripts every other day, and I realise that I wouldn't really need
to run them as far as I got the output files, I just do in case there is an
update in the original ontology release, so I'm not sure if there is a way to
check that the obo file that I'm importing is different from a previous one that
I used to generate previous versions of the output files. Does this make sense?
* Each script consists of 3 functions, and I guess they could be simplify to 
just one? It would also be possible to have the different output files merged
into a single one since they are all related. I would like to know what are 
your thoughts on that.
* Since the 2 scripts are almost identical, I know I should wrap them into just
one single script / function, but I still struggle to do this.
* Is there a way to create an empty directory (empty folder data)?