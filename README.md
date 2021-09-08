# Text mining in German management plans

## Table of contents

* [General info](#1-general-info)
* [Technologies](#2-technologies)
* [Setup](#3-setup)


## 1. General info

Belowground biodiversity has been linked to many ecosystem functions and services related again to human health and wellbeing, but faces threats similar to the aboveground system. Acting to halt biodiversity loss requires transformative change and effective conservation of all ecosystems. Indeed, soil biodiversity has been neglected in several global biodiversity assessments and conservation actions. To better understand how and why soil biodiversity and related ecosystem functions have lesser priority for nature conservation, it is important to get an overview of how society, and particularly policy-makers, have addressed it in the past and present. 

With this analysis, we aimed to review the current status of soil protection by 
* summarizing past and current soil-related Germany and international policies forming the basis for nature conservation, and 
* revealing the role of soil in nature conservation management by text mining in German management plans.

## 2. Technologies

Analysis are done in:
* R version 3.6.3
* RStudio version 1.2.5033
* ArcMap version 10.7.1

Attached R packages:
* callr_3.5.1
* cistem_1.0 
* data.table_1.13.6 
* doParallel_1.0.16
* dplyr_1.0.2 
* EnvStats_2.4.0 
* forcats_0.5.0 
* foreach_1.5.1 
* ggpubr_0.4.0
* igraph_1.2.6 
* influential_2.0.1 
* iterators_1.0.13
* lubridate_1.7.9.2 
* ggplot2_3.3.3 
* ggraph_2.0.4
* naniar_0.6.0             
* NLP_0.2-1         
* pdftools_2.3.1       
* psych_2.0.12           
* purrr_0.3.4 
* RColorBrewer_1.1-2
* readr_1.4.0 
* reshape2_1.4.4 
* sp_1.4-5
* staplr_3.1.1   
* stringr_1.4.0 
* tabulizer_0.2.2 
* tibble_3.0.4 
* tidyr_1.1.2 
* tidytext_0.2.6
* tidyverse_1.3.0   
* tm_0.7-8  
* wordcloud_2.6

## 3. Setup

If R and ArcGIS are already installed, you only have to download this repository.

### 3.1 Installation guide

To run the text analysis, you have to start with the script `0_first_settings.R`.
After installing and loading all packages necessary, you can continue with the text mining process (see [Instructions](#33-instructions-for-use)).

Typical install time on a "normal" desktop computer: ~10min


### 3.2 Demo

To see if the installation works, it is recommended to run the analysis on a subset of documents (i.e., management plans) first. This can be either done by choosing only one of the 16 Federal States (i.e., one folder), or by taking only 10-15 documents per state. Alternatively, you can go year-wise and select only one year between 2001-2020.

* Select only one Federal State: change `s=folders` into `s=folders[1]`
* Take only 10 documents per Federal State: replace `length(files)` by `10`
* Select only one year: set `temp.year` or `yr` to `2012`

To fix error messages among others, you can try to run the code within loops line by line with i (or j or k) defined by hand according to the necessary type of object.

The output should be saved according to the R script. The expected run time for demo on a "normal" desktop computer depends on the subset of data used.

### 3.3 Instructions for use

The text mining process includes the following steps: 

* Extract metadata (i.e., year of publication) from the pdf documents `0_get_metadata.R`
* Merge pdf files using regular expression and R, if necessary: `0_merge_pdf.R`
* Extract and clean the text from pdf files: `1_text_cleaning_tidy.R` (~24h)
* Define keywords for following analysis: `2a_define_keywords.R` 
* Text analysis: extract tokens and create term-document-matrix: `2_create_tdm-tidy.R` (~8h in parallel)
* Text analysis but split into two files and to run in parallel: first, extract tokens with `2_create_tdm_1tokens_parallel.R`, second create term-document-matrix `2_create_tdm_2bigrams_parallel.R`
* Merge output tables per year into one occurrence matrix: `3_merge_cooccurrence.R`
* (do the same to summarize the number of bigrams: `3_merge_bigrams.R`)
* Visualize the number of management plans per Federal State: `4_plot_Germany_noPlans.R`
* Visualize the co- and occurrences in various ways (network, barplot, effect sizes): `4_Plotting.R`
* Visualize the timeline on soil-related policies: `4_plot_timeline.R`

Note: The policy timeline is based on the table `Timeline_EUpolicies.csv`.

#### Reproduction instructions

To reproduce the analysis right from the beginning, you have to first download the German management plan documents. They can be found on the websites of the German Federal States (see `State_doc_summary.csv`). If you follow the same folder structure, you can continue with the process described above.

# References

Associated publication:
[1] Zeiss et al. (2021): ...

Related repository:
...
