# Microbiota community typing

## Overview

This repository contains workflow examples for Non-negative Matrix Factorization (NMF), 
Dirichlet Multinomial Mixtures (DMM) and Latent Dirichlet Allocation (LDA)
that are unsupervised learning techniques for microbiota community typing. These workflows are linked to a 
master's thesis project aiming to compare these three techniques, with a naive clustering derived from 
a dominant taxon of a sample. We employ an all-cause mortality association strength as a quantitative metrics to 
distinguish biologically relevant structure in the data. The objective of the work is to comprehensively study the 
techniques for low-resolution microbiota community typing enabling the definition of key characteristics 
of common microbiota assemblages in a Finnish population. In addition, the scripts for survival analyses using 
Cox regression are provided, as well as for various visualization purposes. The data references are removed.  

## Contents

```
├── create_tse.R
├── cross_validation
│   ├── cross_validation.R
│   ├── lda_test_similarity.R
│   ├── plot_predicted_clusters.R
│   ├── plot_predicted_drivers.R
│   ├── predict.R
│   └── survival_prediction.R
├── dmm
│   ├── calculate_dmm.R
│   ├── plot_dmm_clusters.R
│   ├── plot_drivers.R
│   └── survival_dmm.R
├── dominant
│   ├── create_dominant.R
│   ├── plot_dominant.R
│   └── survival_dominant.R
├── lda
│   ├── calculate_lda.R
│   ├── plot_lda_clusters.R
│   ├── plot_topics.R
│   └── survival_lda.R
├── nmf
│   ├── calculate_nmf.R
│   ├── plot_components.R
│   ├── plot_nmf_clusters.R
│   └── survival_nmf.R
└── pcoa.R
```
