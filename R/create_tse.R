# Import the GG2 metagenome data into a TreeSE object
# containing phenotype data combined with
# microbiome data

tse <- import_greengenes2_MGS(
    pheno_FR07=pheno_FR07,
    pheno_FR02=pheno_FR02,
    endpoints=endpoints,
    feat_table=feat_table,
    taxa_table=taxa_table,
    phylo_tree=phylo_tree,
    read_counts=read_counts
)

saveRDS(tse, "tse.RData")
