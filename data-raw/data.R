library(bigrquery)
library(dplyr)

project <- "tensile-axiom-167413"

sql <- "SELECT DISTINCT Gene_name FROM `tensile-axiom-167413.cosmic.cosmic`"

genes <- query_exec(sql, project = project, use_legacy_sql = FALSE) %>%
  dplyr::rename(gene_name = Gene_name)


sql <- "SELECT DISTINCT Gene_name, Mutation_CDS, Mutation_AA FROM `tensile-axiom-167413.cosmic.cosmic`"

variants <- query_exec(sql, project = project, use_legacy_sql = FALSE,
                       max_pages = Inf)

for(i in 1:ncol(variants)) {
  variants[[i]] <- tolower(variants[[i]])
}

devtools::use_data(genes, variants, overwrite = TRUE)
