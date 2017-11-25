library(bigrquery)
library(dplyr)

project <- "tensile-axiom-167413"

sql <- "SELECT DISTINCT Gene_name FROM `tensile-axiom-167413.cosmic.cosmic`"

genes <- query_exec(sql, project = project, use_legacy_sql = FALSE) %>%
  dplyr::select(Gene_name) %>%
  dplyr::rename(word = Gene_name) %>%
  dplyr::mutate(word = tolower(word),
                gene = TRUE) %>%
  dplyr::distinct() %>%
  dplyr::arrange(word)


sql <- "SELECT DISTINCT Gene_name, Mutation_CDS, Mutation_AA FROM `tensile-axiom-167413.cosmic.cosmic`"

variants <- query_exec(sql, project = project, use_legacy_sql = FALSE,
                       max_pages = Inf)

for(i in 1:ncol(variants)) {
  variants[[i]] <- tolower(variants[[i]])
}

amino_acids <- read.table(header = TRUE, sep = ",", text = '
amino_Acid,      abbr,    symbol
Alanine,         Ala,     A
Cysteine,        Cys,     C
Aspartic Acid,   Asp,     D
Glutamic Acid,   Glu,     E
Phenylalanine,   Phe,     F
Glycine,         Gly,     G
Histidine,       His,     H
Isoleucine,      Ile,     I
Lysine,          Lys,     K
Leucine,         Leu,     L
Methionine,      Met,     M
Asparagine,      Asn,     N
Proline,         Pro,     P       
Glutamine,       Gln,     Q
Arginine,        Arg,     R
Serine,          Ser,     S
Threonine,       Thr,     T
Valine,          Val,     V
Tryptophan,      Trp,     W
Tyrosine,        Tyr,     Y                          
                          ', stringsAsFactors = FALSE, strip.white = TRUE)
for(i in names(amino_acids)) {
  amino_acids[[i]] <- tolower(amino_acids[[i]])
}

devtools::use_data(genes, variants, amino_acids, overwrite = TRUE)
