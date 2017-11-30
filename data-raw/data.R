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


words_alpha <- readLines("https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt")
data("stop_words")
words_alpha <- words_alpha[!words_alpha %in% stop_words$word]
oma_stop_words <- grep("oma", words_alpha, value = TRUE)
oma_stop_words <- grep("fibroma$|blastoma|carcinoma|omatosis|sarcoma|glioma$|lipoma$|cystoma$|myxoma$|cytoma$|myoma$", 
                  oma_stop_words, invert = TRUE, value = TRUE)
# oma_words <- grep("oma$", oma_stop_words, value = TRUE)
tumors <- c("acanthoma", "acestoma", "actinomycoma", "adamantinoma", "adamantoma", 
            "adenoacanthoma", "adenochondroma", "adenocystoma", "adenofibroma",
            "adenolymphoma", "adenoliomyofibroma", "adenolipoma", "adenoma", "adenomyofibroma",
            "adenomyoma", "adenomyxoma", "adipofibroma", "adipoma", "angiochondroma",
            "angiofibroma", "angioglioma", "angiokeratoma", "angiolymphoma", "angiolipoma",
            "angioma", "angiomyoma", "angionoma", "astrocytoma", "blepharoadenoma",
            "blepharoatheroma", "blepharocoloboma", "botryomycoma", "cementoma", "cerebroma",
            "chylangioma", "chloroma", "cholesteatoma", "chondroangioma", "chondroendothelioma",
            "chondrofibroma", "chondrolipoma", "chondroma", "chondromyoma", "chondromyxoma",
            "chondrosteoma", "chorioepithelioma", "chorioma", "chorionepithelioma", 
            "choristoma", "cylindroma", "cystadenoma", "cystoadenoma", "cystoepithelioma",
            "cystofibroma", "cystoma", "cystomyoma", "cystomyxoma", "cytoma", "craniopharyngioma",
            "cryptoglioma", "dacryoma", "deciduoma", "dentinoma", "dermatomyoma", "desmocytoma",
            "desmoma", "ecchymoma", "ecchondroma", "embryoma", "encephaloma", "enchondroma",
            "endosteoma", "endostoma", "endothelioma", "endotheliomyoma", "endotheliomyxoma", 
            "ependymoma", "epicoeloma", "epistoma", "epithelioma", "fibroadenoma", "fibroangioma",
            "fibrochondroma", "fibrocystoma", "fibroenchondroma", "fibroneuroma", "galactoma",
            "ganglioneuroma", "gyroma", "hepatoma", "hypernephroma", "inochondroma", "inoepithelioma",
            "lymphadenoma", "lymphangioendothelioma", "lymphoma", "lipochondroma", "luteoma",
            "mastadenoma", "mastochondroma", "melanoma", "meningioma", "mesothelioma", "myeloma",
            "myoneuroma", "myxadenoma", "myxoneuroma", "nephradenoma", "neurinoma", "odontoma", 
            "omphaloma", "oophoroma", "orchiencephaloma", "oscheoma", "osteocephaloma", "osteoencephaloma",
            "osteosteatoma", "papilloma", "periangioma", "periostoma", "perithelioma", "pinealoma",
            "sarcoadenoma", "scirrhoma", "splenoma", "teratoma", "thymoma", "trichoepithelioma",
            "tuberculoma")

# writeLines(oma_stop_words, con = "data-raw/oma_words.txt")

oma_stop_words <- oma_stop_words[!oma_stop_words %in% tumors]

devtools::use_data(genes, variants, amino_acids, oma_stop_words, overwrite = TRUE)
