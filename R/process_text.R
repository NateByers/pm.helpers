# library(dplyr)
# library(bigrquery)
# project <- "tensile-axiom-167413"
# sql <- "SELECT *
# FROM [tensile-axiom-167413:cosmic.training_variants] variants
# INNER JOIN [tensile-axiom-167413:cosmic.training_text] text
# ON variants.ID = text.ID
# WHERE variants.ID = 946"
# q <- query_exec(sql, project = project, useLegacySql = FALSE)


#' #' @import dplyr tidytext
#' process_text <- function(text, gene_variants) {
#'   # text <- q$text_Text
#' 
#'   sentences <- data.frame(txt = tolower(text), stringsAsFactors = FALSE) %>%
#'     tidytext::unnest_tokens(sentence, txt, token = "regex", pattern = "\\. ") %>%
#'     dplyr::filter(grepl("\\S", sentence)) %>%
#'     dplyr::mutate(sentence_number = row_number())
#' 
#'   words <- lapply(sentences$sentence_number, function(i, sentences) {
#'     words <- data_frame(txt = sentences[["sentence"]][i]) %>%
#'       tidytext::unnest_tokens(word, txt, token = "regex", pattern = "\\s+") %>%
#'       dplyr::mutate(word = gsub("\\(|\\)", "", word))
#' 
#'     cbind(sentences[i, ], words)
#' 
#'   }, sentences = sentences)
#' 
#'   words <- Reduce(rbind, words)
#' 
#'   stops <- stop_words %>%
#'     dplyr::mutate(stop_word = TRUE) %>%
#'     dplyr::select(word, stop_word) %>%
#'     dplyr::distinct()
#' 
#'   words <- words %>%
#'     dplyr::left_join(stops, "word") %>%
#'     dplyr::mutate(stop_word = ifelse(is.na(stop_word), FALSE, stop_word))
#' 
#'   genes_ <- genes %>%
#'     dplyr::mutate(gene_name = tolower(gene_name)) %>%
#'     dplyr::semi_join(words, c("gene_name" = "word"))
#' 
#'   cds <- grep("^c\\.", words$word, value = TRUE)
#'   cds <- sub("[^a-z0-9+]$", "", cds)
#'   aa <- grep("^p\\.", words$word, value = TRUE)
#'   aa <- sub("[^a-z0-9+]$", "", aa)
#' 
#'   variants_ <- variants %>%
#'     dplyr::filter(Mutation_CDS %in% cds | Mutation_AA %in% aa)
#' 
#'   words <- words %>%
#'     dplyr::left_join(genes_, c("word" = "gene_name")) %>%
#'     dplyr::mutate(gene = ifelse(is.na(gene), FALSE, gene))
#' 
#'   words
#' }

#' @import dplyr tidytext stringr
process_text <- function(text, gene_variant) {
  # text <- q$text_Text
  
  sentences <- data.frame(txt = tolower(text), stringsAsFactors = FALSE) %>%
    tidytext::unnest_tokens(sentence, txt, token = "regex", pattern = "\\. ") %>%
    dplyr::filter(grepl("\\S", sentence)) %>%
    dplyr::mutate(sentence_number = row_number())
  
  words <- suppressWarnings(lapply(sentences$sentence_number, function(i, sentences) {
    # i <- 1
    words <- data_frame(txt = sentences[["sentence"]][i]) %>%
      tidytext::unnest_tokens(word, txt, token = "regex", pattern = "\\s+") %>%
      dplyr::mutate(word = gsub("\\(|\\)", "", word),
                    word = sub(",", "", word))
    
    cbind(sentences[i, ], words)
    
  }, sentences = sentences))
  
  words <- Reduce(rbind, words)
  
  # add stop words
  stops <- stop_words %>%
    dplyr::mutate(stop_word = TRUE) %>%
    dplyr::select(word, stop_word) %>%
    dplyr::distinct()
  
  words <- words %>%
    dplyr::left_join(stops, "word") %>%
    dplyr::mutate(stop_word = ifelse(is.na(stop_word), FALSE, stop_word))
  
  # add genes
  words <- words %>%
    dplyr::left_join(genes, "word") %>%
    dplyr::mutate(gene = ifelse(gene, word, NA))
  
  # add variants
  words <- add_variants(words)
  
 
}

add_variants <- function(x) {
  # x <- words
  
  x <- x %>%
    dplyr::mutate(variant = ifelse(grepl("p\\.", word), sub("p\\.", "", word), NA))
  
  proteins <- stringr::str_split_fixed(x$variant, "\\d{1,}", 2) %>%
    as.data.frame(stringsAsFactors = FALSE)
  names(proteins) <- c("from", "to")
  
  proteins <- dplyr::left_join(proteins, select(amino_acids, abbr, symbol),
                               c("from" = "abbr")) %>%
    dplyr::mutate(from = ifelse(is.na(symbol), from, symbol)) %>%
    dplyr::select(-symbol)
  proteins <- dplyr::left_join(proteins, select(amino_acids, abbr, symbol),
                               c("to" = "abbr")) %>%
    dplyr::mutate(to = ifelse(is.na(symbol), to, symbol)) %>%
    dplyr::select(-symbol)
  
  proteins$location <- stringr::str_extract(x$variant, "\\d{1,}")
  
  proteins <- proteins %>%
    dplyr::mutate(paste0(from, location, to))
  
  
}

