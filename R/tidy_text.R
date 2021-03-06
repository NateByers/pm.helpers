# library(dplyr)
# library(bigrquery)
# project <- "tensile-axiom-167413"
# sql <- "SELECT *
# FROM [tensile-axiom-167413:cosmic.training_variants] variants
# INNER JOIN [tensile-axiom-167413:cosmic.training_text] text
# ON variants.ID = text.ID
# WHERE variants.ID = 1844"
# q <- query_exec(sql, project = project, useLegacySql = FALSE)




#' @import dplyr tidytext stringr tm
#' @export
tidy_text <- function(text) {
  # text <- q$text_Text
  sentences <- data.frame(txt = tolower(text), stringsAsFactors = FALSE) %>%
    tidytext::unnest_tokens(sentence, txt, token = "regex", pattern = "\\. ") %>%
    dplyr::filter(grepl("\\S", sentence)) %>%
    dplyr::mutate(sentence_number = row_number())
  
  words <- suppressWarnings(lapply(sentences$sentence_number, function(i, sentences) {
    words <- data_frame(txt = sentences[["sentence"]][i]) %>%
      tidytext::unnest_tokens(word, txt, token = "regex", pattern = "\\s+") %>%
      dplyr::mutate(word = sub(",", "", word))
    
    cbind(sentences[i, ], words)
    
  }, sentences = sentences))
  
  words <- Reduce(rbind, words) %>%
    dplyr::mutate(word_number = row_number()) %>%
    dplyr::group_by(sentence_number) %>%
    dplyr::mutate(sentence_word_number = row_number()) %>%
    dplyr::ungroup()
  
  # add stop words
  stops <- tidytext::stop_words %>%
    dplyr::mutate(stop_word = TRUE) %>%
    dplyr::select(word, stop_word) %>%
    dplyr::distinct()
  
  words <- words %>%
    dplyr::left_join(stops, "word") %>%
    dplyr::mutate(stop_word = ifelse(is.na(stop_word), FALSE, stop_word))
  
  # add disease column
  words <- add_disease(words) %>%
    dplyr::mutate(word = gsub("\\(|\\)", "", word))
  
  # add gene column
  words <- add_gene(words)
  
  # add variant column
  words <- add_variant(words)
  
  words
}

add_gene <- function(x) {
  
  x$word_join <- sapply(x$word, function(y) {
    if(grepl("-", y)) {
      splits <- strsplit(y, "-")[[1]]
      lengths <- sapply(splits, nchar)
      y <- splits[lengths == max(lengths)][1]
    }
    y
  })
  
  x <- x %>%
    dplyr::mutate(word_join = gsub("\\(|\\)", "", word_join)) %>% 
    dplyr::left_join(genes, c("word_join" = "word")) %>%
    dplyr::mutate(gene = ifelse(gene, word, gene),
                  gene = gsub("\\(|\\)", "", gene)) %>%
    dplyr::select(-word_join)
  
  x
}

add_variant <- function(x) {
  # x <- words
  x <- x %>%
    dplyr::mutate(variant = ifelse(grepl("p\\.", word), sub("p\\.", "", word), NA))
  
  possible_variants <- cbind(x$word, stringr::str_split_fixed(x$word, "\\d{1,}", 2),
                             stringsAsFactors = FALSE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(word = V1, prefix = V2, suffix = V3)
  possible_variants$variant_ <- as.vector(apply(possible_variants, 1, function(row) {
    (row[[2]] %in% amino_acids$abbr & row[[3]] %in% amino_acids$abbr) |
      (row[[2]] %in% amino_acids$symbol & row[[3]] %in% amino_acids$symbol)
  }))
  possible_variants <- possible_variants %>%
    dplyr::filter(variant_) %>%
    dplyr::select(word, variant_)
  
  x <- x %>%
    dplyr::left_join(possible_variants, "word") %>%
    dplyr::mutate(variant = ifelse(variant_, word, variant)) %>%
    dplyr::select(-variant_)

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
    dplyr::mutate(variant = paste0(from, location, to),
                  variant = ifelse(is.na(location), NA, variant))
  
  x$variant <- proteins$variant
  
  x
}

add_disease <- function(x) {
  # x <- words
  diseases <- x %>%
    dplyr::filter(grepl("cancer|oma", word),
                  !grepl("-", word)) %>%
    dplyr::mutate(word = gsub("\\(|\\)|\\.|\\d", "", word)) %>%
    dplyr::filter(!word %in% pm.helpers::oma_stop_words) %>%
    dplyr::pull(word) %>%
    unique()
  
  if(length(diseases) == 0){
    x$disease <- FALSE
    return(x)
  }
  abbrevs <- sapply(diseases, function(disease, x) {
    first_occurance <- which(x$word == disease)[1]
    if(grepl("^\\(.+\\)$", x$word[first_occurance + 1])) {
      abbrev <- gsub("\\(|\\)", "", x$word[first_occurance + 1])
    } else {
      abbrev <- NA
    }
    abbrev
  }, x = x)
  
  diseases <- c(diseases, unique(abbrevs[!is.na(abbrevs)]))
  diseases <- data.frame(word_join = diseases, disease = TRUE, stringsAsFactors = FALSE)
  
  x <- x %>%
    dplyr::mutate(word_join = gsub("\\(|\\)|\\.|\\d", "", word)) %>%
    dplyr::left_join(diseases,"word_join") %>%
    dplyr::mutate(disease = ifelse(is.na(disease), FALSE, TRUE)) %>%
    dplyr::select(-word_join)
  
  x
}

