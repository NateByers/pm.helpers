# library(bigrquery)
# project <- "tensile-axiom-167413"
# sql <- "SELECT *
# FROM [tensile-axiom-167413:cosmic.training_variants] train
# INNER JOIN [tensile-axiom-167413:cosmic.training_text] test
# ON test.ID = train.ID
# WHERE test.ID = 946"
# q <- query_exec(sql, project = project, useLegacySql = FALSE)

# genes <- sql <- "SELECT distinct(Gene_name)
# FROM `tensile-axiom-167413.cosmic.cosmic`"
#
# genes <- query_exec(sql, project = project, use_legacy_sql = FALSE,
#                             max_pages = Inf)



#' @import dplyr tidytext
process_text <- function(text, gene_variants) {
  # text <- q$test_Text

  sentences <- data.frame(txt = tolower(q$test_Text), stringsAsFactors = FALSE) %>%
    tidytext::unnest_tokens(sentence, txt, token = "regex", pattern = "\\. ") %>%
    dplyr::filter(grepl("\\S", sentence)) %>%
    dplyr::mutate(sentence_number = row_number())

  words <- lapply(sentences$sentence_number, function(i, sentences) {
    words <- data_frame(txt = sentences[["sentence"]][i]) %>%
      tidytext::unnest_tokens(word, txt, token = "regex", pattern = "\\s+") %>%
      dplyr::mutate(word = gsub("\\(|\\)", "", word))

    cbind(sentences[i, ], words)

  }, sentences = sentences)

  words <- Reduce(rbind, words)

  stops <- stop_words %>%
    dplyr::mutate(stop_word = TRUE) %>%
    dplyr::select(word, stop_word) %>%
    dplyr::distinct()

  words <- words %>%
    dplyr::left_join(stops, "word") %>%
    dplyr::mutate(stop_word = ifelse(is.na(stop_word), FALSE, stop_word))

  genes_ <- genes %>%
    dplyr::mutate(gene_name = tolower(gene_name)) %>%
    dplyr::semi_join(words, c("gene_name" = "word"))

  cds <- grep("^c\\.", words$word, value = TRUE)
  cds <- sub("[^a-z0-9+]$", "", cds)
  aa <- grep("^p\\.", words$word, value = TRUE)
  aa <- sub("[^a-z0-9+]$", "", aa)

  variants_ <- variants %>%
    dplyr::mutate_all(.funs = tolower(.)) %>%
    dplyr::filter(Mutation_CDS %in% cds | Mutation_AA %in% aa) %>%
    dplyr::arrange(starts_with("Mutation"))

  words <- words %>%
    dplyr::left_join(genes_, "word") %>%
    dplyr::mutate(gene = ifelse(is.na(gene), FALSE, gene))

  words
}

