#' @export
#' @import tidytext tidyr
process_text <- function(tidy_txt, gene_name, variant_name) {
  # tidy_txt <- tidy_text(q$text_Text)
  variant_locations <- tidy_txt %>%
    dplyr::filter(variant == variant_name) %>%
    dplyr::pull(word_number)
  
  variant_count <- length(variant_locations)
 
  variant_disease_distances <- sapply(variant_locations, function(x, txt) {
    disease_loations <- txt %>%
      dplyr::filter(disease) %>%
      dplyr::pull(word_number)
    min(abs(x - disease_loations))
  }, txt = tidy_txt)
  
  variant_disease_mean <- mean(variant_disease_distances)
  variant_disease_min <- min(variant_disease_distances)
  
  variant_variant_distances <- sapply(variant_locations, 
                                      function(x, txt, variant_name) {
    variant_loations <- txt %>%
      dplyr::filter(variant != variant_name, !is.na(variant)) %>%
      dplyr::pull(word_number)
    min(abs(x - variant_loations))
  }, txt = tidy_txt, variant_name = variant_name)
  
  variant_variant_mean <- mean(variant_variant_distances)
  variant_variant_min <- min(variant_variant_distances)
  
  variant_in_disease_sentence <- tidy_txt %>%
    dplyr::group_by(sentence_number) %>%
    dplyr::filter(variant == variant_name, disease) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(count = n()) %>%
    dplyr::pull(count)
  
  gene_in_disease_sentence <- tidy_txt %>%
    dplyr::group_by(sentence_number) %>%
    dplyr::filter(gene == gene_name, disease) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(count = n()) %>%
    dplyr::pull(count)
  
  
  variant_sentences <- tidy_txt %>%
    dplyr::filter(variant == variant_name) %>%
    dplyr::pull(sentence_number) %>%
    unique()
  
  sentiment_schema <- expand.grid(variant_sentences,
                                  c("positive", "negative"),
                                  stringsAsFactors = FALSE)
  names(sentiment_schema) <- c("sentence_number", "sentiment")
  
  variant_sentence_sentiments <- tidy_txt %>%
    dplyr::filter(sentence_number %in% variant_sentences) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), "word") %>%
    dplyr::group_by(sentence_number, sentiment) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::full_join(sentiment_schema, c("sentence_number", "sentiment")) %>%
    dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
    tidyr::spread(sentiment, count) %>%
    dplyr::mutate(score = positive - negative) %>%
    dplyr::pull(score)
    
  variant_sentiment_mean <- mean(variant_sentence_sentiments)
  variant_sentiment_max <- max(variant_sentence_sentiments)
  
  data.frame(gene = gene_name, variant = variant_name, variant_count, variant_disease_mean,
             variant_disease_min, variant_variant_mean, variant_variant_min,
             variant_sentiment_mean, variant_sentiment_max, gene_in_disease_sentence,
             stringsAsFactors = FALSE)
} 

