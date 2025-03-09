# retrieval_mode.R

#' Search text for keywords and extract surrounding context.
#'
#' A simple keyword-based retrieval that finds occurrences of keywords in the text 
#' and returns snippets around them. Useful for quick filtering of relevant sections.
#'
#' @param text A character string (or vector of strings) to search within.
#' @param keywords A character vector of keywords (single words or phrases).
#' @param window_chars Number of characters to include before and after each hit (default 200).
#' @return A list of passages containing the keywords.
search_text <- function(text, keywords, window_chars = 200) {
  text_str <- paste(text, collapse = " ")
  results <- list()
  for (kw in keywords) {
    matches <- gregexpr(kw, text_str, ignore.case = TRUE)[[1]]
    for (m in matches[matches != -1]) {
      start <- max(1, m - window_chars)
      end <- min(nchar(text_str), m + attr(matches, "match.length")[1] + window_chars)
      snippet <- substr(text_str, start, end)
      results <- c(results, snippet)
    }
  }
  return(unique(results))
}

#' Answer a question using Retrieval mode (extract relevant text then query GPT).
#'
#' This function consolidates the document text and attempts to extract only the parts relevant to the question.
#' It then asks GPT the question using just those relevant parts as context.
#' If relevant text extraction fails or yields nothing, it can fall back to chunked mode.
#'
#' @param chunks A character vector of text chunks (from parse_text).
#' @param question The question to answer.
#' @param model GPT model to use for filtering and answering (default "gpt-3.5-turbo").
#' @param temperature Temperature for the OpenAI API (default 0.0 for deterministic answers).
#' @param max_tokens Max tokens for the answer (defaults to model's limit).
#' @param presence_penalty Presence penalty for the API (default 0.0).
#' @param frequency_penalty Frequency penalty for the API (default 0.0).
#' @param num_retries Number of API call retries on failure (default 5).
#' @param pause_base Base wait time for exponential backoff between retries (default 3 seconds).
#' @param use_parallel If falling back to chunked mode, whether to use parallel processing (default FALSE).
#' @param fallback Whether to fall back to chunked mode if no relevant text is found (default TRUE).
#' @return A answer string to the question.
gpt_read_retrieval <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0, 
                               max_tokens = NULL, presence_penalty = 0.0, frequency_penalty = 0.0,
                               num_retries = 5, pause_base = 3, use_parallel = FALSE, fallback = TRUE) {
  # Combine all chunks into one text (the full document content)
  full_text <- paste(chunks, collapse = "\n\n")
  # Ensure we don't exceed model context window for the filtering step
  model_limits <- get_model_limits(model)
  if (is.null(max_tokens)) max_tokens <- model_limits$output_tokens
  allowed_input_tokens <- model_limits$context_window - max_tokens - 1000  # reserve some buffer
  if (estimate_token_count(full_text) > allowed_input_tokens) {
    message("Document is too large for a single retrieval query; using chunked approach instead.")
    if (!fallback) {
      return("")  # if fallback not allowed, return empty indicating failure
    }
    # Fall back to chunked mode for answering
    return(gpt_read_chunked(chunks, question, model = model, temperature = temperature, max_tokens = max_tokens,
                            presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                            num_retries = num_retries, pause_base = pause_base, use_parallel = use_parallel))
  }
  # Construct prompt to have GPT extract relevant sections
  relevant_prompt <- paste(
    "You are an assistant that helps extract only the sections of a document that are relevant",
    "to answering a given question. Return the text snippets that are most pertinent to the question,",
    "and omit any parts that are not helpful for answering.",
    "\n\nQuestion:\n", question,
    "\n\nDocument:\n", full_text
  )
  messages_filter <- list(
    list(role = "system", content = "You selectively extract relevant text for a question."),
    list(role = "user", content = relevant_prompt)
  )
  relevant_text <- process_api_call(messages_filter, model = model, temperature = 0.0, 
                                    max_tokens = ifelse(is.null(max_tokens), 2048, max_tokens),
                                    presence_penalty = 0.0, frequency_penalty = 0.0,
                                    num_retries = num_retries, pause_base = pause_base)
  if (is.null(relevant_text) || nchar(trimws(relevant_text)) == 0) {
    warning("No relevant text extracted by GPT.")
    if (!fallback) {
      return("")  # no relevant info found and no fallback
    }
    # Fall back to chunked mode if GPT could not find anything
    return(gpt_read_chunked(chunks, question, model = model, temperature = temperature, max_tokens = max_tokens,
                            presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                            num_retries = num_retries, pause_base = pause_base, use_parallel = use_parallel))
  }
  # Now ask GPT the final question using only the relevant text
  message("Using filtered relevant text to answer the question...")
  messages_answer <- list(
    list(role = "system", content = "You are a research assistant who answers questions based solely on the provided excerpts."),
    list(role = "user", content = paste("Relevant Text:\n", relevant_text)),
    list(role = "user", content = paste("Question:\n", question))
  )
  answer <- process_api_call(messages_answer, model = model, temperature = temperature, max_tokens = max_tokens,
                             presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                             num_retries = num_retries, pause_base = pause_base)
  return(answer)
}
