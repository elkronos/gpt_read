# chunked_mode.R

#' Answer a question by querying each text chunk (Deep Thinking Mode).
#'
#' In this mode, the document is split into chunks which are each queried with the question.
#' Each chunk's response is then combined and a final answer is generated from all partial responses.
#'
#' @param chunks A character vector of text chunks.
#' @param question The question to answer.
#' @param model GPT model to use for chunk querying (default "gpt-3.5-turbo").
#' @param temperature Temperature for the OpenAI API (default 0.0).
#' @param max_tokens Maximum tokens for each chunk response (defaults to model's output limit).
#' @param system_message_1 System prompt for chunk-level querying (instructions for how to answer using chunk text).
#' @param system_message_2 System prompt for final merging of answers.
#' @param presence_penalty Presence penalty for the API (default 0.0).
#' @param frequency_penalty Frequency penalty for the API (default 0.0).
#' @param num_retries Number of API retries for each call (default 5).
#' @param pause_base Base pause time for retry backoff (default 3).
#' @param delay_between_chunks Delay (seconds) between chunk requests to avoid rate limits (default 0).
#' @param use_parallel Whether to process chunks in parallel (default FALSE). If TRUE, ensure a future plan is set.
#' @return A single consolidated answer string.
gpt_read_chunked <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0, max_tokens = NULL,
                             system_message_1 = "You are a helpful assistant. Answer the question using ONLY the given text. If the text does not contain the answer, say so.",
                             system_message_2 = "You are a content editor who will merge multiple pieces of answers into one comprehensive answer.",
                             presence_penalty = 0.0, frequency_penalty = 0.0,
                             num_retries = 5, pause_base = 3, delay_between_chunks = 0,
                             use_parallel = FALSE) {
  if (is.null(question) || nchar(trimws(question)) == 0) {
    stop("Question must be provided for chunked processing.")
  }
  # Determine token limits
  model_limits <- get_model_limits(model)
  if (is.null(max_tokens)) {
    max_tokens <- model_limits$output_tokens  # allow model's full output size for each chunk answer
  }
  # Allowed input tokens per chunk (context window minus question and output allowance)
  allowed_input_tokens <- model_limits$context_window - max_tokens - estimate_token_count(question) - 50
  
  # Function to query a single chunk (and sub-chunk if it's still too large)
  query_chunk <- function(chunk) {
    if (estimate_token_count(chunk) > allowed_input_tokens) {
      # If chunk is still too large for context, split it further into subchunks
      tokens <- unlist(strsplit(chunk, "\\s+"))
      sub_chunks <- split(tokens, ceiling(seq_along(tokens) / allowed_input_tokens))
      sub_chunks <- lapply(sub_chunks, paste, collapse = " ")
      sub_responses <- c()
      for (subchunk in sub_chunks) {
        msgs <- list(
          list(role = "system", content = system_message_1),
          list(role = "user", content = subchunk),
          list(role = "user", content = question)
        )
        resp <- process_api_call(msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                                 presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                                 num_retries = num_retries, pause_base = pause_base)
        if (!is.null(resp) && nzchar(resp)) sub_responses <- c(sub_responses, resp)
        if (delay_between_chunks > 0) Sys.sleep(delay_between_chunks)
      }
      return(paste(sub_responses, collapse = " "))
    } else {
      # Single-chunk straightforward query
      msgs <- list(
        list(role = "system", content = system_message_1),
        list(role = "user", content = chunk),
        list(role = "user", content = question)
      )
      resp <- process_api_call(msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                               presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                               num_retries = num_retries, pause_base = pause_base)
      return(ifelse(is.null(resp), "", resp))
    }
  }
  
  # Query all chunks (in parallel if use_parallel is TRUE and a future plan is set)
  responses <- if (use_parallel) {
    future.apply::future_lapply(chunks, query_chunk)
  } else {
    lapply(chunks, query_chunk)
  }
  responses <- unlist(responses)
  
  # If all responses indicate no info found, handle that
  if (all(nzchar(responses) == FALSE) || 
      all(grepl("not found|no information|not applicable", tolower(responses)))) {
    final_answer <- "The answer to the question was not found in the provided document."
  } else {
    # Merge the chunk responses into one prompt for final answer synthesis
    combined_content <- paste(responses, collapse = "\n\n")
    merge_msgs <- list(
      list(role = "system", content = system_message_2),
      list(role = "user", content = combined_content),
      list(role = "user", content = paste("Question:", question))
    )
    final_answer <- process_api_call(merge_msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                                     presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                                     num_retries = num_retries, pause_base = pause_base)
    if (is.null(final_answer) || !nzchar(final_answer)) {
      final_answer <- paste(responses, collapse = " ")  # fallback: concatenate responses if merge failed
    }
  }
  return(stringr::str_trim(final_answer))
}
