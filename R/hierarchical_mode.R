# hierarchical_mode.R

#' Answer a question using hierarchical two-pass reading of the document.
#'
#' First, each chunk of the document is summarized with a focus on the question (producing a higher-level digest of that chunk).
#' Then, the summaries of all chunks are combined and used as context to answer the question in detail.
#'
#' @param chunks A character vector of text chunks.
#' @param question The question to answer.
#' @param model GPT model to use (default "gpt-3.5-turbo").
#' @param temperature Temperature for the OpenAI API (default 0.0 for deterministic summaries/answers).
#' @param summary_max_tokens Max tokens for each summary (default 512).
#' @param answer_max_tokens Max tokens for the final answer (default 1024).
#' @param use_parallel Whether to summarize chunks in parallel (default FALSE).
#' @param num_retries API retry count (default 5).
#' @param pause_base Base pause for retries (default 3).
#' @return A answer string derived from hierarchical reading.
gpt_read_hierarchical <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0,
                                  summary_max_tokens = 512, answer_max_tokens = 1024,
                                  use_parallel = FALSE, num_retries = 5, pause_base = 3) {
  if (is.null(question) || nchar(trimws(question)) == 0) {
    stop("Question must be provided for hierarchical mode.")
  }
  message("Summarizing document in relation to the question...")
  # Define a function to summarize one chunk with focus on the question
  summarize_chunk <- function(chunk_text) {
    prompt <- paste("Summarize the following document excerpt with respect to the question.",
                    "Focus on any information that might be relevant to answering the question.",
                    "\n\nExcerpt:\n", chunk_text, "\n\nQuestion:\n", question)
    msgs <- list(
      list(role = "system", content = "You are a helpful assistant summarizing text for a question."),
      list(role = "user", content = prompt)
    )
    summary <- process_api_call(msgs, model = model, temperature = temperature, max_tokens = summary_max_tokens,
                                presence_penalty = 0.0, frequency_penalty = 0.0,
                                num_retries = num_retries, pause_base = pause_base)
    return(ifelse(is.null(summary), "", summary))
  }
  # Summarize each chunk (parallel if enabled)
  chunk_summaries <- if (use_parallel) {
    future.apply::future_lapply(chunks, summarize_chunk)
  } else {
    lapply(chunks, summarize_chunk)
  }
  chunk_summaries <- unlist(chunk_summaries)
  # Filter out any empty or irrelevant summaries (if any)
  rel_summaries <- chunk_summaries[nchar(trimws(chunk_summaries)) > 0]
  if (length(rel_summaries) == 0) {
    warning("No relevant content found in summaries; falling back to direct chunked answering.")
    return(gpt_read_chunked(chunks, question, model = model, temperature = temperature,
                            max_tokens = answer_max_tokens, use_parallel = use_parallel))
  }
  # Combine summaries and ask the final question
  combined_summary <- paste(rel_summaries, collapse = "\n\n")
  message("Using combined summaries to answer the question...")
  final_prompt <- paste("Based on the following summaries of a document, answer the question in detail.\n\n",
                        "Summaries:\n", combined_summary, "\n\nQuestion:\n", question)
  msgs_final <- list(
    list(role = "system", content = "You are a knowledgeable assistant who uses summaries of a document to answer questions."),
    list(role = "user", content = final_prompt)
  )
  answer <- process_api_call(msgs_final, model = model, temperature = temperature, max_tokens = answer_max_tokens,
                             presence_penalty = 0.0, frequency_penalty = 0.0,
                             num_retries = num_retries, pause_base = pause_base)
  if (is.null(answer) || !nzchar(answer)) {
    warning("Final answer generation failed; returning combined summaries as answer.")
    answer <- combined_summary  # fallback: at least provide the summaries if no final answer
  }
  return(stringr::str_trim(answer))
}
