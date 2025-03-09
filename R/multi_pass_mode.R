# multi_pass_mode.R

#' Refine an existing answer using the document text (follow-up pass).
#'
#' This function takes an initial answer and tries to improve it by consulting the document again.
#' It uses the original question and the current answer to prompt GPT for any corrections or additions based on the text.
#'
#' @param chunks The text chunks of the document.
#' @param question The original question.
#' @param current_answer The current answer that we want to refine.
#' @param model GPT model to use for refinement (default "gpt-3.5-turbo").
#' @param max_tokens Maximum tokens for the refined answer (default 1024).
#' @param num_retries Number of API retries (default 5).
#' @param pause_base Base pause for retries (default 3).
#' @return A refined answer string.
refine_answer <- function(chunks, question, current_answer, model = "gpt-3.5-turbo",
                          max_tokens = 1024, num_retries = 5, pause_base = 3) {
  # Use a subset of the document if possible for refinement: for example, search for keywords from current answer
  keywords <- unlist(strsplit(current_answer, "\\s+"))
  keywords <- keywords[nchar(keywords) > 4]  # use longer words from the answer as possible key terms
  snippets <- search_text(chunks, keywords[1:min(5, length(keywords))], window_chars = 300)
  doc_excerpt <- paste(snippets, collapse = "\n\n")
  refine_prompt <- paste(
    "Here is an initial answer to a question and some relevant excerpts from the document.",
    "Please refine the answer to be more accurate and complete using the provided document information.",
    "\n\nQuestion:\n", question,
    "\n\nCurrent Answer:\n", current_answer,
    "\n\nDocument Excerpts:\n", doc_excerpt
  )
  msgs <- list(
    list(role = "system", content = "You are a fact-checker and editor improving the answer using the document."),
    list(role = "user", content = refine_prompt)
  )
  refined <- process_api_call(msgs, model = model, temperature = 0.0, max_tokens = max_tokens,
                              presence_penalty = 0.0, frequency_penalty = 0.0,
                              num_retries = num_retries, pause_base = pause_base)
  return(ifelse(is.null(refined) || !nzchar(refined), current_answer, stringr::str_trim(refined)))
}

#' Answer a question using a multi-pass approach (combining retrieval and chunked results).
#'
#' This method uses two different passes: one by extracting relevant text (Retrieval mode) and one by chunked querying (Deep Thinking mode).
#' The results of both are then merged to produce a final answer, leveraging both strategies.
#'
#' @param chunks Text chunks of the document.
#' @param question The question to answer.
#' @param model GPT model to use (default "gpt-3.5-turbo").
#' @param use_parallel Whether to use parallel processing for any chunked operations (default FALSE).
#' @param ... Additional parameters passed to the underlying retrieval/chunked functions (e.g., temperature, max_tokens).
#' @return A combined answer string.
gpt_read_multipass <- function(chunks, question, model = "gpt-3.5-turbo", use_parallel = FALSE, ...) {
  message("Multi-pass strategy: performing Retrieval and Chunked modes...")
  # First pass: Retrieval mode (without falling back automatically)
  answer1 <- gpt_read_retrieval(chunks, question, model = model, use_parallel = use_parallel, fallback = FALSE, ...)
  # Second pass: Chunked mode
  answer2 <- gpt_read_chunked(chunks, question, model = model, use_parallel = use_parallel, ...)
  # If one of the answers is empty or not found, use the other directly
  if (is.null(answer1) || !nzchar(trimws(answer1))) {
    final_answer <- answer2
  } else if (is.null(answer2) || !nzchar(trimws(answer2))) {
    final_answer <- answer1
  } else {
    # Both methods yielded some answer; merge them via GPT for a comprehensive result
    merge_prompt <- paste("Answer from retrieval method:\n", answer1,
                          "\n\nAnswer from chunked method:\n", answer2,
                          "\n\nMerge these answers into one comprehensive, accurate answer to the question.")
    msgs_merge <- list(
      list(role = "system", content = "You are a moderator who combines answers from different approaches into one."),
      list(role = "user", content = merge_prompt)
    )
    combined <- process_api_call(msgs_merge, model = model, temperature = 0.0, 
                                 max_tokens = get_model_limits(model)$output_tokens,
                                 presence_penalty = 0.0, frequency_penalty = 0.0, 
                                 num_retries = 5, pause_base = 3)
    # Fallback: if merge failed, just concatenate
    if (is.null(combined) || !nzchar(trimws(combined))) {
      final_answer <- paste(answer1, answer2, sep="\n\n---\n\n")
    } else {
      final_answer <- combined
    }
  }
  return(stringr::str_trim(final_answer))
}
