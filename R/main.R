# main.R

#' Answer a question about a document using various processing modes.
#'
#' This high-level function orchestrates the document reading and question answering.
#' It allows different modes of operation:
#' - "Retrieval": extract relevant sections and query GPT once.
#' - "Chunked": split the document and query GPT chunk by chunk (Deep Thinking).
#' - "Semantic": use semantic-aware chunking and then sort chunks by semantic relevance before querying.
#' - "Hierarchical": summarize sections first, then answer using the summaries.
#' - "MultiPass": combine retrieval and chunked strategies for an answer.
#'
#' @param file_path Path to the document file.
#' @param question The question to ask about the document.
#' @param mode The processing mode (one of "Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass").
#' @param use_parallel Whether to enable parallel processing for chunk-based modes (default FALSE).
#' @param refine If TRUE, perform an additional refinement pass on the final answer (default FALSE).
#' @param ... Additional arguments passed to lower-level functions (e.g., model, temperature, etc.).
#' @return The answer to the question (as a character string).
answer_question <- function(file_path, question, mode = c("Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass"),
                            use_parallel = FALSE, refine = FALSE, ...) {
  mode <- match.arg(mode)
  
  if (missing(file_path) || !file.exists(file_path)) {
    stop("Please provide a valid file path.")
  }
  if (missing(question) || nchar(trimws(question)) == 0) {
    stop("Please provide a non-empty question.")
  }
  
  message("Parsing document and preparing text chunks...")
  # Choose chunking method: "semantic" for Semantic mode, otherwise "naive".
  chunk_method <- if (mode == "Semantic") "semantic" else "naive"
  # Do not pass ... here since parse_text does not accept extra parameters.
  chunks <- parse_text(file_path, chunk_method = chunk_method)
  message("Document split into ", length(chunks), " chunks.")
  
  # For Semantic mode, sort the chunks by semantic similarity.
  if (mode == "Semantic") {
    message("Sorting chunks by semantic relevance...")
    chunks <- sort_chunks_by_semantic(chunks, question)
  }
  
  answer <- NULL
  if (mode == "Retrieval") {
    message("Mode: Retrieval - extracting relevant sections and answering...")
    answer <- gpt_read_retrieval(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "Chunked") {
    message("Mode: Chunked - querying each chunk and aggregating answers...")
    answer <- gpt_read_chunked(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "Semantic") {
    message("Mode: Semantic - using semantic sorting and chunked querying...")
    answer <- gpt_read_chunked(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "Hierarchical") {
    message("Mode: Hierarchical - summarizing and then answering...")
    answer <- gpt_read_hierarchical(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "MultiPass") {
    message("Mode: MultiPass - combining retrieval and chunked strategies...")
    answer <- gpt_read_multipass(chunks, question, use_parallel = use_parallel, ...)
  }
  
  if (refine) {
    message("Refining the answer with an additional pass...")
    answer <- refine_answer(chunks, question, answer, ...)
  }
  
  log_query(question, answer)
  
  cat("\n====================\nQuestion: ", question, "\n\nAnswer:\n", answer, "\n====================\n")
  return(invisible(answer))
}
