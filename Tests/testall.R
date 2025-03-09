#!/usr/bin/env Rscript

library(testthat)
library(stringr)
library(future.apply)

### --- Dummy implementations & mocks for dependencies --- ###

# Return a fixed token configuration for testing.
get_model_limits <- function(model) {
  list(output_tokens = 50, context_window = 200)
}

# For testing, assume 1 word = 1 token.
estimate_token_count <- function(text) {
  length(unlist(strsplit(text, "\\s+")))
}

# Stub process_api_call that returns a predictable answer based on the last "user" message.
process_api_call <- function(msgs, model, temperature, max_tokens, 
                             presence_penalty, frequency_penalty,
                             num_retries, pause_base) {
  user_msgs <- sapply(msgs, function(x) if (x$role == "user") x$content else "")
  return(paste("Response:", tail(user_msgs, 1)))
}

# Override Sys.sleep during tests to avoid actual delays.
original_sleep <- Sys.sleep
Sys.sleep <- function(seconds) { }

### --- Dummy implementations for answer_question dependencies --- ###

# Dummy file parser that always returns two chunks.
parse_text <- function(file_path, chunk_method, ...) {
  return(c("Chunk one text", "Chunk two text"))
}

# Dummy implementation for Retrieval mode.
gpt_read_retrieval <- function(chunks, question, use_parallel, ...) {
  return("Retrieval answer")
}

# Redefine gpt_read_chunked using our dummy process_api_call.
gpt_read_chunked <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0, max_tokens = NULL,
                             system_message_1 = "You are a helpful assistant. Answer the question using ONLY the given text. If the text does not contain the answer, say so.",
                             system_message_2 = "You are a content editor who will merge multiple pieces of answers into one comprehensive answer.",
                             presence_penalty = 0.0, frequency_penalty = 0.0,
                             num_retries = 5, pause_base = 3, delay_between_chunks = 0,
                             use_parallel = FALSE) {
  if (is.null(question) || nchar(trimws(question)) == 0) {
    stop("Question must be provided for chunked processing.")
  }
  
  model_limits <- get_model_limits(model)
  if (is.null(max_tokens)) {
    max_tokens <- model_limits$output_tokens
  }
  allowed_input_tokens <- model_limits$context_window - max_tokens - estimate_token_count(question) - 50
  
  query_chunk <- function(chunk) {
    if (estimate_token_count(chunk) > allowed_input_tokens) {
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
        resp <- process_api_call(msgs, model, temperature, max_tokens,
                                 presence_penalty, frequency_penalty,
                                 num_retries, pause_base)
        if (!is.null(resp) && nzchar(resp)) sub_responses <- c(sub_responses, resp)
        if (delay_between_chunks > 0) Sys.sleep(delay_between_chunks)
      }
      return(paste(sub_responses, collapse = " "))
    } else {
      msgs <- list(
        list(role = "system", content = system_message_1),
        list(role = "user", content = chunk),
        list(role = "user", content = question)
      )
      resp <- process_api_call(msgs, model, temperature, max_tokens,
                               presence_penalty, frequency_penalty,
                               num_retries, pause_base)
      return(ifelse(is.null(resp), "", resp))
    }
  }
  
  responses <- if (use_parallel) {
    future.apply::future_lapply(chunks, query_chunk)
  } else {
    lapply(chunks, query_chunk)
  }
  responses <- unlist(responses)
  
  if (all(nzchar(responses) == FALSE) || 
      all(grepl("not found|no information|not applicable", tolower(responses)))) {
    final_answer <- "The answer to the question was not found in the provided document."
  } else {
    combined_content <- paste(responses, collapse = "\n\n")
    merge_msgs <- list(
      list(role = "system", content = system_message_2),
      list(role = "user", content = combined_content),
      list(role = "user", content = paste("Question:", question))
    )
    final_answer <- process_api_call(merge_msgs, model, temperature, max_tokens,
                                     presence_penalty, frequency_penalty,
                                     num_retries, pause_base)
    if (is.null(final_answer) || !nzchar(final_answer)) {
      final_answer <- paste(responses, collapse = " ")
    }
  }
  return(str_trim(final_answer))
}

gpt_read_hierarchical <- function(chunks, question, use_parallel, ...) {
  return("Hierarchical answer")
}

gpt_read_multipass <- function(chunks, question, use_parallel, ...) {
  return("MultiPass answer")
}

refine_answer <- function(chunks, question, answer, ...) {
  return(paste(answer, "- refined"))
}

log_query <- function(question, answer) {
  invisible(NULL)
}

# High-level function from main.R.
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
  chunk_method <- if (mode == "Semantic") "semantic" else "naive"
  chunks <- parse_text(file_path, chunk_method = chunk_method, ...)
  message("Document split into ", length(chunks), " chunks.")
  answer <- NULL
  if (mode == "Retrieval") {
    message("Mode: Retrieval - extracting relevant sections and answering...")
    answer <- gpt_read_retrieval(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "Chunked") {
    message("Mode: Chunked - querying each chunk and aggregating answers...")
    answer <- gpt_read_chunked(chunks, question, use_parallel = use_parallel, ...)
  } else if (mode == "Semantic") {
    message("Mode: Semantic - using semantic chunking and chunked querying...")
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

### --------------------------
### Test Cases using testthat
### --------------------------

test_that("gpt_read_chunked handles empty question", {
  expect_error(gpt_read_chunked(chunks = c("Some text"), question = ""),
               "Question must be provided for chunked processing.")
})

test_that("gpt_read_chunked uses default model limits when max_tokens is NULL", {
  ans <- gpt_read_chunked(chunks = c("Test chunk"), question = "What is this?")
  expect_true(grepl("Response:", ans))
})

test_that("gpt_read_chunked splits oversized chunk", {
  long_chunk <- paste(rep("word", 500), collapse = " ")
  ans <- gpt_read_chunked(chunks = c(long_chunk), question = "What is this?")
  expect_true(nchar(ans) > 0)
})

test_that("gpt_read_chunked handles delay_between_chunks", {
  sleep_calls <- 0
  original_sleep_local <- Sys.sleep
  Sys.sleep <<- function(seconds) { sleep_calls <<- sleep_calls + 1 }
  on.exit(Sys.sleep <<- original_sleep_local)
  
  long_chunk <- paste(rep("word", 500), collapse = " ")
  gpt_read_chunked(chunks = c(long_chunk), question = "What is this?", delay_between_chunks = 1)
  expect_true(sleep_calls > 0)
})

test_that("gpt_read_chunked uses parallel processing when enabled", {
  parallel_called <- FALSE
  # Use testthat's local_mock to override future.apply::future_lapply.
  local_mock(
    `future.apply::future_lapply` = function(X, FUN, ...) {
      parallel_called <<- TRUE
      lapply(X, FUN, ...)
    }
  )
  gpt_read_chunked(chunks = c("Chunk 1", "Chunk 2"), question = "What is this?", use_parallel = TRUE)
  expect_true(parallel_called)
})

test_that("answer_question stops on invalid file path", {
  expect_error(answer_question("nonexistent_file.txt", "Test question"),
               "Please provide a valid file path.")
})

test_that("answer_question stops on empty question", {
  tmp <- tempfile()
  writeLines("Test content", tmp)
  on.exit(unlink(tmp))
  expect_error(answer_question(tmp, ""),
               "Please provide a non-empty question.")
})

test_that("answer_question processes each mode correctly", {
  tmp <- tempfile()
  writeLines("Test content", tmp)
  on.exit(unlink(tmp))
  
  retrieval_ans <- answer_question(tmp, "Test retrieval", mode = "Retrieval")
  expect_equal(retrieval_ans, "Retrieval answer")
  
  chunked_ans <- answer_question(tmp, "Test chunked", mode = "Chunked")
  expect_true(grepl("Response:", chunked_ans))
  
  semantic_ans <- answer_question(tmp, "Test semantic", mode = "Semantic")
  expect_true(grepl("Response:", semantic_ans))
  
  hierarchical_ans <- answer_question(tmp, "Test hierarchical", mode = "Hierarchical")
  expect_equal(hierarchical_ans, "Hierarchical answer")
  
  multipass_ans <- answer_question(tmp, "Test multipass", mode = "MultiPass")
  expect_equal(multipass_ans, "MultiPass answer")
})

test_that("answer_question applies refinement when requested", {
  tmp <- tempfile()
  writeLines("Test content", tmp)
  on.exit(unlink(tmp))
  
  refined_ans <- answer_question(tmp, "Test refine", mode = "Retrieval", refine = TRUE)
  expect_true(grepl("- refined", refined_ans))
})

# Restore original Sys.sleep
Sys.sleep <- original_sleep
