#!/usr/bin/env Rscript
# full_uat_qa.R
#
# This script demonstrates a complete user acceptance test (UAT) for the document Q&A system.
# It logs the system's processing workflow by displaying the chain-of-thought, such as an initial
# retrieval phase that may return an incorrect answer (e.g., "Washington D.C.") and a subsequent
# refinement phase that corrects it to ("Paris").
#
# In "Semantic" mode, the script also shows the sorted text chunks (by semantic similarity) to
# confirm that the semantic grouping is operating as expected.
#
# An additional test forces the document to split into multiple chunks by lowering the
# chunk_token_limit. A mixed-quality document example is also provided to examine how the system
# handles conflicting information.
#
# To run this script from the terminal, use:
#   Rscript full_uat_qa.R
#
# Optionally, set your API key if making actual API calls:
# Sys.setenv(OPENAI_API_KEY = "your-api-key")

# ------------------------------
# Load required functions and packages using relative paths.
# ------------------------------
source("../R/packages.R")
source("../R/utils.R")
source("../R/text_processing.R")
source("../R/retrieval_mode.R")
source("../R/chunked_mode.R")
source("../R/hierarchical_mode.R")
source("../R/multi_pass_mode.R")
source("../R/main.R")

# ---- Override process_api_call to simulate the system's workflow ----
process_api_call <- function(msgs, model, temperature, max_tokens,
                             presence_penalty, frequency_penalty,
                             num_retries, pause_base) {
  # Log all messages being sent.
  cat(">> API Call with messages:\n")
  for (m in msgs) {
    cat("   [", m$role, "]: ", m$content, "\n", sep = "")
  }
  
  last_msg <- tail(sapply(msgs, function(x) x$content), 1)
  
  # Simulate behavior based on context.
  if (grepl("You selectively extract relevant text", msgs[[1]]$content, fixed = TRUE)) {
    cat(">> Simulating extraction of relevant text...\n")
    # Simulate a false extraction even if the document includes correct information.
    return("Extracted Snippet: According to the document, the capital of France is Washington D.C.")
  }
  
  if (grepl("Relevant Text:", last_msg, fixed = TRUE)) {
    cat(">> Simulating answering based on extracted text...\n")
    # Return the false answer based on the extracted snippet.
    return("According to the document, the capital of France is Washington D.C.")
  }
  
  if (grepl("fact-checker", msgs[[1]]$content, fixed = TRUE)) {
    cat(">> Simulating refinement process...\n")
    # Simulate correcting the answer.
    return("The capital of France is Paris.")
  }
  
  # For chunk-level queries (simulate chunk responses).
  if (grepl("Chunk one text", last_msg) || grepl("Chunk two text", last_msg)) {
    cat(">> Simulating chunk-level response...\n")
    return(paste0("Response: ", last_msg))
  }
  
  # Default: return the last message.
  return(paste("Response:", last_msg))
}

# ------------------------------
# Logging Setup
# ------------------------------
log_file <- "uat_qa_log.txt"
sink(log_file, append = FALSE, split = TRUE)
cat("========== UAT LOG ==========\n")
cat("Timestamp:", Sys.time(), "\n\n")

# ------------------------------
# Prepare Sample Documents
# ------------------------------

# Accurate document sample.
accurate_text <- paste(
  "France is a country in Western Europe with a rich historical background and diverse cultural heritage.",
  "Geographically, France spans from the sunny Mediterranean in the south to the cool, windy shores of the English Channel in the north.",
  "The capital of France is Paris, renowned as a global center for art, fashion, gastronomy, and culture.",
  "Economically, France is a leader in aerospace, luxury goods, agriculture, and tourism.",
  "Culturally, it is celebrated for its cuisine, wine production, and artistic legacy.",
  "Historically, France has played a central role in shaping modern Europe.",
  sep = "\n\n"
)

# Mixed-quality document sample (contains both accurate and false information).
mixed_text <- paste(
  "France is a country in Western Europe with a rich historical background and diverse cultural heritage.",
  "According to some outdated sources, the capital of France is Washington D.C., although this is false.",
  "Geographically, France spans from the sunny Mediterranean in the south to the cool, windy shores of the English Channel in the north.",
  "In reality, the capital of France is Paris, a major center for art, fashion, gastronomy, and culture.",
  "Economically, France is at the forefront of several industries including aerospace and tourism.",
  "Culturally, while some sources may claim otherwise, French cuisine and wine are among the best in the world.",
  "Historically, France has always maintained Paris as its political and cultural hub.",
  sep = "\n\n"
)

# For this test, use the mixed-quality document.
sample_text <- mixed_text

cat("----- Sample Document -----\n")
cat(sample_text, "\n\n")

# Write the sample document to a temporary file.
temp_file <- tempfile(fileext = ".txt")
writeLines(sample_text, temp_file)
cat("Temporary file created at:", temp_file, "\n\n")

# ------------------------------
# Log Parsed Chunks with Default Token Limit
# ------------------------------
cat("----- Parsing Document with Default Token Limit -----\n")
parsed_chunks <- parse_text(temp_file, chunk_method = "naive")
for (i in seq_along(parsed_chunks)) {
  cat(sprintf("Chunk %d: %s\n", i, parsed_chunks[i]))
}
cat("\n")

# ------------------------------
# Test Multiple Chunks by Lowering the Token Limit
# ------------------------------
cat("----- Testing Multiple Chunks by Lowering Token Limit -----\n")
# Clear the cache for this document.
file_key <- normalizePath(temp_file, winslash = "/", mustWork = FALSE)
if (exists(".doc_cache", envir = .GlobalEnv)) {
  doc_cache <- get(".doc_cache", envir = .GlobalEnv)
  if (!is.null(doc_cache[[file_key]])) {
    rm(list = file_key, envir = doc_cache)
  }
}
# Force multiple chunks with a low token limit (e.g., 50 tokens).
parsed_chunks_multiple <- parse_text(temp_file, chunk_method = "naive", chunk_token_limit = 50)
cat("Parsed Chunks with chunk_token_limit=50:\n")
for (i in seq_along(parsed_chunks_multiple)) {
  cat(sprintf("Chunk %d: %s\n", i, parsed_chunks_multiple[i]))
}
cat("\n")

# ------------------------------
# Test Semantic Sorting Functionality
# ------------------------------
cat("----- Testing Semantic Sorting Functionality -----\n")
sorted_chunks <- sort_chunks_by_semantic(parsed_chunks, "What is the capital of France?")
cat("Sorted Chunks (in descending order of similarity):\n")
for (i in seq_along(sorted_chunks)) {
  cat(sprintf("Sorted Chunk %d: %s\n", i, sorted_chunks[i]))
}
cat("\n")

# ------------------------------
# Define the Sample Question
# ------------------------------
sample_question <- "What is the capital of France?"
cat("----- Sample Question -----\n")
cat(sample_question, "\n\n")

cat("============================================\n")
cat("LIVE TEST: API calls for each processing mode\n")
cat("============================================\n\n")

# ------------------------------
# Test Retrieval Mode
# ------------------------------
cat("----- Testing Retrieval Mode -----\n")
retrieval_answer <- answer_question(temp_file, sample_question, mode = "Retrieval")
cat("Retrieval Answer:\n", retrieval_answer, "\n\n")

# ------------------------------
# Test Chunked Mode
# ------------------------------
cat("----- Testing Chunked Mode -----\n")
chunked_answer <- answer_question(temp_file, sample_question, mode = "Chunked")
cat("Chunked Answer:\n", chunked_answer, "\n\n")

# ------------------------------
# Test Semantic Mode
# ------------------------------
cat("----- Testing Semantic Mode -----\n")
semantic_answer <- answer_question(temp_file, sample_question, mode = "Semantic")
cat("Semantic Answer:\n", semantic_answer, "\n\n")

# ------------------------------
# Test Hierarchical Mode
# ------------------------------
cat("----- Testing Hierarchical Mode -----\n")
hierarchical_answer <- answer_question(temp_file, sample_question, mode = "Hierarchical")
cat("Hierarchical Answer:\n", hierarchical_answer, "\n\n")

# ------------------------------
# Test MultiPass Mode
# ------------------------------
cat("----- Testing MultiPass Mode -----\n")
multipass_answer <- answer_question(temp_file, sample_question, mode = "MultiPass")
cat("MultiPass Answer:\n", multipass_answer, "\n\n")

# ------------------------------
# Test Refinement Mode
# ------------------------------
cat("----- Testing Refinement on Retrieval Mode -----\n")
refined_answer <- answer_question(temp_file, sample_question, mode = "Retrieval", refine = TRUE)
cat("Refined Answer:\n", refined_answer, "\n\n")

# ------------------------------
# Cleanup
# ------------------------------
unlink(temp_file)
cat("========== UAT complete ==========\n")
cat("Check 'uat_qa_log.txt' for full details of all intermediate outputs.\n")

# Restore console output.
sink()
