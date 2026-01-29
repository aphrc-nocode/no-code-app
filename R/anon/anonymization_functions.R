# anonymization_functions.R

# Load required packages
library(dplyr)
library(sdcMicro)
library(digest)
library(uuid)

# --- MASKING ---
# Replace every character in character columns with "*"
apply_masking <- function(df, cols){
  df %>%
    mutate(across(
      all_of(cols),
      ~ if (is.character(.x)) stringr::str_replace_all(.x, ".", "*") else .x
    ))
}
attr(apply_masking, "description") <- "Replace each character in specified columns with '*'"

# --- SUPPRESSION ---
# Remove the specified columns entirely
apply_suppression <- function(df, cols) {
  df %>% select(-any_of(cols))
}
attr(apply_suppression, "description") <- "Drop the specified columns"

# --- BUCKETING ---
# Groups numeric values into fixed-width bins starting at 0
apply_bucketing <- function(df, cols, bin_size) {
  df %>%
    mutate(across(
      all_of(cols),
      ~ {
        max_val <- max(.x, na.rm = TRUE)
        breaks  <- seq(0, max_val + bin_size, by = bin_size)
        labels  <- paste0(head(breaks, -1), "-", breaks[-1] - 1)
        cut(.x, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
      }
    ))
}
attr(apply_bucketing, "description") <- "Bucket numeric columns into fixed-width intervals starting at 0"

# --- PSEUDONYMIZATION ---
# Hash each value in the specified columns with SHA-256
apply_pseudonymization <- function(df, cols) {
  df %>%
    mutate(across(
      all_of(cols),
      ~ digest(as.character(.x), algo = "sha256")
    ))
}
attr(apply_pseudonymization, "description") <- "Replace values with SHA-256 hash digests"

# --- TOKENIZATION ---
# Replace each value with a random 10-character alphanumeric token
apply_tokenization <- function(df, cols, seed = 123) {
  set.seed(seed)
  df %>%
    mutate(across(
      all_of(cols),
      ~ replicate(n(), paste0(sample(c(0:9, letters, LETTERS), 10, TRUE), collapse = ""))
    ))
}
attr(apply_tokenization, "description") <- "Replace values with random 10-character tokens"

# --- BASIC K-ANONYMITY ---
# Keeps only those QID-groups of size >= k
apply_k_anonymity <- function(df, cols, k) {
  df %>%
    group_by(across(all_of(cols))) %>%
    mutate(.group_size = n()) %>%
    ungroup() %>%
    filter(.group_size >= k) %>%
    select(-.group_size)
}
attr(apply_k_anonymity, "description") <- "Filter to only those groups whose size >= k"

# --- EXTENDED K-ANONYMITY ---
# Suppress direct IDs, generalize numeric QIDs, then enforce k-anonymity via sdcMicro
apply_k_extended <- function(df, qids, k, bucket_cols = list(), direct_ids = character()) {
  # 1) Suppress direct identifiers
  df_proc <- df %>% select(-any_of(direct_ids))
  df_proc$row_id_temp <- seq_len(nrow(df_proc))
  
  # 2) Generalize numeric QIDs
  for (col in names(bucket_cols)) {
    df_proc[[col]] <- bucket_cols[[col]](df_proc[[col]])
  }
  
  # 3) sdcMicro k-anonymity
  qids2 <- intersect(qids, names(df_proc))
  sdcObj <- createSdcObj(dat = df_proc, keyVars = qids2)
  sdcObj <- kAnon(sdcObj, k = k)
  df_k   <- extractManipData(sdcObj)
  
  # 4) Combine matched & unmatched
  matched   <- df_k %>% select(-row_id_temp)
  unmatched <- df_proc %>% filter(!row_id_temp %in% df_k$row_id_temp) %>% select(-row_id_temp)
  bind_rows(matched, unmatched)
}
attr(apply_k_extended, "description") <- "Extended k-anonymity with bucketing & suppression"

# --- L-DIVERSITY ---
# Keep only those QID groups where the sensitive attribute has >= l distinct values
apply_l_diversity <- function(df, qids, sensitive_attr, l) {
  df %>%
    group_by(across(all_of(qids))) %>%
    filter(n_distinct(.data[[sensitive_attr]]) >= l) %>%
    ungroup()
}
attr(apply_l_diversity, "description") <- "Filter groups to ensure >= l diversity in the sensitive attribute"

# --- T-CLOSENESS ---
# Keep only those QID groups whose distribution of the sensitive attribute is within threshold t of the global distribution
apply_t_closeness <- function(df, qids, sensitive_attr, t) {
  # compute global distribution
  global_dist <- df %>% count(.data[[sensitive_attr]]) %>% mutate(prop = n / sum(n))
  
  df %>%
    group_by(across(all_of(qids))) %>%
    filter({
      local <- count(cur_data(), .data[[sensitive_attr]]) %>% mutate(prop = n / sum(n))
      # total variation distance
      tvd <- sum(abs(global_dist$prop - local$prop)) / 2
      tvd <= t
    }) %>%
    ungroup()
}
attr(apply_t_closeness, "description") <- "Filter groups whose sensitive-attribute distribution is within t of the global distribution"
