library(jsonlite)
# The annotation packages (org.Hs.eg.db, biomaRt) are only needed by the annotation/DP
# functions, not by the feature-selection data path. FS background workers set
# FS_WORKER_LITE=1 to skip loading them — this cuts job startup time and memory.
#if (!identical(Sys.getenv("FS_WORKER_LITE"), "1")) {
#  library(org.Hs.eg.db)
#  library(ensembldb)
#}

# Import shared utility functions
source("shared_utils.R")

# ---- Illumina probe quality + detection P-val filters (ported from Microarray_processing.R) ----

ILLUMINA_PROBE_QUALITY_PLATFORMS <- c(
  "illuminaHumanv3", "illuminaHumanv4", "illuminaMousev2", "illuminaRatv1"
)

filter_probes_by_quality <- function(expr_mat, platform_id) {
  if (!platform_id %in% ILLUMINA_PROBE_QUALITY_PLATFORMS) return(expr_mat)
  db_name  <- paste0(platform_id, ".db")
  map_name <- paste0(platform_id, "PROBEQUALITY")

  # Ensure local R-lib directory is included in library paths
  local_lib <- if (exists("get_local_r_lib")) get_local_r_lib() else file.path(getwd(), "R-lib")
  bundled_lib <- file.path(getwd(), "R-lib")
  search_libs <- unique(c(local_lib, bundled_lib, .libPaths()))
  search_libs <- search_libs[dir.exists(search_libs)]

  suppressPackageStartupMessages(
    tryCatch(library(db_name, character.only = TRUE, lib.loc = search_libs), error = function(e) {
      tryCatch(library(db_name, character.only = TRUE), error = function(e2) NULL)
    })
  )
  if (!exists(map_name)) {
    cat(sprintf("[PROBE FILTER] '%s' not found — skipping quality filter.\n", map_name))
    return(expr_mat)
  }
  ids       <- rownames(expr_mat)
  qual_list <- suppressWarnings(AnnotationDbi::mget(ids, get(map_name), ifnotfound = NA))
  drop_q    <- c("Bad", "No match")
  keep <- vapply(qual_list, function(x) {
    x <- as.character(x)
    if (all(is.na(x))) return(FALSE)
    !any(x %in% drop_q)
  }, logical(1))
  cat(sprintf("[PROBE FILTER] %s: %d -> %d probes (%d Bad/No match removed).\n",
              platform_id, length(ids), sum(keep), sum(!keep)))
  expr_mat[keep, , drop = FALSE]
}

filter_by_detection_pval <- function(expr_mat, det_mat, threshold = 0.05, proportion = 0.5) {
  if (is.null(det_mat) || is.null(expr_mat)) return(expr_mat)
  probes <- intersect(rownames(expr_mat), rownames(det_mat))
  if (!length(probes)) return(expr_mat)

  expr_cols <- colnames(expr_mat)
  det_cols  <- colnames(det_mat)

  # Match sample columns between expr_mat and det_mat without mutating colnames(expr_mat)
  common_exact <- intersect(expr_cols, det_cols)
  if (length(common_exact) > 0) {
    det <- det_mat[probes, common_exact, drop = FALSE]
  } else {
    # Match by stripping common suffixes (e.g. .AVG_Signal vs .Detection Pval)
    clean_expr <- sub("(?i)[._ -]*(avg[._ -]*signal|signal|expr).*$", "", expr_cols)
    clean_det  <- sub("(?i)[._ -]*(detection[._ -]*pval|det[._ -]*pval|pval).*$", "", det_cols)

    m_idx <- match(clean_expr, clean_det)
    valid_m <- which(!is.na(m_idx))
    if (length(valid_m) > 0) {
      det <- det_mat[probes, m_idx[valid_m], drop = FALSE]
    } else if (ncol(det_mat) == ncol(expr_mat)) {
      # Fallback to column index order if column counts match
      det <- det_mat[probes, , drop = FALSE]
    } else {
      cat("[DETECTION FILTER] Could not match detection columns to sample columns — skipping detection filter.\n")
      return(expr_mat)
    }
  }

  detected <- det < threshold
  detected[is.na(detected)] <- FALSE
  n_samples <- ncol(det)
  if (n_samples == 0) return(expr_mat)

  frac_det <- rowSums(detected) / n_samples
  keep <- frac_det >= proportion
  cat(sprintf("[DETECTION FILTER] %d -> %d probes (Pval<%.3g in >=%.0f%% of %d samples; %d removed).\n",
              length(probes), sum(keep), threshold, proportion * 100, n_samples, sum(!keep)))

  expr_mat[probes[keep], , drop = FALSE]
}

apply_illumina_probe_filters <- function(expr_mat, ds_id, gene_id_type = NULL, data_type = "microarray") {
  if (is.null(expr_mat) || nrow(expr_mat) == 0 || ncol(expr_mat) == 0) return(expr_mat)
  if (!identical(tolower(data_type %||% ""), "microarray")) return(expr_mat)

  # Resolve platform ID
  if (is.null(gene_id_type) || !nzchar(gene_id_type)) {
    meta_path <- if (exists("get_session_path")) get_session_path(ds_id, "%s_expr_metadata.rds") else sprintf("tmp/%s_expr_metadata.rds", ds_id)
    if (file.exists(meta_path)) {
      meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
      gene_id_type <- meta$geneIdType %||% meta$platform %||% ""
    }
  }

  clean_platform <- gene_id_type
  if (!is.null(clean_platform) && !clean_platform %in% ILLUMINA_PROBE_QUALITY_PLATFORMS) {
    clean_id <- sub("\\.db$", "", clean_platform)
    if (clean_id %in% ILLUMINA_PROBE_QUALITY_PLATFORMS) {
      clean_platform <- clean_id
    } else {
      platform_map <- c(
        "HumanHT-12 v3" = "illuminaHumanv3",
        "HumanHT-12 v4" = "illuminaHumanv4",
        "MouseRef-8 v2" = "illuminaMousev2",
        "RatRef-12"     = "illuminaRatv1"
      )
      if (clean_platform %in% names(platform_map)) {
        clean_platform <- platform_map[[clean_platform]]
      }
    }
  }

  if (is.null(clean_platform) || !clean_platform %in% ILLUMINA_PROBE_QUALITY_PLATFORMS) {
    return(expr_mat)
  }

  # 1. Quality filter (Bad / No match)
  expr_mat <- tryCatch(
    filter_probes_by_quality(expr_mat, clean_platform),
    error = function(e) { cat("[WARN] Probe quality filter failed:", conditionMessage(e), "\n"); expr_mat }
  )

  # 2. Detection P-value filter
  det_path <- if (exists("get_session_path")) get_session_path(ds_id, "%s_detection_pval.rds") else sprintf("tmp/%s_detection_pval.rds", ds_id)
  if (!file.exists(det_path)) {
    det_path <- sprintf("tmp/%s_detection_pval.rds", ds_id)
  }
  if (file.exists(det_path)) {
    det_mat <- tryCatch(readRDS(det_path), error = function(e) NULL)
    if (!is.null(det_mat)) {
      expr_mat <- tryCatch(
        filter_by_detection_pval(expr_mat, det_mat),
        error = function(e) { cat("[WARN] Detection P-val filter failed:", conditionMessage(e), "\n"); expr_mat }
      )
    }
  }

  return(expr_mat)
}


align_and_filter_dataset <- function(ds_id, clin_meta, clin_parsed, clin_cols) {
  # 1. Clean clinical data (remove missing values in Sample ID Column, Batch Column, Group Column)
  clin_sample_id_col <- clin_meta$sampleIdCol
  clin_group_col     <- clin_meta$groupCol
  clin_batch_col     <- clin_meta$batchCol

  clin_sample_idx <- if (!is.null(clin_sample_id_col) && clin_sample_id_col != "") which(clin_cols == clin_sample_id_col) else integer(0)
  clin_group_idx  <- if (!is.null(clin_group_col) && clin_group_col != "") which(clin_cols == clin_group_col) else integer(0)
  clin_batch_idx  <- if (!is.null(clin_batch_col) && clin_batch_col != "") which(clin_cols == clin_batch_col) else integer(0)

  indices_to_check <- c(clin_sample_idx, clin_group_idx, clin_batch_idx)
  indices_to_check <- indices_to_check[indices_to_check > 0 & indices_to_check <= length(clin_cols)]

  is_val_missing <- function(val) {
    if (is.null(val)) return(TRUE)
    if (length(val) == 0) return(TRUE)
    if (is.list(val)) {
      val <- val[[1]]
    }
    if (is.null(val) || is.na(val)) return(TRUE)
    val_clean <- as.character(val)
    if (val_clean == "NA" || val_clean == "NaN" || trimws(val_clean) == "") return(TRUE)
    return(FALSE)
  }

  cleaned_clin_parsed <- list()
  valid_clin_samples <- c()
  
  if (length(clin_parsed) > 0 && length(clin_cols) > 0) {
    n_rows <- if (is.matrix(clin_parsed)) nrow(clin_parsed) else length(clin_parsed)
    for (i in 1:n_rows) {
      row_vals <- if (is.matrix(clin_parsed)) clin_parsed[i, ] else clin_parsed[[i]]
      
      is_missing_row <- FALSE
      for (idx in indices_to_check) {
        if (length(row_vals) >= idx) {
          if (is_val_missing(row_vals[[idx]])) {
            is_missing_row <- TRUE
            break
          }
        } else {
          is_missing_row <- TRUE
          break
        }
      }
      
      samp_id <- NULL
      if (length(clin_sample_idx) > 0 && length(row_vals) >= clin_sample_idx) {
        samp_id <- row_vals[[clin_sample_idx]]
      }
      
      if (!is_missing_row) {
        if (!is.null(samp_id) && !is.na(samp_id) && as.character(samp_id) != "") {
          cleaned_clin_parsed[[length(cleaned_clin_parsed) + 1]] <- row_vals
          valid_clin_samples <- c(valid_clin_samples, as.character(samp_id))
        }
      }
    }
  }

  # 2. Get the expression samples from files/stacks.
  # Let's check if the expression file exists
  expr_file_path <- get_session_path(ds_id, "%s_expression.csv", fallback = TRUE)
  if (file.exists(expr_file_path)) {
    expr_df <- read_csv_preserve_id(expr_file_path)
    if (is.na(colnames(expr_df)[1]) || !nzchar(trimws(colnames(expr_df)[1]))) {
      colnames(expr_df)[1] <- "GeneID"
    }
    gene_id_col <- colnames(expr_df)[1]
    
    meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds", fallback = TRUE)
    gene_info_cols <- character(0)
    if (file.exists(meta_path)) {
      meta <- readRDS(meta_path)
      gene_info_cols <- if (!is.null(meta$geneInfoCols)) unlist(meta$geneInfoCols) else character(0)
    }
    gene_info_cols <- setdiff(as.character(gene_info_cols), gene_id_col)
    gene_info_cols <- gene_info_cols[nzchar(trimws(gene_info_cols))]
    
    expr_samples <- setdiff(colnames(expr_df), c(gene_id_col, gene_info_cols, "entrez_id", "gene_symbol", "gene_biotype"))
    
    # 3. Intersect
    common_samples <- intersect(expr_samples, valid_clin_samples)

    # GUARD: if expression and clinical share NO samples (e.g. differently-formatted
    # sample IDs like "Sample_1" vs "S1"), do NOT overwrite the expression matrix/stacks
    # with 0 columns — that would silently and irreversibly destroy the dataset.
    # Skip all destructive filtering and return the cleaned clinical rows untouched.
    if (length(common_samples) == 0) {
      cat(sprintf(
        "[ALIGN] WARNING: dataset %s has NO overlapping samples between expression (%d) and clinical (%d); skipping alignment/filtering to avoid data loss. Check that sample IDs match.\n",
        ds_id, length(expr_samples), length(valid_clin_samples)
      ))
      return(cleaned_clin_parsed)
    }

    # 4. Filter expression CSV
    keep_cols <- unique(c(gene_id_col, gene_info_cols, common_samples))
    keep_cols <- keep_cols[nzchar(trimws(keep_cols)) & keep_cols %in% colnames(expr_df)]
    expr_df_filtered <- expr_df[, keep_cols, drop = FALSE]
    write.csv(expr_df_filtered, file = expr_file_path, row.names = FALSE)
    
    # 5. Filter main stack
    main_stack_file <- get_session_path(ds_id, "%s_main_stack.rds", fallback = TRUE)
    if (file.exists(main_stack_file)) {
      stack <- tryCatch(readRDS(main_stack_file), error = function(e) list())
      if (is.list(stack) && length(stack) > 0) {
        for (j in seq_along(stack)) {
          m <- stack[[j]]$data
          if (is.matrix(m) || is.data.frame(m)) {
            common_cols <- colnames(m)[colnames(m) %in% common_samples]
            stack[[j]]$data <- m[, common_cols, drop = FALSE]
          }
        }
        saveRDS(stack, main_stack_file)
      }
    }

    # 6. Filter counts stack
    counts_stack_file <- get_session_path(ds_id, "%s_counts_stack.rds", fallback = TRUE)
    if (file.exists(counts_stack_file)) {
      stack <- tryCatch(readRDS(counts_stack_file), error = function(e) list())
      if (is.list(stack) && length(stack) > 0) {
        for (j in seq_along(stack)) {
          m <- stack[[j]]$data
          if (is.matrix(m) || is.data.frame(m)) {
            common_cols <- colnames(m)[colnames(m) %in% common_samples]
            stack[[j]]$data <- m[, common_cols, drop = FALSE]
          }
        }
        saveRDS(stack, counts_stack_file)
      }
    }

    # 7. Also filter other individual RDS files if they exist
    rds_suffixes <- c(
      "tmp/%s_expr_matrix.rds",
      "tmp/%s_expr_matrix_annotated.rds",
      "tmp/%s_processed_expr.rds",
      "tmp/%s_normalized_expr.rds",
      "tmp/%s_normalized_expr_actual.rds",
      "tmp/%s_batch_expr.rds",
      "tmp/%s_batch_expr_actual.rds",
      "tmp/%s_non_normalized_expr.rds",
      "tmp/%s_annotated_raw_counts.rds",
      "tmp/%s_processed_raw_counts.rds",
      "tmp/%s_batch_raw_counts.rds"
    )
    for (suffix in rds_suffixes) {
      fpath <- sprintf(suffix, ds_id)
      if (file.exists(fpath)) {
        m <- tryCatch(readRDS(fpath), error = function(e) NULL)
        if (!is.null(m) && (is.matrix(m) || is.data.frame(m))) {
          common_cols <- colnames(m)[colnames(m) %in% common_samples]
          m_filtered <- m[, common_cols, drop = FALSE]
          saveRDS(m_filtered, fpath)
        }
      }
    }
    
    # 8. Filter clinical data to common_samples
    final_clin_parsed <- list()
    if (length(cleaned_clin_parsed) > 0 && length(clin_sample_idx) > 0) {
      for (row in cleaned_clin_parsed) {
        samp_id <- row[[clin_sample_idx]]
        if (as.character(samp_id) %in% common_samples) {
          final_clin_parsed[[length(final_clin_parsed) + 1]] <- row
        }
      }
    } else {
      final_clin_parsed <- cleaned_clin_parsed
    }
    
    return(final_clin_parsed)
  } else {
    return(cleaned_clin_parsed)
  }
}



# -------------------------------------------------------------------------
# LIFO Stack Architecture: Main Workflow Stack & Counts Stack
# -------------------------------------------------------------------------

push_main_stack <- function(ds_id, data, step_name, metadata = list()) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_main_stack.rds")
  stack <- if (file.exists(stack_file) && step_name != "upload") tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  
  entry <- list(
    step = step_name,
    data = data,
    metadata = metadata,
    timestamp = Sys.time()
  )
  stack[[length(stack) + 1]] <- entry
  saveRDS(stack, stack_file, compress = FALSE)
  
  uid <- get_user_id(base_id)
  ds_info <- get_backend_datasets(base_id)
  ds_module <- ds_info$module %||% "dp"
  ds_parent_module <- ds_info$parentModule %||% ds_module
  ds_is_inline <- isTRUE(ds_info$isInline)

  if (step_name == "upload") {
    p_main <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_main, compress = FALSE)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, "upload", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
  } else if (step_name == "annotation") {
    p_anno <- get_session_path(base_id, "%s_expr_matrix_annotated.rds")
    p_main <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_anno, compress = FALSE)
    saveRDS(data, p_main, compress = FALSE)
    annotated_csv_path <- get_session_path(base_id, "%s_annotated_matrix.csv")
    tryCatch(fast_write_csv(get_formatted_matrix_df(base_id, data), annotated_csv_path, row.names = FALSE), error = function(e) NULL)
    register_export_file(uid, "annotated_matrix", base_id, p_anno, ds_module, "annotation", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, "annotation", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)

    anno_res_rds <- get_session_path(base_id, "%s_annotation_results.rds")
    if (file.exists(anno_res_rds)) {
      register_export_file(uid, "annotation_results", base_id, anno_res_rds, ds_module, "annotation", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    }
    unmapped_res_rds <- get_session_path(base_id, "%s_unmapped_results.rds")
    if (file.exists(unmapped_res_rds)) {
      register_export_file(uid, "unmapped_results", base_id, unmapped_res_rds, ds_module, "annotation", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    }
  } else if (step_name == "processing") {
    p_proc <- get_session_path(base_id, "%s_processed_expr.rds")
    p_main <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_proc, compress = FALSE)
    saveRDS(data, p_main, compress = FALSE)
    processed_csv_path <- get_session_path(base_id, "%s_processed_matrix.csv")
    tryCatch(fast_write_csv(get_formatted_matrix_df(base_id, data), processed_csv_path, row.names = FALSE), error = function(e) NULL)
    register_export_file(uid, "processed_matrix", base_id, p_proc, ds_module, "processing", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, "processing", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
  } else if (step_name == "normalization") {
    p_norm <- get_session_path(base_id, "%s_normalized_expr.rds")
    p_main <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_norm, compress = FALSE)
    saveRDS(data, get_session_path(base_id, "%s_normalized_expr_actual.rds"), compress = FALSE)
    saveRDS(data, p_main, compress = FALSE)
    normalized_csv_path <- get_session_path(base_id, "%s_normalized_matrix.csv")
    tryCatch(fast_write_csv(get_formatted_matrix_df(base_id, data), normalized_csv_path, row.names = FALSE), error = function(e) NULL)
    register_export_file(uid, "normalized_matrix", base_id, p_norm, ds_module, "normalization", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, "normalization", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
  } else if (step_name == "batch") {
    p_batch <- get_session_path(base_id, "%s_batch_expr.rds")
    p_main  <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_batch, compress = FALSE)
    saveRDS(data, get_session_path(base_id, "%s_batch_expr_actual.rds"), compress = FALSE)
    saveRDS(data, p_main, compress = FALSE)
    batch_csv_path <- get_session_path(base_id, "%s_batch_corrected_matrix.csv")
    tryCatch(fast_write_csv(get_formatted_matrix_df(base_id, data), batch_csv_path, row.names = FALSE), error = function(e) NULL)
    register_export_file(uid, "batch_corrected_matrix", base_id, p_batch, ds_module, "batch", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, "batch", ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
  } else {
    p_main <- get_session_path(base_id, "%s_expr_matrix.rds")
    saveRDS(data, p_main, compress = FALSE)
    register_export_file(uid, "final_matrix", base_id, p_main, ds_module, step_name, ext = "csv", parentModule = ds_parent_module, isInline = ds_is_inline)
  }
  return(entry)
}

pop_main_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_main_stack.rds")
  if (!file.exists(stack_file)) return(NULL)
  stack <- readRDS(stack_file)
  if (length(stack) == 0) return(NULL)
  top_entry <- stack[[length(stack)]]
  stack <- stack[-length(stack)]
  saveRDS(stack, stack_file)
  return(top_entry)
}

get_main_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_main_stack.rds")
  if (!file.exists(stack_file)) return(list())
  stack <- tryCatch(readRDS(stack_file), error = function(e) list())
  if (!is.list(stack)) stack <- list()
  return(stack)
}

get_latest_main_stack <- function(ds_id) {
  stack <- get_main_stack(ds_id)
  if (length(stack) == 0) {
    cat(sprintf("[TRACK] get_latest_main_stack: ds_id = %s, stack = main_stack, step = NULL (empty stack)\n", ds_id))
    return(NULL)
  }
  entry <- stack[[length(stack)]]
  if (!is.null(entry) && !is.null(entry$data) && !is.null(rownames(entry$data))) {
    valid_idx <- !is.na(rownames(entry$data)) & rownames(entry$data) != "" & rownames(entry$data) != "NA" & rownames(entry$data) != "NaN"
    if (any(!valid_idx)) {
      entry$data <- entry$data[valid_idx, , drop = FALSE]
    }
  }
  data_info <- if (is.null(dim(entry$data))) paste("length =", length(entry$data)) else paste(dim(entry$data), collapse = "x")
  cat(sprintf("[TRACK] get_latest_main_stack: ds_id = %s, stack = main_stack, step = %s, data dims = %s\n", 
              ds_id, entry$step, data_info))
  return(entry)
}

push_counts_stack <- function(ds_id, count_data, step_name, metadata = list()) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_counts_stack.rds")
  stack <- if (file.exists(stack_file) && step_name != "upload") tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  
  entry <- list(
    step = step_name,
    data = count_data,
    metadata = metadata,
    timestamp = Sys.time()
  )
  stack[[length(stack) + 1]] <- entry
  saveRDS(stack, stack_file, compress = FALSE)
  
  if (step_name == "upload") {
    saveRDS(count_data, get_session_path(base_id, "%s_non_normalized_expr.rds"), compress = FALSE)
  } else if (step_name == "annotation") {
    saveRDS(count_data, get_session_path(base_id, "%s_annotated_raw_counts.rds"), compress = FALSE)
    saveRDS(count_data, get_session_path(base_id, "%s_non_normalized_expr.rds"), compress = FALSE)
  } else if (step_name == "processing") {
    saveRDS(count_data, get_session_path(base_id, "%s_processed_raw_counts.rds"), compress = FALSE)
    saveRDS(count_data, get_session_path(base_id, "%s_non_normalized_expr.rds"), compress = FALSE)
  } else if (step_name == "batch") {
    saveRDS(count_data, get_session_path(base_id, "%s_batch_raw_counts.rds"), compress = FALSE)
    saveRDS(count_data, get_session_path(base_id, "%s_non_normalized_expr.rds"), compress = FALSE)
  }
  return(entry)
}

pop_counts_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_counts_stack.rds")
  if (!file.exists(stack_file)) return(NULL)
  stack <- readRDS(stack_file)
  if (length(stack) == 0) return(NULL)
  top_entry <- stack[[length(stack)]]
  stack <- stack[-length(stack)]
  saveRDS(stack, stack_file, compress = FALSE)
  return(top_entry)
}

get_counts_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_counts_stack.rds")
  if (!file.exists(stack_file)) return(list())
  stack <- tryCatch(readRDS(stack_file), error = function(e) list())
  if (!is.list(stack)) stack <- list()
  return(stack)
}

get_latest_counts_stack <- function(ds_id) {
  stack <- get_counts_stack(ds_id)
  if (length(stack) == 0) return(NULL)
  return(stack[[length(stack)]])
}

truncate_stack_to_step <- function(ds_id, step_name) {
  truncate_stacks_to_step(ds_id, step_name, is_redo = TRUE)
}

push_de_stack <- function(ds_id, data, step_name = "de", metadata = list()) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_de_stack.rds")
  stack <- if (file.exists(stack_file) && step_name != "upload") tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  
  entry <- list(
    step = step_name,
    data = data,
    metadata = metadata,
    timestamp = Sys.time()
  )
  stack[[length(stack) + 1]] <- entry
  saveRDS(stack, stack_file, compress = FALSE)
  return(entry)
}

get_latest_de_stack <- function(ds_id, step_name = NULL, expr_only = FALSE) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_de_stack.rds")
  if (!file.exists(stack_file)) return(NULL)
  stack <- tryCatch(readRDS(stack_file), error = function(e) list())
  if (length(stack) == 0) return(NULL)
  
  if (!is.null(step_name)) {
    for (i in seq(length(stack), 1, by = -1)) {
      if (identical(stack[[i]]$step, step_name)) {
        return(stack[[i]])
      }
    }
    return(NULL)
  }
  if (expr_only) {
    for (i in seq(length(stack), 1, by = -1)) {
      if (!identical(stack[[i]]$step, "meta")) {
        return(stack[[i]])
      }
    }
    return(NULL)
  }
  return(stack[[length(stack)]])
}

push_inline_de_main_stack <- function(ds_id, data, step_name = "de", metadata = list()) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_inline_de_main_stack.rds")
  stack <- if (file.exists(stack_file)) tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  entry <- list(step = step_name, data = data, metadata = metadata, timestamp = Sys.time())
  stack[[length(stack) + 1]] <- entry
  saveRDS(stack, stack_file)
  return(entry)
}

push_inline_de_counts_stack <- function(ds_id, data, step_name = "de", metadata = list()) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_inline_de_counts_stack.rds")
  stack <- if (file.exists(stack_file)) tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  entry <- list(step = step_name, data = data, metadata = metadata, timestamp = Sys.time())
  stack[[length(stack) + 1]] <- entry
  saveRDS(stack, stack_file)
  return(entry)
}

get_latest_inline_de_main_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_inline_de_main_stack.rds")
  if (!file.exists(stack_file)) return(NULL)
  stack <- tryCatch(readRDS(stack_file), error = function(e) list())
  if (length(stack) == 0) return(NULL)
  entry <- stack[[length(stack)]]
  if (!is.null(entry) && !is.null(entry$data) && !is.null(rownames(entry$data))) {
    valid_idx <- !is.na(rownames(entry$data)) & rownames(entry$data) != "" & rownames(entry$data) != "NA" & rownames(entry$data) != "NaN"
    if (any(!valid_idx)) {
      entry$data <- entry$data[valid_idx, , drop = FALSE]
    }
  }
  return(entry)
}

get_latest_inline_de_counts_stack <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack_file <- get_session_path(base_id, "%s_inline_de_counts_stack.rds")
  if (!file.exists(stack_file)) return(NULL)
  stack <- tryCatch(readRDS(stack_file), error = function(e) list())
  if (length(stack) == 0) return(NULL)
  entry <- stack[[length(stack)]]
  if (!is.null(entry) && !is.null(entry$data) && !is.null(rownames(entry$data))) {
    valid_idx <- !is.na(rownames(entry$data)) & rownames(entry$data) != "" & rownames(entry$data) != "NA" & rownames(entry$data) != "NaN"
    if (any(!valid_idx)) {
      entry$data <- entry$data[valid_idx, , drop = FALSE]
    }
  }
  return(entry)
}

get_latest_normalized_matrix <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  stack <- get_main_stack(base_id)
  
  # 1. Search LIFO (top to bottom) for a normalized matrix entry in main stack
  if (length(stack) > 0) {
    for (i in seq(length(stack), 1, by = -1)) {
      step_i <- stack[[i]]$step
      meta_i <- stack[[i]]$metadata
      if (step_i == "normalization" || (step_i == "batch" && isTRUE(meta_i$reNormalized)) || isTRUE(meta_i$isNormalized)) {
        data_info <- if (is.null(dim(stack[[i]]$data))) paste("length =", length(stack[[i]]$data)) else paste(dim(stack[[i]]$data), collapse = "x")
        cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, retrieved from main_stack, computed at step: %s, data dims = %s\n", base_id, step_i, data_info))
        return(stack[[i]]$data)
      }
    }
  }
  
  # 2. Check explicit normalized RDS files
  norm_actual <- get_session_path(base_id, "%s_normalized_expr_actual.rds")
  if (file.exists(norm_actual)) {
    mat <- readRDS(norm_actual)
    data_info <- if (is.null(dim(mat))) paste("length =", length(mat)) else paste(dim(mat), collapse = "x")
    cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, retrieved normalized_expr_actual.rds, step = normalization/batch, data dims = %s\n", base_id, data_info))
    return(mat)
  }
  norm_expr <- get_session_path(base_id, "%s_normalized_expr.rds")
  norm_cfg  <- get_session_path(base_id, "%s_norm_config.rds")
  if (file.exists(norm_expr) && file.exists(norm_cfg)) {
    mat <- readRDS(norm_expr)
    data_info <- if (is.null(dim(mat))) paste("length =", length(mat)) else paste(dim(mat), collapse = "x")
    cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, retrieved normalized_expr.rds, step = normalization, data dims = %s\n", base_id, data_info))
    return(mat)
  }
  
  # 3. For raw readcounts with no normalized data: use latest count data from counts stack
  meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
  meta_info <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL
  dtype <- if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts"
  is_norm <- if (!is.null(meta_info$isNormalized)) isTRUE(meta_info$isNormalized) else FALSE
  
  if (dtype == "readcounts" && !is_norm) {
    cat(sprintf("[INLINE-FS] No normalized data found for raw readcounts '%s'. Using latest count data from counts stack.\n", base_id))
    latest_counts <- get_latest_count_matrix(base_id)
    if (!is.null(latest_counts)) {
      data_info <- if (is.null(dim(latest_counts))) paste("length =", length(latest_counts)) else paste(dim(latest_counts), collapse = "x")
      cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, fell back to latest count matrix from counts_stack, data dims = %s\n", base_id, data_info))
      return(latest_counts)
    }
  }
  
  # 4. Fallback to latest matrix from main stack
  if (length(stack) > 0) {
    mat <- stack[[length(stack)]]$data
    data_info <- if (is.null(dim(mat))) paste("length =", length(mat)) else paste(dim(mat), collapse = "x")
    cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, retrieved top main_stack entry, computed at step: %s, data dims = %s\n", base_id, stack[[length(stack)]]$step, data_info))
    return(mat)
  }
  
  # 5. Last fallback
  mat <- get_latest_step_matrix(base_id, "fs")
  data_info <- if (is.null(dim(mat))) paste("length =", length(mat)) else paste(dim(mat), collapse = "x")
  cat(sprintf("[TRACK] get_latest_normalized_matrix: ds_id = %s, retrieved via get_latest_step_matrix (fs), data dims = %s\n", base_id, data_info))
  return(mat)
}

get_latest_count_matrix <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  latest_c <- get_latest_counts_stack(base_id)
  if (!is.null(latest_c) && !is.null(latest_c$data)) {
    data_info <- if (is.null(dim(latest_c$data))) paste("length =", length(latest_c$data)) else paste(dim(latest_c$data), collapse = "x")
    cat(sprintf("[TRACK] get_latest_count_matrix: ds_id = %s, retrieved from counts_stack, computed at step: %s, data dims = %s\n", base_id, latest_c$step, data_info))
    return(latest_c$data)
  }
  
  # Fallback to get_latest_raw_count_matrix
  raw_mat <- get_latest_raw_count_matrix(base_id)
  data_info <- if (is.null(dim(raw_mat))) paste("length =", length(raw_mat)) else paste(dim(raw_mat), collapse = "x")
  cat(sprintf("[TRACK] get_latest_count_matrix: ds_id = %s, retrieved raw/fallback count matrix, step = upload/annotation, data dims = %s\n", base_id, data_info))
  return(raw_mat)
}



get_latest_step_matrix <- function(ds_id, current_step) {
  base_id <- get_base_id(ds_id)
  
  mat <- NULL
  latest_main <- get_latest_main_stack(base_id)
  if (!is.null(latest_main) && !is.null(latest_main$data)) {
    mat <- latest_main$data
  } else {
    batch_expr <- get_session_path(base_id, "%s_batch_expr.rds")
    if (!file.exists(batch_expr)) batch_expr <- get_session_path(base_id, "%s_batch_batch_expr.rds")
    if (file.exists(batch_expr)) {
      mat <- readRDS(batch_expr)
    } else {
      norm_expr <- get_session_path(base_id, "%s_normalized_expr.rds")
      if (!file.exists(norm_expr)) norm_expr <- get_session_path(base_id, "%s_normalization_normalized_expr.rds")
      if (file.exists(norm_expr)) {
        mat <- readRDS(norm_expr)
      } else {
        proc_expr <- get_session_path(base_id, "%s_processed_expr.rds")
        if (!file.exists(proc_expr)) proc_expr <- get_session_path(base_id, "%s_processing_processed_expr.rds")
        if (file.exists(proc_expr)) {
          mat <- readRDS(proc_expr)
        } else {
          anno_expr <- get_session_path(base_id, "%s_expr_matrix_annotated.rds")
          if (!file.exists(anno_expr)) anno_expr <- get_session_path(base_id, "%s_annotation_expr_matrix_annotated.rds")
          if (file.exists(anno_expr)) {
            mat <- readRDS(anno_expr)
          } else {
            upload_expr <- get_session_path(base_id, "%s_expr_matrix.rds")
            if (!file.exists(upload_expr)) upload_expr <- get_session_path(base_id, "%s_upload_expr_matrix.rds")
            if (file.exists(upload_expr)) {
              mat <- readRDS(upload_expr)
            }
          }
        }
      }
    }
  }
  
  if (!is.null(mat) && !is.null(rownames(mat))) {
    valid_idx <- !is.na(rownames(mat)) & rownames(mat) != "" & rownames(mat) != "NA" & rownames(mat) != "NaN"
    if (any(!valid_idx)) {
      mat <- mat[valid_idx, , drop = FALSE]
    }
  }
  return(mat)
}

get_latest_raw_count_matrix <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  
  mat <- NULL
  latest_count <- get_latest_counts_stack(base_id)
  if (!is.null(latest_count) && !is.null(latest_count$data)) {
    mat <- latest_count$data
  } else {
    batch_raw <- get_session_path(base_id, "%s_batch_raw_counts.rds")
    if (!file.exists(batch_raw)) batch_raw <- get_session_path(base_id, "%s_batch_expr_actual.rds")
    if (file.exists(batch_raw)) {
      mat <- readRDS(batch_raw)
    } else {
      proc_raw <- get_session_path(base_id, "%s_processed_raw_counts.rds")
      if (file.exists(proc_raw)) {
        mat <- readRDS(proc_raw)
      } else {
        anno_raw <- get_session_path(base_id, "%s_annotated_raw_counts.rds")
        if (file.exists(anno_raw)) {
          mat <- readRDS(anno_raw)
        } else {
          mat <- get_non_normalized_expr_matrix(base_id)
        }
      }
    }
  }
  
  if (!is.null(mat) && !is.null(rownames(mat))) {
    valid_idx <- !is.na(rownames(mat)) & rownames(mat) != "" & rownames(mat) != "NA" & rownames(mat) != "NaN"
    if (any(!valid_idx)) {
      mat <- mat[valid_idx, , drop = FALSE]
    }
  }
  return(mat)
}

get_non_normalized_expr_matrix <- function(ds_id) {
  non_norm_expr <- get_session_path(ds_id, "%s_upload_non_normalized_expr.rds")
  if (!file.exists(non_norm_expr)) non_norm_expr <- get_session_path(ds_id, "%s_non_normalized_expr.rds")
  if (file.exists(non_norm_expr)) {
    return(readRDS(non_norm_expr))
  }
  
  upload_expr <- get_session_path(ds_id, "%s_upload_expr_matrix.rds")
  if (!file.exists(upload_expr)) upload_expr <- get_session_path(ds_id, "%s_expr_matrix.rds")
  if (file.exists(upload_expr)) {
    return(readRDS(upload_expr))
  }
  return(NULL)
}

apply_normalization_to_matrix <- function(expr, d_method = "tmm", d_transform_type = "log2", d_prior_count = 0.5, d_data_type = "readcounts") {
  if (is.null(expr)) return(NULL)
  
  # Backward compatibility for boolean transform parameters
  if (is.logical(d_transform_type)) {
    d_transform_type <- if (isTRUE(d_transform_type)) "log2" else "none"
  } else if (is.null(d_transform_type) || d_transform_type == "" || d_transform_type == "FALSE") {
    d_transform_type <- "none"
  } else if (d_transform_type == "TRUE") {
    d_transform_type <- "log2"
  }

  # Methods with built-in transformation or that should not be log-transformed
  if (tolower(d_method) %in% c("vst", "rlog", "vsn")) {
    d_transform_type <- "none"
    d_prior_count <- 0
  }
  
  norm_expr <- expr
  if (d_method == "tmm") {
    dge <- edgeR::DGEList(counts = expr)
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
    if (d_transform_type == "log2") {
      norm_expr <- edgeR::cpm(dge, log = TRUE, prior.count = d_prior_count)
    } else if (d_transform_type == "log10") {
      norm_cpm <- edgeR::cpm(dge, log = FALSE)
      norm_expr <- log10(norm_cpm + d_prior_count)
    } else {
      norm_expr <- edgeR::cpm(dge, log = FALSE)
    }
  } else if (d_method == "cpm") {
    libsizes <- colSums(expr)
    norm_expr <- t(t(expr) / libsizes) * 1e6
    if (d_transform_type == "log2") {
      norm_expr <- log2(norm_expr + d_prior_count)
    } else if (d_transform_type == "log10") {
      norm_expr <- log10(norm_expr + d_prior_count)
    }
  } else if (d_method == "quantile") {
    # Apply log transformation BEFORE quantile normalization to match Microarray_check.R and prevent skewing the distribution.
    expr_log <- expr
    if (d_transform_type == "log2") {
      expr_log <- log2(expr_log + d_prior_count)
    } else if (d_transform_type == "log10") {
      expr_log <- log10(expr_log + d_prior_count)
    }
    
    norm_expr <- tryCatch({
      if (tolower(d_data_type) == "microarray") {
        limma::normalizeBetweenArrays(as.matrix(expr_log), method = "quantile")
      } else {
        res <- preprocessCore::normalize.quantiles(as.matrix(expr_log))
        rownames(res) <- rownames(expr_log)
        colnames(res) <- colnames(expr_log)
        res
      }
    }, error = function(e) expr_log)
  } else if (d_method == "vsn") {
    norm_expr <- tryCatch({
      fit <- vsn::vsn2(as.matrix(expr))
      vsn::predict(fit, as.matrix(expr))
    }, error = function(e) {
      if (d_transform_type == "log10") {
        log10(expr + d_prior_count)
      } else {
        log2(expr + d_prior_count)
      }
    })
  } else if (d_method == "vst") {
    norm_expr <- tryCatch({
      coldata <- data.frame(sample_id = colnames(expr), row.names = colnames(expr))
      dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = round(as.matrix(expr)),
        colData   = coldata,
        design    = ~ 1
      )
      tryCatch(
        SummarizedExperiment::assay(DESeq2::vst(dds, blind = TRUE)),
        error = function(e) {
          if (grepl("nsub|less than", conditionMessage(e), ignore.case = TRUE)) {
            SummarizedExperiment::assay(
              DESeq2::varianceStabilizingTransformation(dds, blind = TRUE)
            )
          } else {
            stop(e)
          }
        }
      )
    }, error = function(e) {
      if (d_transform_type == "log10") {
        log10(expr + d_prior_count)
      } else {
        log2(expr + d_prior_count)
      }
    })
  } else if (d_method == "rlog") {
    norm_expr <- tryCatch({
      DESeq2::rlog(round(as.matrix(expr)))
    }, error = function(e) {
      if (d_transform_type == "log10") {
        log10(expr + d_prior_count)
      } else {
        log2(expr + d_prior_count)
      }
    })
  } else if (d_method == "none" || d_method == "skip") {
    if (d_transform_type == "log2") {
      norm_expr <- log2(expr + d_prior_count)
    } else if (d_transform_type == "log10") {
      norm_expr <- log10(expr + d_prior_count)
    }
  }
  return(norm_expr)
}

# Helper to parse expression payload into a proper numeric matrix and metadata
parse_expression_data <- function(parsed_data, columns, gene_id_col, gene_info_cols = NULL) {
  if (is.null(parsed_data) || length(parsed_data) == 0) return(NULL)
  if (is.null(columns) || length(columns) == 0) return(NULL)

  # Convert list of rows to data frame
  if (is.matrix(parsed_data)) {
    df <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
  } else {
    max_len <- max(sapply(parsed_data, length))
    rows <- lapply(parsed_data, function(x) {
      if (length(x) < max_len) c(x, rep(NA, max_len - length(x))) else x
    })
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  }

  columns <- as.character(columns)
  n_cols <- ncol(df)
  if (length(columns) != n_cols) {
    if (length(columns) > n_cols) {
      columns <- columns[1:n_cols]
    } else {
      extra <- paste0("Column_", seq(length(columns) + 1, n_cols))
      columns <- c(columns, extra)
    }
  }
  colnames(df) <- as.character(columns)
  
  gene_id_col <- as.character(gene_id_col %||% "")
  if (!(gene_id_col %in% columns)) {
    gene_id_col <- columns[1]
  }
  
  # Ensure all gene IDs are valid (non-NA, non-empty, and not literal "NA" or "NaN" strings)
  gene_col_vals <- trimws(as.character(df[[gene_id_col]]))
  valid_rows <- !is.na(gene_col_vals) & gene_col_vals != "" & gene_col_vals != "NA" & gene_col_vals != "NaN"
  if (any(!valid_rows)) {
    cat(sprintf("[PARSE] Filtering out %d rows with NA/empty Gene IDs\n", sum(!valid_rows)))
    df <- df[valid_rows, , drop = FALSE]
  }
  
  # Auto-detect other gene metadata columns if not explicitly passed
  if (is.null(gene_info_cols) || length(gene_info_cols) == 0) {
    candidate_cols <- setdiff(columns, gene_id_col)
    is_numeric_col <- sapply(candidate_cols, function(col) {
      vals <- df[[col]]
      vals_clean <- vals[!is.na(vals) & vals != ""]
      if (length(vals_clean) == 0) return(FALSE)
      # Check if values can be converted to numeric without inducing NAs
      num_vals <- suppressWarnings(as.numeric(trimws(as.character(vals_clean))))
      return(!any(is.na(num_vals)))
    })
    gene_info_cols <- candidate_cols[!is_numeric_col]
  } else {
    gene_info_cols <- as.character(gene_info_cols)
  }
  
  gene_ids <- make.unique(as.character(trimws(as.character(df[[gene_id_col]]))))
  expr_cols <- setdiff(columns, c(gene_id_col, gene_info_cols))
  if (length(expr_cols) == 0) {
    expr_cols <- setdiff(columns, gene_id_col)
  }
  expr_cols <- as.character(expr_cols)
  
  expr_mat_df <- df[, expr_cols, drop = FALSE]
  expr_mat <- safe_numeric_matrix(expr_mat_df)
  if (is.null(expr_mat)) {
    expr_mat <- matrix(numeric(0), nrow = nrow(df), ncol = 0)
  } else if (!is.matrix(expr_mat)) {
    expr_mat <- matrix(expr_mat, nrow = nrow(df), ncol = length(expr_cols))
  }
  
  # Set Gene ID column as row.names
  if (nrow(expr_mat) == length(gene_ids)) {
    rownames(expr_mat) <- as.character(gene_ids)
  } else {
    rownames(expr_mat) <- make.unique(as.character(seq_len(nrow(expr_mat))))
  }
  
  if (ncol(expr_mat) == length(expr_cols)) {
    colnames(expr_mat) <- as.character(expr_cols)
  } else {
    colnames(expr_mat) <- paste0("Sample_", seq_len(ncol(expr_mat)))
  }
  
  # Keep only gene_id_col and wipe other pre-existing annotation columns
  anno_df <- data.frame(gene_ids = as.character(gene_ids), stringsAsFactors = FALSE)
  colnames(anno_df) <- gene_id_col
  rownames(anno_df) <- as.character(gene_ids)

  return(list(expr = expr_mat, anno = anno_df, gene_ids = as.character(gene_ids), samples = as.character(expr_cols)))
}

# Helper to parse clinical payload
parse_clinical_data <- function(clinical_parsed_data, clinical_columns, sample_id_col) {
  if (is.null(clinical_parsed_data) || length(clinical_parsed_data) == 0) return(NULL)

  if (is.data.frame(clinical_parsed_data)) {
    df <- clinical_parsed_data
  } else if (is.matrix(clinical_parsed_data)) {
    df <- as.data.frame(clinical_parsed_data, stringsAsFactors = FALSE)
  } else {
    df <- as.data.frame(do.call(rbind, clinical_parsed_data), stringsAsFactors = FALSE)
  }

  if (!is.null(clinical_columns) && length(clinical_columns) > 0) {
    if (length(clinical_columns) == ncol(df)) {
      colnames(df) <- as.character(clinical_columns)
    } else {
      colnames(df) <- as.character(colnames(df))
    }
  } else {
    colnames(df) <- as.character(colnames(df))
  }

  for (col in colnames(df)) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  sample_id_col <- as.character(sample_id_col %||% "")
  if (nzchar(sample_id_col) && (sample_id_col %in% colnames(df))) {
    ids <- trimws(as.character(df[[sample_id_col]]))
    ids[is.na(ids) | ids == ""] <- "Unknown"
    df[[sample_id_col]] <- ids
    rownames(df) <- make.unique(ids)
  } else {
    ids <- trimws(as.character(df[[1]]))
    ids[is.na(ids) | ids == ""] <- "Unknown"
    df[[1]] <- ids
    rownames(df) <- make.unique(ids)
  }
  return(df)
}

# Re-assemble a data frame back to list of rows matching frontend parsedData
rebuild_parsed_data <- function(expr_mat, anno_df, gene_id_col) {
  common_genes <- intersect(rownames(expr_mat), rownames(anno_df))
  expr_mat <- expr_mat[common_genes, , drop = FALSE]
  anno_df <- anno_df[common_genes, , drop = FALSE]

  df <- cbind(anno_df, as.data.frame(expr_mat))
  columns <- as.character(colnames(df))
  n_rows <- nrow(df)
  
  if (n_rows == 0) {
    return(list(columns = columns, parsedData = list()))
  }
  
  preview_n <- min(10L, n_rows)
  preview_mat <- as.matrix(df[seq_len(preview_n), , drop = FALSE])
  preview_list <- lapply(seq_len(preview_n), function(i) unname(as.character(preview_mat[i, ])))
  
  if (n_rows > preview_n) {
    gene_id_idx <- if (nzchar(gene_id_col) && gene_id_col %in% columns) match(gene_id_col, columns) else 1L
    rem_genes <- as.character(df[[gene_id_idx]][(preview_n + 1L):n_rows])
    
    rem_list <- lapply(rem_genes, function(gid) {
      row_entry <- vector("character", length(columns))
      row_entry[gene_id_idx] <- as.character(gid)
      row_entry
    })
    parsed_data <- c(preview_list, rem_list)
  } else {
    parsed_data <- preview_list
  }

  return(list(columns = as.character(columns), parsedData = parsed_data))
}

# Helper to get the most suitable expression matrix for a given step, data type, and normalization status
get_suitable_expression_matrix <- function(ds_id, step = NULL, data_type = "readcounts", is_norm = FALSE) {
  is_norm <- isTRUE(is_norm)
  base_id <- get_base_id(ds_id)
  if (is.null(step)) step <- "latest"
  
  if (step == "de") {
    latest_de <- get_latest_de_stack(base_id, expr_only = TRUE)
    if (!is.null(latest_de) && !is.null(latest_de$data)) {
      mat <- latest_de$data
      attr(mat, "source_stack") <- "de_stack"
      attr(mat, "computed_step") <- latest_de$step
      return(mat)
    }
  }
  
  if (step == "fs") {
    latest_main <- get_latest_main_stack(base_id)
    if (!is.null(latest_main) && !is.null(latest_main$data)) {
      mat <- latest_main$data
      attr(mat, "source_stack") <- "main_stack"
      attr(mat, "computed_step") <- latest_main$step
      return(mat)
    }
    mat <- get_latest_normalized_matrix(base_id)
    if (!is.null(mat)) {
      attr(mat, "source_stack") <- "main_stack"
      # find step from stack
      stack <- get_main_stack(base_id)
      if (length(stack) > 0) {
        for (i in seq(length(stack), 1, by = -1)) {
          if (identical(stack[[i]]$data, mat)) {
            attr(mat, "computed_step") <- stack[[i]]$step
            break
          }
        }
      }
      if (is.null(attr(mat, "computed_step"))) attr(mat, "computed_step") <- "normalization"
    }
    return(mat)
  }
  
  is_readcounts_raw <- (data_type == "readcounts" && !is_norm)
  
  allowed_steps <- c("batch", "normalization", "processing", "annotation", "upload")
  if (step == "pca") {
    if (is_readcounts_raw) {
      allowed_steps <- c("batch", "processing", "annotation", "upload")
    } else {
      allowed_steps <- c("batch", "normalization", "processing", "annotation", "upload")
    }
  } else if (step == "batch") {
    if (is_readcounts_raw) {
      allowed_steps <- c("processing", "annotation", "upload")
    } else {
      allowed_steps <- c("normalization", "processing", "annotation", "upload")
    }
  } else if (step == "normalization") {
    allowed_steps <- c("processing", "annotation", "upload")
  } else if (step == "processing") {
    allowed_steps <- c("annotation", "normalization", "upload")
  } else if (step == "annotation") {
    allowed_steps <- if (identical(tolower(data_type), "microarray")) c("normalization", "upload") else c("upload")
  }

  # For raw readcounts (un-normalized): peek latest valid counts_stack entry
  if (is_readcounts_raw && step %in% c("batch", "de", "pca", "latest")) {
    stack_c <- get_counts_stack(base_id)
    if (length(stack_c) > 0) {
      for (i in seq(length(stack_c), 1, by = -1)) {
        if (!is.null(stack_c[[i]]$step) && stack_c[[i]]$step %in% allowed_steps && !is.null(stack_c[[i]]$data)) {
          cat(sprintf("[LIFO STACK] Picked counts stack entry ('%s') for dataset %s at step %s\n", stack_c[[i]]$step, base_id, step))
          mat <- stack_c[[i]]$data
          attr(mat, "source_stack") <- "counts_stack"
          attr(mat, "computed_step") <- stack_c[[i]]$step
          return(mat)
        }
      }
    }
  }
  
  # For main workflow: peek latest valid main_stack entry
  stack_main <- get_main_stack(base_id)
  if (length(stack_main) > 0) {
    for (i in seq(length(stack_main), 1, by = -1)) {
      if (!is.null(stack_main[[i]]$step) && stack_main[[i]]$step %in% allowed_steps && !is.null(stack_main[[i]]$data)) {
        cat(sprintf("[LIFO STACK] Picked main stack entry ('%s') for dataset %s at step %s\n", stack_main[[i]]$step, base_id, step))
        mat <- stack_main[[i]]$data
        attr(mat, "source_stack") <- "main_stack"
        attr(mat, "computed_step") <- stack_main[[i]]$step
        return(mat)
      }
    }
  }
  
  # Fallback to checking RDS file paths on disk if stack file is not yet initialized
  files <- list(
    batch = get_session_path(base_id, "%s_batch_expr.rds"),
    normalization = get_session_path(base_id, "%s_normalized_expr.rds"),
    processing = get_session_path(base_id, "%s_processed_expr.rds"),
    annotation = get_session_path(base_id, "%s_expr_matrix_annotated.rds"),
    upload = get_session_path(base_id, "%s_expr_matrix.rds")
  )
  
  for (s in allowed_steps) {
    f_path <- files[[s]]
    if (!is.null(f_path) && file.exists(f_path)) {
      mat <- readRDS(f_path)
      if (!is.null(mat)) {
        cat(sprintf("[SUITABLE DATASET] Picked %s matrix for dataset %s at step %s\n", s, base_id, step))
        attr(mat, "source_stack") <- "rds_file"
        attr(mat, "computed_step") <- s
        return(mat)
      }
    }
  }
  
  return(NULL)
}

# Helper to load expression data from CSV or cache
get_backend_dataset <- function(dataset_id, original = FALSE, step = NULL) {
  parsed_id <- get_backend_datasets(dataset_id)
  dataset_id <- parsed_id$base_id
  
  if (is.null(step)) {
    step <- parsed_id$step
  }

  # Check if local dataset data exists
  local_cache_path <- get_session_path(dataset_id, "%s_original_parsed.rds")
  local_csv_path   <- get_session_path(dataset_id, "%s_expression.csv")
  local_stack_path <- get_session_path(dataset_id, "%s_main_stack.rds")
  local_expr_path  <- get_session_path(dataset_id, "%s_expr_matrix.rds")

  has_local_data <- file.exists(local_cache_path) || file.exists(local_csv_path) ||
                    file.exists(local_stack_path) || file.exists(local_expr_path)

  # Only redirect to parent if dataset is inline/inherited AND local files are missing
  is_inherited <- isTRUE(parsed_id$isInline)
  if (!has_local_data && is_inherited) {
    parent_id <- parsed_id$parentDatasetId
    if (is.null(parent_id) || !nzchar(parent_id) || parent_id == dataset_id) {
      if (!is.null(parsed_id$parentModule) && nzchar(parsed_id$parentModule) && !is.null(parsed_id$module) && parsed_id$parentModule != parsed_id$module) {
        parent_id <- sub(paste0("_", parsed_id$module, "$"), paste0("_", parsed_id$parentModule), dataset_id)
      }
    }
    if (!is.null(parent_id) && nzchar(parent_id) && parent_id != dataset_id) {
      parent_check <- get_session_path(parent_id, "%s_main_stack.rds")
      if (file.exists(parent_check) || file.exists(get_session_path(parent_id, "%s_expression.csv"))) {
        cat(sprintf("[INHERIT] Redirecting dataset %s -> parent %s\n", dataset_id, parent_id))
        dataset_id <- parent_id
        parsed_id$base_id <- parent_id
      }
    }
  }

  cache_path <- get_session_path(dataset_id, "%s_original_parsed.rds")
  resolved_from <- "csv_file"
  resolved_step <- if (is.null(step)) "upload" else step
  
  if (file.exists(cache_path)) {
    parsed_data <- readRDS(cache_path)
    resolved_from <- "rds_cache"
    
    if (original) {
      data_dims <- if (is.null(dim(parsed_data$expr))) paste("length =", length(parsed_data$expr)) else paste(dim(parsed_data$expr), collapse = "x")
      cat(sprintf("[TRACK] get_backend_dataset: dataset_id = %s, original = TRUE, resolved from = rds_cache, step = %s, data dims = %s\n",
                  dataset_id, step, data_dims))
      return(parsed_data)
    }
  } else {
    csv_path <- get_session_path(dataset_id, "%s_expression.csv")
    if (!original) {
      anno_csv <- get_session_path(dataset_id, "%s_annotation_results.csv")
      if (file.exists(anno_csv)) {
        csv_path <- anno_csv
      }
    }
    meta_path <- get_session_path(dataset_id, "%s_expr_metadata.rds")
    
    if (!file.exists(csv_path)) {
      backup_csv <- get_session_path(dataset_id, "%s_expression_original_backup.csv")
      if (file.exists(backup_csv)) {
        file.copy(backup_csv, csv_path, overwrite = TRUE)
      } else {
        cat(sprintf("[WARNING] get_backend_dataset: expression file not found for %s\n", dataset_id))
        return(NULL)
      }
    }
    
    df <- read_csv_preserve_id(csv_path)
    
    gene_id_col <- colnames(df)[1]
    gene_info_cols <- character(0)
    is_norm <- FALSE
    data_type <- "readcounts"
    platform <- ""
    gene_id_type <- "ensembl"
    
    if (file.exists(meta_path)) {
      meta <- readRDS(meta_path)
      gene_id_col <- if (!is.null(meta$geneIdCol) && meta$geneIdCol != "") meta$geneIdCol else gene_id_col
      gene_info_cols <- if (!is.null(meta$geneInfoCols)) meta$geneInfoCols else gene_info_cols
      is_norm <- if (!is.null(meta$isNormalized)) isTRUE(meta$isNormalized) else is_norm
      data_type <- if (!is.null(meta$dataType)) meta$dataType else data_type
      platform <- if (!is.null(meta$platform)) meta$platform else platform
      gene_id_type <- if (!is.null(meta$geneIdType)) meta$geneIdType else gene_id_type
    }
    
    parsed_data <- parse_expression_data(as.matrix(df), colnames(df), gene_id_col, gene_info_cols)
    if (is.null(parsed_data)) return(NULL)
    
    parsed_data$geneIdCol <- gene_id_col
    parsed_data$geneInfoCols <- gene_info_cols
    parsed_data$isNormalized <- is_norm
    parsed_data$dataType <- data_type
    parsed_data$platform <- platform
    parsed_data$geneIdType <- gene_id_type
    
    # Save the original parsed cache if we loaded the base expression file
    orig_csv_path <- get_session_path(dataset_id, "%s_expression.csv")
    if (csv_path == orig_csv_path) {
      saveRDS(parsed_data, cache_path)
    }
  }
  
  # Overwrite parsed_data$expr with the most suitable RDS expression matrix if original is FALSE
  if (!original) {
    expr_latest <- get_suitable_expression_matrix(dataset_id, step, parsed_data$dataType %||% "readcounts", isTRUE(parsed_data$isNormalized))
    if (!is.null(expr_latest)) {
      # Extract attributes set in get_suitable_expression_matrix
      if (!is.null(attr(expr_latest, "source_stack"))) {
        resolved_from <- attr(expr_latest, "source_stack")
      }
      if (!is.null(attr(expr_latest, "computed_step"))) {
        resolved_step <- attr(expr_latest, "computed_step")
      }
      
      resolved_mapping_path <- get_session_path(dataset_id, "%s_resolved_mapping.rds")
      if (file.exists(resolved_mapping_path)) {
        resolved_mapping <- tryCatch(readRDS(resolved_mapping_path), error = function(e) NULL)
        if (!is.null(resolved_mapping)) {
          common_ids <- intersect(rownames(expr_latest), rownames(resolved_mapping))
          if (length(common_ids) > 0) {
            parsed_data$expr <- expr_latest[common_ids, , drop = FALSE]
            parsed_data$anno <- resolved_mapping[common_ids, c("entrez_id", "gene_symbol"), drop = FALSE]
            parsed_data$gene_ids <- common_ids
            parsed_data$samples <- colnames(parsed_data$expr)
            # Mark as resolved
            expr_latest <- NULL
          }
        }
      }
      
      if (!is.null(expr_latest)) {
        common_genes <- intersect(rownames(expr_latest), rownames(parsed_data$anno))
        if (length(common_genes) > 0) {
          parsed_data$expr <- expr_latest[common_genes, , drop = FALSE]
          parsed_data$anno <- parsed_data$anno[common_genes, , drop = FALSE]
          parsed_data$gene_ids <- common_genes
          parsed_data$samples <- colnames(parsed_data$expr)
        } else {
          parsed_data$expr <- expr_latest
          parsed_data$gene_ids <- rownames(expr_latest)
          parsed_data$samples <- colnames(expr_latest)
        }
      }
    }
    # parsed_data$expr now holds the (possibly gene-subset) result; drop the full
    # "latest" matrix reference so we don't keep a second full matrix alive until return.
    expr_latest <- NULL
  }

  data_dims <- if (is.null(dim(parsed_data$expr))) paste("length =", length(parsed_data$expr)) else paste(dim(parsed_data$expr), collapse = "x")
  cat(sprintf("[TRACK] get_backend_dataset: dataset_id = %s, original = %s, resolved from = %s, step = %s, data dims = %s\n",
              dataset_id, as.character(original), resolved_from, resolved_step, data_dims))
  
  return(parsed_data)
}

# 1. Gene Annotation Endpoint Handler
# Helper: normalise user-facing biotype labels to biomaRt/OrgDb internal strings
normalise_biotypes <- function(biotype_str) {
  if (is.null(biotype_str) || biotype_str == "") return(NULL)
  
  # Split comma-separated string into a vector
  labels <- trimws(unlist(strsplit(biotype_str, ",")))
  
  label_map <- c(
    "protein-coding"  = "protein_coding",
    "protein coding"  = "protein_coding",
    "proteincoding"   = "protein_coding",
    "lncrna"          = "lncRNA",
    "pseudogenes"     = "pseudogene",
    "pseudogene"      = "pseudogene",
    "rrna"            = "rRNA",
    "snrna"           = "snRNA",
    "snorna"          = "snoRNA",
    "mirna"           = "miRNA",
    "all"             = "all"
  )
  
  mapped <- sapply(labels, function(lbl) {
    key <- tolower(lbl)
    if (key %in% names(label_map)) label_map[[key]] else lbl
  })
  
  return(unname(mapped))
}

collapse_duplicate_ids_parallel <- function(expr, ids, data_type, is_normalized = FALSE) {
  unique_ids <- unique(ids)
  if (length(unique_ids) == length(ids)) {
    rownames(expr) <- ids
    return(expr)
  }
  
  dt_lower <- if (!is.null(data_type)) tolower(trimws(as.character(data_type))) else "readcounts"
  is_norm <- isTRUE(is_normalized)
  is_sum_collapse <- (dt_lower == "readcounts" && !is_norm)
  
  if (is_sum_collapse) {
    return(rowsum(expr, group = ids, reorder = FALSE))
  } else {
    if (requireNamespace("limma", quietly = TRUE)) {
      return(limma::avereps(expr, ID = ids))
    } else {
      sums <- rowsum(expr, group = ids, reorder = FALSE)
      group_counts <- as.vector(table(factor(ids, levels = rownames(sums))))
      return(sweep(sums, 1, group_counts, "/"))
    }
  }
}

annotate_genes <- function(strategy, source, organisms, biotypes, datasets) {
  cat("[PROCESSING] Running gene mapping...\n")

  process_single_annotation <- function(d) {
    ds_id_full <- if (!is.null(d$datasetId)) d$datasetId else d$id
    ds_id      <- get_base_id(ds_id_full)
    d_strategy <- if (!is.null(d$strategy))  d$strategy  else strategy
    d_organism <- if (!is.null(d$organism))  d$organism  else organisms
    d_data_type <- if (!is.null(d$dataType)) d$dataType  else "readcounts"

    # Normalise biotype labels sent from the client
    raw_biotype <- if (!is.null(d$biotype) && d$biotype != "") d$biotype else biotypes
    d_biotype   <- normalise_biotypes(raw_biotype)

    # Always load expression matrix and metadata from server-stored files
    is_ma <- identical(tolower(d_data_type), "microarray")
    parsed <- get_backend_dataset(ds_id_full, original = !is_ma, step = "annotation")
    if (is.null(parsed)) {
      cat(sprintf("  [WARNING] No server-stored expression matrix found for %s, returning zeros.\n", ds_id_full))
      return(list(datasetId = ds_id_full, total = 0, mapped = 0, unmapped = 0, unique = 0, multi = 0, retained = 0))
    }
    is_norm <- if (!is.null(parsed$isNormalized)) isTRUE(parsed$isNormalized) else FALSE

    # ── Tier 1 Cache Check ───────────────────────────────────────────────────
    step_cache <- get_step_cache_meta(ds_id, "annotation")
    upstream_fp <- get_matrix_fingerprint(parsed$expr)
    out_file <- get_session_path(ds_id, "%s_expr_matrix_annotated.rds")
    
    if (!is.null(step_cache) &&
        identical(step_cache$strategy, d_strategy) &&
        identical(step_cache$organism, d_organism) &&
        identical(step_cache$biotype,  d_biotype) &&
        identical(step_cache$upstream_fp, upstream_fp) &&
        file.exists(out_file)) {
      cat(sprintf("[CACHE] Reusing cached annotation for dataset %s\n", ds_id))
      annotated_mat <- tryCatch(readRDS(out_file), error = function(e) NULL)
      if (!is.null(annotated_mat)) {
        push_main_stack(ds_id, annotated_mat, step_name = "annotation", metadata = list(strategy = d_strategy, organism = d_organism, biotype = d_biotype, dataType = d_data_type))
        if (isTRUE(d_data_type == "readcounts")) {
          push_counts_stack(ds_id, annotated_mat, step_name = "annotation", metadata = list(strategy = d_strategy, organism = d_organism, biotype = d_biotype, dataType = d_data_type))
        }
        ensure_sequential_matrices(ds_id)
        return(step_cache$result)
      }
    }

    # gene_id_col and gene_id_type come from saved server metadata (via get_backend_dataset)
    gene_id_col  <- parsed$geneIdCol
    gene_id_type <- parsed$geneIdType
    cat(sprintf("  Dataset %s: geneIdCol='%s', geneIdType='%s'\n", ds_id, gene_id_col, gene_id_type))

    # Apply Illumina probe quality and detection P-val filters if microarray
    if (identical(tolower(d_data_type %||% parsed$dataType %||% ""), "microarray")) {
      parsed$expr <- apply_illumina_probe_filters(parsed$expr, ds_id, gene_id_type, d_data_type %||% parsed$dataType)
      parsed$gene_ids <- rownames(parsed$expr)
      if (!is.null(parsed$anno) && nrow(parsed$anno) > 0) {
        anno_idx <- match(rownames(parsed$expr), rownames(parsed$anno))
        parsed$anno <- parsed$anno[!is.na(anno_idx), , drop = FALSE]
      }
    }

    cat(sprintf("[CONSOLE] Dataset: %s (annotation)\n", ds_id))
    cat(sprintf("[CONSOLE]   Input features before: %d\n", length(parsed$gene_ids)))
    cat(sprintf("[CONSOLE]   Duplicate IDs before: %d\n", sum(duplicated(parsed$gene_ids))))
    cat(sprintf("[CONSOLE]   Missing values before: %d\n", sum(is.na(parsed$expr))))

    # ── Skip path ─────────────────────────────────────────────────────────────
    if (isTRUE(d_data_type == "others") || tolower(d_strategy) == "skip") {
      cat(sprintf("  Skipping annotation for dataset %s\n", ds_id))
      gene_ids_vec <- rownames(parsed$expr)

      # Clean/consistent skipped annotation table structure
      input_col_name <- paste0(tolower(gene_id_type), "_id")
      df_anno_res <- data.frame(
        input_id = gene_ids_vec,
        entrez_id = rep(NA, length(gene_ids_vec)),
        gene_symbol = gene_ids_vec,
        gene_biotype = rep("unknown", length(gene_ids_vec)),
        stringsAsFactors = FALSE
      )
      colnames(df_anno_res)[1] <- input_col_name

      saveRDS(df_anno_res, file = get_session_path(ds_id, "%s_annotation_results.rds"))
      write.csv(df_anno_res, file  = get_session_path(ds_id, "%s_annotation_results.csv"), row.names = FALSE)

      df_unmapped_res <- df_anno_res[0, , drop = FALSE]
      saveRDS(df_unmapped_res, file = get_session_path(ds_id, "%s_unmapped_results.rds"))
      write.csv(df_unmapped_res, file = get_session_path(ds_id, "%s_unmapped_results.csv"), row.names = FALSE)

      uid <- get_user_id(ds_id)
      register_export_file(uid, "annotation_results", ds_id, get_session_path(ds_id, "%s_annotation_results.rds"), "dp", "annotation", ext = "csv")
      register_export_file(uid, "unmapped_results", ds_id, get_session_path(ds_id, "%s_unmapped_results.rds"), "dp", "annotation", ext = "csv")

      push_main_stack(ds_id, parsed$expr, step_name = "annotation", metadata = list(strategy = "skip", dataType = d_data_type))
      if (isTRUE(d_data_type == "readcounts")) {
        push_counts_stack(ds_id, parsed$expr, step_name = "annotation", metadata = list(strategy = "skip", dataType = d_data_type))
      }

      # Also keep the annotated matrix RDS under legacy name
      saveRDS(parsed$expr, file = get_session_path(ds_id, "%s_expr_matrix_annotated.rds"))
      saveRDS(parsed$expr, file = get_session_path(ds_id, "%s_processed_expr.rds"))
      saveRDS(parsed$expr, file = get_session_path(ds_id, "%s_expr_matrix.rds"))

      # Update metadata: geneIdCol is now gene_symbol
      meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
      if (file.exists(meta_path)) {
        meta <- readRDS(meta_path)
        meta$geneIdCol   <- "gene_symbol"
        meta$geneIdType  <- "genename"
        meta$geneInfoCols <- c("entrez_id", "gene_symbol")
        saveRDS(meta, file = meta_path)
      }

      n <- length(gene_ids_vec)
      cat(sprintf("[CONSOLE]   Output features after: %d\n", n))
      cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(gene_ids_vec))))
      cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(parsed$expr))))

      rebuilt <- rebuild_parsed_data(parsed$expr, parsed$anno, parsed$geneIdCol)
      pre_qc <- tryCatch(prepare_pre_processing_qc(parsed$expr, ds_id, d_data_type, is_norm, d$name), error = function(e) NULL)
      res <- list(
        datasetId  = ds_id_full,
        total      = n,
        mapped     = n,
        unmapped   = 0L,
        unique     = length(unique(gene_ids_vec)),
        multi      = 0L,
        retained   = n,
        columns    = rebuilt$columns,
        parsedData = rebuilt$parsedData,
        sampleIds  = colnames(parsed$expr),
        nSamples   = as.integer(ncol(parsed$expr)),
        qc         = if (!is.null(pre_qc)) list(before = pre_qc, meanVariance = pre_qc$meanVariance, distribution = pre_qc$distribution, pca = pre_qc$pca) else NULL
      )
      save_step_cache_meta(ds_id, "annotation", list(
        strategy = d_strategy,
        organism = d_organism,
        biotype  = d_biotype,
        upstream_fp = upstream_fp,
        result   = res
      ))
      return(res)
    }

    # ── Phase 1: Clean version suffix and Parallel collapse of duplicate IDs ──
    keys <- if (!is.null(rownames(parsed$expr))) rownames(parsed$expr) else parsed$gene_ids
    
    # Strip Ensembl version suffixes if ID type is ensembl
    if (tolower(gene_id_type) == "ensembl") {
      cat(sprintf("  Cleaning version suffixes from Ensembl IDs for dataset %s...\n", ds_id))
      # Remove rows with NA gene IDs before processing
      na_mask <- !is.na(keys) & keys != "" & keys != "NA"
      if (any(!na_mask)) {
        cat(sprintf("  Removing %d rows with NA/empty gene IDs for dataset %s...\n", sum(!na_mask), ds_id))
        parsed$expr <- parsed$expr[na_mask, , drop = FALSE]
        if (!is.null(parsed$anno) && (is.matrix(parsed$anno) || is.data.frame(parsed$anno))) {
          parsed$anno <- parsed$anno[na_mask, , drop = FALSE]
        }
        keys <- keys[na_mask]
        parsed$gene_ids <- keys
      }
      keys <- sub("\\.[0-9]+$", "", keys, perl = TRUE, useBytes = TRUE)
      rownames(parsed$expr) <- keys
      if (!is.null(parsed$anno) && (is.matrix(parsed$anno) || is.data.frame(parsed$anno))) {
        rownames(parsed$anno) <- keys
      }
      parsed$gene_ids <- keys
    }
    
    # Collapse duplicated gene IDs
    if (any(duplicated(keys))) {
      cat(sprintf("  Collapsing duplicate input IDs for dataset %s...\n", ds_id))
      expr_collapsed <- collapse_duplicate_ids_parallel(parsed$expr, keys, d_data_type, is_norm)
      if (!is.null(parsed$anno) && (is.matrix(parsed$anno) || is.data.frame(parsed$anno))) {
        match_idx <- match(rownames(expr_collapsed), keys)
        parsed$anno <- parsed$anno[match_idx, , drop = FALSE]
        rownames(parsed$anno) <- rownames(expr_collapsed)
      }
      parsed$expr <- expr_collapsed
      keys <- rownames(expr_collapsed)
      parsed$gene_ids <- keys
    } else {
      expr_collapsed <- parsed$expr
    }
    
    # Standardize organism database package name
    species_name <- "Homo sapiens"
    org_db <- org.Hs.eg.db::org.Hs.eg.db
    if (any(grepl("mus|musculus|mouse", tolower(d_organism), perl = TRUE))) {
      species_name <- "Mus musculus"
      if (requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
        org_db <- org.Mm.eg.db::org.Mm.eg.db
      }
    } else if (any(grepl("rattus|rat", tolower(d_organism), perl = TRUE))) {
      species_name <- "Rattus norvegicus"
      if (requireNamespace("org.Rn.eg.db", quietly = TRUE)) {
        org_db <- org.Rn.eg.db::org.Rn.eg.db
      }
    } else if (any(grepl("sus|pig|scrofa", tolower(d_organism), perl = TRUE))) {
      species_name <- "Sus scrofa"
      if (requireNamespace("org.Ss.eg.db", quietly = TRUE)) {
        org_db <- org.Ss.eg.db::org.Ss.eg.db
      }
    } else if (any(grepl("gallus|chicken", tolower(d_organism), perl = TRUE))) {
      species_name <- "Gallus gallus"
      if (requireNamespace("org.Gg.eg.db", quietly = TRUE)) {
        org_db <- org.Gg.eg.db::org.Gg.eg.db
      }
    }
    
    # ── Phase 2: Platform / ID Mapping ──
    raw_mapping <- NULL
    
    # Check if we can reuse cached raw mapping from a previous run to speed up redo
    raw_mapping_file <- get_session_path(ds_id, "%s_raw_mapping.rds")
    if (file.exists(raw_mapping_file)) {
      cached <- tryCatch(readRDS(raw_mapping_file), error = function(e) NULL)
      if (!is.null(cached) && 
          identical(cached$organism, d_organism) && 
          identical(cached$gene_id_type, gene_id_type) && 
          identical(cached$keys, keys) &&
          identical(cached$microarrayPlatformId, d$microarrayPlatformId) &&
          identical(cached$platformFamily, d$platformFamily)) {
        cat(sprintf("  [ANNOTATION] Reusing cached raw mapping for dataset %s...\n", ds_id))
        raw_mapping <- cached$raw_mapping
      }
    }
    
    is_microarray_platform <- !is.null(d$microarrayPlatformId) && d$microarrayPlatformId != "" && tolower(d_data_type) == "microarray"
    
    if (is.null(raw_mapping) && is_microarray_platform && !is.null(d$platformFamily)) {
      platforms_json_path <- "sample_data/microarray_platforms.json"
      pkg_name <- NULL
      if (file.exists(platforms_json_path)) {
        platforms_json <- jsonlite::read_json(platforms_json_path)
        platform_family <- tolower(d$platformFamily)
        platform_id <- d$microarrayPlatformId
        platform_list <- platforms_json[[platform_family]]
        if (!is.null(platform_list)) {
          for (p in platform_list) {
            if (p$platform == platform_id) {
              pkg_name <- p$annotation_package
              # ── Override org_db from the platform's hardcoded organism ──
              # Each platform in microarray_platforms.json has an "organism" field
              # (e.g. "Mus musculus" for MoGene, "Rattus norvegicus" for RatRef-12).
              # This overrides whatever the user passed as d$organism, ensuring the
              # GENETYPE lookup always uses the species-correct annotation DB.
              if (!is.null(p$organism) && nzchar(p$organism)) {
                platform_org <- p$organism
                cat(sprintf("  [ANNOTATION] Platform '%s' has hardcoded organism: '%s'. Overriding org_db.\n",
                            platform_id, platform_org))
                if (grepl("mus|musculus|mouse", tolower(platform_org), perl = TRUE)) {
                  if (requireNamespace("org.Mm.eg.db", quietly = TRUE))
                    org_db <- org.Mm.eg.db::org.Mm.eg.db
                } else if (grepl("rattus|rat", tolower(platform_org), perl = TRUE)) {
                  if (requireNamespace("org.Rn.eg.db", quietly = TRUE))
                    org_db <- org.Rn.eg.db::org.Rn.eg.db
                } else if (grepl("sus|pig|scrofa", tolower(platform_org), perl = TRUE)) {
                  if (requireNamespace("org.Ss.eg.db", quietly = TRUE))
                    org_db <- org.Ss.eg.db::org.Ss.eg.db
                } else if (grepl("gallus|chicken", tolower(platform_org), perl = TRUE)) {
                  if (requireNamespace("org.Gg.eg.db", quietly = TRUE))
                    org_db <- org.Gg.eg.db::org.Gg.eg.db
                } else {
                  # Default / Homo sapiens — already the default org_db
                  org_db <- org.Hs.eg.db::org.Hs.eg.db
                }
              }
              break
            }
          }
        }
      }
      
      if (!is.null(pkg_name) && !requireNamespace(pkg_name, quietly = TRUE)) {
        cat(sprintf("  [ANNOTATION] Platform package %s is not installed. Attempting to install it via BiocManager into local library...\n", pkg_name))
        tryCatch({
          target_lib <- get_local_r_lib()
          if (!dir.exists(target_lib)) dir.create(target_lib, recursive = TRUE, showWarnings = FALSE)
          if (!(target_lib %in% .libPaths())) .libPaths(unique(c(target_lib, .libPaths())))
          
          if (!requireNamespace("BiocManager", quietly = TRUE)) {
            install.packages("BiocManager", lib = target_lib, repos = "https://cloud.r-project.org", quietly = TRUE)
          }
          BiocManager::install(pkg_name, lib = target_lib, update = FALSE, ask = FALSE, quietly = FALSE)
          if (!(target_lib %in% .libPaths())) .libPaths(unique(c(target_lib, .libPaths())))
        }, error = function(e) {
          cat(sprintf("  [WARNING] Failed to install platform package %s: %s\n", pkg_name, e$message))
        })
      }
      
      if (!is.null(pkg_name) && requireNamespace(pkg_name, quietly = TRUE)) {
        cat(sprintf("  [ANNOTATION] Querying platform package %s for %s...\n", pkg_name, d$microarrayPlatformId))
        pkg_env <- loadNamespace(pkg_name)
        anno_db <- get(pkg_name, envir = pkg_env)
        
        # ── Step 1: Query probe → ENTREZID + SYMBOL from platform annotation DB ──
        mapped_probes <- tryCatch({
          suppressMessages(AnnotationDbi::select(
            anno_db,
            keys    = keys,
            columns = c("PROBEID", "ENTREZID", "SYMBOL"),
            keytype = "PROBEID",
            multivals = "first"
          ))
        }, error = function(e) {
          cols <- intersect(AnnotationDbi::columns(anno_db), c("PROBEID", "ENTREZID", "SYMBOL"))
          suppressMessages(AnnotationDbi::select(anno_db, keys = keys, columns = cols, keytype = "PROBEID"))
        })
        cat(sprintf("  [ANNOTATION] Platform DB returned %d rows for %d probe keys.\n",
                    nrow(mapped_probes), length(keys)))
        
        # ── Step 2: Query ENTREZID → GENETYPE from organism DB ──
        mapped_entrez_keys <- unique(na.omit(mapped_probes$ENTREZID))
        if (length(mapped_entrez_keys) > 0) {
          biotypes_ma <- tryCatch({
            suppressMessages(AnnotationDbi::select(
              org_db,
              keys    = mapped_entrez_keys,
              keytype = "ENTREZID",
              columns = c("ENTREZID", "GENETYPE")
            ))
          }, error = function(e) NULL)
          if (!is.null(biotypes_ma)) {
            biotypes_ma <- biotypes_ma[!duplicated(biotypes_ma$ENTREZID), , drop = FALSE]
            mapped_probes <- merge(mapped_probes, biotypes_ma, by = "ENTREZID", all.x = TRUE)
          } else {
            mapped_probes$GENETYPE <- NA_character_
          }
        } else {
          mapped_probes$GENETYPE <- NA_character_
        }
        
        # ── Step 3: Valid-first 1:N resolution (from Microarray_processing.R annotate_probes) ──
        # For probes with at least one fully-valid row, drop invalid rows so !duplicated picks the valid one.
        is_valid_ma <- !is.na(mapped_probes$PROBEID)  & mapped_probes$PROBEID  != "" &
                       !is.na(mapped_probes$ENTREZID) & mapped_probes$ENTREZID != "" &
                       !is.na(mapped_probes$SYMBOL)   & mapped_probes$SYMBOL   != "" &
                       !is.na(mapped_probes$GENETYPE) & mapped_probes$GENETYPE != ""
        probes_with_valid_ma <- unique(mapped_probes$PROBEID[is_valid_ma])
        keep_mask_ma <- is_valid_ma | !(mapped_probes$PROBEID %in% probes_with_valid_ma)
        mapped_probes <- mapped_probes[keep_mask_ma, , drop = FALSE]
        
        # ── Step 4: Detect remaining 1:N mappings (console only) ──
        dup_mask_ma  <- duplicated(mapped_probes$PROBEID) |
                        duplicated(mapped_probes$PROBEID, fromLast = TRUE)
        n_multi_probe_ma <- length(unique(mapped_probes$PROBEID[dup_mask_ma]))
        if (n_multi_probe_ma > 0) {
          cat(sprintf("  [ANNOTATION] %d probe IDs still have multiple Entrez hits after valid-first filter — keeping first row.\n",
                      n_multi_probe_ma))
        }
        
        # ── Step 5: Keep first row per PROBEID ──
        anno_dedup_ma <- mapped_probes[!duplicated(mapped_probes$PROBEID), , drop = FALSE]
        
        # ── Step 6: Standardise to raw_mapping schema ──
        raw_mapping <- data.frame(
          input_id     = as.character(anno_dedup_ma$PROBEID),
          entrez_id    = as.character(anno_dedup_ma$ENTREZID),
          gene_symbol  = as.character(anno_dedup_ma$SYMBOL),
          gene_biotype = gsub("-", "_", as.character(anno_dedup_ma$GENETYPE)),
          stringsAsFactors = FALSE
        )
        cat(sprintf("  [ANNOTATION] Microarray: %d unique probes resolved.\n", nrow(raw_mapping)))
      } else {
        cat(sprintf("  [WARNING] Platform package %s not available. Falling back to direct org_db mapping.\n", pkg_name))
      }
    }
    
    if (is.null(raw_mapping)) {
      # ── Standard RNA-seq / gene ID query (from RNA_processing.R annotate_ensembl_to_entrez) ──
      norm_id_type <- tolower(gene_id_type)
      key_type <- "ENSEMBL"
      if (norm_id_type %in% c("entrez", "entrezid", "entrez_id")) {
        key_type <- "ENTREZID"
      } else if (norm_id_type %in% c("genename", "symbol", "gene_name", "external_gene_name")) {
        key_type <- "SYMBOL"
      }
      
      cat(sprintf("  [ANNOTATION] Querying org_db with keytype '%s' for %d unique IDs.\n",
                  key_type, length(unique(keys))))
      
      # ── Step 1: Query org_db ──
      org_mapping <- tryCatch({
        suppressMessages(AnnotationDbi::select(
          org_db,
          keys    = unique(as.character(keys)),
          columns = c("ENTREZID", "SYMBOL", "GENETYPE"),
          keytype = key_type,
          multiVals = "first"
        ))
      }, error = function(e) {
        df <- data.frame(
          keys     = unique(as.character(keys)),
          ENTREZID = rep(NA_character_, length(unique(keys))),
          SYMBOL   = rep(NA_character_, length(unique(keys))),
          GENETYPE = rep(NA_character_,  length(unique(keys))),
          stringsAsFactors = FALSE
        )
        colnames(df)[1] <- key_type
        df
      })
      
      if (!(key_type %in% colnames(org_mapping))) {
        org_mapping[[key_type]] <- unique(as.character(keys))
      }
      
      cat(sprintf("  [ANNOTATION] org_db returned %d rows (input unique IDs: %d).\n",
                  nrow(org_mapping), length(unique(keys))))
      
      # ── Step 2: Valid-first 1:N resolution ──
      # For IDs that have at least one fully-valid row, drop invalid rows so !duplicated picks the valid one.
      is_valid_rna <- !is.na(org_mapping[[key_type]])  & org_mapping[[key_type]]  != "" &
                      !is.na(org_mapping$ENTREZID)     & org_mapping$ENTREZID     != "" &
                      !is.na(org_mapping$SYMBOL)       & org_mapping$SYMBOL       != "" &
                      !is.na(org_mapping$GENETYPE)     & org_mapping$GENETYPE     != ""
      ids_with_valid_rna <- unique(org_mapping[[key_type]][is_valid_rna])
      keep_mask_rna <- is_valid_rna | !(org_mapping[[key_type]] %in% ids_with_valid_rna)
      org_mapping <- org_mapping[keep_mask_rna, , drop = FALSE]
      
      # ── Step 3: Detect remaining 1:N mappings ──
      dup_mask_rna <- duplicated(org_mapping[[key_type]]) |
                      duplicated(org_mapping[[key_type]], fromLast = TRUE)
      n_multi_rna <- length(unique(org_mapping[[key_type]][dup_mask_rna]))
      if (n_multi_rna > 0) {
        cat(sprintf("  [ANNOTATION] %d %s IDs still have multiple Entrez hits after valid-first filter — keeping first row.\n",
                    n_multi_rna, key_type))
      }
      
      # ── Step 4: Keep first row per input ID ──
      org_mapping <- org_mapping[!duplicated(org_mapping[[key_type]]), , drop = FALSE]
      
      cat(sprintf("  [ANNOTATION] Annotation rows after dedup: %d (input unique IDs: %d).\n",
                  nrow(org_mapping), length(unique(keys))))
      
      # ── Step 5: Normalise biotype separator (- → _) as in RNA_processing.R ──
      if ("GENETYPE" %in% colnames(org_mapping)) {
        org_mapping$GENETYPE <- gsub("-", "_", as.character(org_mapping$GENETYPE))
      }
      
      # ── Step 6: Build raw_mapping (all original keys, including unmapped) ──
      raw_mapping <- data.frame(
        input_id     = as.character(org_mapping[[key_type]]),
        entrez_id    = as.character(org_mapping$ENTREZID),
        gene_symbol  = as.character(org_mapping$SYMBOL),
        gene_biotype = as.character(org_mapping$GENETYPE),
        stringsAsFactors = FALSE
      )
    }
    
    # Ensure all input keys are represented in raw_mapping
    keys_df <- data.frame(input_id = keys, stringsAsFactors = FALSE)
    raw_mapping <- merge(keys_df, raw_mapping, by = "input_id", all.x = TRUE)
    
    # Classify as unmapped (set entrez_id to NA) if mapped to entrez ID but NA values exist in either gene symbols or gene type
    unmapped_mask <- (is.na(raw_mapping$gene_symbol) | raw_mapping$gene_symbol == "") |
                     (is.na(raw_mapping$gene_biotype) | raw_mapping$gene_biotype == "")
    raw_mapping$entrez_id[unmapped_mask] <- NA
    
    raw_mapping$gene_biotype[is.na(raw_mapping$gene_biotype) | raw_mapping$gene_biotype == ""] <- "unknown"
    
    # Cache the raw mapping results if not already cached
    raw_mapping_file <- get_session_path(ds_id, "%s_raw_mapping.rds")
    if (!file.exists(raw_mapping_file)) {
      cached_data <- list(
        raw_mapping = raw_mapping,
        organism = d_organism,
        gene_id_type = gene_id_type,
        keys = keys,
        microarrayPlatformId = d$microarrayPlatformId,
        platformFamily = d$platformFamily
      )
      saveRDS(cached_data, raw_mapping_file)
    }
    
    # ── Phase 3: Filter out unmapped features ──
    # A profile is mapped if it has non-NA, non-empty entrez_id, gene_symbol, and gene_biotype
    is_mapped_profile <- !is.na(raw_mapping$entrez_id) & raw_mapping$entrez_id != "" &
                         !is.na(raw_mapping$gene_symbol) & raw_mapping$gene_symbol != "" &
                         !is.na(raw_mapping$gene_biotype) & raw_mapping$gene_biotype != ""
                         
    # Compute return stats based on the initial mapping
    n_total_input <- length(keys)
    n_mapped <- sum(keys %in% raw_mapping$input_id[is_mapped_profile])
    n_unmapped <- n_total_input - n_mapped
    n_unique_symbols <- length(unique(raw_mapping$gene_symbol[is_mapped_profile]))
    
    # Keep only the mapped profiles for further resolution
    mapped_mapping <- raw_mapping[is_mapped_profile, ]
    
    # Check if the input ID type is Entrez ID
    is_input_entrez <- tolower(gene_id_type) %in% c("entrez", "entrezid", "entrez_id")
    
    if (is_input_entrez) {
      # If input is Entrez ID: multi-mapped means an input_id (Entrez ID) maps to multiple distinct biotypes
      biotype_map <- unique(mapped_mapping[, c("input_id", "gene_biotype")])
      input_counts <- table(biotype_map$input_id)
      all_multi_input_ids <- names(input_counts[input_counts > 1])
    } else {
      # If input is not Entrez ID: multi-mapped means an input_id maps to multiple distinct Entrez IDs
      entrez_map <- unique(mapped_mapping[, c("input_id", "entrez_id")])
      input_counts <- table(entrez_map$input_id)
      all_multi_input_ids <- names(input_counts[input_counts > 1])
    }
    n_multi_features <- sum(keys %in% all_multi_input_ids)
    
    # ── Phase 4: Apply Multi-Mapping Resolution & Collapse ──
    strategy_val <- tolower(d_strategy)
    
    if (strategy_val == "keep-first" || strategy_val == "keep first") {
      # Keep first profile for each input ID
      resolved_mapping <- mapped_mapping[!duplicated(mapped_mapping$input_id), ]
    } else {
      # "filter" (or default to filter since keep-all is removed)
      # Remove all multi-mapped input IDs
      resolved_mapping <- mapped_mapping[!(mapped_mapping$input_id %in% all_multi_input_ids), ]
      resolved_mapping <- resolved_mapping[!duplicated(resolved_mapping$input_id), ]
    }
    
    # Align expression matrix to remaining keys
    expr_filtered <- expr_collapsed[resolved_mapping$input_id, , drop = FALSE]
    
    # Collapse duplicate Entrez IDs (sum for readcounts, mean for other platforms)
    cat(sprintf("  [COLLAPSE] Collapsing post-resolution duplicate Entrez IDs for dataset %s...\n", ds_id))
    expr_filtered <- collapse_duplicate_ids_parallel(expr_filtered, resolved_mapping$entrez_id, d_data_type, is_norm)
    
    # Collapse resolved_mapping table on Entrez ID keeping the first occurrence of gene_symbol and gene_biotype
    resolved_mapping <- resolved_mapping[!duplicated(resolved_mapping$entrez_id), ]
    resolved_mapping <- resolved_mapping[match(rownames(expr_filtered), resolved_mapping$entrez_id), ]
    
    # Filter by biotype (comes before gene symbol collapse to prevent pollution of name space)
    if (!is.null(d_biotype) && length(d_biotype) > 0 && !("all" %in% tolower(d_biotype))) {
      # Normalize database biotypes
      db_biotypes_clean <- tolower(resolved_mapping$gene_biotype)
      db_biotypes_clean <- gsub("-", "_", db_biotypes_clean, fixed = TRUE)
      db_biotypes_clean <- gsub(" ", "_", db_biotypes_clean, fixed = TRUE)
      db_biotypes_clean[db_biotypes_clean == "pseudo"] <- "pseudogene"
      db_biotypes_clean[db_biotypes_clean == "ncrna"] <- "lncrna"
      
      # Normalize client biotypes
      client_biotypes_clean <- tolower(d_biotype)
      client_biotypes_clean <- gsub("-", "_", client_biotypes_clean, fixed = TRUE)
      client_biotypes_clean <- gsub(" ", "_", client_biotypes_clean, fixed = TRUE)
      
      # Apply filter
      keep_biotype_idx <- db_biotypes_clean %in% client_biotypes_clean
      resolved_mapping <- resolved_mapping[keep_biotype_idx, ]
      expr_filtered <- expr_filtered[resolved_mapping$entrez_id, , drop = FALSE]
    }
    
    # Collapse duplicate Gene Symbols (sum for readcounts, mean for other platforms)
    cat(sprintf("  [COLLAPSE] Collapsing post-resolution duplicate Gene Symbols for dataset %s...\n", ds_id))
    expr_filtered <- collapse_duplicate_ids_parallel(expr_filtered, resolved_mapping$gene_symbol, d_data_type, is_norm)
    
    # Collapse resolved_mapping table on gene_symbol keeping the first occurrence
    resolved_mapping <- resolved_mapping[!duplicated(resolved_mapping$gene_symbol), ]
    resolved_mapping <- resolved_mapping[match(rownames(expr_filtered), resolved_mapping$gene_symbol), ]
    
    # Rownames and identifier column must be gene_symbols moving forward
    resolved_mapping$gene_symbol <- make.unique(resolved_mapping$gene_symbol)
    rownames(expr_filtered) <- resolved_mapping$gene_symbol
    rownames(resolved_mapping) <- resolved_mapping$gene_symbol

    
    # ── Phase 5: Persist results ──
    dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(resolved_mapping, file = get_session_path(ds_id, "%s_resolved_mapping.rds"))
    
    push_main_stack(ds_id, expr_filtered, step_name = "annotation", metadata = list(strategy = d_strategy, organism = d_organism, biotype = d_biotype, dataType = d_data_type))
    if (isTRUE(d_data_type == "readcounts")) {
      push_counts_stack(ds_id, expr_filtered, step_name = "annotation", metadata = list(strategy = d_strategy, organism = d_organism, biotype = d_biotype, dataType = d_data_type))
    }
    
    # Construct mapping summary results for ALL original keys (Full annotation table: ID -> symbol & biotype)
    keys_all <- if (!is.null(parsed$anno) && nrow(parsed$anno) > 0) rownames(parsed$anno) else rownames(parsed$expr)
    if (is.null(keys_all)) keys_all <- parsed$gene_ids

    if (tolower(gene_id_type) == "ensembl") {
      clean_keys_all <- sub("\\.[0-9]+$", "", keys_all, perl = TRUE, useBytes = TRUE)
    } else {
      clean_keys_all <- keys_all
    }
    
    # Match clean_keys_all to raw_mapping$input_id
    match_idx <- match(clean_keys_all, raw_mapping$input_id)
    
    entrez_vec  <- raw_mapping$entrez_id[match_idx]
    symbol_vec  <- raw_mapping$gene_symbol[match_idx]
    biotype_vec <- raw_mapping$gene_biotype[match_idx]

    # Full annotation results (File 2: annotation_results)
    df_anno_res <- data.frame(
      input_id = keys_all,
      entrez_id = entrez_vec,
      gene_symbol = symbol_vec,
      gene_biotype = biotype_vec,
      stringsAsFactors = FALSE
    )
    
    # Dynamically name the first column
    colnames(df_anno_res)[1] <- paste0(tolower(gene_id_type), "_id")
    
    anno_res_rds <- get_session_path(ds_id, "%s_annotation_results.rds")
    anno_res_csv <- get_session_path(ds_id, "%s_annotation_results.csv")
    saveRDS(df_anno_res, file = anno_res_rds)
    write.csv(df_anno_res, file = anno_res_csv, row.names = FALSE)
    
    uid <- get_user_id(ds_id)
    register_export_file(uid, "annotation_results", ds_id, anno_res_rds, "dp", "annotation", ext = "csv")

    # Filtered-out gene IDs (File 3: unmapped_results - served ONLY to AnnotationStep.tsx)
    is_unmapped <- is.na(entrez_vec) | entrez_vec == "" |
                   is.na(symbol_vec) | symbol_vec == "" |
                   is.na(biotype_vec) | biotype_vec == "" | tolower(biotype_vec) == "unknown"

    is_multi_filtered <- rep(FALSE, length(clean_keys_all))
    if (strategy_val %in% c("filter", "filter-all", "filter_all")) {
      is_multi_filtered <- clean_keys_all %in% all_multi_input_ids
    }

    is_biotype_filtered <- rep(FALSE, length(clean_keys_all))
    if (!is.null(d_biotype) && length(d_biotype) > 0 && !("all" %in% tolower(d_biotype))) {
      raw_biotypes_clean <- tolower(biotype_vec)
      raw_biotypes_clean <- gsub("-", "_", raw_biotypes_clean, fixed = TRUE)
      raw_biotypes_clean <- gsub(" ", "_", raw_biotypes_clean, fixed = TRUE)
      raw_biotypes_clean[raw_biotypes_clean == "pseudo"] <- "pseudogene"
      raw_biotypes_clean[raw_biotypes_clean == "ncrna"] <- "lncrna"
      
      client_biotypes_clean <- tolower(d_biotype)
      client_biotypes_clean <- gsub("-", "_", client_biotypes_clean, fixed = TRUE)
      client_biotypes_clean <- gsub(" ", "_", client_biotypes_clean, fixed = TRUE)
      
      is_biotype_filtered <- (!is_unmapped) & !(raw_biotypes_clean %in% client_biotypes_clean)
    }

    filtered_out_mask <- is_unmapped | is_multi_filtered | is_biotype_filtered

    df_unmapped_res <- data.frame(
      input_id = keys_all[filtered_out_mask],
      entrez_id = entrez_vec[filtered_out_mask],
      gene_symbol = symbol_vec[filtered_out_mask],
      gene_biotype = biotype_vec[filtered_out_mask],
      stringsAsFactors = FALSE
    )
    colnames(df_unmapped_res)[1] <- paste0(tolower(gene_id_type), "_id")

    unmapped_res_rds <- get_session_path(ds_id, "%s_unmapped_results.rds")
    unmapped_res_csv <- get_session_path(ds_id, "%s_unmapped_results.csv")
    saveRDS(df_unmapped_res, file = unmapped_res_rds)
    write.csv(df_unmapped_res, file = unmapped_res_csv, row.names = FALSE)

    register_export_file(uid, "unmapped_results", ds_id, unmapped_res_rds, "dp", "annotation", ext = "csv")
    
    # Update cached metadata: downstream steps will use gene_symbol as the gene-ID column
    meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
    if (file.exists(meta_path)) {
      meta <- readRDS(meta_path)
      meta$geneIdCol   <- "gene_symbol"
      meta$geneIdType  <- "genename"
      meta$geneInfoCols <- c("entrez_id", "gene_symbol")
      saveRDS(meta, file = meta_path)
    }
    
    cat(sprintf("[CONSOLE]   Output features after: %d\n", nrow(expr_filtered)))
    cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(rownames(expr_filtered)))))
    cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(expr_filtered))))
    
    rebuilt <- rebuild_parsed_data(expr_filtered, resolved_mapping[, c("entrez_id", "gene_symbol"), drop = FALSE], "gene_symbol")
    pre_qc <- tryCatch(prepare_pre_processing_qc(expr_filtered, ds_id, d_data_type, is_norm, d$name), error = function(e) NULL)
    res <- list(
      datasetId  = ds_id_full,
      total      = n_total_input,
      mapped     = n_mapped,
      unmapped   = n_unmapped,
      unique     = n_unique_symbols,
      multi      = n_multi_features,
      retained   = nrow(expr_filtered),
      columns    = rebuilt$columns,
      parsedData = rebuilt$parsedData,
      sampleIds  = colnames(expr_filtered),
      nSamples   = as.integer(ncol(expr_filtered)),
      qc         = if (!is.null(pre_qc)) list(before = pre_qc, meanVariance = pre_qc$meanVariance, distribution = pre_qc$distribution, pca = pre_qc$pca) else NULL
    )
    save_step_cache_meta(ds_id, "annotation", list(
      strategy = d_strategy,
      organism = d_organism,
      biotype  = d_biotype,
      upstream_fp = upstream_fp,
      result   = res
    ))
    res
  }

  results <- if (length(datasets) > 1 && get_effective_cores() > 1) {
    run_parallel_lapply(
      datasets,
      process_single_annotation,
      var_list = c("strategy", "source", "organisms", "biotypes", "process_single_annotation",
                   "get_base_id", "normalise_biotypes", "get_backend_dataset", "get_session_path",
                   "register_export_file", "get_user_id", "push_main_stack", "push_counts_stack",
                   "collapse_duplicate_ids_parallel", "rebuild_parsed_data", "%||%",
                   "apply_illumina_probe_filters", "filter_probes_by_quality", "filter_by_detection_pval", "ILLUMINA_PROBE_QUALITY_PLATFORMS",
                   "get_step_cache_meta", "save_step_cache_meta", "get_matrix_fingerprint", "ensure_sequential_matrices",
                   "get_local_r_lib", "read_csv_preserve_id", "get_suitable_expression_matrix", "safe_str",
                   "get_dataset_display_name", "get_sample_clinical_vector", "compute_normal_pca",
                   "compute_single_qc", "build_hexbin_mv_ggplot", "build_dist_ggplot", "build_pca_ggplot",
                   "save_plot_formats", "prepare_pre_processing_qc", "get_clinical_path", "get_clin_metadata_path", "parse_clinical_data"),
      pkg_list = c("jsonlite", "edgeR", "limma", "AnnotationDbi", "ggplot2", "matrixStats")
    )
  } else {
    lapply(datasets, process_single_annotation)
  }

  return(results)
}

# ── QC & Two-Entity PCA Helpers ──────────────────────────────────────────────

get_dataset_display_name <- function(ds_id, d_name = NULL) {
  base_id <- get_base_id(ds_id)
  if (!is.null(d_name) && nzchar(trimws(as.character(d_name))) &&
      !identical(as.character(d_name), as.character(ds_id)) &&
      !identical(as.character(d_name), as.character(base_id))) {
    return(trimws(as.character(d_name)))
  }
  meta_candidates <- c(
    get_session_path(base_id, "%s_expr_metadata.rds", fallback = TRUE),
    sprintf("tmp/%s_expr_metadata.rds", base_id),
    get_session_path(base_id, "%s_upload_expr_metadata.rds", fallback = TRUE),
    sprintf("tmp/%s_upload_expr_metadata.rds", base_id)
  )
  for (mp in unique(meta_candidates)) {
    if (file.exists(mp)) {
      meta <- tryCatch(readRDS(mp), error = function(e) NULL)
      cand <- meta$datasetName %||% meta$name %||% ""
      if (nzchar(trimws(as.character(cand))) &&
          !identical(as.character(cand), as.character(ds_id)) &&
          !identical(as.character(cand), as.character(base_id))) {
        return(trimws(as.character(cand)))
      }
    }
  }
  if (grepl("_d([0-9]+)$", base_id)) {
    return(sub("^.*_d([0-9]+)$", "Dataset \\1", base_id))
  }
  return(base_id)
}

get_sample_clinical_vector <- function(ds_id, samples, col_name = NULL, fallback_cols = character(0), default_val = "Sample") {
  base_id <- get_base_id(ds_id)
  out_vec <- rep(as.character(default_val), length(samples))
  if (length(samples) == 0) return(out_vec)

  clin_path <- get_clinical_path(base_id)
  if (!file.exists(clin_path)) {
    clin_path_full <- get_clinical_path(ds_id)
    if (file.exists(clin_path_full)) clin_path <- clin_path_full
  }
  clin_meta_path <- get_clin_metadata_path(base_id)
  if (!file.exists(clin_meta_path)) {
    clin_meta_path_full <- get_clin_metadata_path(ds_id)
    if (file.exists(clin_meta_path_full)) clin_meta_path <- clin_meta_path_full
  }

  meta <- if (file.exists(clin_meta_path)) tryCatch(readRDS(clin_meta_path), error = function(e) NULL) else NULL
  sample_id_col <- meta$sampleIdCol %||% ""

  clin_df <- NULL
  if (file.exists(clin_path)) {
    raw_clin <- tryCatch(read.csv(clin_path, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(raw_clin) && ncol(raw_clin) > 0) {
      if (!nzchar(sample_id_col)) sample_id_col <- colnames(raw_clin)[1]
      clin_df <- tryCatch(parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col), error = function(e) NULL)
    }
  }
  if (is.null(clin_df) || nrow(clin_df) == 0) return(out_vec)

  target_col <- col_name %||% ""
  if (!nzchar(target_col) || !(target_col %in% colnames(clin_df))) {
    meta_col <- meta$groupCol %||% ""
    if (nzchar(meta_col) && (meta_col %in% colnames(clin_df))) {
      target_col <- meta_col
    } else if (length(fallback_cols) > 0) {
      found <- intersect(fallback_cols, colnames(clin_df))
      if (length(found) > 0) target_col <- found[1]
    }
  }

  if (nzchar(target_col) && (target_col %in% colnames(clin_df))) {
    matched <- match(samples, rownames(clin_df))
    valid_m <- !is.na(matched)
    if (any(valid_m)) {
      vals <- as.character(clin_df[matched[valid_m], target_col])
      vals[is.na(vals) | !nzchar(trimws(vals))] <- default_val
      out_vec[valid_m] <- vals
    }
  }
  return(out_vec)
}

compute_normal_pca <- function(expr_mat, ds_id, is_unnormalized_rc = FALSE, ds_name = NULL, save_latest = TRUE) {
  if (is.null(expr_mat) || !is.matrix(expr_mat) || nrow(expr_mat) < 2 || ncol(expr_mat) < 2) return(NULL)
  base_id <- get_base_id(ds_id)
  disp_name <- get_dataset_display_name(base_id, ds_name)

  # Work strictly on a temporary copy so original expression matrix is never modified
  expr_pca <- as.matrix(expr_mat)
  if (any(is.na(expr_pca))) {
    rm_vals <- rowMeans(expr_pca, na.rm = TRUE)
    rm_vals[!is.finite(rm_vals)] <- 0
    na_idx <- which(is.na(expr_pca), arr.ind = TRUE)
    if (nrow(na_idx) > 0) expr_pca[na_idx] <- rm_vals[na_idx[, 1]]
  }

  if (isTRUE(is_unnormalized_rc)) {
    expr_pca <- log2(pmax(expr_pca, 0) + 1)
  }

  row_vars <- matrixStats::rowVars(expr_pca, na.rm = TRUE)
  keep <- is.finite(row_vars) & row_vars > 1e-8
  if (sum(keep) < 2) return(NULL)
  expr_pca <- expr_pca[keep, , drop = FALSE]

  pca_fit <- tryCatch(prcomp(t(expr_pca), scale. = TRUE), error = function(e) NULL)
  if (is.null(pca_fit) || ncol(pca_fit$x) < 1) return(NULL)

  coords <- pca_fit$x[, seq_len(min(2L, ncol(pca_fit$x))), drop = FALSE]
  if (ncol(coords) < 2) {
    coords <- cbind(coords, rep(0, nrow(coords)))
  }
  total_var <- sum(pca_fit$sdev^2)
  var_exp <- if (total_var > 0) pca_fit$sdev^2 / total_var else c(0, 0)
  pc1_lbl <- sprintf("PC1 (%.1f%%)", (var_exp[1] %||% 0) * 100)
  pc2_lbl <- sprintf("PC2 (%.1f%%)", (if (length(var_exp) >= 2) var_exp[2] else 0) * 100)

  samples <- colnames(expr_pca) %||% rownames(coords) %||% paste0("Sample_", seq_len(nrow(coords)))
  group_vec <- get_sample_clinical_vector(
    base_id, samples, col_name = NULL,
    fallback_cols = c("Group", "group", "Condition", "condition", "Status", "Class", "Phenotype"),
    default_val = "All Samples"
  )

  pts <- lapply(seq_along(samples), function(i) {
    list(
      sample  = as.character(samples[i]),
      pc1     = round(unname(as.numeric(coords[i, 1])), 4),
      pc2     = round(unname(as.numeric(coords[i, 2])), 4),
      group   = as.character(group_vec[i]),
      dataset = disp_name
    )
  })

  pca_obj <- list(
    pc1Label          = pc1_lbl,
    pc2Label          = pc2_lbl,
    title             = disp_name,
    isLog2Transformed = isTRUE(is_unnormalized_rc),
    points            = pts
  )

  if (isTRUE(save_latest)) {
    tryCatch(saveRDS(pca_obj, get_session_path(base_id, "%s_latest_normal_pca.rds")), error = function(e) NULL)
  }
  return(pca_obj)
}

compute_single_qc <- function(expr_mat, ds_id, is_unnormalized_rc = FALSE, ds_name = NULL, save_latest_pca = TRUE) {
  if (is.null(expr_mat) || nrow(expr_mat) < 2 || ncol(expr_mat) < 1) return(NULL)
  base_id <- get_base_id(ds_id)
  disp_name <- get_dataset_display_name(base_id, ds_name)

  row_means <- rowMeans(expr_mat, na.rm = TRUE)
  row_vars  <- matrixStats::rowVars(as.matrix(expr_mat), na.rm = TRUE)
  valid_mv  <- is.finite(row_means) & is.finite(row_vars)

  m_clean <- row_means[valid_mv]
  v_clean <- row_vars[valid_mv]
  names_clean <- rownames(expr_mat)[valid_mv] %||% paste0("Feature_", seq_along(m_clean))

  fin_vals <- expr_mat[is.finite(expr_mat)]
  if (length(fin_vals) > 10) {
    lo <- as.numeric(quantile(fin_vals, 0.005, na.rm = TRUE))
    hi <- as.numeric(quantile(fin_vals, 0.995, na.rm = TRUE))
  } else if (length(fin_vals) > 0) {
    lo <- min(fin_vals, na.rm = TRUE)
    hi <- max(fin_vals, na.rm = TRUE)
  } else {
    lo <- 0; hi <- 1
  }
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
    lo <- 0; hi <- 1
  }
  grid_vals <- seq(lo, hi, length.out = 61)

  sample_densities <- lapply(seq_len(ncol(expr_mat)), function(j) {
    s_vals <- expr_mat[is.finite(expr_mat[, j]), j]
    s_name <- colnames(expr_mat)[j] %||% sprintf("Sample_%d", j)
    if (length(s_vals) > 1 && diff(range(s_vals)) > 0) {
      d <- density(s_vals, from = lo, to = hi, n = 61)
      y_pts <- d$y
    } else {
      y_pts <- rep(0, 61)
    }
    list(name = s_name, y = round(unname(y_pts), 4))
  })

  pca_res <- compute_normal_pca(expr_mat, base_id, is_unnormalized_rc = is_unnormalized_rc, ds_name = disp_name, save_latest = save_latest_pca)

  return(list(
    datasetName = disp_name,
    meanVariance = list(
      means = round(unname(m_clean), 4),
      vars  = round(unname(v_clean), 4),
      names = as.character(names_clean)
    ),
    distribution = list(
      grid    = round(unname(grid_vals), 4),
      samples = sample_densities
    ),
    pca = pca_res
  ))
}

build_hexbin_mv_ggplot <- function(mv_data, title_str) {
  m <- as.numeric(mv_data$means)
  v <- as.numeric(mv_data$vars)
  keep <- is.finite(m) & is.finite(v)
  m <- m[keep]; v <- v[keep]
  if (length(m) < 2) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() + ggplot2::labs(title = title_str, x = "Mean Expression", y = "Variance"))
  }

  x_min <- min(m); x_max <- max(m)
  y_min <- min(v); y_max <- max(v)
  x_span <- if (x_max > x_min) (x_max - x_min) else 1
  y_span <- if (y_max > y_min) (y_max - y_min) else 1

  nx <- 32
  ny <- 28
  dx <- 1 / nx
  dy <- 1 / ny
  r_x <- dx / 1.92
  r_y <- dy / 1.65

  u <- (m - x_min) / x_span
  w <- (v - y_min) / y_span

  iy <- round(w / dy)
  ix <- round((u - ifelse(iy %% 2 == 1, 0.5 * dx, 0)) / dx)
  bin_key <- paste0(ix, "_", iy)
  counts <- table(bin_key)

  uniq_keys <- names(counts)
  parts <- do.call(rbind, strsplit(uniq_keys, "_"))
  b_ix <- as.numeric(parts[, 1])
  b_iy <- as.numeric(parts[, 2])
  b_cnt <- as.numeric(counts)

  cx <- (b_ix * dx + ifelse(b_iy %% 2 == 1, 0.5 * dx, 0)) * x_span + x_min
  cy <- (b_iy * dy) * y_span + y_min

  angles <- seq(pi / 6, 2 * pi, by = pi / 3)
  cos_a <- cos(angles) * r_x * x_span
  sin_a <- sin(angles) * r_y * y_span

  n_bins <- length(uniq_keys)
  poly_df <- data.frame(
    hex_id = rep(seq_len(n_bins), each = 6),
    x      = as.vector( outer(cos_a, cx, "+") ),
    y      = as.vector( outer(sin_a, cy, "+") ),
    count  = rep(b_cnt, each = 6)
  )
  df_mv <- data.frame(Mean = m, Variance = v)

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = poly_df,
      ggplot2::aes(x = x, y = y, group = hex_id, fill = count),
      color = "#ffffff", linewidth = 0.08
    ) +
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "Features", option = "D") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = title_str, x = "Mean Expression", y = "Variance") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))

  if (nrow(df_mv) >= 6) {
    p <- p + tryCatch(
      ggplot2::geom_smooth(
        data = df_mv,
        ggplot2::aes(x = Mean, y = Variance),
        color = "#ef4444", se = FALSE, linewidth = 0.9
      ),
      error = function(e) NULL
    )
  }
  return(p)
}

build_dist_ggplot <- function(dist_data, title_str) {
  grid_vals <- as.numeric(dist_data$grid)
  samples <- dist_data$samples
  if (is.null(samples) || length(samples) == 0 || length(grid_vals) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() + ggplot2::labs(title = title_str, x = "Expression Value", y = "Density"))
  }
  df_list <- lapply(samples, function(sd) {
    data.frame(
      x = grid_vals,
      density = as.numeric(sd$y),
      sample = as.character(sd$name),
      stringsAsFactors = FALSE
    )
  })
  df_dist <- do.call(rbind, df_list)
  ggplot2::ggplot(df_dist, ggplot2::aes(x = x, y = density, color = sample)) +
    ggplot2::geom_line(alpha = 0.78, linewidth = 0.75) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = title_str, x = "Expression Value", y = "Density") +
    ggplot2::theme(
      legend.position = if (length(samples) <= 15) "bottom" else "none",
      plot.title = ggplot2::element_text(face = "bold", size = 11)
    )
}

build_pca_ggplot <- function(pca_data, color_by = "group", title_str = NULL) {
  if (is.null(pca_data) || is.null(pca_data$points) || length(pca_data$points) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() + ggplot2::labs(title = title_str, x = "PC1", y = "PC2"))
  }
  pts <- pca_data$points
  df_pca <- data.frame(
    Sample  = vapply(pts, function(p) as.character(p$sample %||% ""), character(1)),
    PC1     = vapply(pts, function(p) as.numeric(p$pc1 %||% 0), numeric(1)),
    PC2     = vapply(pts, function(p) as.numeric(p$pc2 %||% 0), numeric(1)),
    Group   = vapply(pts, function(p) as.character(p$group %||% "All Samples"), character(1)),
    Batch   = vapply(pts, function(p) as.character(p$batch %||% p$group %||% "Batch1"), character(1)),
    Dataset = vapply(pts, function(p) as.character(p$dataset %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
  col_var <- if (identical(tolower(color_by), "batch")) "Batch" else if (identical(tolower(color_by), "dataset")) "Dataset" else "Group"
  df_pca[[col_var]] <- as.factor(df_pca[[col_var]])

  sub_txt <- if (isTRUE(pca_data$isLog2Transformed)) "Computed on log2(count + 1) temporary copy" else NULL

  ggplot2::ggplot(df_pca, ggplot2::aes(x = PC1, y = PC2, color = .data[[col_var]])) +
    ggplot2::geom_point(size = 3.2, alpha = 0.86) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      title    = title_str,
      subtitle = sub_txt,
      x        = pca_data$pc1Label %||% "PC1",
      y        = pca_data$pc2Label %||% "PC2",
      color    = col_var
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9, color = "#64748b")
    )
}

save_plot_formats <- function(plot_obj, pdf_path, png_path, svg_path = NULL, tiff_path = NULL, width = 7, height = 5) {
  tryCatch({
    ggplot2::ggsave(pdf_path, plot = plot_obj, width = width, height = height, dpi = 300, limitsize = FALSE)
  }, error = function(e) cat(sprintf("[PLOT] PDF save failed (%s): %s\n", pdf_path, conditionMessage(e))))
  tryCatch({
    ggplot2::ggsave(png_path, plot = plot_obj, width = width, height = height, dpi = 300, bg = "white", limitsize = FALSE)
  }, error = function(e) cat(sprintf("[PLOT] PNG save failed (%s): %s\n", png_path, conditionMessage(e))))
  if (!is.null(svg_path) && nzchar(svg_path)) {
    tryCatch({
      grDevices::svg(svg_path, width = width, height = height)
      if (inherits(plot_obj, "ggplot")) {
        print(plot_obj)
      } else {
        grid::grid.newpage()
        grid::grid.draw(plot_obj)
      }
      grDevices::dev.off()
    }, error = function(e) {
      if (grDevices::dev.cur() > 1) tryCatch(grDevices::dev.off(), error = function(e2) NULL)
    })
  }
  if (!is.null(tiff_path) && nzchar(tiff_path)) {
    tryCatch({
      ggplot2::ggsave(tiff_path, plot = plot_obj, width = width, height = height, dpi = 300, bg = "white", limitsize = FALSE)
    }, error = function(e) NULL)
  }
}

prepare_pre_processing_qc <- function(expr_mat, ds_id, data_type = "readcounts", is_normalized = FALSE, ds_name = NULL) {
  if (is.null(expr_mat) || nrow(expr_mat) < 2 || ncol(expr_mat) < 1) return(NULL)
  base_id <- get_base_id(ds_id)
  uid_val <- get_user_id(base_id)
  if (is.null(uid_val) || !nzchar(uid_val)) uid_val <- "user"

  disp_name <- get_dataset_display_name(base_id, ds_name)
  is_raw_rc <- identical(tolower(as.character(data_type %||% "readcounts")), "readcounts") && !isTRUE(is_normalized)

  saveRDS(expr_mat, get_session_path(base_id, "%s_pre_processing_expr.rds"))
  pre_qc <- compute_single_qc(expr_mat, base_id, is_unnormalized_rc = is_raw_rc, ds_name = disp_name, save_latest_pca = TRUE)
  if (is.null(pre_qc)) return(NULL)
  saveRDS(pre_qc, get_session_path(base_id, "%s_pre_qc.rds"))

  tryCatch({
    p_mv   <- build_hexbin_mv_ggplot(pre_qc$meanVariance, sprintf("%s — Before Processing (Mean-Variance)", disp_name))
    p_dist <- build_dist_ggplot(pre_qc$distribution, sprintf("%s — Before Processing (Sample Distribution)", disp_name))
    p_pca  <- build_pca_ggplot(pre_qc$pca, color_by = "group", title_str = sprintf("%s — Before Processing (Normal PCA)", disp_name))

    mv_pdf <- get_session_path(base_id, "qc_mean_variance_%s.pdf")
    mv_png <- get_session_path(base_id, "qc_mean_variance_%s.png")
    mv_svg <- get_session_path(base_id, "qc_mean_variance_%s.svg")
    save_plot_formats(p_mv, mv_pdf, mv_png, mv_svg, width = 7, height = 4.8)
    register_export_file(uid_val, "qc_mean_variance", base_id, mv_pdf, "dp", "processing", ext = "pdf")

    dist_pdf <- get_session_path(base_id, "qc_distribution_%s.pdf")
    dist_png <- get_session_path(base_id, "qc_distribution_%s.png")
    dist_svg <- get_session_path(base_id, "qc_distribution_%s.svg")
    save_plot_formats(p_dist, dist_pdf, dist_png, dist_svg, width = 7, height = 4.8)
    register_export_file(uid_val, "qc_distribution", base_id, dist_pdf, "dp", "processing", ext = "pdf")

    qcpca_pdf <- get_session_path(base_id, "qc_pca_%s.pdf")
    qcpca_png <- get_session_path(base_id, "qc_pca_%s.png")
    qcpca_svg <- get_session_path(base_id, "qc_pca_%s.svg")
    save_plot_formats(p_pca, qcpca_pdf, qcpca_png, qcpca_svg, width = 7, height = 4.8)
    register_export_file(uid_val, "qc_pca", base_id, qcpca_pdf, "dp", "processing", ext = "pdf")

    pca_pdf <- get_session_path(base_id, "pca_plots_%s.pdf")
    pca_png <- get_session_path(base_id, "pca_plots_%s.png")
    pca_svg <- get_session_path(base_id, "pca_plots_%s.svg")
    save_plot_formats(p_pca, pca_pdf, pca_png, pca_svg, width = 6.5, height = 4.8)
    register_export_file(uid_val, "pca_plots", base_id, pca_pdf, "dp", "processing", ext = "pdf")
  }, error = function(e) {
    cat(sprintf("[QC] Pre-processing plot file generation warning: %s\n", conditionMessage(e)))
  })

  return(pre_qc)
}

run_pre_processing_qc <- function(datasets) {
  results <- list()
  for (d in datasets) {
    ds_id_full <- d$datasetId %||% d$id
    base_id <- get_base_id(ds_id_full)
    pre_qc_path <- get_session_path(base_id, "%s_pre_qc.rds")
    pre_qc <- if (file.exists(pre_qc_path)) tryCatch(readRDS(pre_qc_path), error = function(e) NULL) else NULL
    if (is.null(pre_qc)) {
      expr_in <- get_latest_step_matrix(base_id, "processing")
      if (is.null(expr_in)) {
        parsed <- get_backend_dataset(ds_id_full, step = "processing")
        expr_in <- parsed$expr
      }
      meta_info <- tryCatch(readRDS(get_session_path(base_id, "%s_expr_metadata.rds", fallback = TRUE)), error = function(e) NULL)
      dtype <- d$dataType %||% meta_info$dataType %||% "readcounts"
      is_n  <- if (!is.null(d$isNormalized)) isTRUE(d$isNormalized) else isTRUE(meta_info$isNormalized)
      pre_qc <- prepare_pre_processing_qc(expr_in, base_id, dtype, is_n, d$name)
    }
    results[[length(results) + 1]] <- list(
      datasetId = ds_id_full,
      qc = if (!is.null(pre_qc)) list(
        before       = pre_qc,
        meanVariance = pre_qc$meanVariance,
        distribution = pre_qc$distribution,
        pca          = pre_qc$pca
      ) else NULL
    )
  }
  return(results)
}

# 2. Data Filtering and Imputation Handler
generate_qc_plots <- function(expr, ds_id, uid_val = NULL, ds_name = NULL, before_expr = NULL, is_unnormalized_rc = NULL) {
  if (is.null(expr) || nrow(expr) < 2 || ncol(expr) < 1) return(NULL)
  base_id <- get_base_id(ds_id)
  if (is.null(uid_val) || !nzchar(uid_val)) uid_val <- get_user_id(base_id)
  if (is.null(uid_val) || !nzchar(uid_val)) uid_val <- "user"

  disp_name <- get_dataset_display_name(base_id, ds_name)
  if (is.null(is_unnormalized_rc)) {
    meta_info <- tryCatch(readRDS(get_session_path(base_id, "%s_expr_metadata.rds", fallback = TRUE)), error = function(e) NULL)
    dtype <- meta_info$dataType %||% "readcounts"
    is_n  <- isTRUE(meta_info$isNormalized)
    is_unnormalized_rc <- identical(tolower(as.character(dtype)), "readcounts") && !is_n
  }

  # 1. Retrieve already-stored Before-Processing QC (or compute from before_expr if missing)
  pre_qc_path <- get_session_path(base_id, "%s_pre_qc.rds")
  before_qc <- if (file.exists(pre_qc_path)) tryCatch(readRDS(pre_qc_path), error = function(e) NULL) else NULL
  if (is.null(before_qc)) {
    b_mat <- before_expr
    if (is.null(b_mat)) {
      pre_mat_path <- get_session_path(base_id, "%s_pre_processing_expr.rds")
      if (file.exists(pre_mat_path)) b_mat <- tryCatch(readRDS(pre_mat_path), error = function(e) NULL)
    }
    if (is.null(b_mat)) b_mat <- expr
    before_qc <- compute_single_qc(b_mat, base_id, is_unnormalized_rc = is_unnormalized_rc, ds_name = disp_name, save_latest_pca = FALSE)
    if (!is.null(before_qc)) {
      tryCatch(saveRDS(before_qc, pre_qc_path), error = function(e) NULL)
    }
  } else {
    before_qc$datasetName <- disp_name
    if (!is.null(before_qc$pca)) before_qc$pca$title <- disp_name
  }

  # 2. Compute After-Processing QC (and update %s_latest_normal_pca.rds)
  after_qc <- compute_single_qc(expr, base_id, is_unnormalized_rc = is_unnormalized_rc, ds_name = disp_name, save_latest_pca = TRUE)
  if (is.null(after_qc)) return(NULL)

  # 3. Render 2-row stacked (Upper = Before, Lower = After) QC plot files & single Normal PCA plot
  tryCatch({
    p_mv_before <- build_hexbin_mv_ggplot(before_qc$meanVariance, sprintf("%s — Before Processing (Mean-Variance)", disp_name))
    p_mv_after  <- build_hexbin_mv_ggplot(after_qc$meanVariance,  sprintf("%s — After Processing (Mean-Variance)", disp_name))
    g_mv <- gridExtra::arrangeGrob(p_mv_before, p_mv_after, ncol = 1)

    mv_pdf <- get_session_path(base_id, "qc_mean_variance_%s.pdf")
    mv_png <- get_session_path(base_id, "qc_mean_variance_%s.png")
    mv_svg <- get_session_path(base_id, "qc_mean_variance_%s.svg")
    save_plot_formats(g_mv, mv_pdf, mv_png, mv_svg, width = 7.2, height = 8.8)
    register_export_file(uid_val, "qc_mean_variance", base_id, mv_pdf, "dp", "processing", ext = "pdf")

    p_dist_before <- build_dist_ggplot(before_qc$distribution, sprintf("%s — Before Processing (Sample Distribution)", disp_name))
    p_dist_after  <- build_dist_ggplot(after_qc$distribution,  sprintf("%s — After Processing (Sample Distribution)", disp_name))
    g_dist <- gridExtra::arrangeGrob(p_dist_before, p_dist_after, ncol = 1)

    dist_pdf <- get_session_path(base_id, "qc_distribution_%s.pdf")
    dist_png <- get_session_path(base_id, "qc_distribution_%s.png")
    dist_svg <- get_session_path(base_id, "qc_distribution_%s.svg")
    save_plot_formats(g_dist, dist_pdf, dist_png, dist_svg, width = 7.2, height = 8.8)
    register_export_file(uid_val, "qc_distribution", base_id, dist_pdf, "dp", "processing", ext = "pdf")

    p_pca_before <- build_pca_ggplot(before_qc$pca, color_by = "group", title_str = sprintf("%s — Before Processing (Normal PCA)", disp_name))
    p_pca_after  <- build_pca_ggplot(after_qc$pca,  color_by = "group", title_str = sprintf("%s — After Processing (Normal PCA)", disp_name))
    g_pca <- gridExtra::arrangeGrob(p_pca_before, p_pca_after, ncol = 1)

    qcpca_pdf <- get_session_path(base_id, "qc_pca_%s.pdf")
    qcpca_png <- get_session_path(base_id, "qc_pca_%s.png")
    qcpca_svg <- get_session_path(base_id, "qc_pca_%s.svg")
    save_plot_formats(g_pca, qcpca_pdf, qcpca_png, qcpca_svg, width = 7.2, height = 8.8)
    register_export_file(uid_val, "qc_pca", base_id, qcpca_pdf, "dp", "processing", ext = "pdf")

    # Entity 1: Normal PCA plot (latest processed state, overwritten later only by Normalization Normal PCA)
    p_normal_latest <- build_pca_ggplot(after_qc$pca, color_by = "group", title_str = sprintf("%s — Normal PCA (Processed)", disp_name))
    pca_pdf <- get_session_path(base_id, "pca_plots_%s.pdf")
    pca_png <- get_session_path(base_id, "pca_plots_%s.png")
    pca_svg <- get_session_path(base_id, "pca_plots_%s.svg")
    save_plot_formats(p_normal_latest, pca_pdf, pca_png, pca_svg, width = 6.5, height = 4.8)
    register_export_file(uid_val, "pca_plots", base_id, pca_pdf, "dp", "processing", ext = "pdf")
  }, error = function(e) {
    cat(sprintf("[QC] Plot file generation failed: %s\n", conditionMessage(e)))
  })

  return(list(
    datasetName  = disp_name,
    before       = before_qc,
    after        = after_qc,
    meanVariance = after_qc$meanVariance,
    distribution = after_qc$distribution,
    pca          = after_qc$pca
  ))
}

ensure_qc_plots <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  mv_pdf <- get_session_path(base_id, "qc_mean_variance_%s.pdf")
  dist_pdf <- get_session_path(base_id, "qc_distribution_%s.pdf")
  qcpca_pdf <- get_session_path(base_id, "qc_pca_%s.pdf")
  if (file.exists(mv_pdf) && file.exists(dist_pdf) && file.exists(qcpca_pdf)) return(invisible(TRUE))

  expr <- NULL
  candidates <- list(
    get_session_path(base_id, "%s_processed_expr.rds"),
    sprintf("tmp/%s_processed.rds", base_id),
    get_session_path(base_id, "%s_expr_matrix_annotated.rds"),
    get_session_path(base_id, "%s_expr_matrix.rds")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      expr <- tryCatch(readRDS(p), error = function(e) NULL)
      if (!is.null(expr) && is.matrix(expr) && nrow(expr) > 0) break
      expr <- NULL
    }
  }
  if (is.null(expr)) return(invisible(FALSE))
  generate_qc_plots(expr, base_id)
  invisible(TRUE)
}

process_datasets <- function(payload) {
  cat("[PROCESSING] Filtering and imputing missing data...\n")

  process_single_processing <- function(d) {
    ds_id_full <- d$datasetId
    ds_id <- get_base_id(ds_id_full)
    
    # Check if processing is explicitly skipped
    if (identical(d$filterMethod, "skip")) {
      cat(sprintf("  Skipping processing for dataset %s\n", ds_id))
      if (file.exists(sprintf("tmp/%s_processed.rds", ds_id))) file.remove(sprintf("tmp/%s_processed.rds", ds_id))
      if (file.exists(sprintf("tmp/%s_processing.csv", ds_id))) file.remove(sprintf("tmp/%s_processing.csv", ds_id))
      
      # Copy from processed (annotation) or raw
      expr <- get_latest_step_matrix(ds_id, "processing")
      if (!is.null(expr)) {
        saveRDS(expr, file = sprintf("tmp/%s_processed_expr.rds", ds_id))
        saveRDS(expr, file = sprintf("tmp/%s_expr_matrix.rds", ds_id))
      }
      
      parsed <- get_backend_dataset(ds_id_full, step = "processing")
      if (is.null(parsed)) return(list(datasetId = ds_id_full, inputFeatures = 0, retainedFeatures = 0))
      
      cat(sprintf("[CONSOLE] Dataset: %s (processing skipped)\n", ds_id_full))
      cat(sprintf("[CONSOLE]   Output features after: %d\n", length(parsed$gene_ids)))
      cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(parsed$gene_ids))))
      cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(parsed$expr))))

      rebuilt <- rebuild_parsed_data(parsed$expr, parsed$anno, parsed$geneIdCol)
      qc_res <- tryCatch(generate_qc_plots(parsed$expr, ds_id, get_user_id(ds_id), ds_name = d$name, before_expr = parsed$expr), error = function(e) NULL)

      return(list(
        datasetId = ds_id_full,
        inputFeatures = length(parsed$gene_ids),
        removedFeatures = 0,
        retainedFeatures = length(parsed$gene_ids),
        missingValuesCount = 0,
        parsedData = rebuilt$parsedData,
        columns = rebuilt$columns,
        sampleIds = colnames(parsed$expr),
        nSamples = as.integer(ncol(parsed$expr)),
        qc = qc_res
      ))
    }
    
    # Load input from the most recent step with results
    expr <- get_latest_step_matrix(ds_id, "processing")
    
    parsed <- get_backend_dataset(ds_id_full, step = "processing")
    if (is.null(parsed)) return(list(datasetId = ds_id_full, inputFeatures = 0, retainedFeatures = 0))
    
    if (is.null(expr)) {
      expr <- parsed$expr
    }
    expr_before_proc <- expr

    # ── Tier 1 Cache Check ───────────────────────────────────────────────────
    step_cache <- get_step_cache_meta(ds_id, "processing")
    upstream_fp <- get_matrix_fingerprint(expr)
    out_file <- get_session_path(ds_id, "%s_processed_expr.rds")
    
    if (!is.null(step_cache) &&
        identical(step_cache$filterMethod, d$filterMethod) &&
        identical(step_cache$filterThreshold, d$filterParams$varianceThreshold) &&
        identical(step_cache$readcountThreshold, d$filterParams$readcountThreshold) &&
        identical(step_cache$abundanceMethod, d$filterParams$abundanceMethod) &&
        identical(step_cache$abundanceThreshold, d$filterParams$abundanceThreshold) &&
        identical(step_cache$abundanceSamplePercent, d$filterParams$abundanceSamplePercent) &&
        identical(step_cache$minCpm, d$filterParams$cpmThreshold) &&
        identical(step_cache$minSamples, d$filterParams$minSamples) &&
        identical(step_cache$naRemovePercent, d$naRemovePercent) &&
        identical(step_cache$missingMethod, d$missingMethod) &&
        identical(step_cache$kNeighbors, d$missingParams$knnK) &&
        identical(step_cache$upstream_fp, upstream_fp) &&
        file.exists(out_file)) {
      cat(sprintf("[CACHE] Reusing cached processing for dataset %s\n", ds_id))
      processed_mat <- tryCatch(readRDS(out_file), error = function(e) NULL)
      if (!is.null(processed_mat)) {
        push_main_stack(ds_id, processed_mat, step_name = "processing", metadata = list(filterMethod = d$filterMethod, missingMethod = d$missingMethod))
        meta_info <- tryCatch(readRDS(get_session_path(ds_id, "%s_expr_metadata.rds")), error = function(e) NULL)
        dtype <- if (!is.null(d$dataType)) d$dataType else if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts"
        is_n <- if (!is.null(d$isNormalized)) isTRUE(d$isNormalized) else if (!is.null(meta_info$isNormalized)) isTRUE(meta_info$isNormalized) else FALSE
        if (dtype == "readcounts" && !is_n) {
          push_counts_stack(ds_id, processed_mat, step_name = "processing", metadata = list(filterMethod = d$filterMethod, missingMethod = d$missingMethod))
        }
        ensure_sequential_matrices(ds_id)
        return(step_cache$result)
      }
    }
    
    cat(sprintf("[CONSOLE] Dataset: %s (processing)\n", ds_id))
    cat(sprintf("[CONSOLE]   Input features before: %d\n", nrow(expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs before: %d\n", sum(duplicated(rownames(expr)))))
    cat(sprintf("[CONSOLE]   Missing values before: %d\n", sum(is.na(expr))))
    
    anno <- parsed$anno
    # Align annotation to current matrix rownames using fast integer matching
    anno_idx <- match(rownames(expr), rownames(anno))
    anno <- anno[!is.na(anno_idx), , drop = FALSE]
    
    # Step 1: Remove features with too much missing values (first in the pipeline)
    na_remove_percent <- d$naRemovePercent
    if (!is.null(na_remove_percent) && !identical(na_remove_percent, "") && any(is.na(expr))) {
      na_pct_val <- as.numeric(na_remove_percent) / 100
      cat(sprintf("  Removing features with missing values in more than %s%% of samples (threshold ratio: %s)...\n", na_remove_percent, na_pct_val))
      
      na_proportions <- rowSums(is.na(expr)) / ncol(expr)
      keep_rows <- na_proportions <= na_pct_val
      
      expr <- expr[keep_rows, , drop = FALSE]
      anno <- anno[keep_rows, , drop = FALSE]
      cat(sprintf("  After removing features with excessive missing values: %d features remain\n", nrow(expr)))
    }

    # Step 2: Missing Value Handling (Imputation)
    missing_method <- d$missingMethod
    should_handle_na <- (isTRUE(d$isNA) || (!is.null(missing_method) && missing_method != "none" && missing_method != "skip")) && any(is.na(expr))
    if (should_handle_na) {
      cat(sprintf("  Imputing missing values using method: %s...\n", missing_method))
      na_indices <- is.na(expr)
      
      if (missing_method == "knn") {
        expr <- tryCatch({
          k_val <- if (!is.null(d$missingParams$knnK)) as.numeric(d$missingParams$knnK) else 10
          na_rows <- which(rowSums(is.na(expr)) > 0)
          if (length(na_rows) == 0) {
            expr
          } else {
            comp_rows <- which(rowSums(is.na(expr)) == 0)
            if (length(na_rows) < nrow(expr) && length(comp_rows) > 0) {
              max_donors <- min(length(comp_rows), 3000L)
              donor_idx <- if (length(comp_rows) > max_donors) sample(comp_rows, max_donors) else comp_rows
              sub_idx <- c(na_rows, donor_idx)
              sub_mat <- expr[sub_idx, , drop = FALSE]
              imp_sub <- impute::impute.knn(sub_mat, k = min(k_val, length(donor_idx)), maxp = max(3000L, length(sub_idx)))$data
              expr[na_rows, ] <- imp_sub[seq_along(na_rows), , drop = FALSE]
              expr
            } else {
              impute::impute.knn(expr, k = k_val, maxp = max(3000L, nrow(expr)))$data
            }
          }
        }, error = function(e) {
          # Fallback to mean imputation if KNN errors out or impute package not loaded
          row_means <- rowMeans(expr, na.rm = TRUE)
          row_means[is.na(row_means)] <- 0
          expr[is.na(expr)] <- row_means[which(is.na(expr), arr.ind = TRUE)[, 1]]
          expr
        })
      } else if (missing_method == "mean") {
        row_means <- rowMeans(expr, na.rm = TRUE)
        row_means[is.na(row_means)] <- 0
        expr[is.na(expr)] <- row_means[which(is.na(expr), arr.ind = TRUE)[, 1]]
      } else if (missing_method == "median") {
        row_medians <- matrixStats::rowMedians(as.matrix(expr), na.rm = TRUE)
        row_medians[is.na(row_medians)] <- 0
        expr[is.na(expr)] <- row_medians[which(is.na(expr), arr.ind = TRUE)[, 1]]
      } else if (missing_method == "zero") {
        expr[is.na(expr)] <- 0
      } else if (missing_method == "remove") {
        keep_rows <- rowSums(is.na(expr)) == 0
        expr <- expr[keep_rows, , drop = FALSE]
        anno <- anno[keep_rows, , drop = FALSE]
      }

      # If raw readcounts, floor imputed values to non-negative integers to preserve discrete count properties
      meta_info <- tryCatch(readRDS(get_session_path(ds_id, "%s_expr_metadata.rds")), error = function(e) NULL)
      ds_dtype <- if (!is.null(d$dataType)) d$dataType else if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts"
      ds_is_n <- if (!is.null(d$isNormalized)) isTRUE(d$isNormalized) else if (!is.null(meta_info$isNormalized)) isTRUE(meta_info$isNormalized) else FALSE
      is_raw_rc <- identical(ds_dtype, "readcounts") && !isTRUE(ds_is_n)

      if (is_raw_rc && missing_method != "remove" && any(na_indices)) {
        cat("  Flooring imputed values to nearest integer for raw readcounts...\n")
        expr[na_indices] <- floor(pmax(expr[na_indices], 0))
      }
    }

    # Step 2: Low-count / low-variance Filtering (performed AFTER missing value handling)
    filter_method <- d$filterMethod
    if (!is.null(filter_method) && filter_method != "none" && filter_method != "skip") {
      cat(sprintf("  Filtering features using method: %s...\n", filter_method))
      
      methods_list <- trimws(unlist(strsplit(filter_method, ",")))
      
      final_keep <- rep(TRUE, nrow(expr))
      orig_libsizes <- colSums(expr, na.rm = TRUE)
      
      for (m in methods_list) {
        # Only evaluate on features that have passed the previous filters
        sub_expr <- expr[final_keep, , drop = FALSE]
        
        if (m == "cpm") {
          cpm_val <- if (!is.null(d$filterParams$cpmThreshold)) as.numeric(d$filterParams$cpmThreshold) else 1.0
          min_s <- if (!is.null(d$filterParams$minSamples)) as.integer(d$filterParams$minSamples) else 2
          
          # compute CPM safely using original library sizes via fast sweep
          cpms <- sweep(sub_expr, 2, orig_libsizes, "/") * 1e6
          keep_sub <- rowSums(cpms > cpm_val) >= min_s
          keep_sub[is.na(keep_sub)] <- FALSE
          
          # Map back to original index
          keep_indices <- rep(FALSE, nrow(expr))
          keep_indices[final_keep] <- keep_sub
          final_keep <- final_keep & keep_indices
        } else if (m == "abundance") {
          is_raw_rc <- identical(d$dataType, "readcounts") && !isTRUE(d$isNormalized)
          if (is_raw_rc) {
            sample_pct <- if (!is.null(d$filterParams$abundanceSamplePercent)) as.numeric(d$filterParams$abundanceSamplePercent) else 50
            thresh_val <- if (!is.null(d$filterParams$readcountThreshold)) as.numeric(d$filterParams$readcountThreshold) else 10

            # Logic: filter out feature if value < threshold in at least sample_pct % of samples
            # Retain if count of samples where value < threshold is strictly LESS THAN (sample_pct / 100 * total_samples)
            low_sample_count <- rowSums(sub_expr < thresh_val, na.rm = TRUE)
            low_sample_ratio <- low_sample_count / ncol(sub_expr)

            keep_sub <- (low_sample_ratio < (sample_pct / 100))
            keep_sub[is.na(keep_sub)] <- FALSE
          } else {
            abund_method <- if (!is.null(d$filterParams$abundanceMethod)) as.character(d$filterParams$abundanceMethod) else "mean"
            raw_thresh <- if (!is.null(d$filterParams$abundanceThreshold)) as.numeric(d$filterParams$abundanceThreshold) else 10

            if (abund_method == "median") {
              vals <- matrixStats::rowMedians(as.matrix(sub_expr), na.rm = TRUE)
            } else {
              vals <- rowMeans(sub_expr, na.rm = TRUE)
            }

            vals_clean <- vals
            vals_clean[is.na(vals_clean)] <- -Inf
            rk <- rank(-vals_clean, ties.method = "random")
            remain_num <- ceiling(length(vals_clean) * (1 - (raw_thresh / 100)))
            remain_num <- max(0L, min(length(vals_clean), as.integer(remain_num)))

            keep_sub <- rk <= remain_num
            keep_sub[is.na(keep_sub)] <- FALSE
          }

          keep_indices <- rep(FALSE, nrow(expr))
          keep_indices[final_keep] <- keep_sub
          final_keep <- final_keep & keep_indices
        } else if (m == "variance") {
          # Treat threshold as a percentile percentage (0 to 100) from the UI
          raw_thresh <- if (!is.null(d$filterParams$varianceThreshold)) as.numeric(d$filterParams$varianceThreshold) else 10
          probs_val <- raw_thresh / 100
          
          vars <- matrixStats::rowVars(sub_expr, na.rm = TRUE)
          var_thresh <- quantile(vars, probs = probs_val, na.rm = TRUE)
          
          keep_sub <- vars >= var_thresh
          keep_sub[is.na(keep_sub)] <- FALSE
          
          keep_indices <- rep(FALSE, nrow(expr))
          keep_indices[final_keep] <- keep_sub
          final_keep <- final_keep & keep_indices
        }
      }
      # Now apply the combined filter at the end
      expr <- expr[final_keep, , drop = FALSE]
      anno <- anno[final_keep, , drop = FALSE]
    }

    # Save sequentially and push to stack
    push_main_stack(ds_id, expr, step_name = "processing", metadata = list(filterMethod = d$filterMethod, missingMethod = d$missingMethod))
    meta_info <- tryCatch(readRDS(get_session_path(ds_id, "%s_expr_metadata.rds")), error = function(e) NULL)
    dtype <- if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts"
    is_n <- if (!is.null(meta_info$isNormalized)) isTRUE(meta_info$isNormalized) else FALSE
    if (dtype == "readcounts" && !is_n) {
      push_counts_stack(ds_id, expr, step_name = "processing", metadata = list(filterMethod = d$filterMethod, missingMethod = d$missingMethod))
    }

    # Save processing results
    saveRDS(expr, file = sprintf("tmp/%s_processed.rds", ds_id))

    cat(sprintf("[CONSOLE]   Output features after: %d\n", nrow(expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(rownames(expr)))))
    cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(expr))))

    rebuilt <- rebuild_parsed_data(expr, anno, parsed$geneIdCol)
    is_raw_rc_proc <- identical(tolower(as.character(dtype)), "readcounts") && !isTRUE(is_n)
    qc_res <- tryCatch(generate_qc_plots(expr, ds_id, get_user_id(ds_id), ds_name = d$name, before_expr = expr_before_proc, is_unnormalized_rc = is_raw_rc_proc), error = function(e) NULL)

    res <- list(
      datasetId = ds_id_full,
      inputFeatures = length(parsed$gene_ids),
      removedFeatures = length(parsed$gene_ids) - nrow(expr),
      retainedFeatures = nrow(expr),
      missingValuesCount = sum(is.na(expr)),
      parsedData = rebuilt$parsedData,
      columns = rebuilt$columns,
      sampleIds = colnames(expr),
      nSamples = as.integer(ncol(expr)),
      qc = qc_res
    )
    save_step_cache_meta(ds_id, "processing", list(
      filterMethod = d$filterMethod,
      filterThreshold = d$filterParams$varianceThreshold,
      readcountThreshold = d$filterParams$readcountThreshold,
      abundanceMethod = d$filterParams$abundanceMethod,
      abundanceThreshold = d$filterParams$abundanceThreshold,
      abundanceSamplePercent = d$filterParams$abundanceSamplePercent,
      minCpm = d$filterParams$cpmThreshold,
      minSamples = d$filterParams$minSamples,
      naRemovePercent = d$naRemovePercent,
      missingMethod = d$missingMethod,
      kNeighbors = d$missingParams$knnK,
      upstream_fp = upstream_fp,
      result = res
    ))
    res
  }

  results <- if (length(payload) > 1 && get_effective_cores() > 1) {
    run_parallel_lapply(
      payload,
      process_single_processing,
      var_list = c("process_single_processing", "get_base_id", "get_backend_dataset", "rebuild_parsed_data",
                   "get_session_path", "get_latest_step_matrix", "push_main_stack", "push_counts_stack",
                   "register_export_file", "get_user_id", "%||%", "generate_qc_plots",
                   "get_step_cache_meta", "save_step_cache_meta", "get_matrix_fingerprint", "ensure_sequential_matrices",
                   "impute_missing_knn", "get_suitable_expression_matrix", "safe_str",
                   "get_dataset_display_name", "get_sample_clinical_vector", "compute_normal_pca",
                   "compute_single_qc", "build_hexbin_mv_ggplot", "build_dist_ggplot", "build_pca_ggplot",
                   "save_plot_formats", "prepare_pre_processing_qc", "get_clinical_path", "get_clin_metadata_path", "parse_clinical_data"),
      pkg_list = c("jsonlite", "impute", "matrixStats", "ggplot2", "gridExtra")
    )
  } else {
    lapply(payload, process_single_processing)
  }

  return(results)
}

# Helper to write boxplot to PNG
write_boxplot_file <- function(expr_mat, file_path, title) {
  tryCatch({
    png(file_path, width = 600, height = 400, res = 100)
    dev_num <- dev.cur()
    on.exit({
      if (dev_num %in% dev.list()) {
        dev.off(dev_num)
      }
    }, add = TRUE)
    if (is.null(colnames(expr_mat))) {
      colnames(expr_mat) <- paste0("S", seq_len(ncol(expr_mat)))
    }
    par(mar = c(6, 4, 3, 2) + 0.1)
    boxplot(expr_mat, main = title, las = 2, col = "#3b82f6", outline = FALSE, cex.axis = 0.7, ylab = "Value")
    if (dev_num %in% dev.list()) {
      dev.off(dev_num)
    }
  }, error = function(e) {
    cat(sprintf("[ERROR] write_boxplot_file failed: %s\n", e$message))
  })
}

# Helper to write boxplot to PDF
write_boxplot_pdf <- function(expr_mat, file_path, title) {
  tryCatch({
    pdf(file_path, width = 7, height = 5)
    if (is.null(colnames(expr_mat))) {
      colnames(expr_mat) <- paste0("S", seq_len(ncol(expr_mat)))
    }
    par(mar = c(6, 4, 3, 2) + 0.1)
    boxplot(expr_mat, main = title, las = 2, col = "#3b82f6", outline = FALSE, cex.axis = 0.7, ylab = "Value")
    dev.off()
  }, error = function(e) {
    cat(sprintf("[ERROR] write_boxplot_pdf failed: %s\n", e$message))
  })
}

# Generalized helper: ensure boxplot_before files exist for a dataset.
# Uses whatever earliest available matrix exists (upload / annotation / processed / normalized / batch).
# Called lazily from the export handler so it works regardless of which steps were run.
ensure_boxplot_before <- function(ds_id) {
  base_id <- get_base_id(ds_id)

  png_path <- get_session_path(base_id, "boxplot_before_%s.png")
  pdf_path <- get_session_path(base_id, "boxplot_before_%s.pdf")

  # Already exists — nothing to do
  if (file.exists(pdf_path) || file.exists(png_path)) return(invisible(TRUE))

  # Find the best "pre-normalization" matrix.
  # Priority: processed > annotated > upload raw  (we deliberately skip normalized/batch
  # because those are post-normalization and we want the "before" picture).
  expr <- NULL
  candidates <- list(
    get_session_path(base_id, "%s_processed_expr.rds"),
    get_session_path(base_id, "%s_expr_matrix_annotated.rds"),
    get_session_path(base_id, "%s_non_normalized_expr.rds"),
    get_session_path(base_id, "%s_expr_matrix.rds")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      expr <- tryCatch(readRDS(p), error = function(e) NULL)
      if (!is.null(expr) && is.matrix(expr) && nrow(expr) > 0) break
      expr <- NULL
    }
  }

  if (is.null(expr)) {
    cat(sprintf("[WARNING] ensure_boxplot_before: no matrix found for %s\n", base_id))
    return(invisible(FALSE))
  }

  # Sanitise rownames
  if (!is.null(rownames(expr))) {
    valid <- !is.na(rownames(expr)) & rownames(expr) != "" & rownames(expr) != "NA"
    expr <- expr[valid, , drop = FALSE]
  }

  tryCatch({
    ds_display_name <- get_dataset_display_name(base_id)
    write_boxplot_file(expr, png_path, paste0(ds_display_name, " (Sample Distribution)"))
    write_boxplot_pdf(expr,  pdf_path, paste0(ds_display_name, " (Sample Distribution)"))
    register_export_file(get_user_id(base_id), "boxplot_before", base_id, pdf_path, "dp", "normalization", ext = "pdf")
    cat(sprintf("[INFO] ensure_boxplot_before: generated boxplot files for %s\n", base_id))
    invisible(TRUE)
  }, error = function(e) {
    cat(sprintf("[WARNING] ensure_boxplot_before: failed for %s: %s\n", base_id, e$message))
    invisible(FALSE)
  })
}

# 3. Normalization Handler
normalize_datasets <- function(datasets, method, transform_type, prior_count) {
  cat(sprintf("[PROCESSING] Normalizing counts using method: %s...\n", method))

  process_single_norm <- function(d) {
    ds_id_full <- if (!is.null(d$datasetId)) d$datasetId else d$id
    ds_id <- get_base_id(ds_id_full)
    
    d_method <- if (!is.null(d$method)) d$method else method
    
    # Extract transformationType or logTransform fallback
    d_transform_type <- if (!is.null(d$transformationType)) d$transformationType else {
      log_val <- if (!is.null(d$logTransform)) isTRUE(d$logTransform) else if (is.logical(transform_type)) transform_type else (transform_type != "none")
      if (log_val) "log2" else "none"
    }
    if (is.logical(transform_type)) {
      transform_type <- if (isTRUE(transform_type)) "log2" else "none"
    }
    if (!is.null(transform_type) && !is.logical(transform_type) && is.null(d$transformationType)) {
      d_transform_type <- transform_type
    }
    
    d_prior_count <- if (!is.null(d$priorCount)) as.numeric(d$priorCount) else prior_count
    d_data_type <- if (!is.null(d$dataType)) d$dataType else "readcounts"
    ds_display_name <- get_dataset_display_name(ds_id, d$name)

    # Check if skipped
    if (d_method == "skip" || d_method == "none") {
      cat(sprintf("  Skipping normalization for dataset %s\n", ds_id_full))
      if (file.exists(sprintf("tmp/%s_normalized.rds", ds_id))) file.remove(sprintf("tmp/%s_normalized.rds", ds_id))
      if (file.exists(sprintf("tmp/%s_normalization.csv", ds_id))) file.remove(sprintf("tmp/%s_normalization.csv", ds_id))
      if (file.exists(sprintf("tmp/%s_norm_config.rds", ds_id))) file.remove(sprintf("tmp/%s_norm_config.rds", ds_id))
      if (file.exists(get_session_path(ds_id, "%s_norm_config.rds"))) file.remove(get_session_path(ds_id, "%s_norm_config.rds"))
      
      expr <- get_latest_step_matrix(ds_id, "normalization")
      if (!is.null(expr)) {
        saveRDS(expr, file = sprintf("tmp/%s_normalized_expr.rds", ds_id))
        saveRDS(expr, file = sprintf("tmp/%s_expr_matrix.rds", ds_id))
      }
      
      parsed <- get_backend_dataset(ds_id_full, step = "normalization")
      if (is.null(parsed)) return(list(datasetId = ds_id_full))

      cat(sprintf("[CONSOLE] Dataset: %s (normalization skipped)\n", ds_id_full))
      cat(sprintf("[CONSOLE]   Output features after: %d\n", length(parsed$gene_ids)))
      cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(parsed$gene_ids))))
      cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(parsed$expr))))

      rebuilt <- rebuild_parsed_data(parsed$expr, parsed$anno, parsed$geneIdCol)
      
      png_before_path <- get_session_path(ds_id, "boxplot_before_%s.png")
      png_after_path  <- get_session_path(ds_id, "boxplot_after_%s.png")
      pdf_before_path <- get_session_path(ds_id, "boxplot_before_%s.pdf")
      pdf_after_path  <- get_session_path(ds_id, "boxplot_after_%s.pdf")
      
      write_boxplot_file(parsed$expr, png_before_path, paste0(ds_display_name, " (Before/After - Skipped)"))
      file.copy(png_before_path, png_after_path, overwrite = TRUE)
      write_boxplot_pdf(parsed$expr, pdf_before_path, paste0(ds_display_name, " (Before/After - Skipped)"))
      file.copy(pdf_before_path, pdf_after_path, overwrite = TRUE)
      register_export_file(get_user_id(ds_id), "boxplot_before", ds_id, pdf_before_path, "dp", "normalization", ext = "pdf")
      register_export_file(get_user_id(ds_id), "boxplot_after", ds_id, pdf_after_path, "dp", "normalization", ext = "pdf")
      
      boxplot_before <- if (file.exists(png_before_path)) base64enc::base64encode(png_before_path) else ""
      boxplot_after <- boxplot_before

      is_raw_rc <- identical(tolower(as.character(d_data_type)), "readcounts") && !isTRUE(parsed$isNormalized)
      norm_pca <- tryCatch(compute_normal_pca(parsed$expr, ds_id, is_unnormalized_rc = is_raw_rc, ds_name = ds_display_name, save_latest = TRUE), error = function(e) NULL)
      if (!is.null(norm_pca)) {
        tryCatch({
          p_norm_pca <- build_pca_ggplot(norm_pca, color_by = "group", title_str = sprintf("%s — Normal PCA", ds_display_name))
          pca_pdf <- get_session_path(ds_id, "pca_plots_%s.pdf")
          pca_png <- get_session_path(ds_id, "pca_plots_%s.png")
          pca_svg <- get_session_path(ds_id, "pca_plots_%s.svg")
          save_plot_formats(p_norm_pca, pca_pdf, pca_png, pca_svg, width = 6.5, height = 4.8)
          register_export_file(get_user_id(ds_id), "pca_plots", ds_id, pca_pdf, "dp", "normalization", ext = "pdf")
        }, error = function(e) NULL)
      }

      return(list(
        datasetId = ds_id_full,
        parsedData = rebuilt$parsedData,
        columns = rebuilt$columns,
        sampleIds = colnames(parsed$expr),
        boxplotBefore = boxplot_before,
        boxplotAfter = boxplot_after,
        normalPca = norm_pca,
        nFeatures = as.integer(nrow(parsed$expr)),
        nSamples = as.integer(ncol(parsed$expr)),
        retainedFeatures = as.integer(nrow(parsed$expr)),
        inputFeatures = as.integer(nrow(parsed$expr))
      ))
    }
    
    # Load processed matrix from most recent step with results
    expr <- get_latest_step_matrix(ds_id, "normalization")
    
    parsed <- get_backend_dataset(ds_id_full, step = "normalization")
    if (is.null(parsed)) return(list(datasetId = ds_id_full))
    
    if (is.null(expr)) {
      expr <- parsed$expr
    }

    # ── Tier 1 Cache Check ───────────────────────────────────────────────────
    step_cache <- get_step_cache_meta(ds_id, "normalization")
    upstream_fp <- get_matrix_fingerprint(expr)
    out_file <- get_session_path(ds_id, "%s_normalized_expr.rds")
    
    if (!is.null(step_cache) &&
        identical(step_cache$method, d_method) &&
        identical(step_cache$transformationType, d_transform_type) &&
        identical(step_cache$priorCount, d_prior_count) &&
        identical(step_cache$dataType, d_data_type) &&
        identical(step_cache$upstream_fp, upstream_fp) &&
        file.exists(out_file)) {
      cat(sprintf("[CACHE] Reusing cached normalization for dataset %s\n", ds_id))
      norm_mat <- tryCatch(readRDS(out_file), error = function(e) NULL)
      if (!is.null(norm_mat)) {
        push_main_stack(ds_id, norm_mat, step_name = "normalization", metadata = list(method = d_method, logTransform = d_transform_type != "none", transformationType = d_transform_type, priorCount = d_prior_count, dataType = d_data_type))
        ensure_sequential_matrices(ds_id)
        return(step_cache$result)
      }
    }

    # Validate that counts/expression matrix contains at least one feature/value
    if (is.null(expr) || nrow(expr) == 0 || ncol(expr) == 0) {
      stop("Input count matrix 'expr' is empty. Please check your filtering/processing thresholds to ensure features are retained.")
    }

    cat(sprintf("[CONSOLE] Dataset: %s (normalization)\n", ds_id))
    cat(sprintf("[CONSOLE]   Input features before: %d\n", nrow(expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs before: %d\n", sum(duplicated(rownames(expr)))))
    cat(sprintf("[CONSOLE]   Missing values before: %d\n", sum(is.na(expr))))
    
    anno <- parsed$anno
    anno_idx <- match(rownames(expr), rownames(anno))
    anno <- anno[!is.na(anno_idx), , drop = FALSE]

    norm_expr <- apply_normalization_to_matrix(expr, d_method, d_transform_type, d_prior_count, d_data_type)
    norm_cfg_obj <- list(method = d_method, logTransform = d_transform_type != "none", transformationType = d_transform_type, priorCount = d_prior_count, dataType = d_data_type)
    saveRDS(norm_cfg_obj, file = sprintf("tmp/%s_norm_config.rds", ds_id))
    tryCatch(saveRDS(norm_cfg_obj, file = get_session_path(ds_id, "%s_norm_config.rds")), error = function(e) NULL)

    # Illumina probe quality + detection P-val filter (post-normalization)
    data_type <- d_data_type %||% parsed$dataType %||% ""
    gene_id_type <- d$geneIdType %||% parsed$geneIdType %||% d$microarrayPlatformId %||% parsed$platform
    norm_expr <- apply_illumina_probe_filters(norm_expr, ds_id, gene_id_type, data_type)

    # Save sequentially and push to main stack
    push_main_stack(ds_id, norm_expr, step_name = "normalization", metadata = list(method = d_method, logTransform = d_transform_type != "none", transformationType = d_transform_type, priorCount = d_prior_count, dataType = d_data_type))

    # Save normalization results (Fix 1)
    saveRDS(norm_expr, file = sprintf("tmp/%s_normalized.rds", ds_id))

    cat(sprintf("[CONSOLE]   Output features after: %d\n", nrow(norm_expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(rownames(norm_expr)))))
    cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(norm_expr))))

    rebuilt <- rebuild_parsed_data(norm_expr, anno, parsed$geneIdCol)

    png_before_path <- get_session_path(ds_id, "boxplot_before_%s.png")
    png_after_path  <- get_session_path(ds_id, "boxplot_after_%s.png")
    pdf_before_path <- get_session_path(ds_id, "boxplot_before_%s.pdf")
    pdf_after_path  <- get_session_path(ds_id, "boxplot_after_%s.pdf")
    
    write_boxplot_file(expr, png_before_path, paste0(ds_display_name, " — Before Normalization"))
    write_boxplot_file(norm_expr, png_after_path, paste0(ds_display_name, " — After Normalization"))
    write_boxplot_pdf(expr, pdf_before_path, paste0(ds_display_name, " — Before Normalization"))
    write_boxplot_pdf(norm_expr, pdf_after_path, paste0(ds_display_name, " — After Normalization"))
    register_export_file(get_user_id(ds_id), "boxplot_before", ds_id, pdf_before_path, "dp", "normalization", ext = "pdf")
    register_export_file(get_user_id(ds_id), "boxplot_after", ds_id, pdf_after_path, "dp", "normalization", ext = "pdf")
    
    boxplot_before <- if (file.exists(png_before_path) && requireNamespace("base64enc", quietly = TRUE)) base64enc::base64encode(png_before_path) else ""
    boxplot_after  <- if (file.exists(png_after_path) && requireNamespace("base64enc", quietly = TRUE)) base64enc::base64encode(png_after_path) else ""

    # Compute Entity 1 Normal PCA on normalized expression matrix (and update %s_latest_normal_pca.rds)
    norm_pca <- tryCatch(compute_normal_pca(norm_expr, ds_id, is_unnormalized_rc = FALSE, ds_name = ds_display_name, save_latest = TRUE), error = function(e) NULL)
    if (!is.null(norm_pca)) {
      tryCatch({
        p_norm_pca <- build_pca_ggplot(norm_pca, color_by = "group", title_str = sprintf("%s — Normal PCA (Normalized)", ds_display_name))
        pca_pdf <- get_session_path(ds_id, "pca_plots_%s.pdf")
        pca_png <- get_session_path(ds_id, "pca_plots_%s.png")
        pca_svg <- get_session_path(ds_id, "pca_plots_%s.svg")
        save_plot_formats(p_norm_pca, pca_pdf, pca_png, pca_svg, width = 6.5, height = 4.8)
        register_export_file(get_user_id(ds_id), "pca_plots", ds_id, pca_pdf, "dp", "normalization", ext = "pdf")
      }, error = function(e) NULL)
    }

    # For microarray datasets (where Normalization precedes Annotation and Processing), also update pre-processing QC snapshot
    if (identical(tolower(as.character(d_data_type)), "microarray")) {
      tryCatch(prepare_pre_processing_qc(norm_expr, ds_id, d_data_type, is_normalized = TRUE, ds_name = ds_display_name), error = function(e) NULL)
    }

    res <- list(
      datasetId = ds_id_full,
      parsedData = rebuilt$parsedData,
      columns = rebuilt$columns,
      sampleIds = colnames(norm_expr),
      boxplotBefore = boxplot_before,
      boxplotAfter = boxplot_after,
      normalPca = norm_pca,
      nFeatures = as.integer(nrow(norm_expr)),
      nSamples = as.integer(ncol(norm_expr)),
      retainedFeatures = as.integer(nrow(norm_expr)),
      inputFeatures = as.integer(nrow(expr))
    )
    save_step_cache_meta(ds_id, "normalization", list(
      method = d_method,
      transformationType = d_transform_type,
      priorCount = d_prior_count,
      dataType = d_data_type,
      upstream_fp = upstream_fp,
      result = res
    ))
    res
  }

  results <- if (length(datasets) > 1 && get_effective_cores() > 1) {
    run_parallel_lapply(
      datasets,
      process_single_norm,
      var_list = c("method", "transform_type", "prior_count", "process_single_norm",
                   "get_base_id", "get_backend_dataset", "rebuild_parsed_data", "get_session_path",
                   "get_latest_step_matrix", "write_boxplot_file", "write_boxplot_pdf", "register_export_file",
                   "get_user_id", "apply_normalization_to_matrix", "push_main_stack", "push_counts_stack", "%||%",
                   "ILLUMINA_PROBE_QUALITY_PLATFORMS", "filter_probes_by_quality", "filter_by_detection_pval", "apply_illumina_probe_filters",
                   "get_step_cache_meta", "save_step_cache_meta", "get_matrix_fingerprint", "ensure_sequential_matrices",
                   "get_suitable_expression_matrix", "safe_str",
                   "get_dataset_display_name", "get_sample_clinical_vector", "compute_normal_pca",
                   "compute_single_qc", "build_hexbin_mv_ggplot", "build_dist_ggplot", "build_pca_ggplot",
                   "save_plot_formats", "prepare_pre_processing_qc", "get_clinical_path", "get_clin_metadata_path", "parse_clinical_data"),
      pkg_list = c("jsonlite", "edgeR", "limma", "DESeq2", "vsn", "preprocessCore", "ggplot2", "matrixStats")
    )
  } else {
    lapply(datasets, process_single_norm)
  }

  return(results)
}

plot_to_base64 <- function(plot_obj, width = 600, height = 450) {
  tmp <- tempfile(fileext = ".png")
  tryCatch({
    if (inherits(plot_obj, "ggplot")) {
      ggplot2::ggsave(
        filename  = tmp,
        plot      = plot_obj,
        width     = width / 100,
        height    = height / 100,
        dpi       = 100,
        limitsize = FALSE,
        bg        = "white"
      )
    } else {
      png(tmp, width = width, height = height, res = 100)
      dev_num <- dev.cur()
      on.exit({
        if (dev_num %in% dev.list()) {
          dev.off(dev_num)
        }
      }, add = TRUE)
      print(plot_obj)
      if (dev_num %in% dev.list()) {
        dev.off(dev_num)
      }
    }
    txt <- base64enc::base64encode(tmp)
    if (file.exists(tmp)) file.remove(tmp)
    return(txt)
  }, error = function(e) {
    cat(sprintf("[ERROR] plot_to_base64 failed: %s\n", e$message))
    if (file.exists(tmp)) file.remove(tmp)
    return("")
  })
}

# Robust VST normalization helper
robust_vst <- function(counts_mat) {
  tryCatch({
    col_data <- data.frame(row.names = colnames(counts_mat), dummy = rep("A", ncol(counts_mat)))
    rounded_counts <- round(counts_mat)
    rounded_counts[rounded_counts < 0] <- 0
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = rounded_counts, colData = col_data, design = ~1)
    vsd <- DESeq2::vst(dds, blind = TRUE)
    return(SummarizedExperiment::assay(vsd))
  }, error = function(e) {
    cat("[PCA] VST failed, falling back to log2(counts + 1):", conditionMessage(e), "\n")
    return(log2(counts_mat + 1))
  })
}

attach_batch_to_pca_obj <- function(pca_obj, batch_vec, samples = NULL, ds_name = NULL) {
  if (is.null(pca_obj) || is.null(pca_obj$points) || length(pca_obj$points) == 0) return(NULL)
  pts <- pca_obj$points
  pt_samples <- vapply(pts, function(p) as.character(p$sample %||% ""), character(1))
  if (!is.null(samples) && length(samples) > 0) {
    idx <- match(samples, pt_samples)
    if (any(is.na(idx))) return(NULL)
    pts <- pts[idx]
  }
  for (i in seq_along(pts)) {
    b_val <- if (i <= length(batch_vec)) as.character(batch_vec[i]) else "Batch1"
    pts[[i]]$batch <- b_val
    if (!is.null(ds_name) && nzchar(ds_name)) pts[[i]]$dataset <- ds_name
  }
  pca_obj$points <- pts
  if (!is.null(ds_name) && nzchar(ds_name)) pca_obj$title <- ds_name
  return(pca_obj)
}

# 4. Dimensionality PCA Handler (Entity 2 — Batch-Stratified Pre-Batch PCA)
compute_pca <- function(datasets) {
  cat("[PROCESSING] Computing PCA coordinate projection...\n")

  expr_list <- list()
  clin_combined <- data.frame()
  datasets_pca <- list()

  for (d in datasets) {
    ds_id_full <- if (!is.null(d$datasetId)) d$datasetId else (if (!is.null(d$id)) d$id else "")
    ds_id <- get_base_id(ds_id_full)
    ds_display_name <- get_dataset_display_name(ds_id, d$name)

    parsed_expr <- get_backend_dataset(ds_id_full, step = "pca")
    if (is.null(parsed_expr)) next
    expr <- parsed_expr$expr
    cat("Number of rows:", nrow(expr), "\n")
    
    if (any(is.na(expr))) {
      rm_vals <- rowMeans(expr, na.rm = TRUE)
      rm_vals[!is.finite(rm_vals)] <- 0
      na_idx <- which(is.na(expr), arr.ind = TRUE)
      if (nrow(na_idx) > 0) expr[na_idx] <- rm_vals[na_idx[, 1]]
    }

    # Load clinical data
    clin_path <- get_clinical_path(ds_id)
    if (!file.exists(clin_path)) {
      clin_path_full <- get_clinical_path(ds_id_full)
      if (file.exists(clin_path_full)) clin_path <- clin_path_full
    }
    clin_meta_path <- get_clin_metadata_path(ds_id)
    if (!file.exists(clin_meta_path)) {
      clin_meta_path_full <- get_clin_metadata_path(ds_id_full)
      if (file.exists(clin_meta_path_full)) clin_meta_path <- clin_meta_path_full
    }
    
    clin_df <- NULL
    meta <- NULL
    if (file.exists(clin_meta_path)) {
      meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
    }

    sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
      d$clinicalSampleIdCol
    } else if (!is.null(d$sampleIdCol) && nzchar(d$sampleIdCol)) {
      d$sampleIdCol
    } else if (!is.null(meta$sampleIdCol)) {
      meta$sampleIdCol
    } else {
      ""
    }

    if (file.exists(clin_path)) {
      raw_clin <- read.csv(clin_path, check.names = FALSE, stringsAsFactors = FALSE)
      if (sample_id_col == "" && ncol(raw_clin) > 0) sample_id_col <- colnames(raw_clin)[1]
      clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
    } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0) {
      clin_df <- parse_clinical_data(d$clinicalParsedData, d$clinicalColumns, sample_id_col)
    }

    samples <- colnames(expr)
    batch_vec <- rep("Batch1", length(samples))
    
    batch_col <- if (!is.null(d$clinicalBatchCol) && nzchar(d$clinicalBatchCol)) {
      d$clinicalBatchCol
    } else if (!is.null(d$batchCol) && nzchar(d$batchCol)) {
      d$batchCol
    } else if (!is.null(meta$batchCol)) {
      meta$batchCol
    } else {
      ""
    }

    if (!is.null(clin_df)) {
      if (batch_col == "" || !(batch_col %in% colnames(clin_df))) {
        cand_batch <- c("Batch", "batch", "Center", "Site", "Plate", "Run", "Cohort", "Study")
        found_batch <- intersect(cand_batch, colnames(clin_df))
        if (length(found_batch) > 0) {
          batch_col <- found_batch[1]
        }
      }
      if (nzchar(batch_col) && batch_col %in% colnames(clin_df)) {
        for (i in seq_along(samples)) {
          sample_name <- samples[i]
          if (sample_name %in% rownames(clin_df)) {
            row <- clin_df[sample_name, ]
            batch_vec[i] <- as.character(row[[batch_col]])
          }
        }
      }
    }

    # Determine whether Normalization was performed
    norm_cfg_file <- get_session_path(ds_id, "%s_norm_config.rds")
    if (!file.exists(norm_cfg_file)) norm_cfg_file <- sprintf("tmp/%s_norm_config.rds", ds_id)
    norm_cfg <- if (file.exists(norm_cfg_file)) tryCatch(readRDS(norm_cfg_file), error = function(e) NULL) else NULL
    is_normalized_step_done <- (!is.null(norm_cfg) && !(norm_cfg$method %in% c("skip", "none"))) ||
                               file.exists(get_session_path(ds_id, "%s_normalized_expr_actual.rds")) ||
                               file.exists(sprintf("tmp/%s_normalized_expr_actual.rds", ds_id)) ||
                               file.exists(sprintf("tmp/%s_normalized.rds", ds_id))
    dtype_val <- tolower(as.character(d$dataType %||% parsed_expr$dataType %||% "readcounts"))
    is_unnorm_rc <- !is_normalized_step_done && identical(dtype_val, "readcounts") && !isTRUE(parsed_expr$isNormalized)

    expr_pca <- expr
    if (is_unnorm_rc) {
      expr_pca <- log2(pmax(expr, 0) + 1)
    }

    expr_list[[ds_id]] <- expr_pca
    df_d <- data.frame(
      Sample    = samples,
      Dataset   = rep(ds_display_name, length(samples)),
      Batch     = as.factor(batch_vec),
      stringsAsFactors = FALSE
    )
    rownames(df_d) <- paste0(ds_id, "_", samples)
    clin_combined <- rbind(clin_combined, df_d)

    # Reuse already-computed Normal PCA coordinates if available, and stratify by Batch
    latest_pca_path <- get_session_path(ds_id, "%s_latest_normal_pca.rds")
    latest_pca <- if (file.exists(latest_pca_path)) tryCatch(readRDS(latest_pca_path), error = function(e) NULL) else NULL
    batch_pca_ind <- attach_batch_to_pca_obj(latest_pca, batch_vec, samples, ds_display_name)
    if (is.null(batch_pca_ind)) {
      computed_pca <- compute_normal_pca(expr, ds_id, is_unnormalized_rc = is_unnorm_rc, ds_name = ds_display_name, save_latest = TRUE)
      batch_pca_ind <- attach_batch_to_pca_obj(computed_pca, batch_vec, samples, ds_display_name)
    }

    pca_ind_str <- ""
    if (!is.null(batch_pca_ind)) {
      pca_ind_str <- tryCatch({
        p_ind <- build_pca_ggplot(batch_pca_ind, color_by = "batch", title_str = sprintf("%s — PCA Before Batch Correction", ds_display_name))
        pdf_path <- get_session_path(ds_id, "pca_plots_before_batch_%s.pdf")
        png_path <- get_session_path(ds_id, "pca_plots_before_batch_%s.png")
        svg_path <- get_session_path(ds_id, "pca_plots_before_batch_%s.svg")
        save_plot_formats(p_ind, pdf_path, png_path, svg_path, width = 6.5, height = 4.8)
        register_export_file(get_user_id(ds_id), "pca_plots_before_batch", ds_id, pdf_path, "dp", "batch", ext = "pdf")
        base64enc::base64encode(png_path)
      }, error = function(e) "")
    }
    datasets_pca[[ds_id_full]] <- list(
      plot_batch     = pca_ind_str,
      pcaInteractive = batch_pca_ind
    )
  }

  if (length(expr_list) == 0) return(list())

  # Find common genes across all loaded expression matrices
  common_genes <- NULL
  for (id in names(expr_list)) {
    genes <- rownames(expr_list[[id]])
    if (is.null(common_genes)) {
      common_genes <- genes
    } else {
      common_genes <- intersect(common_genes, genes)
    }
  }

  if (is.null(common_genes) || length(common_genes) < 2) {
    return(list(plot_batch = "", plot_group = "", plot_covariate = "", combined = list(plot_batch = ""), datasets = datasets_pca))
  }

  for (id in names(expr_list)) {
    m <- expr_list[[id]][common_genes, , drop = FALSE]
    colnames(m) <- paste0(id, "_", colnames(m))
    expr_list[[id]] <- m
  }
  combined_expr <- do.call(cbind, expr_list)
  rm(expr_list); invisible(gc(FALSE))

  gene_vars <- apply(combined_expr, 1, var)
  combined_expr <- combined_expr[is.finite(gene_vars) & gene_vars > 0, , drop = FALSE]
  if (nrow(combined_expr) < 2) {
    return(list(plot_batch = "", plot_group = "", plot_covariate = "", combined = list(plot_batch = ""), datasets = datasets_pca))
  }
  pca_fit <- tryCatch(
    prcomp(t(combined_expr), scale. = TRUE),
    error = function(e) { cat("[PCA] combined prcomp failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(pca_fit)) {
    return(list(plot_batch = "", plot_group = "", plot_covariate = "", combined = list(plot_batch = ""), datasets = datasets_pca))
  }
  pca_coords <- pca_fit$x[, 1:2, drop = FALSE]

  var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
  pc1_label <- sprintf("PC1 (%.1f%%)", var_explained[1] * 100)
  pc2_label <- sprintf("PC2 (%.1f%%)", var_explained[2] * 100)

  samples_in_pca <- rownames(pca_coords)
  clin_aligned <- clin_combined[samples_in_pca, , drop = FALSE]

  comb_pts <- lapply(seq_len(nrow(pca_coords)), function(i) {
    list(
      sample  = as.character(clin_aligned$Sample[i]),
      pc1     = round(unname(as.numeric(pca_coords[i, 1])), 4),
      pc2     = round(unname(as.numeric(pca_coords[i, 2])), 4),
      batch   = as.character(clin_aligned$Batch[i]),
      group   = as.character(clin_aligned$Batch[i]),
      dataset = as.character(clin_aligned$Dataset[i])
    )
  })
  comb_pca_obj <- list(
    pc1Label = pc1_label,
    pc2Label = pc2_label,
    title    = "Combined Datasets — Before Batch Correction",
    points   = comb_pts
  )

  p1 <- build_pca_ggplot(comb_pca_obj, color_by = "batch", title_str = NULL)
  p1_export <- build_pca_ggplot(comb_pca_obj, color_by = "batch", title_str = "Combined Datasets — Pre-Correction PCA (Batch Effect)")
  cat("[PCA] Completed PCA computation...\n")
  b64_p1 <- plot_to_base64(p1)
  return(list(
    plot_batch            = b64_p1,
    plot_group            = "",
    plot_covariate        = "",
    plot_batch_export     = plot_to_base64(p1_export),
    plot_group_export     = "",
    plot_covariate_export = "",
    combined = list(
      plot_batch     = b64_p1,
      pcaInteractive = comb_pca_obj
    ),
    datasets = datasets_pca
  ))
}

correct_batch_effects <- function(method, method_others, datasets) {
  cat(sprintf("[PROCESSING] Applying batch correction (method: %s, method_others: %s)...\n", method, method_others))

  expr_before_list <- list()
  expr_after_list  <- list()
  clin_combined    <- data.frame()
  datasets_pca     <- list()

  for (d in datasets) {
    ds_id_full <- d$id
    ds_id <- get_base_id(ds_id_full)
    ds_display_name <- get_dataset_display_name(ds_id, d$name)
    
    # Load expression (for raw readcounts, use the most recent raw count matrix stack)
    dtype <- d$dataType
    if (is.null(dtype)) dtype <- "readcounts"
    is_norm <- isTRUE(d$isNormalized)
    is_raw_readcounts <- (dtype == "readcounts" && !is_norm)
    
    # Check if user had already run Normalization step before Batch Correction
    norm_cfg_file <- get_session_path(ds_id, "%s_norm_config.rds")
    if (!file.exists(norm_cfg_file)) norm_cfg_file <- sprintf("tmp/%s_norm_config.rds", ds_id)
    norm_cfg <- if (file.exists(norm_cfg_file)) tryCatch(readRDS(norm_cfg_file), error = function(e) NULL) else NULL
    has_prev_norm <- !is.null(norm_cfg) && !(norm_cfg$method %in% c("skip", "none"))
    pre_batch_norm_mat <- NULL
    if (has_prev_norm) {
      for (np in c(get_session_path(ds_id, "%s_normalized_expr_actual.rds"), sprintf("tmp/%s_normalized_expr_actual.rds", ds_id), sprintf("tmp/%s_normalized.rds", ds_id))) {
        if (file.exists(np)) {
          pre_batch_norm_mat <- tryCatch(readRDS(np), error = function(e) NULL)
          if (!is.null(pre_batch_norm_mat)) break
        }
      }
    }

    rds_path <- sprintf("tmp/%s_expr_matrix.rds", ds_id)
    expr <- NULL
    if (is_raw_readcounts) {
      # Use raw counts stack for ComBat-seq
      expr <- get_latest_raw_count_matrix(ds_id)
    } else {
      # Use normalized/processed matrix stack
      expr <- get_latest_step_matrix(ds_id, "batch")
    }
    
    parsed <- get_backend_dataset(ds_id_full, step = "batch")
    if (is.null(parsed)) next
    if (is.null(expr)) expr <- parsed$expr

    cat(sprintf("[CONSOLE] Dataset: %s (batch correction)\n", ds_id))
    cat(sprintf("[CONSOLE]   Input features before: %d\n", nrow(expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs before: %d\n", sum(duplicated(rownames(expr)))))
    cat(sprintf("[CONSOLE]   Missing values before: %d\n", sum(is.na(expr))))
    
    # Load clinical df
    clin_path <- get_clinical_path(ds_id)
    if (!file.exists(clin_path)) {
      clin_path_full <- get_clinical_path(ds_id_full)
      if (file.exists(clin_path_full)) clin_path <- clin_path_full
    }
    clin_meta_path <- get_clin_metadata_path(ds_id)
    if (!file.exists(clin_meta_path)) {
      clin_meta_path_full <- get_clin_metadata_path(ds_id_full)
      if (file.exists(clin_meta_path_full)) clin_meta_path <- clin_meta_path_full
    }
    clin_df <- NULL
    meta <- NULL
    if (file.exists(clin_meta_path)) {
      meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
    }

    sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
      d$clinicalSampleIdCol
    } else if (!is.null(d$sampleIdCol) && nzchar(d$sampleIdCol)) {
      d$sampleIdCol
    } else if (!is.null(meta$sampleIdCol)) {
      meta$sampleIdCol
    } else {
      ""
    }

    if (file.exists(clin_path)) {
      raw_clin <- read.csv(clin_path, check.names = FALSE, stringsAsFactors = FALSE)
      if (sample_id_col == "" && ncol(raw_clin) > 0) sample_id_col <- colnames(raw_clin)[1]
      clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
    } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0) {
      clin_df <- parse_clinical_data(d$clinicalParsedData, d$clinicalColumns, sample_id_col)
    }
    
    if (is.null(clin_df)) next
    
    common_samples <- intersect(colnames(expr), rownames(clin_df))
    common_samples <- common_samples[!is.na(common_samples) & common_samples != "" & common_samples != "NA" & common_samples != "NaN"]
    expr <- expr[, common_samples, drop = FALSE]
    clin_df <- clin_df[common_samples, , drop = FALSE]
    
    batch_col <- if (!is.null(d$clinicalBatchCol) && nzchar(d$clinicalBatchCol)) {
      d$clinicalBatchCol
    } else if (!is.null(d$batchCol) && nzchar(d$batchCol)) {
      d$batchCol
    } else if (!is.null(meta$batchCol)) {
      meta$batchCol
    } else {
      ""
    }

    if (batch_col == "" || !(batch_col %in% colnames(clin_df))) {
      cand_batch <- c("Batch", "batch", "Center", "Site", "Plate", "Run", "Cohort", "Study")
      found_batch <- intersect(cand_batch, colnames(clin_df))
      if (length(found_batch) > 0) {
        batch_col <- found_batch[1]
      }
    }
    
    corrected_expr <- expr
    curr_method <- if (is_raw_readcounts) method else method_others
    norm_mat <- NULL

    batch_vec <- if (!is.null(batch_col) && batch_col != "" && (batch_col %in% colnames(clin_df))) {
      as.factor(clin_df[[batch_col]])
    } else {
      factor(rep("batch1", ncol(expr)))
    }

    # ── Tier 1 Cache Check ───────────────────────────────────────────────────
    step_cache <- get_step_cache_meta(ds_id, "batch")
    upstream_fp <- get_matrix_fingerprint(expr)
    clinical_fp <- get_matrix_fingerprint(clin_df)
    out_file <- get_session_path(ds_id, "%s_batch_expr_actual.rds")
    
    is_cache_hit <- (!is.null(step_cache) &&
        identical(step_cache$method, curr_method) &&
        identical(step_cache$clinicalBatchCol, d$clinicalBatchCol) &&
        identical(step_cache$clinicalGroupCol, d$clinicalGroupCol) &&
        identical(step_cache$upstream_fp, upstream_fp) &&
        identical(step_cache$clinical_fp, clinical_fp) &&
        file.exists(out_file))

    # Skip handling
    if (is_cache_hit) {
      cat(sprintf("[CACHE] Reusing cached batch correction for dataset %s\n", ds_id))
      corrected_expr <- tryCatch(readRDS(out_file), error=function(e) expr)
      if (is_raw_readcounts && curr_method == "combat_seq" && has_prev_norm) {
        norm_transform <- if (!is.null(norm_cfg$transformationType)) norm_cfg$transformationType else (if (isTRUE(norm_cfg$logTransform)) "log2" else "none")
        norm_data_type <- if (!is.null(norm_cfg$dataType)) norm_cfg$dataType else dtype
        norm_mat <- apply_normalization_to_matrix(corrected_expr, norm_cfg$method, norm_transform, norm_cfg$priorCount, norm_data_type)
        push_main_stack(ds_id, norm_mat, step_name = "batch", metadata = list(method = "combat_seq", reNormalized = TRUE, normMethod = norm_cfg$method))
      } else {
        push_main_stack(ds_id, corrected_expr, step_name = "batch", metadata = list(method = curr_method))
      }
      ensure_sequential_matrices(ds_id)
    } else if (curr_method == "skip" || curr_method == "none") {
      cat(sprintf("  Skipping batch correction for dataset %s\n", ds_id))
      if (file.exists(sprintf("tmp/%s_batch.rds", ds_id))) file.remove(sprintf("tmp/%s_batch.rds", ds_id))
      if (file.exists(sprintf("tmp/%s_batch.csv", ds_id))) file.remove(sprintf("tmp/%s_batch.csv", ds_id))
      if (file.exists(sprintf("tmp/%s_batch_expr_actual.rds", ds_id))) file.remove(sprintf("tmp/%s_batch_expr_actual.rds", ds_id))
      
      expr_prev <- get_latest_step_matrix(ds_id, "batch")
      if (!is.null(expr_prev)) {
        saveRDS(expr_prev, file = sprintf("tmp/%s_batch_expr.rds", ds_id))
        saveRDS(expr_prev, file = rds_path)
      }
    } else if (!is.null(batch_col) && batch_col != "" && (batch_col %in% colnames(clin_df))) {
      batch_vec <- as.factor(clin_df[[batch_col]])
      
      corrected_expr <- tryCatch({
        if (curr_method == "combat") {
          sva::ComBat(dat = expr, batch = batch_vec)
        } else if (curr_method == "combat_seq") {
          sva::ComBat_seq(counts = round(expr), batch = batch_vec)
        } else if (curr_method == "limma_removebatch") {
          limma::removeBatchEffect(x = expr, batch = batch_vec)
        } else {
          expr
        }
      }, error = function(e) {
        cat(sprintf("[WARNING] Batch correction failed for dataset %s: %s\n", ds_id, e$message))
        expr
      })
      
      saveRDS(corrected_expr, file = rds_path)
      saveRDS(corrected_expr, file = sprintf("tmp/%s_batch_expr.rds", ds_id))
      saveRDS(corrected_expr, file = sprintf("tmp/%s_batch_expr_actual.rds", ds_id))
      saveRDS(corrected_expr, file = sprintf("tmp/%s_batch.rds", ds_id))
      
      if (is_raw_readcounts && curr_method == "combat_seq") {
        push_counts_stack(ds_id, corrected_expr, step_name = "batch", metadata = list(method = "combat_seq"))
        saveRDS(corrected_expr, file = sprintf("tmp/%s_batch_raw_counts.rds", ds_id))
        
        if (has_prev_norm) {
          norm_transform <- if (!is.null(norm_cfg$transformationType)) norm_cfg$transformationType else (if (isTRUE(norm_cfg$logTransform)) "log2" else "none")
          cat(sprintf("[BATCH] Re-normalizing ComBat-seq count data using user's normalization method '%s' (transformationType: %s, priorCount: %g) for %s...\n",
                      norm_cfg$method, norm_transform, norm_cfg$priorCount, ds_id))

          norm_data_type <- if (!is.null(norm_cfg$dataType)) norm_cfg$dataType else dtype
          norm_mat <- apply_normalization_to_matrix(corrected_expr, norm_cfg$method, norm_transform, norm_cfg$priorCount, norm_data_type)
          saveRDS(norm_mat, file = sprintf("tmp/%s_normalized_expr.rds", ds_id))
          push_main_stack(ds_id, norm_mat, step_name = "batch", metadata = list(method = "combat_seq", reNormalized = TRUE, normMethod = norm_cfg$method))
          
          base_id <- get_base_id(ds_id)
          stack_file <- get_session_path(base_id, "%s_main_stack.rds")
          if (file.exists(stack_file)) {
            stack <- tryCatch(readRDS(stack_file), error = function(e) list())
            if (is.list(stack) && length(stack) > 0) {
              replaced <- FALSE
              for (i in seq_along(stack)) {
                if (stack[[i]]$step == "normalization") {
                  stack[[i]]$data <- norm_mat
                  stack[[i]]$metadata$isNormalized <- TRUE
                  stack[[i]]$metadata$batchCorrected <- TRUE
                  stack[[i]]$metadata$batchMethod <- "combat_seq"
                  replaced <- TRUE
                  break
                }
              }
              if (replaced) {
                saveRDS(stack, stack_file)
                cat(sprintf("[BATCH] Replaced the existing normalized result in the main stack for %s\n", ds_id))
              }
            }
          }
        } else {
          cat(sprintf("[BATCH] Normalization step was not performed by user for %s. Preserving batch-corrected counts without log2 transformation on main stack.\n", ds_id))
          push_main_stack(ds_id, corrected_expr, step_name = "batch", metadata = list(method = "combat_seq", reNormalized = FALSE))
        }
      } else {
        push_main_stack(ds_id, corrected_expr, step_name = "batch", metadata = list(method = curr_method))
      }

      tryCatch({
        if (!is.null(batch_col) && batch_col != "" && (batch_col %in% colnames(clin_df))) {
          clin_df$Batch <- factor(clin_df[[batch_col]])
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = round(corrected_expr), colData = clin_df, design = ~ Batch)
          saveRDS(dds, file = sprintf("tmp/%s_deseq2_obj.rds", ds_id))
          dge <- edgeR::DGEList(counts = corrected_expr, group = clin_df$Batch)
          saveRDS(dge, file = sprintf("tmp/%s_edgeR_obj.rds", ds_id))
        } else {
          dds <- DESeq2::DESeqDataSetFromMatrix(countData = round(corrected_expr), colData = clin_df, design = ~ 1)
          saveRDS(dds, file = sprintf("tmp/%s_deseq2_obj.rds", ds_id))
          dge <- edgeR::DGEList(counts = corrected_expr)
          saveRDS(dge, file = sprintf("tmp/%s_edgeR_obj.rds", ds_id))
        }
      }, error = function(e) {
        cat("[WARNING] Failed to update DESeq2/edgeR objects:", e$message, "\n")
      })
    }
    
    # Determine exact matrices for Before & After Batch Correction PCA
    # Before: normalized data if normalization was performed, else latest data (log2(x+1) if raw readcounts)
    before_pca_mat <- if (is_raw_readcounts) {
      if (has_prev_norm && !is.null(pre_batch_norm_mat) && all(common_samples %in% colnames(pre_batch_norm_mat))) {
        pre_batch_norm_mat[, common_samples, drop = FALSE]
      } else {
        log2(pmax(expr, 0) + 1)
      }
    } else {
      expr
    }

    # After: for ComBat-seq raw readcounts, use re-normalized norm_mat if normalized previously, else log2(corrected_expr + 1)
    after_pca_mat <- if (curr_method == "skip" || curr_method == "none") {
      before_pca_mat
    } else if (is_raw_readcounts && curr_method == "combat_seq") {
      if (has_prev_norm && !is.null(norm_mat) && all(common_samples %in% colnames(norm_mat))) {
        norm_mat[, common_samples, drop = FALSE]
      } else {
        log2(pmax(corrected_expr, 0) + 1)
      }
    } else if (is_raw_readcounts) {
      log2(pmax(corrected_expr, 0) + 1)
    } else {
      corrected_expr
    }

    expr_before_list[[ds_id]] <- before_pca_mat
    expr_after_list[[ds_id]]  <- after_pca_mat
    
    if (!is_cache_hit && curr_method != "skip" && curr_method != "none") {
      save_step_cache_meta(ds_id, "batch", list(
        method = curr_method,
        clinicalBatchCol = d$clinicalBatchCol,
        clinicalGroupCol = d$clinicalGroupCol,
        upstream_fp = upstream_fp,
        clinical_fp = clinical_fp
      ))
    }
    
    batch_val_vec <- if (!is.null(batch_col) && batch_col %in% colnames(clin_df)) as.character(clin_df[[batch_col]]) else rep("Batch1", length(common_samples))
    
    cat(sprintf("[CONSOLE]   Output features after: %d\n", nrow(corrected_expr)))
    cat(sprintf("[CONSOLE]   Duplicate IDs after: %d\n", sum(duplicated(rownames(corrected_expr)))))
    cat(sprintf("[CONSOLE]   Missing values after: %d\n", sum(is.na(corrected_expr))))

    df_d <- data.frame(
      Sample  = common_samples,
      Dataset = rep(ds_display_name, length(common_samples)),
      Batch   = as.factor(batch_val_vec),
      stringsAsFactors = FALSE
    )
    rownames(df_d) <- paste0(ds_id, "_", common_samples)
    clin_combined <- rbind(clin_combined, df_d)

    # Individual Before-Batch PCA (reuses %s_latest_normal_pca.rds coordinates stratified by batch)
    latest_pca_path <- get_session_path(ds_id, "%s_latest_normal_pca.rds")
    latest_pca <- if (file.exists(latest_pca_path)) tryCatch(readRDS(latest_pca_path), error = function(e) NULL) else NULL
    pca_before_ind <- attach_batch_to_pca_obj(latest_pca, batch_val_vec, common_samples, ds_display_name)
    if (is.null(pca_before_ind)) {
      comp_b <- compute_normal_pca(before_pca_mat, ds_id, is_unnormalized_rc = FALSE, ds_name = ds_display_name, save_latest = FALSE)
      if (!is.null(comp_b)) comp_b$isLog2Transformed <- (is_raw_readcounts && !has_prev_norm)
      pca_before_ind <- attach_batch_to_pca_obj(comp_b, batch_val_vec, common_samples, ds_display_name)
    }

    # Individual After-Batch PCA
    if (curr_method == "skip" || curr_method == "none") {
      pca_after_ind <- pca_before_ind
    } else {
      comp_a <- compute_normal_pca(after_pca_mat, ds_id, is_unnormalized_rc = FALSE, ds_name = ds_display_name, save_latest = FALSE)
      if (!is.null(comp_a)) comp_a$isLog2Transformed <- (is_raw_readcounts && !has_prev_norm)
      pca_after_ind <- attach_batch_to_pca_obj(comp_a, batch_val_vec, common_samples, ds_display_name)
    }

    tryCatch(saveRDS(list(before = pca_before_ind, after = pca_after_ind), get_session_path(ds_id, "%s_batch_pca.rds")), error = function(e) NULL)

    ind_before <- ""
    ind_after  <- ""
    uid_val <- get_user_id(ds_id)
    if (!is.null(pca_before_ind)) {
      ind_before <- tryCatch({
        p_b <- build_pca_ggplot(pca_before_ind, color_by = "batch", title_str = sprintf("%s — PCA Before Batch Correction", ds_display_name))
        pdf_b  <- get_session_path(ds_id, "pca_plots_before_batch_%s.pdf")
        png_b  <- get_session_path(ds_id, "pca_plots_before_batch_%s.png")
        svg_b  <- get_session_path(ds_id, "pca_plots_before_batch_%s.svg")
        tiff_b <- get_session_path(ds_id, "pca_plots_before_batch_%s.tiff")
        save_plot_formats(p_b, pdf_b, png_b, svg_b, tiff_b, width = 6.5, height = 4.8)
        register_export_file(uid_val, "pca_plots_before_batch", ds_id, pdf_b, "dp", "batch", ext = "pdf")
        base64enc::base64encode(png_b)
      }, error = function(e) "")
    }
    if (!is.null(pca_after_ind)) {
      ind_after <- tryCatch({
        p_a <- build_pca_ggplot(pca_after_ind, color_by = "batch", title_str = sprintf("%s — PCA After Batch Correction", ds_display_name))
        pdf_a  <- get_session_path(ds_id, "pca_plots_after_batch_%s.pdf")
        png_a  <- get_session_path(ds_id, "pca_plots_after_batch_%s.png")
        svg_a  <- get_session_path(ds_id, "pca_plots_after_batch_%s.svg")
        tiff_a <- get_session_path(ds_id, "pca_plots_after_batch_%s.tiff")
        save_plot_formats(p_a, pdf_a, png_a, svg_a, tiff_a, width = 6.5, height = 4.8)
        register_export_file(uid_val, "pca_plots_after_batch", ds_id, pdf_a, "dp", "batch", ext = "pdf")
        base64enc::base64encode(png_a)
      }, error = function(e) "")
    }

    datasets_pca[[ds_id_full]] <- list(
      before_batch         = ind_before,
      after_batch          = ind_after,
      pcaBeforeInteractive = pca_before_ind,
      pcaAfterInteractive  = pca_after_ind
    )
  }

  common_genes <- NULL
  for (id in names(expr_before_list)) {
    genes <- intersect(rownames(expr_before_list[[id]]), rownames(expr_after_list[[id]]))
    if (is.null(common_genes)) {
      common_genes <- genes
    } else {
      common_genes <- intersect(common_genes, genes)
    }
  }

  if (length(common_genes) > 5) {
    combined_before <- do.call(cbind, lapply(names(expr_before_list), function(id) {
      mat <- expr_before_list[[id]][common_genes, , drop = FALSE]
      colnames(mat) <- paste0(id, "_", colnames(mat))
      mat
    }))

    combined_after <- do.call(cbind, lapply(names(expr_after_list), function(id) {
      mat <- expr_after_list[[id]][common_genes, , drop = FALSE]
      colnames(mat) <- paste0(id, "_", colnames(mat))
      mat
    }))

    comb_before_pca <- combined_before
    comb_after_pca  <- combined_after
    if (any(is.na(comb_before_pca))) comb_before_pca[is.na(comb_before_pca)] <- 0
    if (any(is.na(comb_after_pca)))  comb_after_pca[is.na(comb_after_pca)]  <- 0

    .keep <- {
      vb <- apply(comb_before_pca, 1, var); va <- apply(comb_after_pca, 1, var)
      is.finite(vb) & vb > 0 & is.finite(va) & va > 0
    }
    comb_before_pca <- comb_before_pca[.keep, , drop = FALSE]
    comb_after_pca  <- comb_after_pca[.keep, , drop = FALSE]
    pca_fit_before <- prcomp(t(comb_before_pca), scale. = TRUE)
    pca_fit_after  <- prcomp(t(comb_after_pca),  scale. = TRUE)
    pca_before <- pca_fit_before$x[, 1:2, drop = FALSE]
    pca_after  <- pca_fit_after$x[,  1:2, drop = FALSE]

    var_before <- pca_fit_before$sdev^2 / sum(pca_fit_before$sdev^2)
    var_after  <- pca_fit_after$sdev^2  / sum(pca_fit_after$sdev^2)
    before_pc1_lbl <- sprintf("PC1 (%.1f%%)", var_before[1] * 100)
    before_pc2_lbl <- sprintf("PC2 (%.1f%%)", var_before[2] * 100)
    after_pc1_lbl  <- sprintf("PC1 (%.1f%%)", var_after[1]  * 100)
    after_pc2_lbl  <- sprintf("PC2 (%.1f%%)", var_after[2]  * 100)

    samples_in_pca <- rownames(pca_before)
    clin_aligned <- clin_combined[samples_in_pca, , drop = FALSE]

    comb_before_pts <- lapply(seq_len(nrow(pca_before)), function(i) {
      list(
        sample  = as.character(clin_aligned$Sample[i]),
        pc1     = round(unname(as.numeric(pca_before[i, 1])), 4),
        pc2     = round(unname(as.numeric(pca_before[i, 2])), 4),
        batch   = as.character(clin_aligned$Batch[i]),
        group   = as.character(clin_aligned$Batch[i]),
        dataset = as.character(clin_aligned$Dataset[i])
      )
    })
    comb_after_pts <- lapply(seq_len(nrow(pca_after)), function(i) {
      list(
        sample  = as.character(clin_aligned$Sample[i]),
        pc1     = round(unname(as.numeric(pca_after[i, 1])), 4),
        pc2     = round(unname(as.numeric(pca_after[i, 2])), 4),
        batch   = as.character(clin_aligned$Batch[i]),
        group   = as.character(clin_aligned$Batch[i]),
        dataset = as.character(clin_aligned$Dataset[i])
      )
    })

    comb_before_obj <- list(
      pc1Label = before_pc1_lbl,
      pc2Label = before_pc2_lbl,
      title    = "Combined Datasets — Before Batch Correction",
      points   = comb_before_pts
    )
    comb_after_obj <- list(
      pc1Label = after_pc1_lbl,
      pc2Label = after_pc2_lbl,
      title    = "Combined Datasets — After Batch Correction",
      points   = comb_after_pts
    )

    pb_batch <- build_pca_ggplot(comb_before_obj, color_by = "batch", title_str = NULL)
    pa_batch <- build_pca_ggplot(comb_after_obj,  color_by = "batch", title_str = NULL)

    pb_batch_exp <- build_pca_ggplot(comb_before_obj, color_by = "batch", title_str = "Combined Datasets — Before Correction (Batch)")
    pa_batch_exp <- build_pca_ggplot(comb_after_obj,  color_by = "batch", title_str = "Combined Datasets — After Correction (Batch)")

    return(list(
      before_batch            = plot_to_base64(pb_batch),
      before_group            = "",
      before_covariate        = "",
      after_batch             = plot_to_base64(pa_batch),
      after_group             = "",
      after_covariate         = "",
      before_batch_export     = plot_to_base64(pb_batch_exp),
      before_group_export     = "",
      before_covariate_export = "",
      after_batch_export      = plot_to_base64(pa_batch_exp),
      after_group_export      = "",
      after_covariate_export  = "",
      combined = list(
        before_batch         = plot_to_base64(pb_batch),
        after_batch          = plot_to_base64(pa_batch),
        pcaBeforeInteractive = comb_before_obj,
        pcaAfterInteractive  = comb_after_obj
      ),
      datasets = datasets_pca
    ))
  }

  return(list(
    before_batch = "",
    after_batch  = "",
    combined     = list(before_batch = "", after_batch = ""),
    datasets     = datasets_pca
  ))
}

run_upload_datasets <- function(datasets_list) {
  find_uploaded_file_local <- function(upload_id, user_id = NULL) {
    if (is.null(upload_id) || !nzchar(upload_id)) return(NULL)
    find_uploaded_file(upload_id, user_id)
  }

  results <- list()
  for (ds in datasets_list) {
    ds_id_full <- ds$datasetId
    ds_id   <- get_base_id(ds_id_full)
    ds_name <- if (!is.null(ds$datasetName)) ds$datasetName else (if (!is.null(ds$name)) ds$name else ds_id_full)
    cat(sprintf("[LOG] run_upload_datasets: processing dataset '%s' (id=%s)\n", ds_name, ds_id_full))

    res_entry <- list(datasetId = ds_id_full, missingValuesCount = 0)

    # Map flat metadata fields from API payload with module and inheritance fields
    parsed_ds_info <- get_backend_datasets(ds_id_full)
    ds_module      <- parsed_ds_info$module %||% "dp"
    ds_parent_mod  <- ds$parentModule %||% ds_module
    ds_is_inline   <- isTRUE(ds$isInline) || (!is.null(ds$parentModule) && ds$parentModule != ds_module)
    ds_parent_id   <- ds$parentDatasetId %||% NULL

    expr_meta <- list(
      datasetName          = ds_name,
      isNormalized         = ds$isNormalized,
      dataType             = ds$dataType,
      platform             = ds$platform,
      geneInfoCols         = ds$geneInfoCols,
      geneIdCol            = ds$geneIdCol,
      geneIdType           = ds$geneIdType,
      featureOrientation   = ds$featureOrientation,
      featureIndexValue    = ds$featureIndexValue,
      datasetPurpose       = ds$datasetPurpose,
      isInternalValidation = ds$isInternalValidation,
      module               = ds_module,
      parentModule         = ds_parent_mod,
      isInline             = ds_is_inline,
      parentDatasetId      = ds_parent_id
    )

    clin_meta <- list(
      sampleIdCol     = ds$sampleIdCol,
      groupCol        = ds$groupCol,
      batchCol        = ds$batchCol,
      otherCovariates = ds$otherCovariates,
      referenceGroup  = ds$referenceGroup,
      comparisonGroup = ds$comparisonGroup,
      positiveClass   = ds$positiveClass %||% ds$fs_positiveClass,
      negativeClass   = ds$negativeClass %||% ds$fs_negativeClass,
      module          = ds_module,
      parentModule    = ds_parent_mod,
      isInline        = ds_is_inline,
      parentDatasetId = ds_parent_id
    )

    # Always persist metadata to local RDS files (survives when raw data is not sent)
    dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
    saveRDS(expr_meta, file = sprintf("tmp/%s_expr_metadata.rds", ds_id))
    tryCatch(saveRDS(expr_meta, file = get_session_path(ds_id, "%s_expr_metadata.rds")), error = function(e) NULL)
    tryCatch(saveRDS(expr_meta, file = get_session_path(ds_id, "%s_upload_expr_metadata.rds")), error = function(e) NULL)
    saveRDS(clin_meta, file = sprintf("tmp/%s_clin_metadata.rds", ds_id))
    tryCatch(saveRDS(clin_meta, file = get_session_path(ds_id, "%s_clin_metadata.rds")), error = function(e) NULL)

    expr_cols   <- ds$expressionColumns
    expr_parsed <- ds$expressionParsedData
    expr_raw    <- ds$expressionRawText
    expr_file_path_in <- ds$expressionFilePath
    expr_upload_id <- ds$expressionUploadId

    expr_df <- NULL

    is_desktop_backend <- identical(get_app_mode(), "desktop")

    # 0. Desktop mode: CEL folder (Affymetrix only)
    cel_folder_path <- ds$celFolderPath %||% NULL
    if (!is.null(cel_folder_path) && nzchar(cel_folder_path)) {
      cel_folder_path <- path.expand(cel_folder_path)
    }
    cel_processed <- FALSE
    if (is_desktop_backend && !is.null(cel_folder_path) && nzchar(cel_folder_path) && dir.exists(cel_folder_path)) {
      cat(sprintf("[CEL] Processing folder: %s\n", cel_folder_path))
      # Ensure server correctly ONLY reads CEL files (.CEL or .CEL.gz, case-insensitive)
      cel_files <- list.files(cel_folder_path, pattern = "(?i)\\.cel(\\.gz)?$", full.names = TRUE)
      if (length(cel_files) == 0) {
        res_entry$error <- "No .CEL files found in folder."
        cel_processed <- TRUE
      } else {
        cat(sprintf("[CEL] Found %d CEL files. Executing ReadAffy and rma()...\n", length(cel_files)))
        suppressPackageStartupMessages({
          library(affy, character.only = FALSE)
          library(Biobase, character.only = FALSE)
        })
        raw_data <- affy::ReadAffy(filenames = cel_files)
        Biobase::sampleNames(raw_data) <- sub("(?i)\\.cel(\\.gz)?$", "", basename(cel_files))

        # Auto-detect chip platform from CEL annotation header
        raw_annot <- tryCatch(Biobase::annotation(raw_data), error = function(e) "")
        detected_platform <- NULL
        platforms_json_path <- "sample_data/microarray_platforms.json"
        if (file.exists(platforms_json_path)) {
          platforms_json <- tryCatch(jsonlite::read_json(platforms_json_path), error = function(e) NULL)
          if (!is.null(platforms_json$affymetrix) && !is.null(raw_annot) && nzchar(raw_annot)) {
            clean_annot <- tolower(gsub("[^a-z0-9]", "", raw_annot))
            for (p in platforms_json$affymetrix) {
              pkg_base <- tolower(gsub("[^a-z0-9]", "", sub("[.]db$", "", p$annotation_package %||% "")))
              cdf_base <- tolower(gsub("[^a-z0-9]", "", sub("cdf.*$", "", p$cdf %||% "")))
              plat_base <- tolower(gsub("[^a-z0-9]", "", p$platform %||% ""))
              if (clean_annot %in% c(pkg_base, cdf_base, plat_base) || grepl(clean_annot, pkg_base) || grepl(clean_annot, cdf_base)) {
                detected_platform <- p$platform
                break
              }
            }
          }
        }
        chosen_platform <- ds$geneIdType
        if (is.null(chosen_platform) || !nzchar(chosen_platform) || identical(chosen_platform, "probe_id")) {
          chosen_platform <- detected_platform %||% "HG-U133 Plus 2.0"
        }
        cat(sprintf("[CEL] Platform: '%s' (detected annotation: '%s')\n", chosen_platform, raw_annot %||% "none"))

        norm_eset   <- affy::rma(raw_data, verbose = FALSE); rm(raw_data); gc()
        expr_matrix <- Biobase::exprs(norm_eset);            rm(norm_eset); gc()

        expr_meta$isNormalized <- TRUE
        expr_meta$geneIdCol    <- "probe_id"
        expr_meta$geneInfoCols <- list("probe_id")
        expr_meta$platform     <- ds$platform %||% "affymetrix"
        expr_meta$geneIdType   <- chosen_platform
        expr_meta$microarrayPlatformId <- chosen_platform
        expr_meta$dataType     <- "microarray"

        df_cel <- data.frame(
          probe_id = rownames(expr_matrix),
          as.data.frame(expr_matrix, check.names = FALSE),
          check.names = FALSE, stringsAsFactors = FALSE
        )

        expr_csv_path <- get_session_path(ds_id, "%s_expression.csv")
        fast_write_csv(df_cel, expr_csv_path, row.names = FALSE)
        if (expr_csv_path != sprintf("tmp/%s_expression.csv", ds_id)) {
          fast_write_csv(df_cel, sprintf("tmp/%s_expression.csv", ds_id), row.names = FALSE)
        }
        backup_csv_path <- get_session_path(ds_id, "%s_expression_original_backup.csv")
        file.copy(expr_csv_path, backup_csv_path, overwrite = TRUE)
        if (backup_csv_path != sprintf("tmp/%s_expression_original_backup.csv", ds_id)) {
          file.copy(expr_csv_path, sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)
        }
        res_entry$expressionFile <- expr_csv_path

        saveRDS(expr_meta, get_session_path(ds_id, "%s_expr_metadata.rds"))
        saveRDS(expr_meta, sprintf("tmp/%s_expr_metadata.rds", ds_id))
        saveRDS(expr_meta, get_session_path(ds_id, "%s_upload_expr_metadata.rds"))
        saveRDS(expr_meta, sprintf("tmp/%s_upload_expr_metadata.rds", ds_id))

        push_main_stack(ds_id, expr_matrix, step_name = "upload", metadata = expr_meta)
        main_stack_path <- get_session_path(ds_id, "%s_main_stack.rds")
        main_stack_backup <- get_session_path(ds_id, "%s_main_stack_original_backup.rds")
        file.copy(main_stack_path, main_stack_backup, overwrite = TRUE)
        if (main_stack_path != sprintf("tmp/%s_main_stack.rds", ds_id)) {
          tryCatch(file.copy(sprintf("tmp/%s_main_stack.rds", ds_id), sprintf("tmp/%s_main_stack_original_backup.rds", ds_id), overwrite = TRUE), error = function(e) NULL)
        }

        parsed_data_fresh <- list(
          expr         = expr_matrix,
          anno         = df_cel[, 1, drop = FALSE],
          gene_ids     = as.character(rownames(expr_matrix)),
          samples      = as.character(colnames(expr_matrix)),
          geneIdCol    = "probe_id",
          geneInfoCols = "probe_id",
          isNormalized = TRUE,
          dataType     = "microarray",
          platform     = expr_meta$platform %||% "affymetrix",
          geneIdType   = expr_meta$geneIdType %||% "HG-U133 Plus 2.0"
        )
        saveRDS(parsed_data_fresh, file = get_session_path(ds_id, "%s_original_parsed.rds"))
        saveRDS(parsed_data_fresh, file = sprintf("tmp/%s_original_parsed.rds", ds_id))

        missing_count <- sum(is.na(expr_matrix))
        res_entry$missingValuesCount <- missing_count
        expr_meta$missingValuesCount <- missing_count

        parsed_ds_info <- get_backend_datasets(ds_id_full)
        if (!is.null(parsed_ds_info$module) && parsed_ds_info$module == "de") {
          push_de_stack(ds_id, expr_matrix, step_name = "upload", metadata = expr_meta)
        }

        res_entry$total     <- nrow(expr_matrix)
        res_entry$nFeatures <- nrow(expr_matrix)
        res_entry$samples   <- ncol(expr_matrix)
        res_entry$nSamples  <- ncol(expr_matrix)
        res_entry$columns   <- colnames(df_cel)
        res_entry$sampleIds <- colnames(expr_matrix)
        res_entry$geneIdType <- expr_meta$geneIdType
        res_entry$microarrayPlatformId <- expr_meta$geneIdType
        expr_df <- NULL  # skip normal CSV loading
        cel_processed <- TRUE
      }
    }

    if (!cel_processed && is.null(expr_df)) {
      # 1. Desktop mode: Direct file path (strictly only in desktop app mode)
      if (is_desktop_backend && !is.null(expr_file_path_in) && nzchar(expr_file_path_in) && file.exists(expr_file_path_in)) {
        cat(sprintf("[LOG] run_upload_datasets: reading local file path '%s'\n", expr_file_path_in))
        file.copy(expr_file_path_in, sprintf("tmp/%s_expression.csv", ds_id), overwrite = TRUE)
        file.copy(expr_file_path_in, sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)
        expr_df <- read_csv_preserve_id(sprintf("tmp/%s_expression.csv", ds_id))
        expr_cols <- colnames(expr_df)
        res_entry$expressionFile <- sprintf("tmp/%s_expression.csv", ds_id)
      } else if (!is.null(expr_upload_id) && nzchar(expr_upload_id)) {
        # 2. Web mode: Chunk upload ID
        chunk_file <- find_uploaded_file_local(expr_upload_id, get_user_id(ds_id_full))
        if (!is.null(chunk_file) && file.exists(chunk_file)) {
          cat(sprintf("[LOG] run_upload_datasets: assembling from chunk upload '%s'\n", chunk_file))
          file.copy(chunk_file, sprintf("tmp/%s_expression.csv", ds_id), overwrite = TRUE)
          file.copy(chunk_file, sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)
          unlink(chunk_file, force = TRUE)
          expr_df <- read_csv_preserve_id(sprintf("tmp/%s_expression.csv", ds_id))
          expr_cols <- colnames(expr_df)
          res_entry$expressionFile <- sprintf("tmp/%s_expression.csv", ds_id)
        }
      } else if (!is.null(expr_raw) && nzchar(expr_raw)) {
        # 3. Raw text payload
        delim <- ","
        if (grepl("\t", expr_raw)) {
          delim <- "\t"
        }
        expr_df <- read.delim(text = expr_raw, sep = delim, check.names = FALSE, stringsAsFactors = FALSE)
        expr_cols <- colnames(expr_df)
        file_path <- sprintf("tmp/%s_expression.csv", ds_id)
        fast_write_csv(expr_df, file = file_path, row.names = FALSE)
        file.copy(file_path, sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)
        res_entry$expressionFile <- file_path
      } else if (!is.null(expr_parsed) && length(expr_parsed) > 0 && !is.null(expr_cols)) {
        # 4. Parsed 2D array payload
        expr_result <- save_temp_csv(ds_id, "expression", expr_cols, expr_parsed)
        res_entry$expressionFile <- expr_result$file
        file.copy(expr_result$file, sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)

        if (is.list(expr_parsed) && length(expr_parsed) > 0) {
          max_len <- max(sapply(expr_parsed, length))
          rows_padded <- lapply(expr_parsed, function(x) {
            x_char <- as.character(sapply(x, function(val) if (is.null(val)) "" else val))
            if (length(x_char) < max_len) c(x_char, rep("", max_len - length(x_char))) else x_char
          })
          mat <- matrix(unlist(rows_padded), nrow = length(rows_padded), byrow = TRUE)
          expr_df <- as.data.frame(mat, stringsAsFactors = FALSE)
          if (!is.null(expr_cols) && length(expr_cols) == ncol(expr_df)) {
            colnames(expr_df) <- expr_cols
          }
        }
      } else {
        # 5. Metadata-only update (or unchanged data): Restore from existing backup
        backup_file <- sprintf("tmp/%s_expression_original_backup.csv", ds_id)
        orig_file <- sprintf("tmp/%s_expression.csv", ds_id)
        if (file.exists(backup_file)) {
          cat(sprintf("[LOG] run_upload_datasets: restoring expression data from backup '%s' for metadata update\n", backup_file))
          file.copy(backup_file, orig_file, overwrite = TRUE)
          expr_df <- read_csv_preserve_id(orig_file)
          expr_cols <- colnames(expr_df)
          res_entry$expressionFile <- orig_file
          # Invalidate stale intermediate caches
          unlink(sprintf("tmp/%s_original_parsed.rds", ds_id), force = TRUE)
          unlink(sprintf("tmp/%s_resolved_mapping.rds", ds_id), force = TRUE)
          unlink(sprintf("tmp/%s_normalized_expr.rds", ds_id), force = TRUE)
          unlink(sprintf("tmp/%s_normalized_expr_actual.rds", ds_id), force = TRUE)
          unlink(sprintf("tmp/%s_batch_corrected.rds", ds_id), force = TRUE)
          unlink(sprintf("tmp/%s_processed_matrix.rds", ds_id), force = TRUE)
        } else if (file.exists(orig_file)) {
          cat(sprintf("[LOG] run_upload_datasets: restoring expression data from '%s' for metadata update\n", orig_file))
          expr_df <- read_csv_preserve_id(orig_file)
          expr_cols <- colnames(expr_df)
          res_entry$expressionFile <- orig_file
        }
      }
    }

    if (!is.null(expr_df) && ncol(expr_df) > 0) {
      if (is.na(colnames(expr_df)[1]) || !nzchar(trimws(colnames(expr_df)[1]))) {
        colnames(expr_df)[1] <- if (!is.null(expr_meta$geneIdCol) && nzchar(trimws(expr_meta$geneIdCol))) expr_meta$geneIdCol else "GeneID"
      }
      # Invalidate stale intermediate caches for this dataset ID
      sess_user_id <- get_user_id(ds_id_full)
      dirs_to_clean <- unique(c("tmp", if (nzchar(sess_user_id)) file.path("tmp/user_sessions", sess_user_id) else NULL))
      dirs_to_clean <- dirs_to_clean[dir.exists(dirs_to_clean)]
      cache_patterns <- c(
        "_original_parsed\\.rds$",
        "_resolved_mapping\\.rds$",
        "_normalized_expr\\.rds$",
        "_normalized_expr_actual\\.rds$",
        "_batch_corrected\\.rds$",
        "_batch_expr_actual\\.rds$",
        "_batch_expr\\.rds$",
        "_processed_matrix\\.rds$",
        "_processed_expr\\.rds$",
        "_expr_matrix_annotated\\.rds$",
        "_annotation_results\\.rds$",
        "_annotation_results\\.csv$",
        "_unmapped_results\\.rds$",
        "_unmapped_results\\.csv$"
      )
      for (d_dir in dirs_to_clean) {
        f_list <- list.files(d_dir, full.names = TRUE)
        for (f_item in f_list) {
          if (grepl(ds_id, basename(f_item))) {
            for (c_pat in cache_patterns) {
              if (grepl(c_pat, basename(f_item))) {
                unlink(f_item, force = TRUE)
                break
              }
            }
          }
        }
      }

      # Handle featureOrientation / featureIndexValue:
      # If featureIndexValue > 0 and expressionColumns was provided with actual sample names,
      # ensure column headers reflect the sample names and the sample-ID row is dropped.
      feat_idx <- if (!is.null(ds$featureIndexValue)) as.integer(ds$featureIndexValue) else 0L
      expr_cols_from_payload <- ds$expressionColumns
      if (feat_idx > 0L && !is.null(expr_cols_from_payload) && length(expr_cols_from_payload) >= 2) {
        if (ncol(expr_df) == length(expr_cols_from_payload)) {
          colnames(expr_df) <- expr_cols_from_payload
        }
        # If the sample header row ended up in data row feat_idx, drop it
        if (nrow(expr_df) >= feat_idx) {
          first_data_row <- as.character(expr_df[feat_idx, -1])
          if (any(first_data_row %in% as.character(expr_cols_from_payload[-1]))) {
            expr_df <- expr_df[-seq_len(feat_idx), , drop = FALSE]
          }
        }
      }

      raw_info_cols <- unique(as.character(unlist(expr_meta$geneInfoCols %||% list())))
      raw_info_cols <- raw_info_cols[nzchar(trimws(raw_info_cols))]

      gene_id_col   <- if (!is.null(expr_meta$geneIdCol) && nzchar(trimws(expr_meta$geneIdCol)) && expr_meta$geneIdCol %in% colnames(expr_df)) expr_meta$geneIdCol else colnames(expr_df)[1]
      gene_info_cols <- setdiff(intersect(raw_info_cols, colnames(expr_df)), gene_id_col)
      expr_meta$geneInfoCols <- as.list(gene_info_cols)

      det_pval_cols <- unique(as.character(unlist(ds$detectionPvalCols %||% list())))
      if (length(det_pval_cols) == 0 && (identical(tolower(expr_meta$dataType %||% ""), "microarray") || identical(tolower(ds$dataType %||% ""), "microarray"))) {
        # Safety fallback: auto-detect Detection Pval columns so they never leak into sample columns
        det_pval_cols <- grep("(?i)detection.*pval", colnames(expr_df), perl = TRUE, value = TRUE)
      }
      det_pval_cols <- intersect(det_pval_cols, colnames(expr_df))
      sample_cols   <- setdiff(colnames(expr_df), c(gene_id_col, unlist(gene_info_cols), det_pval_cols))
      if (length(det_pval_cols) > 0) {
        det_rows <- as.character(expr_df[[gene_id_col]])
        det_mat_save <- tryCatch({
          m <- data.matrix(expr_df[, det_pval_cols, drop = FALSE])
          rownames(m) <- det_rows; m
        }, error = function(e) NULL)
        if (!is.null(det_mat_save)) {
          saveRDS(det_mat_save, get_session_path(ds_id, "%s_detection_pval.rds"))
          saveRDS(det_mat_save, sprintf("tmp/%s_detection_pval.rds", ds_id))
        }
      }
      expr_meta$detectionPvalCols <- as.list(det_pval_cols)
      if (length(sample_cols) > 0) {
        sub_sample_df <- expr_df[, sample_cols, drop = FALSE]
        expr_matrix <- safe_numeric_matrix(sub_sample_df)
        gene_ids <- if (!is.null(expr_df[[gene_id_col]])) as.character(trimws(as.character(expr_df[[gene_id_col]]))) else paste0("Feature_", seq_len(nrow(expr_df)))
        rownames(expr_matrix) <- as.character(gene_ids)
        colnames(expr_matrix) <- as.character(sample_cols)

        data_type <- if (!is.null(expr_meta$dataType)) expr_meta$dataType else "readcounts"

        push_main_stack(ds_id, expr_matrix, step_name = "upload", metadata = expr_meta)
        if (data_type == "readcounts") {
          push_counts_stack(ds_id, expr_matrix, step_name = "upload", metadata = expr_meta)
        }
        parsed_ds_info <- get_backend_datasets(ds_id_full)
        if (!is.null(parsed_ds_info$module) && parsed_ds_info$module == "de") {
          push_de_stack(ds_id, expr_matrix, step_name = "upload", metadata = expr_meta)
        }

        # Retain the original name of the Gene ID column
        original_gene_id_col <- if (!is.null(expr_meta$geneIdCol) && nzchar(trimws(expr_meta$geneIdCol))) expr_meta$geneIdCol else gene_id_col
        # Ensure any matching column in gene_info_cols is removed to avoid duplicates
        expr_meta$geneInfoCols <- setdiff(as.character(unlist(expr_meta$geneInfoCols %||% list())), original_gene_id_col)
        expr_meta$geneInfoCols <- expr_meta$geneInfoCols[nzchar(trimws(expr_meta$geneInfoCols))]
        gene_info_cols <- expr_meta$geneInfoCols

        df_out <- data.frame(gene_ids = as.character(gene_ids), check.names = FALSE, stringsAsFactors = FALSE)
        colnames(df_out)[1] <- as.character(original_gene_id_col)
        if (length(gene_info_cols) > 0) {
          for (info_col in gene_info_cols) {
            if (nzchar(trimws(info_col)) && info_col %in% colnames(expr_df) && !is.null(expr_df[[info_col]])) {
              df_out[[info_col]] <- as.character(expr_df[[info_col]])
            }
          }
        }
        df_expr <- as.data.frame(expr_matrix, check.names = FALSE, stringsAsFactors = FALSE)
        df_out <- cbind(df_out, df_expr)

        fast_write_csv(df_out, file = sprintf("tmp/%s_expression.csv", ds_id), row.names = FALSE)

        missing_count <- sum(is.na(expr_matrix))
        res_entry$missingValuesCount <- missing_count
        expr_meta$missingValuesCount <- missing_count

        saveRDS(expr_meta, file = sprintf("tmp/%s_expr_metadata.rds", ds_id))
        saveRDS(expr_meta, file = sprintf("tmp/%s_upload_expr_metadata.rds", ds_id))

        parsed_data_fresh <- list(
          expr         = expr_matrix,
          anno         = df_out[, 1, drop = FALSE],
          gene_ids     = as.character(gene_ids),
          samples      = as.character(sample_cols),
          geneIdCol    = as.character(original_gene_id_col),
          geneInfoCols = as.character(gene_info_cols),
          isNormalized = isTRUE(expr_meta$isNormalized),
          dataType     = expr_meta$dataType %||% "readcounts",
          platform     = expr_meta$platform %||% "",
          geneIdType   = expr_meta$geneIdType %||% "ensembl"
        )
        saveRDS(parsed_data_fresh, file = sprintf("tmp/%s_original_parsed.rds", ds_id))

        file.copy(sprintf("tmp/%s_expression.csv", ds_id), sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)
        file.copy(sprintf("tmp/%s_main_stack.rds", ds_id), sprintf("tmp/%s_main_stack_original_backup.rds", ds_id), overwrite = TRUE)
        if (file.exists(sprintf("tmp/%s_counts_stack.rds", ds_id))) {
          file.copy(sprintf("tmp/%s_counts_stack.rds", ds_id), sprintf("tmp/%s_counts_stack_original_backup.rds", ds_id), overwrite = TRUE)
        }

        res_entry$total <- nrow(expr_matrix)
        res_entry$nFeatures <- nrow(expr_matrix)
        res_entry$samples <- ncol(expr_matrix)
        res_entry$nSamples <- ncol(expr_matrix)
        res_entry$columns <- as.character(colnames(df_out))
        res_entry$sampleIds <- as.character(sample_cols)
      } else {
        cat(sprintf("[LOG] No sample columns detected. Writing info-only/gene list dataset for '%s'...\n", ds_id))
        original_gene_id_col <- if (!is.null(expr_meta$geneIdCol) && expr_meta$geneIdCol != "") expr_meta$geneIdCol else gene_id_col
        expr_meta$geneInfoCols <- setdiff(expr_meta$geneInfoCols, original_gene_id_col)
        
        id_col_name <- if (gene_id_col %in% colnames(expr_df)) gene_id_col else colnames(expr_df)[1]
        df_out <- data.frame(as.character(trimws(as.character(expr_df[[id_col_name]]))), check.names = FALSE, stringsAsFactors = FALSE)
        colnames(df_out)[1] <- as.character(original_gene_id_col)
        if (ncol(expr_df) > 1) {
          other_cols <- setdiff(colnames(expr_df), c(id_col_name, original_gene_id_col))
          for (col in other_cols) {
            df_out[[col]] <- as.character(expr_df[[col]])
          }
        }
        fast_write_csv(df_out, file = sprintf("tmp/%s_expression.csv", ds_id), row.names = FALSE)

        expr_meta$geneInfoCols <- list()
        saveRDS(expr_meta, file = sprintf("tmp/%s_expr_metadata.rds", ds_id))
        saveRDS(expr_meta, file = sprintf("tmp/%s_upload_expr_metadata.rds", ds_id))

        file.copy(sprintf("tmp/%s_expression.csv", ds_id), sprintf("tmp/%s_expression_original_backup.csv", ds_id), overwrite = TRUE)

        res_entry$total <- nrow(df_out)
        res_entry$nFeatures <- nrow(df_out)
        res_entry$samples <- 0
        res_entry$nSamples <- 0
        res_entry$columns <- as.character(colnames(df_out))
        res_entry$sampleIds <- character(0)
      }
    }

    clin_cols   <- ds$clinicalColumns
    clin_parsed <- ds$clinicalParsedData
    clin_raw    <- ds$clinicalRawText
    clin_file_path_in <- ds$clinicalFilePath
    clin_upload_id <- ds$clinicalUploadId
    clin_df <- NULL

    if (is_desktop_backend && !is.null(clin_file_path_in) && nzchar(clin_file_path_in) && file.exists(clin_file_path_in)) {
      cat(sprintf("[LOG] run_upload_datasets: reading clinical direct file '%s'\n", clin_file_path_in))
      clin_df <- read_csv_preserve_id(clin_file_path_in)
      clin_cols <- colnames(clin_df)
      clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
    } else if (!is.null(clin_upload_id) && nzchar(clin_upload_id)) {
      chunk_clin <- find_uploaded_file_local(clin_upload_id, get_user_id(ds_id_full))
      if (!is.null(chunk_clin) && file.exists(chunk_clin)) {
        cat(sprintf("[LOG] run_upload_datasets: reading chunk-uploaded clinical file '%s'\n", chunk_clin))
        clin_df <- read_csv_preserve_id(chunk_clin)
        clin_cols <- colnames(clin_df)
        clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
        unlink(chunk_clin, force = TRUE)
      }
    } else if (!is.null(clin_raw) && nzchar(clin_raw)) {
      delim <- ","
      if (grepl("\t", clin_raw)) {
        delim <- "\t"
      }
      clin_df <- read.delim(text = clin_raw, sep = delim, check.names = FALSE, stringsAsFactors = FALSE)
      clin_cols <- colnames(clin_df)
      clin_parsed <- lapply(1:nrow(clin_df), function(r) {
        as.list(clin_df[r, ])
      })
    } else if ((is.null(clin_parsed) || length(clin_parsed) == 0) && file.exists(sprintf("tmp/%s_clinical.csv", ds_id))) {
      # Clinical metadata update on existing clinical file
      clin_df <- read_csv_preserve_id(sprintf("tmp/%s_clinical.csv", ds_id))
      clin_cols <- colnames(clin_df)
      clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
    }

    if (!is.null(clin_parsed) && length(clin_parsed) > 0 && !is.null(clin_cols)) {
      saveRDS(clin_meta, file = sprintf("tmp/%s_clin_metadata.rds", ds_id))
      final_clin_parsed <- align_and_filter_dataset(ds_id, clin_meta, clin_parsed, clin_cols)
      clin_result <- save_temp_csv(ds_id, "clinical", clin_cols, final_clin_parsed)
      res_entry$clinicalFile <- clin_result$file
    }

    # Authoritative calculation of sample alignment & statistics
    expr_sample_names <- res_entry$sampleIds %||% character(0)
    clin_sample_names <- character(0)
    group_vals <- character(0)

    if (!is.null(clin_df) && nrow(clin_df) > 0) {
      s_id_col <- clin_meta$sampleIdCol %||% colnames(clin_df)[1]
      if (s_id_col %in% colnames(clin_df)) {
        clin_sample_names <- as.character(clin_df[[s_id_col]])
        clin_sample_names <- clin_sample_names[!is.na(clin_sample_names) & clin_sample_names != ""]
      }
      grp_col <- clin_meta$groupCol %||% ""
      if (grp_col %in% colnames(clin_df)) {
        group_vals <- unique(as.character(clin_df[[grp_col]]))
        group_vals <- group_vals[!is.na(group_vals) & group_vals != ""]
      }
    }

    matching_samples <- if (length(clin_sample_names) > 0 && length(expr_sample_names) > 0) intersect(expr_sample_names, clin_sample_names) else expr_sample_names
    missing_clin <- if (length(clin_sample_names) > 0) setdiff(expr_sample_names, clin_sample_names) else character(0)
    clin_no_expr <- if (length(expr_sample_names) > 0) setdiff(clin_sample_names, expr_sample_names) else character(0)

    res_entry$matchingSamples        <- matching_samples
    res_entry$missingClinSamples     <- missing_clin
    res_entry$clinicalNoExprSamples  <- clin_no_expr
    res_entry$groups                 <- group_vals
    res_entry$clinicalColumns        <- if (!is.null(clin_cols)) clin_cols else character(0)

    # Pre-compute initial Pre-Processing QC (Hexbin Mean-Variance, Sample Distribution, Normal PCA)
    if (identical(ds_module, "dp")) {
      expr_latest_up <- get_latest_step_matrix(ds_id, "upload")
      if (!is.null(expr_latest_up) && is.matrix(expr_latest_up) && nrow(expr_latest_up) >= 2 && ncol(expr_latest_up) >= 1) {
        pre_qc <- tryCatch(
          prepare_pre_processing_qc(expr_latest_up, ds_id, expr_meta$dataType %||% "readcounts", isTRUE(expr_meta$isNormalized), ds_name),
          error = function(e) NULL
        )
        if (!is.null(pre_qc)) {
          res_entry$qc <- list(
            before       = pre_qc,
            meanVariance = pre_qc$meanVariance,
            distribution = pre_qc$distribution,
            pca          = pre_qc$pca
          )
        }
      }
    }

    results[[length(results) + 1]] <- res_entry
  }
  return(results)
}

run_skip_step <- function(step, dataset_ids) {
  for (ds_id in dataset_ids) {
    delete_downstream_files(ds_id, step, is_redo = TRUE)
  }
  return(list(status = "success", message = sprintf("Skipped step %s, removed results", step)))
}

run_redo_step <- function(step, dataset_ids) {
  for (ds_id_full in dataset_ids) {
    delete_downstream_files(ds_id_full, step, is_redo = TRUE)
  }
  return(list(status = "success", message = "Discarded downstream results"))
}

generate_pca_plots_for_export <- function(ds_id) {
  base_id <- get_base_id(ds_id)
  user_id_val <- get_user_id(ds_id)
  if (is.null(user_id_val) || !nzchar(user_id_val)) user_id_val <- get_user_id(base_id)
  disp_name <- get_dataset_display_name(base_id)

  meta_path <- get_session_path(base_id, "%s_expr_metadata.rds", fallback = TRUE)
  meta <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL
  is_norm <- isTRUE(meta$isNormalized)
  dtype <- tolower(as.character(meta$dataType %||% "readcounts"))

  norm_cfg_file <- get_session_path(base_id, "%s_norm_config.rds", fallback = TRUE)
  norm_cfg <- if (file.exists(norm_cfg_file)) tryCatch(readRDS(norm_cfg_file), error = function(e) NULL) else NULL
  has_prev_norm <- (!is.null(norm_cfg) && !(norm_cfg$method %in% c("skip", "none"))) ||
                   file.exists(get_session_path(base_id, "%s_normalized_expr_actual.rds")) ||
                   file.exists(sprintf("tmp/%s_normalized_expr_actual.rds", base_id)) ||
                   file.exists(sprintf("tmp/%s_normalized.rds", base_id))

  # 1. Entity 1 — Normal PCA (pca_plots_<base_id>): always ensure it exists and is unstratified by batch
  pca_pdf <- get_session_path(base_id, "pca_plots_%s.pdf")
  pca_png <- get_session_path(base_id, "pca_plots_%s.png")
  pca_svg <- get_session_path(base_id, "pca_plots_%s.svg")
  if (!file.exists(pca_pdf) || !file.exists(pca_png)) {
    latest_pca_path <- get_session_path(base_id, "%s_latest_normal_pca.rds")
    norm_pca_obj <- if (file.exists(latest_pca_path)) tryCatch(readRDS(latest_pca_path), error = function(e) NULL) else NULL
    if (is.null(norm_pca_obj)) {
      expr_norm_src <- get_suitable_expression_matrix(base_id, step = "normalization")
      if (is.null(expr_norm_src)) expr_norm_src <- get_suitable_expression_matrix(base_id, step = "processing")
      if (is.null(expr_norm_src)) expr_norm_src <- get_suitable_expression_matrix(base_id, step = "upload")
      is_unnorm_rc <- !is_norm && !has_prev_norm && identical(dtype, "readcounts")
      norm_pca_obj <- compute_normal_pca(expr_norm_src, base_id, is_unnormalized_rc = is_unnorm_rc, ds_name = disp_name, save_latest = TRUE)
    }
    if (!is.null(norm_pca_obj)) {
      p_norm <- build_pca_ggplot(norm_pca_obj, color_by = "group", title_str = sprintf("%s — Normal PCA", disp_name))
      save_plot_formats(p_norm, pca_pdf, pca_png, pca_svg, width = 6.5, height = 4.8)
      register_export_file(user_id_val, "pca_plots", base_id, pca_pdf, "dp", "normalization", ext = "pdf")
    }
  }

  # 2. Entity 2 — Batch-Stratified PCA (pca_plots_before_batch & pca_plots_after_batch)
  has_batch <- file.exists(sprintf("tmp/%s_batch_expr_actual.rds", base_id)) ||
               file.exists(get_session_path(base_id, "%s_batch_expr_actual.rds")) ||
               file.exists(sprintf("tmp/%s_batch_expr.rds", base_id)) ||
               file.exists(get_session_path(base_id, "%s_batch_expr.rds")) ||
               file.exists(sprintf("tmp/%s_batch.rds", base_id)) ||
               file.exists(sprintf("tmp/%s_batch_corrected_expr.rds", base_id))

  if (has_batch) {
    pdf_b  <- get_session_path(base_id, "pca_plots_before_batch_%s.pdf")
    png_b  <- get_session_path(base_id, "pca_plots_before_batch_%s.png")
    svg_b  <- get_session_path(base_id, "pca_plots_before_batch_%s.svg")
    tiff_b <- get_session_path(base_id, "pca_plots_before_batch_%s.tiff")

    pdf_a  <- get_session_path(base_id, "pca_plots_after_batch_%s.pdf")
    png_a  <- get_session_path(base_id, "pca_plots_after_batch_%s.png")
    svg_a  <- get_session_path(base_id, "pca_plots_after_batch_%s.svg")
    tiff_a <- get_session_path(base_id, "pca_plots_after_batch_%s.tiff")

    batch_pca_rds <- get_session_path(base_id, "%s_batch_pca.rds")
    saved_batch_pca <- if (file.exists(batch_pca_rds)) tryCatch(readRDS(batch_pca_rds), error = function(e) NULL) else NULL

    if (!file.exists(pdf_b) && !is.null(saved_batch_pca$before)) {
      p_b <- build_pca_ggplot(saved_batch_pca$before, color_by = "batch", title_str = sprintf("%s — PCA Before Batch Correction", disp_name))
      save_plot_formats(p_b, pdf_b, png_b, svg_b, tiff_b, width = 6.5, height = 4.8)
      register_export_file(user_id_val, "pca_plots_before_batch", base_id, pdf_b, "dp", "batch", ext = "pdf")
    }
    if (!file.exists(pdf_a) && !is.null(saved_batch_pca$after)) {
      p_a <- build_pca_ggplot(saved_batch_pca$after, color_by = "batch", title_str = sprintf("%s — PCA After Batch Correction", disp_name))
      save_plot_formats(p_a, pdf_a, png_a, svg_a, tiff_a, width = 6.5, height = 4.8)
      register_export_file(user_id_val, "pca_plots_after_batch", base_id, pdf_a, "dp", "batch", ext = "pdf")
    }
  }
}

# Run supplement clinical asynchronously in worker process
run_supplement_clinical <- function(payload) {
  ds_id_full <- payload$datasetId
  ds_id <- get_base_id(ds_id_full)
  clin_cols <- payload$clinicalColumns
  clin_parsed <- payload$clinicalParsedData
  clin_raw <- payload$clinicalRawText
  clin_file_path <- payload$clinicalFilePath
  clin_upload_id <- payload$clinicalUploadId
  clin_sample_id_col <- payload$clinicalSampleIdCol
  clin_group_col <- payload$clinicalGroupCol
  clin_batch_col <- payload$clinicalBatchCol
  clin_other_covariates <- payload$clinicalOtherCovariates
  pos_class <- if (!is.null(payload$positiveClass)) payload$positiveClass else payload$fs_positiveClass
  neg_class <- if (!is.null(payload$negativeClass)) payload$negativeClass else payload$fs_negativeClass

  cat(sprintf("[LOG] run_supplement_clinical called for datasetId='%s'\n", ds_id_full))

  user_id <- get_user_id(ds_id_full)
  clin_df <- NULL
  is_desktop_backend <- identical(get_app_mode(), "desktop")
  if (is_desktop_backend && !is.null(clin_file_path) && nzchar(clin_file_path) && file.exists(clin_file_path)) {
    cat(sprintf("[LOG] run_supplement_clinical reading clinical direct path '%s'\n", clin_file_path))
    clin_df <- read_csv_preserve_id(clin_file_path)
    clin_cols <- colnames(clin_df)
    clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
  } else if (!is.null(clin_upload_id) && nzchar(clin_upload_id)) {
    assembled_path <- find_uploaded_file(clin_upload_id, user_id)
    if (!is.null(assembled_path) && file.exists(assembled_path)) {
      cat(sprintf("[LOG] run_supplement_clinical reading chunk-uploaded clinical file '%s'\n", assembled_path))
      clin_df <- read_csv_preserve_id(assembled_path)
      clin_cols <- colnames(clin_df)
      clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
    } else {
      cat(sprintf("[LOG] run_supplement_clinical chunk-uploaded clinical file '%s' not found for user '%s'\n", clin_upload_id, user_id))
    }
  } else if (!is.null(clin_raw) && nzchar(clin_raw)) {
    delim <- if (grepl("\t", clin_raw)) "\t" else ","
    clin_df <- read.delim(text = clin_raw, sep = delim, check.names = FALSE, stringsAsFactors = FALSE)
    clin_cols <- colnames(clin_df)
    clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
  }

  if (is.null(clin_df) && (is.null(clin_parsed) || length(clin_parsed) == 0)) {
    existing_clin_path <- get_clinical_path(ds_id, fallback = TRUE)
    if (file.exists(existing_clin_path)) {
      cat(sprintf("[LOG] run_supplement_clinical reading existing server clinical file '%s'\n", existing_clin_path))
      clin_df <- read_csv_preserve_id(existing_clin_path)
      clin_cols <- colnames(clin_df)
      clin_parsed <- lapply(1:nrow(clin_df), function(r) as.list(clin_df[r, ]))
    }
  }

  if (is.null(clin_parsed) || length(clin_parsed) == 0) {
    if (!is.null(payload$clinicalParsedData) && length(payload$clinicalParsedData) > 0) {
      clin_parsed <- payload$clinicalParsedData
      if (is.null(clin_cols) || length(clin_cols) == 0) {
        clin_cols <- payload$clinicalColumns
      }
    }
  }

  if (is.null(ds_id) || is.null(clin_cols) || is.null(clin_parsed) || length(clin_parsed) == 0) {
    stop("Missing required clinical fields or data")
  }

  clin_meta <- list(
    sampleIdCol     = clin_sample_id_col,
    groupCol        = clin_group_col,
    batchCol        = clin_batch_col,
    otherCovariates = clin_other_covariates,
    positiveClass   = pos_class,
    negativeClass   = neg_class
  )

  # Clean and align datasets using current ds_id
  final_clin_parsed <- align_and_filter_dataset(ds_id, clin_meta, clin_parsed, clin_cols)

  # Write clinical metadata and data exclusively for target ds_id
  clin_meta_target_path <- get_clin_metadata_path(ds_id, fallback = TRUE)
  saveRDS(clin_meta, file = clin_meta_target_path)
  tryCatch(saveRDS(clin_meta, file = sprintf("tmp/%s_clin_metadata.rds", ds_id)), error = function(e) NULL)
  save_temp_csv(ds_id, "clinical", clin_cols, final_clin_parsed)

  clin_result <- list(file = sprintf("tmp/%s_clinical.csv", ds_id))

  # Read updated expression dimensions
  n_samples <- 0
  n_features <- 0
  sample_cols <- character(0)
  expr_file_path <- get_session_path(ds_id, "%s_expression.csv", fallback = TRUE)
  if (file.exists(expr_file_path)) {
    expr_df <- read_csv_preserve_id(expr_file_path)
    meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds", fallback = TRUE)
    gene_info_cols <- character(0)
    if (file.exists(meta_path)) {
      meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
      gene_info_cols <- if (!is.null(meta$geneInfoCols)) unlist(meta$geneInfoCols) else character(0)
    }
    gene_id_col <- colnames(expr_df)[1]
    sample_cols <- setdiff(colnames(expr_df), c(gene_id_col, gene_info_cols, "entrez_id", "gene_symbol", "gene_biotype"))
    n_samples <- length(sample_cols)
    n_features <- nrow(expr_df)
  }

  # Compute clinical groups if groupCol provided
  groups_list <- list()
  if (!is.null(clin_group_col) && nzchar(clin_group_col) && !is.null(clin_cols) && clin_group_col %in% clin_cols && length(final_clin_parsed) > 0) {
    g_idx <- which(clin_cols == clin_group_col)[1]
    raw_vals <- vapply(final_clin_parsed, function(row) as.character(row[[g_idx]] %||% ""), character(1))
    valid_vals <- unique(raw_vals[nzchar(raw_vals) & raw_vals != "NA" & raw_vals != "NaN"])
    groups_list <- as.list(valid_vals)
  }

  return(list(
    status = "success",
    datasetId = ds_id_full,
    clinicalFile = clin_result$file,
    samples = n_samples,
    sampleIds = sample_cols,
    total = n_features,
    groups = groups_list,
    message = sprintf("Supplemented clinical data for %s", ds_id_full)
  ))
}

