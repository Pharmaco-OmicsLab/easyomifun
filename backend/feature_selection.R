# ==============================================================================
# PIPELINE CONFIGURATION NOTE:
# The entire classification and evaluation pipeline MUST compute and report 
# BALANCED ACCURACY (unbiased by class imbalance) rather than simple overall accuracy.
# Make sure any new evaluation, metric presentation, or cross-validation loop 
# returns/displays balanced accuracy under the 'acc' field for frontend display.
# ==============================================================================

library(jsonlite)

if (identical(Sys.info()[["sysname"]], "Linux")) {
  options(bitmapType = "cairo")
}

# Import helpers from processing.R
source("processing.R")
source("shared_utils.R")
source("Plot_ROC.R")

# Model labels map for display names
MODEL_LABELS_MAP <- list(
  stabl = "STABL",
  boruta = "Boruta",
  gbm = "GBM",
  randomforest = "RF",
  logistic = "Logistic Regression",
  svm = "SVM"
)

parse_models_payload <- function(models_input, top_parameters = NULL) {
  model_names <- c()
  params_map  <- list()

  if (is.character(models_input)) {
    model_names <- models_input
    if (!is.null(top_parameters) && is.list(top_parameters)) {
      params_map <- top_parameters
    }
  } else if (is.list(models_input)) {
    keys <- names(models_input)
    if (!is.null(keys) && any(nzchar(keys))) {
      for (k in keys) {
        if (!nzchar(k)) next
        item <- models_input[[k]]
        m_name <- k
        p <- list()
        if (is.list(item)) {
          if (!is.null(item$model) && as.character(item$model) != "") {
            m_name <- as.character(item$model)
          } else if (!is.null(item$name) && as.character(item$name) != "") {
            m_name <- as.character(item$name)
          }
          if (!is.null(item$parameters) && is.list(item$parameters)) {
            p <- item$parameters
          }
        }
        model_names <- c(model_names, m_name)
        params_map[[m_name]] <- p
      }
    } else {
      for (item in models_input) {
        if (is.character(item)) {
          model_names <- c(model_names, item)
        } else if (is.list(item)) {
          m_name <- if (!is.null(item$model)) item$model else (if (!is.null(item$name)) item$name else item[[1]])
          m_name <- as.character(m_name)
          p <- if (!is.null(item$parameters) && is.list(item$parameters)) item$parameters else list()
          model_names <- c(model_names, m_name)
          params_map[[m_name]] <- p
        }
      }
    }
  }

  if (!is.null(top_parameters) && is.list(top_parameters)) {
    for (m in names(top_parameters)) {
      if (is.null(params_map[[m]]) || length(params_map[[m]]) == 0) {
        params_map[[m]] <- top_parameters[[m]]
      }
    }
  }

  return(list(models = unique(model_names), parameters = params_map))
}

get_param <- function(params, model_name, key, default) {
  if (is.null(params)) return(default)
  val <- NULL
  if (!is.null(params[[model_name]]) && is.list(params[[model_name]]) && !is.null(params[[model_name]][[key]])) {
    val <- params[[model_name]][[key]]
  } else if (!is.null(params[[key]])) {
    val <- params[[key]]
  }
  if (is.null(val)) return(default)
  if (is.numeric(default)) {
    num_val <- suppressWarnings(as.numeric(val))
    if (length(num_val) > 0 && !is.na(num_val)) return(num_val) else return(default)
  }
  return(as.character(val))
}

# Helper to detect the specific data type of a dataset
get_dataset_data_type <- function(d) {
  if (!is.null(d$dataType) && nzchar(as.character(d$dataType))) {
    return(tolower(as.character(d$dataType)))
  }
  if (!is.null(d$submittedDataType) && nzchar(as.character(d$submittedDataType))) {
    return(tolower(as.character(d$submittedDataType)))
  }
  base_id <- get_base_id(d$id)
  meta_path <- sprintf("tmp/%s_upload_expr_metadata.rds", base_id)
  if (!file.exists(meta_path)) meta_path <- sprintf("tmp/%s_expr_metadata.rds", base_id)
  if (file.exists(meta_path)) {
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (!is.null(meta$dataType) && nzchar(as.character(meta$dataType))) {
      return(tolower(as.character(meta$dataType)))
    }
  }
  return("readcounts")
}

# Helper to classify data type into higher hierarchy group
# transcriptomics: readcounts, microarray
# proteomics: proteomics
# others: others
get_hierarchy_group <- function(data_type) {
  if (is.null(data_type) || is.na(data_type) || data_type == "") return("others")
  dt <- tolower(as.character(data_type))
  if (dt %in% c("readcounts", "microarray")) {
    return("transcriptomics")
  } else if (dt == "proteomics") {
    return("proteomics")
  } else {
    return("others")
  }
}

# Helper to determine if a dataset is standalone FS vs inline FS submodule
get_ds_is_standalone_fs <- function(ds_id_full) {
  parsed <- get_backend_datasets(ds_id_full)
  if (!is.null(parsed$isInline)) {
    return(!isTRUE(parsed$isInline))
  }
  if (!is.null(parsed$module) && !is.null(parsed$parentModule)) {
    return(parsed$module == "fs" && parsed$parentModule == "fs")
  }
  return(grepl("_fs", ds_id_full))
}

# Helper to extract feature names from a parsed expression dataset according to its orientation
get_dataset_features <- function(parsed_expr, ds_id_full) {
  if (is.null(parsed_expr) || is.null(parsed_expr$expr)) return(character(0))
  expr <- parsed_expr$expr
  
  # Check metadata orientation if available
  base_id <- get_base_id(ds_id_full)
  meta_path <- get_session_path(base_id, "%s_upload_expr_metadata.rds")
  if (!file.exists(meta_path)) meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
  orientation <- "column"
  if (file.exists(meta_path)) {
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (!is.null(meta$featureOrientation)) orientation <- meta$featureOrientation
  }
  
  # Load clinical sample IDs to disambiguate rows vs columns
  clin_path <- get_clinical_path(ds_id_full)
  clin_meta_path <- get_clin_metadata_path(ds_id_full)
  clin_samples <- character(0)
  if (file.exists(clin_path) && file.exists(clin_meta_path)) {
    raw_clin <- tryCatch(read_csv_preserve_id(clin_path), error = function(e) NULL)
    meta_c <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
    if (!is.null(raw_clin) && !is.null(meta_c)) {
      clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), meta_c$sampleIdCol)
      if (!is.null(clin_df)) clin_samples <- rownames(clin_df)
    }
  }
  
  if (length(clin_samples) > 0) {
    if (length(intersect(rownames(expr), clin_samples)) > length(intersect(colnames(expr), clin_samples))) {
      # Rows are samples -> Columns are features (features as headers)
      return(as.character(colnames(expr)))
    } else {
      # Columns are samples -> Rows are features (features in a single column)
      return(as.character(rownames(expr)))
    }
  }
  
  if (orientation == "headers" || orientation == "row") {
    return(as.character(colnames(expr)))
  } else {
    return(as.character(rownames(expr)))
  }
}

# Helper to retrieve shared features across multiple datasets
get_shared_features_from_datasets <- function(datasets) {
  if (length(datasets) <= 1) return(NULL)
  
  all_feature_sets <- list()
  for (d in datasets) {
    ds_id_val <- d$id %||% d$datasetId
    is_standalone <- get_ds_is_standalone_fs(ds_id_val)
    parsed_expr <- tryCatch(
      get_backend_dataset(ds_id_val, original = is_standalone, step = "fs"),
      error = function(e) NULL
    )
    feats <- get_dataset_features(parsed_expr, ds_id_val)
    if (length(feats) > 0) {
      all_feature_sets[[ds_id_val]] <- as.character(feats)
    }
  }
  
  if (length(all_feature_sets) <= 1) return(NULL)
  
  shared_features <- Reduce(intersect, all_feature_sets)
  return(as.character(shared_features))
}

# Helper to merge expression and clinical data for ML tasks
prepare_ml_data <- function(d, max_features = NULL, shared_features = NULL) {
  ds_id_full <- d$id
  cat(sprintf("[DEBUG] prepare_ml_data: ds_id_full = %s\n", ds_id_full))
  parsed_id <- get_backend_datasets(ds_id_full)
  ds_id <- parsed_id$base_id
  cat(sprintf("[DEBUG] prepare_ml_data: base_id = %s\n", ds_id))
  
  # Load expression metadata
  orientation <- "column"
  feat_index <- 1
  meta_path <- get_session_path(ds_id, "%s_upload_expr_metadata.rds")
  if (!file.exists(meta_path)) meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
  cat(sprintf("[DEBUG] prepare_ml_data: meta_path = %s (exists = %s)\n", meta_path, file.exists(meta_path)))
  if (file.exists(meta_path)) {
    meta <- readRDS(meta_path)
    if (!is.null(meta$featureOrientation)) orientation <- meta$featureOrientation
    if (!is.null(meta$featureIndexValue)) feat_index <- as.numeric(meta$featureIndexValue)
  }
  cat(sprintf("[DEBUG] prepare_ml_data: orientation = %s, feat_index = %d\n", orientation, feat_index))
  
  # Inline FS uses processed main stack data from DP (original = FALSE)
  # Standalone FS also prioritizes clean uploaded/processed matrix (original = FALSE), falling back to raw cache
  is_standalone_fs <- get_ds_is_standalone_fs(ds_id_full)
  cat(sprintf("[DEBUG] prepare_ml_data: is_standalone_fs = %s (from metadata)\n", is_standalone_fs))
  parsed_expr <- get_backend_dataset(ds_id_full, original = FALSE, step = "fs")
  if (is.null(parsed_expr) || is.null(parsed_expr$expr)) {
    parsed_expr <- get_backend_dataset(ds_id_full, original = TRUE)
  }
  cat(sprintf("[DEBUG] prepare_ml_data: parsed_expr is NULL = %s\n", is.null(parsed_expr)))
  if (is.null(parsed_expr) || is.null(parsed_expr$expr)) {
    stop(sprintf("Expression data could not be loaded for dataset '%s' (%s).", d$name %||% ds_id_full, ds_id_full))
  }
  expr <- parsed_expr$expr
  cat(sprintf("[DEBUG] prepare_ml_data: expr dimensions = %s\n", paste(dim(expr), collapse = "x")))
  
  # Load clinical df
  clin_path <- get_clinical_path(ds_id_full, fallback = TRUE)
  clin_meta_path <- get_clin_metadata_path(ds_id_full, fallback = TRUE)
  cat(sprintf("[DEBUG] prepare_ml_data: clin_path = %s (exists = %s)\n", clin_path, file.exists(clin_path)))
  cat(sprintf("[DEBUG] prepare_ml_data: clin_meta_path = %s (exists = %s)\n", clin_meta_path, file.exists(clin_meta_path)))
  clin_df <- NULL
  if (file.exists(clin_path) && file.exists(clin_meta_path)) {
    raw_clin <- read_csv_preserve_id(clin_path)
    meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
    sample_id_col <- if (!is.null(meta$sampleIdCol)) meta$sampleIdCol else (d$clinicalSampleIdCol %||% colnames(raw_clin)[1])
    clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
  } else if (file.exists(clin_path)) {
    raw_clin <- read_csv_preserve_id(clin_path)
    sample_id_col <- d$clinicalSampleIdCol %||% colnames(raw_clin)[1]
    clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
  } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0 && !is.null(d$clinicalColumns) && length(d$clinicalColumns) > 0) {
    samp_id_col <- d$clinicalSampleIdCol %||% d$clinicalColumns[1]
    raw_clin <- if (is.data.frame(d$clinicalParsedData)) {
      d$clinicalParsedData
    } else {
      mat_rows <- lapply(d$clinicalParsedData, function(row) {
        if (is.list(row)) sapply(row, function(x) if (is.null(x)) "" else as.character(x)) else as.character(row)
      })
      as.data.frame(do.call(rbind, mat_rows), stringsAsFactors = FALSE)
    }
    colnames(raw_clin) <- d$clinicalColumns
    clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), samp_id_col)
  }
  
  cat(sprintf("[DEBUG] prepare_ml_data: clin_df is NULL = %s\n", is.null(clin_df)))
  if (is.null(clin_df)) {
    stop(sprintf("Clinical data or clinical metadata not found for dataset '%s' (%s).", d$name %||% ds_id_full, ds_id_full))
  }
  
  # Determine group column
  group_col <- d$clinicalGroupCol %||% d$groupCol
  if (is.null(group_col) || !nzchar(group_col) || !(group_col %in% colnames(clin_df))) {
    if (file.exists(clin_meta_path)) {
      meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
      if (!is.null(meta$groupCol) && nzchar(meta$groupCol) && meta$groupCol %in% colnames(clin_df)) {
        group_col <- meta$groupCol
      }
    }
  }
  if (is.null(group_col) || !nzchar(group_col) || !(group_col %in% colnames(clin_df))) {
    tmp_meta_path <- sprintf("tmp/%s_clin_metadata.rds", base_id)
    if (file.exists(tmp_meta_path)) {
      meta_tmp <- tryCatch(readRDS(tmp_meta_path), error = function(e) NULL)
      if (!is.null(meta_tmp$groupCol) && nzchar(meta_tmp$groupCol) && meta_tmp$groupCol %in% colnames(clin_df)) {
        group_col <- meta_tmp$groupCol
      }
    }
  }
  if (is.null(group_col) || !nzchar(group_col) || !(group_col %in% colnames(clin_df))) {
    cand_group <- intersect(c("Group", "group", "Condition", "condition", "Status", "status", "Diagnosis", "diagnosis", "Disease Type", "disease_type", "Type", "type"), colnames(clin_df))
    if (length(cand_group) > 0) {
      group_col <- cand_group[1]
    }
  }
  if (is.null(group_col) || !nzchar(group_col) || !(group_col %in% colnames(clin_df))) {
    stop(sprintf("Group column '%s' not found in clinical data for '%s'. Available: %s",
                 group_col %||% "NULL", d$name %||% ds_id_full, paste(colnames(clin_df), collapse=", ")))
  }
  
  # Determine positive and negative classes
  pos_class <- d$positiveClass %||% d$fs_positiveClass
  neg_class <- d$negativeClass %||% d$fs_negativeClass
  if (is.null(pos_class) || !nzchar(pos_class) || is.null(neg_class) || !nzchar(neg_class)) {
    if (file.exists(clin_meta_path)) {
      meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
      if (!is.null(meta)) {
        if (is.null(pos_class) || !nzchar(pos_class)) pos_class <- meta$positiveClass %||% meta$fs_positiveClass
        if (is.null(neg_class) || !nzchar(neg_class)) neg_class <- meta$negativeClass %||% meta$fs_negativeClass
      }
    }
  }

  create_ordered_factor <- function(vals) {
    if (!is.null(pos_class) && nzchar(pos_class) && !is.null(neg_class) && nzchar(neg_class)) {
      u_vals <- unique(as.character(vals))
      if (pos_class %in% u_vals && neg_class %in% u_vals) {
        return(factor(as.character(vals), levels = c(neg_class, pos_class)))
      }
    }
    return(as.factor(vals))
  }

  # Filter out empty or NA group values
  valid_samples <- rownames(clin_df)[!is.na(clin_df[[group_col]]) & clin_df[[group_col]] != ""]
  clin_df <- clin_df[valid_samples, , drop = FALSE]
  
  # Align samples between expression matrix and clinical metadata
  samples_in_cols <- intersect(colnames(expr), rownames(clin_df))
  samples_in_rows <- intersect(rownames(expr), rownames(clin_df))
  
  if (length(samples_in_cols) > 0) {
    # Orientation: Features are ROWS (single column in raw file), Samples are COLUMNS
    if (!is.null(shared_features)) {
      keep_rows <- intersect(rownames(expr), shared_features)
      if (length(keep_rows) > 0) {
        expr <- expr[keep_rows, , drop = FALSE]
      }
    }
    common_samples <- intersect(colnames(expr), rownames(clin_df))
    expr_sub <- expr[, common_samples, drop = FALSE]
    clin_df <- clin_df[common_samples, , drop = FALSE]
    groups <- create_ordered_factor(clin_df[[group_col]])
    if (length(levels(groups)) != 2) {
      stop(sprintf("Clinical group column '%s' must contain exactly 2 unique classes for feature selection in dataset '%s' (found %d).", group_col, d$name %||% ds_id_full, length(levels(groups))))
    }
    
    if (is.null(max_features)) {
      top_genes <- as.character(rownames(expr_sub))
    } else {
      p_values <- apply(expr_sub, 1, function(row) {
        tryCatch({
          anova_fit <- aov(row ~ groups)
          summary(anova_fit)[[1]][["Pr(>F)"]][1]
        }, error = function(e) 1.0)
      })
      p_values[is.na(p_values)] <- 1.0
      top_genes <- as.character(names(sort(p_values))[1:min(max_features, length(p_values))])
    }
    
    ml_df <- as.data.frame(t(expr_sub[top_genes, , drop = FALSE]))
    colnames(ml_df) <- make.names(as.character(top_genes))
    ml_df$Group <- groups
    return(list(df = ml_df, original_genes = as.character(top_genes), clin_df = clin_df))
    
  } else if (length(samples_in_rows) > 0) {
    # Orientation: Features are COLUMNS (headers in raw file), Samples are ROWS
    if (!is.null(shared_features)) {
      keep_cols <- intersect(colnames(expr), shared_features)
      if (length(keep_cols) > 0) {
        expr <- expr[, keep_cols, drop = FALSE]
      }
    }
    common_samples <- intersect(rownames(expr), rownames(clin_df))
    expr_sub <- expr[common_samples, , drop = FALSE]
    clin_df <- clin_df[common_samples, , drop = FALSE]
    groups <- create_ordered_factor(clin_df[[group_col]])
    if (length(levels(groups)) != 2) {
      stop(sprintf("Clinical group column '%s' must contain exactly 2 unique classes for feature selection in dataset '%s' (found %d).", group_col, d$name %||% ds_id_full, length(levels(groups))))
    }
    
    if (is.null(max_features)) {
      top_genes <- as.character(colnames(expr_sub))
    } else {
      p_values <- apply(expr_sub, 2, function(col_data) {
        tryCatch({
          anova_fit <- aov(col_data ~ groups)
          summary(anova_fit)[[1]][["Pr(>F)"]][1]
        }, error = function(e) 1.0)
      })
      p_values[is.na(p_values)] <- 1.0
      top_genes <- as.character(names(sort(p_values))[1:min(max_features, length(p_values))])
    }
    
    ml_df <- as.data.frame(expr_sub[, top_genes, drop = FALSE])
    colnames(ml_df) <- make.names(as.character(top_genes))
    ml_df$Group <- groups
    return(list(df = ml_df, original_genes = as.character(top_genes), clin_df = clin_df))
    
  } else {
    stop(sprintf("No matching samples found between expression data and clinical data for '%s'. Check sample IDs in both files.", d$name %||% ds_id_full))
  }
}

# Helper to compute classification metrics (accuracy, balanced accuracy, PPV, NPV, AUC)
evaluate_predictions <- function(predictions, true_labels, probabilities = NULL, target_prevalence = NULL) {
  pred_str <- as.character(predictions)
  true_str <- as.character(true_labels)

  orig_lvls <- levels(true_labels)
  lvls <- unique(c(orig_lvls, true_str, pred_str))
  lvls <- lvls[!is.na(lvls) & lvls != ""]
  if (length(lvls) < 2) lvls <- c("Class1", "Class2")

  predictions <- factor(pred_str, levels = lvls)
  true_labels <- factor(true_str, levels = lvls)

  # Guard: drop NA rows
  valid <- !is.na(predictions) & !is.na(true_labels)
  predictions <- predictions[valid]
  true_labels <- true_labels[valid]
  if (!is.null(probabilities)) probabilities <- probabilities[valid]

  # Confusion matrix
  tbl <- table(FactorPredicted = factor(predictions, levels = lvls), True = factor(true_labels, levels = lvls))

  # Calculate Balanced Accuracy.
  per_class_recall <- diag(tbl) / pmax(1, colSums(tbl))
  per_class_recall[is.nan(per_class_recall)] <- 0
  bacc <- mean(per_class_recall)

  # Binary classifier case (assume second level is positive class)
  if (length(lvls) == 2) {
    tp <- tbl[2, 2]
    fp <- tbl[2, 1]
    fn <- tbl[1, 2]
    tn <- tbl[1, 1]

    ppv_emp <- if ((tp + fp) > 0) tp / (tp + fp) else 0  # Positive Predictive Value (Empirical)
    npv_emp <- if ((tn + fn) > 0) tn / (tn + fn) else 0  # Negative Predictive Value (Empirical)
    sens    <- if ((tp + fn) > 0) tp / (tp + fn) else 0  # Sensitivity (Recall)
    spec    <- if ((tn + fp) > 0) tn / (tn + fp) else 0  # Specificity

    ppv <- ppv_emp
    npv <- npv_emp

    # Optional Bayes' theorem prevalence scaling if target_prevalence specified
    if (!is.null(target_prevalence) && is.numeric(target_prevalence) && target_prevalence > 0 && target_prevalence < 1) {
      prev <- as.numeric(target_prevalence)
      ppv_scaled <- (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
      npv_scaled <- (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
      ppv <- ppv_scaled
      npv <- npv_scaled
    }

    auc_val <- NA
    fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))

    if (!is.null(probabilities) && length(probabilities) > 1) {
      pos_probs <- as.numeric(probabilities)
      valid_p <- !is.na(pos_probs) & !is.na(true_labels)
      pos_probs <- pos_probs[valid_p]
      true_sub  <- true_labels[valid_p]

      if (length(unique(pos_probs)) > 1 && length(unique(true_sub)) > 1) {
        roc_obj <- tryCatch({
          pROC::roc(response = true_sub, predictor = pos_probs, quiet = TRUE)
        }, error = function(e) NULL)

        if (!is.null(roc_obj)) {
          auc_val <- as.numeric(pROC::auc(roc_obj))
          spec_roc <- roc_obj$specificities
          sens_roc <- roc_obj$sensitivities
          fpr_vec <- 1 - spec_roc
          tpr_vec <- sens_roc
          ord <- order(fpr_vec, tpr_vec)
          fpr_tpr <- list(fpr = c(0, fpr_vec[ord], 1), tpr = c(0, tpr_vec[ord], 1))
        } else {
          auc_val <- bacc
          fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))
        }
      } else if (length(unique(pos_probs)) == 1) {
        cat(sprintf("[WARN] evaluate_predictions: all predicted probs are identical (%.4f). Model did not discriminate. Returning AUC=accuracy as proxy.\n", pos_probs[1]))
        auc_val <- bacc
        fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))
      } else {
        cat(sprintf("[WARN] evaluate_predictions: test split contains only one class. AUC undefined; using accuracy as proxy.\n"))
        auc_val <- bacc
        fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))
      }
    } else {
      cat("[WARN] evaluate_predictions: no valid probability predictions — using accuracy as AUC proxy.\n")
      auc_val <- bacc
      fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))
    }
  } else {
    tp <- 0; fp <- 0; fn <- 0; tn <- 0
    ppv_emp <- bacc; npv_emp <- bacc; ppv <- bacc; npv <- bacc; sens <- bacc; spec <- bacc; auc_val <- bacc
    fpr_tpr <- list(fpr = c(0, 1), tpr = c(0, 1))
  }

  return(list(
    acc     = round(bacc, 3),
    bacc    = round(bacc, 3),
    ppv     = round(ppv, 3),
    npv     = round(npv, 3),
    ppv_emp = round(ppv_emp, 3),
    npv_emp = round(npv_emp, 3),
    sens    = round(sens, 3),
    auc     = if (is.na(auc_val)) round(bacc, 3) else round(auc_val, 3),
    fpr     = fpr_tpr$fpr,
    tpr     = fpr_tpr$tpr,
    tp      = tp,
    fp      = fp,
    fn      = fn,
    tn      = tn
  ))
}

# ─── Parameter helpers (values arrive from the frontend as strings or numbers) ──
fs_param_num <- function(params, key, default) {
  if (is.null(params) || is.null(params[[key]])) return(default)
  v <- suppressWarnings(as.numeric(params[[key]]))
  if (length(v) == 0 || is.na(v)) default else v
}
fs_param_str <- function(params, key, default) {
  if (is.null(params) || is.null(params[[key]])) return(default)
  as.character(params[[key]])
}

# ─── STABL feature selection (upstream gregbellan/Stabl via reticulate) ─────────
# Runs stability selection in classification mode (binary or multiclass).
# Returns list(selected = character, importance = named numeric over feature cols)
# or NULL if the Python `stabl` package / reticulate is unavailable.
stabl_select <- function(train_df, params, ds_id = NULL, n_threads = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cat("[STABL] reticulate not available; cannot run STABL.\n")
    return(NULL)
  }

  Sys.setenv(RETICULATE_MINICONDA_ENABLED = "FALSE")
  options(reticulate.prompt = FALSE)

  # ── Python environment resolution (priority order) ──────────────────────────
  # 1. STABL_PYTHON env var  = explicit full path to a python executable
  # 2. RETICULATE_PYTHON env var = reticulate configured python path
  # 3. RETICULATE_MINICONDA_PATH env var = miniconda path (resolve python within)
  # 4. CONDA_PREFIX env var  = active conda env when launched
  # 5. Conda env hosting R binary itself (dirname(dirname(R.home())))
  # 6. STABL_CONDA_ENV env var = conda env name
  # 7. Fallback candidate paths (Electron bundled miniconda, conda env: easyomifun)
  stabl_py  <- Sys.getenv("STABL_PYTHON", "")
  stabl_env <- Sys.getenv("STABL_CONDA_ENV", "")
  ret_py    <- Sys.getenv("RETICULATE_PYTHON", "")
  env_conda <- Sys.getenv("RETICULATE_MINICONDA_PATH", "")

  tryCatch({
    resolved_py <- ""
    
    # 1. STABL_PYTHON
    if (nzchar(stabl_py)) {
      norm_p <- tryCatch(normalizePath(stabl_py, winslash = "/", mustWork = FALSE), error = function(e) stabl_py)
      if (file.exists(norm_p)) {
        resolved_py <- norm_p
      }
    }
    
    # 2. RETICULATE_PYTHON
    if (!nzchar(resolved_py) && nzchar(ret_py)) {
      norm_p <- tryCatch(normalizePath(ret_py, winslash = "/", mustWork = FALSE), error = function(e) ret_py)
      if (file.exists(norm_p)) {
        resolved_py <- norm_p
      }
    }
    
    # 3. RETICULATE_MINICONDA_PATH
    if (!nzchar(resolved_py) && nzchar(env_conda)) {
      norm_c <- tryCatch(normalizePath(env_conda, winslash = "/", mustWork = FALSE), error = function(e) env_conda)
      py_name <- if (.Platform$OS.type == "windows") "python.exe" else file.path("bin", "python")
      cand_py <- file.path(norm_c, py_name)
      if (file.exists(cand_py)) {
        resolved_py <- cand_py
      }
    }
    
    # 4. Active running Conda environment (CONDA_PREFIX)
    if (!nzchar(resolved_py)) {
      conda_prefix <- Sys.getenv("CONDA_PREFIX", "")
      if (nzchar(conda_prefix)) {
        py_name <- if (.Platform$OS.type == "windows") "python.exe" else file.path("bin", "python")
        cand_py <- file.path(conda_prefix, py_name)
        if (file.exists(cand_py)) {
          cat(sprintf("[STABL] Auto-detected running conda env Python (CONDA_PREFIX): %s\n", cand_py))
          resolved_py <- cand_py
        }
      }
    }
    
    # 5. Conda env hosting R binary itself (R.home prefix)
    if (!nzchar(resolved_py)) {
      r_home_prefix <- dirname(dirname(R.home()))
      py_name <- if (.Platform$OS.type == "windows") "python.exe" else file.path("bin", "python")
      cand_py <- file.path(r_home_prefix, py_name)
      if (file.exists(cand_py)) {
        cat(sprintf("[STABL] Auto-detected Conda env hosting R (R.home): %s\n", cand_py))
        resolved_py <- cand_py
      }
    }

    # 6. STABL_CONDA_ENV named environment
    if (!nzchar(resolved_py) && nzchar(stabl_env)) {
      cat(sprintf("[STABL] Using STABL_CONDA_ENV: %s\n", stabl_env))
      reticulate::use_condaenv(stabl_env, required = TRUE)
    }

    # 7. Fallback candidate search (Electron bundled miniconda, conda env: easyomifun)
    if (!nzchar(resolved_py) && !nzchar(stabl_env)) {
      appdata <- Sys.getenv("APPDATA", "")
      home <- Sys.getenv("HOME", "")
      candidates <- c()
      if (.Platform$OS.type == "windows" && nzchar(appdata)) {
        candidates <- c(
          file.path(appdata, "easyomifun", "miniconda", "python.exe"),
          file.path(appdata, "EasyOmiFun", "miniconda", "python.exe"),
          file.path(getwd(), "miniconda", "python.exe"),
          "C:/miniconda3/envs/easyomifun/python.exe"
        )
      } else if (nzchar(home)) {
        candidates <- c(
          file.path(getwd(), "miniconda", "bin", "python"),
          file.path(getwd(), "miniconda", "bin", "python3"),
          file.path(home, "Library", "EasyOmiFun", "miniconda", "bin", "python"),
          file.path(home, "Library", "EasyOmiFun", "miniconda", "bin", "python3"),
          file.path(home, "Library", "easyomifun", "miniconda", "bin", "python"),
          file.path(home, "Library", "easyomifun", "miniconda", "bin", "python3"),
          file.path(home, "Library", "Application Support", "easyomifun", "miniconda", "bin", "python"),
          file.path(home, "Library", "Application Support", "easyomifun", "miniconda", "bin", "python3"),
          file.path(home, "Library", "Application Support", "EasyOmiFun", "miniconda", "bin", "python"),
          file.path(home, "Library", "Application Support", "EasyOmiFun", "miniconda", "bin", "python3"),
          file.path(home, ".config", "easyomifun", "miniconda", "bin", "python"),
          file.path(home, ".config", "EasyOmiFun", "miniconda", "bin", "python"),
          file.path(home, "miniconda3", "envs", "easyomifun", "bin", "python"),
          file.path(home, "miniconda3", "envs", "easyomifun", "bin", "python3"),
          file.path(home, "anaconda3", "envs", "easyomifun", "bin", "python"),
          file.path(home, ".conda", "envs", "easyomifun", "bin", "python")
        )
      }
      for (c in candidates) {
        if (file.exists(c)) {
          resolved_py <- c
          cat(sprintf("[STABL] Falling back to known app/conda env Python: %s\n", resolved_py))
          break
        }
      }
    }

    if (nzchar(resolved_py)) {
      cat(sprintf("[STABL] Using resolved Python: %s\n", resolved_py))
      reticulate::use_python(resolved_py, required = TRUE)
      Sys.setenv(RETICULATE_PYTHON = resolved_py)
      Sys.setenv(STABL_PYTHON = resolved_py)
    } else if (!nzchar(stabl_env)) {
      cat("[STABL] No Python env resolved. Set STABL_PYTHON or run the app inside the correct conda env.\n")
    }
  }, error = function(e) {
    cat("[STABL] Python env setup error:", conditionMessage(e), "\n")
  })

  # Fail fast if Python or the `stabl` package is missing.
  stabl_ok <- tryCatch(
    isTRUE(reticulate::py_module_available("stabl")),
    error = function(e) { cat("[STABL] Python init failed:", conditionMessage(e), "\n"); FALSE }
  )
  if (!stabl_ok) {
    cat("[STABL] `stabl` python package not found in the configured env.\n")
    cat(sprintf("[STABL] Configured Python: %s\n", tryCatch(reticulate::py_config()$python, error = function(e) "unknown")))
    cat("        Fix: activate that conda env before launching the Shiny app, or set STABL_PYTHON.\n")
    return(NULL)
  }

  base_name    <- fs_param_str(params, "base_estimator", "Logistic L1")
  n_bootstraps <- as.integer(fs_param_num(params, "n_bootstraps", 300))
  art_type     <- fs_param_str(params, "artificial_type", "random_permutation")
  art_prop     <- fs_param_num(params, "artificial_proportion", 1.0)
  fdr_thr      <- fs_param_num(params, "fdr_threshold", 0.1)
  hard_thr     <- fs_param_num(params, "hard_threshold", 0.5)
  alpha_val    <- fs_param_num(params, "alpha", 1.0)
  max_iter_val <- as.integer(fs_param_num(params, "max_iter", 100000))
  # Parallelism: Use allocated effective cores without oversubscription
  eff_cores    <- if (!is.null(n_threads)) max(1L, as.integer(n_threads)) else get_effective_cores()
  stabl_njobs  <- as.integer(Sys.getenv("STABL_NJOBS", as.character(eff_cores)))
  if (is.na(stabl_njobs) || stabl_njobs < 1 || stabl_njobs > eff_cores) stabl_njobs <- eff_cores
  stabl_njobs  <- min(stabl_njobs, eff_cores)

  feature_cols <- setdiff(colnames(train_df), "Group")
  X <- as.matrix(train_df[, feature_cols, drop = FALSE])
  storage.mode(X) <- "double"
  y <- as.integer(train_df$Group) - 1L
  n_classes <- length(levels(train_df$Group))

  # Diagnostics — make a slow/stuck run visible in the backend console.
  cls <- table(train_df$Group)
  cat(sprintf("[STABL] fitting: %d samples x %d features | base=%s n_bootstraps=%d artificial=%s n_jobs=%d | classes: %s\n",
              nrow(train_df), length(feature_cols), base_name, n_bootstraps, art_type, stabl_njobs,
              paste(sprintf("%s=%d", names(cls), as.integer(cls)), collapse = ", ")))
  if (length(feature_cols) > 2000) {
    cat(sprintf("[STABL] note: %d features with single-thread STABL — expect a long run (minutes+).\n",
                length(feature_cols)))
  }
  if (length(cls) > 0 && min(as.integer(cls)) < 5) {
    cat(sprintf("[STABL] warning: smallest class has %d samples — bootstrap resampling may be very slow.\n",
                min(as.integer(cls))))
  }
  t0 <- Sys.time()

  # Construct export paths if ds_id is provided
  stabl_plot_path_pdf <- ""
  stabl_plot_path_png <- ""
  fdr_plot_path_pdf   <- ""
  fdr_plot_path_png   <- ""

  if (!is.null(ds_id) && nzchar(ds_id)) {
    stabl_plot_path_pdf <- get_session_path(ds_id, "stabl_path_%s.pdf")
    stabl_plot_path_png <- get_session_path(ds_id, "stabl_path_%s.png")
    fdr_plot_path_pdf   <- get_session_path(ds_id, "fdr_path_%s.pdf")
    fdr_plot_path_png   <- get_session_path(ds_id, "fdr_path_%s.png")
  }

  res <- tryCatch({
    # Define the Python bridge once (idempotent) then call it.
    reticulate::py_run_string("
def _stabl_run(X, y, feature_names, n_bootstraps, artificial_type, artificial_proportion,
               fdr_threshold, hard_threshold, base_name, n_classes, n_jobs, alpha_val, max_iter_val,
               stabl_path_pdf, stabl_path_png, fdr_path_pdf, fdr_path_png):
    import os
    os.environ['MPLBACKEND'] = 'Agg'
    os.environ['OBJC_DISABLE_INITIALIZE_FORK_SAFETY'] = 'YES'
    try:
        import matplotlib
        matplotlib.use('Agg', force=True)
    except Exception:
        pass
    import numpy as np, pandas as pd
    from stabl.stabl import Stabl
    from sklearn.linear_model import LogisticRegression
    from sklearn.base import BaseEstimator
    if not hasattr(BaseEstimator, '_validate_data'):
        from sklearn.utils.validation import check_array, check_X_y
        def _compat_validate_data(self, X='no_validation', y='no_validation', reset=True,
                                  validate_separately=False, cast_to_ndarray=True, **check_params):
            no_X = isinstance(X, str)
            no_y = isinstance(y, str)
            if not no_X and reset:
                try:
                    self.n_features_in_ = np.asarray(X).shape[1]
                    if hasattr(X, 'columns'):
                        self.feature_names_in_ = np.asarray(X.columns, dtype=object)
                except Exception:
                    pass
            arr_keys = ('accept_sparse', 'dtype', 'order', 'copy', 'ensure_2d',
                        'allow_nd', 'ensure_min_samples', 'ensure_min_features')
            ca = {k: v for k, v in check_params.items() if k in arr_keys}
            if no_X and no_y:
                return None
            if no_y:
                return check_array(X, **ca)
            if no_X:
                return y
            xy_keys = arr_keys + ('multi_output', 'y_numeric')
            return check_X_y(X, y, **{k: v for k, v in check_params.items() if k in xy_keys})
        BaseEstimator._validate_data = _compat_validate_data
    feature_names = [str(f) for f in feature_names]
    Xdf = pd.DataFrame(np.asarray(X, dtype=float), columns=feature_names)
    yv = np.asarray(y).ravel().astype(int)
    n_classes = int(n_classes)
    
    base_name_str = str(base_name).lower()
    if base_name_str in ['elasticnet', 'logistic elastic net', 'logistic elasticnet']:
        base = LogisticRegression(solver='saga', l1_ratio=float(alpha_val),
                                  class_weight='balanced', max_iter=int(max_iter_val), random_state=42)
    else:
        base = LogisticRegression(solver='liblinear', l1_ratio=1.0,
                                  class_weight='balanced', max_iter=int(max_iter_val), random_state=42)
    import joblib
    at = None if str(artificial_type) == 'none' else str(artificial_type)
    kwargs = dict(base_estimator=base, n_bootstraps=int(n_bootstraps), artificial_type=at,
                  artificial_proportion=float(artificial_proportion),
                  random_state=42, n_jobs=int(n_jobs), verbose=0)
    if at is None:
        kwargs['hard_threshold'] = float(hard_threshold)
    else:
        kwargs['fdr_threshold_range'] = np.arange(0.1, 1.0, 0.01)
    s = Stabl(**kwargs)
    try:
        with joblib.parallel_backend('threading', n_jobs=int(n_jobs)):
            s.fit(Xdf, yv)
    except Exception:
        s.fit(Xdf, yv)
    
    # Save stability path and FDR graph plots
    from stabl.stabl import plot_stabl_path, plot_fdr_graph
    
    if stabl_path_pdf:
        try:
            plot_stabl_path(s, show_fig=False, export_file=True, path=str(stabl_path_pdf))
        except Exception as e:
            print('[STABL] Failed to save stability path PDF:', e)
            
    if stabl_path_png:
        try:
            plot_stabl_path(s, show_fig=False, export_file=True, path=str(stabl_path_png))
        except Exception as e:
            print('[STABL] Failed to save stability path PNG:', e)
            
    if at is not None:
        if fdr_path_pdf:
            try:
                plot_fdr_graph(s, show_fig=False, export_file=True, path=str(fdr_path_pdf))
            except Exception as e:
                print('[STABL] Failed to save FDR graph PDF:', e)
        if fdr_path_png:
            try:
                plot_fdr_graph(s, show_fig=False, export_file=True, path=str(fdr_path_png))
            except Exception as e:
                print('[STABL] Failed to save FDR graph PNG:', e)
                
    scores = np.asarray(s.stabl_scores_)
    per_feat = scores.max(axis=1) if scores.ndim == 2 else scores
    support = s.get_support(indices=False)
    return {'scores': [float(v) for v in per_feat],
            'support': [bool(b) for b in support],
            'features': feature_names}
")
    py <- reticulate::py
    out <- py$`_stabl_run`(X, y, feature_cols, n_bootstraps, art_type, art_prop,
                           fdr_thr, hard_thr, base_name, n_classes, stabl_njobs, alpha_val, max_iter_val,
                           stabl_plot_path_pdf, stabl_plot_path_png, fdr_plot_path_pdf, fdr_plot_path_png)
    out
  }, error = function(e) {
    cat("[STABL] run failed (is the `stabl` python package installed?):", conditionMessage(e), "\n")
    NULL
  })

  cat(sprintf("[STABL] fit finished in %.1fs\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (is.null(res)) return(NULL)

  feats <- unlist(res$features)
  scores <- as.numeric(unlist(res$scores))
  support <- as.logical(unlist(res$support))
  names(scores) <- feats

  selected <- feats[support]
  list(selected = selected, importance = scores)
}

# ─── Boruta all-relevant feature selection (native R `Boruta` package) ──────────
# Returns list(selected = character, importance = named numeric over feature cols)
# or NULL if the Boruta package is unavailable.
boruta_select <- function(train_df, params, n_threads = NULL) {
  if (!requireNamespace("Boruta", quietly = TRUE)) {
    cat("[BORUTA] R package 'Boruta' not installed; cannot run Boruta.\n")
    return(NULL)
  }

  max_runs       <- max(11, as.integer(fs_param_num(params, "max_runs", 100)))
  p_value        <- fs_param_num(params, "p_value", 0.01)
  max_depth      <- as.integer(fs_param_num(params, "max_depth", 10))
  max_depth      <- max(1, min(20, max_depth))
  ntree          <- as.integer(fs_param_num(params, "n_estimators", 500))
  keep_tentative <- identical(fs_param_str(params, "keep_tentative", "no"), "yes")

  feature_cols <- setdiff(colnames(train_df), "Group")
  X <- train_df[, feature_cols, drop = FALSE]
  y <- train_df$Group

  eff_cores <- if (!is.null(n_threads)) max(1L, as.integer(n_threads)) else get_effective_cores()
  res <- tryCatch({
    bres <- Boruta::Boruta(x = X, y = y, maxRuns = max_runs, pValue = p_value,
                           getImp = Boruta::getImpRfZ, ntree = ntree, num.threads = eff_cores, doTrace = 0,
                           maxdepth = max_depth, max.depth = max_depth)
    if (keep_tentative) {
      bres <- Boruta::TentativeRoughFix(bres)
    }
    bres
  }, error = function(e) {
    cat("[BORUTA] run failed:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(res)) return(NULL)

  # Median importance per real feature (drop non-finite runs and shadow columns).
  imp_hist <- res$ImpHistory
  shadow_cols <- grepl("^shadow", colnames(imp_hist))
  imp_hist <- imp_hist[, !shadow_cols, drop = FALSE]
  med_imp <- apply(imp_hist, 2, function(col) {
    v <- col[is.finite(col)]
    if (length(v) == 0) 0 else median(v, na.rm = TRUE)
  })
  # Align to feature_cols ordering
  importance <- rep(0, length(feature_cols))
  names(importance) <- feature_cols
  common <- intersect(names(med_imp), feature_cols)
  importance[common] <- pmax(med_imp[common], 0)  # negative Z importances floored to 0

  decisions <- res$finalDecision  # named factor: Confirmed / Tentative / Rejected
  confirmed <- names(decisions)[decisions == "Confirmed"]
  if (keep_tentative) {
    confirmed <- union(confirmed, names(decisions)[decisions == "Tentative"])
  }
  confirmed <- intersect(confirmed, feature_cols)
  # Sort confirmed features based on Boruta's Z importance score descending
  confirmed <- confirmed[order(importance[confirmed], decreasing = TRUE)]

  list(selected = confirmed, importance = importance)
}

# 1. Fit ML model and extract features importance
fit_model_and_importance <- function(model_name, train_df, gene_mapping, params = NULL, ds_id = NULL, n_threads = NULL) {
  importance_vector <- rep(0, ncol(train_df) - 1)
  names(importance_vector) <- setdiff(colnames(train_df), "Group")

  fitted_model <- NULL
  fit_warning  <- NULL   # set when a selector silently falls back (surfaced to the UI)

  # caret trainControl – no internal resampling for speed
  ctrl <- caret::trainControl(
    method = "none",
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )

  # Ensure Group levels are valid R identifiers (caret requirement)
  orig_lvls <- levels(train_df$Group)
  safe_lvls <- make.names(orig_lvls, unique = TRUE)
  levels(train_df$Group) <- safe_lvls

  eff_cores <- if (!is.null(n_threads)) max(1L, as.integer(n_threads)) else get_effective_cores()

  set.seed(42)
  tryCatch({
    if (model_name == "randomforest") {
      mtry_val <- max(1, floor(sqrt(ncol(train_df) - 1)))
      max_depth_val <- get_param(params, "randomforest", "max_depth", 10)
      max_depth_val <- max(1, min(20, max_depth_val))
      num_trees_val <- get_param(params, "randomforest", "num_trees", 500)
      
      ranger_max_depth <- if (max_depth_val > 0) max_depth_val else NULL
      fit <- caret::train(
        Group ~ ., data = train_df,
        method = "ranger",
        trControl = ctrl,
        tuneGrid = data.frame(mtry = mtry_val, splitrule = "gini", min.node.size = 1),
        num.trees = num_trees_val,
        max.depth = ranger_max_depth,
        num.threads = eff_cores,
        importance = "impurity"
      )
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "boruta_refit") {
      mtry_val <- max(1, floor(sqrt(ncol(train_df) - 1)))
      max_depth_val <- get_param(params, "boruta", "max_depth", 10)
      max_depth_val <- max(1, min(20, max_depth_val))
      num_trees_val <- get_param(params, "boruta", "n_estimators", 500)
      
      ranger_max_depth <- if (max_depth_val > 0) max_depth_val else NULL
      fit <- caret::train(
        Group ~ ., data = train_df,
        method = "ranger",
        trControl = ctrl,
        tuneGrid = data.frame(mtry = mtry_val, splitrule = "gini", min.node.size = 1),
        num.trees = num_trees_val,
        max.depth = ranger_max_depth,
        num.threads = eff_cores,
        importance = "impurity"
      )
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "svm") {
      svm_c      <- get_param(params, "svm", "C", 1.0)
      svm_weight <- get_param(params, "svm", "weight", 1.0)
      
      fit <- caret::train(
        Group ~ ., data = train_df,
        method = "svmLinearWeights",
        trControl = ctrl,
        tuneGrid = data.frame(cost = svm_c, weight = svm_weight),
        prob.model = TRUE
      )
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "logistic") {
      lambda_val <- get_param(params, "logistic", "lambda", 0.1)
      lambda_val <- max(0.0001, lambda_val)
      alpha_val  <- get_param(params, "logistic", "alpha", 1.0)
      alpha_val  <- max(0, min(1, alpha_val))
      max_it     <- get_param(params, "logistic", "max_iter", 1000)
      
      n_classes <- length(levels(train_df$Group))
      fam_val   <- if (n_classes > 2) "multinomial" else "binomial"
      
      fit <- tryCatch({
        caret::train(
          Group ~ ., data = train_df,
          method = "glmnet",
          family = fam_val,
          trControl = ctrl,
          tuneGrid = data.frame(alpha = alpha_val, lambda = lambda_val),
          maxit = as.integer(max_it)
        )
      }, error = function(e) {
        cat("[WARNING] Logistic regression (glmnet) failed, falling back to standard glm:", e$message, "\n")
        caret::train(
          Group ~ ., data = train_df,
          method = "glm",
          family = fam_val,
          trControl = ctrl
        )
      })
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "gbm") {
      n_trees           <- as.integer(get_param(params, "gbm", "n.trees", 500))
      interaction_depth <- as.integer(get_param(params, "gbm", "interaction.depth", 3))
      shrinkage_val     <- as.numeric(get_param(params, "gbm", "shrinkage", 0.1))
      min_obs           <- as.integer(get_param(params, "gbm", "n.minobsinnode", 10))

      # Defensive adjustments for gbm on small datasets
      n_train_est <- nrow(train_df)
      bag_frac <- 0.5
      if (n_train_est * 0.5 <= 2 * min_obs + 1) {
        bag_frac <- 1.0
      }
      if (n_train_est * bag_frac <= 2 * min_obs + 1) {
        min_obs <- max(2L, floor((n_train_est * bag_frac - 1) / 2))
      }

      fit <- caret::train(
        Group ~ ., data = train_df,
        method = "gbm",
        trControl = ctrl,
        tuneGrid = data.frame(
          n.trees = n_trees,
          interaction.depth = interaction_depth,
          shrinkage = shrinkage_val,
          n.minobsinnode = min_obs
        ),
        bag.fraction = bag_frac,
        verbose = FALSE
      )
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "stabl_refit") {
      mtry_val <- max(1, floor(sqrt(ncol(train_df) - 1)))
      max_depth_val <- get_param(params, "stabl", "refit_max_depth", get_param(params, "stabl", "max_depth", 10))
      max_depth_val <- max(1, min(20, max_depth_val))
      num_trees_val <- get_param(params, "stabl", "refit_n_estimators", get_param(params, "stabl", "refit_num_trees", get_param(params, "stabl", "n_estimators", 500)))
      
      ranger_max_depth <- if (max_depth_val > 0) max_depth_val else NULL
      fit <- caret::train(
        Group ~ ., data = train_df,
        method = "ranger",
        trControl = ctrl,
        tuneGrid = data.frame(mtry = mtry_val, splitrule = "gini", min.node.size = 1),
        num.trees = num_trees_val,
        max.depth = ranger_max_depth,
        num.threads = eff_cores,
        importance = "impurity"
      )
      imp_raw <- caret::varImp(fit, scale = FALSE)$importance
      importance_vector <- setNames(
        as.numeric(rowMeans(imp_raw)),
        rownames(imp_raw)
      )
      fitted_model <- fit

    } else if (model_name == "stabl" || model_name == "boruta") {
      if (model_name == "boruta") {
        # 1. Run Boruta feature selection to find all-relevant features
        sel <- boruta_select(train_df, params[["boruta"]], n_threads = eff_cores)
        selected_feats <- NULL
        if (!is.null(sel) && !is.null(sel$selected) && length(sel$selected) > 0) {
          selected_feats <- sel$selected
        } else if (requireNamespace("Boruta", quietly = TRUE)) {
          max_runs_val <- max(11, get_param(params, "boruta", "max_runs", 100))
          pval_val <- get_param(params, "boruta", "p_value", 0.01)
          max_depth_val <- get_param(params, "boruta", "max_depth", 10)
          max_depth_val <- max(1, min(20, max_depth_val))
          fit_boruta <- tryCatch(Boruta::Boruta(Group ~ ., data = train_df, doTrace = 0, maxRuns = max_runs_val, pValue = pval_val, maxdepth = max_depth_val, max.depth = max_depth_val, num.threads = eff_cores), error = function(e) NULL)
          if (!is.null(fit_boruta)) {
            selected_feats <- Boruta::getSelectedAttributes(fit_boruta, withTentative = FALSE)
            imp <- Boruta::attStats(fit_boruta)
            sel_imp <- setNames(pmax(imp$meanImp, 0), rownames(imp))
            sel <- list(selected = selected_feats, importance = sel_imp)
          }
        }

        feat_all <- setdiff(colnames(train_df), "Group")
        if (is.null(sel)) {
          if (!requireNamespace("Boruta", quietly = TRUE)) {
            fit_warning <- "Boruta could not run (R 'Boruta' package unavailable) — the result shown is a RandomForest fallback on all features, not all-relevant selection."
          } else {
            fit_warning <- "Boruta run failed — the result shown is a RandomForest fallback on all features, not all-relevant selection."
          }
        }
        if (is.null(selected_feats) || length(selected_feats) == 0) {
          selected_feats <- feat_all
        }
        selected_feats <- intersect(make.names(selected_feats), feat_all)
        if (length(selected_feats) == 0) selected_feats <- feat_all

        # 2. Retrain a Random Forest with those Boruta-selected features to get the final model using ranger
        boruta_train_df <- train_df[, c(selected_feats, "Group"), drop = FALSE]
        boruta_train_df$Group <- factor(boruta_train_df$Group)
        
        mtry_val <- max(1, floor(sqrt(ncol(boruta_train_df) - 1)))
        max_depth_val <- get_param(params, "boruta", "max_depth", 10)
        max_depth_val <- max(1, min(20, max_depth_val))
        num_trees_val <- get_param(params, "boruta", "n_estimators", 500)
        ranger_max_depth <- if (max_depth_val > 0) max_depth_val else NULL
        
        fit <- caret::train(
          Group ~ ., data = boruta_train_df,
          method = "ranger",
          trControl = ctrl,
          tuneGrid = data.frame(mtry = mtry_val, splitrule = "gini", min.node.size = 1),
          num.trees = num_trees_val,
          max.depth = ranger_max_depth,
          num.threads = eff_cores,
          importance = "impurity"
        )

        iv <- rep(0, length(feat_all))
        names(iv) <- feat_all
        if (!is.null(sel) && !is.null(sel$importance)) {
          common <- intersect(names(sel$importance), feat_all)
          iv[common] <- sel$importance[common]
        }
        

        
        iv[is.na(iv) | is.nan(iv)] <- 0
        
        # Keep ONLY Boruta-selected features in the importance vector
        iv[setdiff(names(iv), selected_feats)] <- 0
        
        for (nm in selected_feats) {
          if (nm %in% names(iv)) {
            val <- iv[nm]
            if (is.na(val) || is.nan(val) || val == 0) {
              iv[nm] <- 1.0
            }
          }
        }

        importance_vector <- iv
        fitted_model <- fit
      } else {
        # STABL
        sel <- stabl_select(train_df, params[["stabl"]], ds_id = ds_id, n_threads = eff_cores)
        iv <- rep(0, ncol(train_df) - 1)
        names(iv) <- setdiff(colnames(train_df), "Group")
        if (!is.null(sel)) {
          selected_genes <- sel$selected
          if (length(selected_genes) > 0) {
            # Selected features get 1.0 importance since there is no continuous importance score
            iv[selected_genes] <- 1.0
          }
          importance_vector <- iv
          
          if (length(selected_genes) == 0) {
            cat("[STABL] Warning: STABL selected 0 features. No fallback.\n")
            fitted_model <- NULL
            fit_warning <- "STABL selected 0 features. No fallback."
          } else {
            sel_train_df <- train_df[, c(selected_genes, "Group"), drop = FALSE]
            fit_res <- fit_model_and_importance("stabl_refit", sel_train_df, gene_mapping, params, n_threads = eff_cores)
            fitted_model <- fit_res$model
          }
        } else {
          stop("STABL Python package is not available or failed to run. Check the log file for details.")
        }
      }
    }
  }, error = function(e) {
    cat("[ERROR] fit_model_and_importance for", model_name, ":", e$message, "\n")
    stop(e)
  })

  # Align importance_vector to expected feature names defensively
  feat_names <- setdiff(colnames(train_df), "Group")
  aligned_imp <- rep(0, length(feat_names))
  names(aligned_imp) <- feat_names
  for (nm in names(importance_vector)) {
    if (nm %in% feat_names) aligned_imp[nm] <- importance_vector[nm]
  }
  importance_vector <- aligned_imp

  # Normalize importance values so they sum to 1.0 (proportion normalization)
  sum_imp <- sum(importance_vector, na.rm = TRUE)
  if (sum_imp > 0) importance_vector <- importance_vector / sum_imp
  importance_vector[is.nan(importance_vector) | is.na(importance_vector)] <- 0

  # Optimized O(1) make.names -> original gene identifier lookup
  name_lookup <- as.character(gene_mapping)
  if (is.null(names(gene_mapping)) || any(!nzchar(names(gene_mapping)))) {
    names(name_lookup) <- make.names(as.character(gene_mapping))
  } else {
    names(name_lookup) <- as.character(names(gene_mapping))
  }

  clean_names <- names(importance_vector)
  feature_list <- lapply(seq_along(importance_vector), function(i) {
    clean_name <- clean_names[i]
    orig_name <- name_lookup[[clean_name]]
    if (is.null(orig_name) || length(orig_name) == 0) orig_name <- clean_name

    list(
      gene = as.character(orig_name),
      importance = as.numeric(importance_vector[i]),
      rank = i
    )
  })

  if (!is.null(fitted_model)) {
    attr(fitted_model, "orig_levels") <- orig_lvls
    attr(fitted_model, "safe_levels") <- safe_lvls
  }

  return(list(model = fitted_model, features = feature_list, warning = fit_warning))
}

# ---------------------------------------------------------------
# Predict probabilities and classes using caret model
# ---------------------------------------------------------------
predict_ml <- function(model_name, fit, test_df) {
  orig_lvls <- attr(fit, "orig_levels") %||% levels(test_df$Group)
  safe_lvls <- attr(fit, "safe_levels") %||% orig_lvls

  predictions <- factor(rep(orig_lvls[1], nrow(test_df)), levels = orig_lvls)
  probabilities <- rep(0.5, nrow(test_df))

  if (is.null(fit)) return(list(predictions = predictions, probabilities = probabilities))

  tryCatch({
    if (inherits(fit, "train")) {
      feat_cols <- setdiff(colnames(test_df), "Group")
      test_x <- test_df[, feat_cols, drop = FALSE]

      # Align to model's expected predictors
      model_preds <- tryCatch(fit$finalModel$xNames, error = function(e) NULL)
      if (is.null(model_preds)) {
        model_preds <- tryCatch(colnames(fit$trainingData)[colnames(fit$trainingData) != ".outcome"], error = function(e) NULL)
      }
      if (!is.null(model_preds)) {
        missing_cols <- setdiff(model_preds, colnames(test_x))
        for (mc in missing_cols) test_x[[mc]] <- 0
        test_x <- test_x[, model_preds, drop = FALSE]
      }

      pred_class <- tryCatch(
        predict(fit, newdata = test_x, type = "raw"),
        error = function(e) { cat("[ERROR] predict raw:", e$message, "\n"); NULL }
      )

      pred_prob <- tryCatch(
        predict(fit, newdata = test_x, type = "prob"),
        error = function(e) NULL
      )

      if (!is.null(pred_class)) {
        pred_c_str <- as.character(pred_class)
        pred_c_mapped <- orig_lvls[match(pred_c_str, safe_lvls)]
        pred_c_mapped[is.na(pred_c_mapped)] <- pred_c_str[is.na(pred_c_mapped)]
        predictions <- factor(pred_c_mapped, levels = orig_lvls)
      }

      if (!is.null(pred_prob) && is.data.frame(pred_prob)) {
        prob_cols <- colnames(pred_prob)
        mapped_cols <- orig_lvls[match(prob_cols, safe_lvls)]
        mapped_cols[is.na(mapped_cols)] <- prob_cols[is.na(mapped_cols)]
        colnames(pred_prob) <- mapped_cols

        pos_class <- orig_lvls[2]
        if (pos_class %in% colnames(pred_prob)) {
          probabilities <- pred_prob[[pos_class]]
        } else {
          probabilities <- pred_prob[[ncol(pred_prob)]]
        }
      }
    } else if (inherits(fit, "randomForest")) {
      feat_cols <- setdiff(colnames(test_df), "Group")
      test_x <- test_df[, feat_cols, drop = FALSE]
      rf_feats <- rownames(fit$importance)
      if (!is.null(rf_feats)) {
        missing_cols <- setdiff(rf_feats, colnames(test_x))
        for (mc in missing_cols) test_x[[mc]] <- 0
        test_x <- test_x[, rf_feats, drop = FALSE]
      }

      pred_class <- tryCatch(predict(fit, newdata = test_x, type = "response"), error = function(e) NULL)
      pred_prob <- tryCatch(predict(fit, newdata = test_x, type = "prob"), error = function(e) NULL)

      if (!is.null(pred_class)) {
        pred_c_str <- as.character(pred_class)
        pred_c_mapped <- orig_lvls[match(pred_c_str, safe_lvls)]
        pred_c_mapped[is.na(pred_c_mapped)] <- pred_c_str[is.na(pred_c_mapped)]
        predictions <- factor(pred_c_mapped, levels = orig_lvls)
      }

      if (!is.null(pred_prob) && (is.matrix(pred_prob) || is.data.frame(pred_prob))) {
        pred_prob_df <- as.data.frame(pred_prob)
        prob_cols <- colnames(pred_prob_df)
        mapped_cols <- orig_lvls[match(prob_cols, safe_lvls)]
        mapped_cols[is.na(mapped_cols)] <- prob_cols[is.na(mapped_cols)]
        colnames(pred_prob_df) <- mapped_cols

        pos_class <- orig_lvls[2]
        if (pos_class %in% colnames(pred_prob_df)) {
          probabilities <- pred_prob_df[[pos_class]]
        } else {
          probabilities <- pred_prob_df[[ncol(pred_prob_df)]]
        }
      }
    }
  }, error = function(e) {
    cat("[ERROR] Prediction failed for model", model_name, ":", e$message, "\n")
  })

  return(list(predictions = predictions, probabilities = as.numeric(probabilities)))
}

# Helper to resolve dataset purposes, handle same-type batch correction pooling & validation strategies
resolve_and_preprocess_datasets <- function(datasets, split_ratio, multi_dataset_mode = "combine") {
  train_dss <- list()
  test_dss <- list()
  
  for (d in datasets) {
    base_id <- get_base_id(d$id)
    fs_meta_path <- get_session_path(base_id, "%s_fs_meta.rds")
    
    saved_fs_meta <- if (file.exists(fs_meta_path)) tryCatch(readRDS(fs_meta_path), error = function(e) NULL) else NULL
    
    raw_purpose <- d$datasetPurpose %||% d$fs_datasetPurpose %||% saved_fs_meta$datasetPurpose
    if (is.null(raw_purpose) || raw_purpose == "" || raw_purpose == "split") {
      purpose <- "train-and-test"
    } else {
      purpose <- raw_purpose
    }
    
    val_strat <- d$validationStrategy %||% d$fs_validationStrategy %||% saved_fs_meta$validationStrategy %||% "train-test-split"
    
    if (val_strat == "cv-only") {
      ratio <- 1.0
      is_internal_val <- FALSE
    } else {
      ratio <- d$trainRatio %||% d$fs_trainRatio %||% saved_fs_meta$trainRatio %||% split_ratio %||% 0.7
      if (is.null(ratio) || is.na(as.numeric(ratio))) ratio <- 0.7
      is_internal_val <- if (!is.null(d$fs_isInternalValidation)) isTRUE(d$fs_isInternalValidation) else if (!is.null(d$isInternalValidation)) isTRUE(d$isInternalValidation) else if (!is.null(saved_fs_meta$isInternalValidation)) isTRUE(saved_fs_meta$isInternalValidation) else (purpose == "train-and-test")
    }
    
    d$fs_trainRatio <- as.numeric(ratio)
    d$trainRatio <- as.numeric(ratio)
    d$fs_datasetPurpose <- purpose
    d$datasetPurpose <- purpose
    d$fs_validationStrategy <- val_strat
    d$validationStrategy <- val_strat
    d$fs_isInternalValidation <- is_internal_val
    d$isInternalValidation <- is_internal_val
    
    enabled_prev <- d$enabledTargetPrevalence %||% d$fs_enabledTargetPrevalence %||% saved_fs_meta$enabledTargetPrevalence %||% FALSE
    prev_val <- d$targetPrevalence %||% d$fs_targetPrevalence %||% saved_fs_meta$targetPrevalence %||% NULL

    d$fs_enabledTargetPrevalence <- isTRUE(enabled_prev)
    d$enabledTargetPrevalence <- isTRUE(enabled_prev)
    d$fs_targetPrevalence <- if (isTRUE(enabled_prev) && !is.null(prev_val)) as.numeric(prev_val) else NULL
    d$targetPrevalence <- if (isTRUE(enabled_prev) && !is.null(prev_val)) as.numeric(prev_val) else NULL
    
    # Save/update fs_meta.rds on server disk
    tryCatch({
      saveRDS(list(
        datasetPurpose = purpose,
        validationStrategy = val_strat,
        trainRatio = as.numeric(ratio),
        isInternalValidation = is_internal_val,
        enabledTargetPrevalence = isTRUE(enabled_prev),
        targetPrevalence = if (isTRUE(enabled_prev) && !is.null(prev_val)) as.numeric(prev_val) else NULL
      ), fs_meta_path)
    }, error = function(e) {
      cat(sprintf("[WARNING] Could not save fs_meta.rds for %s: %s\n", base_id, e$message))
    })
    
    if (purpose == "test") {
      test_dss[[d$id]] <- d
    } else {
      train_dss[[d$id]] <- d
    }
  }
  
  # Fallback if no training dataset
  if (length(train_dss) == 0 && length(datasets) > 0) {
    d <- datasets[[1]]
    d$fs_datasetPurpose <- "train"
    d$datasetPurpose <- "train"
    train_dss[[d$id]] <- d
    test_dss[[d$id]] <- NULL
  }
  
  # Multi-dataset pooling with ComBat batch correction:
  # ONLY apply when multi_dataset_mode == "combine" AND for the EXACT same data type (readcounts+readcounts, microarray+microarray, proteomics+proteomics).
  # NEVER pool 'others' type. NEVER cross-type pool (readcounts+microarray are distinct types).
  if (identical(multi_dataset_mode, "combine") && length(train_dss) > 1) {
    cat(sprintf("[ML] Combine mode requested for %d training datasets. Checking for same-type pooling pools...\n", length(train_dss)))
    
    # Group training datasets by exact data type
    type_groups <- list()
    for (d_id in names(train_dss)) {
      d <- train_dss[[d_id]]
      dt <- get_dataset_data_type(d)
      if (is.null(type_groups[[dt]])) type_groups[[dt]] <- list()
      type_groups[[dt]][[d_id]] <- d
    }
    
    final_train_dss <- list()
    
    for (dt in names(type_groups)) {
      group_dss <- type_groups[[dt]]
      # Rule: 'others' is NEVER pooled. Only pool same-type if count >= 2.
      if (dt != "others" && length(group_dss) > 1) {
        cat(sprintf("[ML] Pooling %d datasets of exact type '%s' with ComBat batch correction...\n", length(group_dss), dt))
        
        expr_list <- list()
        clin_list <- list()
        common_genes <- NULL
        
        for (d_id in names(group_dss)) {
          d <- group_dss[[d_id]]
          is_standalone_fs <- get_ds_is_standalone_fs(d$id)
          parsed_expr <- get_backend_dataset(d$id, original = FALSE, step = "fs")
          if (is.null(parsed_expr) || is.null(parsed_expr$expr)) {
            parsed_expr <- get_backend_dataset(d$id, original = TRUE)
          }
          if (is.null(parsed_expr)) next
          
          expr <- parsed_expr$expr
          clin_path <- get_clinical_path(d$id)
          clin_meta_path <- get_clin_metadata_path(d$id)
          clin_df <- NULL
          if (file.exists(clin_path) && file.exists(clin_meta_path)) {
            raw_clin <- read_csv_preserve_id(clin_path)
            meta <- readRDS(clin_meta_path)
            clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), meta$sampleIdCol)
          }
          
          group_col <- d$clinicalGroupCol
          if (is.null(group_col) || group_col == "" || !(group_col %in% colnames(clin_df))) {
            if (file.exists(clin_meta_path)) {
              meta <- readRDS(clin_meta_path)
              group_col <- meta$groupCol
            }
          }
          
          if (is.null(clin_df) || is.null(group_col) || group_col == "" || !(group_col %in% colnames(clin_df))) next
          
          valid_samples <- rownames(clin_df)[!is.na(clin_df[[group_col]]) & clin_df[[group_col]] != ""]
          clin_df <- clin_df[valid_samples, , drop = FALSE]
          
          samples_in_cols <- intersect(colnames(expr), rownames(clin_df))
          samples_in_rows <- intersect(rownames(expr), rownames(clin_df))
          
          if (length(samples_in_cols) > 0) {
            expr <- expr[, samples_in_cols, drop = FALSE]
            clin_df <- clin_df[samples_in_cols, , drop = FALSE]
          } else if (length(samples_in_rows) > 0) {
            expr <- t(expr[samples_in_rows, , drop = FALSE])
            clin_df <- clin_df[samples_in_rows, , drop = FALSE]
          } else {
            next
          }
          
          gene_ids <- rownames(expr)
          if (is.null(common_genes)) {
            common_genes <- gene_ids
          } else {
            common_genes <- intersect(common_genes, gene_ids)
          }
          
          expr_list[[d_id]] <- expr
          clin_list[[d_id]] <- list(df = clin_df, group_col = group_col)
        }
        
        if (length(expr_list) > 1 && length(common_genes) > 0) {
          merged_expr_list <- list()
          merged_clin_list <- list()
          batch_vec_list <- list()
          
          for (d_id in names(expr_list)) {
            expr_sub <- expr_list[[d_id]][common_genes, , drop = FALSE]
            colnames(expr_sub) <- paste0(d_id, "_", colnames(expr_sub))
            merged_expr_list[[d_id]] <- expr_sub
            
            clin_sub <- clin_list[[d_id]]$df
            group_col <- clin_list[[d_id]]$group_col
            
            sample_ids <- paste0(d_id, "_", rownames(clin_sub))
            df_c <- data.frame(
              SampleID = sample_ids,
              Group = clin_sub[[group_col]],
              Batch = d_id,
              stringsAsFactors = FALSE
            )
            if (length(sample_ids) > 0) {
              rownames(df_c) <- sample_ids
            }
            merged_clin_list[[d_id]] <- df_c
            batch_vec_list[[d_id]] <- rep(d_id, ncol(expr_sub))
          }
          # The full per-dataset matrices are no longer needed once the common-gene subsets
          # (merged_expr_list) are built; free them before the memory-heavy cbind + ComBat.
          rm(expr_list); invisible(gc(FALSE))

          merged_expr <- do.call(cbind, merged_expr_list)
          merged_clin <- do.call(rbind, merged_clin_list)
          merged_batch <- unlist(batch_vec_list)
          rm(merged_expr_list); invisible(gc(FALSE))
          
          corrected_expr <- tryCatch({
            if (requireNamespace("sva", quietly = TRUE)) {
              sva::ComBat(dat = merged_expr, batch = merged_batch)
            } else if (requireNamespace("limma", quietly = TRUE)) {
              limma::removeBatchEffect(merged_expr, batch = merged_batch)
            } else {
              merged_expr
            }
          }, error = function(e) {
            cat("[WARNING] Batch correction of merged datasets failed:", e$message, "\n")
            merged_expr
          })
          
          first_ds_id <- names(group_dss)[1]
          user_id <- get_user_id(first_ds_id)
          parsed_first <- get_backend_datasets(first_ds_id)
          parent_mod_val <- parsed_first$parentModule %||% "fs"
          is_inline_val  <- isTRUE(parsed_first$isInline)

          merged_id <- if (!is.null(user_id) && nzchar(user_id)) sprintf("%s_merged_%s_training_fs", user_id, dt) else sprintf("merged_%s_training_fs", dt)
          
          merged_expr_meta_path <- get_session_path(merged_id, "%s_expr_metadata.rds")
          merged_expr_mat_path  <- get_session_path(merged_id, "%s_expr_matrix.rds")
          merged_clin_meta_path <- get_session_path(merged_id, "%s_clin_metadata.rds")
          pooled_expr_path      <- get_session_path(merged_id, "%s_expression.csv")
          pooled_clin_path      <- get_session_path(merged_id, "%s_clinical.csv")
          
          dir.create(dirname(merged_expr_meta_path), showWarnings = FALSE, recursive = TRUE)
          saveRDS(list(
            featureOrientation   = "column",
            featureIndexValue    = 0,
            dataType             = dt,
            module               = "fs",
            parentModule         = parent_mod_val,
            isInline             = is_inline_val
          ), merged_expr_meta_path)
          saveRDS(corrected_expr, merged_expr_mat_path)
          
          gene_ids <- rownames(corrected_expr)
          if (is.null(gene_ids)) gene_ids <- paste0("Feature_", seq_len(nrow(corrected_expr)))
          df_csv <- data.frame(GeneID = gene_ids, as.data.frame(corrected_expr), check.names = FALSE, stringsAsFactors = FALSE)
          write.csv(df_csv, pooled_expr_path, row.names = FALSE)
          register_export_file(user_id, "pooled_expression_matrix", merged_id, pooled_expr_path, "fs", "training", parentModule = parent_mod_val, isInline = is_inline_val)
          
          write_clin <- merged_clin[, c("SampleID", "Group"), drop = FALSE]
          write.csv(write_clin, pooled_clin_path, row.names = FALSE)
          register_export_file(user_id, "pooled_clinical_data", merged_id, pooled_clin_path, "fs", "training", parentModule = parent_mod_val, isInline = is_inline_val)
          first_d_obj <- group_dss[[1]]
          pooled_pos <- first_d_obj$positiveClass %||% first_d_obj$fs_positiveClass
          pooled_neg <- first_d_obj$negativeClass %||% first_d_obj$fs_negativeClass
          if (is.null(pooled_pos) || !nzchar(pooled_pos)) {
            meta_first <- tryCatch(readRDS(get_clin_metadata_path(first_ds_id)), error = function(e) NULL)
            if (!is.null(meta_first)) {
              pooled_pos <- meta_first$positiveClass %||% meta_first$fs_positiveClass
              pooled_neg <- meta_first$negativeClass %||% meta_first$fs_negativeClass
            }
          }

          saveRDS(list(
            sampleIdCol   = "SampleID",
            groupCol      = "Group",
            positiveClass = pooled_pos,
            negativeClass = pooled_neg,
            module        = "fs",
            parentModule  = parent_mod_val,
            isInline      = is_inline_val
          ), merged_clin_meta_path)
          
          merged_dataset <- list(
            id = merged_id,
            name = sprintf("Pooled %s Training Datasets", toupper(dt)),
            dataType = dt,
            clinicalGroupCol = "Group",
            clinicalSampleIdCol = "SampleID",
            positiveClass = pooled_pos,
            negativeClass = pooled_neg,
            fs_positiveClass = pooled_pos,
            fs_negativeClass = pooled_neg,
            fs_datasetPurpose = "train",
            datasetPurpose = "train",
            fs_trainRatio = split_ratio,
            fs_isInternalValidation = any(sapply(group_dss, function(x) isTRUE(x$fs_isInternalValidation)))
          )
          
          final_train_dss[[merged_id]] <- merged_dataset
        } else {
          for (d_id in names(group_dss)) {
            final_train_dss[[d_id]] <- group_dss[[d_id]]
          }
        }
      } else {
        # Keep individual datasets if count == 1 or if type is 'others'
        for (d_id in names(group_dss)) {
          final_train_dss[[d_id]] <- group_dss[[d_id]]
        }
      }
    }
    train_dss <- final_train_dss
  }
  
  return(list(train = train_dss, test = test_dss))
}

# ---------------------------------------------------------------
# 2. Main Feature Selection endpoint
# ---------------------------------------------------------------
# Compute the shared (intersection) feature set across all datasets in-memory.
# Returns NULL when there is only one dataset (no filtering needed).
# Does NOT write to any disk file.
compute_shared_features <- function(datasets) {
  if (length(datasets) <= 1) return(NULL)
  
  cat("[ML] Multiple datasets detected. Computing shared feature set in-memory with orientation awareness...\n")
  shared_features <- get_shared_features_from_datasets(datasets)
  
  if (is.null(shared_features) || length(shared_features) == 0) {
    stop("No shared features found across the selected datasets. Cannot proceed with feature selection.")
  }
  cat(sprintf("[ML] Shared features intersection: %d features across %d datasets.\n", length(shared_features), length(datasets)))
  return(shared_features)
}

run_feature_selection <- function(datasets, models, split_ratio, parameters, max_features_select = 10, progress_file = NULL, multi_dataset_mode = "combine") {
  load_packages_globally(c("caret", "Boruta", "randomForest", "e1071", "glmnet", "MASS", "class", "ranger", "gbm", "pROC", "reticulate", "ggplot2"))
  cat(sprintf("[ML] Running Feature Selection (max_features_select = %s, multi_dataset_mode = %s)...\n", max_features_select, multi_dataset_mode))

  # If multiple datasets, compute the shared feature set in-memory (no disk writes)
  shared_features <- compute_shared_features(datasets)

  parsed_ml <- parse_models_payload(models, parameters)
  models <- parsed_ml$models
  parameters <- parsed_ml$parameters

  # Write a small JSON progress file (read by the async poll endpoint for a live %).
  .write_progress <- function(current, total, model, phase = "fitting") {
    if (is.null(progress_file)) return(invisible())
    tryCatch(
      writeLines(jsonlite::toJSON(list(current = current, total = total, model = model, phase = phase),
                                  auto_unbox = TRUE), progress_file),
      error = function(e) NULL
    )
  }
  .n_models <- length(models)
  .model_i  <- 0

  # ── Top-level guard: always return metrics or a structured error payload ──
  # The caller (job worker or sync route) checks for `.error` in the result
  # and surfaces it as a toast notification on the frontend.
  tryCatch_fs <- function(expr) {
    tryCatch(expr, error = function(e) {
      msg <- conditionMessage(e)
      cat("[ERROR] run_feature_selection top-level:", msg, "\n")
      list(.error = msg)
    })
  }

  # Classify datasets into train and test
  resolved <- resolve_and_preprocess_datasets(datasets, split_ratio, multi_dataset_mode = multi_dataset_mode)
  train_dss <- resolved$train
  test_dss  <- resolved$test
  all_dss   <- c(train_dss, test_dss)

  results <- list()

  for (d_id in names(all_dss)) {
    d <- all_dss[[d_id]]
    # Set seed here so it covers prepare_ml_data (ANOVA sort, sample alignment) AND
    # Global seed set once before data prep and all model fitting — RNG runs
    # sequentially through models to match testing_feature_selection.R behavior.
    set.seed(42)
    # Prepare data using full feature space for all models (no ANOVA pre-filter).
    # shared_features restricts to the intersection across datasets (in-memory, no disk writes).
    ml_data <- prepare_ml_data(d, max_features = NULL, shared_features = shared_features)
    if (is.null(ml_data)) {
      cat(sprintf("[DEBUG] run_feature_selection: prepare_ml_data returned NULL for %s\n", d_id))
      next
    }

    df <- ml_data$df
    gene_mapping <- ml_data$original_genes
    names(gene_mapping) <- make.names(gene_mapping)

    # Split train/test  (seed already set above, before prepare_ml_data)
    n_samples <- nrow(df)
    
    ds_split_ratio <- d$fs_trainRatio
    if (is.null(ds_split_ratio)) ds_split_ratio <- split_ratio
    if (is.null(ds_split_ratio)) ds_split_ratio <- 0.7
    
    val_strategy <- d$fs_validationStrategy %||% d$validationStrategy %||% "train-test-split"
    if (ds_split_ratio >= 1.0 || val_strategy == "cv-only") {
      train_df <- df
      test_df  <- df
    } else {
      train_idx <- tryCatch(caret::createDataPartition(df$Group, p = ds_split_ratio, list = FALSE), error = function(e) seq_len(nrow(df)))
      train_df <- df[train_idx, , drop = FALSE]
      test_df  <- df[-train_idx, , drop = FALSE]
      if (nrow(test_df) == 0) test_df <- train_df
    }
    
    # Store the train and test set to session-isolated RDS files
    base_id <- get_base_id(d$id)
    saveRDS(train_df, get_session_path(base_id, "%s_fs_train_split.rds"))
    saveRDS(test_df, get_session_path(base_id, "%s_fs_test_split.rds"))
    saveRDS(parameters, get_session_path(base_id, "%s_fs_parameters.rds"))

    # Save CSV files for train set expr & clin, and test set expr & clin
    tryCatch({
      clin_df_orig <- ml_data$clin_df
      uid_val <- get_user_id(base_id)
      
      # 1. Train set expression CSV
      train_expr_df <- cbind(SampleID = rownames(train_df), train_df[, setdiff(colnames(train_df), "Group"), drop = FALSE])
      write.csv(train_expr_df, get_session_path(base_id, "%s_fs_train_expr.csv"), row.names = FALSE)

      # 2. Train set clinical CSV
      train_clin_path <- get_session_path(base_id, "%s_fs_train_clin.csv")
      if (!is.null(clin_df_orig) && any(rownames(train_df) %in% rownames(clin_df_orig))) {
        train_clin_df <- cbind(SampleID = rownames(train_df), clin_df_orig[rownames(train_df), , drop = FALSE])
        write.csv(train_clin_df, train_clin_path, row.names = FALSE)
      } else {
        train_clin_df <- data.frame(SampleID = rownames(train_df), Group = train_df$Group, stringsAsFactors = FALSE)
        write.csv(train_clin_df, train_clin_path, row.names = FALSE)
      }
      register_export_file(uid_val, "fs_train_split_clinical", base_id, train_clin_path, "fs", "split")

      # 3. Test set expression CSV
      if (val_strategy != "cv-only" && ds_split_ratio < 1.0) {
        test_expr_df <- cbind(SampleID = rownames(test_df), test_df[, setdiff(colnames(test_df), "Group"), drop = FALSE])
        write.csv(test_expr_df, get_session_path(base_id, "%s_fs_test_expr.csv"), row.names = FALSE)
      }

      # 4. Test set clinical CSV
      if (val_strategy != "cv-only" && ds_split_ratio < 1.0) {
        test_clin_path <- get_session_path(base_id, "%s_fs_test_clin.csv")
        if (!is.null(clin_df_orig) && any(rownames(test_df) %in% rownames(clin_df_orig))) {
          test_clin_df <- cbind(SampleID = rownames(test_df), clin_df_orig[rownames(test_df), , drop = FALSE])
          write.csv(test_clin_df, test_clin_path, row.names = FALSE)
        } else {
          test_clin_df <- data.frame(SampleID = rownames(test_df), Group = test_df$Group, stringsAsFactors = FALSE)
          write.csv(test_clin_df, test_clin_path, row.names = FALSE)
        }
        register_export_file(uid_val, "fs_test_split_clinical", base_id, test_clin_path, "fs", "split")
      }
      cat(sprintf("  Saved train/test expression and clinical CSV files for %s.\n", base_id))
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save train/test CSV files for %s: %s\n", base_id, e$message))
    })

    if (nrow(test_df) == 0) {
      test_df <- train_df # Fallback to train if test size is zero
    }

    # Skip feature selection training for test datasets
    if (!(d_id %in% names(train_dss))) {
      next
    }

    accuracies          <- list()
    loss_histories      <- list()
    all_features        <- list()
    model_features      <- list()
    roc_data            <- list()
    roc_raw_data        <- list()
    performance_metrics <- list()
    job_warnings        <- character(0)

    # ── Sequential Model Fitting (Full Cores per Model) ───────────────────────
    # Each model runs in sequence, subscribing to all available CPU cores
    # (get_effective_cores()). This maximizes performance for heavy algorithms (STABL,
    # Boruta, Ranger) while providing live per-model progress updates and lower RAM usage.
    current_train_fp <- get_matrix_fingerprint(train_df)
    eff_cores <- get_effective_cores()

    for (mi in seq_along(models)) {
      m <- models[[mi]]
      .model_i <- .model_i + 1
      .write_progress(.model_i, .n_models, m, "fitting")
      cat(sprintf("  Fitting model: %s (using %d cores)...\n", m, eff_cores))

      imp_path  <- get_session_path(base_id, sprintf("%%s_fs_full_importances_%s.rds", m))
      meta_path <- get_session_path(base_id, sprintf("%%s_fs_discovery_cache_meta_%s.rds", m))

      cache_hit <- FALSE
      if (file.exists(imp_path) && file.exists(meta_path)) {
        saved <- tryCatch(readRDS(meta_path), error = function(e) NULL)
        if (!is.null(saved) &&
            identical(saved$upstream_fingerprint, current_train_fp) &&
            identical(saved$train_ratio,          as.numeric(d$fs_trainRatio)) &&
            identical(saved$validation_strategy,  as.character(d$fs_validationStrategy)) &&
            identical(saved$group_col,            as.character(d$clinicalGroupCol)) &&
            identical(saved$hyperparams,          parameters[[m]])) {
          feats <- tryCatch(readRDS(imp_path), error = function(e) NULL)
          if (!is.null(feats)) {
            cat(sprintf("[FS] Cache hit for model '%s' — skipping discovery.\n", m))
            model_feats <- feats
            for (i in seq_along(model_feats)) model_feats[[i]]$rank <- i
            model_result <- list(
              ok          = TRUE,
              warning     = NULL,
              eval_res    = list(acc=NA, fpr=numeric(0), tpr=numeric(0), auc=NA, ppv=NA, npv=NA, prec=NA, rec=NA),
              fold_scores = numeric(0),
              model_feats = model_feats
            )
            cache_hit <- TRUE
          }
        }
      }

      if (!cache_hit) {
        model_result <- tryCatch({
          train_m <- train_df
          test_m  <- test_df

          res <- fit_model_and_importance(m, train_m, gene_mapping, parameters, ds_id = d$id, n_threads = eff_cores)
          fit <- res$model
          model_warning <- res$warning
          model_feats <- res$features

          # Sort model_feats by importance descending
          model_feats <- model_feats[order(sapply(model_feats, function(f) f$importance), decreasing = TRUE)]
          # Exclude features with importance == 0
          model_feats <- Filter(function(f) f$importance > 0, model_feats)

          if (is.null(fit)) {
            if (m == "stabl" && length(model_feats) == 0) {
              cat("[STABL] Warning: STABL selected 0 features. Proceeding with empty model.\n")
            } else {
              stop(sprintf("Model training returned NULL for '%s'. Check data and parameters.", m))
            }
          }

          # Re-rank features
          if (length(model_feats) > 0) {
            for (i in seq_along(model_feats)) {
              model_feats[[i]]$rank <- i
            }
          }

          # Evaluate the fitted model on the training set to get real training performance metrics and ROC curves
          eval_res <- tryCatch({
            if (is.null(fit)) {
              list(acc = 0.0, fpr = numeric(0), tpr = numeric(0), auc = 0.0, ppv = 0.0, npv = 0.0, prec = 0.0, rec = 0.0)
            } else {
              train_m_eval <- train_m
              lvls_orig <- levels(train_m_eval$Group)
              safe_lvls <- make.names(lvls_orig, unique = TRUE)
              levels(train_m_eval$Group) <- safe_lvls

              pred_res <- predict_ml(m, fit, train_m_eval)
              ev <- evaluate_predictions(pred_res$predictions, train_m_eval$Group, pred_res$probabilities, target_prevalence = d$targetPrevalence)

              list(
                acc  = ev$bacc,
                fpr  = ev$fpr,
                tpr  = ev$tpr,
                auc  = ev$auc,
                ppv  = ev$ppv %||% ev$prec,
                npv  = ev$npv %||% ev$rec,
                prec = ev$ppv %||% ev$prec,
                rec  = ev$npv %||% ev$rec,
                tp   = ev$tp,
                fp   = ev$fp,
                fn   = ev$fn,
                tn   = ev$tn,
                prob = pred_res$probabilities,
                truth = train_m_eval$Group
              )
            }
          }, error = function(e) {
            cat("[WARNING] Failed to evaluate model training performance:", e$message, "\n")
            list(acc = 0.5, fpr = numeric(0), tpr = numeric(0), auc = 0.5, ppv = 0.5, npv = 0.5, prec = 0.5, rec = 0.5)
          })

          list(
            ok          = TRUE,
            warning     = model_warning,
            eval_res    = eval_res,
            fold_scores = numeric(0),
            model_feats = model_feats
          )
        }, error = function(e) {
          err_msg <- conditionMessage(e)
          cat(sprintf("[ERROR] Feature selection model '%s' failed: %s\n", m, err_msg))
          list(ok = FALSE, message = err_msg)
        })
      }

      if (!isTRUE(model_result$ok)) {
        # Propagate a structured error so the frontend toast fires
        err_payload <- list(
          .error  = sprintf("Model '%s' failed during feature selection: %s", m, model_result$message),
          .model  = m,
          .stage  = "feature_selection"
        )
        results[[d_id]] <- err_payload
        # Stop processing further models for this dataset on hard failure
        cat(sprintf("[ERROR] Aborting feature selection for dataset '%s' due to model '%s' failure.\n", d_id, m))
        next
      }

      if (!is.null(model_result$warning)) job_warnings <- union(job_warnings, model_result$warning)

      eval_res    <- model_result$eval_res
      fold_scores <- model_result$fold_scores
      model_feats <- model_result$model_feats

      # Collect importance from first model to represent overall rank
      if (length(all_features) == 0) {
        all_features <<- model_feats
      }

      # Save full importances for refit
      saveRDS(model_feats, get_session_path(base_id, sprintf("%%s_fs_full_importances_%s.rds", m)))

      # Save discovery cache sidecar (internal only — never register_export_file)
      tryCatch(
        saveRDS(
          list(
            upstream_fingerprint = current_train_fp,
            train_ratio          = as.numeric(d$fs_trainRatio),
            validation_strategy  = as.character(d$fs_validationStrategy),
            group_col            = as.character(d$clinicalGroupCol),
            hyperparams          = parameters[[m]]
          ),
          get_session_path(base_id, sprintf("%%s_fs_discovery_cache_meta_%s.rds", m))
        ),
        error = function(e) cat(sprintf("[FS] Could not save discovery cache meta for model '%s': %s\n", m, e$message))
      )

      # Collect importance from first model to represent overall rank
      if (length(all_features) == 0) {
        all_features <<- model_feats
      }

      model_features[[m]] <- model_feats

      if (length(all_features) == 0 && length(model_feats) > 0) {
        all_features <- model_feats
      }

      accuracies[[m]]     <- round(eval_res$acc, 3)
      roc_data[[m]]       <- list(fpr = as.numeric(eval_res$fpr), tpr = as.numeric(eval_res$tpr), auc = round(eval_res$auc, 3))
      
      roc_obj_tmp <- tryCatch({
        pROC::roc(response = eval_res$truth, predictor = eval_res$prob, quiet = TRUE)
      }, error = function(e) NULL)

      b_res <- if (!is.null(roc_obj_tmp)) {
        tryCatch(compute_bootstrap_roc(roc_obj_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
      } else NULL

      ci_auc <- if (!is.null(b_res)) b_res$ci_auc else c(NA_real_, NA_real_)
      if (!is.null(roc_obj_tmp) && !is.null(b_res)) {
        roc_obj_tmp$ci_auc  <- b_res$ci_auc
        roc_obj_tmp$band_df <- b_res$band_df
      }
      roc_raw_data[[m]] <- roc_obj_tmp
      ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"

      performance_metrics[[m]] <- list(
        auc      = round(eval_res$auc, 3),
        ci       = ci_str,
        ci_lower = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
        ci_upper = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
        acc      = round(eval_res$acc, 3),
        ppv      = round(eval_res$ppv %||% eval_res$prec, 3),
        npv      = round(eval_res$npv %||% eval_res$rec, 3),
        prec     = round(eval_res$ppv %||% eval_res$prec, 3),
        rec      = round(eval_res$npv %||% eval_res$rec, 3),
        fpr      = as.numeric(eval_res$fpr),
        tpr      = as.numeric(eval_res$tpr),
        tp       = eval_res$tp,
        fp       = eval_res$fp,
        fn       = eval_res$fn,
        tn       = eval_res$tn
      )

      loss_histories[[m]] <- fold_scores
    }

    # Generate ROC Plot for training validation
    roc_plot_base64 <- ""
    standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
    valid_raw_models <- intersect(standard_order, intersect(names(roc_raw_data), models))
    valid_raw_models <- Filter(function(m) !is.null(roc_raw_data[[m]]), valid_raw_models)
    if (length(valid_raw_models) > 0) {
      roc_raw_data_mapped <- roc_raw_data[valid_raw_models]
      names(roc_raw_data_mapped) <- sapply(names(roc_raw_data_mapped), function(m) MODEL_LABELS_MAP[[m]] %||% m)
      p_roc <- plot_roc_multi(roc_raw_data_mapped, title = NULL, show_auc_in_legend = FALSE, bands = TRUE, band = "ci", n_boot = 150) +
        ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.key.spacing.y = grid::unit(0.5, "cm")) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
      
      tryCatch({
        roc_train_pdf <- get_session_path(d$id, "roc_train_%s.pdf")
        ggplot2::ggsave(filename = roc_train_pdf, plot = p_roc, width = 10, height = 7)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_train_%s.png"), plot = p_roc, width = 10, height = 7, dpi = 300)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_train_%s.tiff"), plot = p_roc, width = 10, height = 7, dpi = 300)
        register_export_file(get_user_id(d$id), "roc_all_models_train", d$id, roc_train_pdf, "fs", "training", ext = "pdf")
      }, error = function(e) {
        cat("[WARNING] Failed to save training ROC plot:", e$message, "\n")
      })

      # Per-model ROC curves — all models have probs (boruta→ranger, stabl→glmnet)
      for (m in valid_raw_models) {
        roc_obj <- roc_raw_data[[m]]
        if (is.null(roc_obj)) next
        model_label <- MODEL_LABELS_MAP[[m]] %||% m
        tryCatch({
          p_roc_single <- plot_roc(roc_obj,
                                   title = sprintf("%s — Training", model_label),
                                   band = "ci", n_boot = 150, show_folds = FALSE)
          m_roc_pdf <- get_session_path(d$id, sprintf("%%s_roc_train_%s.pdf", m))
          save_roc(p_roc_single,
                   m_roc_pdf,
                   width = 8, height = 7, dpi = 300)
          save_roc(p_roc_single,
                   get_session_path(d$id, sprintf("%%s_roc_train_%s.png", m)),
                   width = 8, height = 7, dpi = 300)
          register_export_file(get_user_id(d$id), "roc_train_model", d$id, m_roc_pdf, "fs", "training", model = m, ext = "pdf")
          rm(p_roc_single, roc_obj)
        }, error = function(e) {
          cat(sprintf("[WARNING] Per-model training ROC (%s): %s\n", m, e$message))
        })
      }
      
      roc_plot_base64 <- tryCatch(plot_to_base64(p_roc, width = 930, height = 650), error = function(e) "")
      rm(roc_raw_data_mapped, p_roc)
      invisible(gc(verbose = FALSE))
    }

    # Save and register all_models_performance_train CSV and per-model selected features / importances
    tryCatch({
      uid_val <- get_user_id(d$id)
      if (length(performance_metrics) > 0) {
        p_models <- names(performance_metrics)
        perf_df <- data.frame(
          Model = sapply(p_models, function(m) MODEL_LABELS_MAP[[m]] %||% toupper(m)),
          AUC = sapply(p_models, function(m) performance_metrics[[m]]$auc %||% NA),
          AUC_95_CI = sapply(p_models, function(m) performance_metrics[[m]]$ci %||% NA),
          Balanced_Accuracy = sapply(p_models, function(m) performance_metrics[[m]]$acc %||% NA),
          PPV = sapply(p_models, function(m) performance_metrics[[m]]$ppv %||% NA),
          NPV = sapply(p_models, function(m) performance_metrics[[m]]$npv %||% NA),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        colnames(perf_df) <- c("Model", "AUC", "95% CI (AUC)", "Accuracy", "PPV", "NPV")
        d_base_id <- get_base_id(d$id)
        perf_train_path <- get_session_path(d_base_id, "%s_all_models_performance_train.csv")
        write.csv(perf_df, perf_train_path, row.names = FALSE)
        register_export_file(uid_val, "all_models_performance_train", d_base_id, perf_train_path, "fs", "training")
      }

      d_base_id <- get_base_id(d$id)
      for (m in models) {
        m_feats <- model_features[[m]]
        if (!is.null(m_feats) && length(m_feats) > 0) {
          gene_names <- as.character(sapply(m_feats, function(f) f$gene))
          if (m %in% c("stabl", "boruta")) {
            sf_df <- data.frame(Gene = gene_names, stringsAsFactors = FALSE)
          } else {
            sf_df <- data.frame(Rank = seq_along(m_feats), Gene = gene_names, Importance = sapply(m_feats, function(f) f$importance %||% NA), stringsAsFactors = FALSE)
          }
          sf_path <- get_session_path(d_base_id, sprintf("%%s_selected_features_%s.csv", m))
          write.csv(sf_df, sf_path, row.names = FALSE)
          register_export_file(uid_val, "selected_features", d_base_id, sf_path, "fs", "training", model = m)

          if (!m %in% c("stabl", "boruta")) {
            fi_path <- get_session_path(d_base_id, sprintf("%%s_feature_importance_%s.csv", m))
            write.csv(sf_df, fi_path, row.names = FALSE)
            register_export_file(uid_val, "feature_importance", d_base_id, fi_path, "fs", "training", model = m)
          }
        }
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save training performance/feature tables for %s: %s\n", d$id, e$message))
    })

    # Calculate overlap count and Venn plot if multiple models are run
    overlap_count <- 0
    overlap_genes <- character(0)
    venn_plot_base64 <- ""
    if (length(models) > 1) {
      model_sets <- list()
      for (m in models) {
        m_feats <- model_features[[m]]
        if (m %in% c("boruta", "stabl")) {
          model_sets[[m]] <- sapply(m_feats, function(f) f$gene)
        } else {
          m_imps <- sapply(m_feats, function(f) f$importance)
          m_genes <- sapply(m_feats, function(f) f$gene)
          n_el <- find_elbow_point(m_imps)
          model_sets[[m]] <- if (n_el > 0) m_genes[1:n_el] else character(0)
        }
      }
      overlap_genes <- Reduce(intersect, model_sets)
      overlap_count <- length(overlap_genes)
    }

    # Sort overall features by importance descending
    all_features <- all_features[order(sapply(all_features, function(f) f$importance), decreasing = TRUE)]
    # Cap the payload: full-genome selectors can produce tens of thousands of rows.
    if (length(all_features) > 500) all_features <- all_features[1:500]
    # Remove importance score and rank for STABL and Boruta in model_features and all_features
    for (m in names(model_features)) {
      if (m %in% c("boruta", "stabl")) {
        model_features[[m]] <- lapply(model_features[[m]], function(f) {
          f$importance <- NULL
          f$rank <- NULL
          f
        })
      }
    }
    if (length(models) > 0 && models[1] %in% c("boruta", "stabl")) {
      all_features <- lapply(all_features, function(f) {
        f$importance <- NULL
        f$rank <- NULL
        f
      })
    } else {
      for (i in seq_along(all_features)) {
        all_features[[i]]$rank <- i
      }
    }

    results[[d_id]] <- list(
      features            = all_features,
      model_features      = model_features,
      accuracies          = accuracies,
      loss_histories      = loss_histories,
      roc_data            = roc_data,
      performance_metrics = performance_metrics,
      roc_plot            = roc_plot_base64,
      venn_plot           = venn_plot_base64,
      overlap_count       = overlap_count,
      warnings            = as.list(job_warnings)
    )
  }

  return(results)
}

# ---------------------------------------------------------------
# 3. Cross-Validation Handler
# ---------------------------------------------------------------
run_cross_validation <- function(models, cv_method, folds, datasets, multi_dataset_mode = "combine") {
  load_packages_globally(c("caret", "Boruta", "randomForest", "e1071", "glmnet", "MASS", "class", "ranger", "gbm", "pROC", "reticulate", "ggplot2"))
  cat(sprintf("[ML] Running Cross Validation (method: %s, multi_dataset_mode: %s)...\n", cv_method, multi_dataset_mode))

  parsed_ml <- parse_models_payload(models, NULL)
  models <- parsed_ml$models

  # Classify datasets into train and test
  resolved <- resolve_and_preprocess_datasets(datasets, 0.7, multi_dataset_mode = multi_dataset_mode)
  train_dss <- resolved$train
  test_dss  <- resolved$test

  cv_dss <- list()
  for (d_id in names(train_dss)) {
    cv_dss[[d_id]] <- train_dss[[d_id]]
  }
  for (t_id in names(test_dss)) {
    t <- test_dss[[t_id]]
    val_strat <- t$fs_validationStrategy %||% t$validationStrategy %||% "train-test-split"
    if (val_strat == "cv-only" || val_strat == "cv-and-test") {
      cv_dss[[t_id]] <- t
    }
  }

  # Compute shared feature set in-memory for multi-dataset runs (no disk writes)
  shared_features <- compute_shared_features(datasets)

  results <- list()

  for (d_id in names(cv_dss)) {
    d <- cv_dss[[d_id]]
    cat(sprintf("[DEBUG] run_cross_validation: dataset id = %s\n", d$id))
    ml_data <- prepare_ml_data(d, max_features = NULL, shared_features = shared_features)
    if (is.null(ml_data)) next

    # Load stored train split if it exists, to perform cross-validation on the train set
    base_id <- get_base_id(d$id)
    train_split_path <- get_session_path(base_id, "%s_fs_train_split.rds")
    if (file.exists(train_split_path)) {
      cat(sprintf("  Loading stored train split for %s to perform Cross Validation...\n", d$name))
      df <- readRDS(train_split_path)
    } else {
      df <- ml_data$df
    }
    gene_mapping <- ml_data$original_genes
    
    # Load parameters if available
    params_path <- get_session_path(base_id, "%s_fs_parameters.rds")
    params <- if (file.exists(params_path)) readRDS(params_path) else NULL

    # Ensure Group levels are valid R identifiers (caret requirement)
    lvls_orig <- levels(df$Group)
    safe_lvls <- make.names(lvls_orig, unique = TRUE)
    levels(df$Group) <- safe_lvls

    # Defensive: remove constant columns
    feat_cols <- setdiff(colnames(df), "Group")
    non_constant <- sapply(feat_cols, function(cn) length(unique(df[[cn]])) > 1)
    df <- df[, c(feat_cols[non_constant], "Group"), drop = FALSE]

    n_samples <- nrow(df)

    # Determine CV method (LOOCV if N < 50 or explicitly requested, else 5-fold)
    is_loocv <- (n_samples < 50)
    if (!is.null(folds)) {
      if (folds == "loocv" || folds == "-1" || folds == -1) {
        is_loocv <- TRUE
      } else {
        folds_num <- suppressWarnings(as.numeric(folds))
        if (!is.na(folds_num)) {
          if (folds_num == -1 || folds_num >= n_samples) {
            is_loocv <- TRUE
          } else if (folds_num >= 2) {
            is_loocv <- FALSE
          }
        }
      }
    }

    if (is_loocv) {
      cv_method_str <- "LOOCV"
      cv_folds <- lapply(1:n_samples, function(i) i)
    } else {
      cv_method_str <- "5-fold"
      k_folds <- if (!is.null(folds) && !is.na(as.numeric(folds)) && as.numeric(folds) >= 2) min(as.numeric(folds), n_samples) else min(5, n_samples)
      set.seed(42)
      cv_folds <- caret::createFolds(df$Group, k = k_folds, list = TRUE, returnTrain = FALSE)
    }

    ds_results <- list()
    trained_fits <- list()
    cv_roc_data <- list()
    cv_raw_roc_data <- list()

    for (m in models) {
      cat(sprintf("  Cross-validating model: %s (%s)...\n", m, cv_method_str))

      cv_model_result <- tryCatch({
        top_feats_path <- get_session_path(base_id, sprintf("%%s_fs_top_features_%s.rds", m))
        if (file.exists(top_feats_path)) {
          top_genes_orig  <- readRDS(top_feats_path)
          top_genes_clean <- make.names(top_genes_orig)
          sel_cols <- top_genes_clean[top_genes_clean %in% colnames(df)]
          df_cv <- df[, c(sel_cols, "Group"), drop = FALSE]
          gene_mapping_cv <- top_genes_orig
          names(gene_mapping_cv) <- top_genes_clean
          cat(sprintf("  Model %s: Using top %d selected features for Cross-Validation.\n", m, length(top_genes_orig)))
        } else {
          df_cv <- df
          gene_mapping_cv <- gene_mapping
        }

        retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)

        fold_results <- list()
        all_probs <- numeric(n_samples)

        eff_c <- get_effective_cores()
        process_fold <- function(f_idx) {
          val_idx <- cv_folds[[f_idx]]

          cv_train <- df_cv[-val_idx, , drop = FALSE]
          cv_val   <- df_cv[val_idx, , drop = FALSE]

          fit_res <- fit_model_and_importance(retrain_model_name, cv_train, gene_mapping_cv, params, n_threads = eff_c)
          if (is.null(fit_res$model)) stop(sprintf("CV fold fit failed for model '%s'", m))

          pred_res <- predict_ml(retrain_model_name, fit_res$model, cv_val)
          
          ev_fold <- if (!is_loocv) {
            evaluate_predictions(pred_res$predictions, cv_val$Group, pred_res$probabilities, target_prevalence = d$targetPrevalence)
          } else {
            NULL
          }
          
          list(
            val_idx = val_idx,
            probabilities = pred_res$probabilities,
            predictions = pred_res$predictions,
            ev_fold = ev_fold
          )
        }

        fold_outputs <- lapply(seq_along(cv_folds), process_fold)

        for (f_idx in seq_along(fold_outputs)) {
          fo <- fold_outputs[[f_idx]]
          all_probs[fo$val_idx] <- fo$probabilities
          if (!is_loocv) {
            fold_results[[f_idx]] <- fo$ev_fold
          }
        }

        pred_fac <- factor(ifelse(all_probs >= 0.5, safe_lvls[2], safe_lvls[1]), levels = safe_lvls)
        ev_global <- evaluate_predictions(pred_fac, df$Group, all_probs, target_prevalence = d$targetPrevalence)

        if (is_loocv) {
          mean_auc   <- ev_global$auc
          sd_auc     <- 0.0
          mean_bacc  <- ev_global$bacc
          mean_ppv   <- ev_global$ppv %||% ev_global$prec
          mean_npv   <- ev_global$npv %||% ev_global$rec
        } else {
          fold_aucs  <- sapply(fold_results, function(x) x$auc)
          fold_baccs <- sapply(fold_results, function(x) x$bacc)
          fold_ppvs  <- sapply(fold_results, function(x) x$ppv %||% x$prec)
          fold_npvs  <- sapply(fold_results, function(x) x$npv %||% x$rec)

          mean_auc   <- mean(fold_aucs, na.rm = TRUE)
          sd_auc     <- if (length(fold_aucs) > 1) stats::sd(fold_aucs, na.rm = TRUE) else 0
          if (is.na(sd_auc)) sd_auc <- 0
          mean_bacc  <- mean(fold_baccs, na.rm = TRUE)
          mean_ppv   <- mean(fold_ppvs,  na.rm = TRUE)
          mean_npv   <- mean(fold_npvs,  na.rm = TRUE)
        }

        roc_tmp <- tryCatch({
          pROC::roc(response = df$Group, predictor = all_probs, quiet = TRUE)
        }, error = function(e) NULL)

        b_res <- if (!is.null(roc_tmp)) {
          tryCatch(compute_bootstrap_roc(roc_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
        } else NULL

        ci_auc <- if (!is.null(b_res)) {
          b_res$ci_auc
        } else if (!is_loocv && length(fold_aucs) > 1) {
          z <- stats::qnorm(0.975)
          se <- stats::sd(fold_aucs, na.rm = TRUE) / sqrt(length(fold_aucs))
          c(max(0, mean_auc - z * se), min(1, mean_auc + z * se))
        } else {
          c(NA_real_, NA_real_)
        }

        if (!is.null(roc_tmp) && !is.null(b_res)) {
          roc_tmp$ci_auc  <- b_res$ci_auc
          roc_tmp$band_df <- b_res$band_df
        }
        ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"

        list(
          ok         = TRUE,
          model      = m,
          bestScore  = sprintf("%.3f", mean_auc),
          best_auc   = mean_auc,
          ci         = ci_str,
          ci_lower   = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
          ci_upper   = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
          std        = sprintf("%.3f", sd_auc),
          std_val    = sd_auc,
          accuracy   = sprintf("%.2f", mean_bacc),
          ppv        = sprintf("%.2f", mean_ppv),
          npv        = sprintf("%.2f", mean_npv),
          ev_global  = ev_global,
          prob       = all_probs,
          truth      = df$Group,
          roc_obj    = roc_tmp
        )
      }, error = function(e) {
        err_msg <- conditionMessage(e)
        cat(sprintf("[ERROR] Cross-validation model '%s' failed: %s\n", m, err_msg))
        list(
          ok         = FALSE,
          model      = m,
          .error     = err_msg,
          bestScore  = "0.000", best_auc = 0.0,
          ci         = "N/A", ci_lower = NA, ci_upper = NA,
          std        = "0.000", std_val  = 0,
          accuracy   = "0.00",
          ppv        = "0.00", npv       = "0.00"
        )
      })

      ds_results[[length(ds_results) + 1]] <- cv_model_result
      
      if (isTRUE(cv_model_result$ok) && !is.null(cv_model_result$ev_global)) {
        cv_roc_data[[m]] <- list(
          fpr = as.numeric(cv_model_result$ev_global$fpr),
          tpr = as.numeric(cv_model_result$ev_global$tpr),
          auc = cv_model_result$ev_global$auc
        )
        cv_raw_roc_data[[m]] <- cv_model_result$roc_obj %||% tryCatch({
          pROC::roc(response = cv_model_result$truth, predictor = cv_model_result$prob, quiet = TRUE)
        }, error = function(e) NULL)
      }
    }
    
    # Generate CV Curves plot and save as PDF/PNG/JPEG
    tryCatch({
      cv_plots <- list()
      for (m in names(trained_fits)) {
        fit_obj <- trained_fits[[m]]
        if (!is.null(fit_obj) && inherits(fit_obj, "train") && nrow(fit_obj$results) > 1) {
          res_df <- fit_obj$results
          metric_col <- fit_obj$metric
          param_cols <- setdiff(colnames(res_df), c("ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD", "Accuracy", "Kappa", "AccuracySD", "KappaSD"))
          if (length(param_cols) > 0) {
            x_col <- param_cols[1]
            p <- ggplot2::ggplot(res_df, ggplot2::aes(x = .data[[x_col]], y = .data[[metric_col]])) +
              ggplot2::geom_line(color = "#2563eb", linewidth = 1) +
              geom_point(color = "#2563eb", size = 2) +
              ggplot2::theme_minimal()
            cv_plots[[m]] <- p
          }
        }
      }
      if (length(cv_plots) > 0) {
        pdf_path <- get_session_path(d$id, "cv_curves_%s.pdf")
        png_path <- get_session_path(d$id, "cv_curves_%s.png")
        tiff_path <- get_session_path(d$id, "cv_curves_%s.tiff")
        
        dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
        
        pdf(pdf_path, width = 7, height = 5)
        for (m in names(cv_plots)) {
          print(cv_plots[[m]])
        }
        dev.off()
        
        ggplot2::ggsave(filename = png_path, plot = cv_plots[[1]], width = 7, height = 5, dpi = 300)
        ggplot2::ggsave(filename = tiff_path, plot = cv_plots[[1]], width = 7, height = 5, dpi = 300)
        cat(sprintf("  Saved CV validation curves for %s to tmp/.\n", d$id))
      }
    }, error = function(e) {
      cat("[WARNING] Failed to generate CV curves plot:", e$message, "\n")
    })
    
    # Generate ROC Plot for Cross Validation
    cv_roc_plot_base64 <- ""
    standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
    valid_cv_models <- intersect(standard_order, intersect(names(cv_raw_roc_data), models))
    valid_cv_models <- Filter(function(m) !is.null(cv_raw_roc_data[[m]]), valid_cv_models)
    
    if (length(valid_cv_models) > 0) {
      cv_raw_roc_data_mapped <- cv_raw_roc_data[valid_cv_models]
      names(cv_raw_roc_data_mapped) <- sapply(names(cv_raw_roc_data_mapped), function(m) MODEL_LABELS_MAP[[m]] %||% m)
      p_roc <- plot_roc_multi(cv_raw_roc_data_mapped, title = NULL, show_auc_in_legend = FALSE, bands = TRUE, band = "ci", n_boot = 150) +
        ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.key.spacing.y = grid::unit(0.5, "cm")) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
      
      tryCatch({
        roc_cv_pdf <- get_session_path(d$id, "roc_cv_%s.pdf")
        ggplot2::ggsave(filename = roc_cv_pdf, plot = p_roc, width = 10, height = 7)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_cv_%s.png"), plot = p_roc, width = 10, height = 7, dpi = 300)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_cv_%s.tiff"), plot = p_roc, width = 10, height = 7, dpi = 300)
        register_export_file(get_user_id(d$id), "roc_cv", d$id, roc_cv_pdf, "fs", "cv", ext = "pdf")
      }, error = function(e) {
        cat("[WARNING] Failed to save CV ROC plot:", e$message, "\n")
      })

      # Per-model CV ROC curves
      for (m in valid_cv_models) {
        roc_obj <- cv_raw_roc_data[[m]]
        if (is.null(roc_obj)) next
        model_label <- MODEL_LABELS_MAP[[m]] %||% m
        tryCatch({
          p_roc_single <- plot_roc(roc_obj,
                                   title = sprintf("%s — CV", model_label),
                                   band = "ci", n_boot = 150, show_folds = FALSE)
          m_cv_roc_pdf <- get_session_path(d$id, sprintf("%%s_roc_cv_%s.pdf", m))
          save_roc(p_roc_single,
                   m_cv_roc_pdf,
                   width = 8, height = 7, dpi = 300)
          save_roc(p_roc_single,
                   get_session_path(d$id, sprintf("%%s_roc_cv_%s.png", m)),
                   width = 8, height = 7, dpi = 300)
          register_export_file(get_user_id(d$id), "roc_cv_model", d$id, m_cv_roc_pdf, "fs", "cv", model = m, ext = "pdf")
          rm(p_roc_single, roc_obj)
        }, error = function(e) {
          cat(sprintf("[WARNING] Per-model CV ROC (%s): %s\n", m, e$message))
        })
      }
      
      cv_roc_plot_base64 <- tryCatch(plot_to_base64(p_roc, width = 930, height = 650), error = function(e) "")
      rm(cv_raw_roc_data_mapped, p_roc)
      invisible(gc(verbose = FALSE))
    }

    # Save and register all_models_performance_cv CSV
    tryCatch({
      uid_val <- get_user_id(d$id)
      if (length(ds_results) > 0) {
        cv_df <- data.frame(
          Model = sapply(ds_results, function(x) MODEL_LABELS_MAP[[x$model]] %||% toupper(x$model)),
          AUC = sapply(ds_results, function(x) x$bestScore %||% NA),
          AUC_95_CI = sapply(ds_results, function(x) x$ci %||% NA),
          Balanced_Accuracy = sapply(ds_results, function(x) x$accuracy %||% NA),
          PPV = sapply(ds_results, function(x) x$ppv %||% NA),
          NPV = sapply(ds_results, function(x) x$npv %||% NA),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        colnames(cv_df) <- c("Model", "AUC", "95% CI (AUC)", "Accuracy", "PPV", "NPV")
        cv_csv_path <- get_session_path(d$id, "%s_all_models_performance_cv.csv")
        write.csv(cv_df, cv_csv_path, row.names = FALSE)
        register_export_file(uid_val, "cv_results", d$id, cv_csv_path, "fs", "cv")
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save CV performance table for %s: %s\n", d$id, e$message))
    })

    # Clean up the returned metrics to not include ev_global (to keep JSON response small)
    for (idx in seq_along(ds_results)) {
      ds_results[[idx]]$ev_global <- NULL
    }

    results[[d_id]] <- list(
      metrics = ds_results,
      roc_plot = cv_roc_plot_base64
    )
  }

  # Map merged training dataset results back to original input dataset IDs
  input_ids <- sapply(datasets, function(x) x$id)
  for (d_id in input_ids) {
    if (is.null(results[[d_id]])) {
      for (res_id in names(results)) {
        if (grepl("merged_", res_id) && grepl("_training", res_id)) {
          results[[d_id]] <- results[[res_id]]
          break
        }
      }
    }
  }

  # Copy cv_curves and roc_cv files from merged to individual dataset paths
  for (d_id in input_ids) {
    for (res_id in names(results)) {
      if (grepl("merged_", res_id) && grepl("_training", res_id) && res_id != d_id) {
        for (ext in c("pdf", "png", "tiff")) {
          src_file <- get_session_path(res_id, sprintf("cv_curves_%%s.%s", ext))
          dest_file <- get_session_path(d_id, sprintf("cv_curves_%%s.%s", ext))
          if (file.exists(src_file)) {
            file.copy(src_file, dest_file, overwrite = TRUE)
          }
        }
        # Also copy CV ROC plots
        for (ext in c("pdf", "png", "tiff")) {
          src_roc <- get_session_path(res_id, sprintf("roc_cv_%%s.%s", ext))
          dest_roc <- get_session_path(d_id, sprintf("roc_cv_%%s.%s", ext))
          if (file.exists(src_roc)) {
            file.copy(src_roc, dest_roc, overwrite = TRUE)
          }
        }
      }
    }
  }

  return(results)
}

# ---------------------------------------------------------------
# 4. Independent Validation Testing Handler
# ---------------------------------------------------------------
run_testing <- function(models, datasets, multi_dataset_mode = "combine") {
  load_packages_globally(c("caret", "Boruta", "randomForest", "e1071", "glmnet", "MASS", "class", "ranger", "gbm", "pROC", "reticulate", "ggplot2"))
  cat(sprintf("[ML] Running Independent Validation Testing (multi_dataset_mode = %s)...\n", multi_dataset_mode))

  parsed_ml <- parse_models_payload(models, NULL)
  models <- parsed_ml$models

  cat(sprintf("[TEST] run_testing: models = %s\n", paste(models, collapse=", ")))
  cat(sprintf("[TEST] run_testing: datasets = %s\n", paste(sapply(datasets, function(x) x$id), collapse=", ")))

  results <- list()
  
  # 1. Classify datasets into train and test
  resolved <- resolve_and_preprocess_datasets(datasets, 0.7, multi_dataset_mode = multi_dataset_mode)
  train_dss <- resolved$train
  test_dss  <- resolved$test

  # Compute shared feature set in-memory for multi-dataset runs (no disk writes)
  shared_features <- compute_shared_features(datasets)

  # 2. Train models on each training dataset
  trained_models <- list()
  for (m in models) {
    trained_models[[m]] <- list()
  }
  
  # Store internal validation performance metrics for training datasets
  internal_validation_metrics <- list()
  for (d_id in names(train_dss)) {
    internal_validation_metrics[[d_id]] <- list()
  }

  for (d_id in names(train_dss)) {
    d <- train_dss[[d_id]]
    cat(sprintf("[TESTING] run_testing: trained on %s (purpose = %s, isInternalVal = %s)\n", d$id, d$fs_datasetPurpose, d$fs_isInternalValidation))
    ml_data <- tryCatch({
      prepare_ml_data(d, max_features = NULL, shared_features = shared_features)
    }, error = function(e) {
      cat(sprintf("[ERROR] prepare_ml_data failed for dataset %s: %s\n", d$id, e$message))
      NULL
    })
    
    if (is.null(ml_data)) {
      err_msg <- sprintf("Dataset '%s' could not be prepared for ML training. Check expression data and clinical group column.", d$name %||% d$id)
      cat(sprintf("[ERROR] %s\n", err_msg))
      for (m in models) {
        internal_validation_metrics[[d_id]][[m]] <- list(
          .error = err_msg,
          auc = 0, acc = 0, ppv = 0, npv = 0, prec = 0, rec = 0,
          fpr = c(0, 1), tpr = c(0, 1)
        )
      }
      next
    }
    
    df_all <- ml_data$df
    
    # Check if internal validation is enabled
    is_internal_val <- isTRUE(d$fs_isInternalValidation) || (!is.null(d$fs_datasetPurpose) && d$fs_datasetPurpose == "train-and-test")
    train_ratio <- d$fs_trainRatio %||% 0.7
    
    # Check if we have saved splits for this training dataset
    base_id <- get_base_id(d$id)
    train_split_path <- get_session_path(base_id, "%s_fs_train_split.rds")
    test_split_path  <- get_session_path(base_id, "%s_fs_test_split.rds")
    
    if (file.exists(train_split_path) && file.exists(test_split_path)) {
      cat(sprintf("  Loading stored train/test splits for %s...\n", d$name))
      train_df <- readRDS(train_split_path)
      if (is_internal_val) {
        val_df <- readRDS(test_split_path)
      } else {
        val_df <- NULL
      }
    } else {
      # Fallback to partitioning if no splits exist
      if (is_internal_val) {
        set.seed(42)
        train_idx <- caret::createDataPartition(df_all$Group, p = train_ratio, list = FALSE)
        train_df <- df_all[train_idx, , drop = FALSE]
        val_df   <- df_all[-train_idx, , drop = FALSE]
      } else {
        train_df <- df_all
        val_df   <- NULL
      }
    }
    
    # Load parameters if available
    params_path <- get_session_path(base_id, "%s_fs_parameters.rds")
    params <- if (file.exists(params_path)) readRDS(params_path) else NULL
    
    # Train each model type
    # ── Parallel model training (across models) ───────────────────────────────
    # fit_model_and_importance() re-seeds (set.seed(42)) on every fit, so training
    # models concurrently is bit-identical to the former serial loop. `test_train_one_model`
    # is a PURE function of `m`; the accumulator writes (trained_models /
    # internal_validation_metrics) are replayed by the master below in the ORIGINAL
    # model order.
    eff_cores <- get_effective_cores()
    for (mi in seq_along(models)) {
      m <- models[[mi]]
      cat(sprintf("  Training %s on %s (using %d cores)...\n", m, d$name, eff_cores))

      train_result <- tryCatch({
        # Subset to top features if they were selected and saved
        train_df_cv <- train_df
        val_df_cv   <- val_df
        top_feats_path <- get_session_path(base_id, sprintf("%%s_fs_top_features_%s.rds", m))
        fit <- NULL
        top_genes_orig <- character(0)
        if (file.exists(top_feats_path)) {
          top_genes_orig  <- readRDS(top_feats_path)
          top_genes_clean <- make.names(top_genes_orig)
          if (length(top_genes_orig) > 0) {
            sel_cols <- top_genes_clean[top_genes_clean %in% colnames(train_df)]
            train_df_cv <- train_df[, c(sel_cols, "Group"), drop = FALSE]
            if (!is.null(val_df)) {
              val_sel_cols <- top_genes_clean[top_genes_clean %in% colnames(val_df)]
              val_df_cv <- val_df[, c(val_sel_cols, "Group"), drop = FALSE]
            }
            cat(sprintf("  Model %s: Using top %d selected features for independent training/validation.\n", m, length(top_genes_orig)))
            
            final_model_path <- get_session_path(base_id, sprintf("%%s_fs_final_model_%s.rds", m))
            if (file.exists(final_model_path)) {
              cat(sprintf("  Model %s: Loading pre-fit final model for testing.\n", m))
              fit <- readRDS(final_model_path)
            } else {
              retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)
              fit_res <- fit_model_and_importance(retrain_model_name, train_df_cv, ml_data$original_genes, params, n_threads = eff_cores)
              fit     <- fit_res$model
            }
          }
        }

        if (is.null(fit)) {
          if (m == "stabl" && length(top_genes_orig) == 0) {
            cat("[STABL] No features selected. Proceeding with empty fit.\n")
          } else {
            stop(sprintf("Model training returned NULL for '%s' on dataset '%s'.", m, d$name))
          }
        }

        # If internal validation is enabled, evaluate on validation partition
        int_val_metrics <- NULL
        if (is_internal_val && !is.null(val_df_cv) && nrow(val_df_cv) > 0) {
          if (is.null(fit)) {
            int_val_metrics <- list(
              auc  = 0.0,
              acc  = 0.0,
              ppv  = 0.0,
              npv  = 0.0,
              prec = 0.0,
              rec  = 0.0,
              fpr  = numeric(0),
              tpr  = numeric(0),
              tp   = 0,
              fp   = 0,
              fn   = 0,
              tn   = 0
            )
          } else {
            val_df_cv$Group <- factor(as.character(val_df_cv$Group), levels = levels(train_df_cv$Group))
            common_feats    <- intersect(colnames(train_df_cv), colnames(val_df_cv))
            val_df_aligned  <- val_df_cv[, common_feats, drop = FALSE]
            pred_res <- predict_ml(m, fit, val_df_aligned)
            eval_res <- evaluate_predictions(pred_res$predictions, val_df_aligned$Group, pred_res$probabilities, target_prevalence = d$targetPrevalence)
            
            roc_tmp <- tryCatch({
              pROC::roc(response = val_df_aligned$Group, predictor = pred_res$probabilities, quiet = TRUE)
            }, error = function(e) NULL)

            b_res <- if (!is.null(roc_tmp)) {
              tryCatch(compute_bootstrap_roc(roc_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
            } else NULL

            ci_auc <- if (!is.null(b_res)) b_res$ci_auc else c(NA_real_, NA_real_)
            if (!is.null(roc_tmp) && !is.null(b_res)) {
              roc_tmp$ci_auc  <- b_res$ci_auc
              roc_tmp$band_df <- b_res$band_df
            }
            ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"

            int_val_metrics <- list(
              auc      = round(eval_res$auc, 3),
              ci       = ci_str,
              ci_lower = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
              ci_upper = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
              acc      = round(eval_res$bacc, 3),
              ppv      = round(eval_res$ppv %||% eval_res$prec, 3),
              npv      = round(eval_res$npv %||% eval_res$rec, 3),
              prec     = round(eval_res$ppv %||% eval_res$prec, 3),
              rec      = round(eval_res$npv %||% eval_res$rec, 3),
              fpr      = as.numeric(eval_res$fpr),
              tpr      = as.numeric(eval_res$tpr),
              tp       = eval_res$tp,
              fp       = eval_res$fp,
              fn       = eval_res$fn,
              tn       = eval_res$tn,
              prob     = pred_res$probabilities,
              truth    = val_df_aligned$Group,
              roc_obj  = roc_tmp
            )
          }
        }

        list(ok = TRUE, fit = fit, int_val_metrics = int_val_metrics)

      }, error = function(e) {
        err_msg <- conditionMessage(e)
        cat(sprintf("[ERROR] Testing: model training '%s' on '%s' failed: %s\n", m, d$name, err_msg))
        list(ok = FALSE, message = err_msg)
      })

      if (!isTRUE(train_result$ok)) {
        # Store error as a structured entry for the frontend to surface as toast
        internal_validation_metrics[[d_id]][[m]] <- list(
          .error = sprintf("Training '%s' failed on '%s': %s", m, d$name, train_result$message),
          .model = m, .stage = "testing_train",
          auc = 0, acc = 0, ppv = 0, npv = 0, prec = 0, rec = 0,
          fpr = c(0, 1), tpr = c(0, 1)
        )
        next
      }

      trained_models[[m]][[d_id]] <- train_result$fit
      if (!is.null(train_result$int_val_metrics)) {
        internal_validation_metrics[[d_id]][[m]] <- train_result$int_val_metrics
      }
    }
  }

  # 3. Evaluate on test datasets
  # Protocol: Refit model on the test dataset's train split (70%) with the selected feature signature from D_train,
  # and evaluate on the test dataset's held-out test split (30%).
  testing_metrics <- list()
  for (t_id in names(test_dss)) {
    testing_metrics[[t_id]] <- list()
  }

  for (t_id in names(test_dss)) {
    t <- test_dss[[t_id]]
    t_type <- get_dataset_data_type(t)
    t_hier <- get_hierarchy_group(t_type)
    cat(sprintf("[TESTING] run_testing: testing on test dataset %s (type = %s, hierarchy = %s)\n", t$id, t_type, t_hier))
    
    test_ml <- tryCatch({
      prepare_ml_data(t, max_features = NULL, shared_features = shared_features)
    }, error = function(e) {
      cat(sprintf("[ERROR] prepare_ml_data failed for test dataset %s: %s\n", t$id, e$message))
      NULL
    })
    
    if (is.null(test_ml)) {
      err_msg <- sprintf("Test dataset '%s' could not be prepared. Check clinical annotations.", t$name %||% t$id)
      cat(sprintf("[ERROR] %s\n", err_msg))
      for (m in models) {
        testing_metrics[[t_id]][[m]] <- list(
          .error = err_msg,
          auc = 0, acc = 0, ppv = 0, npv = 0, prec = 0, rec = 0,
          fpr = c(0, 1), tpr = c(0, 1)
        )
      }
      next
    }
    
    test_df <- test_ml$df
    t_val_strat <- t$fs_validationStrategy %||% "train-test-split"
    t_train_ratio <- t$fs_trainRatio %||% 0.7
    
    # Check if testing dataset is evaluated using CV or Split
    is_test_cv <- (t_val_strat == "cv-only")
    n_samples_t <- nrow(test_df)
    
    if (is_test_cv) {
      is_loocv_t <- (n_samples_t < 50)
      if (is_loocv_t) {
        cv_folds_t <- lapply(1:n_samples_t, function(i) i)
      } else {
        cv_folds_t <- caret::createFolds(test_df$Group, k = 5, list = TRUE, returnTrain = FALSE)
      }
      t_train_df <- NULL
      t_test_df  <- test_df
    } else {
      # Split test dataset into train (e.g. 70%) and test (e.g. 30%) for refitting & evaluating
      set.seed(42)
      if (n_samples_t >= 4 && length(unique(test_df$Group)) > 1) {
        t_train_idx <- tryCatch(caret::createDataPartition(test_df$Group, p = t_train_ratio, list = FALSE), error = function(e) seq_len(n_samples_t))
        t_train_df <- test_df[t_train_idx, , drop = FALSE]
        t_test_df  <- test_df[-t_train_idx, , drop = FALSE]
        if (nrow(t_test_df) == 0) t_test_df <- t_train_df
      } else {
        t_train_df <- test_df
        t_test_df  <- test_df
      }
    }
    
    for (m in models) {
      cat(sprintf("  Evaluating %s on test dataset %s...\n", m, t$name))

      # ── Per-model tryCatch: never silently ignore prediction failures ─────────
      test_result <- tryCatch({

        probs_list   <- list()
        train_levels <- NULL

        for (d_id in names(train_dss)) {
          d_train <- train_dss[[d_id]]
          d_train_type <- get_dataset_data_type(d_train)
          d_train_hier <- get_hierarchy_group(d_train_type)
          
          # Cross-hierarchy check: Disallow transcriptomics <-> proteomics
          if (d_train_hier != t_hier) {
            cat(sprintf("[TESTING] Skipping cross-hierarchy testing between %s (%s) and %s (%s)\n", d_id, d_train_hier, t_id, t_hier))
            next
          }

          base_id_train   <- get_base_id(d_train$id)
          top_feats_path  <- get_session_path(base_id_train, sprintf("%%s_fs_top_features_%s.rds", m))
          params_path_t   <- get_session_path(base_id_train, "%s_fs_parameters.rds")
          params          <- if (file.exists(params_path_t)) readRDS(params_path_t) else NULL
          
          if (file.exists(top_feats_path)) {
            top_genes_orig  <- readRDS(top_feats_path)
            top_genes_clean <- make.names(top_genes_orig)
            
            # Use test_df columns for intersection check
            sel_cols <- top_genes_clean[top_genes_clean %in% colnames(test_df)]
            
            if (length(sel_cols) > 0) {
              if (is_test_cv) {
                # Run Cross-Validation refitting on test_df in parallel
                all_probs_t <- numeric(n_samples_t)
                
                eff_c <- get_effective_cores()
                process_fold_t <- function(f_idx) {
                  val_idx <- cv_folds_t[[f_idx]]
                  cv_train_t <- test_df[-val_idx, , drop = FALSE]
                  cv_val_t   <- test_df[val_idx, , drop = FALSE]
                  
                  t_train_sub <- cv_train_t[, c(sel_cols, "Group"), drop = FALSE]
                  t_val_sub   <- cv_val_t[, c(sel_cols, "Group"), drop = FALSE]
                  
                  retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)
                  fit_res <- fit_model_and_importance(retrain_model_name, t_train_sub, test_ml$original_genes, params, n_threads = eff_c)
                  fit_refit <- fit_res$model
                  
                  if (!is.null(fit_refit)) {
                    curr_levels <- levels(t_train_sub$Group)
                    t_val_sub$Group <- factor(as.character(t_val_sub$Group), levels = curr_levels)
                    pred_res <- predict_ml(m, fit_refit, t_val_sub)
                    return(list(val_idx = val_idx, probs = pred_res$probabilities, curr_levels = curr_levels))
                  }
                  return(list(val_idx = val_idx, probs = rep(0.5, length(val_idx)), curr_levels = NULL))
                }
                
                fold_outputs_t <- lapply(seq_along(cv_folds_t), process_fold_t)
                
                for (fo in fold_outputs_t) {
                  all_probs_t[fo$val_idx] <- fo$probs
                  if (is.null(train_levels) && !is.null(fo$curr_levels)) train_levels <- fo$curr_levels
                }
                probs_list[[d_id]] <- all_probs_t
              } else {
                # Standard train-test split refitting
                t_train_sub <- t_train_df[, c(sel_cols, "Group"), drop = FALSE]
                t_test_sub  <- t_test_df[, c(sel_cols, "Group"), drop = FALSE]
                
                base_id_t <- get_base_id(t$id)
                final_model_path_t <- get_session_path(base_id_t, sprintf("%%s_fs_final_model_%s.rds", m))
                fit_refit <- NULL
                if (file.exists(final_model_path_t)) {
                  fit_refit <- readRDS(final_model_path_t)
                }
                
                if (is.null(fit_refit)) {
                  retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)
                  fit_res <- fit_model_and_importance(retrain_model_name, t_train_sub, test_ml$original_genes, params, n_threads = eff_c)
                  fit_refit <- fit_res$model
                }
                
                if (!is.null(fit_refit)) {
                  curr_levels <- levels(t_train_sub$Group)
                  if (is.null(train_levels)) train_levels <- curr_levels
                  t_test_sub$Group <- factor(as.character(t_test_sub$Group), levels = curr_levels)
                  pred_res <- predict_ml(m, fit_refit, t_test_sub)
                  probs_list[[d_id]] <- pred_res$probabilities
                }
              }
            }
          }
          
          # If refitting not possible, fall back to direct prediction if fitted model available
          if (is.null(probs_list[[d_id]])) {
            fit <- trained_models[[m]][[d_id]]
            if (!is.null(fit)) {
              curr_levels <- attr(fit, "orig_levels") %||% fit$levels
              if (is.null(train_levels)) train_levels <- curr_levels
              test_df_aligned <- t_test_df
              test_df_aligned$Group <- factor(as.character(test_df_aligned$Group), levels = curr_levels)
              pred_res <- predict_ml(m, fit, test_df_aligned)
              probs_list[[d_id]] <- pred_res$probabilities
            }
          }
        }

        if (length(probs_list) == 0) {
          if (m == "stabl") {
            eval_res <- list(auc = 0.0, acc = 0.0, bacc = 0.0, ppv = 0.0, npv = 0.0, prec = 0.0, rec = 0.0, fpr = numeric(0), tpr = numeric(0), tp = 0, fp = 0, fn = 0, tn = 0)
            list(ok = TRUE, eval_res = eval_res)
          } else {
            stop(sprintf("No compatible trained/refitted models available for '%s' on test dataset '%s'.", m, t$name))
          }
        } else {
          avg_probs <- rowMeans(do.call(cbind, probs_list), na.rm = TRUE)

          if (is.null(train_levels) || length(train_levels) < 2) {
            train_levels <- levels(t_test_df$Group)
          }

          pred_classes <- factor(ifelse(avg_probs >= 0.5, train_levels[2], train_levels[1]), levels = train_levels)
          actual_group <- factor(as.character(t_test_df$Group), levels = train_levels)
          eval_res     <- evaluate_predictions(pred_classes, actual_group, avg_probs, target_prevalence = t$targetPrevalence)

          list(
            ok       = TRUE,
            eval_res = eval_res,
            prob     = avg_probs,
            truth    = actual_group
          )
        }

      }, error = function(e) {
        err_msg <- conditionMessage(e)
        cat(sprintf("[ERROR] Testing prediction '%s' on test dataset '%s' failed: %s\n", m, t$name, err_msg))
        list(ok = FALSE, message = err_msg)
      })

      if (!isTRUE(test_result$ok)) {
        testing_metrics[[t_id]][[m]] <- list(
          .error = sprintf("Testing '%s' on '%s' failed: %s", m, t$name, test_result$message),
          .model = m, .stage = "testing_predict",
          auc = 0, acc = 0, ppv = 0, npv = 0, prec = 0, rec = 0,
          fpr = c(0, 1), tpr = c(0, 1)
        )
        next
      }

      eval_res <- test_result$eval_res
      roc_tmp <- tryCatch({
        pROC::roc(response = test_result$truth, predictor = test_result$prob, quiet = TRUE)
      }, error = function(e) NULL)

      b_res <- if (!is.null(roc_tmp)) {
        tryCatch(compute_bootstrap_roc(roc_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
      } else NULL

      ci_auc <- if (!is.null(b_res)) b_res$ci_auc else c(NA_real_, NA_real_)
      if (!is.null(roc_tmp) && !is.null(b_res)) {
        roc_tmp$ci_auc  <- b_res$ci_auc
        roc_tmp$band_df <- b_res$band_df
      }
      ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"

      testing_metrics[[t_id]][[m]] <- list(
        auc      = round(eval_res$auc, 3),
        ci       = ci_str,
        ci_lower = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
        ci_upper = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
        acc      = round(eval_res$bacc, 3),
        ppv      = round(eval_res$ppv %||% eval_res$prec, 3),
        npv      = round(eval_res$npv %||% eval_res$rec, 3),
        prec     = round(eval_res$ppv %||% eval_res$prec, 3),
        rec      = round(eval_res$npv %||% eval_res$rec, 3),
        fpr      = as.numeric(eval_res$fpr),
        tpr      = as.numeric(eval_res$tpr),
        tp       = eval_res$tp,
        fp       = eval_res$fp,
        fn       = eval_res$fn,
        tn       = eval_res$tn,
        prob     = test_result$prob,
        truth    = test_result$truth,
        roc_obj  = roc_tmp
      )
    }
  }

  # 4. Generate ROC Plots and compile final results per dataset
  # Use all datasets that were sent (train + test), so every dataset gets a result
  all_eval_ids <- unique(c(names(test_dss), names(train_dss)))
  cat("[ML] Making ROC Curve...\n")
  for (ds_id in all_eval_ids) {
    # Prefer test-set metrics if available; fall back to internal validation
    if (ds_id %in% names(test_dss) && length(testing_metrics[[ds_id]]) > 0) {
      ds_metrics <- testing_metrics[[ds_id]]
    } else if (ds_id %in% names(train_dss)) {
      ds_metrics <- internal_validation_metrics[[ds_id]]
      if (is.null(ds_metrics)) ds_metrics <- list()
    } else {
      next
    }
    
    roc_plot_base64 <- ""
    standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
    valid_models <- intersect(standard_order, names(ds_metrics))
    valid_models <- Filter(function(m) !is.null(ds_metrics[[m]]) && !is.null(ds_metrics[[m]]$prob) && !is.null(ds_metrics[[m]]$truth), valid_models)
    
    if (length(valid_models) > 0) {
      roc_raw_data_mapped <- lapply(valid_models, function(m) {
        ds_metrics[[m]]$roc_obj %||% tryCatch({
          pROC::roc(response = ds_metrics[[m]]$truth, predictor = ds_metrics[[m]]$prob, quiet = TRUE)
        }, error = function(e) NULL)
      })
      names(roc_raw_data_mapped) <- sapply(valid_models, function(m) MODEL_LABELS_MAP[[m]] %||% m)
      roc_raw_data_mapped <- Filter(Negate(is.null), roc_raw_data_mapped)
      
      if (length(roc_raw_data_mapped) > 0) {
        p_roc <- plot_roc_multi(roc_raw_data_mapped, title = NULL, show_auc_in_legend = FALSE, bands = TRUE, band = "ci", n_boot = 150) +
        ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.key.spacing.y = grid::unit(0.5, "cm")) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
        
        tryCatch({
          roc_test_pdf <- get_session_path(ds_id, "roc_test_%s.pdf")
          ggplot2::ggsave(filename = roc_test_pdf, plot = p_roc, width = 10, height = 7)
          ggplot2::ggsave(filename = get_session_path(ds_id, "roc_test_%s.png"), plot = p_roc, width = 10, height = 7, dpi = 300)
          ggplot2::ggsave(filename = get_session_path(ds_id, "roc_test_%s.tiff"), plot = p_roc, width = 10, height = 7, dpi = 300)
          register_export_file(get_user_id(ds_id), "roc_test_all", ds_id, roc_test_pdf, "fs", "testing", ext = "pdf")
        }, error = function(e) {
          cat("[WARNING] Failed to save ROC plot:", e$message, "\n")
        })

        for (m in valid_models) {
          roc_obj <- ds_metrics[[m]]$roc_obj %||% tryCatch(
            pROC::roc(response  = ds_metrics[[m]]$truth,
                      predictor = ds_metrics[[m]]$prob,
                      quiet     = TRUE),
            error = function(e) NULL
          )
          if (is.null(roc_obj)) next
          model_label <- MODEL_LABELS_MAP[[m]] %||% m
          tryCatch({
            p_roc_single <- plot_roc(roc_obj,
                                     title = sprintf("%s — Test", model_label),
                                     band  = "ci", n_boot = 150, show_folds = FALSE)
            m_test_roc_pdf <- get_session_path(ds_id, sprintf("%%s_roc_test_%s.pdf", m))
            save_roc(p_roc_single,
                     m_test_roc_pdf,
                     width = 8, height = 7, dpi = 300)
            save_roc(p_roc_single,
                     get_session_path(ds_id, sprintf("%%s_roc_test_%s.png", m)),
                     width = 8, height = 7, dpi = 300)
            register_export_file(get_user_id(ds_id), "roc_test_model", ds_id, m_test_roc_pdf, "fs", "testing", model = m, ext = "pdf")
            rm(p_roc_single, roc_obj)
          }, error = function(e) {
            cat(sprintf("[WARNING] Per-model test ROC (%s): %s\n", m, e$message))
          })
        }
        
        roc_plot_base64 <- tryCatch(plot_to_base64(p_roc, width = 930, height = 650), error = function(e) {
          cat("[WARNING] plot_to_base64 failed:", e$message, "\n")
          ""
        })
        rm(roc_raw_data_mapped, p_roc)
        invisible(gc(verbose = FALSE))
      }
    }
    
    # Save and register all_models_performance_test CSV and per-model confusion_matrix
    tryCatch({
      uid_val <- get_user_id(ds_id)
      models_in_metrics <- intersect(standard_order, names(ds_metrics))
      if (length(models_in_metrics) > 0) {
        test_df <- data.frame(
          Model = sapply(models_in_metrics, function(m) MODEL_LABELS_MAP[[m]] %||% toupper(m)),
          AUC = sapply(models_in_metrics, function(m) ds_metrics[[m]]$auc %||% NA),
          AUC_95_CI = sapply(models_in_metrics, function(m) ds_metrics[[m]]$ci %||% NA),
          Accuracy = sapply(models_in_metrics, function(m) ds_metrics[[m]]$acc %||% NA),
          PPV = sapply(models_in_metrics, function(m) ds_metrics[[m]]$ppv %||% NA),
          NPV = sapply(models_in_metrics, function(m) ds_metrics[[m]]$npv %||% NA),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        colnames(test_df) <- c("Model", "AUC", "95% CI (AUC)", "Accuracy", "PPV", "NPV")
        test_csv_path <- get_session_path(ds_id, "%s_all_models_performance_test.csv")
        write.csv(test_df, test_csv_path, row.names = FALSE)
        register_export_file(uid_val, "testing_results", ds_id, test_csv_path, "fs", "testing")
        
        # Per-model confusion matrix
        for (m in models_in_metrics) {
          met <- ds_metrics[[m]]
          if (!is.null(met) && !is.null(met$tp)) {
            cm_df <- data.frame(
              Prediction = c("Predicted Positive", "Predicted Negative"),
              Actual_Positive = c(met$tp, met$fn),
              Actual_Negative = c(met$fp, met$tn),
              stringsAsFactors = FALSE
            )
            cm_path <- get_session_path(ds_id, sprintf("%%s_confusion_matrix_%s.csv", m))
            write.csv(cm_df, cm_path, row.names = FALSE)
            register_export_file(uid_val, "confusion_matrix", ds_id, cm_path, "fs", "testing", model = m)
          }
        }
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save testing performance/confusion matrix for %s: %s\n", ds_id, e$message))
    })

    # Always emit roc_plot so the frontend always gets the field
    ds_metrics$roc_plot <- roc_plot_base64

    # Clean up raw prediction arrays before returning JSON
    for (m in intersect(standard_order, names(ds_metrics))) {
      if (is.list(ds_metrics[[m]])) {
        ds_metrics[[m]]$prob <- NULL
        ds_metrics[[m]]$truth <- NULL
      }
    }

    # Only add to results if we have at least one model entry or a roc_plot
    if (length(ds_metrics) > 0) {
      results[[ds_id]] <- ds_metrics
    }
  }

  # Map merged training dataset results back to original input dataset IDs
  input_ids <- sapply(datasets, function(x) x$id)
  for (d_id in input_ids) {
    if (is.null(results[[d_id]])) {
      for (res_id in names(results)) {
        if (grepl("merged_", res_id) && grepl("_training", res_id)) {
          results[[d_id]] <- results[[res_id]]
          # Also copy ROC curves
          for (ext in c("pdf", "png", "tiff")) {
            src_roc <- get_session_path(res_id, sprintf("roc_test_%%s.%s", ext))
            dest_roc <- get_session_path(d_id, sprintf("roc_test_%%s.%s", ext))
            if (file.exists(src_roc)) {
              file.copy(src_roc, dest_roc, overwrite = TRUE)
            }
          }
          break
        }
      }
    }
  }

  # Safety: if results is still empty but we have internal validation, include them
  if (length(results) == 0 && length(internal_validation_metrics) > 0) {
    cat("[WARNING] run_testing: no results compiled — falling back to internal validation metrics.\n")
    for (ds_id in names(internal_validation_metrics)) {
      ds_metrics <- internal_validation_metrics[[ds_id]]
      if (!is.null(ds_metrics) && length(ds_metrics) > 0) {
        ds_metrics$roc_plot <- ""
        results[[ds_id]] <- ds_metrics
      }
    }
  }

  cat(sprintf("[ML] run_testing complete: %d dataset result(s) compiled.\n", length(results)))
  return(results)
}

# -----------------------------------------------------------------
# Geometric/Package Elbow & Refit Feature Helper Functions
# -----------------------------------------------------------------

find_elbow_point <- function(importances) {
  n <- length(importances)
  if (n <= 2) return(n)
  
  if (requireNamespace("segmented", quietly = TRUE)) {
    res <- tryCatch({
      x <- 1:n
      y <- importances
      df <- data.frame(x = x, y = y)
      fit <- lm(y ~ x, data = df)
      # Fit a segmented model with Z=x and initial guess psi at median index
      fit_seg <- segmented::segmented(fit, seg.Z = ~x, psi = round(n / 2))
      breakpoint <- fit_seg$psi[, "Est."]
      
      if (!is.null(breakpoint) && length(breakpoint) > 0) {
        idx <- round(breakpoint[1])
        if (!is.na(idx) && idx >= 1 && idx <= n) {
          return(idx)
        }
      }
      NULL
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(res)) {
      return(res)
    }
  }
  
  # Fallback to distance-from-diagonal geometric method
  tryCatch({
    x <- 1:n
    y <- importances
    a <- y[n] - y[1]
    b <- -(n - 1)
    c <- (n - 1) * y[1] - (y[n] - y[1]) * 1
    
    distances <- abs(a * x + b * y + c) / sqrt(a^2 + b^2)
    return(which.max(distances))
  }, error = function(e) {
    return(n)
  })
}

select_by_percentage <- function(importances, pct) {
  sum_imp <- sum(importances)
  if (sum_imp == 0) return(length(importances))
  cum_sum <- cumsum(importances)
  idx <- which(cum_sum >= (pct / 100) * sum_imp)[1]
  if (is.na(idx)) return(length(importances))
  return(idx)
}

run_refit_features <- function(datasets, models, split_ratio, parameters, selection_method, percentage_val = NULL, max_features_val = NULL) {
  load_packages_globally(c("caret", "Boruta", "randomForest", "e1071", "glmnet", "MASS", "class", "ranger", "gbm", "pROC", "reticulate", "ggplot2"))
  
  log_info <- switch(selection_method,
    "breakoff"     = "method = breakoff",
    "overlap"      = "method = overlap",
    "percentage"   = sprintf("method = percentage, pct = %s", if (!is.null(percentage_val) && !is.na(as.numeric(percentage_val))) as.numeric(percentage_val) else 80),
    "max_features" = sprintf("method = max_features, max = %s", if (!is.null(max_features_val) && !is.na(as.numeric(max_features_val))) as.numeric(max_features_val) else 10),
    sprintf("method = %s", selection_method)
  )
  cat(sprintf("[ML] Refitting Features (%s)...\n", log_info))
  
  parsed_ml <- parse_models_payload(models, parameters)
  models <- parsed_ml$models
  parameters <- parsed_ml$parameters
  
  resolved <- resolve_and_preprocess_datasets(datasets, split_ratio)
  train_dss <- resolved$train
  test_dss  <- resolved$test
  
  results <- list()
  
  for (d_id in names(train_dss)) {
    d <- train_dss[[d_id]]
    base_id <- get_base_id(d$id)
    
    # Load splits
    train_split_path <- get_session_path(base_id, "%s_fs_train_split.rds")
    test_split_path  <- get_session_path(base_id, "%s_fs_test_split.rds")
    if (!file.exists(train_split_path)) next
    
    train_df <- readRDS(train_split_path)
    test_df  <- readRDS(test_split_path)
    saveRDS(parameters, get_session_path(base_id, "%s_fs_parameters.rds"))
    
    # Pre-calculate overlap features if overlap selection is selected
    overlap_genes_all <- character(0)
    if (selection_method == "overlap" && length(models) > 1) {
      model_sets <- list()
      for (mi in models) {
        importances_path <- get_session_path(base_id, sprintf("%%s_fs_full_importances_%s.rds", mi))
        if (file.exists(importances_path)) {
          model_feats_mi <- readRDS(importances_path)
          mi_imps <- sapply(model_feats_mi, function(f) f$importance)
          mi_genes <- sapply(model_feats_mi, function(f) f$gene)
          if (mi %in% c("boruta", "stabl")) {
            model_sets[[mi]] <- mi_genes
          } else {
            n_el <- find_elbow_point(mi_imps)
            model_sets[[mi]] <- if (n_el > 0) mi_genes[1:n_el] else character(0)
          }
        }
      }
      overlap_genes_all <- Reduce(intersect, model_sets)
    }

    accuracies          <- list()
    loss_histories      <- list()
    roc_data            <- list()
    roc_raw_data        <- list()
    performance_metrics <- list()
    job_warnings        <- character(0)
    selected_features_out <- list()
    
    # ── Parallel per-model refit ──────────────────────────────────────────────
    # Each model's refit is deterministic and independent: fit_model_and_importance()
    # calls set.seed(42) at the start of every fit (including immediately before the
    # Parallel per-model refit is disabled; running refit sequentially to ensure
    # stability and proper resource management.
    eff_cores <- get_effective_cores()
    for (mi in seq_along(models)) {
      m  <- models[[mi]]
      cat(sprintf("  Refitting model: %s on %s (using %d cores)...\n", m, d$name, eff_cores))

      importances_path <- get_session_path(base_id, sprintf("%%s_fs_full_importances_%s.rds", m))
      if (!file.exists(importances_path)) next

      model_feats <- readRDS(importances_path)

      # Select features
      importances <- sapply(model_feats, function(f) f$importance)
      genes <- sapply(model_feats, function(f) f$gene)

      n_features <- length(importances)
      is_boruta_stabl <- m %in% c("boruta", "stabl")

      if (selection_method == "overlap") {
        top_genes_orig <- genes[genes %in% overlap_genes_all]
        if (length(top_genes_orig) == 0) {
          if (m == "stabl") {
            top_genes_orig <- character(0)
          } else {
            top_genes_orig <- genes[1]
          }
        }
        n_features <- length(top_genes_orig)
      } else {
        if (!is_boruta_stabl && n_features > 0) {
          if (selection_method %in% c("elbow", "breakoff")) {
            n_features <- find_elbow_point(importances)
          } else if (selection_method == "percentage") {
            pct_use <- if (!is.null(percentage_val) && !is.na(as.numeric(percentage_val))) as.numeric(percentage_val) else 80
            n_features <- select_by_percentage(importances, pct_use)
          } else if (selection_method == "max_features") {
            max_use <- if (!is.null(max_features_val) && !is.na(as.numeric(max_features_val))) as.numeric(max_features_val) else 10
            n_features <- min(max_use, length(importances))
          }
        }
        if (n_features < 1) {
          if (m == "stabl") {
            n_features <- 0
          } else {
            n_features <- 1
          }
        }
        if (n_features == 0) {
          top_genes_orig <- character(0)
        } else {
          top_genes_orig <- genes[1:n_features]
        }
      }
      top_genes_clean <- make.names(top_genes_orig)

      # Save top features (Crucial for CV/Testing)
      saveRDS(top_genes_orig, get_session_path(base_id, sprintf("%%s_fs_top_features_%s.rds", m)))

      if (length(top_genes_orig) == 0) {
        accuracies[[m]]     <- 0.0
        roc_data[[m]]       <- list(fpr = numeric(0), tpr = numeric(0), auc = 0.0)
        performance_metrics[[m]] <- list(
          auc  = 0.0,
          acc  = 0.0,
          prec = 0.0,
          rec  = 0.0,
          fpr  = numeric(0),
          tpr  = numeric(0),
          tp   = 0,
          fp   = 0,
          fn   = 0,
          tn   = 0
        )
        selected_features_out[[m]] <- list()
        next
      }

      # Retrain model on selected features
      sel_cols_tr   <- top_genes_clean[top_genes_clean %in% colnames(train_df)]
      sel_cols_te   <- top_genes_clean[top_genes_clean %in% colnames(test_df)]
      train_df_eval <- train_df[, c(sel_cols_tr, "Group"), drop = FALSE]
      test_df_eval  <- test_df[, c(sel_cols_te, "Group"), drop = FALSE]

      gene_mapping <- top_genes_orig
      names(gene_mapping) <- top_genes_clean

      retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)

      res_retrain <- tryCatch(
        fit_model_and_importance(retrain_model_name, train_df_eval, gene_mapping, parameters, n_threads = eff_cores),
        error = function(e) {
          stop(sprintf("Re-training %s with selected features failed: %s", m, e$message))
        }
      )

      fit <- res_retrain$model
      if (is.null(fit)) stop(sprintf("Re-training '%s' returned NULL model.", m))

      # Save final fit model
      saveRDS(fit, get_session_path(base_id, sprintf("%%s_fs_final_model_%s.rds", m)))

      # Predict & Evaluate
      pred_res <- predict_ml(retrain_model_name, fit, train_df_eval)
      eval_res <- evaluate_predictions(pred_res$predictions, train_df_eval$Group, pred_res$probabilities, target_prevalence = d$targetPrevalence)

      # CV folds score
      fold_scores <- tryCatch({
        k_folds  <- min(5, nrow(train_df_eval))
        fold_idx <- split(sample(seq_len(nrow(train_df_eval))), rep(1:k_folds, length.out = nrow(train_df_eval)))

        cv_fold_func <- function(vi) {
          cv_train <- train_df_eval[-vi, , drop = FALSE]
          cv_val   <- train_df_eval[vi, , drop = FALSE]
          if (nrow(cv_train) < 2 || nrow(cv_val) < 1) return(eval_res$bacc)
          cv_res  <- fit_model_and_importance(retrain_model_name, cv_train, gene_mapping, parameters, n_threads = eff_cores)
          cv_pred <- predict_ml(retrain_model_name, cv_res$model, cv_val)
          cv_eval <- evaluate_predictions(cv_pred$predictions, cv_val$Group, cv_pred$probabilities, target_prevalence = d$targetPrevalence)
          cv_eval$bacc
        }

        sapply(fold_idx, function(vi) {
          tryCatch(cv_fold_func(vi), error = function(e) eval_res$bacc)
        })
      }, error = function(e) rep(eval_res$bacc, 5))

      accuracies[[m]]     <- round(eval_res$bacc, 3)
      roc_data[[m]]       <- list(fpr = as.numeric(eval_res$fpr), tpr = as.numeric(eval_res$tpr), auc = round(eval_res$auc, 3))
      
      roc_obj_tmp <- tryCatch({
        pROC::roc(response = train_df_eval$Group, predictor = pred_res$probabilities, quiet = TRUE)
      }, error = function(e) NULL)

      b_res <- if (!is.null(roc_obj_tmp)) {
        tryCatch(compute_bootstrap_roc(roc_obj_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
      } else NULL

      ci_auc <- if (!is.null(b_res)) b_res$ci_auc else c(NA_real_, NA_real_)
      if (!is.null(roc_obj_tmp) && !is.null(b_res)) {
        roc_obj_tmp$ci_auc  <- b_res$ci_auc
        roc_obj_tmp$band_df <- b_res$band_df
      }
      roc_raw_data[[m]] <- roc_obj_tmp
      ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"

      performance_metrics[[m]] <- list(
        auc      = round(eval_res$auc, 3),
        ci       = ci_str,
        ci_lower = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
        ci_upper = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
        acc      = round(eval_res$bacc, 3),
        ppv      = round(eval_res$ppv %||% eval_res$prec, 3),
        npv      = round(eval_res$npv %||% eval_res$rec, 3),
        prec     = round(eval_res$ppv %||% eval_res$prec, 3),
        rec      = round(eval_res$npv %||% eval_res$rec, 3),
        fpr      = as.numeric(eval_res$fpr),
        tpr      = as.numeric(eval_res$tpr),
        tp       = eval_res$tp,
        fp       = eval_res$fp,
        fn       = eval_res$fn,
        tn       = eval_res$tn
      )

      loss_histories[[m]] <- as.numeric(fold_scores)

      # Keep subset features list for response
      selected_feats_model <- model_feats[1:n_features]
      for (i in seq_along(selected_feats_model)) {
        selected_feats_model[[i]]$rank <- i
      }
      if (length(selected_features_out) == 0) {
        selected_features_out <- selected_feats_model
      }
    }

    # Generate ROC plot
    roc_plot_base64 <- ""
    standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
    valid_raw_models <- intersect(standard_order, intersect(names(roc_raw_data), models))
    valid_raw_models <- Filter(function(m) !is.null(roc_raw_data[[m]]), valid_raw_models)
    if (length(valid_raw_models) > 0) {
      roc_raw_data_mapped <- roc_raw_data[valid_raw_models]
      names(roc_raw_data_mapped) <- sapply(names(roc_raw_data_mapped), function(m) MODEL_LABELS_MAP[[m]] %||% m)
      p_roc <- plot_roc_multi(roc_raw_data_mapped, title = NULL, show_auc_in_legend = FALSE, bands = TRUE, band = "ci", n_boot = 150) +
        ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.key.spacing.y = grid::unit(0.5, "cm")) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
        
      tryCatch({
        roc_train_pdf <- get_session_path(d$id, "roc_train_%s.pdf")
        ggplot2::ggsave(filename = roc_train_pdf, plot = p_roc, width = 10, height = 7)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_train_%s.png"), plot = p_roc, width = 10, height = 7, dpi = 300)
        ggplot2::ggsave(filename = get_session_path(d$id, "roc_train_%s.tiff"), plot = p_roc, width = 10, height = 7, dpi = 300)
        register_export_file(get_user_id(d$id), "roc_all_models_train", d$id, roc_train_pdf, "fs", "refit", ext = "pdf")
      }, error = function(e) {
        cat("[WARNING] Failed to save training ROC plot:", e$message, "\n")
      })
      
      for (m in valid_raw_models) {
        roc_obj <- roc_raw_data[[m]]
        if (is.null(roc_obj)) next
        model_label <- MODEL_LABELS_MAP[[m]] %||% m
        tryCatch({
          p_roc_single <- plot_roc(roc_obj,
                                   title = sprintf("%s — Refit", model_label),
                                   band = "ci", n_boot = 150, show_folds = FALSE)
          m_refit_pdf <- get_session_path(d$id, sprintf("%%s_roc_train_%s.pdf", m))
          save_roc(p_roc_single,
                   m_refit_pdf,
                   width = 8, height = 7, dpi = 300)
          save_roc(p_roc_single,
                   get_session_path(d$id, sprintf("%%s_roc_train_%s.png", m)),
                   width = 8, height = 7, dpi = 300)
          register_export_file(get_user_id(d$id), "roc_train_model", d$id, m_refit_pdf, "fs", "refit", model = m, ext = "pdf")
          rm(p_roc_single, roc_obj)
        }, error = function(e) {
          cat(sprintf("[WARNING] Per-model refit ROC (%s): %s\n", m, e$message))
        })
      }
      
      roc_plot_base64 <- tryCatch(plot_to_base64(p_roc, width = 930, height = 650), error = function(e) "")
      rm(roc_raw_data_mapped, p_roc)
      invisible(gc(verbose = FALSE))
    }
    
    # Save and register all_models_performance_train and selected_features for refit
    tryCatch({
      uid_val <- get_user_id(base_id)
      if (length(performance_metrics) > 0) {
        p_models <- names(performance_metrics)
        perf_df <- data.frame(
          Model = sapply(p_models, function(m) MODEL_LABELS_MAP[[m]] %||% toupper(m)),
          AUC = sapply(p_models, function(m) performance_metrics[[m]]$auc %||% NA),
          AUC_95_CI = sapply(p_models, function(m) performance_metrics[[m]]$ci %||% NA),
          Balanced_Accuracy = sapply(p_models, function(m) performance_metrics[[m]]$acc %||% NA),
          PPV = sapply(p_models, function(m) performance_metrics[[m]]$ppv %||% NA),
          NPV = sapply(p_models, function(m) performance_metrics[[m]]$npv %||% NA),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        colnames(perf_df) <- c("Model", "AUC", "95% CI (AUC)", "Accuracy", "PPV", "NPV")
        canonical_base_id <- get_base_id(base_id)
        perf_train_path <- get_session_path(canonical_base_id, "%s_all_models_performance_train.csv")
        write.csv(perf_df, perf_train_path, row.names = FALSE)
        register_export_file(uid_val, "all_models_performance_train", canonical_base_id, perf_train_path, "fs", "refit")
      }

      canonical_base_id <- get_base_id(base_id)
      for (m in models) {
        top_path <- get_session_path(canonical_base_id, sprintf("%%s_fs_top_features_%s.rds", m))
        if (file.exists(top_path)) {
          top_genes <- as.character(readRDS(top_path))
          if (m %in% c("stabl", "boruta")) {
            sf_df <- data.frame(Gene = as.character(top_genes), stringsAsFactors = FALSE)
          } else {
            sf_df <- data.frame(Rank = seq_along(top_genes), Gene = as.character(top_genes), stringsAsFactors = FALSE)
          }
          sf_path <- get_session_path(canonical_base_id, sprintf("%%s_selected_features_%s.csv", m))
          write.csv(sf_df, sf_path, row.names = FALSE)
          register_export_file(uid_val, "selected_features", canonical_base_id, sf_path, "fs", "refit", model = m)
        }
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save refit performance/feature tables for %s: %s\n", base_id, e$message))
    })
    
    results[[d_id]] <- list(
      features            = selected_features_out,
      accuracies          = accuracies,
      loss_histories      = loss_histories,
      roc_data            = roc_data,
      performance_metrics = performance_metrics,
      roc_plot            = roc_plot_base64,
      warnings            = as.list(job_warnings)
    )
  }

  # Also refit models for external test datasets using features selected on compatible training datasets
  for (t_id in names(test_dss)) {
    t <- test_dss[[t_id]]
    base_id <- get_base_id(t$id)
    t_type <- get_dataset_data_type(t)
    t_hier <- get_hierarchy_group(t_type)
    
    # Find compatible training dataset (same hierarchy group)
    comp_train_id <- NULL
    for (d_id in names(train_dss)) {
      d_train <- train_dss[[d_id]]
      d_train_type <- get_dataset_data_type(d_train)
      d_train_hier <- get_hierarchy_group(d_train_type)
      if (d_train_hier == t_hier) {
        comp_train_id <- d_id
        break
      }
    }
    
    if (is.null(comp_train_id)) {
      cat(sprintf("  No compatible training dataset found for external test dataset %s. Skipping refit.\n", t$name))
      next
    }
    
    comp_base_id <- get_base_id(comp_train_id)
    
    # Load splits for the test dataset
    train_split_path <- get_session_path(base_id, "%s_fs_train_split.rds")
    test_split_path  <- get_session_path(base_id, "%s_fs_test_split.rds")
    if (!file.exists(train_split_path)) {
      cat(sprintf("  Train split for %s not found. Skipping refit.\n", t$name))
      next
    }
    
    train_df <- readRDS(train_split_path)
    test_df  <- readRDS(test_split_path)
    saveRDS(parameters, get_session_path(base_id, "%s_fs_parameters.rds"))
    
    accuracies          <- list()
    loss_histories      <- list()
    roc_data            <- list()
    roc_raw_data        <- list()
    performance_metrics <- list()
    job_warnings        <- character(0)
    selected_features_out <- list()
    
    # Prepare test dataset data object to access gene names mapping
    test_ml <- tryCatch({
      prepare_ml_data(t, max_features = NULL)
    }, error = function(e) NULL)
    if (is.null(test_ml)) next
    
    for (m in models) {
      # Load training dataset's top features signature
      comp_top_feats_path <- get_session_path(comp_base_id, sprintf("%%s_fs_top_features_%s.rds", m))
      if (!file.exists(comp_top_feats_path)) next
      
      top_genes_orig <- readRDS(comp_top_feats_path)
      top_genes_clean <- make.names(top_genes_orig)
      
      # Save top features for this test dataset too
      saveRDS(top_genes_orig, get_session_path(base_id, sprintf("%%s_fs_top_features_%s.rds", m)))
      
      # Copy full importances
      comp_importances_path <- get_session_path(comp_base_id, sprintf("%%s_fs_full_importances_%s.rds", m))
      if (file.exists(comp_importances_path)) {
        file.copy(comp_importances_path, get_session_path(base_id, sprintf("%%s_fs_full_importances_%s.rds", m)), overwrite = TRUE)
      }
      
      if (length(top_genes_orig) == 0) {
        accuracies[[m]]     <- 0.0
        roc_data[[m]]       <- list(fpr = numeric(0), tpr = numeric(0), auc = 0.0)
        performance_metrics[[m]] <- list(
          auc = 0.0, acc = 0.0, ppv = 0.0, npv = 0.0,
          prec = 0.0, rec = 0.0, fpr = numeric(0), tpr = numeric(0),
          tp = 0, fp = 0, fn = 0, tn = 0
        )
        selected_features_out[[m]] <- list()
        next
      }
      
      sel_cols_tr   <- top_genes_clean[top_genes_clean %in% colnames(train_df)]
      sel_cols_te   <- top_genes_clean[top_genes_clean %in% colnames(test_df)]
      train_df_eval <- train_df[, c(sel_cols_tr, "Group"), drop = FALSE]
      test_df_eval  <- test_df[, c(sel_cols_te, "Group"), drop = FALSE]
      
      gene_mapping <- top_genes_orig
      names(gene_mapping) <- top_genes_clean
      
      retrain_model_name <- if (m == "boruta") "boruta_refit" else (if (m == "stabl") "stabl_refit" else m)
      
      res_retrain <- tryCatch({
        fit_model_and_importance(retrain_model_name, train_df_eval, gene_mapping, parameters, n_threads = eff_cores)
      }, error = function(e) {
        cat(sprintf("  Re-training model %s on test dataset %s failed: %s\n", m, t$name, e$message))
        NULL
      })
      
      if (is.null(res_retrain) || is.null(res_retrain$model)) next
      fit <- res_retrain$model
      
      # Save final fit model for test dataset
      saveRDS(fit, get_session_path(base_id, sprintf("%%s_fs_final_model_%s.rds", m)))
      
      # Predict & Evaluate on the test dataset's train split
      pred_res <- predict_ml(retrain_model_name, fit, train_df_eval)
      eval_res <- evaluate_predictions(pred_res$predictions, train_df_eval$Group, pred_res$probabilities, target_prevalence = t$targetPrevalence)
      
      # CV folds score
      fold_scores <- tryCatch({
        k_folds  <- min(5, nrow(train_df_eval))
        fold_idx <- split(sample(seq_len(nrow(train_df_eval))), rep(1:k_folds, length.out = nrow(train_df_eval)))
        sapply(fold_idx, function(vi) {
          tryCatch({
            cv_train <- train_df_eval[-vi, , drop = FALSE]
            cv_val   <- train_df_eval[vi, , drop = FALSE]
            if (nrow(cv_train) < 2 || nrow(cv_val) < 1) return(eval_res$acc)
            cv_res  <- fit_model_and_importance(retrain_model_name, cv_train, gene_mapping, parameters, n_threads = eff_cores)
            cv_pred <- predict_ml(retrain_model_name, cv_res$model, cv_val)
            cv_eval <- evaluate_predictions(cv_pred$predictions, cv_val$Group, cv_pred$probabilities, target_prevalence = t$targetPrevalence)
            cv_eval$bacc
          }, error = function(e) eval_res$bacc)
        })
      }, error = function(e) rep(eval_res$bacc, 5))
      
      accuracies[[m]]     <- round(eval_res$bacc, 3)
      roc_data[[m]]       <- list(fpr = as.numeric(eval_res$fpr), tpr = as.numeric(eval_res$tpr), auc = round(eval_res$auc, 3))
      
      roc_obj_tmp <- tryCatch({
        pROC::roc(response = train_df_eval$Group, predictor = pred_res$probabilities, quiet = TRUE)
      }, error = function(e) NULL)

      b_res <- if (!is.null(roc_obj_tmp)) {
        tryCatch(compute_bootstrap_roc(roc_obj_tmp, n_boot = 150, conf.level = 0.95), error = function(e) NULL)
      } else NULL

      ci_auc <- if (!is.null(b_res)) b_res$ci_auc else c(NA_real_, NA_real_)
      if (!is.null(roc_obj_tmp) && !is.null(b_res)) {
        roc_obj_tmp$ci_auc  <- b_res$ci_auc
        roc_obj_tmp$band_df <- b_res$band_df
      }
      roc_raw_data[[m]] <- roc_obj_tmp
      ci_str <- if (!any(is.na(ci_auc))) sprintf("[%.3f, %.3f]", ci_auc[1], ci_auc[2]) else "N/A"
      
      performance_metrics[[m]] <- list(
        auc      = round(eval_res$auc, 3),
        ci       = ci_str,
        ci_lower = if (!is.na(ci_auc[1])) round(ci_auc[1], 3) else NA,
        ci_upper = if (!is.na(ci_auc[2])) round(ci_auc[2], 3) else NA,
        acc      = round(eval_res$bacc, 3),
        ppv      = round(eval_res$ppv %||% eval_res$prec, 3),
        npv      = round(eval_res$npv %||% eval_res$rec, 3),
        prec     = round(eval_res$ppv %||% eval_res$prec, 3),
        rec      = round(eval_res$npv %||% eval_res$rec, 3),
        fpr      = as.numeric(eval_res$fpr),
        tpr      = as.numeric(eval_res$tpr),
        tp       = eval_res$tp,
        fp       = eval_res$fp,
        fn       = eval_res$fn,
        tn       = eval_res$tn
      )
      
      loss_histories[[m]] <- as.numeric(fold_scores)
      
      # Keep subset features list for response
      selected_feats_model <- top_genes_orig
      selected_features_out[[m]] <- selected_feats_model
    }
    
    # Generate ROC plot for test dataset training/validation
    roc_plot_base64 <- ""
    standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
    valid_raw_models <- intersect(standard_order, intersect(names(roc_raw_data), models))
    valid_raw_models <- Filter(function(m) !is.null(roc_raw_data[[m]]), valid_raw_models)
    if (length(valid_raw_models) > 0) {
      roc_raw_data_mapped <- roc_raw_data[valid_raw_models]
      names(roc_raw_data_mapped) <- sapply(names(roc_raw_data_mapped), function(m) MODEL_LABELS_MAP[[m]] %||% m)
      p_roc <- plot_roc_multi(roc_raw_data_mapped, title = NULL, show_auc_in_legend = FALSE, bands = TRUE, band = "ci", n_boot = 150) +
        ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.key.spacing.y = grid::unit(0.5, "cm")) +
        ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
        
      tryCatch({
        t_roc_pdf <- get_session_path(t$id, "roc_train_%s.pdf")
        ggplot2::ggsave(filename = t_roc_pdf, plot = p_roc, width = 10, height = 7)
        ggplot2::ggsave(filename = get_session_path(t$id, "roc_train_%s.png"), plot = p_roc, width = 10, height = 7, dpi = 300)
        ggplot2::ggsave(filename = get_session_path(t$id, "roc_train_%s.tiff"), plot = p_roc, width = 10, height = 7, dpi = 300)
        register_export_file(get_user_id(t$id), "roc_all_models_train", t$id, t_roc_pdf, "fs", "refit", ext = "pdf")
      }, error = function(e) {
        cat("[WARNING] Failed to save testing ROC plot:", e$message, "\n")
      })
      
      roc_plot_base64 <- tryCatch(plot_to_base64(p_roc, width = 930, height = 650), error = function(e) "")
      rm(roc_raw_data_mapped, p_roc)
      invisible(gc(verbose = FALSE))
    }

    # Save and register all_models_performance_train and selected_features for test refit
    tryCatch({
      test_base_id <- get_base_id(t$id)
      uid_val <- get_user_id(t$id) %||% get_user_id(base_id)
      if (length(performance_metrics) > 0) {
        p_models <- names(performance_metrics)
        perf_df <- data.frame(
          Model = sapply(p_models, function(m) MODEL_LABELS_MAP[[m]] %||% toupper(m)),
          AUC = sapply(p_models, function(m) performance_metrics[[m]]$auc %||% NA),
          AUC_95_CI = sapply(p_models, function(m) performance_metrics[[m]]$ci %||% NA),
          Balanced_Accuracy = sapply(p_models, function(m) performance_metrics[[m]]$acc %||% NA),
          PPV = sapply(p_models, function(m) performance_metrics[[m]]$ppv %||% NA),
          NPV = sapply(p_models, function(m) performance_metrics[[m]]$npv %||% NA),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        colnames(perf_df) <- c("Model", "AUC", "95% CI (AUC)", "Accuracy", "PPV", "NPV")
        perf_train_path <- get_session_path(test_base_id, "%s_all_models_performance_train.csv")
        write.csv(perf_df, perf_train_path, row.names = FALSE)
        register_export_file(uid_val, "all_models_performance_train", test_base_id, perf_train_path, "fs", "refit")
      }

      for (m in models) {
        top_path <- get_session_path(test_base_id, sprintf("%%s_fs_top_features_%s.rds", m))
        if (file.exists(top_path)) {
          top_genes <- as.character(readRDS(top_path))
          if (m %in% c("stabl", "boruta")) {
            sf_df <- data.frame(Gene = as.character(top_genes), stringsAsFactors = FALSE)
          } else {
            sf_df <- data.frame(Rank = seq_along(top_genes), Gene = as.character(top_genes), stringsAsFactors = FALSE)
          }
          sf_path <- get_session_path(test_base_id, sprintf("%%s_selected_features_%s.csv", m))
          write.csv(sf_df, sf_path, row.names = FALSE)
          register_export_file(uid_val, "selected_features", test_base_id, sf_path, "fs", "refit", model = m)
        }
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] Failed to save test refit performance/feature tables for %s: %s\n", t$id, e$message))
    })
    
    results[[t_id]] <- list(
      features            = selected_features_out,
      accuracies          = accuracies,
      loss_histories      = loss_histories,
      roc_data            = roc_data,
      performance_metrics = performance_metrics,
      roc_plot            = roc_plot_base64,
      warnings            = as.list(job_warnings)
    )
  }
  
  # Map merged training dataset results back to original input dataset IDs
  input_ids <- sapply(datasets, function(x) x$id)
  for (d_id in input_ids) {
    if (is.null(results[[d_id]])) {
      for (res_id in names(results)) {
        if (grepl("merged_", res_id) && grepl("_training", res_id)) {
          results[[d_id]] <- results[[res_id]]
          # Also copy ROC curves
          for (ext in c("pdf", "png", "tiff")) {
            src_roc <- get_session_path(res_id, sprintf("roc_train_%%s.%s", ext))
            dest_roc <- get_session_path(d_id, sprintf("roc_train_%%s.%s", ext))
            if (file.exists(src_roc)) {
              file.copy(src_roc, dest_roc, overwrite = TRUE)
            }
          }
          break
        }
      }
    }
  }
  
  return(results)
}
