# Finalization and report generation functions
# These functions compile the results from compute jobs, format them into markdown,
# and append/update the user session report.

finalize_meta_results <- function(per_class, datasets_by_class, method_val, pvalue_method,
                                  eff_model, votes, pval_thresh, logfc_thresh,
                                  meta_ds_id, user_id, module) {
  final_res <- NULL
  all_full_rows <- list()

  for (dclass in names(datasets_by_class)) {
    res <- per_class[[dclass]]
    if (is.null(res)) next
    class_ds <- datasets_by_class[[dclass]]
    final_res <- res

    full_rows <- if (!is.null(res$full_results)) res$full_results else (if (!is.null(res$results)) res$results else res)
    if (is.list(full_rows)) {
      all_full_rows <- c(all_full_rows, full_rows)
    }

    filename <- sprintf("%s_%s_%s_meta_analyzed_results.csv", user_id, module, dclass)
    meta_csv_path <- get_session_path(meta_ds_id, filename)
    meta_headers <- if (identical(method_val, "combine_pvalue")) {
      c("Feature", "FoldChange", "Combined LogFC", "P-value", "adj.P.Val.")
    } else if (identical(method_val, "effect_size")) {
      c("Feature", "FoldChange", "Combined Effects Size", "P-value", "adj.P.Val.")
    } else {
      NULL
    }
    save_list_to_csv(full_rows, meta_csv_path, headers = meta_headers)
    save_list_to_csv(full_rows, get_session_path(meta_ds_id, "%s_meta_analyzed_results.csv"), headers = meta_headers)
    save_list_to_csv(full_rows, sprintf("tmp/%s_meta_analyzed_results.csv", meta_ds_id), headers = meta_headers)
    register_export_file(user_id, "meta_results", meta_ds_id, meta_csv_path, module, "meta", ext = "csv")

    if (method_val == "effect_size") {
      generate_meta_plots(full_rows, meta_ds_id, pval_thresh, logfc_thresh, data_class = dclass)
      forest_p <- get_session_path(meta_ds_id, "%s_forest_plots.pdf")
      volcano_p <- get_session_path(meta_ds_id, "%s_volcano_plots.pdf")
      if (file.exists(forest_p)) register_export_file(user_id, "forest_plots", meta_ds_id, forest_p, module, "meta", ext = "pdf")
      if (file.exists(volcano_p)) register_export_file(user_id, "de_volcano", meta_ds_id, volcano_p, module, "meta", ext = "pdf")

      tryCatch({
        het_rows <- lapply(full_rows, function(r) {
          list(
            gene = r$gene,
            tau2 = r$tau2,
            tau  = r$tau,
            i2   = r$i2
          )
        })
        het_path <- get_session_path(meta_ds_id, "%s_heterogeneity_report.csv")
        save_list_to_csv(het_rows, het_path)
        register_export_file(user_id, "heterogeneity_report", meta_ds_id, het_path, module, "meta", ext = "csv")
      }, error = function(e) NULL)
    }

    df_meta <- NULL
    if (is.data.frame(full_rows)) {
      df_meta <- full_rows
    } else if (is.list(full_rows) && length(full_rows) > 0) {
      tryCatch({
        rows <- lapply(full_rows, function(item) {
          flat <- unlist(lapply(names(item), function(nm) {
            val <- item[[nm]]
            if (is.list(val) || length(val) > 1) {
              setNames(paste(unlist(val), collapse=";"), nm)
            } else {
              setNames(as.character(val), nm)
            }
          }))
          as.data.frame(t(flat), stringsAsFactors = FALSE)
        })
        df_meta <- do.call(rbind, rows)
      }, error = function(e) NULL)
    }
    if (!is.null(df_meta)) {
      for (d in class_ds) {
        d_id <- if (!is.null(d$id)) d$id else d$datasetId
        if (!is.null(d_id) && d_id != "") {
          push_de_stack(d_id, df_meta, step_name = "meta", metadata = list(method = method_val))
        }
      }
    }
  }

  if (is.null(final_res)) return(NULL)

  final_res$full_results <- if (length(all_full_rows) > 0) all_full_rows else list()
  final_res$results <- if (length(all_full_rows) > 0) all_full_rows else list()
  final_res$top10 <- if (length(all_full_rows) > 0) head(all_full_rows, 10) else list()
  .sig <- function(r) {
    if (identical(method_val, "effect_size")) {
      q <- r$qval
      g_val <- r$hedges_g
      ci_l <- if (!is.null(r$ci_lower)) r$ci_lower else 0
      ci_u <- if (!is.null(r$ci_upper)) r$ci_upper else 0
      has_ci <- !is.null(r$ci_lower) && !is.null(r$ci_upper)
      
      sig_res <- !is.null(q) && !is.na(q) && q < pval_thresh
      if (has_ci) {
        sig_res <- sig_res && !(ci_l <= 0 && ci_u >= 0)
      }
      if (!is.null(g_val) && !is.na(g_val)) {
        sig_res <- sig_res && abs(g_val) >= logfc_thresh
      }
      return(sig_res)
    } else if (identical(method_val, "combine_pvalue")) {
      q <- r$qval
      fc <- if (!is.null(r$logFC)) r$logFC else (if (!is.null(r$fc)) r$fc else 0)
      return(!is.null(q) && !is.na(q) && q < pval_thresh && abs(fc) >= logfc_thresh)
    } else {
      return(TRUE)
    }
  }
  .up  <- function(r) {
    (!is.null(r$dir) && (r$dir == "Up" || r$dir == "up")) || 
    (!is.null(r$fc) && r$fc > 0) ||
    (!is.null(r$hedges_g) && r$hedges_g > 0)
  }
  .dn  <- function(r) {
    (!is.null(r$dir) && (r$dir == "Down" || r$dir == "down")) || 
    (!is.null(r$fc) && r$fc < 0) ||
    (!is.null(r$hedges_g) && r$hedges_g < 0)
  }

  is_vote <- identical(method_val, "vote_counting")
  is_shared <- identical(method_val, "shared_genes")
  overlapping_val <- if (!is.null(final_res$stats$overlappingGenes)) final_res$stats$overlappingGenes else 0
  num_sig_val <- if (length(all_full_rows) == 0) 0 else if (is_vote || is_shared) length(all_full_rows) else sum(vapply(all_full_rows, .sig, logical(1)))
  sig_up_val  <- if (length(all_full_rows) == 0) 0 else sum(vapply(all_full_rows, function(r) (is_vote || is_shared || .sig(r)) && .up(r), logical(1)))
  sig_dn_val  <- if (length(all_full_rows) == 0) 0 else sum(vapply(all_full_rows, function(r) (is_vote || is_shared || .sig(r)) && .dn(r), logical(1)))

  orig_stats <- final_res$stats
  final_res$stats <- list(
    totalFeatures    = length(all_full_rows),
    numDatasets      = if (!is.null(orig_stats$numDatasets)) orig_stats$numDatasets else length(datasets_by_class[[1]]),
    overlappingGenes = overlapping_val,
    numSignificant   = num_sig_val,
    sigUp            = sig_up_val,
    sigDown          = sig_dn_val,
    tau2             = orig_stats$tau2,
    tau              = orig_stats$tau,
    i2               = orig_stats$i2
  )

  pretty_method <- switch(method_val,
    "combine_pvalue" = "Combine P-values",
    "effect_size" = "Effect Size Meta-analysis",
    "vote_counting" = "Vote Counting",
    method_val
  )

  meta_md <- sprintf(
    "Differential expression meta-analysis was performed to integrate findings across multiple datasets.\n\n### Meta-Analysis Methodology\n- **Feature Space Construction:** The analysis was executed using the **Union** of all feature IDs present across the input datasets to preserve maximum genomic coverage across differing platforms.\n- **Method & Statistical Method:** The meta-analysis employed the **%s** method.\n",
    pretty_method
  )
  if (method_val == "combine_pvalue") {
    pm <- if (!is.null(pvalue_method)) pvalue_method else "fisher"
    meta_md <- paste0(meta_md, sprintf("  - **P-value Combination Method:** %s\n  - **Minimum Study Count ($k \\ge 2$):** Features present in only a single dataset ($k < 2$) were filtered out and excluded prior to combination.\n  - **Combined logFC Calculation:** The server used the logFC from each dataset to calculate the combined logFC (mean value across datasets) and computed the meta p-value using the sample-size weighted **%s** method using the metapro R package.\n", toupper(pm), pm))
  } else if (method_val == "effect_size") {
    em <- if (!is.null(eff_model)) eff_model else "random"
    meta_md <- paste0(meta_md, sprintf("  - **Effect Size Model:** %s\n  - **Minimum Study Count ($k \\ge 2$):** Features present in only a single dataset ($k < 2$) were filtered out and excluded prior to running `metafor::rma` model fitting.\n  - **Combined Effect Size Calculation:** For the effect size analysis, Hedges' g value was computed. The combined effect size is the combined Hedges' g value, not the logFC, calculated using the **%s** effect size model.\n", toupper(em), em))
  } else if (method_val == "vote_counting") {
    meta_md <- paste0(meta_md, sprintf("  - **Minimum Vote Threshold:** %s\n", votes))
  }
  meta_md <- paste0(meta_md, "- **Multiple Testing Correction (FDR):** P-values were adjusted using the Benjamini-Hochberg (BH) procedure. Crucially, the adjustment was applied strictly to the features passing the minimum study count ($k \\ge 2$) threshold to avoid inflating the false discovery rate hypothesis space.\n")
  meta_md <- paste0(meta_md, sprintf(
    "- **Significance Thresholds:** p-value/FDR threshold &le; **%s**, fold change / effect size threshold &ge; **%s**\n\n### Results Summary\n- **Total Shared Genes (Intersection):** %d\n- **Total Meta-Analyzed Genes (Genuine features with $k \\ge 2$):** %d\n- **Significantly Regulated Genes:** %d\n- **Significantly Up-regulated:** %d\n- **Significantly Down-regulated:** %d\n",
    pval_thresh, logfc_thresh,
    overlapping_val,
    length(all_full_rows),
    num_sig_val,
    sig_up_val,
    sig_dn_val
  ))

  append_step_to_report(user_id, "Differential Expression Meta-Analysis", meta_md, module = module)
  final_res
}

finalize_upload_datasets <- function(res) {
  return(res)
}

finalize_annotation <- function(res, datasets, strategy, organisms, biotypes) {
  for (r in res) {
    if (!is.null(r$datasetId)) {
      tryCatch({
        d_config <- NULL
        for (d in datasets) {
          if (!is.null(d$datasetId) && as.character(d$datasetId) == as.character(r$datasetId)) {
            d_config <- d
            break
          }
        }
        dataset_name <- if (!is.null(d_config$name) && nzchar(d_config$name)) d_config$name else r$datasetId
        user_id_val <- get_user_id(r$datasetId)
        
        strategy_label <- switch(strategy,
          "keep-first" = "Keep first mapped gene symbol (sum read counts / mean microarray expression)",
          "filter" = "Filter out multi-mapped genes",
          "skip" = "Skip annotation",
          strategy
        )
        
        annotation_md <- sprintf(
          "Gene identifier annotation and mapping was performed for dataset **%s**.\n\n**Organism:** %s\n**Gene Biotype Selection:** %s\n**Multi-mapping Resolution Strategy:** %s\n\n**Mapping Statistics:**\n- Total input features: %d\n- Successfully mapped to gene symbols: %d\n- Unmapped features: %d\n- Unique symbols: %d\n- Multi-mapped features: %d",
          dataset_name, organisms %||% "Human (Homo sapiens)", biotypes %||% "Protein-coding", strategy_label,
          r$total, r$mapped, r$unmapped, r$unique, r$multi
        )
        
        section_title <- sprintf("Gene Annotation & Identifier Mapping — %s", dataset_name)
        append_step_to_report(user_id_val, section_title, annotation_md, module = "dp")
      }, error = function(e) {
        cat("[WARNING] Failed to append annotation results to report:", e$message, "\n")
      })
    }
  }
  return(res)
}

finalize_processing <- function(res, body_data) {
  for (r in res) {
    if (!is.null(r$datasetId) && !is.null(r$parsedData) && length(r$parsedData) > 0) {
      tryCatch({
        d_config <- NULL
        for (bd in body_data) {
          if (!is.null(bd$datasetId) && as.character(bd$datasetId) == as.character(r$datasetId)) {
            d_config <- bd
            break
          }
        }
        if (!is.null(d_config)) {
          filt_method <- d_config$filterMethod %||% "none"
          impute_method <- d_config$imputeMethod %||% "none"
          
          filt_text <- ""
          if (filt_method == "skip") {
            filt_text <- "No variance or expression filtering was applied."
          } else if (filt_method == "variance") {
            var_cutoff <- d_config$varianceCutoff %||% 10
            filt_text <- sprintf("Variance filtering was applied, retaining features with expression variance exceeding the **%s%%** percentile threshold.", var_cutoff)
          } else if (filt_method == "cpm") {
            cpm_thresh <- d_config$cpmThreshold %||% 1
            cpm_samples <- d_config$cpmSamplesCount %||% 1
            filt_text <- sprintf("Counts-per-million (CPM) filtering was applied, retaining features with CPM &ge; **%s** in at least **%s** samples.", cpm_thresh, cpm_samples)
          } else if (filt_method == "min_count") {
            min_count <- d_config$minCountThreshold %||% 10
            filt_text <- sprintf("Minimum count filtering was applied, retaining features with raw counts &ge; **%s** in at least one sample.", min_count)
          } else {
            filt_text <- "No filtering was applied."
          }
          
          na_remove_pct <- d_config$naRemovePercent
          na_remove_text <- ""
          if (!is.null(na_remove_pct) && !identical(na_remove_pct, "")) {
            na_remove_text <- sprintf("Features with missing values in more than **%s%%** of samples were removed.", na_remove_pct)
          }

          impute_text <- ""
          if (impute_method == "knn") {
            k_val <- d_config$knnNeighbors %||% 5
            impute_text <- sprintf("Missing values were imputed using **k-Nearest Neighbors (kNN)** imputation with k=%s, estimated from the observed expression profile.", k_val)
          } else if (impute_method == "median") {
            impute_text <- "Missing values were imputed using the **median** value of each feature across samples."
          } else {
            impute_text <- "No missing value imputation was performed."
          }
          
          input_f <- r$inputFeatures %||% 0
          retained_f <- r$retainedFeatures %||% 0
          dataset_name <- if (!is.null(d_config$name) && nzchar(d_config$name)) d_config$name else r$datasetId
          
          parts <- c(
            sprintf("Data processing and quality control was conducted for dataset **%s** (Data Type: %s).", dataset_name, d_config$dataType %||% "readcounts"),
            if (nzchar(na_remove_text)) na_remove_text else NULL,
            if (nzchar(impute_text)) impute_text else NULL,
            if (nzchar(filt_text)) filt_text else NULL,
            sprintf("**Feature counts:**\n- Input features: %d\n- Retained features: %d (removed %d features)", input_f, retained_f, input_f - retained_f)
          )
          processing_md <- paste(parts, collapse = "\n\n")
          
          section_title <- sprintf("Data Filtering & Missing Value Imputation — %s", dataset_name)
          append_step_to_report(get_user_id(r$datasetId), section_title, processing_md, module = "dp")
        }
      }, error = function(e) {
        cat("[WARNING] Failed to append processing results to report:", e$message, "\n")
      })
    }
  }
  return(res)
}

finalize_normalization <- function(res, datasets, method_val, transform_type, prior_count) {
  for (r in res) {
    if (!is.null(r$datasetId) && !is.null(r$parsedData) && length(r$parsedData) > 0) {
      tryCatch({
        d_config <- NULL
        for (d in datasets) {
          if (!is.null(d$id) && as.character(d$id) == as.character(r$datasetId)) {
            d_config <- d
            break
          }
        }
        if (!is.null(d_config)) {
          norm_method <- toupper(method_val)
          d_transform_type <- if (!is.null(d_config$transformationType)) d_config$transformationType else (if (isTRUE(d_config$logTransform)) "log2" else "none")
          log_text <- if (d_transform_type == "log2") {
            if (tolower(method_val) == "tmm") {
              sprintf("calculated on the raw read counts, followed by computing CPMs and applying log&sub2; transformation (with a prior count of %s)", prior_count)
            } else {
              sprintf("log&sub2; transformed (with a prior count of %s)", prior_count)
            }
          } else if (d_transform_type == "log10") {
            if (tolower(method_val) == "tmm") {
              sprintf("calculated on the raw read counts, followed by computing CPMs and applying log&sub1;&sub0; transformation (with a prior count of %s)", prior_count)
            } else {
              sprintf("log&sub1;&sub0; transformed (with a prior count of %s)", prior_count)
            }
          } else {
            "not log transformed"
          }
          dtype_text <- d_config$dataType %||% "readcounts"
          
          n_features <- length(r$parsedData)
          n_samples <- if (!is.null(r$columns)) length(r$columns) - 1 else 0
          
          dataset_name <- if (!is.null(d_config$name) && nzchar(d_config$name)) d_config$name else r$datasetId
          norm_md <- sprintf(
            "Expression data normalization was performed for dataset **%s** (Data Type: %s).\n\n**Method:** %s\n**Log Transformation:** The data was %s.\n\n**Dataset dimensions:**\n- Features: %d\n- Samples: %d",
            dataset_name, dtype_text, norm_method, log_text, n_features, n_samples
          )
          norm_section_title <- sprintf("Expression Normalization — %s", dataset_name)
          append_step_to_report(get_user_id(r$datasetId), norm_section_title, norm_md, module = "dp")
        }
      }, error = function(e) {
        cat("[WARNING] Failed to append normalization results to report:", e$message, "\n")
      })
    }

    for (d in datasets) {
      d_id <- d$id %||% d$datasetId
      if (!is.null(d_id)) {
        tryCatch(ensure_boxplot_before(d_id), error = function(e) NULL)
        tryCatch(generate_pca_plots_for_export(d_id), error = function(e) NULL)
      }
    }
  }
  return(res)
}

finalize_batch_correction <- function(res, datasets, method_val, method_others) {
  if (length(datasets) > 0) {
    for (d in datasets) {
      d_id <- d$id %||% d$datasetId
      if (!is.null(d_id)) {
        tryCatch(generate_pca_plots_for_export(d_id), error = function(e) NULL)
        tryCatch(generate_pca_plots_for_export(get_base_id(d_id)), error = function(e) NULL)
      }
    }

    ds_id_first <- datasets[[1]]$id
    user_id_val <- get_user_id(ds_id_first)
    if (is.null(user_id_val) || !nzchar(user_id_val)) user_id_val <- get_user_id(get_base_id(ds_id_first))
    
    batch_col <- datasets[[1]]$clinicalBatchCol %||% "Batch"
    
    ds_names <- sapply(datasets, function(d) if (!is.null(d$name) && nzchar(d$name)) d$name else d$id)
    ds_names_str <- paste(ds_names, collapse = ", ")
    
    correct_method <- method_val
    if (datasets[[1]]$dataType != "readcounts") {
      correct_method <- method_others
    }
    
    batch_md <- sprintf(
      "Batch effect correction was applied across the following datasets: **%s**.\n\n**Correction Method:** %s\n**Batch Covariate Column:** %s\n\nCorrected expression data has been saved to the session repository. Quality control PCA plots before and after batch correction have been generated to assess correction efficacy.",
      ds_names_str, toupper(correct_method), batch_col
    )
    
    append_step_to_report(user_id_val, "Batch Effect Correction", batch_md, module = "dp")
  }
  return(res)
}

finalize_pca <- function(res) {
  return(res)
}

finalize_ea <- function(res, config) {
  dataset_id <- if (!is.null(config$datasetId)) config$datasetId else ""
  methods_val <- if (!is.null(config$methods)) config$methods else list("ora")
  ora_db      <- config$oraDatabase
  gsea_db     <- config$gseaDatabase
  pval_cutoff <- config$pValueCutoff
  qval_cutoff <- config$qValueCutoff
  min_size    <- config$minGeneSetSize
  max_size    <- config$maxGeneSetSize
  organism    <- config$organism

  tryCatch({
    user_id_val <- get_user_id(dataset_id)
    if (is.null(user_id_val) || !nzchar(user_id_val)) {
      user_id_val <- get_user_id(config$datasetId)
    }
    if (is.null(user_id_val) || !nzchar(user_id_val)) {
      user_id_val <- "user"
    }
    
    methods_str <- paste(toupper(methods_val), collapse = " and ")
    dbs_queried <- c()
    if ("ora" %in% methods_val) dbs_queried <- c(dbs_queried, ora_db)
    if ("gsea" %in% methods_val) dbs_queried <- c(dbs_queried, gsea_db)
    dbs_str <- paste(unique(dbs_queried), collapse = ", ")
    
    n_mapped <- res$mappedCount %||% res$mappedGenesCount %||% 0
    n_total <- res$totalInputCount %||% res$totalInputGenes %||% 0
    
    n_sig_ora <- length(res$oraResults)
    n_sig_gsea <- length(res$gseaResults)
    
    dataset_name <- get_dataset_name(get_base_id(dataset_id))
    if (is.null(dataset_name) || dataset_name == "") {
      dataset_name <- dataset_id
    }
    
    ea_md <- sprintf(
      "Enrichment and functional annotation analysis was conducted for dataset **%s**.\n\n**Analysis Method(s):** %s\n**Organism:** %s\n**Databases Queried:** %s\n**Significance Cutoffs:** p-value cutoff &le; **%s**, q-value cutoff &le; **%s**\n**Gene Set Size Range:** %s - %s\n\n**Results Summary:**\n- Input genes mapped to Entrez IDs: %d out of %d total input genes\n- ORA terms returned: %d\n- GSEA terms returned: %d",
      dataset_name, methods_str, organism %||% "Unknown", dbs_str, pval_cutoff, qval_cutoff, min_size, max_size, n_mapped, n_total, n_sig_ora, n_sig_gsea
    )
    
    module_val <- get_backend_datasets(dataset_id)$module
    if (is.null(module_val) || !nzchar(module_val)) {
      module_val <- if (grepl("_dp", dataset_id)) "dp" else (if (grepl("_de", dataset_id)) "de" else "ea")
    }
    append_step_to_report(user_id_val, sprintf("Enrichment Analysis — %s", dataset_name), ea_md, module = module_val)
  }, error = function(e) {
    cat("[WARNING] Failed to append EA report:", e$message, "\n")
  })
  
  res$oraObjects <- NULL
  res$gseObjects <- NULL
  return(res)
}

finalize_de_results <- function(res, datasets, pval_thresh, logfc_thresh, adjust_method, fallback_method) {
  for (ds_id in names(res)) {
    base_id <- get_base_id(ds_id)
    user_id_val <- get_user_id(ds_id)
    if (is.null(user_id_val) || !nzchar(user_id_val)) user_id_val <- get_user_id(base_id)
    module_val <- get_backend_datasets(ds_id)$module %||% get_backend_datasets(base_id)$module %||% (if (grepl("_dp", ds_id) || grepl("_dp", base_id)) "dp" else "de")
    
    de_all_path <- get_session_path(base_id, "%s_de_results.csv")
    de_headers <- c("Feature", "FoldChange", "log2FoldChange", "P-value", "adj.P.Val.")
    if (!is.null(res[[ds_id]]$full_results) && length(res[[ds_id]]$full_results) > 0) {
      save_list_to_csv(res[[ds_id]]$full_results, sprintf("tmp/%s_de_results.csv", ds_id), headers = de_headers)
      save_list_to_csv(res[[ds_id]]$full_results, sprintf("tmp/%s_de_results.csv", base_id), headers = de_headers)
      save_list_to_csv(res[[ds_id]]$full_results, de_all_path, headers = de_headers)
    } else if (!file.exists(de_all_path) && !file.exists(sprintf("tmp/%s_de_results.csv", ds_id))) {
      # If CSV was not written, try reconstructing from de_raw_table.rds
      raw_table_path <- get_session_path(base_id, "%s_de_raw_table.rds")
      if (!file.exists(raw_table_path)) raw_table_path <- sprintf("tmp/%s_de_raw_table.rds", base_id)
      if (file.exists(raw_table_path)) {
        raw_df <- tryCatch(readRDS(raw_table_path), error = function(e) NULL)
        if (!is.null(raw_df) && nrow(raw_df) > 0) {
          raw_df$logFC[is.na(raw_df$logFC)] <- 0
          raw_df$pValue[is.na(raw_df$pValue)] <- 1
          raw_df$adjPValue[is.na(raw_df$adjPValue)] <- 1
          raw_df$baseMean[is.na(raw_df$baseMean)] <- 0
          raw_df$significant <- raw_df$adjPValue < pval_thresh & abs(raw_df$logFC) >= logfc_thresh
          raw_df$direction <- ifelse(raw_df$significant, ifelse(raw_df$logFC > 0, "up", "down"), "ns")
          ord_idx <- order(raw_df$adjPValue, raw_df$pValue)
          sorted_raw <- raw_df[ord_idx, ]
          sorted_raw$FoldChange <- 2^(sorted_raw$logFC)
          export_df <- data.frame(
            Feature = sorted_raw$gene,
            FoldChange = sorted_raw$FoldChange,
            log2FoldChange = sorted_raw$logFC,
            `P-value` = sorted_raw$pValue,
            `adj.P.Val.` = sorted_raw$adjPValue,
            baseMean = sorted_raw$baseMean,
            significant = sorted_raw$significant,
            direction = sorted_raw$direction,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          write.csv(export_df, de_all_path, row.names = FALSE)
        }
      } else if (!is.null(res[[ds_id]]$results) && length(res[[ds_id]]$results) > 0) {
        save_list_to_csv(res[[ds_id]]$results, de_all_path)
      }
    }
    register_export_file(user_id_val, "de_all_results", base_id, de_all_path, "de", "analysis", ext = "csv")

    # Extract and register significant results if present
    if (file.exists(de_all_path)) {
      tryCatch({
        de_df <- read.csv(de_all_path, check.names = FALSE, stringsAsFactors = FALSE)
        sig_col <- intersect(c("significant","sig","Significant"), colnames(de_df))[1]
        if (!is.na(sig_col)) {
          sig_df <- de_df[de_df[[sig_col]] %in% c(TRUE, "TRUE", "true", "True", 1, "1"), , drop = FALSE]
        } else {
          padj_col <- intersect(c("padj","adj.P.Val","FDR","p.adj"), colnames(de_df))[1]
          sig_df <- if (!is.na(padj_col)) de_df[!is.na(de_df[[padj_col]]) & as.numeric(de_df[[padj_col]]) < pval_thresh, ] else de_df
        }
        de_sig_path <- get_session_path(base_id, "%s_de_sig_results.csv")
        write.csv(sig_df, de_sig_path, row.names = FALSE)
        register_export_file(user_id_val, "de_sig_results", base_id, de_sig_path, "de", "analysis", ext = "csv")
      }, error = function(e) NULL)
    }

    # Register volcano and MA plots if they were generated
    volcano_p <- sprintf("tmp/%s_volcano.pdf", ds_id)
    if (file.exists(volcano_p)) register_export_file(user_id_val, "de_volcano", base_id, volcano_p, "de", "analysis", ext = "pdf")
    ma_p <- sprintf("tmp/%s_ma.pdf", ds_id)
    if (file.exists(ma_p)) register_export_file(user_id_val, "de_ma", base_id, ma_p, "de", "analysis", ext = "pdf")
  }

  for (ds_id in names(res)) {
    tryCatch({
      base_id     <- get_base_id(ds_id)
      user_id_val <- get_user_id(ds_id)
      
      d_config <- NULL
      for (d in datasets) {
        d_id_chk <- if (!is.null(d$datasetId)) d$datasetId else d$id
        if (!is.null(d_id_chk)) {
          if (as.character(d_id_chk) == as.character(ds_id) ||
              get_base_id(d_id_chk) == base_id ||
              grepl(d_id_chk, ds_id, fixed = TRUE) ||
              grepl(ds_id, d_id_chk, fixed = TRUE)) {
            d_config <- d
            break
          }
        }
      }
      
      dataset_name <- NULL
      if (!is.null(d_config$name) && nzchar(d_config$name)) {
        dataset_name <- d_config$name
      } else {
        dataset_name <- get_dataset_name(base_id)
      }
      if (is.null(dataset_name) || !nzchar(dataset_name)) {
        dataset_name <- base_id
      }
      
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        user_id_val <- get_user_id(base_id)
      }
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        if (!is.null(d_config) && !is.null(d_config$id)) {
          user_id_val <- get_user_id(d_config$id)
        }
      }
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        user_id_val <- "user"
      }
      
      candidate_de_csvs <- c(
        sprintf("tmp/%s_de_results.csv", ds_id),
        sprintf("tmp/%s_de_results.csv", base_id),
        get_session_path(ds_id, "%s_de_results.csv"),
        get_session_path(base_id, "%s_de_results.csv"),
        session_file_path(ds_id, sprintf("%s_de_results.csv", ds_id)),
        session_file_path(ds_id, sprintf("%s_de_results.csv", base_id)),
        get_session_path(base_id, "%s_dp_de_results.csv")
      )
      
      de_csv <- NULL
      for (cand in candidate_de_csvs) {
        if (file.exists(cand)) {
          de_csv <- cand
          break
        }
      }
      
      expr_rds <- get_session_path(base_id, "%s_expr_matrix.rds")

      if (!is.null(de_csv) && file.exists(de_csv)) {
        de_df  <- read.csv(de_csv, check.names = FALSE, stringsAsFactors = FALSE)

        expr <- NULL
        latest_de <- get_latest_de_stack(ds_id)
        if (!is.null(latest_de) && !is.null(latest_de$data)) expr <- latest_de$data
        if (is.null(expr)) {
          latest_inline <- get_latest_inline_de_main_stack(ds_id)
          if (!is.null(latest_inline) && !is.null(latest_inline$data)) expr <- latest_inline$data
        }
        if (is.null(expr)) {
          latest_main <- get_latest_main_stack(ds_id)
          if (!is.null(latest_main) && !is.null(latest_main$data)) expr <- latest_main$data
        }
        if (is.null(expr) && file.exists(expr_rds)) {
          expr <- tryCatch(readRDS(expr_rds), error = function(e) NULL)
        }

        if (!is.null(expr) && nrow(expr) > 0) {
          sig_col <- intersect(c("significant","sig","Significant"), colnames(de_df))[1]
          if (!is.na(sig_col)) {
            sig_df <- de_df[de_df[[sig_col]] %in% c(TRUE, "TRUE", 1, "1"), , drop = FALSE]
          } else {
            padj_col <- intersect(c("padj","adj.P.Val","FDR","p.adj"), colnames(de_df))[1]
            sig_df   <- if (!is.na(padj_col)) de_df[!is.na(de_df[[padj_col]]) & as.numeric(de_df[[padj_col]]) < pval_thresh, ] else de_df[1:min(10,nrow(de_df)),]
          }

          padj_col <- intersect(c("padj","adj.P.Val","FDR","p.adj"), colnames(sig_df))[1]
          if (!is.na(padj_col)) {
            sig_df <- sig_df[order(as.numeric(sig_df[[padj_col]])), ]
          } else {
            pval_col <- intersect(c("pvalue","P.Value","pValue","PValue","p.value"), colnames(sig_df))[1]
            if (!is.na(pval_col)) sig_df <- sig_df[order(as.numeric(sig_df[[pval_col]])), ]
          }
          top10 <- head(sig_df, 10)

          gene_col <- colnames(top10)[1]
          top_genes <- as.character(top10[[gene_col]])
          top_genes <- top_genes[top_genes %in% rownames(expr)]

          if (length(top_genes) >= 2) {
            hmap_mat <- expr[top_genes, , drop = FALSE]
            hmap_mat <- t(scale(t(hmap_mat)))
            hmap_mat[is.nan(hmap_mat)] <- 0

            hmap_path <- session_file_path(ds_id, sprintf("%s_de_heatmap_top10.pdf", base_id))
            pdf(hmap_path, width = 9, height = 6)
            if (requireNamespace("pheatmap", quietly = TRUE)) {
              pheatmap::pheatmap(
                hmap_mat,
                cluster_rows  = TRUE,
                cluster_cols  = TRUE,
                color         = colorRampPalette(c("#2166AC","white","#D6604D"))(100),
                main          = sprintf("Top %d Significant DE Genes — %s", length(top_genes), dataset_name),
                fontsize_row  = 9,
                fontsize_col  = 7,
                border_color  = NA
              )
            } else {
              heatmap(hmap_mat, scale = "none",
                      main = sprintf("Top %d Significant DE Genes — %s", length(top_genes), dataset_name))
            }
            dev.off()
            cat(sprintf("[DE] Heatmap saved: %s\n", hmap_path))
            register_export_file(user_id_val, "de_heatmap_top10", base_id, hmap_path, "de", "analysis", ext = "pdf")
          }
        }
      }
    }, error = function(e) {
      cat(sprintf("[WARNING] DE heatmap failed for %s: %s\n", ds_id, e$message))
      if (dev.cur() > 1) dev.off()
    })
  }

  for (ds_id in names(res)) {
    tryCatch({
      base_id     <- get_base_id(ds_id)
      user_id_val <- get_user_id(ds_id)
      
      d_config <- NULL
      for (d in datasets) {
        d_id_chk <- if (!is.null(d$datasetId)) d$datasetId else d$id
        if (!is.null(d_id_chk)) {
          if (as.character(d_id_chk) == as.character(ds_id) ||
              get_base_id(d_id_chk) == base_id ||
              grepl(d_id_chk, ds_id, fixed = TRUE) ||
              grepl(ds_id, d_id_chk, fixed = TRUE)) {
            d_config <- d
            break
          }
        }
      }

      dataset_name <- NULL
      if (!is.null(d_config$name) && nzchar(d_config$name)) {
        dataset_name <- d_config$name
      } else {
        dataset_name <- get_dataset_name(base_id)
      }
      if (is.null(dataset_name) || !nzchar(dataset_name)) {
        dataset_name <- base_id
      }
      
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        user_id_val <- get_user_id(base_id)
      }
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        if (!is.null(d_config) && !is.null(d_config$id)) {
          user_id_val <- get_user_id(d_config$id)
        }
      }
      if (is.null(user_id_val) || !nzchar(user_id_val)) {
        user_id_val <- "user"
      }

      ref_group <- if (!is.null(d_config)) (d_config$referenceGroup %||% d_config$de_referenceGroup %||% "Control") else "Control"
      comp_group <- if (!is.null(d_config)) (d_config$comparisonGroup %||% d_config$de_comparisonGroup %||% "Treatment") else "Treatment"
      de_method <- if (!is.null(d_config) && !is.null(d_config$method) && nzchar(d_config$method)) d_config$method else (res[[ds_id]]$actualMethod %||% fallback_method)

      n_sig_up <- 0
      n_sig_down <- 0

      if (!is.null(res[[ds_id]]$stats)) {
        n_sig_up   <- res[[ds_id]]$stats$sigUp %||% 0
        n_sig_down <- res[[ds_id]]$stats$sigDown %||% 0
      }
      
      candidate_de_csvs <- c(
        sprintf("tmp/%s_de_results.csv", ds_id),
        sprintf("tmp/%s_de_results.csv", base_id),
        get_session_path(ds_id, "%s_de_results.csv"),
        get_session_path(base_id, "%s_de_results.csv"),
        session_file_path(ds_id, sprintf("%s_de_results.csv", ds_id)),
        session_file_path(ds_id, sprintf("%s_de_results.csv", base_id)),
        get_session_path(base_id, "%s_dp_de_results.csv")
      )
      
      de_csv <- NULL
      for (cand in candidate_de_csvs) {
        if (file.exists(cand)) {
          de_csv <- cand
          break
        }
      }

      if ((n_sig_up == 0 && n_sig_down == 0) && !is.null(de_csv) && file.exists(de_csv)) {
        de_df  <- read.csv(de_csv, check.names = FALSE, stringsAsFactors = FALSE)
        sig_col <- intersect(c("significant","sig","Significant"), colnames(de_df))[1]
        logfc_col <- intersect(c("log2FoldChange", "logFC", "logFC_Mean", "log2FC"), colnames(de_df))[1]

        if (!is.na(sig_col) && !is.na(logfc_col)) {
          sig_rows <- de_df[[sig_col]] %in% c(TRUE, "TRUE", 1, "1")
          n_sig_up <- sum(sig_rows & de_df[[logfc_col]] > 0, na.rm = TRUE)
          n_sig_down <- sum(sig_rows & de_df[[logfc_col]] < 0, na.rm = TRUE)
        } else {
          pval_col <- intersect(c("padj","adj.P.Val","FDR","p.adj"), colnames(de_df))[1]
          if (!is.na(pval_col) && !is.na(logfc_col)) {
            sig_rows <- !is.na(de_df[[pval_col]]) & as.numeric(de_df[[pval_col]]) < pval_thresh & abs(as.numeric(de_df[[logfc_col]])) >= logfc_thresh
            n_sig_up <- sum(sig_rows & de_df[[logfc_col]] > 0, na.rm = TRUE)
            n_sig_down <- sum(sig_rows & de_df[[logfc_col]] < 0, na.rm = TRUE)
          }
        }
      }

      n_sig_total <- n_sig_up + n_sig_down

      de_md <- sprintf(
        "Differential expression (DE) analysis was conducted for dataset **%s**.\n\n**DE Method:** %s\n**Comparison:** %s (Comparison Group) vs %s (Reference Group)\n**Significance Thresholds:** Adjusted p-value &le; **%s** (%s correction), |log₂ fold-change| &ge; **%s**\n\n**Results Summary:**\n- Significant differentially expressed features: %d\n- Up-regulated: %d\n- Down-regulated: %d",
        dataset_name, toupper(de_method), comp_group, ref_group, pval_thresh, adjust_method, logfc_thresh, n_sig_total, n_sig_up, n_sig_down
      )

      de_section_title <- sprintf("Differential Expression Analysis — %s", dataset_name)
      module_val <- get_backend_datasets(ds_id)$module
      if (is.null(module_val) || !nzchar(module_val)) {
        module_val <- get_backend_datasets(base_id)$module
      }
      if (is.null(module_val) || !nzchar(module_val)) {
        module_val <- if (grepl("_dp", ds_id) || grepl("_dp", base_id)) "dp" else "de"
      }
      append_step_to_report(user_id_val, de_section_title, de_md, module = module_val)
    }, error = function(e) {
      cat(sprintf("[WARNING] DE report append failed for %s: %s\n", ds_id, e$message))
    })
  }

  res
}

extract_model_names <- function(models) {
  if (is.null(models) || length(models) == 0) return(character(0))
  if (is.character(models)) return(models)
  if (is.list(models)) {
    nm <- names(models)
    if (!is.null(nm) && any(nzchar(nm))) {
      valid_nm <- nm[nzchar(nm) & !grepl("^[0-9]+$", nm)]
      if (length(valid_nm) > 0) return(valid_nm)
    }
    extracted <- sapply(models, function(item) {
      if (is.character(item)) return(item[1])
      if (is.list(item)) return(item[["model"]] %||% item[["name"]] %||% item[["id"]] %||% item[[1]])
      return(as.character(item))
    })
    return(as.character(extracted))
  }
  return(as.character(models))
}

if (!exists("get_param_val", mode = "function")) {
  get_param_val <- function(params, model_name, keys, default) {
    if (is.null(params)) return(default)
    if (!is.character(keys)) keys <- as.character(keys)
    
    # 1. Look inside params[[model_name]] sublist
    if (!is.null(params[[model_name]]) && is.list(params[[model_name]])) {
      sub <- params[[model_name]]
      for (k in keys) {
        if (!is.null(sub[[k]])) {
          val <- sub[[k]]
          if (is.numeric(default)) {
            num_v <- suppressWarnings(as.numeric(val))
            if (length(num_v) > 0 && !is.na(num_v)) return(num_v)
          }
          return(val)
        }
      }
    }
    
    # 2. Look in top-level params
    for (k in keys) {
      if (!is.null(params[[k]])) {
        val <- params[[k]]
        if (is.numeric(default)) {
          num_v <- suppressWarnings(as.numeric(val))
          if (length(num_v) > 0 && !is.na(num_v)) return(num_v)
        }
        return(val)
      }
    }
    
    return(default)
  }
}

if (!exists("get_param", mode = "function")) {
  get_param <- function(params, model_name, key, default) {
    get_param_val(params, model_name, key, default)
  }
}

if (!exists("get_dataset_data_type", mode = "function")) {
  get_dataset_data_type <- function(d) {
    if (is.character(d)) d <- list(id = d)
    if (!is.null(d$dataType) && nzchar(as.character(d$dataType))) return(tolower(as.character(d$dataType)))
    if (!is.null(d$submittedDataType) && nzchar(as.character(d$submittedDataType))) return(tolower(as.character(d$submittedDataType)))
    d_id <- d$id %||% d$datasetId
    if (!is.null(d_id) && nzchar(as.character(d_id))) {
      base_id <- get_base_id(d_id)
      meta_path <- sprintf("tmp/%s_upload_expr_metadata.rds", base_id)
      if (!file.exists(meta_path)) meta_path <- sprintf("tmp/%s_expr_metadata.rds", base_id)
      if (file.exists(meta_path)) {
        meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
        if (!is.null(meta$dataType) && nzchar(as.character(meta$dataType))) return(tolower(as.character(meta$dataType)))
      }
    }
    return("readcounts")
  }
}

finalize_fs <- function(
  datasets,
  models,
  train_ratio,
  multi_dataset_mode = "combine",
  selection_method = "breakoff",
  percentage_val   = NULL,
  max_features_val = NULL,
  fs_results       = NULL,   # Run 1 output
  refit_results    = NULL,   # Run 2 output
  cv_results       = NULL,   # Run 3 CV output
  testing_results  = NULL,   # Run 3 testing output
  user_id          = "user",
  module           = "fs",
  parameters       = NULL
) {
  # Enforce standard model order
  standard_order <- c("stabl", "boruta", "gbm", "randomforest", "logistic", "svm")
  models_clean   <- extract_model_names(models)
  models         <- intersect(standard_order, tolower(models_clean))

  MODEL_DISPLAY_NAMES <- list(
    stabl = "STABL",
    boruta = "Boruta",
    gbm = "GBM",
    randomforest = "Random Forest (RF)",
    logistic = "Logistic Regression",
    svm = "SVM"
  )

  # Check if inline FS in Data Processing module
  is_inline_dp <- identical(module, "dp") || any(grepl("_dp", sapply(datasets, function(d) d$id %||% "")))
  section_title <- if (is_inline_dp) "Inline Feature Selection" else "Feature Selection"
  module_dest   <- if (is_inline_dp) "dp" else "fs"

  first_ds_id <- if (length(datasets) > 0) datasets[[1]]$id else "fs"
  first_base_id <- get_base_id(first_ds_id)

  # Fallback to loading session results from disk if not provided in arguments
  if (is.null(fs_results) || length(fs_results) == 0) {
    saved_train_res <- get_session_path(first_base_id, "%s_fs_training_results.rds")
    if (file.exists(saved_train_res)) {
      fs_results <- tryCatch(readRDS(saved_train_res), error = function(e) NULL)
    }
  }
  if (is.null(refit_results) || length(refit_results) == 0) {
    saved_train_res <- get_session_path(first_base_id, "%s_fs_training_results.rds")
    if (file.exists(saved_train_res)) {
      refit_results <- tryCatch(readRDS(saved_train_res), error = function(e) NULL)
    }
  }
  if (is.null(cv_results) || length(cv_results) == 0) {
    cv_map <- list()
    for (d in datasets) {
      d_id <- d$id %||% d$datasetId
      cv_path <- get_session_path(get_base_id(d_id), "%s_fs_cv_results.rds")
      if (file.exists(cv_path)) {
        res_cv <- tryCatch(readRDS(cv_path), error = function(e) NULL)
        if (!is.null(res_cv)) cv_map[[d_id]] <- res_cv
      }
    }
    if (length(cv_map) > 0) cv_results <- cv_map
  }
  if (is.null(testing_results) || length(testing_results) == 0) {
    saved_test_res <- get_session_path(first_base_id, "%s_fs_testing_results.rds")
    if (file.exists(saved_test_res)) {
      testing_results <- tryCatch(readRDS(saved_test_res), error = function(e) NULL)
    }
  }
  if (is.null(parameters) || length(parameters) == 0) {
    saved_params_path <- get_session_path(first_base_id, "%s_fs_parameters.rds")
    if (file.exists(saved_params_path)) {
      parameters <- tryCatch(readRDS(saved_params_path), error = function(e) NULL)
    }
  }

  # Recover models if still empty
  if (length(models) == 0) {
    candidate_models <- c()
    if (!is.null(fs_results)) {
      for (k in names(fs_results)) {
        candidate_models <- c(candidate_models, names(fs_results[[k]]$model_features), names(fs_results[[k]]$performance_metrics), names(fs_results[[k]]$accuracies))
      }
    }
    if (!is.null(refit_results)) {
      for (k in names(refit_results)) {
        candidate_models <- c(candidate_models, names(refit_results[[k]]$model_features), names(refit_results[[k]]$performance_metrics), names(refit_results[[k]]$accuracies))
      }
    }
    if (!is.null(cv_results)) {
      for (k in names(cv_results)) {
        c_items <- if (is.list(cv_results[[k]]) && !is.null(cv_results[[k]]$metrics)) cv_results[[k]]$metrics else cv_results[[k]]
        if (is.list(c_items)) {
          for (c_it in c_items) {
            if (is.list(c_it) && !is.null(c_it$model)) candidate_models <- c(candidate_models, as.character(c_it$model))
          }
        }
      }
    }
    for (m_cand in standard_order) {
      if (file.exists(get_session_path(first_base_id, sprintf("%%s_fs_top_features_%s.rds", m_cand))) ||
          file.exists(get_session_path(first_base_id, sprintf("%%s_selected_features_%s.csv", m_cand))) ||
          file.exists(get_session_path(first_base_id, sprintf("%%s_feature_importance_%s.csv", m_cand))) ||
          file.exists(get_session_path(first_base_id, sprintf("%%s_fs_final_model_%s.rds", m_cand)))) {
        candidate_models <- c(candidate_models, m_cand)
      }
    }
    models <- intersect(standard_order, tolower(unique(candidate_models)))
  }

  # Recover parameters
  if (!is.null(parameters)) {
    selection_method <- parameters$selectionMethod %||% selection_method
    percentage_val   <- parameters$percentageCutoff %||% percentage_val
    max_features_val <- parameters$maxFeaturesSelect %||% max_features_val
  }

  # ── §0 Inline Provenance ──────────────────────────────────────────────────
  prov_md <- ""
  if (is_inline_dp) {
    dp_steps_applied <- c()
    main_stk <- if (exists("get_main_stack", mode = "function")) tryCatch(get_main_stack(first_ds_id), error = function(e) list()) else list()
    if (length(main_stk) > 0) {
      for (entry in main_stk) {
        s_name <- entry$step
        s_meta <- entry$metadata
        if (s_name == "annotation") {
          strat <- s_meta$strategy %||% "default"
          org <- s_meta$organisms %||% s_meta$organism %||% "auto"
          dp_steps_applied <- c(dp_steps_applied, sprintf("Gene Annotation (Strategy: %s, Organism: %s)", toupper(strat), org))
        } else if (s_name == "processing") {
          dp_steps_applied <- c(dp_steps_applied, "Data Preprocessing & Quality Filtering (Low-count gene removal)")
        } else if (s_name == "normalization") {
          m_val <- s_meta$method %||% "TMM"
          dp_steps_applied <- c(dp_steps_applied, sprintf("Expression Normalization (Method: %s)", toupper(m_val)))
        } else if (s_name == "batch") {
          b_val <- s_meta$method %||% "ComBat"
          dp_steps_applied <- c(dp_steps_applied, sprintf("Within-Dataset Batch Correction (Method: %s)", b_val))
        }
      }
    }
    if (length(dp_steps_applied) == 0) {
      dp_steps_applied <- c("Expression matrix loaded from upstream Data Processing pipeline")
    }

    prov_md <- paste0(
      "#### Upstream Data Processing Provenance\n\n",
      "Inline Feature Selection was executed on the processed expression matrix generated from the following upstream Data Processing steps:\n\n",
      paste(sapply(dp_steps_applied, function(s) paste0("- **", s, "**")), collapse = "\n"), "\n\n",
      "> [!NOTE]\n",
      "> **Distinction on Batch Operations**: Batch Effect Correction performed in the Data Processing module addresses technical/experimental batch variation within a dataset (e.g. across plates/runs). In contrast, Batch Harmonisation in Feature Selection (Multi-Dataset Pooling) is designed to harmonize multiple distinct study cohorts on their shared feature space prior to biomarker discovery.\n\n"
    )
  }

  # ── §1 Data Preparation & Study Design ──────────────────────────────────
  # Classify datasets into train and test
  train_dss <- list()
  test_dss  <- list()
  for (d in datasets) {
    d_id <- d$id %||% d$datasetId
    base_id <- get_base_id(d_id)
    fs_meta_path <- get_session_path(base_id, "%s_fs_meta.rds")
    saved_meta <- if (file.exists(fs_meta_path)) tryCatch(readRDS(fs_meta_path), error = function(e) NULL) else NULL
    purpose <- d$datasetPurpose %||% d$fs_datasetPurpose %||% saved_meta$datasetPurpose %||% "train-and-test"
    if (purpose == "test") {
      test_dss[[d_id]] <- d
    } else {
      train_dss[[d_id]] <- d
    }
  }
  if (length(train_dss) == 0 && length(datasets) > 0) {
    train_dss[[datasets[[1]]$id %||% datasets[[1]]$datasetId]] <- datasets[[1]]
  }

  dt_types <- unique(sapply(datasets, function(d) get_dataset_data_type(d)))
  is_same_type <- length(dt_types) == 1 && dt_types[1] != "others"
  is_pooled <- identical(multi_dataset_mode, "combine") && length(train_dss) > 1 && is_same_type
  is_cross_type <- length(test_dss) > 0

  prep_topology_md <- ""
  if (is_pooled) {
    # Topology 2: Pooled Multi-Dataset Cohort
    pooled_names <- sapply(train_dss, function(d) d$name %||% d$id)
    prep_topology_md <- paste0(
      "**Execution Topology:** **Pooled / Harmonised Multi-Dataset Cohort**  \n",
      sprintf("- **Constituent Cohorts (%d):** %s\n", length(train_dss), paste(pooled_names, collapse = ", ")),
      sprintf("- **Data Type:** %s\n", toupper(dt_types[1])),
      "- **Feature Space Alignment:** Exact intersection of common gene identifiers across all pooled datasets.\n",
      "- **Cross-Study Batch Harmonisation:** Empirical Bayes framework via **ComBat** (`sva::ComBat`, R package `sva`), using dataset identifiers as batch covariates to eliminate cross-study technical batch effects prior to data partitioning.\n",
      sprintf("- **Cohort Partitioning:** Harmonised matrix partitioned into **%.0f%% training split** (for discovery and model training) and **%.0f%% held-out test split** (for multi-cohort validation).\n",
              train_ratio * 100, (1 - train_ratio) * 100)
    )
  } else if (is_cross_type) {
    # Topology 3: Cross-Type / External Validation Cohorts
    train_names <- sapply(train_dss, function(d) d$name %||% d$id)
    test_names  <- sapply(test_dss, function(d) d$name %||% d$id)
    prep_topology_md <- paste0(
      "**Execution Topology:** **Cross-Type / External Validation Study**  \n",
      sprintf("- **Discovery / Training Cohort(s):** %s (Role: Biomarker discovery, importance ranking, and primary model fitting)\n", paste(train_names, collapse = ", ")),
      sprintf("- **External Validation Cohort(s):** %s (Role: Independent external validation testing)\n", paste(test_names, collapse = ", ")),
      sprintf("- **Discovery Partitioning:** Stratified **%.0f%% training split** / **%.0f%% internal held-out test split** on the discovery cohort.\n",
              train_ratio * 100, (1 - train_ratio) * 100),
      "- **Feature Space Transfer:** Discovered biomarker panel from the Discovery Cohort projected onto the External Validation Cohort's feature space for independent generalizability evaluation.\n"
    )
  } else {
    # Topology 1: Independent Discovery
    ds_names <- sapply(datasets, function(d) d$name %||% d$id)
    prep_topology_md <- paste0(
      "**Execution Topology:** **Independent Cohort Discovery**  \n",
      sprintf("- **Input Cohort(s) (%d):** %s\n", length(datasets), paste(ds_names, collapse = ", ")),
      sprintf("- **Data Type(s):** %s\n", paste(toupper(dt_types), collapse = ", ")),
      sprintf("- **Partitioning Scheme:** Stratified **%.0f%% training split** (for biomarker discovery and model training) and **%.0f%% held-out test split** (for internal validation).\n",
              train_ratio * 100, (1 - train_ratio) * 100),
      "- **Analysis Scope:** Each dataset was processed and evaluated independently as an autonomous entity.\n"
    )
  }

  prep_md <- paste0("### 1. Data Preparation & Study Design\n\n", prov_md, prep_topology_md)

  # ── §2 Feature Discovery (Run 1) ─────────────────────────────────────────
  # Extract per-model hyperparameters configured by the user (with robust defaults)
  stabl_base_raw     <- get_param_val(parameters, "stabl", c("base_estimator", "baseEstimator"), "Logistic L1")
  stabl_is_elastic   <- grepl("elastic", stabl_base_raw, ignore.case = TRUE)
  stabl_base_clean   <- if (stabl_is_elastic) "Logistic Regression with Elastic-Net Penalty" else "Logistic Regression with L1 (LASSO) Penalty"
  stabl_alpha        <- get_param_val(parameters, "stabl", c("alpha", "l1_ratio"), 1.0)
  stabl_lambda       <- get_param_val(parameters, "stabl", c("lambda", "refit_lambda", "penalty"), 0.01)
  stabl_max_iter     <- get_param_val(parameters, "stabl", c("max_iter", "maxit", "max_iterations"), 1000)
  stabl_n_bootstraps <- get_param_val(parameters, "stabl", c("n_bootstraps", "nBootstraps", "n_subsamples", "bootstraps"), 300)
  stabl_art_raw      <- get_param_val(parameters, "stabl", c("artificial_type", "artificialType"), "random_permutation")
  stabl_art_clean    <- if (grepl("knockoff", stabl_art_raw, ignore.case = TRUE)) "Model-X Knockoffs (preserving feature correlation structure)" else "Random Permutation (shuffling feature values across samples)"
  stabl_art_prop     <- get_param_val(parameters, "stabl", c("artificial_proportion", "artificialProportion"), 1.0)
  stabl_fdr          <- get_param_val(parameters, "stabl", c("fdr_threshold", "fdrThreshold", "fdr"), 0.1)
  stabl_hard_thr     <- get_param_val(parameters, "stabl", c("hard_threshold", "hardThreshold"), 0.5)
  stabl_refit_max_depth   <- get_param_val(parameters, "stabl", c("refit_max_depth", "max_depth", "maxDepth"), 10)
  stabl_refit_max_depth_clamped <- max(1, min(20, as.integer(stabl_refit_max_depth)))
  stabl_refit_ntree       <- get_param_val(parameters, "stabl", c("refit_n_estimators", "refit_num_trees", "n_estimators", "num_trees"), 500)

  boruta_max_runs           <- get_param_val(parameters, "boruta", c("max_runs", "maxRuns", "runs"), 100)
  boruta_pval               <- get_param_val(parameters, "boruta", c("p_value", "pValue", "pvalue"), 0.01)
  boruta_max_depth          <- get_param_val(parameters, "boruta", c("max_depth", "maxDepth", "max.depth"), 10)
  boruta_max_depth_clamped  <- max(1, min(20, as.integer(boruta_max_depth)))
  boruta_ntree              <- get_param_val(parameters, "boruta", c("n_estimators", "nEstimators", "num_trees", "num.trees", "ntree"), 500)
  boruta_keep_tentative     <- get_param_val(parameters, "boruta", c("keep_tentative", "keepTentative"), "no")

  rf_num_trees              <- get_param_val(parameters, "randomforest", c("num_trees", "num.trees", "n_estimators", "ntree"), 500)
  rf_max_depth              <- get_param_val(parameters, "randomforest", c("max_depth", "maxDepth", "max.depth"), 10)
  rf_max_depth_clamped      <- max(1, min(20, as.integer(rf_max_depth)))

  gbm_ntrees                <- get_param_val(parameters, "gbm", c("n.trees", "n_trees", "num_trees", "n_estimators"), 500)
  gbm_interaction_depth     <- get_param_val(parameters, "gbm", c("interaction.depth", "interaction_depth", "max_depth"), 3)
  gbm_interaction_depth_cl  <- max(1, min(20, as.integer(gbm_interaction_depth)))
  gbm_shrinkage             <- get_param_val(parameters, "gbm", c("shrinkage", "learning_rate", "eta"), 0.1)
  gbm_min_obs               <- get_param_val(parameters, "gbm", c("n.minobsinnode", "n_minobsinnode", "min_obs", "min_node_size"), 10)

  log_lambda                <- get_param_val(parameters, "logistic", c("lambda", "penalty"), 0.1)
  log_alpha                 <- get_param_val(parameters, "logistic", c("alpha", "l1_ratio"), 1.0)
  log_max_iter              <- get_param_val(parameters, "logistic", c("max_iter", "maxit", "max_iterations"), 1000)

  svm_c                     <- get_param_val(parameters, "svm", c("C", "cost", "cost_c"), 1.0)
  svm_weight                <- get_param_val(parameters, "svm", c("weight", "class_weight"), 1.0)

  model_detail_map <- list(
    stabl = paste0(
      "**STABL (Stability Biomarker Selection)** (`stabl` Python package via `reticulate` bridge):\n",
      "  - *Algorithm Framework:* High-dimensional stability selection with False Discovery Rate (FDR) control via synthetic decoy features.\n",
      "  - *Configured Hyperparameters (Biomarker Discovery):*\n",
      "    - **Base Estimator:** ", stabl_base_clean, " (`", stabl_base_raw, "`)\n",
      "    - **L1/L2 Elastic-Net Mixing Ratio (alpha):** ", stabl_alpha, if (!stabl_is_elastic) " (LASSO L1 penalty)" else " (Elastic-Net penalty)", "\n",
      "    - **Bootstrap Subsampling Iterations (n_bootstraps):** ", stabl_n_bootstraps, " subsampling iterations (50% sampling rate)\n",
      "    - **Artificial (Decoy) Feature Generator (artificial_type):** ", stabl_art_clean, "\n",
      "    - **Artificial Feature Proportion (artificial_proportion):** ", stabl_art_prop, " (", round(as.numeric(stabl_art_prop) * 100), "% of real feature space)\n",
      "    - **Max Iterations per Subsample (max_iter):** ", stabl_max_iter, "\n",
      "  - *Refit Classifier Framework (Final Model Retraining):*\n",
      "    - **Refit Method:** Random Forest (`caret` R package, `method = \"ranger\"`, `ranger` engine)\n",
      "    - **Refit Max Tree Depth (refit_max_depth):** ", stabl_refit_max_depth_clamped, "\n",
      "    - **Refit Number of Trees (refit_n_estimators):** ", stabl_refit_ntree, " trees\n",
      "  - *Selection Mechanism:* Computes empirical selection frequencies along regularisation paths over ", stabl_n_bootstraps, " bootstrap iterations and compares them against decoy noise features to enforce the controlled FDR threshold (&alpha; = ", stabl_fdr, ").\n",
      "  - *Output:* Biomarker signature consisting of stable features exceeding the controlled FDR threshold, followed by refitting with Random Forest (`ranger`)."
    ),
    boruta = paste0(
      "**Boruta (All-Relevant Feature Selection)** (`Boruta` R package, `ranger` engine):\n",
      "  - *Algorithm Framework:* Wrapper-based all-relevant feature selection using Random Forest and randomized shadow attributes.\n",
      "  - *Configured Hyperparameters:*\n",
      "    - **Max Iterations (max_runs):** ", boruta_max_runs, " runs\n",
      "    - **Confidence Cutoff (p_value):** p &le; ", boruta_pval, "\n",
      "    - **Max Tree Depth (max_depth):** ", boruta_max_depth_clamped, "\n",
      "    - **Number of Trees per Forest (n_estimators / num.trees):** ", boruta_ntree, " trees\n",
      "    - **Keep Tentative Features (keep_tentative):** ", toupper(as.character(boruta_keep_tentative)), "\n",
      "  - *Selection Mechanism:* Generates permuted shadow attributes, trains Random Forests (", boruta_ntree, " trees) to compute Z-scores of Gini Mean Decrease Impurity (MDI), and statistically compares real feature Z-scores against the maximum shadow feature Z-score (Z_max) over ", boruta_max_runs, " iterations.\n",
      "  - *Decision Boundary:* Features scoring significantly higher than Z_max (two-sided statistical test, p &le; ", boruta_pval, ") are *Confirmed* (plus *Tentative* if keep_tentative = YES); features scoring significantly lower are *Rejected*.\n",
      "  - *Output:* All-relevant biomarker signature of confirmed features."
    ),
    randomforest = paste0(
      "**Random Forest (RF)** (`caret` R package, `method = \"ranger\"`, `ranger` engine):\n",
      "  - *Algorithm Framework:* Non-parametric ensemble of de-correlated decision trees.\n",
      "  - *Configured Hyperparameters:*\n",
      "    - **Number of Trees (num_trees):** ", rf_num_trees, " trees\n",
      "    - **Max Tree Depth (max_depth):** ", rf_max_depth_clamped, "\n",
      "    - **Variables per Split (mtry):** Dynamic $\\max(1, \\lfloor\\sqrt{p}\\rfloor)$ for $p$ input features\n",
      "    - **Splitting Rule:** Gini Impurity (`splitrule = \"gini\"`)\n",
      "    - **Min Node Size:** `min.node.size = 1`\n",
      "  - *Scoring Protocol:* Feature importance is evaluated via Gini Mean Decrease in Impurity (MDI), normalized to sum to 1.0."
    ),
    gbm = paste0(
      "**Gradient Boosting Model (GBM)** (`caret` R package, `method = \"gbm\"`, `gbm` engine):\n",
      "  - *Algorithm Framework:* Sequential ensemble of boosted decision trees minimizing deviance loss.\n",
      "  - *Configured Hyperparameters:*\n",
      "    - **Boosting Iterations (n.trees):** ", gbm_ntrees, " iterations\n",
      "    - **Max Interaction Depth (interaction.depth):** ", gbm_interaction_depth_cl, "\n",
      "    - **Learning Rate / Shrinkage (shrinkage):** ", gbm_shrinkage, "\n",
      "    - **Min Terminal Node Size (n.minobsinnode):** ", gbm_min_obs, " observations\n",
      "    - **Subsampling Fraction (bag.fraction):** Adaptive (0.5 default; 1.0 for small sample sizes)\n",
      "  - *Scoring Protocol:* Feature importance is evaluated by the total reduction in Bernoulli deviance loss contributed by each feature split (relative influence normalized to sum to 1.0)."
    ),
    logistic = paste0(
      "**Logistic Regression** (`caret` R package, `method = \"glmnet\"`, `glmnet` engine):\n",
      "  - *Algorithm Framework:* Penalized Generalized Linear Model with Elastic-Net regularisation.\n",
      "  - *Configured Hyperparameters:*\n",
      "    - **Regularization Penalty (lambda):** ", log_lambda, "\n",
      "    - **L1/L2 Elastic-Net Mixing Ratio (alpha):** ", log_alpha, if (as.numeric(log_alpha) == 1.0) " (LASSO L1 penalty)" else if (as.numeric(log_alpha) == 0.0) " (Ridge L2 penalty)" else " (Elastic-Net penalty)", "\n",
      "    - **Max Iterations (max_iter / maxit):** ", log_max_iter, "\n",
      "    - **Classification Family:** binomial (binary)\n",
      "  - *Scoring Protocol:* Feature importance is evaluated via standardized regression coefficients from regularized fits, normalized to sum to 1.0."
    ),
    svm = paste0(
      "**Support Vector Machine (SVM)** (`caret` R package, `method = \"svmLinearWeights\"`):\n",
      "  - *Algorithm Framework:* Linear Support Vector Classifier with class-weighted penalty optimization.\n",
      "  - *Configured Hyperparameters:*\n",
      "    - **Regularization Cost Parameter (C / cost):** ", svm_c, "\n",
      "    - **Class Weight Multiplier (weight):** ", svm_weight, "\n",
      "    - **Kernel Function:** Linear Kernel\n",
      "  - *Scoring Protocol:* Feature importance is evaluated via the absolute linear weight vector |w| perpendicular to the optimal separating hyperplane, normalized to sum to 1.0."
    )
  )

  model_lines <- sapply(models, function(m) {
    desc <- model_detail_map[[tolower(m)]] %||%
      sprintf("- **%s**: Feature selection performed with default configuration.", MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m))
    paste0("- ", desc)
  })

  disc_md <- paste0(
    "### 2. Biomarker Discovery & Feature Importance Scoring (Run 1)\n\n",
    "Each model was fitted on the **training split** to generate feature importance rankings (standard ML models) or definitive stability-selected biomarker signatures (STABL, Boruta).\n\n",
    "#### Model Architectures & Configured Hyperparameters\n\n",
    if (length(model_lines) > 0) paste(model_lines, collapse = "\n\n") else "*Standard classification models configured for feature selection.*", "\n"
  )

  # Feature counts table from fs_results / refit_results / disk
  res_for_counts <- refit_results %||% fs_results
  target_d_ids <- if (!is.null(res_for_counts) && length(res_for_counts) > 0) names(res_for_counts) else sapply(datasets, function(d) d$id %||% d$datasetId)
  if (length(target_d_ids) > 0 && length(models) > 0) {
    count_rows <- c()
    for (d_id in target_d_ids) {
      ds_name <- d_id
      for (d in datasets) {
        if (identical(d$id %||% d$datasetId, d_id)) { ds_name <- d$name %||% d$id; break }
      }
      row_vals <- sapply(models, function(m) {
        if (!is.null(fs_results[[d_id]]$model_features[[m]]) && length(fs_results[[d_id]]$model_features[[m]]) > 0) {
          return(as.character(length(fs_results[[d_id]]$model_features[[m]])))
        }
        if (!is.null(refit_results[[d_id]]$model_features[[m]]) && length(refit_results[[d_id]]$model_features[[m]]) > 0) {
          return(as.character(length(refit_results[[d_id]]$model_features[[m]])))
        }
        top_f_path <- get_session_path(get_base_id(d_id), sprintf("%%s_fs_top_features_%s.rds", m))
        if (file.exists(top_f_path)) {
          top_f <- tryCatch(readRDS(top_f_path), error = function(e) NULL)
          if (!is.null(top_f) && length(top_f) > 0) return(as.character(length(top_f)))
        }
        sel_csv_path <- get_session_path(get_base_id(d_id), sprintf("%%s_selected_features_%s.csv", m))
        if (file.exists(sel_csv_path)) {
          sel_csv <- tryCatch(read.csv(sel_csv_path, header = TRUE), error = function(e) NULL)
          if (!is.null(sel_csv) && nrow(sel_csv) > 0) return(as.character(nrow(sel_csv)))
        }
        if (!is.null(res_for_counts[[d_id]]$features) && length(res_for_counts[[d_id]]$features) > 0) {
          return(as.character(length(res_for_counts[[d_id]]$features)))
        }
        return("—")
      })
      count_rows <- c(count_rows,
        paste0("| ", ds_name, " | ", paste(row_vals, collapse = " | "), " |"))
    }
    if (length(count_rows) > 0) {
      model_headers <- sapply(models, function(m) MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m))
      disc_md <- paste0(disc_md,
        "\n#### Discovered Biomarker Counts (Run 1)\n\n",
        paste0("| Dataset | ", paste(model_headers, collapse = " | "), " |\n"),
        paste0("|", paste(rep("---|", length(models) + 1), collapse = ""), "\n"),
        paste(count_rows, collapse = "\n"), "\n")
    }
  }

  # ── §3 Feature Selection & Refit (Run 2) ─────────────────────────────────
  pct_disp <- if (!is.null(percentage_val) && !is.na(as.numeric(percentage_val))) as.numeric(percentage_val) else 80
  max_disp <- if (!is.null(max_features_val) && !is.na(as.numeric(max_features_val))) as.integer(max_features_val) else 10

  sel_method_desc_short <- switch(selection_method,
    "breakoff"     = "Break-off / Elbow Point detection (piecewise linear regression breakpoint)",
    "percentage"   = sprintf("Top-%.0f%% cumulative importance score percentile cutoff", pct_disp),
    "max_features" = sprintf("Top-%d absolute ranked features cutoff", max_disp),
    "overlap"      = "Model consensus overlap (strict intersection of independently selected signatures)",
    selection_method
  )

  sel_method_desc <- switch(selection_method,
    "breakoff"     = paste0("**Break-off Point Detection** (`segmented` R package, `segmented::segmented()`): ",
                            "A piecewise linear regression model is fitted to the sorted descending importance curve; ",
                            "the estimated inflection breakpoint determines the cut-off index of diminishing returns."),
    "percentage"   = sprintf("**Top-%s%%%% Percentile Cutoff**: Retains features whose importance score exceeds the %s-th percentile of the empirical importance score distribution.", pct_disp, pct_disp),
    "max_features" = sprintf("**Max %d Features Cutoff**: Retains the top %d ranked features by absolute importance.",
                             max_disp, max_disp),
    "overlap"      = paste0("**Model Consensus / Overlap**: Takes the strict intersection of biomarker sets independently selected ",
                            "by each model (using the break-off point baseline per model) to generate a high-confidence consensus signature."),
    selection_method
  )

  refit_desc_map <- list(
    stabl = paste0(
      "- **STABL (Stability Selection)** → **Refitted from Identified Stable Features Set**:\n",
      "  - *Identified Feature Set:* Discovered biomarker signature consisting of stable features exceeding the FDR threshold (&alpha; = ", stabl_fdr, ") in Run 1.\n",
      "  - *Refit Protocol:* Upon identifying the stable features set, the final classifier was refitted on the training split using the identified stable features only.\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Random Forest (`caret` R package, `method = \"ranger\"`, `ranger` engine)\n",
      "    - **Number of Trees (refit_n_estimators / num.trees):** ", stabl_refit_ntree, " trees\n",
      "    - **Max Tree Depth (refit_max_depth):** ", stabl_refit_max_depth_clamped, "\n",
      "    - **Variables per Split (mtry):** Dynamic $\\max(1, \\lfloor\\sqrt{p}\\rfloor)$ for $p$ identified features\n",
      "    - **Splitting Rule:** Gini Impurity (`splitrule = \"gini\"`)\n",
      "    - **Minimum Node Size:** `min.node.size = 1`\n",
      "    - **Importance Metric:** Impurity (`importance = \"impurity\"`)"
    ),
    boruta = paste0(
      "- **Boruta (All-Relevant Selection)** → **Refitted from Identified All-Relevant Features Set**:\n",
      "  - *Identified Feature Set:* All-relevant confirmed biomarker attributes scoring significantly higher than shadow attributes (p &le; ", boruta_pval, if (identical(tolower(as.character(boruta_keep_tentative)), "yes")) ", including tentative features" else "", ") in Run 1.\n",
      "  - *Refit Protocol:* Upon identifying the confirmed feature set, the final classifier was refitted on the training split using the confirmed feature subset only.\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Random Forest (`ranger::ranger`, `classification = TRUE`)\n",
      "    - **Number of Trees (num.trees / n_estimators):** ", boruta_ntree, " trees\n",
      "    - **Max Tree Depth (max.depth):** ", boruta_max_depth_clamped, "\n",
      "    - **Variables per Split (mtry):** Dynamic $\\max(1, \\lfloor\\sqrt{p}\\rfloor)$ for $p$ identified features\n",
      "    - **Splitting Rule:** Gini Impurity (`splitrule = \"gini\"`)\n",
      "    - **Minimum Node Size:** `min.node.size = 1`\n",
      "    - **Importance Metric:** Impurity (`importance = \"impurity\"`)"
    ),
    randomforest = paste0(
      "- **Random Forest (RF)** → **Refitted on Selected Feature Subset**:\n",
      "  - *Selection Method:* Features filtered via ", sel_method_desc_short, ".\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Random Forest (`ranger::ranger`, `caret::train`, method = `\"ranger\"`)\n",
      "    - **Number of Trees (num_trees):** ", rf_num_trees, " trees\n",
      "    - **Max Tree Depth (max_depth):** ", rf_max_depth_clamped, "\n",
      "    - **Variables per Split (mtry):** Dynamic $\\max(1, \\lfloor\\sqrt{k}\\rfloor)$ for $k$ selected features\n",
      "    - **Splitting Rule:** Gini Impurity (`splitrule = \"gini\"`)\n",
      "    - **Minimum Node Size:** `min.node.size = 1`"
    ),
    gbm = paste0(
      "- **Gradient Boosting Model (GBM)** → **Refitted on Selected Feature Subset**:\n",
      "  - *Selection Method:* Features filtered via ", sel_method_desc_short, ".\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Gradient Boosting Machine (`gbm::gbm`, `caret::train`, method = `\"gbm\"`)\n",
      "    - **Boosting Iterations (n.trees):** ", gbm_ntrees, " iterations\n",
      "    - **Max Interaction Depth (interaction.depth):** ", gbm_interaction_depth_cl, "\n",
      "    - **Learning Rate / Shrinkage (shrinkage):** ", gbm_shrinkage, "\n",
      "    - **Min Terminal Node Size (n.minobsinnode):** ", gbm_min_obs, " observations\n",
      "    - **Subsampling Fraction (bag.fraction):** Adaptive (0.5 default; 1.0 for small sample sizes)"
    ),
    logistic = paste0(
      "- **Logistic Regression** → **Refitted on Selected Feature Subset**:\n",
      "  - *Selection Method:* Features filtered via ", sel_method_desc_short, ".\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Penalized Logistic Regression (`glmnet::glmnet`, `caret::train`, method = `\"glmnet\"`)\n",
      "    - **L1/L2 Elastic-Net Mixing Ratio (alpha):** ", log_alpha, if (as.numeric(log_alpha) == 1.0) " (LASSO L1 penalty)" else if (as.numeric(log_alpha) == 0.0) " (Ridge L2 penalty)" else " (Elastic-Net penalty)", "\n",
      "    - **Regularization Penalty (lambda):** ", log_lambda, "\n",
      "    - **Max Iterations (maxit):** ", log_max_iter, "\n",
      "    - **Classification Family:** binomial (binary)"
    ),
    svm = paste0(
      "- **Support Vector Machine (SVM)** → **Refitted on Selected Feature Subset**:\n",
      "  - *Selection Method:* Features filtered via ", sel_method_desc_short, ".\n",
      "  - *Refitted Architecture & Hyperparameters:*\n",
      "    - **Refit Model:** Linear Support Vector Machine (`caret::train`, method = `\"svmLinearWeights\"`)\n",
      "    - **Regularization Cost Parameter (C / cost):** ", svm_c, "\n",
      "    - **Class Weight Multiplier (weight):** ", svm_weight, "\n",
      "    - **Kernel Function:** Linear"
    )
  )

  refit_desc <- paste(sapply(models, function(m) {
    refit_desc_map[[tolower(m)]] %||%
      sprintf("- **%s** → Refitted with corresponding classifier on selected features.", MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m))
  }), collapse = "\n\n")

  refit_md <- paste0(
    "### 3. Feature Selection & Final Model Refitting (Run 2)\n\n",
    "For standard machine learning models, features were filtered using the following selection threshold:\n\n",
    "- ", sel_method_desc, "\n\n",
    "> [!NOTE]\n",
    "> **Intrinsic Selection in STABL & Boruta**: STABL and Boruta intrinsically determine their definitive feature signatures during Run 1 (via False Discovery Rate control against decoy noise features and shadow attribute statistical tests, respectively). Upon identifying these feature sets, the final predictive classifiers were retrained directly on the identified feature subsets.\n\n",
    "#### Final Model Retraining on Selected Signatures\n\n",
    if (nzchar(refit_desc)) refit_desc else "*Models refitted on selected biomarker signatures.*", "\n"
  )

  # Training performance table from refit_results / fs_results
  res_for_perf <- refit_results %||% fs_results
  if (!is.null(res_for_perf) && length(res_for_perf) > 0 && length(models) > 0) {
    perf_rows <- c()
    for (d_id in names(res_for_perf)) {
      ds_name <- d_id
      for (d in datasets) {
        if (identical(d$id %||% d$datasetId, d_id)) { ds_name <- d$name %||% d$id; break }
      }
      ds_res <- res_for_perf[[d_id]]
      for (m in models) {
        pm <- ds_res$performance_metrics[[m]]
        if (is.null(pm)) {
          all_perf_path <- get_session_path(get_base_id(d_id), "%s_all_models_performance_train.csv")
          if (file.exists(all_perf_path)) {
            df_perf <- tryCatch(read.csv(all_perf_path, header = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
            if (!is.null(df_perf) && nrow(df_perf) > 0) {
              row_m <- df_perf[tolower(df_perf$Model) == tolower(m) | tolower(df_perf$Model) == tolower(MODEL_DISPLAY_NAMES[[m]]), ]
              if (nrow(row_m) > 0) {
                pm <- list(
                  auc = row_m$AUC[1],
                  ci  = row_m[["95% CI (AUC)"]][1] %||% row_m$AUC_95_CI[1],
                  acc = row_m$Accuracy[1] %||% row_m$Balanced_Accuracy[1],
                  ppv = row_m$PPV[1],
                  npv = row_m$NPV[1]
                )
              }
            }
          }
        }
        
        n_sel <- length(ds_res$model_features[[m]] %||% list())
        if (n_sel == 0) {
          top_f_path <- get_session_path(get_base_id(d_id), sprintf("%%s_fs_top_features_%s.rds", m))
          if (file.exists(top_f_path)) {
            top_f <- tryCatch(readRDS(top_f_path), error = function(e) NULL)
            if (!is.null(top_f)) n_sel <- length(top_f)
          }
          if (n_sel == 0 && !is.null(ds_res$features)) n_sel <- length(ds_res$features)
        }
        
        if (!is.null(pm)) {
          auc_ci_str <- if (!is.null(pm$ci) && nzchar(as.character(pm$ci)) && !identical(as.character(pm$ci), "N/A")) {
            sprintf("%s %s", pm$auc %||% "N/A", pm$ci)
          } else {
            as.character(pm$auc %||% "N/A")
          }
          perf_rows <- c(perf_rows, sprintf(
            "| %s | %s | %d | %s | %s | %s | %s |",
            ds_name, MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m), n_sel,
            auc_ci_str, pm$acc %||% pm$bacc %||% "N/A",
            pm$ppv %||% "N/A", pm$npv %||% "N/A"))
        }
      }
    }
    if (length(perf_rows) > 0) {
      refit_md <- paste0(refit_md,
        "\n#### Training Performance (Evaluated on Training Split)\n\n",
        "| Dataset | Model | Selected Features | AUC (95% CI) | Balanced Acc. | PPV | NPV |\n",
        "|---|---|---|---|---|---|---|\n",
        paste(perf_rows, collapse = "\n"), "\n")
    }
  }

  # ── §4 Comprehensive Model Evaluation (Run 3) ───────────────────────────
  eval_md <- "### 4. Comprehensive Model Evaluation (Run 3)\n\n"

  # 4a CV
  has_cv <- !is.null(cv_results) && length(cv_results) > 0
  if (has_cv) {
    cv_table_rows <- c()
    for (d_id in names(cv_results)) {
      ds_name <- d_id
      for (d in datasets) { if (identical(d$id %||% d$datasetId, d_id)) { ds_name <- d$name %||% d$id; break } }
      raw_cv <- cv_results[[d_id]]
      cv_items <- if (is.list(raw_cv) && !is.null(raw_cv$metrics) && is.list(raw_cv$metrics)) raw_cv$metrics else raw_cv
      if (is.list(cv_items)) {
        for (cv_r in cv_items) {
          if (!is.list(cv_r) || is.null(cv_r$model) || length(cv_r$model) == 0 || !nzchar(as.character(cv_r$model))) next
          m_key <- tolower(as.character(cv_r$model))
          m_disp <- MODEL_DISPLAY_NAMES[[m_key]] %||% toupper(m_key)
          if (!is.null(cv_r$.error)) {
            cv_table_rows <- c(cv_table_rows, sprintf("| %s | %s | Failed: %s | | | |\n",
                                                       ds_name, m_disp, cv_r$.error))
          } else {
            auc_ci_str <- if (!is.null(cv_r$ci) && nzchar(as.character(cv_r$ci)) && !identical(as.character(cv_r$ci), "N/A")) {
              sprintf("%s %s", cv_r$bestScore %||% cv_r$auc %||% "N/A", cv_r$ci)
            } else if (!is.null(cv_r$std) && nzchar(as.character(cv_r$std)) && !identical(as.character(cv_r$std), "N/A")) {
              sprintf("%s ± %s", cv_r$bestScore %||% cv_r$auc %||% "N/A", cv_r$std)
            } else {
              as.character(cv_r$bestScore %||% cv_r$auc %||% "N/A")
            }
            cv_table_rows <- c(cv_table_rows, sprintf("| %s | %s | %s | %s | %s | %s |\n",
              ds_name, m_disp,
              auc_ci_str,
              cv_r$accuracy  %||% cv_r$acc %||% "N/A",
              cv_r$ppv       %||% "N/A", cv_r$npv %||% "N/A"))
          }
        }
      }
    }
    if (length(cv_table_rows) > 0) {
      eval_md <- paste0(eval_md, "#### 4a. Cross-Validation Performance (Training Split Resampling)\n\n",
        "**Method:** `caret::trainControl` (k-fold CV / LOOCV)  \n",
        "**Evaluated on:** Training split only\n\n",
        "| Dataset | Model | Mean AUC (95% CI) | Balanced Acc. | PPV | NPV |\n",
        "|---|---|---|---|---|---|\n",
        paste(cv_table_rows, collapse = ""))
    }
  }

  # 4b/4c Testing
  has_testing <- !is.null(testing_results) && length(testing_results) > 0
  if (has_testing) {
    test_sec_title <- if (is_cross_type) "#### 4b. Independent External Validation Performance" else "#### 4b. Independent Validation / Held-Out Test Performance"
    test_table_rows <- c()
    for (d_id in names(testing_results)) {
      ds_name <- d_id
      d_purpose <- "Internal Test Split (30%)"
      for (d in datasets) {
        if (identical(d$id %||% d$datasetId, d_id)) {
          ds_name <- d$name %||% d$id
          if (identical(d$datasetPurpose %||% d$fs_datasetPurpose, "test")) {
            d_purpose <- "External Validation Cohort"
          }
          break
        }
      }
      ds_res <- testing_results[[d_id]]
      if (is.list(ds_res) && !is.null(ds_res$metrics) && is.list(ds_res$metrics)) {
        ds_res <- ds_res$metrics
      }
      for (m in models) {
        m_res <- ds_res[[m]]
        if (is.null(m_res) && is.list(ds_res)) {
          for (item in ds_res) {
            if (is.list(item) && identical(tolower(as.character(item$model)), tolower(m))) {
              m_res <- item
              break
            }
          }
        }
        if (!is.null(m_res)) {
          if (is.list(m_res) && !is.null(m_res$.error)) {
            test_table_rows <- c(test_table_rows, sprintf("| %s | %s | %s | Failed: %s | | | |\n",
                                                           ds_name, MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m), d_purpose, m_res$.error))
          } else {
            auc_ci_str <- if (!is.null(m_res$ci) && nzchar(as.character(m_res$ci)) && !identical(as.character(m_res$ci), "N/A")) {
              sprintf("%s %s", m_res$auc %||% "N/A", m_res$ci)
            } else {
              as.character(m_res$auc %||% "N/A")
            }
            test_table_rows <- c(test_table_rows, sprintf("| %s | %s | %s | %s | %s | %s | %s |\n",
              ds_name, MODEL_DISPLAY_NAMES[[tolower(m)]] %||% toupper(m), d_purpose,
              auc_ci_str, m_res$acc %||% m_res$bacc %||% "N/A",
              m_res$ppv %||% "N/A", m_res$npv %||% "N/A"))
          }
        }
      }
    }
    if (length(test_table_rows) > 0) {
      eval_md <- paste0(eval_md, "\n", test_sec_title, "\n\n",
        "| Dataset | Model | Cohort Role | AUC (95% CI) | Balanced Acc. | PPV | NPV |\n",
        "|---|---|---|---|---|---|---|\n",
        paste(test_table_rows, collapse = ""))
    }
  }

  if (!has_cv && !has_testing) {
    eval_md <- paste0(eval_md, "*No cross-validation or testing evaluations have been performed yet.*")
  }

  full_md <- paste(prep_md, disc_md, refit_md, eval_md, sep = "\n\n")
  append_step_to_report(user_id, section_title, full_md, module = module_dest)
  invisible(TRUE)
}

finalize_fs_refit <- function(res, orig_out_path, input_data, selection_method, percentage_val, max_features_val, user_id, module = "fs") {
  if (!is.null(res$.error)) return(res)
  
  orig_fs_results <- NULL
  if (!is.null(orig_out_path) && nzchar(orig_out_path) && file.exists(orig_out_path)) {
    orig_fs_results <- tryCatch(readRDS(orig_out_path), error = function(e) NULL)
  }
  
  if (!is.null(orig_out_path) && nzchar(orig_out_path)) {
    tryCatch(saveRDS(res, orig_out_path), error = function(e) NULL)
  }
  
  if (length(names(res)) > 0) {
    for (d_id in names(res)) {
      base_id <- get_base_id(d_id)
      training_results_path <- get_session_path(base_id, "%s_fs_training_results.rds")
      saveRDS(res, training_results_path)
      
      for (m in extract_model_names(input_data$models)) {
        tryCatch({
          feats_to_save <- res[[d_id]]$model_features[[m]] %||% res[[d_id]]$features
          if (m %in% c("stabl", "boruta")) {
            feats_to_save <- lapply(feats_to_save, function(item) {
              item$importance <- NULL
              item
            })
          }
          save_list_to_csv(feats_to_save, sprintf("tmp/feature_importance_%s_%s.csv", m, d_id))
        }, error = function(e) NULL)
      }
    }
    
    tryCatch({
      finalize_fs(
        datasets = input_data$datasets,
        models = input_data$models,
        train_ratio = input_data$split_ratio %||% input_data$train_ratio %||% 0.7,
        multi_dataset_mode = input_data$multi_dataset_mode %||% "combine",
        selection_method = selection_method,
        percentage_val = percentage_val,
        max_features_val = max_features_val,
        fs_results = orig_fs_results,
        refit_results = res,
        user_id = user_id,
        module = module,
        parameters = input_data$parameters
      )
    }, error = function(e) {
      cat("[WARNING] Failed to append FS report in refit:", e$message, "\n")
    })
  }
  return(res)
}

finalize_cv <- function(res, datasets, cv_method, folds, user_id, module = "fs") {
  if (!is.null(res$.error)) return(res)
  
  if (length(datasets) > 0) {
    for (d_id in names(res)) {
      cv_csv <- get_session_path(d_id, "cv_results_%s.csv")
      save_list_to_csv(res[[d_id]], cv_csv)
      saveRDS(res[[d_id]], get_session_path(d_id, "%s_fs_cv_results.rds"))
      register_export_file(user_id, "cv_results", get_base_id(d_id), cv_csv, module, "cv")
    }
    
    tryCatch({
      first_ds_id <- datasets[[1]]$id %||% datasets[[1]]$datasetId
      first_base_id <- get_base_id(first_ds_id)
      saved_fs_res <- NULL
      saved_train_res <- get_session_path(first_base_id, "%s_fs_training_results.rds")
      if (file.exists(saved_train_res)) {
        saved_fs_res <- tryCatch(readRDS(saved_train_res), error = function(e) NULL)
      }
      saved_params <- NULL
      saved_params_path <- get_session_path(first_base_id, "%s_fs_parameters.rds")
      if (file.exists(saved_params_path)) {
        saved_params <- tryCatch(readRDS(saved_params_path), error = function(e) NULL)
      }
      
      models_extracted <- c()
      for (k in names(res)) {
        c_items <- if (is.list(res[[k]]) && !is.null(res[[k]]$metrics)) res[[k]]$metrics else res[[k]]
        if (is.list(c_items)) {
          for (it in c_items) {
            if (is.list(it) && !is.null(it$model)) models_extracted <- c(models_extracted, as.character(it$model))
          }
        }
      }
      models_extracted <- unique(models_extracted)
      if (length(models_extracted) == 0) models_extracted <- NULL

      finalize_fs(
        datasets = datasets,
        models = models_extracted,
        train_ratio = datasets[[1]]$fs_trainRatio %||% 0.7,
        multi_dataset_mode = "combine",
        selection_method = saved_params$selectionMethod %||% "breakoff",
        percentage_val = saved_params$percentageCutoff %||% 80,
        max_features_val = saved_params$maxFeaturesSelect %||% 10,
        fs_results = saved_fs_res,
        refit_results = saved_fs_res,
        cv_results = res,
        user_id = user_id,
        module = module,
        parameters = saved_params
      )
    }, error = function(e) {
      cat("[WARNING] Failed to update FS report in finalize_cv:", e$message, "\n")
    })
  }
  return(res)
}

finalize_testing <- function(res, datasets, models, user_id, module = "fs") {
  if (!is.null(res$.error)) return(res)
  
  if (length(datasets) > 0) {
    for (d in datasets) {
      ds_id <- d$id %||% d$datasetId
      base_id <- get_base_id(ds_id)
      for (m in models) {
        if (!is.null(res[[ds_id]]) && !is.null(res[[ds_id]][[m]])) {
          test_csv <- get_session_path(base_id, sprintf("%%s_testing_performance_%s.csv", m))
          save_list_to_csv(list(res[[ds_id]][[m]]), test_csv)
          register_export_file(user_id, "fs_testing_results", base_id, test_csv, module, "testing", model = m)
        }
      }
      testing_results_path <- get_session_path(base_id, "%s_fs_testing_results.rds")
      saveRDS(res, testing_results_path)
    }
    
    tryCatch({
      first_ds_id <- datasets[[1]]$id %||% datasets[[1]]$datasetId
      first_base_id <- get_base_id(first_ds_id)
      saved_fs_res <- NULL
      saved_train_res <- get_session_path(first_base_id, "%s_fs_training_results.rds")
      if (file.exists(saved_train_res)) {
        saved_fs_res <- tryCatch(readRDS(saved_train_res), error = function(e) NULL)
      }
      saved_params <- NULL
      saved_params_path <- get_session_path(first_base_id, "%s_fs_parameters.rds")
      if (file.exists(saved_params_path)) {
        saved_params <- tryCatch(readRDS(saved_params_path), error = function(e) NULL)
      }

      finalize_fs(
        datasets = datasets,
        models = models,
        train_ratio = datasets[[1]]$fs_trainRatio %||% 0.7,
        multi_dataset_mode = "combine",
        selection_method = saved_params$selectionMethod %||% "breakoff",
        percentage_val = saved_params$percentageCutoff %||% 80,
        max_features_val = saved_params$maxFeaturesSelect %||% 10,
        fs_results = saved_fs_res,
        refit_results = saved_fs_res,
        testing_results = res,
        user_id = user_id,
        module = module,
        parameters = saved_params
      )
    }, error = function(e) {
      cat("[WARNING] Failed to update FS report in finalize_testing:", e$message, "\n")
    })
  }
  return(res)
}

write_table_by_ext <- function(df, file_path, ext) {
  ext <- tolower(ext)
  if (ext == "tsv") {
    write.table(df, file = file_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  } else if (ext == "xlsx") {
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(df, path = file_path)
    #} else if (requireNamespace("openxlsx", quietly = TRUE)) {
    #  openxlsx::write.xlsx(df, file = file_path)
    } else {
      write.csv(df, file = file_path, row.names = FALSE)
    }
  } else {
    # Default to CSV
    write.csv(df, file = file_path, row.names = FALSE)
  }
}

get_mime_content_type <- function(ext) {
  switch(tolower(ext %||% "csv"),
    "pdf"  = "application/pdf",
    "png"  = "image/png",
    "tiff" = "image/tiff",
    "tif"  = "image/tiff",
    "jpeg" = "image/jpeg",
    "jpg"  = "image/jpeg",
    "csv"  = "text/csv",
    "tsv"  = "text/tab-separated-values",
    "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "txt"  = "text/plain",
    "md"   = "text/markdown",
    "zip"  = "application/zip",
    "text/plain"
  )
}

materialize_manifest_entry <- function(entry, src, ext, uid, base_id, file_type) {
  if (is_empty_str(src) || !isTRUE(file.exists(as.character(src)))) return(NULL)
  src <- as.character(src)
  src_ext <- tolower(tools::file_ext(src))
  target_ext <- tolower(ext %||% (if (!is.null(entry$ext) && entry$ext != "rds") entry$ext else "csv") %||% "csv")
  if (target_ext == "rds") target_ext <- "csv"
  date_prefix <- format(Sys.Date(), "%y%m%d")
  dataset_name <- if (!is_empty_str(base_id)) get_dataset_name(base_id) else NULL
  if (!is.null(dataset_name)) {
    dataset_name <- gsub("[:/\\\\?*\"<>| ]", "_", dataset_name)
  }

  parts <- c(date_prefix)
  if (!is.null(dataset_name) && nzchar(dataset_name)) {
    parts <- c(parts, dataset_name)
  } else {
    data_class <- get_session_data_class(uid)
    parts <- c(parts, gsub("[:/\\\\?*\"<>| ]", "_", data_class %||% "dataset"))
  }

  m_val <- clean_manifest_field(entry$model)
  d_val <- clean_manifest_field(entry$db)
  if (!is.null(m_val)) parts <- c(parts, m_val)
  if (!is.null(d_val) && !identical(d_val, m_val)) parts <- c(parts, d_val)

  clean_ft <- gsub("[:/\\\\?*\"<>| ]", "_", safe_str(file_type, "file"))
  if (clean_ft %in% c("final_matrix", "expr_matrix")) clean_ft <- "final_expression_data"
  parts <- c(parts, clean_ft)
  parts <- vapply(parts, function(p) gsub("[:/\\\\?*\"<>| ]", "_", as.character(p)), character(1))
  clean_filename <- sprintf("%s.%s", paste(parts, collapse = "_"), target_ext)

  src_ext <- tolower(tools::file_ext(src))

  # Direct return if exact matching plot/text format
  if (src_ext == target_ext && src_ext %in% c("pdf", "png", "tiff", "tif", "jpeg", "jpg", "csv", "txt", "md")) {
    return(list(
      file_path      = src,
      clean_filename = clean_filename,
      content_type   = get_mime_content_type(target_ext)
    ))
  }

  export_dir <- file.path("tmp/user_sessions", uid, "exports")
  dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
  clean_ft_safe <- gsub("[^A-Za-z0-9_-]", "_", safe_str(file_type, "file"))
  base_id_safe <- gsub("[^A-Za-z0-9_-]", "_", safe_str(base_id, "data"))
  tmp_out <- chartr("\\", "/", file.path(export_dir, sprintf("export_%s_%s_%d_%d.%s", base_id_safe, clean_ft_safe, as.integer(Sys.time()), sample.int(1e6, 1), target_ext)))

  # PDF to PNG/TIFF/JPEG image conversion via pdftools
  if (src_ext == "pdf" && target_ext %in% c("png", "tiff", "tif", "jpeg", "jpg")) {
    conv_ok <- convert_pdf_to_image(src, tmp_out, target_format = target_ext, dpi = 300)
    if (conv_ok && file.exists(tmp_out) && (file.info(tmp_out)$size[1] > 0)) {
      return(list(
        file_path      = tmp_out,
        clean_filename = clean_filename,
        content_type   = get_mime_content_type(target_ext)
      ))
    }
  }

  if (src_ext == "rds") {
    obj <- tryCatch(readRDS(src), error = function(e) NULL)
    if (is.null(obj)) return(NULL)

    if (is.matrix(obj) || is.data.frame(obj)) {
      if (clean_ft %in% c("annotation_results", "unmapped_results", "unmapped_features") ||
          (is.data.frame(obj) && !is.matrix(obj) && ("entrez_id" %in% colnames(obj) || "gene_symbol" %in% colnames(obj) || "gene_biotype" %in% colnames(obj)))) {
        df_out <- obj
        write_table_by_ext(df_out, tmp_out, target_ext)
        return(list(file_path = tmp_out, clean_filename = clean_filename, content_type = get_mime_content_type(target_ext)))
      }
      meta_path <- file.path("tmp/user_sessions", uid, sprintf("%s_expr_metadata.rds", base_id_safe))
      anno_done <- FALSE
      if (file.exists(meta_path)) {
        meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
        if (!is.null(meta) && isTRUE(meta$annotationDone) && !identical(meta$annotationStrategy, "skip")) {
          anno_done <- TRUE
        }
      }
      feature_col <- if (anno_done) "Gene_Name" else "Gene_ID"
      df_out <- get_formatted_matrix_df(base_id, obj, default_col_name = feature_col)
      write_table_by_ext(df_out, tmp_out, target_ext)
      return(list(file_path = tmp_out, clean_filename = clean_filename, content_type = get_mime_content_type(target_ext)))
    } else if (is.list(obj) && !is.null(obj$matrix)) {
      df_out <- get_formatted_matrix_df(base_id, obj$matrix, default_col_name = "Gene_ID")
      write_table_by_ext(df_out, tmp_out, target_ext)
      return(list(file_path = tmp_out, clean_filename = clean_filename, content_type = get_mime_content_type(target_ext)))
    }
  } else if (src_ext == "csv") {
    df <- tryCatch(read.csv(src, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      write_table_by_ext(df, tmp_out, target_ext)
      return(list(file_path = tmp_out, clean_filename = clean_filename, content_type = get_mime_content_type(target_ext)))
    }
  }

  list(file_path = src, clean_filename = clean_filename, content_type = get_mime_content_type(src_ext))
}

resolve_manifest_src_path <- function(uid, rel_path) {
  if (is_empty_str(rel_path)) return(NULL)
  rel_path <- safe_str(rel_path)
  p1 <- file.path("tmp/user_sessions", uid, rel_path)
  if (isTRUE(file.exists(p1))) return(p1)
  p2 <- file.path("tmp", rel_path)
  if (isTRUE(file.exists(p2))) return(p2)
  if (isTRUE(file.exists(rel_path))) return(rel_path)
  
  # Also try sanitized version of rel_path
  clean_base <- basename(rel_path)
  ext <- tools::file_ext(clean_base)
  no_ext <- tools::file_path_sans_ext(clean_base)
  clean_fn <- if (nzchar(ext)) sprintf("%s.%s", gsub("[:/\\\\?*\"<>| ]", "_", no_ext), ext) else gsub("[:/\\\\?*\"<>| ]", "_", clean_base)
  p1_clean <- file.path("tmp/user_sessions", uid, clean_fn)
  if (isTRUE(file.exists(p1_clean))) return(p1_clean)
  p2_clean <- file.path("tmp", clean_fn)
  if (isTRUE(file.exists(p2_clean))) return(p2_clean)
  p1
}

resolve_zip_dir <- function(entry, target_module) {
  m <- safe_str(entry$module)
  k <- safe_str(entry$key)

  # Normalize module domain by key if needed
  if (k %in% c("de_all_results", "de_sig_results", "de_volcano", "de_ma", "de_heatmap_top10", "meta_results", "forest_plots", "heterogeneity_report")) {
    m <- "de"
  } else if (grepl("^(ora_|gsea_)", k)) {
    m <- "ea"
  } else if (grepl("^(fs_|all_models_|cv_|testing_|confusion_|roc_|selected_|feature_|pooled_)", k)) {
    m <- "fs"
  }

  if (target_module == "dp") {
    if (m == "dp")  return("data_processing")
    if (m == "de")  return(file.path("data_processing", "de_analysis"))
    if (m == "ea")  return(file.path("data_processing", "enrichment_analysis"))
    if (m == "fs")  return(file.path("data_processing", "feature_selection"))
    return("data_processing")
  }
  if (target_module %in% c("de", "de_analysis", "de-analysis")) {
    if (m %in% c("dp", "de")) return("de_analysis")
    if (m == "ea") return(file.path("de_analysis", "enrichment_analysis"))
    return("de_analysis")
  }
  if (target_module %in% c("ea", "en", "enrichment")) return("enrichment_analysis")
  if (target_module %in% c("fs", "feature_selection", "feature-selection")) return("feature_selection")
  "misc"
}

build_zip_manifest <- function(uid, module = "dp", ds_ids = NULL) {
  manifest <- load_manifest(uid)
  if (is.null(manifest$entries) || length(manifest$entries) == 0) {
    return(list())
  }

  target_module <- tolower(safe_str(module, "dp"))

  # Identify datasets that have batch correction
  batch_datasets <- unique(vapply(
    Filter(function(e) isTRUE(e$key %in% c("pca_plots_before_batch", "pca_plots_after_batch", "batch_corrected_matrix")), manifest$entries),
    function(e) {
      bid <- get_base_id(e$dsId)
      if (is_empty_str(bid)) "" else as.character(bid)
    },
    character(1)
  ))

  selected_entries <- list()
  base_ids_filter <- if (!is.null(ds_ids) && length(ds_ids) > 0) {
    unique(vapply(ds_ids, function(d) {
      bid <- get_base_id(d)
      if (is_empty_str(bid)) "" else as.character(bid)
    }, character(1)))
  } else NULL

  # Track seen signatures to prevent duplicate entries in zip
  seen_signatures <- list()

  for (e in manifest$entries) {
    # 1. Check parent module ownership
    e_parent <- safe_str(e$parentModule, "")
    if (!nzchar(e_parent)) {
      parsed_d <- if (!is.null(e$dsId) && nzchar(e$dsId)) get_backend_datasets(e$dsId) else list()
      e_parent <- parsed_d$parentModule %||% e$module %||% "dp"
    }
    e_parent <- tolower(safe_str(e_parent))

    # Strict isolation: entry must belong to target module lineage
    if (e_parent != target_module) {
      next
    }

    e_ds <- safe_str(e$dsId, "")
    e_base <- get_base_id(e_ds)

    if (!is.null(base_ids_filter)) {
      is_match <- (nzchar(e_ds) && isTRUE(e_ds %in% ds_ids)) || (nzchar(e_base) && isTRUE(e_base %in% base_ids_filter))
      is_target_global <- (is_empty_str(e_ds) || grepl("^(merged_|meta)", e_base)) && (e_parent == target_module)
      if (!is_match && !is_target_global) {
        next
      }
    }

    # If this dataset has batch correction, exclude generic pca_plots
    if (identical(e$key, "pca_plots") && nzchar(e_base) && isTRUE(e_base %in% batch_datasets)) {
      next
    }

    target_dir <- resolve_zip_dir(e, target_module)
    if (is.null(target_dir) || is.na(target_dir) || !nzchar(target_dir)) next

    src_file <- resolve_manifest_src_path(uid, e$path)
    if (!is.null(src_file) && isTRUE(file.exists(src_file))) {
      e_model <- clean_manifest_field(e$model) %||% ""
      e_db    <- clean_manifest_field(e$db) %||% ""
      sig <- sprintf("%s|%s|%s|%s|%s|%s", safe_str(e$key), e_base, e_model, e_db, safe_str(e$ext), target_dir)
      if (sig %in% seen_signatures) next
      seen_signatures[[length(seen_signatures) + 1]] <- sig

      selected_entries[[length(selected_entries) + 1]] <- list(
        entry      = e,
        src_path   = src_file,
        target_dir = target_dir
      )
    }
  }
  selected_entries
}

resolve_export_file <- function(file_type, ds_id, model = NULL, ext = "csv", user_id = "") {
  uid <- if (!is_empty_str(user_id) && user_id != "user") safe_str(user_id) else get_user_id(ds_id)
  base_id <- get_base_id(ds_id)
  if (is_empty_str(uid) || uid == "user") uid <- get_user_id(base_id)
  if (is_empty_str(uid)) uid <- "user"

  manifest <- load_manifest(uid)
  entry <- find_manifest_entry(manifest, file_type, base_id, model = model, db = model)

  # On-demand generation for QC plots if missing in manifest
  if (is.null(entry)) {
    if (file_type %in% c("pca_plots_before_batch", "pca_plots_after_batch", "pca_plots")) {
      tryCatch(generate_pca_plots_for_export(base_id), error = function(e) NULL)
      manifest <- load_manifest(uid)
      entry <- find_manifest_entry(manifest, file_type, base_id, model = model, db = model)
    } else if (file_type %in% c("boxplot_before", "boxplot_after")) {
      tryCatch(ensure_boxplot_before(base_id), error = function(e) NULL)
      manifest <- load_manifest(uid)
      entry <- find_manifest_entry(manifest, file_type, base_id, model = model, db = model)
    } else if (file_type %in% c("annotation_results", "unmapped_results", "unmapped_features")) {
      fn_prefix <- if (file_type == "annotation_results") "%s_annotation_results" else "%s_unmapped_results"
      anno_p <- file.path("tmp/user_sessions", uid, sprintf(paste0(fn_prefix, ".rds"), base_id))
      if (!file.exists(anno_p)) anno_p <- sprintf(paste0("tmp/", fn_prefix, ".rds"), base_id)
      if (!file.exists(anno_p)) {
        anno_p_csv <- file.path("tmp/user_sessions", uid, sprintf(paste0(fn_prefix, ".csv"), base_id))
        if (!file.exists(anno_p_csv)) anno_p_csv <- sprintf(paste0("tmp/", fn_prefix, ".csv"), base_id)
        if (file.exists(anno_p_csv)) anno_p <- anno_p_csv
      }
      if (file.exists(anno_p)) {
        entry <- list(
          key    = file_type,
          dsId   = base_id,
          path   = basename(anno_p),
          ext    = tools::file_ext(anno_p),
          module = "dp",
          step   = "annotation"
        )
      }
    }
  }

  if (!is.null(entry)) {
    src <- resolve_manifest_src_path(uid, entry$path)
    if (!is.null(src) && isTRUE(file.exists(src))) {
      res <- materialize_manifest_entry(entry, src, ext, uid, base_id, file_type)
      if (!is.null(res) && !is.null(res$file_path) && isTRUE(file.exists(res$file_path))) {
        return(res)
      }
    }
  }
  NULL
}

# Helper to convert Markdown to PDF using instant Python engine, cupsfilter, or headless browser
convert_md_to_pdf <- function(md_path, pdf_path, report_title = "EasyOmiFun Analysis Report") {
  if (is_empty_str(md_path) || !isTRUE(file.exists(as.character(md_path)))) return(FALSE)
  md_path <- normalizePath(as.character(md_path), winslash = "/", mustWork = FALSE)
  
  # ── Tier 1: Fast Python md_to_pdf.py (instant <60ms, pure Python, cross-platform) ──
  py_bin <- Sys.getenv("STABL_PYTHON", "")
  if (!nzchar(py_bin)) py_bin <- Sys.getenv("RETICULATE_PYTHON", "")
  if (!nzchar(py_bin)) {
    conda_p <- Sys.getenv("RETICULATE_MINICONDA_PATH", "")
    if (nzchar(conda_p)) {
      cand_py <- if (.Platform$OS.type == "windows") file.path(conda_p, "python.exe") else file.path(conda_p, "bin", "python")
      if (file.exists(cand_py)) py_bin <- cand_py
    }
  }
  if (!nzchar(py_bin)) {
    home_d <- Sys.getenv("HOME", "")
    py_cand_list <- if (.Platform$OS.type == "windows") {
      c(file.path(Sys.getenv("APPDATA", ""), "EasyOmiFun", "miniconda", "python.exe"),
        "C:/miniconda3/envs/app/python.exe")
    } else {
      c(file.path(home_d, "Library", "EasyOmiFun", "miniconda", "bin", "python"),
        file.path(home_d, "Library", "EasyOmiFun", "miniconda", "bin", "python3"),
        file.path(getwd(), "miniconda", "bin", "python"),
        file.path(getwd(), "miniconda", "bin", "python3"))
    }
    for (c_py in py_cand_list) {
      if (file.exists(c_py)) { py_bin <- c_py; break }
    }
  }
  
  script_path <- file.path(getwd(), "md_to_pdf.py")
  if (nzchar(py_bin) && file.exists(py_bin) && file.exists(script_path)) {
    st_py <- tryCatch(
      system2(py_bin, args = c(shQuote(script_path), shQuote(md_path), shQuote(pdf_path), shQuote(report_title)),
              stdout = FALSE, stderr = FALSE),
      error = function(e) -1
    )
    if (st_py == 0 && file.exists(pdf_path) && (file.info(pdf_path)$size[1] > 0)) {
      return(TRUE)
    }
  }
  
  # ── Tier 2: Native macOS cupsfilter (/usr/sbin/cupsfilter) ──
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    cups_bin <- "/usr/sbin/cupsfilter"
    if (file.exists(cups_bin)) {
      st_cups <- tryCatch(
        system2(cups_bin, args = c("-i", "text/plain", shQuote(md_path)), stdout = pdf_path, stderr = FALSE),
        error = function(e) -1
      )
      if (st_cups == 0 && file.exists(pdf_path) && (file.info(pdf_path)$size[1] > 0)) {
        return(TRUE)
      }
    }
  }
  
  # ── Tier 3: Headless Chromium Browser (Edge, Chrome, Chromium, Brave) ──
  html_body <- NULL
  if (requireNamespace("commonmark", quietly = TRUE)) {
    md_text <- paste(readLines(md_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    html_body <- tryCatch({
      commonmark::markdown_html(md_text, extensions = TRUE)
    }, error = function(e) NULL)
  }
  
  if (!is.null(html_body)) {
    style_css <- "
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; color: #333; padding: 30px; max-width: 800px; margin: 0 auto; }
      h1, h2, h3, h4 { color: #111; margin-top: 1.2em; margin-bottom: 0.4em; }
      h1 { border-bottom: 2px solid #eaecef; padding-bottom: 0.3em; font-size: 1.8em; }
      h2 { border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; font-size: 1.4em; }
      table { border-collapse: collapse; width: 100%; margin: 16px 0; }
      table th, table td { padding: 6px 12px; border: 1px solid #dfe2e5; font-size: 9pt; }
      table tr:nth-child(2n) { background-color: #f6f8fa; }
      @media print { body { padding: 15px; } table, pre, blockquote { page-break-inside: avoid; } }
    "
    html_path <- tempfile(fileext = ".html")
    html_content <- paste0("<!DOCTYPE html><html><head><meta charset='utf-8'><style>", style_css, "</style></head><body>", paste(html_body, collapse = "\n"), "</body></html>")
    writeLines(html_content, con = html_path, useBytes = TRUE)
    
    browser_candidates <- c(
      Sys.getenv("CHROME_PATH"),
      Sys.getenv("BROWSER_PATH"),
      Sys.which("google-chrome"),
      Sys.which("chrome"),
      Sys.which("chromium"),
      Sys.which("chromium-browser"),
      Sys.which("msedge"),
      Sys.which("brave"),
      "C:/Program Files/Google/Chrome/Application/chrome.exe",
      "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
      "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
      "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
      file.path(Sys.getenv("LOCALAPPDATA", "C:/Users/Default/AppData/Local"), "Google/Chrome/Application/chrome.exe"),
      file.path(Sys.getenv("LOCALAPPDATA", "C:/Users/Default/AppData/Local"), "Microsoft/Edge SxS/Application/msedge.exe"),
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
      "/usr/bin/google-chrome",
      "/usr/bin/chromium-browser",
      "/usr/bin/chromium"
    )
    browser_candidates <- browser_candidates[nzchar(browser_candidates) & file.exists(browser_candidates)]
    
    for (b in browser_candidates) {
      cmd <- sprintf("%s --headless --disable-gpu --no-sandbox --print-to-pdf=%s %s", shQuote(b), shQuote(pdf_path), shQuote(html_path))
      st <- tryCatch(system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE), error = function(e) -1)
      if (st == 0 && file.exists(pdf_path) && (file.info(pdf_path)$size[1] > 0)) {
        unlink(html_path)
        return(TRUE)
      }
    }
    
    # ── Tier 4: wkhtmltopdf fallback ──
    wk_bin <- Sys.which("wkhtmltopdf")
    if (nzchar(wk_bin)) {
      st_wk <- tryCatch(system2(wk_bin, args = c(shQuote(html_path), shQuote(pdf_path)), stdout = FALSE, stderr = FALSE), error = function(e) -1)
      if (st_wk == 0 && file.exists(pdf_path) && (file.info(pdf_path)$size[1] > 0)) {
        unlink(html_path)
        return(TRUE)
      }
    }
    unlink(html_path)
  }
  
  return(FALSE)
}

