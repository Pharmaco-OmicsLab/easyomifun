library(jsonlite)

# Import helper functions
source("processing.R")
source("shared_utils.R")

as_bool <- function(val, default = FALSE) {
  if (is.null(val)) return(default)
  if (is.logical(val)) return(isTRUE(val))
  if (is.character(val)) return(tolower(trimws(val)) %in% c("true", "t", "1", "yes"))
  if (is.numeric(val)) return(val != 0)
  return(default)
}

# Vectorized Hedges' g from t-statistics
compute_hedges_g_vec <- function(t_stat, n1, n2) {
  df_val <- n1 + n2 - 2
  d      <- t_stat * sqrt(1/n1 + 1/n2)       # Cohen's d
  J      <- 1 - 3 / (4 * df_val - 1)         # Hedges' correction factor J
  g      <- J * d                              # Hedges' g
  var_g  <- (n1 + n2) / (n1 * n2) + g^2 / (2 * (n1 + n2 - 2))  # Variance of g
  list(g = g, var_g = var_g)
}

# 1. Run Differential Expression (DE) Analysis Handler
run_de_analysis <- function(method = "deseq2", pval_thresh = 0.05, logfc_thresh = 1.0, adjust_method = "BH", datasets = list()) {
  load_packages_globally(c("DESeq2", "edgeR", "limma", "ggplot2"))
  cat(sprintf("[DE] Processing %d dataset(s) for Differential Expression...\n", length(datasets)))
  
  results <- list()
  
  for (d in datasets) {
    ds_id_full <- if (!is.null(d$datasetId)) d$datasetId else d$id
    ds_id <- get_base_id(ds_id_full)
    
    # Per-dataset parameter overrides
    d_method <- if (!is.null(d$method)) d$method else method
    d_pval_thresh <- if (!is.null(d$pValueThreshold)) as.numeric(d$pValueThreshold) else pval_thresh
    d_logfc_thresh <- if (!is.null(d$logFcThreshold)) as.numeric(d$logFcThreshold) else logfc_thresh
    d_adjust_method <- if (!is.null(d$adjustMethod)) d$adjustMethod else adjust_method

    engine_adjust_method <- {
      .am <- tolower(trimws(as.character(if (is.null(d_adjust_method)) "BH" else d_adjust_method)))
      .valid <- c(bh = "BH", by = "BY", bonferroni = "bonferroni", holm = "holm",
                  hochberg = "hochberg", hommel = "hommel", fdr = "fdr", none = "none")
      if (.am %in% names(.valid)) unname(.valid[.am]) else "none"
    }

    ref_grp <- if (!is.null(d$referenceGroup)) d$referenceGroup else d$de_referenceGroup
    comp_grp <- if (!is.null(d$comparisonGroup)) d$comparisonGroup else d$de_comparisonGroup

    parsed_expr <- get_backend_dataset(ds_id, step = "de")
    if (is.null(parsed_expr)) parsed_expr <- get_backend_dataset(ds_id, original = TRUE)
    if (is.null(parsed_expr)) next
    
    # Determine actual DE method based on properties stored in payload, state, or disk
    dtype <- if (!is.null(d$dataType) && nzchar(d$dataType)) {
      d$dataType
    } else if (!is.null(parsed_expr$dataType) && nzchar(parsed_expr$dataType)) {
      parsed_expr$dataType
    } else {
      "readcounts"
    }
    
    is_norm <- if (!is.null(d$isNormalized)) {
      as_bool(d$isNormalized)
    } else if (!is.null(parsed_expr$isNormalized)) {
      as_bool(parsed_expr$isNormalized)
    } else {
      FALSE
    }
    
    expr_meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
    if (file.exists(expr_meta_path)) {
      meta <- tryCatch(readRDS(expr_meta_path), error = function(e) NULL)
      if (is.null(d$dataType) && !is.null(meta$dataType) && nzchar(meta$dataType)) dtype <- meta$dataType
      if (is.null(d$isNormalized) && !is.null(meta$isNormalized)) is_norm <- as_bool(meta$isNormalized)
    }

    actual_method <- tolower(trimws(as.character(d_method)))
    if (dtype %in% c("microarray", "proteomics", "others") || is_norm) {
      actual_method <- "limma"
    } else {
      if (!(actual_method %in% c("deseq2", "edger", "limma_voom"))) {
        actual_method <- "deseq2"
      }
    }

    cat(sprintf("  Analyzing dataset: %s (method=%s, ref=%s, comp=%s)\n", ds_id, actual_method, ref_grp, comp_grp))

    de_table <- NULL
    had_duplicates <- FALSE

    if (is.null(de_table)) {
      expr <- NULL
      latest_de <- get_latest_de_stack(ds_id)
      if (!is.null(latest_de) && !is.null(latest_de$data)) {
        cat("[DE] Loading expression matrix from de_stack...\n")
        expr <- latest_de$data
      }
      
      if (is.null(expr)) {
        if (dtype == "readcounts" && !is_norm && actual_method %in% c("deseq2", "edger", "limma_voom")) {
          cat("[DE] Loading raw count matrix stack...\n")
          expr <- get_latest_raw_count_matrix(ds_id)
        } else {
          cat("[DE] Loading latest expression matrix stack...\n")
          expr <- get_suitable_expression_matrix(ds_id, step = "de", data_type = dtype, is_norm = is_norm)
        }
      }
      
      if (is.null(expr)) expr <- parsed_expr$expr
      if (is.null(expr)) next
      
      # Collapse duplicate Gene IDs if present
      gene_ids <- rownames(expr)
      if (any(duplicated(gene_ids))) {
        had_duplicates <- TRUE
        cat(sprintf("[DE] Duplicate Gene IDs detected for dataset %s. Collapsing...\n", ds_id))
        
        is_norm_data <- is_norm || isTRUE(parsed_expr$isNormalized)
        is_ma_data   <- dtype %in% c("microarray", "proteomics", "others") || tolower(parsed_expr$dataType) %in% c("microarray", "proteomics", "others")
        
        if (is_ma_data || is_norm_data) {
          if (requireNamespace("limma", quietly = TRUE)) {
            expr <- limma::avereps(expr, ID = gene_ids)
          } else {
            sums <- rowsum(expr, group = gene_ids, reorder = FALSE)
            counts <- as.vector(table(factor(gene_ids, levels = rownames(sums))))
            expr <- sweep(sums, 1, counts, "/")
          }
        } else {
          expr <- rowsum(expr, group = gene_ids, reorder = FALSE)
        }
      }
      
      # Load clinical data from metadata on disk
      clin_path <- get_clinical_path(ds_id_full)
      if (!file.exists(clin_path)) {
        clin_path_short <- get_clinical_path(ds_id)
        if (file.exists(clin_path_short)) clin_path <- clin_path_short
      }
      clin_meta_path <- get_clin_metadata_path(ds_id_full)
      if (!file.exists(clin_meta_path)) {
        clin_meta_path_short <- get_clin_metadata_path(ds_id)
        if (file.exists(clin_meta_path_short)) clin_meta_path <- clin_meta_path_short
      }
      
      clin_df <- NULL
      meta_c <- NULL
      if (file.exists(clin_meta_path)) {
        meta_c <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
      }
      
      sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
        d$clinicalSampleIdCol
      } else if (!is.null(d$sampleIdCol) && nzchar(d$sampleIdCol)) {
        d$sampleIdCol
      } else if (!is.null(meta_c$sampleIdCol)) {
        meta_c$sampleIdCol
      } else {
        ""
      }
      
      if (file.exists(clin_path)) {
        raw_clin <- read_csv_preserve_id(clin_path)
        if (sample_id_col == "" && ncol(raw_clin) > 0) sample_id_col <- colnames(raw_clin)[1]
        clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
      } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0) {
        clin_df <- parse_clinical_data(d$clinicalParsedData, d$clinicalColumns, sample_id_col)
      }
      
      if (is.null(clin_df)) next

      # Align clinical metadata and expressions
      common_samples <- intersect(colnames(expr), rownames(clin_df))
      common_samples <- common_samples[!is.na(common_samples) & common_samples != "" & common_samples != "NA" & common_samples != "NaN"]
      clin_df <- clin_df[common_samples, , drop = FALSE]
      expr <- expr[, common_samples, drop = FALSE]
      
      group_col <- if (!is.null(d$clinicalGroupCol) && nzchar(d$clinicalGroupCol)) {
        d$clinicalGroupCol
      } else if (!is.null(d$groupCol) && nzchar(d$groupCol)) {
        d$groupCol
      } else if (!is.null(meta_c$groupCol)) {
        meta_c$groupCol
      } else {
        ""
      }

      if (group_col == "" || !(group_col %in% colnames(clin_df))) {
        cand_grp <- c("Disease Type", "Type", "Group", "Condition", "Diagnosis", "Status", "Phenotype", "Class", "Disease", "disease", "group", "type")
        found_grp <- intersect(cand_grp, colnames(clin_df))
        if (length(found_grp) > 0) {
          group_col <- found_grp[1]
        }
      }

      if (is.null(group_col) || group_col == "" || !(group_col %in% colnames(clin_df))) next

      if (is.null(ref_grp) || ref_grp == "") ref_grp <- meta_c$referenceGroup
      if (is.null(comp_grp) || comp_grp == "") comp_grp <- meta_c$comparisonGroup

      if ((is.null(ref_grp) || ref_grp == "" || is.null(comp_grp) || comp_grp == "") && !is.null(clin_df) && nzchar(group_col) && (group_col %in% colnames(clin_df))) {
        unique_grps <- unique(clin_df[[group_col]][!is.na(clin_df[[group_col]]) & clin_df[[group_col]] != ""])
        if (length(unique_grps) > 0) {
          if (is.null(ref_grp) || ref_grp == "") ref_grp <- unique_grps[1]
          if (is.null(comp_grp) || comp_grp == "") comp_grp <- if (length(unique_grps) > 1) unique_grps[2] else unique_grps[1]
        }
      }

      if (is.null(ref_grp) || ref_grp == "" || is.null(comp_grp) || comp_grp == "") next

      clin_df$Group <- factor(clin_df[[group_col]], levels = c(ref_grp, comp_grp))
      clin_df <- clin_df[!is.na(clin_df$Group), ]
      expr <- expr[, rownames(clin_df), drop = FALSE]

      if (sum(clin_df$Group == ref_grp) == 0 || sum(clin_df$Group == comp_grp) == 0) {
        cat(sprintf("  [ERROR] Insufficient sample sizes for comparison in dataset %s\n", ds_id))
        next
      }

      # ── Tier 1: Statistical Engine Cache Check ───────────────────────────────────
      de_cache_meta  <- get_step_cache_meta(ds_id, "de")
      upstream_fp    <- get_matrix_fingerprint(expr)  # use the actual matrix variable name
      tier1_hit <- !is.null(de_cache_meta) &&
        identical(de_cache_meta$method,       actual_method) &&
        identical(de_cache_meta$ref_grp,      ref_grp) &&
        identical(de_cache_meta$comp_grp,     comp_grp) &&
        identical(de_cache_meta$group_col,    group_col) &&
        identical(de_cache_meta$adjust_method, engine_adjust_method) &&
        identical(de_cache_meta$upstream_fp,  upstream_fp)
      
      raw_table_path <- get_session_path(ds_id, "%s_de_raw_table.rds")
      
      if (tier1_hit && file.exists(raw_table_path)) {
        cat(sprintf("[DE] Re-using cached raw DE table for dataset %s (skipping statistical engine execution)\n", ds_id))
        de_table <- tryCatch(readRDS(raw_table_path), error = function(e) NULL)
        if (is.null(de_table)) tier1_hit <- FALSE
      }
      
      if (!tier1_hit) {
        # Differential expression calculation models
        if (actual_method == "deseq2") {
          de_table <- tryCatch({
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = round(expr), colData = clin_df, design = ~ Group)
            dds <- DESeq2::DESeq(dds, parallel = FALSE)
            res <- DESeq2::results(dds, contrast = c("Group", comp_grp, ref_grp), pAdjustMethod = engine_adjust_method, parallel = FALSE)
            
            df_res <- data.frame(
              gene = rownames(res),
              logFC = res$log2FoldChange,
              pValue = res$pvalue,
              adjPValue = res$padj,
              baseMean = res$baseMean,
              stringsAsFactors = FALSE
            )
            rm(dds, res)
            df_res
          }, error = function(e) {
            cat("[ERROR] DESeq2 execution failed:", e$message, "\n")
            NULL
          })
        }
        
        if (actual_method == "edger" || (actual_method == "deseq2" && is.null(de_table))) {
          de_table <- tryCatch({
            dge <- edgeR::DGEList(counts = expr, group = clin_df$Group)
            dge <- edgeR::calcNormFactors(dge)
            design <- model.matrix(~ Group, data = clin_df)
            dge <- edgeR::estimateDisp(dge, design)
            fit <- edgeR::glmQLFit(dge, design)
            res <- edgeR::glmQLFTest(fit, coef = 2)
            top <- edgeR::topTags(res, n = Inf, adjust.method = engine_adjust_method)
            
            data.frame(
              gene = rownames(top$table),
              logFC = top$table$logFC,
              pValue = top$table$PValue,
              adjPValue = top$table$FDR,
              baseMean = top$table$logCPM,
              stringsAsFactors = FALSE
            )
          }, error = function(e) {
            cat("[ERROR] edgeR execution failed:", e$message, "\n")
            NULL
          })
        }
        
        if (actual_method == "limma_voom" || (actual_method %in% c("deseq2", "edger") && is.null(de_table) && (dtype == "readcounts" && !is_norm))) {
          de_table <- tryCatch({
            dge <- edgeR::DGEList(counts = expr, group = clin_df$Group)
            dge <- edgeR::calcNormFactors(dge)
            design <- model.matrix(~ Group, data = clin_df)
            v   <- limma::voom(dge, design, plot = FALSE)
            fit <- limma::lmFit(v, design)
            fit <- limma::eBayes(fit)
            top <- limma::topTable(fit, coef = 2, number = Inf, adjust.method = engine_adjust_method)
            
            data.frame(
              gene = rownames(top),
              logFC = top$logFC,
              pValue = top$P.Value,
              adjPValue = top$adj.P.Val,
              baseMean = top$AveExpr,
              t = top$t,
              se = if (!is.null(fit$stdev.unscaled) && !is.null(fit$sigma)) fit$stdev.unscaled[rownames(top), 2] * fit$sigma else rep(NA_real_, nrow(top)),
              stringsAsFactors = FALSE
            )
          }, error = function(e) {
            cat("[ERROR] limma_voom execution failed:", e$message, "\n")
            NULL
          })
        }
        
        if (actual_method == "limma" || is.null(de_table)) {
          de_table <- tryCatch({
            design <- model.matrix(~ Group, data = clin_df)
            fit <- limma::lmFit(expr, design)
            fit <- limma::eBayes(fit)
            top <- limma::topTable(fit, coef = 2, number = Inf, adjust.method = engine_adjust_method)
            
            data.frame(
              gene = rownames(top),
              logFC = top$logFC,
              pValue = top$P.Value,
              adjPValue = top$adj.P.Val,
              baseMean = top$AveExpr,
              t = top$t,
              se = if (!is.null(fit$stdev.unscaled) && !is.null(fit$sigma)) fit$stdev.unscaled[rownames(top), 2] * fit$sigma else rep(NA_real_, nrow(top)),
              stringsAsFactors = FALSE
            )
          }, error = function(e) {
            cat("[ERROR] limma execution failed:", e$message, "\n")
            data.frame(gene = character(), logFC = numeric(), pValue = numeric(), adjPValue = numeric(), baseMean = numeric())
          })
        }
        
        if (!is.null(de_table) && nrow(de_table) > 0) {
          saveRDS(de_table, raw_table_path)
          save_step_cache_meta(ds_id, "de", list(
            method        = actual_method,
            ref_grp       = ref_grp,
            comp_grp      = comp_grp,
            group_col     = group_col,
            adjust_method = engine_adjust_method,
            upstream_fp   = upstream_fp
          ))
        }
      }
    }
    
    # Process thresholds, sorting, and plot generation
    if (!is.null(de_table) && nrow(de_table) > 0) {
      de_table$logFC[is.na(de_table$logFC)] <- 0
      de_table$pValue[is.na(de_table$pValue)] <- 1
      de_table$adjPValue[is.na(de_table$adjPValue)] <- 1
      de_table$baseMean[is.na(de_table$baseMean)] <- 0
      
      clean_adj_method <- if (!is.null(d_adjust_method)) tolower(trimws(as.character(d_adjust_method))) else "bh"
      use_raw_p <- clean_adj_method %in% c("none", "raw", "unadjusted", "no adjustment")
      
      if (use_raw_p) {
        de_table$significant <- de_table$pValue < d_pval_thresh & abs(de_table$logFC) >= d_logfc_thresh
      } else {
        de_table$significant <- de_table$adjPValue < d_pval_thresh & abs(de_table$logFC) >= d_logfc_thresh
      }
      de_table$direction <- ifelse(de_table$significant, ifelse(de_table$logFC > 0, "up", "down"), "ns")
      
      # Append geneInfoCols back to de_table if present
      expr_meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
      gene_info_cols <- character(0)
      if (file.exists(expr_meta_path)) {
        meta <- readRDS(expr_meta_path)
        gene_info_cols <- if (!is.null(meta$geneInfoCols)) unlist(meta$geneInfoCols) else character(0)
      }

      # 1. Try mapping from resolved_mapping if annotation was performed
      resolved_mapping_path <- get_session_path(ds_id, "%s_resolved_mapping.rds")
      if (file.exists(resolved_mapping_path)) {
        resolved_mapping <- tryCatch(readRDS(resolved_mapping_path), error = function(e) NULL)
        if (!is.null(resolved_mapping) && is.data.frame(resolved_mapping)) {
          map_idx <- if (!is.null(rownames(resolved_mapping)) && any(de_table$gene %in% rownames(resolved_mapping))) {
            match(de_table$gene, rownames(resolved_mapping))
          } else if ("gene_symbol" %in% colnames(resolved_mapping) && any(de_table$gene %in% resolved_mapping$gene_symbol)) {
            match(de_table$gene, resolved_mapping$gene_symbol)
          } else if ("input_id" %in% colnames(resolved_mapping) && any(de_table$gene %in% resolved_mapping$input_id)) {
            match(de_table$gene, resolved_mapping$input_id)
          } else {
            NULL
          }
          if (!is.null(map_idx)) {
            candidate_cols <- intersect(c(gene_info_cols, "entrez_id", "gene_symbol", "gene_biotype"), colnames(resolved_mapping))
            for (col_name in candidate_cols) {
              if (!(col_name %in% colnames(de_table))) {
                de_table[[col_name]] <- resolved_mapping[[col_name]][map_idx]
              }
            }
          }
        }
      }

      # 2. Try mapping from expression CSV if columns exist in CSV
      expr_csv_path <- get_session_path(ds_id, "%s_expression.csv")
      if (file.exists(expr_csv_path) && length(gene_info_cols) > 0) {
        expr_df <- tryCatch(read_csv_preserve_id(expr_csv_path), error = function(e) NULL)
        if (!is.null(expr_df) && is.data.frame(expr_df) && ncol(expr_df) > 0) {
          id_col <- colnames(expr_df)[1]
          valid_csv_cols <- intersect(gene_info_cols, colnames(expr_df))
          valid_csv_cols <- setdiff(valid_csv_cols, c(id_col, colnames(de_table)))
          if (length(valid_csv_cols) > 0 && id_col %in% colnames(expr_df)) {
            info_df <- expr_df[, c(id_col, valid_csv_cols), drop = FALSE]
            colnames(info_df)[1] <- "gene"
            
            # Merge by matching
            match_idx <- match(de_table$gene, info_df$gene)
            for (info_col in valid_csv_cols) {
              de_table[[info_col]] <- info_df[[info_col]][match_idx]
            }
          }
        }
      }

      # Sort DE table; write sorted CSV as a side-effect to avoid holding
      # a full list-of-lists for all genes in memory.
      ord_idx <- order(de_table$adjPValue, de_table$pValue)
      de_table_sorted <- de_table[ord_idx, ]
      
      # Compute FoldChange = 2^logFC
      fold_change_vec <- 2^(de_table_sorted$logFC)

      # Build exported data.frame with exact headers: Feature, FoldChange, logFC, P-value, adj.P.Val.
      de_table_export <- data.frame(
        Feature = as.character(de_table_sorted$gene),
        FoldChange = fold_change_vec,
        logFC = de_table_sorted$logFC,
        `P-value` = de_table_sorted$pValue,
        `adj.P.Val.` = de_table_sorted$adjPValue,
        baseMean = de_table_sorted$baseMean,
        significant = de_table_sorted$significant,
        direction = de_table_sorted$direction,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      # Append geneInfoCols / extra metadata columns if present
      extra_cols <- setdiff(colnames(de_table_sorted), c("gene", "logFC", "pValue", "adjPValue", "baseMean", "significant", "direction", "FoldChange"))
      if (length(extra_cols) > 0) {
        for (info_col in extra_cols) {
          if (info_col %in% colnames(de_table_sorted)) {
            val <- de_table_sorted[[info_col]]
            de_table_export[[info_col]] <- if (is.character(val) || is.factor(val)) as.character(val) else val
          }
        }
      }

      top10_n <- min(10, nrow(de_table_sorted))
      top10_rows <- lapply(1:top10_n, function(i) {
        row_item <- as.list(de_table_export[i, , drop = FALSE])
        row_item$gene <- de_table_sorted$gene[i]
        row_item$logFC <- de_table_sorted$logFC[i]
        row_item$pValue <- de_table_sorted$pValue[i]
        row_item$adjPValue <- de_table_sorted$adjPValue[i]
        row_item
      })
      de_csv_path <- sprintf("tmp/%s_de_results.csv", ds_id_full)
      tryCatch({
        write.csv(de_table_export, de_csv_path, row.names = FALSE)
        write.csv(de_table_export, get_session_path(ds_id, "%s_de_results.csv"), row.names = FALSE)
        if (ds_id_full != ds_id) {
          write.csv(de_table_export, get_session_path(ds_id_full, "%s_de_results.csv"), row.names = FALSE)
        }
      }, error = function(e) cat(sprintf("[DE] CSV write failed for %s: %s\n", ds_id_full, e$message)))

      df_volc <- data.frame(
        logFC = de_table$logFC,
        pValPlot = if (use_raw_p) de_table$pValue else de_table$adjPValue,
        significant = de_table$significant,
        direction = de_table$direction
      )
      
      y_label <- if (use_raw_p) "-log10(p-value)" else "-log10(adj. p-value)"

      p_volc <- ggplot2::ggplot(df_volc, ggplot2::aes(x = logFC, y = -log10(pValPlot), color = direction)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("up" = "#ef4444", "down" = "#3b82f6", "ns" = "#94a3b8")) +
        ggplot2::geom_vline(xintercept = c(-d_logfc_thresh, d_logfc_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::geom_hline(yintercept = -log10(d_pval_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "log2 Fold Change", y = y_label) +
        ggplot2::theme(legend.position = "bottom")

      df_ma <- data.frame(
        baseMean  = de_table$baseMean,
        logFC     = de_table$logFC,
        direction = de_table$direction
      )
      p_ma <- ggplot2::ggplot(df_ma, ggplot2::aes(x = log10(baseMean + 1), y = logFC, color = direction)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("up" = "#ef4444", "down" = "#3b82f6", "ns" = "#94a3b8")) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "log10(Base Mean)", y = "log2 Fold Change") +
        ggplot2::theme(legend.position = "bottom")
      
      total_features <- nrow(de_table)
      num_sig <- sum(de_table$significant)
      sig_up <- sum(de_table$direction == "up")
      sig_down <- sum(de_table$direction == "down")

      # Persist the REAL volcano/MA plots to the session dir so exports ship them
      # instead of the placeholder produced by pre_generate_exports(). Same paths,
      # so the placeholder is skipped (it only writes when the file is absent).
      tryCatch({
        ggplot2::ggsave(sprintf("tmp/%s_volcano.pdf", ds_id_full), p_volc, width = 7, height = 5)
        ggplot2::ggsave(sprintf("tmp/%s_ma.pdf", ds_id_full), p_ma, width = 7, height = 5)
      }, error = function(e) cat(sprintf("[DE] plot PDF save failed for %s: %s\n", ds_id_full, conditionMessage(e))))

      results[[ds_id_full]] <- list(
        stats = list(
          totalFeatures = total_features,
          numSignificant = num_sig,
          sigUp = sig_up,
          sigDown = sig_down
        ),
        top10 = top10_rows,
        results = top10_rows,
        full_results = NULL,   # Written to CSV as side-effect; not held in memory
        volcanoPlot = plot_to_base64(p_volc),
        maPlot = plot_to_base64(p_ma),
        hadDuplicates = had_duplicates,
        actualMethod = actual_method
      )
      rm(p_volc, p_ma)
    } else {
      results[[ds_id_full]] <- list(
        stats = list(totalFeatures = 0, numSignificant = 0, sigUp = 0, sigDown = 0),
        top10 = list(),
        results = list(),
        full_results = list(),
        volcanoPlot = "",
        maPlot = "",
        hadDuplicates = had_duplicates,
        actualMethod = actual_method
      )
    }
    if (exists("expr", inherits = FALSE)) rm(expr)
    if (exists("clin_df", inherits = FALSE)) rm(clin_df)
    if (exists("de_table", inherits = FALSE)) rm(de_table)
    invisible(gc(verbose = FALSE))
  }

  return(results)
}

# Worker-safe meta-analysis compute: runs run_meta_analysis for each data class that has more than one dataset
run_meta_compute <- function(datasets_by_class, method_val, pvalue_method = "fisher", eff_model = "random",
                             votes = 2, pval_thresh = 0.05, logfc_thresh = 1.0) {
  load_packages_globally(c("metap", "metapro", "metafor", "ggplot2"))
  per_class <- list()
  for (dclass in names(datasets_by_class)) {
    class_ds <- datasets_by_class[[dclass]]
    if (length(class_ds) > 1) {
      per_class[[dclass]] <- run_meta_analysis(method_val, pvalue_method, eff_model,
                                               votes, class_ds, pval_thresh, logfc_thresh,
                                               data_class = dclass)
    }
  }
  per_class
}

# 2. Meta-Analysis Handler
run_meta_analysis <- function(method, pvalue_method, effect_size_model, votes, datasets, pval_thresh = 0.05, logfc_thresh = 1.0, data_class = NULL) {
  cat(sprintf("[DE] Running Meta-Analysis (method=%s, pvalMethod=%s, effectModel=%s, pvalThresh=%.3f, logFcThresh=%.3f)...\n", 
              method, pvalue_method, effect_size_model, pval_thresh, logfc_thresh))

  # Determine omics data class (transcriptomics vs proteomics)
  if (is.null(data_class) && length(datasets) > 0) {
    first_ds <- datasets[[1]]
    if (!is.null(first_ds$dataClass) && nzchar(first_ds$dataClass)) {
      data_class <- first_ds$dataClass
    } else if (!is.null(first_ds$dataType) && nzchar(first_ds$dataType)) {
      data_class <- if (first_ds$dataType == "proteomics") "proteomics" else "transcriptomics"
    } else {
      first_id <- if (!is.null(first_ds$id)) first_ds$id else first_ds$datasetId
      data_class <- get_dataset_data_class(first_id)
    }
  }
  is_proteomics <- identical(data_class, "proteomics")

  # Retrieve previously selected adjustment method from cache of the first dataset
  meta_adjust_method <- "BH"
  if (length(datasets) > 0) {
    first_ds_id <- get_base_id(datasets[[1]]$id)
    cache_meta <- get_step_cache_meta(first_ds_id, "de")
    if (!is.null(cache_meta) && !is.null(cache_meta$adjust_method)) {
      meta_adjust_method <- cache_meta$adjust_method
    }
  }
  if (tolower(trimws(meta_adjust_method)) == "none") {
    meta_adjust_method <- "BH"
  }

  find_gene_col_local <- function(df, pref = "gene") {
    if (!is.null(pref) && pref != "" && pref %in% colnames(df)) return(pref)
    if ("gene" %in% colnames(df)) return("gene")
    if ("Gene" %in% colnames(df)) return("Gene")
    if ("gene_symbol" %in% colnames(df)) return("gene_symbol")
    if ("GeneID" %in% colnames(df)) return("GeneID")
    if ("Feature" %in% colnames(df)) return("Feature")
    return(colnames(df)[1])
  }

  de_list <- list()
  dataset_names <- c()
  
  for (d in datasets) {
    ds_id_full <- d$id
    ds_id <- get_base_id(ds_id_full)
    
    de_entry <- NULL

    # === PRIORITY CHAIN ===
    # 0. Read group metadata (lightweight — tiny RDS, no expr matrix load)
    clin_meta_path_meta <- get_clin_metadata_path(ds_id_full)
    ref_g_meta  <- d$de_referenceGroup
    comp_g_meta <- d$de_comparisonGroup
    group_col_meta <- NULL
    if (file.exists(clin_meta_path_meta)) {
      meta_light <- tryCatch(readRDS(clin_meta_path_meta), error = function(e) NULL)
      if (!is.null(meta_light)) {
        if (is.null(ref_g_meta)  || ref_g_meta  == "") ref_g_meta  <- meta_light$referenceGroup
        if (is.null(comp_g_meta) || comp_g_meta == "") comp_g_meta <- meta_light$comparisonGroup
        group_col_meta <- meta_light$groupCol
      }
    }

    # Determine previously executed DE method for this dataset
    de_meta_cache <- get_step_cache_meta(ds_id, "de")
    prev_de_method <- if (!is.null(de_meta_cache) && !is.null(de_meta_cache$method)) {
      tolower(trimws(as.character(de_meta_cache$method)))
    } else if (!is.null(d$method) && nzchar(d$method)) {
      tolower(trimws(as.character(d$method)))
    } else if (!is.null(d$actualMethod) && nzchar(d$actualMethod)) {
      tolower(trimws(as.character(d$actualMethod)))
    } else {
      NULL
    }

    # 1. Refit cache — check if a valid limma/voom refit cache exists
    refit_cache_file_top <- get_session_path(ds_id, "%s_de_refit_cache.rds")
    if (!file.exists(refit_cache_file_top)) refit_cache_file_top <- sprintf("tmp/%s_de_refit_cache.rds", ds_id)
    if (file.exists(refit_cache_file_top)) {
      refit_cache_top <- tryCatch(readRDS(refit_cache_file_top), error = function(e) NULL)
      if (!is.null(refit_cache_top) &&
          (is.null(ref_g_meta) || identical(refit_cache_top$ref, ref_g_meta)) &&
          (is.null(comp_g_meta) || identical(refit_cache_top$comp, comp_g_meta))) {
        de_entry <- refit_cache_top$de_entry
        cat(sprintf("[META] Loaded cached limma/voom refit for %s\n", d$name))
      }
    }

    # 2. Method-aware DE loading:
    # 2a. When method == "effect_size":
    #     - If prev_de_method was limma or limma_voom: use existing moderated t-statistics directly (no refit)
    #     - If prev_de_method was deseq2 or edger: MUST refit with limma-voom to get moderated t-statistics
    # 2b. When method != "effect_size" (e.g. combine_pvalue, vote_counting):
    #     - Directly use pre-computed DE results from CSV / raw table (DESeq2, edgeR, limma, limma_voom)
    if (is.null(de_entry)) {
      if (identical(method, "effect_size")) {
        if (!is.null(prev_de_method) && prev_de_method %in% c("limma", "limma_voom")) {
          # Check raw table RDS first
          raw_table_path <- get_session_path(ds_id, "%s_de_raw_table.rds")
          if (!file.exists(raw_table_path)) raw_table_path <- sprintf("tmp/%s_de_raw_table.rds", ds_id)
          raw_table <- if (file.exists(raw_table_path)) tryCatch(readRDS(raw_table_path), error = function(e) NULL) else NULL
          
          if (!is.null(raw_table) && nrow(raw_table) > 0 && ("t" %in% colnames(raw_table) || "stat" %in% colnames(raw_table))) {
            t_col <- if ("t" %in% colnames(raw_table)) "t" else "stat"
            t_stat_vals <- as.numeric(raw_table[[t_col]])
            
            # Compute group sizes n1, n2 for Hedges' g
            n1 <- 10; n2 <- 10
            clin_path_meta <- get_clinical_path(ds_id_full)
            if (file.exists(clin_path_meta) && !is.null(ref_g_meta) && !is.null(comp_g_meta)) {
              clin_meta_light <- tryCatch(read.csv(clin_path_meta, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
              if (!is.null(clin_meta_light) && !is.null(group_col_meta) && group_col_meta %in% colnames(clin_meta_light)) {
                n1_cnt <- sum(clin_meta_light[[group_col_meta]] == ref_g_meta, na.rm = TRUE)
                n2_cnt <- sum(clin_meta_light[[group_col_meta]] == comp_g_meta, na.rm = TRUE)
                if (n1_cnt > 0) n1 <- n1_cnt
                if (n2_cnt > 0) n2 <- n2_cnt
              }
            }
            
            hg_result <- compute_hedges_g_vec(t_stat_vals, n1, n2)
            g_val <- hg_result$g
            var_g <- hg_result$var_g
            g_val[is.na(t_stat_vals)] <- NA
            var_g[is.na(t_stat_vals)] <- NA
            
            de_entry <- data.frame(
              gene = as.character(raw_table$gene),
              logFC = as.numeric(raw_table$logFC),
              pval = as.numeric(raw_table$pValue),
              se = if ("se" %in% colnames(raw_table)) as.numeric(raw_table$se) else rep(NA_real_, nrow(raw_table)),
              hedges_g = g_val,
              hedges_g_se = sqrt(var_g),
              hedges_g_var = var_g,
              stringsAsFactors = FALSE
            )
            cat(sprintf("[META] Using existing moderated t-statistics for dataset %s (%s, no refit).\n", d$name, prev_de_method))
          } else {
            # Check CSV for t / stat column
            de_table_path <- get_session_path(ds_id, "%s_de_results.csv")
            if (!file.exists(de_table_path)) de_table_path <- get_session_path(ds_id, "%s_dp_de_results.csv")
            if (!file.exists(de_table_path)) de_table_path <- sprintf("tmp/%s_de_results.csv", ds_id)
            if (file.exists(de_table_path)) {
              de_df <- tryCatch(read_csv_preserve_id(de_table_path), error = function(e) NULL)
              if (!is.null(de_df) && nrow(de_df) > 0) {
                gene_id_col <- if (!is.null(d$geneIdCol) && d$geneIdCol != "") d$geneIdCol else "GeneID"
                g_col <- find_gene_col_local(de_df, gene_id_col)
                fc_col <- intersect(c("logFC", "logfc", "fc", "LogFC", "log2FoldChange", "Log2FC", "log2fc"), colnames(de_df))[1]
                pv_col <- intersect(c("pValue", "pval", "p.value", "P.Value", "p", "pvalue", "PValue", "padj", "adj.P.Val", "FDR"), colnames(de_df))[1]
                se_col <- intersect(c("se", "lfcSE", "StdErr", "Std.Error", "SE"), colnames(de_df))[1]
                stat_col <- intersect(c("t", "stat", "t_stat", "statistic"), colnames(de_df))[1]
                
                if (!is.na(g_col) && !is.na(fc_col) && !is.na(pv_col) && !is.na(stat_col)) {
                  t_stat_vals <- as.numeric(de_df[[stat_col]])
                  n1 <- 10; n2 <- 10
                  clin_path_meta <- get_clinical_path(ds_id_full)
                  if (file.exists(clin_path_meta) && !is.null(ref_g_meta) && !is.null(comp_g_meta)) {
                    clin_meta_light <- tryCatch(read.csv(clin_path_meta, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
                    if (!is.null(clin_meta_light) && !is.null(group_col_meta) && group_col_meta %in% colnames(clin_meta_light)) {
                      n1_cnt <- sum(clin_meta_light[[group_col_meta]] == ref_g_meta, na.rm = TRUE)
                      n2_cnt <- sum(clin_meta_light[[group_col_meta]] == comp_g_meta, na.rm = TRUE)
                      if (n1_cnt > 0) n1 <- n1_cnt
                      if (n2_cnt > 0) n2 <- n2_cnt
                    }
                  }
                  hg_result <- compute_hedges_g_vec(t_stat_vals, n1, n2)
                  g_val <- hg_result$g
                  var_g <- hg_result$var_g
                  g_val[is.na(t_stat_vals)] <- NA
                  var_g[is.na(t_stat_vals)] <- NA
                  
                  de_entry <- data.frame(
                    gene = as.character(de_df[[g_col]]),
                    logFC = as.numeric(de_df[[fc_col]]),
                    pval = as.numeric(de_df[[pv_col]]),
                    se = if (!is.na(se_col)) as.numeric(de_df[[se_col]]) else rep(NA_real_, nrow(de_df)),
                    hedges_g = g_val,
                    hedges_g_se = sqrt(var_g),
                    hedges_g_var = var_g,
                    stringsAsFactors = FALSE
                  )
                  cat(sprintf("[META] Using existing moderated t-statistics from CSV for dataset %s (%s, no refit).\n", d$name, prev_de_method))
                }
              }
            }
          }
        }
        # If prev_de_method was deseq2, edger, or unknown, de_entry remains NULL so it will refit with limma-voom below
      } else {
        # method != "effect_size" (e.g. combine_pvalue, vote_counting): load from CSV
        de_table_path <- get_session_path(ds_id, "%s_de_results.csv")
        if (!file.exists(de_table_path)) de_table_path <- get_session_path(ds_id, "%s_dp_de_results.csv")
        if (!file.exists(de_table_path)) de_table_path <- sprintf("tmp/%s_de_results.csv", ds_id)
        
        if (file.exists(de_table_path)) {
          de_df <- tryCatch(read_csv_preserve_id(de_table_path), error = function(e) NULL)
          if (!is.null(de_df) && nrow(de_df) > 0) {
            gene_id_col <- if (!is.null(d$geneIdCol) && d$geneIdCol != "") d$geneIdCol else "GeneID"
            g_col <- find_gene_col_local(de_df, gene_id_col)
            fc_col <- intersect(c("logFC", "logfc", "fc", "LogFC", "log2FoldChange", "Log2FC", "log2fc"), colnames(de_df))[1]
            pv_col <- intersect(c("pValue", "pval", "p.value", "P.Value", "p", "pvalue", "PValue", "padj", "adj.P.Val", "FDR"), colnames(de_df))[1]
            se_col <- intersect(c("se", "lfcSE", "StdErr", "Std.Error", "SE"), colnames(de_df))[1]
            
            if (!is.na(g_col) && !is.na(fc_col) && !is.na(pv_col)) {
              cat(sprintf("[META] Loading pre-computed DE results for dataset %s directly from CSV (%s).\n", d$name, method))
              de_entry <- data.frame(
                gene = as.character(de_df[[g_col]]),
                logFC = as.numeric(de_df[[fc_col]]),
                pval = as.numeric(de_df[[pv_col]]),
                se = if (!is.na(se_col)) as.numeric(de_df[[se_col]]) else rep(NA_real_, nrow(de_df)),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }
    
    # 3. If de_entry is still NULL, refit with limma-voom (for raw counts) or limma (for normalized counts)
    #    Strictly saved in _de_refit_cache.rds only — NEVER overwrites DESeq2/edgeR DE results!
    if (is.null(de_entry)) {
      parsed_expr <- get_backend_dataset(ds_id, step = "de")
      dtype   <- if (!is.null(d$dataType) && nzchar(d$dataType)) d$dataType else (if (!is.null(parsed_expr$dataType)) parsed_expr$dataType else "readcounts")
      is_norm <- if (!is.null(d$isNormalized)) as_bool(d$isNormalized) else (if (!is.null(parsed_expr$isNormalized)) as_bool(parsed_expr$isNormalized) else FALSE)
      meta_path <- sprintf("tmp/%s_expr_metadata.rds", ds_id)
      if (!file.exists(meta_path)) meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
      if (file.exists(meta_path)) {
        meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
        if (is.null(d$dataType) && !is.null(meta$dataType) && nzchar(meta$dataType)) dtype <- meta$dataType
        if (is.null(d$isNormalized) && !is.null(meta$isNormalized)) is_norm <- as_bool(meta$isNormalized)
      }
      # Load expression data
      expr <- NULL
      latest_de <- get_latest_de_stack(ds_id, expr_only = TRUE)
      if (!is.null(latest_de) && !is.null(latest_de$data)) {
        expr <- latest_de$data
      }
      if (is.null(expr)) {
        if (dtype == "readcounts" && !is_norm) {
          expr <- get_latest_raw_count_matrix(ds_id)
        } else {
          expr <- get_latest_step_matrix(ds_id, "de")
        }
      }
      if (is.null(expr)) expr <- parsed_expr$expr
      if (is.null(expr)) next
      
      # Collapse duplicate Gene IDs
      gene_ids <- rownames(expr)
      if (any(duplicated(gene_ids))) {
        is_norm_data <- is_norm || isTRUE(parsed_expr$isNormalized)
        is_ma_data   <- dtype %in% c("microarray", "proteomics", "others") || tolower(parsed_expr$dataType) %in% c("microarray", "proteomics", "others")
        if (is_ma_data || is_norm_data) {
          if (requireNamespace("limma", quietly = TRUE)) {
            expr <- limma::avereps(expr, ID = gene_ids)
          } else {
            sums <- rowsum(expr, group = gene_ids, reorder = FALSE)
            counts <- as.vector(table(factor(gene_ids, levels = rownames(sums))))
            expr <- sweep(sums, 1, counts, "/")
          }
        } else {
          expr <- rowsum(expr, group = gene_ids, reorder = FALSE)
        }
      }
      
      # Load clinical data
      clin_path <- get_clinical_path(ds_id_full)
      if (!file.exists(clin_path)) {
        clin_path_short <- get_clinical_path(ds_id)
        if (file.exists(clin_path_short)) clin_path <- clin_path_short
      }
      clin_meta_path <- get_clin_metadata_path(ds_id_full)
      if (!file.exists(clin_meta_path)) {
        clin_meta_path_short <- get_clin_metadata_path(ds_id)
        if (file.exists(clin_meta_path_short)) clin_meta_path <- clin_meta_path_short
      }
      clin_df <- NULL
      meta_c <- NULL
      if (file.exists(clin_meta_path)) {
        meta_c <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
      }

      sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
        d$clinicalSampleIdCol
      } else if (!is.null(d$sampleIdCol) && nzchar(d$sampleIdCol)) {
        d$sampleIdCol
      } else if (!is.null(meta_c$sampleIdCol)) {
        meta_c$sampleIdCol
      } else {
        ""
      }

      if (file.exists(clin_path)) {
        raw_clin <- read_csv_preserve_id(clin_path)
        if (sample_id_col == "" && ncol(raw_clin) > 0) sample_id_col <- colnames(raw_clin)[1]
        clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), sample_id_col)
      } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0) {
        clin_df <- parse_clinical_data(d$clinicalParsedData, d$clinicalColumns, sample_id_col)
      }
      
      if (is.null(clin_df)) next
      
      # Align clinical data
      group_col <- if (!is.null(d$clinicalGroupCol) && nzchar(d$clinicalGroupCol)) {
        d$clinicalGroupCol
      } else if (!is.null(d$groupCol) && nzchar(d$groupCol)) {
        d$groupCol
      } else if (!is.null(meta_c$groupCol)) {
        meta_c$groupCol
      } else {
        ""
      }

      if (group_col == "" || !(group_col %in% colnames(clin_df))) {
        cand_grp <- c("Disease Type", "Type", "Group", "Condition", "Diagnosis", "Status", "Phenotype", "Class", "Disease", "disease", "group", "type")
        found_grp <- intersect(cand_grp, colnames(clin_df))
        if (length(found_grp) > 0) {
          group_col <- found_grp[1]
        }
      }

      if (is.null(group_col) || group_col == "" || !(group_col %in% colnames(clin_df))) next

      ref_g <- if (!is.null(d$de_referenceGroup) && nzchar(d$de_referenceGroup)) {
        d$de_referenceGroup
      } else if (!is.null(d$referenceGroup) && nzchar(d$referenceGroup)) {
        d$referenceGroup
      } else if (!is.null(meta_c$referenceGroup) && nzchar(meta_c$referenceGroup)) {
        meta_c$referenceGroup
      } else {
        ""
      }

      comp_g <- if (!is.null(d$de_comparisonGroup) && nzchar(d$de_comparisonGroup)) {
        d$de_comparisonGroup
      } else if (!is.null(d$comparisonGroup) && nzchar(d$comparisonGroup)) {
        d$comparisonGroup
      } else if (!is.null(meta_c$comparisonGroup) && nzchar(meta_c$comparisonGroup)) {
        meta_c$comparisonGroup
      } else {
        ""
      }

      if ((is.null(ref_g) || ref_g == "" || is.null(comp_g) || comp_g == "") && !is.null(clin_df) && nzchar(group_col) && (group_col %in% colnames(clin_df))) {
        unique_grps <- unique(clin_df[[group_col]][!is.na(clin_df[[group_col]]) & clin_df[[group_col]] != ""])
        if (length(unique_grps) > 0) {
          if (is.null(ref_g) || ref_g == "") ref_g <- unique_grps[1]
          if (is.null(comp_g) || comp_g == "") comp_g <- if (length(unique_grps) > 1) unique_grps[2] else unique_grps[1]
        }
      }

      if (is.null(ref_g) || ref_g == "" || is.null(comp_g) || comp_g == "") next

      clin_df$Group <- factor(clin_df[[group_col]], levels = c(ref_g, comp_g))
      clin_df <- clin_df[!is.na(clin_df$Group), ]
      common_samples <- intersect(colnames(expr), rownames(clin_df))
      common_samples <- common_samples[!is.na(common_samples) & common_samples != "" & common_samples != "NA" & common_samples != "NaN"]
      clin_df <- clin_df[common_samples, , drop = FALSE]
      expr <- expr[, common_samples, drop = FALSE]
      
      # Perform fitting
      de_entry <- tryCatch({
        if (ncol(expr) < 2 || nlevels(droplevels(clin_df$Group)) < 2) {
          stop("insufficient samples/classes")
        }
        design <- model.matrix(~ Group, data = clin_df)
        
        n1 <- sum(clin_df$Group == ref_g)
        n2 <- sum(clin_df$Group == comp_g)
        
        if (dtype == "readcounts" && !is_norm) {
          # Use limma-voom for raw counts
          cat(sprintf("[META] Refitting dataset %s with limma-voom for effect size meta-analysis\n", d$name))
          dge <- edgeR::DGEList(counts = expr, group = clin_df$Group)
          dge <- edgeR::calcNormFactors(dge)
          v <- limma::voom(dge, design, plot = FALSE)
          fit <- limma::lmFit(v, design)
        } else {
          # Use standard limma for normalized data (or other methods' fallbacks)
          cat(sprintf("[META] Refitting dataset %s with limma for effect size meta-analysis\n", d$name))
          fit <- limma::lmFit(expr, design)
        }
        
        fit <- limma::eBayes(fit)
        t_stat <- fit$t[, 2]
        logFC <- fit$coefficients[, 2]
        pval <- fit$p.value[, 2]

        # Vectorized Hedges' g from moderated t-statistics
        hg_result <- compute_hedges_g_vec(t_stat, n1, n2)
        g_val <- hg_result$g
        var_g <- hg_result$var_g
        # Preserve NA propagation for missing t-statistics
        g_val[is.na(t_stat)] <- NA
        var_g[is.na(t_stat)] <- NA

        df_out <- data.frame(
          gene = rownames(expr),
          logFC = logFC,
          pval = pval,
          se = fit$stdev.unscaled[, 2] * fit$sigma,
          hedges_g = g_val,
          hedges_g_se = sqrt(var_g),
          hedges_g_var = var_g,
          stringsAsFactors = FALSE
        )
        
        # Save separately in refit cache only (DO NOT overwrite primary DE results)
        tryCatch({
          cache_obj <- list(
            ref = ref_g,
            comp = comp_g,
            group_col = group_col,
            de_entry = df_out
          )
          saveRDS(cache_obj, file = get_session_path(ds_id, "%s_de_refit_cache.rds"))
          saveRDS(cache_obj, file = sprintf("tmp/%s_de_refit_cache.rds", ds_id))
        }, error = function(e) NULL)
        
        df_out
      }, error = function(e) {
        cat(sprintf("[META] refitting failed for %s: %s\n", d$name, conditionMessage(e)))
        NULL
      })
      
      # Free the large expression matrix from memory immediately after building de_entry
      if (!is.null(parsed_expr)) { rm(parsed_expr); invisible(gc(FALSE)) }
      
      if (is.null(de_entry)) next
    }
    
    de_list[[d$name]] <- de_entry
  }

  if (length(de_list) == 0) return(list())

  ds_names <- names(de_list)
  common_genes <- NULL
  union_genes <- NULL
  for (ds_name in names(de_list)) {
    g_set <- de_list[[ds_name]]$gene
    if (is.null(common_genes)) {
      common_genes <- g_set
    } else {
      common_genes <- intersect(common_genes, g_set)
    }
    if (is.null(union_genes)) {
      union_genes <- g_set
    } else {
      union_genes <- union(union_genes, g_set)
    }
  }
  overlapping_count <- if (is.null(common_genes)) 0 else length(common_genes)
  all_genes <- if (is.null(union_genes)) character(0) else as.character(union_genes)
  results <- list()
  num_datasets <- length(datasets)
  cores <- get_effective_cores()

  # Pre-build named lookup vectors for O(1) retrieval instead of subsetting dataframes inside loops
  ds_pvals      <- list()
  ds_logfcs     <- list()
  ds_hedges_gs  <- list()   # Hedges' g per gene per dataset
  ds_hedges_g_vars <- list()  # Hedges' g variance per gene per dataset
  
  for (ds_name in names(de_list)) {
    df       <- de_list[[ds_name]]
    df_genes <- as.character(df$gene)
    
    pvals <- df$pval
    names(pvals) <- df_genes
    ds_pvals[[ds_name]] <- pvals
    
    logfcs <- df$logFC
    names(logfcs) <- df_genes
    ds_logfcs[[ds_name]] <- logfcs
    
    if ("hedges_g" %in% colnames(df)) {
      hedges_gs <- df$hedges_g
      names(hedges_gs) <- df_genes
      ds_hedges_gs[[ds_name]] <- hedges_gs
      
      hedges_g_vars <- df$hedges_g_var
      names(hedges_g_vars) <- df_genes
      ds_hedges_g_vars[[ds_name]] <- hedges_g_vars
    }
    # Effect sizes are computed as Hedges' g via compute_hedges_g_vec().
  }

  # de_list is no longer needed once the O(1) lookup vectors (ds_pvals/ds_logfcs/...) are
  # built; the pooling workers use ds_names + the lookups only. Free it here so we do not
  # hold a second full copy of every dataset's per-gene stats through the mclapply pooling.
  rm(de_list); invisible(gc(FALSE))

  # Method 1: Combine P-values (Fisher's or Stouffer's via metapro)
  if (method == "combine_pvalue") {
    combine_pval_func <- function(g) {
      pvals <- numeric(num_datasets)
      dirs <- numeric(num_datasets)
      valid_count <- 0
      
      for (ds_name in ds_names) {
        pv <- ds_pvals[[ds_name]][g]
        fc <- ds_logfcs[[ds_name]][g]
        if (!is.na(pv) && !is.na(fc)) {
          valid_count <- valid_count + 1
          pvals[valid_count] <- pv
          dirs[valid_count] <- sign(fc)
        }
      }
      
      if (valid_count < 2) return(NULL)
      pvals <- pvals[1:valid_count]
      dirs  <- dirs[1:valid_count]
      pvals[pvals == 0] <- 1e-16

      if (!is.null(pvalue_method) && pvalue_method == "stouffer") {
        # Stouffer's method (Z-score method) using metap / metapro
        combined_pval <- tryCatch({
          eff_signs <- ifelse(dirs >= 0, 1, -1)
          res <- metapro::wZ(p = pvals, eff.sign = eff_signs, is.onetail = FALSE)
          res$p
        }, error = function(e) {
          tryCatch({
            p_one <- metap::two2one(pvals, invert = (dirs < 0))
            res <- metap::sumz(p_one)
            2 * pnorm(abs(res$z), lower.tail = FALSE)
          }, error = function(e2) {
            # Fallback to manual Z-score combination
            z_scores <- sapply(seq_along(pvals), function(i) {
              p <- pvals[i]
              p <- max(min(p, 1 - 1e-16), 1e-16)
              p_one <- if (dirs[i] >= 0) p / 2 else 1 - p / 2
              qnorm(p_one, lower.tail = FALSE)
            })
            z_scores <- z_scores[!is.na(z_scores) & !is.infinite(z_scores)]
            if (length(z_scores) == 0) {
              1.0
            } else {
              z_comb <- sum(z_scores) / sqrt(length(z_scores))
              2 * pnorm(abs(z_comb), lower.tail = FALSE)
            }
          })
        })
      } else {
        # Fisher's method using metap / metapro
        combined_pval <- tryCatch({
          eff_signs <- ifelse(dirs >= 0, 1, -1)
          res <- metapro::wFisher(p = pvals, eff.sign = eff_signs, is.onetail = FALSE)
          res$p
        }, error = function(e) {
          tryCatch({
            p_one_up <- metap::two2one(pvals, invert = (dirs < 0))
            p_one_dn <- metap::two2one(pvals, invert = (dirs > 0))
            res_up <- metap::sumlog(p_one_up)
            res_dn <- metap::sumlog(p_one_dn)
            min(1.0, 2 * min(res_up$p, res_dn$p))
          }, error = function(e2) {
            # Fallback to manual Fisher
            p_ones_up <- sapply(seq_along(pvals), function(i) if (dirs[i] >= 0) pvals[i]/2 else 1 - pvals[i]/2)
            p_ones_dn <- sapply(seq_along(pvals), function(i) if (dirs[i] < 0) pvals[i]/2 else 1 - pvals[i]/2)
            p_ones_up[p_ones_up == 0] <- 1e-16
            p_ones_dn[p_ones_dn == 0] <- 1e-16
            chisq_up <- -2 * sum(log(p_ones_up))
            chisq_dn <- -2 * sum(log(p_ones_dn))
            p_up <- pchisq(chisq_up, df = 2 * length(pvals), lower.tail = FALSE)
            p_dn <- pchisq(chisq_dn, df = 2 * length(pvals), lower.tail = FALSE)
            min(1.0, 2 * min(p_up, p_dn))
          })
        })
      }
      
      logfcs <- numeric(length(ds_names))
      valid_fc_count <- 0
      for (ds_name in ds_names) {
        fc <- ds_logfcs[[ds_name]][g]
        if (!is.na(fc)) {
          valid_fc_count <- valid_fc_count + 1
          logfcs[valid_fc_count] <- fc
        }
      }
      combined_fc <- if (valid_fc_count > 0) mean(logfcs[1:valid_fc_count]) else 0

      avg_dir <- sum(dirs)
      dir_label <- ifelse(avg_dir >= 0, "Up", "Down")
      fold_change <- 2^combined_fc

      list(
        Feature = g,
        FoldChange = fold_change,
        `Combined LogFC` = combined_fc,
        combined = combined_fc,
        `P-value` = combined_pval,
        `adj.P.Val.` = combined_pval,
        gene = g,
        fc = combined_fc,
        logFC = combined_fc,
        se = 0,
        ci = "",
        ci_lower = 0,
        ci_upper = 0,
        pval = combined_pval,
        qval = combined_pval,
        dir = dir_label
      )
    }
    
    combined_rows <- if (length(all_genes) > 2000 && cores > 1) {
      run_parallel_lapply(
        all_genes, combine_pval_func,
        var_list = c("num_datasets", "ds_names", "ds_pvals", "ds_logfcs", "pvalue_method"),
        pkg_list = c("metapro", "metap"),
        cores = cores
      )
    } else {
      lapply(all_genes, combine_pval_func)
    }
    
    combined_rows <- combined_rows[!sapply(combined_rows, is.null)]
    if (length(combined_rows) > 0) {
      p_vals <- sapply(combined_rows, function(r) r$pval)
      q_vals <- p.adjust(p_vals, method = meta_adjust_method)
      for (i in seq_along(combined_rows)) {
        combined_rows[[i]]$qval <- q_vals[i]
        combined_rows[[i]]$`adj.P.Val.` <- q_vals[i]
      }
      combined_rows <- combined_rows[order(p_vals)]
    }
    
    # Generate volcano plot for combine_pvalue (Combined Log2FC vs FDR)
    volc_b64 <- ""
    if (length(combined_rows) > 0 && requireNamespace("ggplot2", quietly = TRUE)) {
      volc_df <- data.frame(
        gene = sapply(combined_rows, function(r) r$gene),
        logFC = sapply(combined_rows, function(r) r$logFC),
        qValue = sapply(combined_rows, function(r) r$qval),
        significant = sapply(combined_rows, function(r) r$qval < pval_thresh & abs(r$logFC) >= logfc_thresh),
        direction = sapply(combined_rows, function(r) ifelse(r$qval < pval_thresh & abs(r$logFC) >= logfc_thresh, ifelse(r$logFC > 0, "up", "down"), "ns"))
      )
      p_volc <- ggplot2::ggplot(volc_df, ggplot2::aes(x = logFC, y = -log10(qValue), color = direction)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("up" = "#ef4444", "down" = "#3b82f6", "ns" = "#94a3b8")) +
        ggplot2::geom_vline(xintercept = c(-logfc_thresh, logfc_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "log2 Fold Change", y = "-log10(adj. p-value)") +
        ggplot2::theme(legend.position = "bottom")
      
      volc_b64 <- plot_to_base64(p_volc)
    }
    top10_rows <- head(combined_rows, 10)
    
    num_sig <- if (length(combined_rows) > 0) sum(sapply(combined_rows, function(r) r$qval < pval_thresh & abs(r$logFC) >= logfc_thresh)) else 0
    sig_up  <- if (length(combined_rows) > 0) sum(sapply(combined_rows, function(r) r$qval < pval_thresh & abs(r$logFC) >= logfc_thresh & r$logFC > 0)) else 0
    sig_down <- if (length(combined_rows) > 0) sum(sapply(combined_rows, function(r) r$qval < pval_thresh & abs(r$logFC) >= logfc_thresh & r$logFC < 0)) else 0

    return(list(
      results = if (length(top10_rows) > 0) top10_rows else list(),
      top10 = if (length(top10_rows) > 0) top10_rows else list(),
      full_results = if (length(combined_rows) > 0) combined_rows else list(),
      stats = list(
        totalFeatures = length(combined_rows),
        numDatasets = num_datasets,
        overlappingGenes = overlapping_count,
        numSignificant = num_sig,
        sigUp = sig_up,
        sigDown = sig_down
      ),
      volcanoPlot = volc_b64,
      maPlot = "",
      forestPlot = ""
    ))
  }
  
  # Method 2: Effect Size Meta-analysis (metafor REML & Fixed Effects)
  if (method == "effect_size") {
    target_rma_method <- if (identical(effect_size_model, "fixed")) "FE" else "REML"
    
    # Extract matrices across all genes
    y_mat <- do.call(cbind, lapply(ds_names, function(d) ds_hedges_gs[[d]][all_genes]))
    v_mat <- do.call(cbind, lapply(ds_names, function(d) ds_hedges_g_vars[[d]][all_genes]))
    rownames(y_mat) <- all_genes
    rownames(v_mat) <- all_genes
    
    valid_mask <- !is.na(y_mat) & !is.na(v_mat) & !is.infinite(v_mat) & (v_mat > 0)
    k_vec <- rowSums(valid_mask)
    valid_genes_idx <- unname(which(k_vec >= 2))
    
    fit_gene_effect_size <- function(i) {
      gene_name <- all_genes[i]
      mask_i <- valid_mask[i, ]
      yi_sub <- y_mat[i, mask_i]
      vi_sub <- v_mat[i, mask_i]
      k_sub <- length(yi_sub)
      if (k_sub < 2) return(NULL)
      
      fit_meta <- tryCatch(
        metafor::rma(yi = yi_sub, vi = vi_sub, method = target_rma_method),
        error = function(e) {
          # Fallback
          w <- 1 / vi_sub
          w[is.infinite(w)] <- 0
          sum_w <- sum(w, na.rm = TRUE)
          if (sum_w <= 0 || is.na(sum_w)) return(NULL)
          
          if (target_rma_method == "FE") {
            fe_est <- sum(w * yi_sub, na.rm = TRUE) / sum_w
            fe_se <- 1 / sqrt(sum_w)
            z_val <- fe_est / max(fe_se, 1e-16)
            p_val <- 2 * pnorm(abs(z_val), lower.tail = FALSE)
            list(
              beta = fe_est,
              se = fe_se,
              pval = p_val,
              ci.lb = fe_est - 1.96 * fe_se,
              ci.ub = fe_est + 1.96 * fe_se,
              tau2 = 0,
              I2 = 0
            )
          } else {
            fe_est <- sum(w * yi_sub, na.rm = TRUE) / sum_w
            q_stat <- sum(w * (yi_sub - fe_est)^2, na.rm = TRUE)
            c_val <- sum_w - (sum(w^2, na.rm = TRUE) / sum_w)
            tau2_est <- max(0, (q_stat - (k_sub - 1)) / max(c_val, 1e-16))
            rw <- 1 / (vi_sub + tau2_est)
            sum_rw <- sum(rw, na.rm = TRUE)
            if (sum_rw <= 0) return(NULL)
            re_est <- sum(rw * yi_sub, na.rm = TRUE) / sum_rw
            re_se <- 1 / sqrt(sum_rw)
            z_val <- re_est / max(re_se, 1e-16)
            p_val <- 2 * pnorm(abs(z_val), lower.tail = FALSE)
            i2_est <- if (q_stat > 0) 100 * max(0, (q_stat - (k_sub - 1)) / q_stat) else 0
            list(
              beta = re_est,
              se = re_se,
              pval = p_val,
              ci.lb = re_est - 1.96 * re_se,
              ci.ub = re_est + 1.96 * re_se,
              tau2 = tau2_est,
              I2 = i2_est
            )
          }
        }
      )
      
      if (is.null(fit_meta) || is.null(fit_meta$pval) || is.na(fit_meta$pval)) return(NULL)
      
      d_val  <- as.numeric(fit_meta$beta[1])
      se_val <- as.numeric(fit_meta$se)
      ci_lb  <- as.numeric(fit_meta$ci.lb)
      ci_ub  <- as.numeric(fit_meta$ci.ub)
      pval_val <- as.numeric(fit_meta$pval)
      tau2_val <- if (!is.null(fit_meta$tau2) && !is.na(fit_meta$tau2)) as.numeric(fit_meta$tau2) else 0
      tau_val  <- sqrt(max(0, tau2_val))
      i2_val   <- if (!is.null(fit_meta$I2) && !is.na(fit_meta$I2)) as.numeric(fit_meta$I2) else 0
      
      fold_change <- 2^d_val
      ci_str <- sprintf("[%.2f, %.2f]", ci_lb, ci_ub)
      
      list(
        Feature = gene_name,
        FoldChange = fold_change,
        `Combined Effects Size` = d_val,
        combined = d_val,
        `P-value` = pval_val,
        `adj.P.Val.` = pval_val,
        gene = gene_name,
        hedges_g = d_val,
        se = se_val,
        ci = ci_str,
        ci_lower = ci_lb,
        ci_upper = ci_ub,
        pval = pval_val,
        qval = pval_val,
        tau2 = round(tau2_val, 4),
        tau = round(tau_val, 4),
        i2 = round(i2_val, 2)
      )
    }
    
    if (length(valid_genes_idx) == 0) {
      effect_rows <- list()
    } else if (length(valid_genes_idx) > 2000 && cores > 1) {
      effect_rows <- run_parallel_lapply(
        valid_genes_idx, fit_gene_effect_size,
        var_list = c("all_genes", "valid_mask", "y_mat", "v_mat", "target_rma_method"),
        pkg_list = c("metafor"),
        cores = cores
      )
      effect_rows <- effect_rows[!sapply(effect_rows, is.null)]
    } else {
      effect_rows <- lapply(valid_genes_idx, fit_gene_effect_size)
      effect_rows <- effect_rows[!sapply(effect_rows, is.null)]
    }
    
    if (length(effect_rows) > 0) {
      p_vals <- sapply(effect_rows, function(r) r$pval)
      q_vals <- p.adjust(p_vals, method = meta_adjust_method)
      for (i in seq_along(effect_rows)) {
        effect_rows[[i]]$qval <- q_vals[i]
        effect_rows[[i]]$`adj.P.Val.` <- q_vals[i]
      }
      effect_rows <- effect_rows[order(p_vals)]
    }
    
    volc_b64 <- ""
    forest_b64 <- ""
    if (length(effect_rows) > 0 && requireNamespace("ggplot2", quietly = TRUE)) {
      volc_df <- data.frame(
        gene = sapply(effect_rows, function(r) r$gene),
        hedges_g = sapply(effect_rows, function(r) r$hedges_g),
        qValue = sapply(effect_rows, function(r) r$qval),
        significant = sapply(effect_rows, function(r) r$qval < pval_thresh & !(r$ci_lower <= 0 & r$ci_upper >= 0) & abs(r$hedges_g) >= logfc_thresh),
        direction = sapply(effect_rows, function(r) ifelse(r$qval < pval_thresh & !(r$ci_lower <= 0 & r$ci_upper >= 0) & abs(r$hedges_g) >= logfc_thresh, ifelse(r$hedges_g > 0, "up", "down"), "ns"))
      )
      p_volc <- ggplot2::ggplot(volc_df, ggplot2::aes(x = hedges_g, y = -log10(qValue), color = direction)) +
        ggplot2::geom_point(size = 1.5, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("up" = "#ef4444", "down" = "#3b82f6", "ns" = "#94a3b8")) +
        ggplot2::geom_vline(xintercept = c(-logfc_thresh, logfc_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed", color = "darkgray") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Hedges' g", y = "-log10(adj. p-value)") +
        ggplot2::theme(legend.position = "bottom")
      
      volc_b64 <- plot_to_base64(p_volc)
      
      top10_rows_temp <- head(effect_rows, 10)
      if (length(top10_rows_temp) > 0) {
        y_lab <- if (is_proteomics) "Protein" else "Gene"
        y_face <- if (is_proteomics) "plain" else "italic"
        cat(sprintf("[META] Inline Forest plot generated [DataClass: %s | Omics Type: %s | y-axis: '%s' | font: '%s']\n",
                    data_class %||% "transcriptomics",
                    if (is_proteomics) "PROTEOMICS" else "TRANSCRIPTOMICS",
                    y_lab,
                    y_face))

        df_forest <- data.frame(
          Gene     = sapply(top10_rows_temp, function(r) r$gene),
          Estimate = sapply(top10_rows_temp, function(r) r$hedges_g),
          Lower    = sapply(top10_rows_temp, function(r) r$ci_lower),
          Upper    = sapply(top10_rows_temp, function(r) r$ci_upper),
          stringsAsFactors = FALSE
        )
        p_forest <- ggplot2::ggplot(df_forest, ggplot2::aes(x = Estimate, y = reorder(Gene, Estimate))) +
          ggplot2::geom_point(size = 3, color = "#2563eb") +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower, xmax = Upper), height = 0.2, color = "#1e293b", linewidth = 0.8) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#94a3b8") +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = "Combined Effect Size (Hedges' g)", y = y_lab) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(face = y_face))
        forest_b64 <- plot_to_base64(p_forest)
      }
    }
    top10_rows <- head(effect_rows, 10)
    
    all_tau2 <- if (length(effect_rows) > 0) sapply(effect_rows, function(r) r$tau2) else numeric(0)
    all_i2 <- if (length(effect_rows) > 0) sapply(effect_rows, function(r) r$i2) else numeric(0)
    median_tau2 <- if (length(all_tau2) > 0) median(all_tau2, na.rm = TRUE) else 0
    median_tau <- sqrt(max(0, median_tau2))
    median_i2 <- if (length(all_i2) > 0) median(all_i2, na.rm = TRUE) else 0

    num_sig <- if (length(effect_rows) > 0) sum(sapply(effect_rows, function(r) r$qval < pval_thresh & !(r$ci_lower <= 0 & r$ci_upper >= 0) & abs(r$hedges_g) >= logfc_thresh)) else 0
    sig_up  <- if (length(effect_rows) > 0) sum(sapply(effect_rows, function(r) r$qval < pval_thresh & !(r$ci_lower <= 0 & r$ci_upper >= 0) & abs(r$hedges_g) >= logfc_thresh & r$hedges_g > 0)) else 0
    sig_down <- if (length(effect_rows) > 0) sum(sapply(effect_rows, function(r) r$qval < pval_thresh & !(r$ci_lower <= 0 & r$ci_upper >= 0) & abs(r$hedges_g) >= logfc_thresh & r$hedges_g < 0)) else 0

    return(list(
      results = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      top10 = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      full_results = if (length(effect_rows) > 0) unname(effect_rows) else list(),
      stats = list(
        totalFeatures = length(effect_rows),
        numDatasets = num_datasets,
        overlappingGenes = overlapping_count,
        numSignificant = num_sig,
        sigUp = sig_up,
        sigDown = sig_down,
        tau2 = round(median_tau2, 4),
        tau = round(median_tau, 4),
        i2 = round(median_i2, 2)
      ),
      volcanoPlot = volc_b64,
      maPlot = "",
      forestPlot = forest_b64
    ))
  }
  
  # Method 3: Vote Counting
  if (method == "vote_counting") {
    min_votes <- if (is.null(votes)) 2 else as.integer(votes)
    total_ds <- length(ds_names)
    
    vote_func <- function(g) {
      net_votes <- 0
      
      for (ds_name in ds_names) {
        pv <- ds_pvals[[ds_name]][g]
        fc <- ds_logfcs[[ds_name]][g]
        if (!is.na(pv) && !is.na(fc)) {
          if (pv < 0.05) {
            if (fc > 0) {
              net_votes <- net_votes + 1
            } else if (fc < 0) {
              net_votes <- net_votes - 1
            }
          }
        }
      }
      
      if (abs(net_votes) < min_votes) return(NULL)
      
      dir_label <- ifelse(net_votes > 0, "Up", "Down")
      
      list(
        gene = g,
        votes = abs(net_votes),
        total = total_ds,
        dir = dir_label
      )
    }
    
    vote_rows <- run_parallel_lapply(
      all_genes, vote_func,
      var_list = c("ds_names", "ds_pvals", "ds_logfcs", "min_votes", "total_ds"),
      cores = cores
    )
    
    vote_rows <- vote_rows[!sapply(vote_rows, is.null)]
    if (length(vote_rows) > 0) {
      vote_rows <- vote_rows[order(sapply(vote_rows, function(r) r$votes), decreasing = TRUE)]
    }
    
    top10_rows <- head(vote_rows, 10)
    sig_up <- if (length(vote_rows) > 0) sum(sapply(vote_rows, function(r) r$dir == "Up")) else 0
    sig_down <- if (length(vote_rows) > 0) sum(sapply(vote_rows, function(r) r$dir == "Down")) else 0

    return(list(
      results = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      top10 = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      full_results = if (length(vote_rows) > 0) unname(vote_rows) else list(),
      stats = list(
        totalFeatures = length(vote_rows),
        numDatasets = num_datasets,
        overlappingGenes = overlapping_count,
        numSignificant = length(vote_rows),
        sigUp = sig_up,
        sigDown = sig_down
      ),
      volcanoPlot = "",
      maPlot = ""
    ))
  }
  
  # Method 4: Shared Genes
  if (method == "shared_genes") {
    shared_func <- function(g) {
      presence <- character(length(ds_names))
      logfcs <- numeric(length(ds_names))
      pvals <- numeric(length(ds_names))
      dirs <- numeric(length(ds_names))
      valid_count <- 0
      
      for (ds_name in ds_names) {
        pv <- ds_pvals[[ds_name]][g]
        fc <- ds_logfcs[[ds_name]][g]
        if (!is.na(pv) && !is.na(fc)) {
          valid_count <- valid_count + 1
          presence[valid_count] <- ds_name
          logfcs[valid_count] <- fc
          pvals[valid_count] <- pv
          dirs[valid_count] <- sign(fc)
        }
      }
      
      # Must be present and significant (p < 0.05) in ALL studies!
      if (valid_count < length(ds_names)) return(NULL)
      if (!all(pvals < 0.05)) return(NULL)
      # Check for direction consensus: all positive or all negative logFC
      if (!(all(logfcs > 0) || all(logfcs < 0))) return(NULL)
      
      avg_dir <- sum(dirs)
      dir_label <- ifelse(avg_dir >= 0, "Up", "Down")
      range_str <- sprintf("[%.2f, %.2f]", min(logfcs), max(logfcs))

      list(
        gene = g,
        presence = paste(presence, collapse = ", "),
        dir = dir_label,
        range = range_str,
        fc = mean(logfcs),
        logFC = mean(logfcs),
        pval = mean(pvals),
        qval = mean(pvals)
      )
    }
    
    shared_rows <- run_parallel_lapply(
      all_genes, shared_func,
      var_list = c("ds_names", "ds_pvals", "ds_logfcs"),
      cores = cores
    )
    
    shared_rows <- shared_rows[!sapply(shared_rows, is.null)]
    if (length(shared_rows) > 0) {
      presence_counts <- sapply(shared_rows, function(r) length(strsplit(r$presence, ", ")[[1]]))
      shared_rows <- shared_rows[order(presence_counts, decreasing = TRUE)]
    }
    
    top10_rows <- head(shared_rows, 10)
    sig_up <- if (length(shared_rows) > 0) sum(sapply(shared_rows, function(r) r$dir == "Up")) else 0
    sig_down <- if (length(shared_rows) > 0) sum(sapply(shared_rows, function(r) r$dir == "Down")) else 0

    return(list(
      results = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      top10 = if (length(top10_rows) > 0) unname(top10_rows) else list(),
      full_results = if (length(shared_rows) > 0) unname(shared_rows) else list(),
      stats = list(
        totalFeatures = length(shared_rows),
        numDatasets = num_datasets,
        overlappingGenes = overlapping_count,
        numSignificant = length(shared_rows),
        sigUp = sig_up,
        sigDown = sig_down
      ),
      volcanoPlot = "",
      maPlot = ""
    ))
  }
  
  return(results)
}

run_inline_de_analysis <- function(payload) {
  if (is.null(payload) || length(payload) == 0) {
    stop("No valid datasets provided for DE analysis.")
  }
  first_obj <- payload[[1]]
  method_val   <- if (!is.null(first_obj$method)) first_obj$method else "deseq2"
  pval_thresh  <- if (!is.null(first_obj$pValueThreshold)) as.numeric(first_obj$pValueThreshold) else 0.05
  logfc_thresh <- if (!is.null(first_obj$logFcThreshold))  as.numeric(first_obj$logFcThreshold)  else 1.0
  adjust_method <- if (!is.null(first_obj$adjustMethod)) first_obj$adjustMethod else "BH"

  datasets <- lapply(payload, function(d) {
    ds_id     <- d$datasetId
    data_type <- if (!is.null(d$dataType)) d$dataType else "readcounts"
    is_norm   <- isTRUE(d$isNormalized)

    # 1. Initialize inline DE stacks using processed data from DP module
    dp_main <- get_latest_main_stack(ds_id)
    if (!is.null(dp_main)) {
      push_inline_de_main_stack(ds_id, dp_main$data, step_name = "dp_processed", metadata = dp_main$metadata)
    }
    dp_counts <- get_latest_counts_stack(ds_id)
    if (!is.null(dp_counts)) {
      push_inline_de_counts_stack(ds_id, dp_counts$data, step_name = "dp_processed", metadata = dp_counts$metadata)
    }

    # 2. Resolve expression matrix from the inline DE stacks
    expr_mat <- NULL
    if (data_type == "readcounts" && !is_norm) {
      latest_c <- get_latest_inline_de_counts_stack(ds_id)
      if (!is.null(latest_c)) {
        expr_mat <- latest_c$data
      }
      cat(sprintf("[INLINE-DE] %s: resolved count matrix from inline DE counts stack.\n", ds_id))
    } else {
      latest_m <- get_latest_inline_de_main_stack(ds_id)
      if (!is.null(latest_m)) {
        expr_mat <- latest_m$data
      }
      if (is.null(expr_mat)) {
        parsed_expr <- get_backend_dataset(ds_id)
        if (!is.null(parsed_expr)) {
          expr_mat <- parsed_expr$expr
        }
      }
      cat(sprintf("[INLINE-DE] %s: resolved latest matrix from inline DE main stack.\n", ds_id))
    }
    
    # Save to de_stack
    if (!is.null(expr_mat)) {
      push_de_stack(ds_id, expr_mat, step_name = "de", metadata = list(dataType = data_type, isNormalized = is_norm))
    }
    
    sync_dataset_metadata(d, module = "de")

    # Load clinical metadata
    clin_path      <- get_clinical_path(ds_id)
    if (!file.exists(clin_path)) {
      clin_path_full <- get_clinical_path(d$id %||% ds_id)
      if (file.exists(clin_path_full)) clin_path <- clin_path_full
    }
    clin_meta_path <- get_clin_metadata_path(ds_id)
    if (!file.exists(clin_meta_path)) {
      clin_meta_path_full <- get_clin_metadata_path(d$id %||% ds_id)
      if (file.exists(clin_meta_path_full)) clin_meta_path <- clin_meta_path_full
    }
    clin_df <- NULL
    meta_clin <- if (file.exists(clin_meta_path)) tryCatch(readRDS(clin_meta_path), error = function(e) NULL) else NULL

    clin_sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
      d$clinicalSampleIdCol
    } else if (!is.null(meta_clin$sampleIdCol)) {
      meta_clin$sampleIdCol
    } else {
      ""
    }
    clin_group_col <- if (!is.null(d$clinicalGroupCol) && nzchar(d$clinicalGroupCol)) {
      d$clinicalGroupCol
    } else if (!is.null(d$groupCol) && nzchar(d$groupCol)) {
      d$groupCol
    } else if (!is.null(meta_clin$groupCol)) {
      meta_clin$groupCol
    } else {
      ""
    }
    clin_batch_col <- if (!is.null(d$clinicalBatchCol) && nzchar(d$clinicalBatchCol)) {
      d$clinicalBatchCol
    } else if (!is.null(d$batchCol) && nzchar(d$batchCol)) {
      d$batchCol
    } else if (!is.null(meta_clin$batchCol)) {
      meta_clin$batchCol
    } else {
      ""
    }

    if (file.exists(clin_path)) {
      raw_clin <- read.csv(clin_path, check.names = FALSE, stringsAsFactors = FALSE)
      if (clin_sample_id_col == "" && ncol(raw_clin) > 0) clin_sample_id_col <- colnames(raw_clin)[1]
      clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), clin_sample_id_col)
    } else if (!is.null(d$clinicalParsedData) && length(d$clinicalParsedData) > 0) {
      rows_padded <- lapply(d$clinicalParsedData, function(x) {
        sapply(x, function(val) if (is.null(val)) "" else as.character(val))
      })
      raw_clin <- as.data.frame(do.call(rbind, rows_padded), stringsAsFactors=FALSE)
      if (!is.null(d$clinicalColumns) && length(d$clinicalColumns) == ncol(raw_clin)) {
        colnames(raw_clin) <- unlist(d$clinicalColumns)
      }
      if (clin_sample_id_col == "" && ncol(raw_clin) > 0) clin_sample_id_col <- colnames(raw_clin)[1]
      clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), clin_sample_id_col)
    }

    if (!is.null(clin_df)) {
      if (clin_group_col == "" || !(clin_group_col %in% colnames(clin_df))) {
        cand_grp <- c("Disease Type", "Type", "Group", "Condition", "Diagnosis", "Status", "Phenotype", "Class", "Disease", "disease", "group", "type")
        found_grp <- intersect(cand_grp, colnames(clin_df))
        if (length(found_grp) > 0) clin_group_col <- found_grp[1]
      }
      if (clin_batch_col == "" || !(clin_batch_col %in% colnames(clin_df))) {
        cand_batch <- c("Batch", "batch", "Center", "Site", "Plate", "Run", "Cohort", "Study")
        found_batch <- intersect(cand_batch, colnames(clin_df))
        if (length(found_batch) > 0) clin_batch_col <- found_batch[1]
      }
    }

    ref_grp <- if (!is.null(d$referenceGroup) && nzchar(d$referenceGroup)) {
      d$referenceGroup
    } else if (!is.null(d$de_referenceGroup) && nzchar(d$de_referenceGroup)) {
      d$de_referenceGroup
    } else if (!is.null(meta_clin$referenceGroup) && nzchar(meta_clin$referenceGroup)) {
      meta_clin$referenceGroup
    } else {
      ""
    }

    cmp_grp <- if (!is.null(d$comparisonGroup) && nzchar(d$comparisonGroup)) {
      d$comparisonGroup
    } else if (!is.null(d$de_comparisonGroup) && nzchar(d$de_comparisonGroup)) {
      d$de_comparisonGroup
    } else if (!is.null(meta_clin$comparisonGroup) && nzchar(meta_clin$comparisonGroup)) {
      meta_clin$comparisonGroup
    } else {
      ""
    }

    if ((ref_grp == "" || cmp_grp == "") && !is.null(clin_df) && nzchar(clin_group_col) && (clin_group_col %in% colnames(clin_df))) {
      unique_grps <- unique(clin_df[[clin_group_col]][!is.na(clin_df[[clin_group_col]]) & clin_df[[clin_group_col]] != ""])
      if (length(unique_grps) > 0) {
        if (ref_grp == "") ref_grp <- unique_grps[1]
        if (cmp_grp == "") cmp_grp <- if (length(unique_grps) > 1) unique_grps[2] else unique_grps[1]
      }
    }

    # Save updated clin metadata to disk
    updated_clin_meta <- list(
      sampleIdCol     = clin_sample_id_col,
      groupCol        = clin_group_col,
      batchCol        = clin_batch_col,
      referenceGroup  = ref_grp,
      comparisonGroup = cmp_grp
    )
    tryCatch(saveRDS(updated_clin_meta, clin_meta_path), error = function(e) NULL)
    tryCatch(saveRDS(updated_clin_meta, sprintf("tmp/%s_clin_metadata.rds", ds_id)), error = function(e) NULL)

    d_method <- if (!is.null(d$method) && nzchar(d$method)) d$method else method_val
    if (data_type %in% c("microarray", "proteomics", "others") || is_norm) {
      d_method <- "limma"
    }

    list(
      id                = ds_id,
      name              = d$name,
      dataType          = data_type,
      isNormalized      = is_norm,
      referenceGroup    = ref_grp,
      comparisonGroup   = cmp_grp,
      clinicalSampleIdCol = clin_sample_id_col,
      clinicalGroupCol  = clin_group_col,
      clinicalBatchCol  = clin_batch_col,
      method            = d_method,
      pValueThreshold   = pval_thresh,
      logFcThreshold    = logfc_thresh,
      adjustMethod      = adjust_method,
      resolvedExpr      = expr_mat,
      resolvedClin      = clin_df
    )
  })

  # Build DE-compatible datasets by injecting resolved matrices
  de_datasets <- lapply(datasets, function(d) {
    expr_mat <- d[["resolvedExpr"]]
    clin_df  <- d[["resolvedClin"]]
    if (is.null(expr_mat)) {
      cat(sprintf("[INLINE-DE] WARNING: no expression matrix for %s. Skipping.\n", d$id))
      return(NULL)
    }
    # Convert matrix back to parsedData / columns for run_de_analysis
    samples <- colnames(expr_mat)
    if (!is.null(clin_df)) {
      common <- intersect(samples, rownames(clin_df))
      common <- common[!is.na(common) & common != "" & common != "NA" & common != "NaN"]
      expr_mat <- expr_mat[, common, drop = FALSE]
      samples  <- common
    }
    gene_id_col <- if (!is.null(d$geneIdCol) && d$geneIdCol != "") d$geneIdCol else "GeneID"
    gene_ids <- rownames(expr_mat)
    mat_df <- as.data.frame(expr_mat)
    mat_df <- cbind(gene_ids, mat_df)
    colnames(mat_df)[1] <- gene_id_col
    list(
      id                  = d$id,
      name                = d$name,
      dataType            = d$dataType,
      isNormalized        = d$isNormalized,
      referenceGroup      = d$referenceGroup,
      comparisonGroup     = d$comparisonGroup,
      parsedData          = lapply(1:nrow(mat_df), function(r) unname(as.list(mat_df[r,]))),
      columns             = colnames(mat_df),
      clinicalParsedData  = if (!is.null(clin_df)) lapply(1:nrow(clin_df), function(r) unname(as.list(clin_df[r,]))) else list(),
      clinicalColumns     = if (!is.null(clin_df)) colnames(clin_df) else character(0),
      clinicalSampleIdCol = d$clinicalSampleIdCol,
      clinicalGroupCol    = d$clinicalGroupCol,
      clinicalBatchCol    = d$clinicalBatchCol,
      method              = d$method,
      pValueThreshold     = d$pValueThreshold,
      logFcThreshold      = d$logFcThreshold,
      adjustMethod        = d$adjustMethod
    )
  })
  de_datasets <- Filter(Negate(is.null), de_datasets)
  if (length(de_datasets) == 0) {
    stop("No processed expression data found in Data Processing pipeline. Please run the Data Processing steps before running Inline DE.")
  }

  res <- run_de_analysis(method_val, pval_thresh, logfc_thresh, adjust_method, de_datasets)

  if (!exists("finalize_de_results", mode = "function")) {
    if (file.exists("report_finalize.R")) {
      source("report_finalize.R")
    } else if (file.exists("backend/report_finalize.R")) {
      source("backend/report_finalize.R")
    }
  }
  if (exists("finalize_de_results", mode = "function")) {
    finalize_de_results(res, de_datasets, pval_thresh, logfc_thresh, adjust_method, method_val)
  }
  return(res)
}


run_inline_de_meta <- function(payload) {
  first_obj     <- payload[[1]]
  method_val    <- first_obj$method
  pvalue_method <- first_obj$pvalueMethod
  eff_model     <- first_obj$effectSizeModel
  votes         <- as.numeric(first_obj$votes)
  pval_thresh   <- if (!is.null(first_obj$pValueThreshold)) as.numeric(first_obj$pValueThreshold) else 0.05
  logfc_thresh  <- if (!is.null(first_obj$logFcThreshold))  as.numeric(first_obj$logFcThreshold)  else 1.0

  datasets <- lapply(payload, function(d) {
    list(
      id                 = d$datasetId,
      name               = d$name,
      de_referenceGroup  = d$de_referenceGroup,
      de_comparisonGroup = d$de_comparisonGroup
    )
  })

  # Group datasets by data class
  datasets_by_class <- list()
  for (d in datasets) {
    ds_id <- if (!is.null(d$id)) d$id else d$datasetId
    dclass <- get_dataset_data_class(ds_id)
    if (dclass %in% c("transcriptomics", "proteomics")) {
      if (is.null(datasets_by_class[[dclass]])) datasets_by_class[[dclass]] <- list()
      datasets_by_class[[dclass]][[length(datasets_by_class[[dclass]]) + 1]] <- d
    }
  }
  
  final_res <- NULL
  all_full_rows <- list()
  first_ds_id <- if (length(datasets) > 0) datasets[[1]]$id else "dp"
  parsed <- get_backend_datasets(first_ds_id)
  user_id <- if (!is.null(parsed$user_id) && parsed$user_id != "") parsed$user_id else "user"
  module <- if (!is.null(parsed$module) && parsed$module != "") parsed$module else "dp"
  meta_ds_id <- sprintf("%s_%s_meta", user_id, module)
  
  for (dclass in names(datasets_by_class)) {
    class_ds <- datasets_by_class[[dclass]]
    if (length(class_ds) > 1) {
      res <- run_meta_analysis(method_val, pvalue_method, eff_model, votes, class_ds, pval_thresh, logfc_thresh, data_class = dclass)
      final_res <- res
      
      full_rows <- if (!is.null(res$full_results)) res$full_results else (if (!is.null(res$results)) res$results else res)
      res$full_results <- NULL               # Drop large result from the res object

      # Write CSV, generate plots, and convert to df_meta WHILE full_rows is still in scope
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
        forest_p  <- get_session_path(meta_ds_id, "%s_forest_plots.pdf")
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
            push_inline_de_main_stack(d_id, df_meta, step_name = "meta", metadata = list(method = method_val))
          }
        }
      }
      # Accumulate for final stats, then free full_rows — all downstream consumers are done
      if (is.list(full_rows)) {
        all_full_rows <- c(all_full_rows, full_rows)
        rm(full_rows); invisible(gc(FALSE))
      }
    }
  }
  
  if (!is.null(final_res)) {
    final_res$full_results <- all_full_rows
    final_res$results <- all_full_rows
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
    num_sig_val <- if (is_vote || is_shared) length(all_full_rows) else sum(vapply(all_full_rows, .sig, logical(1)))
    sig_up_val  <- sum(vapply(all_full_rows, function(r) (is_vote || is_shared || .sig(r)) && .up(r), logical(1)))
    sig_dn_val  <- sum(vapply(all_full_rows, function(r) (is_vote || is_shared || .sig(r)) && .dn(r), logical(1)))
    
    orig_stats <- final_res$stats
    final_res$stats <- list(
      totalFeatures    = length(all_full_rows),
      numDatasets      = if (!is.null(orig_stats$numDatasets)) orig_stats$numDatasets else length(datasets),
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
      meta_md <- paste0(meta_md, sprintf("  - **P-value Combination Method:** %s\n  - **Minimum Study Count ($k \\ge 2$):** Features present in only a single dataset ($k < 2$) were filtered out and excluded prior to combination.\n", toupper(if (is.null(pvalue_method)) "fisher" else pvalue_method)))
    } else if (method_val == "effect_size") {
      meta_md <- paste0(meta_md, sprintf("  - **Effect Size Model:** %s\n  - **Minimum Study Count ($k \\ge 2$):** Features present in only a single dataset ($k < 2$) were filtered out and excluded prior to running `metafor::rma` model fitting.\n", toupper(if (is.null(eff_model)) "random" else eff_model)))
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
    return(final_res)
  }
  return(NULL)
}

