library(jsonlite)

# Helper function to filter GSEA result objects by q-value / adjusted p-value cutoff
filter_gsea_result <- function(gse_obj, qval_cutoff) {
  if (is.null(gse_obj) || is.null(qval_cutoff) || is.na(qval_cutoff)) return(gse_obj)
  if (inherits(gse_obj, "gseaResult") && nrow(gse_obj@result) > 0) {
    q_col <- if ("qvalue" %in% colnames(gse_obj@result) && any(!is.na(gse_obj@result[["qvalue"]]))) {
      "qvalue"
    } else if ("p.adjust" %in% colnames(gse_obj@result)) {
      "p.adjust"
    } else {
      NULL
    }
    if (!is.null(q_col)) {
      keep_idx <- which(!is.na(gse_obj@result[[q_col]]) & as.numeric(gse_obj@result[[q_col]]) <= as.numeric(qval_cutoff))
      gse_obj@result <- gse_obj@result[keep_idx, , drop = FALSE]
    }
  }
  return(gse_obj)
}

# 1. Main Enrichment Analysis Handler
run_enrichment_analysis <- function(method, ora_db, gsea_db, rank_by, pval_cutoff, qval_cutoff, min_size, max_size, organism, genes, gene_scores, gene_id_type = NULL, dataset_id = NULL, direction = NULL) {
  # Determine which organism db to load
  org_pkg <- "org.Hs.eg.db"
  if (any(grepl("musculus|mouse", tolower(organism)))) {
    org_pkg <- "org.Mm.eg.db"
  } else if (any(grepl("rattus|rat", tolower(organism)))) {
    org_pkg <- "org.Rn.eg.db"
  } else if (any(grepl("scrofa|pig", tolower(organism)))) {
    org_pkg <- "org.Ss.eg.db"
  } else if (any(grepl("gallus|chicken", tolower(organism)))) {
    org_pkg <- "org.Gg.eg.db"
  }
  load_packages_globally(c("clusterProfiler", "msigdbr", "ReactomePA", "ggplot2", "enrichplot", "BiocParallel", org_pkg))
  if (requireNamespace("BiocParallel", quietly = TRUE)) {
    eff_c <- get_effective_cores()
    bpp <- get_bioc_parallel_param(eff_c)
    if (!is.null(bpp)) {
      BiocParallel::register(bpp)
    }
  }

  cat(sprintf("[ENRICHMENT] Running %s analysis...\n", toupper(method)))
  print(head(genes, 5))
  species_name <- "Homo sapiens"
  org_db <- org.Hs.eg.db::org.Hs.eg.db
  if (any(grepl("musculus|mouse", tolower(organism)))) {
    species_name <- "Mus musculus"
    org_db <- org.Mm.eg.db::org.Mm.eg.db
  } else if (any(grepl("rattus|rat", tolower(organism)))) {
    species_name <- "Rattus norvegicus"
    org_db <- org.Rn.eg.db::org.Rn.eg.db
  } else if (any(grepl("scrofa|pig", tolower(organism)))) {
    species_name <- "Sus scrofa"
    org_db <- org.Ss.eg.db::org.Ss.eg.db
  } else if (any(grepl("gallus|chicken", tolower(organism)))) {
    species_name <- "Gallus gallus"
    org_db <- org.Gg.eg.db::org.Gg.eg.db
  }

  ora_results <- list()
  gsea_results <- list()
  ora_objects <- list()
  gsea_objects <- list()

  # Surface unsupported organism/database combos instead of silently returning empty:
  # Reactome (ReactomePA) and MSigDB (msigdbr) support far fewer species than GO/OrgDb,
  # so non-core organisms + those DBs typically yield 0 terms for config reasons, not biology.
  is_core_species <- species_name %in% c("Homo sapiens", "Mus musculus", "Rattus norvegicus")
  enrich_warnings <- character(0)
  if (!is_core_species) {
    enrich_warnings <- c(enrich_warnings, sprintf(
      "Organism '%s': Reactome and MSigDB have limited species support and may return no terms — GO enrichment is recommended for this organism.",
      species_name))
  }

  clean_id_type <- if (!is.null(gene_id_type)) tolower(trimws(gene_id_type)) else ""
  sample_genes <- head(genes[!is.na(genes) & genes != ""], 20)
  has_ensembl_pattern <- any(grepl("^ENSG|^ENSMUSG|^ENSRNOG", sample_genes, ignore.case = TRUE))

  if (has_ensembl_pattern) {
    from_type <- "ENSEMBL"
  } else if (clean_id_type %in% c("entrez", "entrezid")) {
    from_type <- "ENTREZID"
  } else if (clean_id_type %in% c("ensembl")) {
    from_type <- "ENSEMBL"
  } else if (clean_id_type %in% c("symbol", "genename", "gene name", "gene_name")) {
    from_type <- "SYMBOL"
  } else {
    from_type <- "SYMBOL"
  }

  if (from_type == "ENSEMBL" && !is.null(genes)) {
    genes <- sub("\\..*$", "", as.character(genes))
  }

  input_genes_clean <- unique(genes[!is.na(genes) & genes != ""])
  input_count <- length(input_genes_clean)
  cat(sprintf("[METRICS] Detected Gene ID Type: %s\n", from_type))
  cat(sprintf("[METRICS] Number of unique Input Gene IDs: %d\n", input_count))

  # Check if we can use existing annotation results instead of running bitr
  anno_res <- NULL
  if (!is.null(dataset_id) && dataset_id != "") {
    resolved_path <- get_session_path(dataset_id, "%s_resolved_mapping.rds")
    if (file.exists(resolved_path)) {
      anno_res <- tryCatch(readRDS(resolved_path), error = function(e) NULL)
    }
    if (is.null(anno_res)) {
      anno_path <- get_session_path(dataset_id, "%s_annotation_results.rds")
      if (file.exists(anno_path)) {
        anno_res <- tryCatch(readRDS(anno_path), error = function(e) NULL)
      }
    }
    if (is.null(anno_res)) {
      uid_tmp <- get_user_id(dataset_id)
      user_dir <- sprintf("tmp/user_sessions/%s", uid_tmp)
      if (dir.exists(user_dir)) {
        sibling_res_maps <- list.files(user_dir, pattern = "_resolved_mapping\\.rds$", full.names = TRUE)
        if (length(sibling_res_maps) > 0) {
          all_maps <- list()
          for (srm in sibling_res_maps) {
            sm <- tryCatch(readRDS(srm), error = function(e) NULL)
            if (!is.null(sm) && is.data.frame(sm) && nrow(sm) > 0) {
              all_maps[[length(all_maps) + 1]] <- sm
            }
          }
          if (length(all_maps) > 0) {
            anno_res <- unique(do.call(rbind, all_maps))
          }
        }
      }
    }
  }

  entrez_map <- NULL
  if (from_type == "ENTREZID") {
    cat("[ENRICHMENT] Gene ID type is ENTREZID. Bypassing mapping and using input genes directly.\n")
    entrez_map <- data.frame(
      ENTREZID = input_genes_clean,
      stringsAsFactors = FALSE
    )
  } else if (!is.null(anno_res)) {
    cat("[ENRICHMENT] Using pre-existing annotation results for symbol-to-entrez mapping.\n")
    sym_col <- if ("gene_symbol" %in% colnames(anno_res)) "gene_symbol" else "SYMBOL"
    ent_col <- if ("entrez_id" %in% colnames(anno_res)) "entrez_id" else "ENTREZID"
    
    if (sym_col %in% colnames(anno_res) && ent_col %in% colnames(anno_res)) {
      valid_map <- anno_res[!is.na(anno_res[[sym_col]]) & anno_res[[sym_col]] != "" &
                            !is.na(anno_res[[ent_col]]) & anno_res[[ent_col]] != "", ]
      unique_pairs <- unique(valid_map[, c(sym_col, ent_col)])
      
      # Strip make.unique suffixes (e.g. Symbol.1 -> Symbol) for matching
      input_genes_stripped <- sub("\\.[0-9]+$", "", input_genes_clean)
      match_idx <- match(input_genes_stripped, unique_pairs[[sym_col]])
      
      entrez_map <- data.frame(
        SYMBOL = input_genes_clean,
        ENTREZID = unique_pairs[[ent_col]][match_idx],
        stringsAsFactors = FALSE
      )
      colnames(entrez_map) <- c(from_type, "ENTREZID")
      entrez_map <- entrez_map[!is.na(entrez_map$ENTREZID) & entrez_map$ENTREZID != "", ]
    }
  }

  if (is.null(entrez_map)) {
    cat("[ENRICHMENT] Running clusterProfiler::bitr for ID mapping...\n")
    entrez_map <- tryCatch({
      clusterProfiler::bitr(input_genes_clean, fromType = from_type, toType = "ENTREZID", OrgDb = org_db)
    }, error = function(e) {
      cat(sprintf("[ERROR] bitr mapping process failed entirely: %s\n", e$message))
      NULL
    })
  }

  mapped_input_genes <- if (!is.null(entrez_map) && nrow(entrez_map) > 0) unique(as.character(entrez_map[[from_type]])) else character(0)
  unmapped_genes <- setdiff(input_genes_clean, mapped_input_genes)
  converted_count <- length(mapped_input_genes)
  cat(sprintf("[METRICS] Number of input Gene IDs converted to Entrez ID: %d\n", converted_count))

  if (converted_count == 0) {
    cat(sprintf("[ERROR] Downstream enrichment aborted. Zero identifiers out of %d inputs matching type %s could be mapped to Entrez IDs in %s database environment.\n", input_count, from_type, species_name))
    return(list(
      oraResults = list(),
      gseaResults = list(),
      oraObjects = list(),
      gseObjects = list(),
      unmappedGenes = as.list(unmapped_genes),
      totalInputCount = input_count,
      mappedCount = 0
    ))
  }

  if (method == "ora") {
    gene_ids <- unique(entrez_map$ENTREZID)

    for (db in ora_db) {
      cat(sprintf("  Querying ORA database: %s...\n", db))
      res_df <- NULL
      db_upper <- toupper(db)

      if (grepl("GO", db_upper)) {
        ont_type <- "BP"
        if (grepl("MF", db_upper)) ont_type <- "MF"
        if (grepl("CC", db_upper)) ont_type <- "CC"
        
        ego_obj <- NULL
        res_df <- tryCatch({
          ego <- clusterProfiler::enrichGO(
            gene = gene_ids,
            OrgDb = org_db,
            ont = ont_type,
            pAdjustMethod = "BH",
            pvalueCutoff = pval_cutoff,
            qvalueCutoff = qval_cutoff,
            minGSSize = min_size,
            maxGSSize = max_size,
            readable = TRUE
          )
          if (!is.null(ego)) {
            ego_obj <- ego
            as.data.frame(ego)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] enrichGO failed for", db, ":", e$message, "\n")
          NULL
        })
        if (!is.null(ego_obj)) {
          ora_objects[[db]] <- ego_obj
        }
      } else if (db_upper == "KEGG") {
        ekeg_obj <- NULL
        res_df <- tryCatch({
          kegg_organism <- "hsa"
          if (species_name == "Mus musculus") kegg_organism <- "mmu"
          if (species_name == "Rattus norvegicus") kegg_organism <- "rno"
          if (species_name == "Sus scrofa") kegg_organism <- "ssc"
          if (species_name == "Gallus gallus") kegg_organism <- "gga"

          ekeg <- clusterProfiler::enrichKEGG(
            gene = as.character(unique(gene_ids)),
            organism = kegg_organism,
            keyType = "kegg",
            pAdjustMethod = "BH",
            pvalueCutoff = pval_cutoff,
            qvalueCutoff = qval_cutoff,
            minGSSize = min_size,
            maxGSSize = max_size
          )
          if (!is.null(ekeg)) {
            ekeg_obj <- ekeg
            as.data.frame(ekeg)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] enrichKEGG failed:", e$message, "\n")
          NULL
        })
        if (!is.null(ekeg_obj)) {
          ora_objects[[db]] <- ekeg_obj
        }
      } else if (db_upper == "REACTOME") {
        ereact_obj <- NULL
        res_df <- tryCatch({
          reactome_organism <- "human"
          if (species_name == "Mus musculus") reactome_organism <- "mouse"
          if (species_name == "Rattus norvegicus") reactome_organism <- "rat"
          if (species_name == "Sus scrofa") reactome_organism <- "pig"
          if (species_name == "Gallus gallus") reactome_organism <- "chicken"

          ereact <- ReactomePA::enrichPathway(
            gene = gene_ids,
            organism = reactome_organism,
            pAdjustMethod = "BH",
            pvalueCutoff = pval_cutoff,
            qvalueCutoff = qval_cutoff,
            minGSSize = min_size,
            maxGSSize = max_size,
            readable = TRUE
          )
          if (!is.null(ereact)) {
            ereact_obj <- ereact
            as.data.frame(ereact)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] enrichPathway failed:", e$message, "\n")
          NULL
        })
        if (!is.null(ereact_obj)) {
          ora_objects[[db]] <- ereact_obj
        }
      }

      if (!is.null(res_df) && nrow(res_df) > 0) {
        for (i in 1:nrow(res_df)) {
          gr_parts <- strsplit(as.character(res_df$GeneRatio[i]), "/")[[1]]
          hits_cnt <- if ("Count" %in% colnames(res_df) && !is.na(res_df$Count[i])) as.integer(res_df$Count[i]) else if (length(gr_parts) >= 1) as.integer(gr_parts[1]) else 0L
          tot_gene <- if (length(gr_parts) > 1) as.integer(gr_parts[2]) else as.integer(converted_count)
          padj_val <- if ("p.adjust" %in% colnames(res_df)) as.numeric(res_df$p.adjust[i]) else as.numeric(res_df$qvalue[i])

          item <- list(
            Description = as.character(res_df$Description[i]),
            Hits_count = hits_cnt,
            Total_input_gene = tot_gene,
            GeneRatio = as.character(res_df$GeneRatio[i]),
            Pvalue = as.numeric(res_df$pvalue[i]),
            P.adjust = padj_val,
            FeaturesID = as.character(res_df$geneID[i]),
            database = db
          )
          if (!is.null(direction) && nzchar(as.character(direction))) {
            item$direction <- if (grepl("up", direction, ignore.case = TRUE)) "up_genes" else if (grepl("down", direction, ignore.case = TRUE)) "down_genes" else direction
          }
          ora_results <- c(ora_results, list(item))
        }
      }
    }
    cat(sprintf("[ENRICHMENT] ORA analysis completed. Found %d significant results.\n", length(ora_results)))
  }

  if (method == "gsea") {
    if (length(gene_scores) == 0) {
      cat("[WARNING] No gene scores provided for GSEA. GSEA aborted.\n")
      return(list(oraResults = list(), gseaResults = list(), oraObjects = list(), gseObjects = list()))
    }

    score_genes <- names(gene_scores)
    score_from_type <- "SYMBOL"
    
    if (any(grepl("^ENSG|^ENSMUSG|^ENSRNOG", score_genes, ignore.case = TRUE))) {
      score_from_type <- "ENSEMBL"
      score_genes <- sub("\\..*$", "", score_genes)
      names(gene_scores) <- score_genes
    } else if (clean_id_type %in% c("entrez", "entrezid")) {
      score_from_type <- "ENTREZID"
    } else if (clean_id_type %in% c("symbol", "genename", "gene name", "gene_name")) {
      score_from_type <- "SYMBOL"
    }

    score_entrez_map <- NULL
    if (score_from_type == "ENTREZID") {
      cat("[ENRICHMENT] GSEA gene ID type is ENTREZID. Bypassing GSEA mapping and using scores directly.\n")
      score_entrez_map <- data.frame(
        ENTREZID = score_genes,
        stringsAsFactors = FALSE
      )
    } else if (!is.null(anno_res)) {
      cat("[ENRICHMENT] Using pre-existing annotation results for GSEA symbol-to-entrez mapping.\n")
      sym_col <- if ("gene_symbol" %in% colnames(anno_res)) "gene_symbol" else "SYMBOL"
      ent_col <- if ("entrez_id" %in% colnames(anno_res)) "entrez_id" else "ENTREZID"
      
      if (sym_col %in% colnames(anno_res) && ent_col %in% colnames(anno_res)) {
        valid_map <- anno_res[!is.na(anno_res[[sym_col]]) & anno_res[[sym_col]] != "" &
                              !is.na(anno_res[[ent_col]]) & anno_res[[ent_col]] != "", ]
        unique_pairs <- unique(valid_map[, c(sym_col, ent_col)])
        
        # Strip make.unique suffixes (e.g. Symbol.1 -> Symbol) for matching
        score_genes_stripped <- sub("\\.[0-9]+$", "", score_genes)
        match_idx <- match(score_genes_stripped, unique_pairs[[sym_col]])
        
        score_entrez_map <- data.frame(
          SYMBOL = score_genes,
          ENTREZID = unique_pairs[[ent_col]][match_idx],
          stringsAsFactors = FALSE
        )
        colnames(score_entrez_map) <- c(score_from_type, "ENTREZID")
        score_entrez_map <- score_entrez_map[!is.na(score_entrez_map$ENTREZID) & score_entrez_map$ENTREZID != "", ]
      }
    }

    if (is.null(score_entrez_map)) {
      cat("[ENRICHMENT] Running clusterProfiler::bitr for GSEA ID mapping...\n")
      score_entrez_map <- tryCatch({
        clusterProfiler::bitr(score_genes, fromType = score_from_type, toType = "ENTREZID", OrgDb = org_db)
      }, error = function(e) {
        NULL
      })
    }

    if (is.null(score_entrez_map) || nrow(score_entrez_map) == 0) {
      cat("[WARNING] Could not map gene scores to ENTREZID. GSEA aborted.\n")
      return(list(oraResults = list(), gseaResults = list(), oraObjects = list(), gseObjects = list()))
    }

    mapped_scores <- unlist(gene_scores[score_entrez_map[[score_from_type]]])
    names(mapped_scores) <- as.character(score_entrez_map$ENTREZID)
    gene_list <- sort(mapped_scores, decreasing = TRUE)
    gene_list <- gene_list[!duplicated(names(gene_list))]

    for (db in gsea_db) {
      cat(sprintf("  Running GSEA against database: %s...\n", db))
      res_df <- NULL
      db_upper <- toupper(db)

      set.seed(42)
      if (grepl("GO", db_upper)) {
        ont_type <- "BP"
        if (grepl("MF", db_upper)) ont_type <- "MF"
        if (grepl("CC", db_upper)) ont_type <- "CC"
        
        gse_go_obj <- NULL
        res_df <- tryCatch({
          gse_go <- clusterProfiler::gseGO(
            geneList = gene_list,
            OrgDb = org_db,
            ont = ont_type,
            pvalueCutoff = pval_cutoff,
            pAdjustMethod = "BH",
            minGSSize = min_size,
            maxGSSize = max_size
          )
          if (!is.null(gse_go)) {
            gse_go <- filter_gsea_result(gse_go, qval_cutoff)
            gse_go_obj <- gse_go
            as.data.frame(gse_go)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] gseGO failed for", db, ":", e$message, "\n")
          NULL
        })
        if (!is.null(gse_go_obj)) {
          gsea_objects[[db]] <- gse_go_obj
        }
      } else if (db_upper == "KEGG") {
        gse_kegg_obj <- NULL
        res_df <- tryCatch({
          kegg_organism <- "hsa"
          if (species_name == "Mus musculus") kegg_organism <- "mmu"
          if (species_name == "Rattus norvegicus") kegg_organism <- "rno"
          if (species_name == "Sus scrofa") kegg_organism <- "ssc"
          if (species_name == "Gallus gallus") kegg_organism <- "gga"

          gse_kegg <- clusterProfiler::gseKEGG(
            geneList = gene_list,
            organism = kegg_organism,
            keyType = "kegg",
            pvalueCutoff = pval_cutoff,
            pAdjustMethod = "BH",
            minGSSize = min_size,
            maxGSSize = max_size
          )
          if (!is.null(gse_kegg)) {
            gse_kegg <- filter_gsea_result(gse_kegg, qval_cutoff)
            gse_kegg_obj <- gse_kegg
            as.data.frame(gse_kegg)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] gseKEGG failed:", e$message, "\n")
          NULL
        })
        if (!is.null(gse_kegg_obj)) {
          gsea_objects[[db]] <- gse_kegg_obj
        }
      } else if (db_upper %in% c("MSIGDB_H", "MSIGDB_C2", "MSIGDB_C5", "REACTOME")) {
        category_val <- "H"
        subcategory_val <- NULL
        
        if (db_upper == "MSIGDB_C2") category_val <- "C2"
        if (db_upper == "MSIGDB_C5") category_val <- "C5"
        if (db_upper == "REACTOME") { category_val <- "C2"; subcategory_val <- "REACTOME" }

        gse_er_obj <- NULL
        res_df <- tryCatch({
          m_all <- msigdbr::msigdbr(species = species_name, category = category_val)
          
          if (!is.null(subcategory_val)) {
            subcat_col <- if ("gs_subcat" %in% colnames(m_all)) "gs_subcat" else "gs_sub_cat"
            m_all <- m_all[toupper(gsub("^CP:", "", m_all[[subcat_col]])) == toupper(subcategory_val), ]
          }
          
          m_t2g <- data.frame(
            gs_name = m_all$gs_name,
            entrez_gene = as.character(m_all$entrez_gene),
            stringsAsFactors = FALSE
          )

          if (nrow(m_t2g) == 0) {
            stop(sprintf("No gene sets found in msigdbr for category '%s' / subcategory '%s'", category_val, subcategory_val))
          }

          gse_er <- clusterProfiler::GSEA(
            geneList = gene_list,
            TERM2GENE = m_t2g,
            pvalueCutoff = pval_cutoff,
            pAdjustMethod = "BH",
            minGSSize = min_size,
            maxGSSize = max_size
          )
          if (!is.null(gse_er)) {
            gse_er <- filter_gsea_result(gse_er, qval_cutoff)
            gse_er_obj <- gse_er
            as.data.frame(gse_er)
          } else {
            NULL
          }
        }, error = function(e) {
          cat("[ERROR] GSEA failed for", db, ":", e$message, "\n")
          NULL
        })
        if (!is.null(gse_er_obj)) {
          gsea_objects[[db]] <- gse_er_obj
        }
      }

      if (!is.null(res_df) && nrow(res_df) > 0) {
        for (i in 1:nrow(res_df)) {
          ce <- if ("core_enrichment" %in% colnames(res_df) && !is.na(res_df$core_enrichment[i])) as.character(res_df$core_enrichment[i]) else ""
          hits_cnt <- if (nzchar(ce)) length(strsplit(ce, "/")[[1]]) else 0L
          es_val <- if ("enrichmentScore" %in% colnames(res_df)) as.numeric(res_df$enrichmentScore[i]) else NA_real_
          padj_val <- if ("p.adjust" %in% colnames(res_df)) as.numeric(res_df$p.adjust[i]) else if ("qvalue" %in% colnames(res_df)) as.numeric(res_df$qvalue[i]) else as.numeric(res_df$pvalue[i])

          item <- list(
            Description = as.character(res_df$Description[i]),
            setSize = as.integer(res_df$setSize[i]),
            Hits = hits_cnt,
            enrichmentScore = es_val,
            NES = as.numeric(res_df$NES[i]),
            pvalue = as.numeric(res_df$pvalue[i]),
            p.adjust = padj_val,
            core_enrichment = ce,
            database = db
          )
          gsea_results <- c(gsea_results, list(item))
        }
      }
    }
    cat(sprintf("[ENRICHMENT] GSEA analysis completed. Found %d significant results.\n", length(gsea_results)))
  }

  # Sort full returned results by significance (P.adjust, then Pvalue for ORA; p.adjust, then pvalue for GSEA)
  top_ora <- ora_results
  if (length(top_ora) > 0) {
    ord <- order(
      sapply(top_ora, function(x) if (!is.null(x$P.adjust)) x$P.adjust else 1),
      sapply(top_ora, function(x) if (!is.null(x$Pvalue)) x$Pvalue else 1)
    )
    top_ora <- top_ora[ord]
  }
  top_gsea <- gsea_results
  if (length(top_gsea) > 0) {
    ord <- order(
      sapply(top_gsea, function(x) if (!is.null(x[["p.adjust"]])) x[["p.adjust"]] else 1),
      sapply(top_gsea, function(x) if (!is.null(x$pvalue)) x$pvalue else 1)
    )
    top_gsea <- top_gsea[ord]
  }

  # If nothing came back but genes DID map, hint that it may be a DB/organism config issue.
  if (length(top_ora) == 0 && length(top_gsea) == 0 && converted_count > 0) {
    enrich_warnings <- c(enrich_warnings,
      "No enriched terms passed the significance cutoffs. Check the organism/database selection (e.g. try GO or a looser p/q cutoff).")
  }

  return(list(
    oraResults = top_ora,
    gseaResults = top_gsea,
    oraObjects = ora_objects,
    gseObjects = gsea_objects,
    unmappedGenes = as.list(unmapped_genes),
    totalInputCount = input_count,
    mappedCount = converted_count,
    warnings = as.list(enrich_warnings)
  ))
}

run_ea_compute <- function(config) {
  methods_val <- if (!is.null(config$methods)) config$methods else list("ora")
  dataset_id  <- if (!is.null(config$datasetId)) config$datasetId else ""
  ora_db      <- config$oraDatabase
  gsea_db     <- config$gseaDatabase
  rank_by     <- config$rankByCol
  pval_cutoff <- config$pValueCutoff
  qval_cutoff <- config$qValueCutoff
  min_size    <- config$minGeneSetSize
  max_size    <- config$maxGeneSetSize
  organism    <- config$organism
  direction   <- if (!is.null(config$direction)) config$direction else "all"
  gene_id_col <- if (!is.null(config$geneIdCol)) config$geneIdCol else ""
  gene_id_type <- if (!is.null(config$geneIdType)) config$geneIdType else ""

  genes       <- NULL
  gene_scores <- NULL

  if (!is.null(dataset_id) && dataset_id != "") {
    parsed_id <- get_backend_datasets(dataset_id)
    base_dataset_id <- parsed_id$base_id
    
    meta_rds_candidates <- c(
      get_session_path(dataset_id, "%s_upload_expr_metadata.rds"),
      get_session_path(dataset_id, "%s_expr_metadata.rds")
    )
    expr_meta <- NULL
    for (mp in meta_rds_candidates) {
      if (file.exists(mp)) {
        expr_meta <- tryCatch(readRDS(mp), error = function(e) NULL)
        if (!is.null(expr_meta)) break
      }
    }
    if (!is.null(expr_meta)) {
      if (!is.null(expr_meta$geneIdType) && expr_meta$geneIdType != "") {
        gene_id_type <- expr_meta$geneIdType
      }
      if ((is.null(gene_id_col) || gene_id_col == "") && !is.null(expr_meta$geneIdCol)) {
        gene_id_col <- expr_meta$geneIdCol
      }
    }
    
    expr_csv_candidates <- c(
      get_session_path(dataset_id, "%s_upload_expression.csv"),
      get_session_path(dataset_id, "%s_expression.csv")
    )
    df_ea <- NULL
    for (cp in expr_csv_candidates) {
      if (file.exists(cp)) {
        df_ea <- tryCatch(read.csv(cp, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
        if (!is.null(df_ea) && nrow(df_ea) > 0) break
      }
    }
    
    if (!is.null(df_ea) && nrow(df_ea) > 0) {
      expr_cols <- colnames(df_ea)
      
      if (is.null(gene_id_col) || gene_id_col == "" || !(gene_id_col %in% expr_cols)) {
        gene_id_col <- expr_cols[1]
      }
      
      # Treat gene ID column as character
      df_ea[[gene_id_col]] <- as.character(df_ea[[gene_id_col]])
      
      # Remove NA / blank genes in the gene ID column
      df_ea <- df_ea[!is.na(df_ea[[gene_id_col]]) & df_ea[[gene_id_col]] != "", , drop = FALSE]
      
      if ("ora" %in% methods_val) {
        # Deduplicate gene ID
        df_ea_ora <- df_ea[!duplicated(df_ea[[gene_id_col]]), , drop = FALSE]
        genes <- df_ea_ora[[gene_id_col]]
      }
      
      if ("gsea" %in% methods_val) {
        # Check if requested rank_by is a valid numeric column
        is_numeric_rank_col <- FALSE
        if (!is.null(rank_by) && rank_by != "" && rank_by %in% expr_cols) {
          test_num <- suppressWarnings(as.numeric(df_ea[[rank_by]]))
          if (sum(!is.na(test_num)) >= (0.5 * nrow(df_ea))) {
            is_numeric_rank_col <- TRUE
          }
        }
        
        if (!is_numeric_rank_col) {
          # Prioritize ranking columns if present
          preferred_pats <- c("log2foldchange", "logfc", "averagefc", "combinedes", "es", "score", "stat", "t", "pvalue")
          found_pref <- FALSE
          for (pat in preferred_pats) {
            match_col <- expr_cols[grepl(paste0("^", pat, "$"), tolower(expr_cols))]
            if (length(match_col) > 0) {
              test_num <- suppressWarnings(as.numeric(df_ea[[match_col[1]]]))
              if (sum(!is.na(test_num)) >= (0.5 * nrow(df_ea))) {
                rank_by <- match_col[1]
                is_numeric_rank_col <- TRUE
                found_pref <- TRUE
                break
              }
            }
          }
          
          if (!found_pref) {
            # Find first valid numeric column
            candidate_cols <- setdiff(expr_cols, c(gene_id_col, if (!is.null(expr_meta$geneInfoCols)) unlist(expr_meta$geneInfoCols) else character(0)))
            for (col in candidate_cols) {
              test_num <- suppressWarnings(as.numeric(df_ea[[col]]))
              if (sum(!is.na(test_num)) >= (0.5 * nrow(df_ea))) {
                rank_by <- col
                is_numeric_rank_col <- TRUE
                break
              }
            }
          }
        }
        
        if (is_numeric_rank_col && !is.null(rank_by) && rank_by %in% expr_cols) {
          df_ea$EA_Rank_Score <- suppressWarnings(as.numeric(df_ea[[rank_by]]))
          df_ea <- df_ea[!is.na(df_ea$EA_Rank_Score), , drop = FALSE]
          
          # Order by the GSEA ranking score
          df_ea <- df_ea[order(df_ea$EA_Rank_Score, decreasing = TRUE), , drop = FALSE]
          
          # Deduplicate gene ID keeping highest rank
          df_ea <- df_ea[!duplicated(df_ea[[gene_id_col]]), , drop = FALSE]
          
          scores <- df_ea$EA_Rank_Score
          names(scores) <- df_ea[[gene_id_col]]
          sorted_scores <- sort(scores, decreasing = TRUE)
          gene_scores <- as.list(sorted_scores)
          if (is.null(genes) || length(genes) == 0) genes <- names(sorted_scores)
        }
      }
    }
  }

  combined_res <- list(oraResults = list(), gseaResults = list(), oraObjects = list(), gseObjects = list())
  for (m in methods_val) {
    res <- run_enrichment_analysis(m, ora_db, gsea_db, rank_by, pval_cutoff, qval_cutoff, min_size, max_size, organism, genes, gene_scores, gene_id_type, dataset_id = dataset_id)
    combined_res$oraResults  <- c(combined_res$oraResults,  res$oraResults)
    combined_res$gseaResults <- c(combined_res$gseaResults, res$gseaResults)
    if (!is.null(res$oraObjects)) {
      combined_res$oraObjects <- c(combined_res$oraObjects, res$oraObjects)
    }
    if (!is.null(res$gseObjects)) {
      combined_res$gseObjects <- c(combined_res$gseObjects, res$gseObjects)
    }
  }
  
  uid_val <- get_user_id(dataset_id)
  ora_headers <- c("Description", "Hits_count", "Total_input_gene", "GeneRatio", "Pvalue", "P.adjust", "FeaturesID")
  gsea_headers <- c("Description", "setSize", "Hits", "enrichmentScore", "NES", "pvalue", "p.adjust", "core_enrichment")

  if (!is.null(ora_db)) {
    for (db in ora_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      db_results <- Filter(function(x) identical(x$database, db) || identical(x$database, db_clean), combined_res$oraResults)
      ora_res_path <- session_file_path(dataset_id, sprintf("%s_ora_results_%s.csv", dataset_id, db_clean))
      save_list_to_csv(db_results, ora_res_path, headers = ora_headers)
      register_export_file(uid_val, "ora_results", dataset_id, ora_res_path, "ea", "ora", db = db_clean)
    }
  }
  if (!is.null(gsea_db)) {
    for (db in gsea_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      db_results <- Filter(function(x) identical(x$database, db) || identical(x$database, db_clean), combined_res$gseaResults)
      gsea_res_path <- session_file_path(dataset_id, sprintf("%s_gsea_results_%s.csv", dataset_id, db_clean))
      save_list_to_csv(db_results, gsea_res_path, headers = gsea_headers)
      register_export_file(uid_val, "gsea_results", dataset_id, gsea_res_path, "ea", "gsea", db = db_clean)
    }
  }

  if ("ora" %in% methods_val && !is.null(combined_res$oraObjects)) {
    for (db in names(combined_res$oraObjects)) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      ora_obj <- combined_res$oraObjects[[db]]
      if (!is.null(ora_obj) && nrow(as.data.frame(ora_obj)) > 0) {
        dot_path <- session_file_path(dataset_id, sprintf("%s_ora_dotplot_%s.pdf", dataset_id, db_clean))
        tryCatch({
          library(enrichplot)
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(ora_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "ora_dotplot", dataset_id, dot_path, "ea", "ora", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
      }
    }
  }

  if ("gsea" %in% methods_val && !is.null(combined_res$gseObjects)) {
    for (db in gsea_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      gse_obj <- combined_res$gseObjects[[db]]
      if (!is.null(gse_obj) && nrow(as.data.frame(gse_obj)) > 0) {
        dot_path   <- session_file_path(dataset_id, sprintf("%s_gsea_dotplot_%s.pdf", dataset_id, db_clean))
        ridge_path <- session_file_path(dataset_id, sprintf("%s_gsea_ridgeplot_%s.pdf", dataset_id, db_clean))
        es_up_path <- session_file_path(dataset_id, sprintf("%s_gsea_esplot_up_%s.pdf", dataset_id, db_clean))
        es_dn_path <- session_file_path(dataset_id, sprintf("%s_gsea_esplot_down_%s.pdf", dataset_id, db_clean))
        
        # Save GSEA Ranked Gene List
        if (exists("sorted_scores") && !is.null(sorted_scores) && length(sorted_scores) > 0) {
          df_ranked <- data.frame(
            Gene = names(sorted_scores),
            RankMetric = unname(sorted_scores),
            stringsAsFactors = FALSE
          )
          df_ranked_list <- lapply(1:nrow(df_ranked), function(i) as.list(df_ranked[i, ]))
          ranked_path <- session_file_path(dataset_id, sprintf("%s_gsea_ranked_list_%s.csv", dataset_id, db_clean))
          save_list_to_csv(df_ranked_list, ranked_path)
          register_export_file(uid_val, "gsea_ranked_list", dataset_id, ranked_path, "ea", "gsea", db = db_clean)
        }
        
        gse_df <- as.data.frame(gse_obj)
        if (!is.null(gse_df) && nrow(gse_df) > 0) {
          cols_to_keep <- intersect(c("ID", "Description", "NES", "pvalue", "p.adjust", "qvalue", "core_enrichment"), colnames(gse_df))
          df_le <- gse_df[, cols_to_keep, drop = FALSE]
          df_le_list <- lapply(1:nrow(df_le), function(i) as.list(df_le[i, ]))
          le_path <- session_file_path(dataset_id, sprintf("%s_gsea_leading_edge_%s.csv", dataset_id, db_clean))
          save_list_to_csv(df_le_list, le_path)
          register_export_file(uid_val, "gsea_leading_edge", dataset_id, le_path, "ea", "gsea", db = db_clean)
        }
        
        library(enrichplot)
        tryCatch({
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(gse_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "gsea_dotplot", dataset_id, dot_path, "ea", "gsea", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
        
        tryCatch({
          pdf(ridge_path, width = 10, height = 7)
          print(enrichplot::ridgeplot(gse_obj, showCategory = 10, label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.2))
          dev.off()
          register_export_file(uid_val, "gsea_ridgeplot", dataset_id, ridge_path, "ea", "gsea", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
        
        gse_df <- as.data.frame(gse_obj)
        if (nrow(gse_df) > 0) {
          up_rows <- gse_df[gse_df$NES > 0, ]
          if (nrow(up_rows) > 0) {
            top_up_id <- up_rows$ID[order(up_rows$NES, decreasing = TRUE)[1]]
            tryCatch({
              pdf(es_up_path, width = 9, height = 6)
              print(enrichplot::gseaplot2(gse_obj, geneSetID = top_up_id, title = gse_df$Description[gse_df$ID == top_up_id]))
              dev.off()
              register_export_file(uid_val, "gsea_esplot_up", dataset_id, es_up_path, "ea", "gsea", db = db_clean, ext = "pdf")
            }, error = function(e) {
              if (dev.cur() > 1) dev.off()
            })
          }
          
          dn_rows <- gse_df[gse_df$NES < 0, ]
          if (nrow(dn_rows) > 0) {
            top_dn_id <- dn_rows$ID[order(dn_rows$NES, decreasing = FALSE)[1]]
            tryCatch({
              pdf(es_dn_path, width = 9, height = 6)
              print(enrichplot::gseaplot2(gse_obj, geneSetID = top_dn_id, title = gse_df$Description[gse_df$ID == top_dn_id]))
              dev.off()
              register_export_file(uid_val, "gsea_esplot_down", dataset_id, es_dn_path, "ea", "gsea", db = db_clean, ext = "pdf")
            }, error = function(e) {
              if (dev.cur() > 1) dev.off()
            })
          }
        }
      }
    }
  }

  return(list(
    oraResults = combined_res$oraResults,
    gseaResults = combined_res$gseaResults,
    mappedCount = if (!is.null(genes)) length(genes) else 0,
    totalInputCount = if (!is.null(genes)) length(genes) else 0
  ))
}

run_inline_ea_compute <- function(payload) {
  config <- payload[[1]]
  dataset_id <- if (!is.null(config$datasetId)) config$datasetId else ""
  methods_val <- if (!is.null(config$methods)) config$methods else list("ora")
  ora_db      <- config$oraDatabase
  gsea_db     <- config$gseaDatabase
  rank_by     <- config$rankByCol
  pval_cutoff <- config$pValueCutoff
  qval_cutoff <- config$qValueCutoff
  min_size    <- config$minGeneSetSize
  max_size    <- config$maxGeneSetSize
  organism    <- config$organism
  source      <- config$source
  gene_id_col <- config$geneIdCol
  gene_id_type <- config$geneIdType
  direction   <- if (!is.null(config$direction)) config$direction else "all"

  base_id <- get_base_id(dataset_id)
  
  # Retrieve metadata to find original geneIdType/geneIdCol if not specified or empty
  meta_info <- NULL
  if (base_id != "") {
    meta_paths <- c(
      get_session_path(base_id, "%s_expr_metadata.rds"),
      sprintf("tmp/%s_upload_expr_metadata.rds", base_id),
      sprintf("tmp/%s_expr_metadata.rds", base_id)
    )
    for (mp in meta_paths) {
      if (file.exists(mp)) {
        meta_info <- tryCatch(readRDS(mp), error = function(e) NULL)
        if (!is.null(meta_info)) break
      }
    }
  }
  
  if (!is.null(meta_info)) {
    if ((is.null(gene_id_type) || gene_id_type == "") && !is.null(meta_info$geneIdType)) {
      gene_id_type <- meta_info$geneIdType
    }
    if ((is.null(gene_id_col) || gene_id_col == "") && !is.null(meta_info$geneIdCol)) {
      gene_id_col <- meta_info$geneIdCol
    }
  }

  # Parse current module, parent module, and user ID
  parsed_ds <- get_backend_datasets(dataset_id)
  parent_mod <- config$parentModule %||% parsed_ds$parentModule
  if (is.null(parent_mod) || !nzchar(parent_mod) || parent_mod == "ea") {
    if (grepl("_dp", dataset_id) || grepl("_dp", base_id) || identical(source, "inline_de") || identical(source, "inline_de_meta") || identical(config$module, "dp") || identical(config$parentModule, "dp")) {
      parent_mod <- "dp"
    } else if (grepl("_de", dataset_id) || grepl("_de", base_id) || identical(source, "de") || identical(source, "meta") || identical(source, "de_meta") || identical(config$parentModule, "de")) {
      parent_mod <- "de"
    } else if (is.list(config$datasets) && length(config$datasets) > 0) {
      if (any(sapply(config$datasets, function(d) grepl("_dp", d$id %||% "")))) {
        parent_mod <- "dp"
      } else if (any(sapply(config$datasets, function(d) grepl("_de", d$id %||% "")))) {
        parent_mod <- "de"
      } else {
        parent_mod <- "ea"
      }
    } else {
      parent_mod <- "ea"
    }
  }
  module_val <- config$module %||% parsed_ds$module %||% "ea"
  
  user_prefix <- if (!is.null(parsed_ds$user_id) && parsed_ds$user_id != "") parsed_ds$user_id else (if (exists("get_user_id", mode = "function")) get_user_id(dataset_id) else "user")
  if (is.null(user_prefix) || !nzchar(user_prefix)) user_prefix <- "user"
  u_id <- user_prefix

  # Determine data type for dataset_id
  data_type <- if (!is.null(config$dataType) && nzchar(as.character(config$dataType))) {
    as.character(config$dataType)
  } else if (!is.null(config$dataClass) && nzchar(as.character(config$dataClass))) {
    as.character(config$dataClass)
  } else if (!is.null(meta_info$dataType)) {
    meta_info$dataType
  } else {
    "readcounts"
  }

  # If the current dataset is "others" type, enrichment analysis is not supported
  if (data_type == "others" || identical(config$dataClass, "others")) {
    cat("[INLINE-EA] Dataset is of 'others' type — enrichment analysis is not performed for this type.\n")
    return(list(
      oraResults = list(), gseaResults = list(),
      unmappedGenes = list(), totalInputGenes = 0, mappedGenesCount = 0,
      detail = "Enrichment analysis is not applicable for 'others' data type."
    ))
  }

  target_is_proteomics <- (data_type == "proteomics" || identical(config$dataClass, "proteomics"))
  dclass <- if (target_is_proteomics) "proteomics" else "transcriptomics"

  # Determine whether this hierarchy has multiple datasets in the active parent module
  num_ds_in_class <- 1
  if (!is.null(config$datasets) && is.list(config$datasets) && length(config$datasets) > 0) {
    # Count datasets matching this hierarchy from frontend-provided dataset list
    num_ds_in_class <- sum(sapply(config$datasets, function(d) {
      dt <- if (!is.null(d$dataType)) d$dataType else "readcounts"
      if (target_is_proteomics) (dt == "proteomics") else (dt %in% c("readcounts", "microarray"))
    }))
  } else {
    # Fallback: check metadata files strictly belonging to this user and parent module
    user_sess_dir <- sprintf("tmp/user_sessions/%s", u_id)
    all_meta_files <- c()
    if (dir.exists(user_sess_dir)) {
      all_meta_files <- list.files(user_sess_dir, pattern = sprintf("^%s_.*_%s_expr_metadata\\.rds$", u_id, parent_mod), full.names = TRUE)
      if (length(all_meta_files) == 0 && parent_mod == "dp") {
        all_meta_files <- list.files(user_sess_dir, pattern = sprintf("^%s_.*_expr_metadata\\.rds$", u_id), full.names = TRUE)
        all_meta_files <- all_meta_files[!grepl("_upload_", all_meta_files) & !grepl("_(de|fs|ea|meta)_", all_meta_files)]
      }
    }
    if (length(all_meta_files) > 0) {
      class_count <- 0
      for (mf in all_meta_files) {
        minf <- tryCatch(readRDS(mf), error = function(e) NULL)
        if (!is.null(minf)) {
          mdt <- if (!is.null(minf$dataType)) minf$dataType else "readcounts"
          is_p <- (mdt == "proteomics")
          if (is_p == target_is_proteomics) class_count <- class_count + 1
        }
      }
      if (class_count > 0) num_ds_in_class <- class_count
    }
  }

  is_meta_source_req <- identical(source, "meta") ||
                        identical(source, "inline_de_meta") ||
                        identical(source, "inline_meta") ||
                        identical(source, "de_meta") ||
                        grepl("meta", tolower(as.character(source %||% ""))) ||
                        grepl("_meta$", dataset_id)

  # Check if multiple datasets exist for this hierarchy but meta-analysis was not requested/performed
  if (num_ds_in_class > 1 && !is_meta_source_req) {
    stop("[ERROR] Multiple datasets of the same type detected. Meta-analysis results are required before running enrichment analysis. Please run meta-analysis first.")
  }

  # 1. Determine whether to load from meta-analysis results
  use_meta_source <- FALSE
  meta_df <- NULL

  if (is_meta_source_req || num_ds_in_class > 1) {
    if (parent_mod == "dp") {
      # In DP/inline flow, try inline_de_main_stack first
      latest_main <- get_latest_inline_de_main_stack(dataset_id)
      if (!is.null(latest_main) && isTRUE(latest_main$step == "meta")) {
        cat("[INLINE-EA] Sourcing genes from top of inline DE main stack (meta-analysis results).\n")
        meta_df <- latest_main$data
        use_meta_source <- TRUE
      }
    } else {
      # In DE/standalone flow, try de_stack step 'meta' first
      latest_de <- get_latest_de_stack(dataset_id, "meta")
      if (!is.null(latest_de)) {
        cat("[INLINE-EA] Sourcing genes from DE stack step 'meta'.\n")
        meta_df <- latest_de$data
        use_meta_source <- TRUE
      }
    }
    
    # If not found in stacks, check disk files STRICTLY within current parent module (zero cross-module contamination)
    if (!use_meta_source) {
      meta_file_candidates <- if (parent_mod == "dp") {
        c(
          sprintf("tmp/user_sessions/%s/%s_dp_%s_meta_analyzed_results.csv", u_id, u_id, dclass),
          get_session_path(sprintf("%s_dp_meta", u_id), sprintf("%%s_%s_meta_analyzed_results.csv", dclass)),
          get_session_path(sprintf("%s_dp", u_id), sprintf("%%s_%s_meta_analyzed_results.csv", dclass)),
          sprintf("tmp/user_sessions/%s/%s_dp_meta_analyzed_results.csv", u_id, u_id),
          get_session_path(sprintf("%s_dp_meta", u_id), "%s_meta_analyzed_results.csv"),
          get_session_path(sprintf("%s_dp", u_id), "%s_meta_analyzed_results.csv"),
          sprintf("tmp/user_sessions/%s/%s_dp_meta_results.csv", u_id, u_id),
          get_session_path(sprintf("%s_dp_meta", u_id), "%s_results.csv"),
          sprintf("tmp/%s_dp_meta_analyzed_results.csv", u_id),
          sprintf("tmp/%s_dp_%s_meta_analyzed_results.csv", u_id, dclass)
        )
      } else {
        c(
          sprintf("tmp/user_sessions/%s/%s_de_%s_meta_analyzed_results.csv", u_id, u_id, dclass),
          get_session_path(sprintf("%s_de_meta", u_id), sprintf("%%s_%s_meta_analyzed_results.csv", dclass)),
          get_session_path(sprintf("%s_de", u_id), sprintf("%%s_%s_meta_analyzed_results.csv", dclass)),
          sprintf("tmp/user_sessions/%s/%s_de_meta_analyzed_results.csv", u_id, u_id),
          get_session_path(sprintf("%s_de_meta", u_id), "%s_meta_analyzed_results.csv"),
          get_session_path(sprintf("%s_de", u_id), "%s_meta_analyzed_results.csv"),
          sprintf("tmp/user_sessions/%s/%s_de_meta_results.csv", u_id, u_id),
          get_session_path(sprintf("%s_de_meta", u_id), "%s_results.csv"),
          sprintf("tmp/%s_de_meta_analyzed_results.csv", u_id),
          sprintf("tmp/%s_de_%s_meta_analyzed_results.csv", u_id, dclass)
        )
      }
      
      for (mf_candidate in meta_file_candidates) {
        if (file.exists(mf_candidate)) {
          cat(sprintf("[INLINE-EA] Sourcing genes from %s meta-analysis file: %s\n", parent_mod, mf_candidate))
          meta_df <- tryCatch(read.csv(mf_candidate, stringsAsFactors = FALSE), error = function(e) NULL)
          if (!is.null(meta_df) && nrow(meta_df) > 0) {
            use_meta_source <- TRUE
            break
          }
        }
      }
    }
    
    if (!use_meta_source || is.null(meta_df)) {
      stop("[ERROR] Multiple datasets of the same type detected. Meta-analysis results are required before running enrichment analysis. Please run meta-analysis first.")
    }
  }

  genes       <- NULL
  gene_scores <- NULL
  genes_up    <- NULL
  genes_down  <- NULL

  if (use_meta_source && !is.null(meta_df)) {
    gene_col <- find_gene_col(meta_df, gene_id_col)
    
    # Treat gene ID column as character
    meta_df[[gene_col]] <- as.character(meta_df[[gene_col]])
    
    # Remove NA / blank genes in the gene ID column
    meta_df <- meta_df[!is.na(meta_df[[gene_col]]) & meta_df[[gene_col]] != "", ]
    
    if ("ora" %in% methods_val) {
      adj_col <- if ("qval" %in% colnames(meta_df)) "qval" else
                 if ("adj_pvalue" %in% colnames(meta_df)) "adj_pvalue" else
                 if ("adj.P.Val." %in% colnames(meta_df)) "adj.P.Val." else
                 if ("adjPValue" %in% colnames(meta_df)) "adjPValue" else
                 if ("P.value" %in% colnames(meta_df)) "P.value" else
                 if ("pValue" %in% colnames(meta_df)) "pValue" else
                 if ("pvalue" %in% colnames(meta_df)) "pvalue" else NULL
      if (!is.null(adj_col)) {
        sig_df <- meta_df[!is.na(meta_df[[adj_col]]) & as.numeric(meta_df[[adj_col]]) <= pval_cutoff, ]
      } else {
        sig_df <- meta_df
      }
      
      # Deduplicate gene ID
      sig_df <- sig_df[!duplicated(sig_df[[gene_col]]), ]
      
      sig_df_up  <- apply_direction_filter(sig_df, "up")
      genes_up   <- as.character(sig_df_up[[gene_col]])
      sig_df_dn  <- apply_direction_filter(sig_df, "down")
      genes_down <- as.character(sig_df_dn[[gene_col]])
      
      sig_df       <- apply_direction_filter(sig_df, direction)
      genes        <- as.character(sig_df[[gene_col]])
    }
    if ("gsea" %in% methods_val) {
      target_col <- NULL
      if (!is.null(rank_by) && rank_by != "" && rank_by %in% colnames(meta_df)) {
        target_col <- rank_by
      } else if (!is.null(rank_by) && rank_by != "" && any(tolower(colnames(meta_df)) == tolower(rank_by))) {
        target_col <- colnames(meta_df)[which(tolower(colnames(meta_df)) == tolower(rank_by))[1]]
      } else {
        target_col <- if ("Combined Effects Size" %in% colnames(meta_df)) "Combined Effects Size" else
                      if ("Combined.Effects.Size" %in% colnames(meta_df)) "Combined.Effects.Size" else
                      if ("Combined Effect Size" %in% colnames(meta_df)) "Combined Effect Size" else
                      if ("Combined.Effect.Size" %in% colnames(meta_df)) "Combined.Effect.Size" else
                      if ("Combined LogFC" %in% colnames(meta_df)) "Combined LogFC" else
                      if ("Combined.LogFC" %in% colnames(meta_df)) "Combined.LogFC" else
                      if ("log2FoldChange" %in% colnames(meta_df)) "log2FoldChange" else
                      if ("hedges_g" %in% colnames(meta_df) && any(!is.na(meta_df[["hedges_g"]]) & meta_df[["hedges_g"]] != 0)) "hedges_g" else
                      if ("combined_hedges_g" %in% colnames(meta_df) && any(!is.na(meta_df[["combined_hedges_g"]]) & meta_df[["combined_hedges_g"]] != 0)) "combined_hedges_g" else
                      if ("g" %in% colnames(meta_df) && any(!is.na(meta_df[["g"]]) & meta_df[["g"]] != 0)) "g" else
                      if ("d" %in% colnames(meta_df) && any(!is.na(meta_df[["d"]]) & meta_df[["d"]] != 0)) "d" else
                      if ("logFC" %in% colnames(meta_df)) "logFC" else
                      if ("LogFC" %in% colnames(meta_df)) "LogFC" else
                      if ("fc" %in% colnames(meta_df)) "fc" else
                      if ("FoldChange" %in% colnames(meta_df)) "FoldChange" else
                      if ("votes" %in% colnames(meta_df)) "votes" else NULL
      }
      pv_col <- if ("pval" %in% colnames(meta_df)) "pval" else
                if ("P.value" %in% colnames(meta_df)) "P.value" else
                if ("pValue" %in% colnames(meta_df)) "pValue" else
                if ("pvalue" %in% colnames(meta_df)) "pvalue" else NULL
      
      if (!is.null(target_col)) {
        # Remove NA in ranking column
        meta_df <- meta_df[!is.na(meta_df[[target_col]]), ]
        # Order by GSEA ranking column
        meta_df <- meta_df[order(meta_df[[target_col]]), ]
        # Deduplicate gene ID
        meta_df <- meta_df[!duplicated(meta_df[[gene_col]]), ]
        
        scores <- as.numeric(meta_df[[target_col]])
      } else if (!is.null(pv_col)) {
        meta_df <- meta_df[!is.na(meta_df[[pv_col]]), ]
        meta_df <- meta_df[order(meta_df[[pv_col]]), ]
        meta_df <- meta_df[!duplicated(meta_df[[gene_col]]), ]
        
        scores <- -log10(as.numeric(meta_df[[pv_col]]))
      } else {
        meta_df <- meta_df[!duplicated(meta_df[[gene_col]]), ]
        scores <- seq_len(nrow(meta_df))
      }
      names(scores) <- as.character(meta_df[[gene_col]])
      scores <- scores[!is.na(scores)]
      sorted_scores <- sort(scores, decreasing = TRUE)
      gene_scores   <- as.list(sorted_scores)
      genes         <- names(sorted_scores)
    }
  } else {
    # Fall back to single DE results
    inline_de_files <- find_de_files(dataset_id, base_id, target_is_proteomics, user_prefix, parent_module = parent_mod)
    cat(sprintf("[INLINE-EA] Sourcing genes from DE results for dataset %s (found %d files).\n", dataset_id, length(inline_de_files)))
    
    all_genes      <- c()
    all_genes_up   <- c()
    all_genes_down <- c()
    all_fc         <- c()
    all_gene_names <- c()

    for (de_file in inline_de_files) {
      de_df <- tryCatch(read.csv(de_file, stringsAsFactors = FALSE), error = function(e) NULL)
      
      # Self-healing: if de_df was previously truncated (<= 10 rows) but raw table cache exists, reconstruct it
      raw_table_path <- get_session_path(base_id, "%s_de_raw_table.rds")
      if (!file.exists(raw_table_path)) raw_table_path <- sprintf("tmp/%s_de_raw_table.rds", base_id)
      if (file.exists(raw_table_path)) {
        raw_df <- tryCatch(readRDS(raw_table_path), error = function(e) NULL)
        if (!is.null(raw_df) && nrow(raw_df) > (if (!is.null(de_df)) nrow(de_df) else 0)) {
          cat(sprintf("[INLINE-EA] Reconstructing full DE table from raw cache (%d features).\n", nrow(raw_df)))
          raw_df$logFC[is.na(raw_df$logFC)] <- 0
          raw_df$pValue[is.na(raw_df$pValue)] <- 1
          raw_df$adjPValue[is.na(raw_df$adjPValue)] <- 1
          raw_df$baseMean[is.na(raw_df$baseMean)] <- 0
          raw_df$significant <- raw_df$adjPValue < (pval_cutoff %||% 0.05) & abs(raw_df$logFC) >= 1.0
          raw_df$direction <- ifelse(raw_df$significant, ifelse(raw_df$logFC > 0, "up", "down"), "ns")
          ord_idx <- order(raw_df$adjPValue, raw_df$pValue)
          de_df <- raw_df[ord_idx, ]
          tryCatch(write.csv(de_df, de_file, row.names = FALSE), error = function(e) NULL)
        }
      }
      
      if (is.null(de_df) || nrow(de_df) == 0) next

      gene_col <- find_gene_col(de_df, gene_id_col)
      
      # Treat gene ID column as character
      de_df[[gene_col]] <- as.character(de_df[[gene_col]])
      
      # Remove NA / blank genes in the gene ID column
      de_df <- de_df[!is.na(de_df[[gene_col]]) & de_df[[gene_col]] != "", ]

      if ("ora" %in% methods_val) {
        sig_col <- if ("significant" %in% colnames(de_df)) "significant" else NULL
        adj_col <- if ("adj.P.Val." %in% colnames(de_df)) "adj.P.Val." else
                   if ("adjPValue" %in% colnames(de_df)) "adjPValue" else
                   if ("adj_pvalue" %in% colnames(de_df)) "adj_pvalue" else
                   if ("P-value" %in% colnames(de_df)) "P-value" else
                   if ("pValue" %in% colnames(de_df)) "pValue" else NULL
        if (!is.null(sig_col)) {
          sig_df <- de_df[de_df[[sig_col]] %in% c(TRUE, "TRUE", "true", "True", 1, "1"), ]
        } else if (!is.null(adj_col)) {
          sig_df <- de_df[!is.na(de_df[[adj_col]]) & de_df[[adj_col]] <= pval_cutoff, ]
        } else {
          sig_df <- de_df
        }
        
        # Deduplicate gene ID for ORA
        sig_df <- sig_df[!duplicated(sig_df[[gene_col]]), ]
        
        sig_df_up    <- apply_direction_filter(sig_df, "up")
        all_genes_up <- c(all_genes_up, as.character(sig_df_up[[gene_col]]))
        
        sig_df_dn    <- apply_direction_filter(sig_df, "down")
        all_genes_down <- c(all_genes_down, as.character(sig_df_dn[[gene_col]]))
        
        sig_df       <- apply_direction_filter(sig_df, direction)
        all_genes    <- c(all_genes, as.character(sig_df[[gene_col]]))
      }
      if ("gsea" %in% methods_val) {
        fc_col <- if (!is.null(rank_by) && rank_by != "" && rank_by %in% colnames(de_df)) rank_by else
                  if (!is.null(rank_by) && rank_by != "" && any(tolower(colnames(de_df)) == tolower(rank_by))) colnames(de_df)[which(tolower(colnames(de_df)) == tolower(rank_by))[1]] else
                  if ("logFC" %in% colnames(de_df)) "logFC" else
                  if ("log2FoldChange" %in% colnames(de_df)) "log2FoldChange" else
                  if ("LogFC" %in% colnames(de_df)) "LogFC" else
                  if ("fc" %in% colnames(de_df)) "fc" else
                  if ("FoldChange" %in% colnames(de_df)) "FoldChange" else NULL
        if (!is.null(fc_col)) {
          # Remove NA in GSEA ranking column
          de_df <- de_df[!is.na(de_df[[fc_col]]), ]
          # Order by ranking column
          de_df <- de_df[order(de_df[[fc_col]]), ]
          # Deduplicate gene ID
          de_df <- de_df[!duplicated(de_df[[gene_col]]), ]
          
          all_gene_names <- c(all_gene_names, as.character(de_df[[gene_col]]))
          all_fc         <- c(all_fc, as.numeric(de_df[[fc_col]]))
        } else {
          de_df <- de_df[!duplicated(de_df[[gene_col]]), ]
          all_genes <- c(all_genes, as.character(de_df[[gene_col]]))
        }
      }
    }

    if ("gsea" %in% methods_val && length(all_gene_names) > 0) {
      scores <- all_fc; names(scores) <- all_gene_names
      scores <- scores[!is.na(scores) & !is.na(names(scores)) & names(scores) != ""]
      # Dedup names of scores to keep largest effect size in order
      scores <- scores[order(scores)]
      scores <- scores[!duplicated(names(scores))]
      
      sorted_scores <- sort(scores, decreasing = TRUE)
      gene_scores <- as.list(sorted_scores)
      genes <- names(sorted_scores)
    }
    if ("ora" %in% methods_val) {
      if (length(all_genes_up) > 0) {
        genes_up <- unique(all_genes_up[!is.na(all_genes_up) & all_genes_up != ""])
      }
      if (length(all_genes_down) > 0) {
        genes_down <- unique(all_genes_down[!is.na(all_genes_down) & all_genes_down != ""])
      }
      if (length(all_genes) > 0) {
        genes <- unique(all_genes[!is.na(all_genes) & all_genes != ""])
      }
    }
  }

  if (is.null(genes) || length(genes) == 0) {
    cat("[INLINE-EA] WARNING: no genes resolved for enrichment.\n")
    
    ora_headers <- c("Description", "Hits_count", "Total_input_gene", "GeneRatio", "Pvalue", "P.adjust", "FeaturesID")
    gsea_headers <- c("Description", "setSize", "Hits", "enrichmentScore", "NES", "pvalue", "p.adjust", "core_enrichment")

    # Save empty results files with headers to prevent 404 on individual card downloads
    if ("ora" %in% methods_val && !is.null(ora_db)) {
      for (db in ora_db) {
        save_list_to_csv(list(), session_file_path(dataset_id, sprintf("%s_ora_results_%s.csv", dataset_id, db)), 
                         headers = ora_headers)
      }
    }
    if ("gsea" %in% methods_val && !is.null(gsea_db)) {
      for (db in gsea_db) {
        save_list_to_csv(list(), session_file_path(dataset_id, sprintf("%s_gsea_results_%s.csv", dataset_id, db)), 
                         headers = gsea_headers)
      }
    }
    
    return(list(oraResults = list(), gseaResults = list(), unmappedGenes = list(), totalInputGenes = 0, mappedGenesCount = 0))
  }

  cat(sprintf("[INLINE-EA] Running %s with %d genes.\n", paste(methods_val, collapse="+"), length(genes)))
  combined_res <- list(
    oraResults = list(),
    gseaResults = list(),
    oraObjects = list(),
    gseObjects = list(),
    unmappedGenes = list(),
    totalInputGenes = 0,
    mappedGenesCount = 0
  )
  for (m in methods_val) {
    if (m == "ora") {
      run_ora_with_dir <- function(target_genes, dir_label) {
        run_enrichment_analysis(m, ora_db, gsea_db, rank_by, pval_cutoff, qval_cutoff,
                                min_size, max_size, organism, target_genes, gene_scores, gene_id_type, dataset_id = dataset_id, direction = dir_label)
      }
      
      if (direction == "all") {
        if (length(genes_up) > 0 && length(genes_down) > 0 && !identical(genes_up, genes_down)) {
          cat("[INLINE-EA] Running ORA for UP and DOWN genes separately.\n")
          res_up   <- run_ora_with_dir(genes_up, "up_genes")
          res_down <- run_ora_with_dir(genes_down, "down_genes")
          
          combined_res$oraResults  <- c(combined_res$oraResults,  res_up$oraResults, res_down$oraResults)
          if (!is.null(res_up$oraObjects)) {
            for (db in names(res_up$oraObjects)) {
              combined_res$oraObjects[[paste0(db, "_up")]] <- res_up$oraObjects[[db]]
            }
          }
          if (!is.null(res_down$oraObjects)) {
            for (db in names(res_down$oraObjects)) {
              combined_res$oraObjects[[paste0(db, "_down")]] <- res_down$oraObjects[[db]]
            }
          }
          if (!is.null(res_up$unmappedGenes) || !is.null(res_down$unmappedGenes)) {
            combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res_up$unmappedGenes), unlist(res_down$unmappedGenes)))
          }
          val_up <- if (!is.null(res_up$totalInputCount)) res_up$totalInputCount else 0
          val_dn <- if (!is.null(res_down$totalInputCount)) res_down$totalInputCount else 0
          tot_input <- max(val_up, val_dn)
          
          map_up <- if (!is.null(res_up$mappedCount)) res_up$mappedCount else 0
          map_dn <- if (!is.null(res_down$mappedCount)) res_down$mappedCount else 0
          map_count <- max(map_up, map_dn)
          
          if (tot_input > combined_res$totalInputGenes) combined_res$totalInputGenes <- tot_input
          if (map_count > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- map_count
        } else if (length(genes_up) > 0 && length(genes_down) == 0) {
          cat("[INLINE-EA] Running ORA for UP genes only (no DOWN genes found).\n")
          res_up <- run_ora_with_dir(genes_up, "up_genes")
          combined_res$oraResults <- c(combined_res$oraResults, res_up$oraResults)
          if (!is.null(res_up$oraObjects)) {
            for (db in names(res_up$oraObjects)) {
              combined_res$oraObjects[[paste0(db, "_up")]] <- res_up$oraObjects[[db]]
            }
          }
          if (!is.null(res_up$unmappedGenes)) combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res_up$unmappedGenes)))
          if (!is.null(res_up$totalInputCount) && res_up$totalInputCount > combined_res$totalInputGenes) combined_res$totalInputGenes <- res_up$totalInputCount
          if (!is.null(res_up$mappedCount) && res_up$mappedCount > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- res_up$mappedCount
        } else if (length(genes_down) > 0 && length(genes_up) == 0) {
          cat("[INLINE-EA] Running ORA for DOWN genes only (no UP genes found).\n")
          res_down <- run_ora_with_dir(genes_down, "down_genes")
          combined_res$oraResults <- c(combined_res$oraResults, res_down$oraResults)
          if (!is.null(res_down$oraObjects)) {
            for (db in names(res_down$oraObjects)) {
              combined_res$oraObjects[[paste0(db, "_down")]] <- res_down$oraObjects[[db]]
            }
          }
          if (!is.null(res_down$unmappedGenes)) combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res_down$unmappedGenes)))
          if (!is.null(res_down$totalInputCount) && res_down$totalInputCount > combined_res$totalInputGenes) combined_res$totalInputGenes <- res_down$totalInputCount
          if (!is.null(res_down$mappedCount) && res_down$mappedCount > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- res_down$mappedCount
        } else {
          cat("[INLINE-EA] Running ORA on all significant genes combined (no distinct up/down found).\n")
          res <- run_ora_with_dir(genes, NULL)
          combined_res$oraResults  <- c(combined_res$oraResults,  res$oraResults)
          if (!is.null(res$oraObjects)) {
            for (db in names(res$oraObjects)) {
              combined_res$oraObjects[[db]] <- res$oraObjects[[db]]
            }
          }
          if (!is.null(res$unmappedGenes)) combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res$unmappedGenes)))
          if (!is.null(res$totalInputCount) && res$totalInputCount > combined_res$totalInputGenes) combined_res$totalInputGenes <- res$totalInputCount
          if (!is.null(res$mappedCount) && res$mappedCount > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- res$mappedCount
        }
      } else {
        dir_label <- if (direction %in% c("up", "down", "up_genes", "down_genes")) {
          if (grepl("up", direction, ignore.case = TRUE)) "up_genes" else "down_genes"
        } else NULL
        target_genes <- if (identical(dir_label, "up_genes")) genes_up else if (identical(dir_label, "down_genes")) genes_down else genes
        cat(sprintf("[INLINE-EA] Running ORA on %s genes.\n", ifelse(is.null(dir_label), "all", toupper(dir_label))))
        res <- run_ora_with_dir(target_genes, dir_label)
        combined_res$oraResults  <- c(combined_res$oraResults,  res$oraResults)
        if (!is.null(res$oraObjects)) {
          for (db in names(res$oraObjects)) {
            target_key <- if (!is.null(dir_label)) paste0(db, "_", if (dir_label == "up_genes") "up" else "down") else db
            combined_res$oraObjects[[target_key]] <- res$oraObjects[[db]]
          }
        }
        if (!is.null(res$unmappedGenes)) combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res$unmappedGenes)))
        if (!is.null(res$totalInputCount) && res$totalInputCount > combined_res$totalInputGenes) combined_res$totalInputGenes <- res$totalInputCount
        if (!is.null(res$mappedCount) && res$mappedCount > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- res$mappedCount
      }
    }
    
    if (m == "gsea") {
      res <- run_enrichment_analysis(m, ora_db, gsea_db, rank_by, pval_cutoff, qval_cutoff,
                                     min_size, max_size, organism, genes, gene_scores, gene_id_type, dataset_id = dataset_id)
      combined_res$gseaResults <- c(combined_res$gseaResults, res$gseaResults)
      if (!is.null(res$gseObjects)) {
        for (db in names(res$gseObjects)) {
          combined_res$gseObjects[[db]] <- res$gseObjects[[db]]
        }
      }
      if (!is.null(res$unmappedGenes)) combined_res$unmappedGenes <- unique(c(combined_res$unmappedGenes, unlist(res$unmappedGenes)))
      if (!is.null(res$totalInputCount) && res$totalInputCount > combined_res$totalInputGenes) combined_res$totalInputGenes <- res$totalInputCount
      if (!is.null(res$mappedCount) && res$mappedCount > combined_res$mappedGenesCount) combined_res$mappedGenesCount <- res$mappedCount
    }
  }

  # Save main ORA results to CSV (direction is the last column if present)
  uid_val <- get_user_id(dataset_id)
  ora_headers_base <- c("Description", "Hits_count", "Total_input_gene", "GeneRatio", "Pvalue", "P.adjust", "FeaturesID")
  gsea_headers <- c("Description", "setSize", "Hits", "enrichmentScore", "NES", "pvalue", "p.adjust", "core_enrichment")

  if ("ora" %in% methods_val && !is.null(ora_db)) {
    for (db in ora_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      db_results <- Filter(function(x) identical(x$database, db) || identical(x$database, db_clean), combined_res$oraResults)
      has_dir <- any(vapply(db_results, function(x) !is.null(x$direction) && nzchar(as.character(x$direction)), logical(1)))
      ora_headers <- if (has_dir) c(ora_headers_base, "direction") else ora_headers_base
      ora_res_path <- session_file_path(dataset_id, sprintf("%s_ora_results_%s.csv", dataset_id, db_clean))
      save_list_to_csv(db_results, ora_res_path, headers = ora_headers)
      register_export_file(uid_val, "ora_results", dataset_id, ora_res_path, "ea", "ora", db = db_clean)
    }
  }

  # Save main GSEA results to CSV
  if ("gsea" %in% methods_val && !is.null(gsea_db)) {
    for (db in gsea_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      db_results <- Filter(function(x) identical(x$database, db) || identical(x$database, db_clean), combined_res$gseaResults)
      gsea_res_path <- session_file_path(dataset_id, sprintf("%s_gsea_results_%s.csv", dataset_id, db_clean))
      save_list_to_csv(db_results, gsea_res_path, headers = gsea_headers)
      register_export_file(uid_val, "gsea_results", dataset_id, gsea_res_path, "ea", "gsea", db = db_clean)
    }
  }

  # Generate ORA plots (split into up/down if present)
  if ("ora" %in% methods_val && !is.null(combined_res$oraObjects)) {
    library(enrichplot)
    for (obj_key in names(combined_res$oraObjects)) {
      ora_obj <- combined_res$oraObjects[[obj_key]]
      if (is.null(ora_obj) || nrow(as.data.frame(ora_obj)) == 0) next
      
      if (grepl("_up$", obj_key)) {
        raw_db <- sub("_up$", "", obj_key)
        raw_db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", raw_db)
        dot_path <- session_file_path(dataset_id, sprintf("%s_ora_dotplot_up_%s.pdf", dataset_id, raw_db_clean))
        tryCatch({
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(ora_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "ora_dotplot_up", dataset_id, dot_path, "ea", "ora", db = raw_db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
      } else if (grepl("_down$", obj_key)) {
        raw_db <- sub("_down$", "", obj_key)
        raw_db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", raw_db)
        dot_path <- session_file_path(dataset_id, sprintf("%s_ora_dotplot_down_%s.pdf", dataset_id, raw_db_clean))
        tryCatch({
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(ora_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "ora_dotplot_down", dataset_id, dot_path, "ea", "ora", db = raw_db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
      } else {
        db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", obj_key)
        dot_path <- session_file_path(dataset_id, sprintf("%s_ora_dotplot_%s.pdf", dataset_id, db_clean))
        tryCatch({
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(ora_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "ora_dotplot", dataset_id, dot_path, "ea", "ora", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
      }
    }
  }

  # Generate GSEA plots
  if ("gsea" %in% methods_val && !is.null(combined_res$gseObjects)) {
    for (db in gsea_db) {
      db_clean <- gsub("[:/\\\\?*\"<>| ]", "_", db)
      gse_obj <- combined_res$gseObjects[[db]]
      if (!is.null(gse_obj) && nrow(as.data.frame(gse_obj)) > 0) {
        dot_path   <- session_file_path(dataset_id, sprintf("%s_gsea_dotplot_%s.pdf", dataset_id, db_clean))
        ridge_path <- session_file_path(dataset_id, sprintf("%s_gsea_ridgeplot_%s.pdf", dataset_id, db_clean))
        es_up_path <- session_file_path(dataset_id, sprintf("%s_gsea_esplot_up_%s.pdf", dataset_id, db_clean))
        es_dn_path <- session_file_path(dataset_id, sprintf("%s_gsea_esplot_down_%s.pdf", dataset_id, db_clean))
        
        # Save GSEA Ranked Gene List
        if (exists("sorted_scores") && !is.null(sorted_scores) && length(sorted_scores) > 0) {
          df_ranked <- data.frame(
            Gene = names(sorted_scores),
            RankMetric = unname(sorted_scores),
            stringsAsFactors = FALSE
          )
          df_ranked_list <- lapply(1:nrow(df_ranked), function(i) as.list(df_ranked[i, ]))
          ranked_path <- session_file_path(dataset_id, sprintf("%s_gsea_ranked_list_%s.csv", dataset_id, db_clean))
          save_list_to_csv(df_ranked_list, ranked_path)
          register_export_file(uid_val, "gsea_ranked_list", dataset_id, ranked_path, "ea", "gsea", db = db_clean)
        }
        
        # Save GSEA Leading Edge Genes
        gse_df <- as.data.frame(gse_obj)
        if (!is.null(gse_df) && nrow(gse_df) > 0) {
          cols_to_keep <- intersect(c("ID", "Description", "NES", "pvalue", "p.adjust", "qvalue", "core_enrichment"), colnames(gse_df))
          df_le <- gse_df[, cols_to_keep, drop = FALSE]
          df_le_list <- lapply(1:nrow(df_le), function(i) as.list(df_le[i, ]))
          le_path <- session_file_path(dataset_id, sprintf("%s_gsea_leading_edge_%s.csv", dataset_id, db_clean))
          save_list_to_csv(df_le_list, le_path)
          register_export_file(uid_val, "gsea_leading_edge", dataset_id, le_path, "ea", "gsea", db = db_clean)
        }
        
        library(enrichplot)
        tryCatch({
          pdf(dot_path, width = 10, height = 7)
          print(enrichplot::dotplot(gse_obj, showCategory = 10, orderBy = "GeneRatio", label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.8))
          dev.off()
          register_export_file(uid_val, "gsea_dotplot", dataset_id, dot_path, "ea", "gsea", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
        
        tryCatch({
          pdf(ridge_path, width = 10, height = 7)
          print(enrichplot::ridgeplot(gse_obj, showCategory = 10, label_format = 60) + 
                  theme(axis.text.y = element_text(size = 12), aspect.ratio = 1.2))
          dev.off()
          register_export_file(uid_val, "gsea_ridgeplot", dataset_id, ridge_path, "ea", "gsea", db = db_clean, ext = "pdf")
        }, error = function(e) {
          if (dev.cur() > 1) dev.off()
        })
        
        gse_df <- as.data.frame(gse_obj)
        if (nrow(gse_df) > 0) {
          up_rows <- gse_df[gse_df$NES > 0, ]
          if (nrow(up_rows) > 0) {
            top_up_id <- up_rows$ID[order(up_rows$NES, decreasing = TRUE)[1]]
            tryCatch({
              pdf(es_up_path, width = 9, height = 6)
              print(enrichplot::gseaplot2(gse_obj, geneSetID = top_up_id, title = gse_df$Description[gse_df$ID == top_up_id]))
              dev.off()
              register_export_file(uid_val, "gsea_esplot_up", dataset_id, es_up_path, "ea", "gsea", db = db_clean, ext = "pdf")
            }, error = function(e) {
              if (dev.cur() > 1) dev.off()
            })
          }
          
          dn_rows <- gse_df[gse_df$NES < 0, ]
          if (nrow(dn_rows) > 0) {
            top_dn_id <- dn_rows$ID[order(dn_rows$NES, decreasing = FALSE)[1]]
            tryCatch({
              pdf(es_dn_path, width = 9, height = 6)
              print(enrichplot::gseaplot2(gse_obj, geneSetID = top_dn_id, title = gse_df$Description[gse_df$ID == top_dn_id]))
              dev.off()
              register_export_file(uid_val, "gsea_esplot_down", dataset_id, es_dn_path, "ea", "gsea", db = db_clean, ext = "pdf")
            }, error = function(e) {
              if (dev.cur() > 1) dev.off()
            })
          }
        }
      }
    }
  }

  # Call finalize_ea to generate report
  finalize_ea(combined_res, config)

  # Clean up objects before returning
  combined_res$oraObjects <- NULL
  combined_res$gseObjects <- NULL
  return(combined_res)
}
