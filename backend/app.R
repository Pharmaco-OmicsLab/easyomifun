# Configure local R library path for self-contained, permission-safe package loading
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
if (length(script.name) > 0) {
  script.dir <- dirname(normalizePath(script.name))
} else {
  script.dir <- getwd()
}
# 1. EasyOmiFun desktop R libraries path settings
env_custom_lib <- Sys.getenv("EASYOMIFUN_RLIB", unset = "")
env_user_lib   <- Sys.getenv("R_LIBS_USER", unset = "")
app_mode       <- Sys.getenv("EASYOMIFUN_APP_MODE", unset = "")

local_lib <- if (nzchar(env_custom_lib)) {
  env_custom_lib
} else if (app_mode == "desktop" && nzchar(env_user_lib) && grepl("EasyOmiFun", env_user_lib, ignore.case = TRUE)) {
  env_user_lib
} else if (nzchar(env_user_lib) && grepl("R-lib", env_user_lib, fixed = TRUE)) {
  env_user_lib
} else {
  file.path(script.dir, "R-lib")
}
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}
local_lib <- normalizePath(local_lib, winslash = "/", mustWork = FALSE)
bundled_lib <- normalizePath(file.path(script.dir, "R-lib"), winslash = "/", mustWork = FALSE)
paths_to_set <- c(local_lib)
if (dir.exists(bundled_lib) && bundled_lib != local_lib) {
  paths_to_set <- c(paths_to_set, bundled_lib)
}
.libPaths(unique(c(paths_to_set, .Library)))
Sys.setenv(R_LIBS_USER = local_lib)
Sys.setenv(R_LIBS_SITE = local_lib)
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
Sys.setenv(R_PARALLEL_PORT = "random")

# Configure local Python (Miniconda) path for self-contained, isolated Python environment
env_conda <- Sys.getenv("RETICULATE_MINICONDA_PATH", unset = "")
conda_path <- if (nzchar(env_conda)) env_conda else {
  if (Sys.info()["sysname"] == "Darwin") {
    file.path(Sys.getenv("HOME"), "Library", "EasyOmiFun", "miniconda")
  } else {
    file.path(script.dir, "miniconda")
  }
}
conda_path <- tryCatch(normalizePath(conda_path, winslash = "/", mustWork = FALSE), error = function(e) conda_path)
if (dir.exists(conda_path)) {
  Sys.setenv(RETICULATE_MINICONDA_PATH = conda_path)
  py_candidates <- if (.Platform$OS.type == "windows") {
    c(file.path(conda_path, "python.exe"), file.path(conda_path, "Scripts", "python.exe"))
  } else {
    c(file.path(conda_path, "bin", "python3.11"),
      file.path(conda_path, "bin", "python"),
      file.path(conda_path, "bin", "python3"),
      file.path(conda_path, "bin", "python3.13"),
      file.path(conda_path, "bin", "python3.12"),
      file.path(conda_path, "bin", "python3.10"))
  }
  for (cand in py_candidates) {
    if (file.exists(cand)) {
      py_path <- tryCatch(normalizePath(cand, winslash = "/", mustWork = FALSE), error = function(e) cand)
      Sys.setenv(STABL_PYTHON = py_path)
      Sys.setenv(RETICULATE_PYTHON = py_path)
      break
    }
  }
}

library(shiny)
library(jsonlite)

options(shiny.maxRequestSize = 500 * 1024^2)
if (identical(Sys.info()[["sysname"]], "Linux")) {
  options(bitmapType = "cairo")
}

# Source shared utility functions
source("shared_utils.R")

# Initialize global threading and hardware limits
setup_threading_environment()

# Source the individual processing modules
source("processing.R")
source("de_analysis.R")
source("feature_selection.R")
source("enrichment.R")
source("report_finalize.R")

# Global reproducibility seed — set once when the server starts.
# Each individual job also resets to 42 before its model training loop.
set.seed(42)

# ─── Async job registry ────────────────────────────────────────────────────────
.fs_jobs <- new.env(parent = emptyenv())
dir.create("tmp/jobs", showWarnings = FALSE, recursive = TRUE)

.export_queue <- new.env(parent = emptyenv())
N_PARALLEL_EXPORT <- 2L

enqueue_export_job <- function(job_id, payload) {
  assign(job_id, list(
    status  = "queued",
    payload = payload,
    started = Sys.time(),
    result  = NULL,
    message = NULL
  ), envir = .export_queue)
}

process_export_queue <- function() {
  tryCatch({
    ids <- ls(.export_queue)
    if (length(ids) > 0) {
      now <- Sys.time()
      for (id in ids) {
        job <- get(id, envir = .export_queue)
        if (as.numeric(difftime(now, job$started, units = "mins")) > 10) {
          rm(list = id, envir = .export_queue)
        }
      }

      ids <- ls(.export_queue)
      n_running <- sum(vapply(ids, function(id) {
        get(id, envir = .export_queue)$status == "running"
      }, logical(1)))

      if (n_running < N_PARALLEL_EXPORT) {
        queued <- Filter(function(id) {
          get(id, envir = .export_queue)$status == "queued"
        }, ids)

        if (length(queued) > 0) {
          job_id <- queued[[1]]
          job <- get(job_id, envir = .export_queue)
          job$status <- "running"
          assign(job_id, job, envir = .export_queue)

          tryCatch({
            p <- job$payload
            res <- resolve_export_file(p$type, p$dsId, p$model, p$ext, p$userId)
            if (is.null(res) || is.null(res$file_path) || !file.exists(res$file_path)) {
              job$status  <- "error"
              job$message <- sprintf("The %s file is not available yet. Run the analysis step first.", p$type)
            } else {
              job$status <- "done"
              job$result <- res
            }
          }, error = function(e) {
            job$status  <- "error"
            job$message <- conditionMessage(e)
          })
          assign(job_id, job, envir = .export_queue)
        }
      }
    }
  }, error = function(e) NULL)
  later::later(process_export_queue, 0.2)
}

if (requireNamespace("later", quietly = TRUE)) {
  later::later(process_export_queue, 0.2)
}

get_max_concurrent_jobs <- function() {
  get_resource_limits()$maxConcurrentJobs
}
.FS_MAX_CONCURRENT <- get_max_concurrent_jobs()
# Finished jobs older than this are swept from the registry and disk (prevents leaks).
.FS_JOB_TTL_SECS   <- as.integer(Sys.getenv("FS_JOB_TTL_SECS", "1800"))  # 30 minutes

# Count jobs whose background process is still alive.
fs_jobs_running <- function() {
  ids <- ls(.fs_jobs)
  if (length(ids) == 0) return(0L)
  sum(vapply(ids, function(id) {
    j <- get(id, envir = .fs_jobs)
    !is.null(j$process) && isTRUE(tryCatch(j$process$is_alive(), error = function(e) FALSE))
  }, logical(1)))
}

# Remove a job from the registry and delete its on-disk in/out/log files.
fs_job_remove <- function(id) {
  if (!exists(id, envir = .fs_jobs, inherits = FALSE)) return(invisible())
  j <- get(id, envir = .fs_jobs)
  files <- c(j$out, j$log, j$progress)
  if (!is.null(j$out)) files <- c(files, sub("_out\\.rds$", "_in.rds", j$out))
  for (f in files) {
    if (!is.null(f) && nzchar(f) && file.exists(f)) tryCatch(file.remove(f), error = function(e) NULL)
  }
  rm(list = id, envir = .fs_jobs)
  invisible()
}

# Sweep finished jobs older than the TTL so .fs_jobs and tmp/ don't grow unbounded.
fs_jobs_cleanup <- function(ttl = .FS_JOB_TTL_SECS) {
  now <- Sys.time()
  for (id in ls(.fs_jobs)) {
    j <- tryCatch(get(id, envir = .fs_jobs), error = function(e) NULL)
    if (is.null(j)) next
    alive <- !is.null(j$process) && isTRUE(tryCatch(j$process$is_alive(), error = function(e) FALSE))
    age <- as.numeric(difftime(now, j$started, units = "secs"))
    if (!alive && age > ttl) fs_job_remove(id)
  }
  invisible()
}

# Immediately terminate running background jobs for a dataset or user
terminate_jobs_for_dataset <- function(dataset_id = NULL, user_id = NULL) {
  base_id <- if (!is.null(dataset_id) && nzchar(dataset_id)) get_base_id(dataset_id) else NULL
  for (jid in ls(.fs_jobs)) {
    job <- tryCatch(get(jid, envir = .fs_jobs), error = function(e) NULL)
    if (is.null(job)) next
    
    match_found <- FALSE
    
    # Check user_id match if provided and dataset_id is NULL (e.g. full user cleanup)
    if (is.null(dataset_id) && !is.null(user_id) && nzchar(user_id)) {
      if (identical(job$user_id, user_id)) {
        match_found <- TRUE
      }
    }
    
    # Check dataset match
    if (!match_found && !is.null(dataset_id) && nzchar(dataset_id)) {
      if (!match_found && !is.null(job$datasets_list)) {
        match_found <- any(vapply(job$datasets_list, function(d) {
          id_val <- if (is.list(d)) (d$datasetId %||% d$id %||% "") else as.character(d)
          identical(id_val, dataset_id) || identical(get_base_id(id_val), base_id)
        }, logical(1)))
      }
      
      if (!match_found && !is.null(job$datasets)) {
        match_found <- any(vapply(job$datasets, function(d) {
          id_val <- if (is.list(d)) (d$datasetId %||% d$id %||% "") else as.character(d)
          identical(id_val, dataset_id) || identical(get_base_id(id_val), base_id)
        }, logical(1)))
      }
      
      if (!match_found && !is.null(job$payload)) {
        if (is.list(job$payload)) {
          if (!is.null(job$payload$datasetIds)) {
            match_found <- any(job$payload$datasetIds %in% c(dataset_id, base_id))
          } else if (!is.null(job$payload$datasetId)) {
            match_found <- identical(job$payload$datasetId, dataset_id) || identical(get_base_id(job$payload$datasetId), base_id)
          } else {
            match_found <- any(vapply(job$payload, function(d) {
              if (is.list(d)) {
                id_val <- d$datasetId %||% d$id %||% ""
                identical(id_val, dataset_id) || identical(get_base_id(id_val), base_id)
              } else {
                identical(as.character(d), dataset_id) || identical(get_base_id(as.character(d)), base_id)
              }
            }, logical(1)))
          }
        }
      }
      
      if (!match_found && !is.null(job$datasets_by_class)) {
        for (cl in names(job$datasets_by_class)) {
          if (any(vapply(job$datasets_by_class[[cl]], function(d) {
            id_val <- if (is.list(d)) (d$datasetId %||% d$id %||% "") else as.character(d)
            identical(id_val, dataset_id) || identical(get_base_id(id_val), base_id)
          }, logical(1)))) {
            match_found <- TRUE
            break
          }
        }
      }
    }
    
    if (match_found) {
      cat(sprintf("[JOB-KILL] Terminating active background process for job '%s' (datasetId='%s', userId='%s')\n",
                  jid, dataset_id %||% "all", user_id %||% "none"))
      if (!is.null(job$process) && isTRUE(tryCatch(job$process$is_alive(), error = function(e) FALSE))) {
        tryCatch(job$process$kill(), error = function(e) NULL)
      }
      fs_job_remove(jid)
    }
  }
}

# Perform physical disk cleanup and report section removal for a dataset
perform_dataset_file_cleanup <- function(dataset_id, scope = "all", user_id = NULL) {
  if (!dir.exists("tmp")) return(invisible(0L))
  if (is.null(user_id) || !nzchar(user_id)) user_id <- get_user_id(dataset_id)
  
  if (!is.null(user_id) && nzchar(user_id)) {
    for (m in c("dp", "de", "fs", "ea", "en", "enrichment")) {
      for (st in c("upload", "annotation", "processing", "normalization", "batch", "de", "meta", "fs", "ea")) {
        tryCatch(remove_sections_from_report(user_id, st, dataset_id, m), error = function(e) NULL)
      }
    }
  }

  base_id   <- get_base_id(dataset_id)
  target_id <- if (!is.null(base_id) && nzchar(base_id)) base_id else sub("_(dp|de|fs|ea|en|enrichment).*", "", dataset_id)
  prefix    <- target_id
  all_files <- list.files("tmp", full.names = TRUE, recursive = TRUE)
  matched_files <- all_files[grepl(prefix, basename(all_files))]
  
  to_delete <- c()
  if (scope == "all") {
    to_delete <- matched_files
  } else if (scope == "expression") {
    to_delete <- matched_files[!grepl("(_clinical\\.csv|_clin_metadata\\.rds)$", basename(matched_files))]
  } else if (scope == "clinical") {
    keep_patterns <- c(
      "_expression\\.csv$",
      "_original_parsed\\.rds$",
      "_expr_metadata\\.rds$",
      "_expression_original_backup\\.csv$",
      "_main_stack_original_backup\\.rds$",
      "_counts_stack_original_backup\\.rds$"
    )
    keep <- FALSE
    for (pat in keep_patterns) {
      keep <- keep | grepl(pat, basename(matched_files))
    }
    to_delete <- matched_files[!keep]
    
    backup_expr_csv <- get_session_path(target_id, "%s_expression_original_backup.csv")
    if (file.exists(backup_expr_csv)) {
      file.copy(backup_expr_csv, get_session_path(target_id, "%s_expression.csv"), overwrite = TRUE)
    }
    backup_main_rds <- get_session_path(target_id, "%s_main_stack_original_backup.rds")
    if (file.exists(backup_main_rds)) {
      file.copy(backup_main_rds, get_session_path(target_id, "%s_main_stack.rds"), overwrite = TRUE)
    }
    backup_counts_rds <- get_session_path(target_id, "%s_counts_stack_original_backup.rds")
    if (file.exists(backup_counts_rds)) {
      file.copy(backup_counts_rds, get_session_path(target_id, "%s_counts_stack.rds"), overwrite = TRUE)
    }
  } else {
    to_delete <- all_files[grepl(sprintf("^%s_%s", target_id, scope), basename(all_files))]
  }
  
  if (length(to_delete) > 0) {
    unlink(to_delete, force = TRUE)
  }
  invisible(length(to_delete))
}

# Helper function to format JSON safely
json_response <- function(data, status_code = 200) {
  return(httpResponse(
    status = status_code,
    content_type = "application/json",
    headers = list(
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Allow-Methods" = "POST, GET, OPTIONS, DELETE",
      "Access-Control-Allow-Headers" = "Content-Type, Authorization"
    ),
    content = jsonlite::toJSON(data, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
  ))
}

get_session_data_class <- function(user_id) {
  if (is.null(user_id) || !nzchar(user_id)) return("others")
  session_dir <- file.path("tmp/user_sessions", user_id)
  if (!dir.exists(session_dir)) return("others")
  meta_files <- list.files(session_dir, pattern = "_expr_metadata\\.rds$", full.names = TRUE)
  if (length(meta_files) > 0) {
    meta <- tryCatch(readRDS(meta_files[1]), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$dataType)) {
      dtype <- meta$dataType
      if (dtype %in% c("readcounts", "microarray")) {
        return("transcriptomics")
      } else if (dtype == "proteomics") {
        return("proteomics")
      } else {
        return("others")
      }
    }
  }
  return("others")
}

submit_async_compute_job <- function(kind, payload) {
  fs_jobs_cleanup()
  max_concurrent <- get_max_concurrent_jobs()
  if (fs_jobs_running() >= max_concurrent) {
    return(json_response(list(status = "error",
      message = sprintf("Server busy: %d compute jobs already running (max: %d). Please retry shortly.",
                        fs_jobs_running(), max_concurrent)), 429))
  }

  # Resolve user_id dynamically based on kind
  user_id <- NULL
  if (kind %in% c("upload", "annotation", "processing", "normalization", "batch", "inline_de", "inline_de_meta", "inline_ea", "skip", "redo", "cv", "testing", "supplement_clinical")) {
    ds_list <- if (!is.null(payload$datasets)) payload$datasets else (if (!is.null(payload$datasetIds)) payload$datasetIds else payload)
    if (is.list(ds_list) && length(ds_list) > 0) {
      first_item <- ds_list[[1]]
      ds_id <- if (is.list(first_item)) {
        if (!is.null(first_item$datasetId)) first_item$datasetId else first_item$id
      } else {
        first_item
      }
      if (!is.null(ds_id)) {
        user_id <- get_user_id(ds_id)
      }
    }
    if (is.null(user_id) && is.list(payload) && !is.null(payload$datasetId)) {
      user_id <- get_user_id(payload$datasetId)
    }
  } else if (kind %in% c("refit", "fs_refit")) {
    refit_job_id <- payload$jobId
    refit_in_path <- ""
    standard_in <- file.path("tmp/jobs", paste0(refit_job_id, "_in.rds"))
    if (file.exists(standard_in)) {
      refit_in_path <- standard_in
    } else {
      user_dirs <- list.dirs("tmp/user_sessions", recursive = FALSE)
      for (ud in user_dirs) {
        path_check <- file.path(ud, "jobs", paste0(refit_job_id, "_in.rds"))
        if (file.exists(path_check)) {
          refit_in_path <- path_check
          break
        }
      }
    }
    if (nzchar(refit_in_path) && file.exists(refit_in_path)) {
      orig_in_data <- tryCatch(readRDS(refit_in_path), error = function(e) NULL)
      if (!is.null(orig_in_data) && length(orig_in_data$datasets) > 0) {
        first_ds <- orig_in_data$datasets[[1]]
        ds_id <- if (is.list(first_ds)) (first_ds$datasetId %||% first_ds$id) else first_ds
        user_id <- get_user_id(ds_id)
      }
    }
  } else if (identical(kind, "pca")) {
    ds_list <- if (!is.null(payload$datasets)) payload$datasets else payload
    if (length(ds_list) > 0) {
      first_item <- ds_list[[1]]
      ds_id <- if (is.list(first_item)) {
        if (!is.null(first_item$id)) first_item$id else first_item$datasetId
      } else {
        first_item
      }
      if (!is.null(ds_id)) {
        user_id <- get_user_id(ds_id)
      }
    }
  } else if (kind %in% c("ea", "inline_ea")) {
    ds_id <- if (is.list(payload)) {
      if (!is.null(payload$datasetId)) payload$datasetId else {
        if (length(payload) > 0 && is.list(payload[[1]])) payload[[1]]$datasetId else NULL
      }
    } else {
      payload
    }
    if (!is.null(ds_id)) {
      user_id <- get_user_id(ds_id)
    }
  } else if (identical(kind, "meta")) {
    ds_list <- if (!is.null(payload$datasets)) payload$datasets else payload
    if (length(ds_list) > 0) {
      first_item <- ds_list[[1]]
      ds_id <- if (is.list(first_item)) {
        if (!is.null(first_item$datasetId)) first_item$datasetId else first_item$id
      } else {
        first_item
      }
      if (!is.null(ds_id)) {
        user_id <- get_user_id(ds_id)
      }
    }
  } else if (identical(kind, "de")) {
    ds_list <- if (!is.null(payload$datasets)) payload$datasets else payload
    if (length(ds_list) > 0) {
      first_item <- ds_list[[1]]
      ds_id <- if (is.list(first_item)) {
        if (!is.null(first_item$datasetId)) first_item$datasetId else first_item$id
      } else {
        first_item
      }
      if (!is.null(ds_id)) {
        user_id <- get_user_id(ds_id)
      }
    }
  }
  
  if (is.null(user_id) || !nzchar(user_id)) {
    user_id <- "user"
  }

  if (kind %in% c("skip", "redo") && !is.null(payload$datasetIds)) {
    for (did in payload$datasetIds) {
      terminate_jobs_for_dataset(did, user_id)
    }
  }

  job_id   <- paste0(kind, "job_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  jobs_dir <- if (nzchar(user_id) && user_id != "user") base::sprintf("tmp/user_sessions/%s/jobs", user_id) else "tmp/jobs"
  base::dir.create(jobs_dir, showWarnings = FALSE, recursive = TRUE)
  in_path  <- file.path(jobs_dir, paste0(job_id, "_in.rds"))
  out_path <- file.path(jobs_dir, paste0(job_id, "_out.rds"))
  log_path <- file.path(jobs_dir, paste0(job_id, ".log"))

  registry_entry <- list(out = out_path, log = log_path, progress = NULL, kind = kind,
                         user_id = user_id, started = Sys.time())
  if (identical(kind, "meta")) {
    first_obj <- payload[[1]]
    method_val    <- first_obj$method
    pvalue_method <- first_obj$pvalueMethod
    eff_model     <- first_obj$effectSizeModel
    votes         <- as.numeric(first_obj$votes)
    pval_thresh   <- if (!is.null(first_obj$pValueThreshold)) as.numeric(first_obj$pValueThreshold) else 0.05
    logfc_thresh  <- if (!is.null(first_obj$logFcThreshold)) as.numeric(first_obj$logFcThreshold) else 1.0

    datasets <- lapply(payload, function(d) {
      sync_dataset_metadata(d, module = "de")
      list(
        id = d$datasetId %||% d$id, name = d$name, parsedData = d$parsedData, columns = d$columns,
        parentModule = d$parentModule, parentDatasetId = d$parentDatasetId, isInline = d$isInline,
        de_referenceGroup = d$de_referenceGroup, de_comparisonGroup = d$de_comparisonGroup
      )
    })

    datasets_by_class <- list()
    for (d in datasets) {
      ds_id <- if (!is.null(d$id)) d$id else d$datasetId
      dclass <- get_dataset_data_class(ds_id)
      if (dclass %in% c("transcriptomics", "proteomics")) {
        if (is.null(datasets_by_class[[dclass]])) datasets_by_class[[dclass]] <- list()
        datasets_by_class[[dclass]][[length(datasets_by_class[[dclass]]) + 1]] <- d
      }
    }

    first_ds_id <- if (length(datasets) > 0) datasets[[1]]$id else "de"
    module  <- if (!is.null(get_backend_datasets(first_ds_id)$module)) get_backend_datasets(first_ds_id)$module else "de"
    meta_ds_id <- sprintf("%s_%s_meta", user_id, module)
    params <- list(method_val = method_val, pvalue_method = pvalue_method, eff_model = eff_model,
                   votes = votes, pval_thresh = pval_thresh, logfc_thresh = logfc_thresh,
                   meta_ds_id = meta_ds_id, user_id = user_id, module = module)
    saveRDS(list(datasets_by_class = datasets_by_class, params = params), in_path)

    registry_entry$datasets_by_class <- datasets_by_class
    registry_entry$params <- params
    registry_entry$meta_ds_id <- meta_ds_id
    registry_entry$module <- module
  } else if (identical(kind, "de")) {
    first_obj <- payload[[1]]
    fallback_method <- if (!is.null(first_obj$method)) first_obj$method else "deseq2"
    pval_thresh <- if (!is.null(first_obj$pValueThreshold)) as.numeric(first_obj$pValueThreshold) else 0.05
    logfc_thresh <- if (!is.null(first_obj$logFcThreshold)) as.numeric(first_obj$logFcThreshold) else 1.0
    adjust_method <- if (!is.null(first_obj$adjustMethod)) first_obj$adjustMethod else "BH"

    datasets <- lapply(payload, function(d) {
      ds_id <- d$datasetId %||% d$id
      sync_dataset_metadata(d, module = "de")
      base_id <- get_base_id(ds_id)
      meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
      meta_info <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL
      data_type <- if (!is.null(d$dataType) && nzchar(d$dataType)) d$dataType else (if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts")
      is_norm <- if (!is.null(d$isNormalized)) as_bool(d$isNormalized) else (if (!is.null(meta_info$isNormalized)) isTRUE(meta_info$isNormalized) else FALSE)
      latest_de <- get_latest_de_stack(ds_id)
      if (is.null(latest_de)) {
        parsed_expr <- get_backend_dataset(ds_id, original = TRUE)
        if (!is.null(parsed_expr) && !is.null(parsed_expr$expr)) {
          latest_de <- push_de_stack(ds_id, parsed_expr$expr, step_name = "de", metadata = list(dataType = data_type, isNormalized = is_norm))
        }
      }
      list(
        datasetId = ds_id,
        name = d$name,
        dataType = data_type,
        isNormalized = is_norm,
        method = if (!is.null(d$method)) d$method else fallback_method,
        parentModule = d$parentModule,
        parentDatasetId = d$parentDatasetId,
        isInline = d$isInline,
        pValueThreshold = d$pValueThreshold,
        logFcThreshold = d$logFcThreshold,
        adjustMethod = d$adjustMethod,
        referenceGroup = d$referenceGroup,
        comparisonGroup = d$comparisonGroup
      )
    })

    params <- list(fallback_method = fallback_method, pval_thresh = pval_thresh,
                   logfc_thresh = logfc_thresh, adjust_method = adjust_method)
    saveRDS(list(datasets = datasets, params = params), in_path)

    registry_entry$datasets <- datasets
    registry_entry$params <- params
  } else if (identical(kind, "upload")) {
    datasets_list <- if (!is.null(payload$datasets)) payload$datasets else payload
    saveRDS(list(datasets_list = datasets_list), in_path)
    registry_entry$datasets_list <- datasets_list
  } else if (identical(kind, "annotation")) {
    first_obj <- payload[[1]]
    strategy <- if (!is.null(first_obj$strategy)) first_obj$strategy else "keep-first"
    source <- "ensembl"
    organisms <- if (!is.null(first_obj$organism)) first_obj$organism else "Human (Homo sapiens)"
    biotypes <- if (!is.null(first_obj$biotype)) first_obj$biotype else "Protein-coding"
    
    datasets <- lapply(payload, function(d) {
      list(
        datasetId = d$datasetId,
        name = d$name,
        dataType = d$dataType,
        strategy = d$strategy,
        organism = d$organism,
        biotype = d$biotype,
        microarrayPlatformId = if (!is.null(d$microarrayPlatformId)) d$microarrayPlatformId else NULL,
        platformFamily = if (!is.null(d$platformFamily)) d$platformFamily else NULL
      )
    })
    saveRDS(list(strategy = strategy, source = source, organisms = organisms, biotypes = biotypes, datasets = datasets), in_path)
    
    registry_entry$datasets <- datasets
    registry_entry$strategy <- strategy
    registry_entry$organisms <- organisms
    registry_entry$biotypes <- biotypes
  } else if (identical(kind, "processing")) {
    saveRDS(list(payload = payload), in_path)
    registry_entry$payload <- payload
  } else if (identical(kind, "normalization")) {
    first_obj <- payload[[1]]
    method_val <- if (!is.null(first_obj$method)) first_obj$method else "tmm"
    transform_type <- if (!is.null(first_obj$transformationType)) first_obj$transformationType else {
      log_val <- if (!is.null(first_obj$logTransform)) isTRUE(first_obj$logTransform) else TRUE
      if (log_val) "log2" else "none"
    }
    prior_count <- if (!is.null(first_obj$priorCount)) as.numeric(first_obj$priorCount) else 0.5
    
    datasets <- lapply(payload, function(d) {
      list(
        id = d$datasetId,
        name = d$name,
        dataType = d$dataType,
        parsedData = d$parsedData,
        columns = d$columns,
        method = d$method,
        logTransform = d$logTransform,
        transformationType = d$transformationType,
        priorCount = d$priorCount
      )
    })
    saveRDS(list(datasets = datasets, method_val = method_val, transform_type = transform_type, prior_count = prior_count), in_path)
    
    registry_entry$datasets <- datasets
    registry_entry$method_val <- method_val
    registry_entry$transform_type <- transform_type
    registry_entry$prior_count <- prior_count
  } else if (identical(kind, "batch")) {
    first_obj <- payload[[1]]
    method_val <- if (!is.null(first_obj$method)) first_obj$method else "combat_seq"
    method_others <- if (!is.null(first_obj$methodOthers)) first_obj$methodOthers else "combat"
    
    datasets <- lapply(payload, function(d) {
      ds_id <- d$datasetId %||% d$id
      sync_dataset_metadata(d, module = "dp")
      
      clin_meta_path <- get_clin_metadata_path(ds_id)
      saved_sample_id_col <- NULL
      saved_group_col <- NULL
      saved_batch_col <- NULL
      if (file.exists(clin_meta_path)) {
        c_meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
        if (!is.null(c_meta)) {
          saved_sample_id_col <- c_meta$sampleIdCol
          saved_group_col <- c_meta$groupCol
          saved_batch_col <- c_meta$batchCol
        }
      }
      list(
        id = ds_id,
        name = d$name,
        dataType = d$dataType,
        parsedData = d$parsedData,
        columns = d$columns,
        clinicalParsedData = d$clinicalParsedData,
        clinicalColumns = d$clinicalColumns,
        clinicalSampleIdCol = d$clinicalSampleIdCol %||% saved_sample_id_col %||% "",
        clinicalGroupCol = d$clinicalGroupCol %||% saved_group_col %||% "",
        clinicalBatchCol = d$clinicalBatchCol %||% saved_batch_col %||% "",
        clinicalOtherCovariates = d$clinicalOtherCovariates %||% list(),
        method = d$method
      )
    })
    saveRDS(list(method_val = method_val, method_others = method_others, datasets = datasets), in_path)
    
    registry_entry$datasets <- datasets
    registry_entry$method_val <- method_val
    registry_entry$method_others <- method_others
  } else if (identical(kind, "pca")) {
    datasets <- if (!is.null(payload$datasets)) payload$datasets else payload
    saveRDS(list(datasets = datasets), in_path)
    registry_entry$datasets <- datasets
  } else if (identical(kind, "ea")) {
    if (is.list(payload)) {
      if (!is.null(payload$datasets) && is.list(payload$datasets)) {
        for (d in payload$datasets) sync_dataset_metadata(d, module = "ea")
      } else if (!is.null(payload$datasetId)) {
        sync_dataset_metadata(list(id = payload$datasetId), module = "ea")
      }
    }
    saveRDS(list(config = payload), in_path)
    registry_entry$config <- payload
  } else if (kind %in% c("skip", "redo")) {
    saveRDS(list(step = payload$step, datasetIds = payload$datasetIds), in_path)
  } else if (kind %in% c("inline_de", "inline_de_meta", "inline_ea")) {
    if (is.list(payload)) {
      for (d in payload) {
        ds_id_str <- d$id %||% d$datasetId %||% ""
        p_mod <- d$parentModule %||% (if (grepl("_dp", ds_id_str)) "dp" else if (grepl("_de", ds_id_str)) "de" else if (identical(kind, "inline_ea")) "dp" else "de")
        d_mod <- if (identical(kind, "inline_ea")) "ea" else p_mod
        d$parentModule <- p_mod
        d$module <- d_mod
        d$isInline <- TRUE
        sync_dataset_metadata(d, module = d_mod)
      }
    }
    saveRDS(list(payload = payload), in_path)
  } else if (identical(kind, "supplement_clinical")) {
    saveRDS(list(payload = payload), in_path)
    registry_entry$payload <- payload
  } else if (kind %in% c("refit", "fs_refit")) {
    refit_job_id     <- payload$jobId
    selection_method <- payload$selectionMethod
    percentage_val   <- if (!is.null(payload$percentageValue) && !identical(payload$percentageValue, "") && !identical(payload$percentageValue, "null")) as.numeric(payload$percentageValue) else NULL
    max_features_val <- if (!is.null(payload$maxFeaturesValue) && !identical(payload$maxFeaturesValue, "") && !identical(payload$maxFeaturesValue, "null")) as.numeric(payload$maxFeaturesValue) else NULL
    
    refit_in_path  <- ""
    refit_out_path <- ""
    standard_in  <- file.path("tmp/jobs", paste0(refit_job_id, "_in.rds"))
    standard_out <- file.path("tmp/jobs", paste0(refit_job_id, "_out.rds"))
    if (file.exists(standard_in)) {
      refit_in_path  <- standard_in
      refit_out_path <- standard_out
    } else {
      user_dirs <- list.dirs("tmp/user_sessions", recursive = FALSE)
      for (ud in user_dirs) {
        path_check <- file.path(ud, "jobs", paste0(refit_job_id, "_in.rds"))
        if (file.exists(path_check)) {
          refit_in_path  <- path_check
          refit_out_path <- file.path(ud, "jobs", paste0(refit_job_id, "_out.rds"))
          break
        }
      }
    }
    
    if (!nzchar(refit_in_path) || !file.exists(refit_in_path)) {
      return(json_response(list(status = "error", message = sprintf("Job input not found for ID: %s", refit_job_id)), 404))
    }
    
    orig_input_data <- readRDS(refit_in_path)
    first_ds_id <- if (length(orig_input_data$datasets) > 0) orig_input_data$datasets[[1]]$id else "fs"
    module_val  <- get_backend_datasets(first_ds_id)$module %||% "fs"
    
    refit_payload <- list(
      jobId = refit_job_id,
      input_data = orig_input_data,
      orig_out_path = refit_out_path,
      selection_method = selection_method,
      percentage_val = percentage_val,
      max_features_val = max_features_val,
      user_id = user_id,
      module = module_val
    )
    saveRDS(refit_payload, in_path)
    registry_entry$refit_payload <- refit_payload
  } else if (identical(kind, "cv")) {
    if (is.list(payload) && !is.null(payload$datasets)) {
      raw_ds_list <- payload$datasets
      models <- payload$models
      cv_method <- payload$cvMethod %||% payload$method %||% "k_fold"
      folds <- if (!is.null(payload$folds)) as.numeric(payload$folds) else 5
      multi_dataset_mode <- payload$multiDatasetMode %||% "combine"
    } else {
      raw_ds_list <- payload
      first_obj <- if (length(raw_ds_list) > 0) raw_ds_list[[1]] else list()
      models <- first_obj$models %||% c("logistic", "svm", "randomforest")
      cv_method <- first_obj$method %||% "k_fold"
      folds <- if (!is.null(first_obj$folds)) as.numeric(first_obj$folds) else 5
      multi_dataset_mode <- "combine"
    }
    
    datasets <- lapply(raw_ds_list, function(d) {
      ds_id <- d$datasetId %||% d$id
      sync_dataset_metadata(d, module = "fs")
      
      clin_meta_path <- get_clin_metadata_path(ds_id)
      saved_group_col <- NULL
      saved_sample_id_col <- NULL
      if (file.exists(clin_meta_path)) {
        c_meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
        if (!is.null(c_meta)) {
          saved_group_col <- c_meta$groupCol
          saved_sample_id_col <- c_meta$sampleIdCol
        }
      }
      
      fs_meta_path <- get_session_path(get_base_id(ds_id), "%s_fs_meta.rds")
      saved_fs_meta <- if (file.exists(fs_meta_path)) tryCatch(readRDS(fs_meta_path), error = function(e) NULL) else NULL
      
      list(
        id = ds_id,
        name = d$name %||% ds_id,
        dataType = d$dataType %||% d$fs_dataType,
        parentModule = d$parentModule,
        parentDatasetId = d$parentDatasetId,
        isInline = d$isInline,
        parsedData = d$parsedData,
        columns = d$columns,
        clinicalParsedData = d$clinicalParsedData,
        clinicalColumns = d$clinicalColumns,
        clinicalGroupCol = d$clinicalGroupCol %||% saved_group_col %||% "",
        clinicalSampleIdCol = d$clinicalSampleIdCol %||% saved_sample_id_col %||% "",
        positiveClass = d$positiveClass %||% d$fs_positiveClass %||% c_meta$positiveClass %||% c_meta$fs_positiveClass %||% "",
        negativeClass = d$negativeClass %||% d$fs_negativeClass %||% c_meta$negativeClass %||% c_meta$fs_negativeClass %||% "",
        fs_positiveClass = d$positiveClass %||% d$fs_positiveClass %||% c_meta$positiveClass %||% c_meta$fs_positiveClass %||% "",
        fs_negativeClass = d$negativeClass %||% d$fs_negativeClass %||% c_meta$negativeClass %||% c_meta$fs_negativeClass %||% "",
        fs_trainRatio = d$fs_trainRatio %||% d$trainRatio %||% saved_fs_meta$trainRatio %||% 0.7,
        fs_datasetPurpose = d$fs_datasetPurpose %||% d$datasetPurpose %||% saved_fs_meta$datasetPurpose %||% "train",
        fs_validationStrategy = d$fs_validationStrategy %||% d$validationStrategy %||% saved_fs_meta$validationStrategy %||% "train-test-split",
        fs_isInternalValidation = if (!is.null(d$fs_isInternalValidation)) isTRUE(d$fs_isInternalValidation) else if (!is.null(saved_fs_meta$isInternalValidation)) isTRUE(saved_fs_meta$isInternalValidation) else TRUE
      )
    })

    for (i in seq_along(datasets)) {
      dataset_id <- datasets[[i]]$id
      if (is.null(datasets[[i]]$parsedData) || length(datasets[[i]]$parsedData) == 0) {
        dp_file <- sprintf("tmp/%s_dp_results.csv", dataset_id)
        if (!file.exists(dp_file)) dp_file <- "tmp/dp_results.csv"
        
        if (file.exists(dp_file)) {
          df <- read.csv(dp_file, stringsAsFactors = FALSE, check.names = FALSE)
          datasets[[i]]$columns <- colnames(df)
          datasets[[i]]$parsedData <- lapply(1:nrow(df), function(r) unname(as.character(unlist(df[r, ]))))
          if (any(grepl("^Sample", colnames(df), ignore.case = TRUE))) {
            datasets[[i]]$fs_featuresOrientation <- "rows"
          } else {
            datasets[[i]]$fs_featuresOrientation <- "headers"
          }
        }
      }
    }
    
    first_ds_id <- if (length(datasets) > 0) datasets[[1]]$id else "fs"
    module_val  <- get_backend_datasets(first_ds_id)$module %||% "fs"

    cv_payload <- list(
      models = models,
      cv_method = cv_method,
      folds = folds,
      datasets = datasets,
      multi_dataset_mode = multi_dataset_mode,
      user_id = user_id,
      module = module_val
    )
    saveRDS(cv_payload, in_path)
    registry_entry$cv_payload <- cv_payload
  } else if (identical(kind, "testing")) {
    raw_ds_list <- if (is.list(payload) && !is.null(payload$datasets)) payload$datasets else payload
    first_obj <- if (is.list(raw_ds_list) && length(raw_ds_list) > 0) raw_ds_list[[1]] else list()
    models <- if (!is.null(payload$models)) payload$models else (first_obj$models %||% c("logistic", "svm", "randomforest"))
    multi_dataset_mode <- payload$multiDatasetMode %||% "combine"
    
    datasets <- lapply(raw_ds_list, function(d) {
      ds_id <- d$datasetId %||% d$id
      sync_dataset_metadata(d, module = "fs")
      
      clin_meta_path <- get_clin_metadata_path(ds_id)
      saved_group_col <- NULL
      saved_sample_id_col <- NULL
      c_meta <- NULL
      if (file.exists(clin_meta_path)) {
        c_meta <- tryCatch(readRDS(clin_meta_path), error = function(e) NULL)
        if (!is.null(c_meta)) {
          saved_group_col <- c_meta$groupCol
          saved_sample_id_col <- c_meta$sampleIdCol
        }
      }
      
      fs_meta_path <- get_session_path(get_base_id(ds_id), "%s_fs_meta.rds")
      saved_fs_meta <- if (file.exists(fs_meta_path)) tryCatch(readRDS(fs_meta_path), error = function(e) NULL) else NULL
      
      list(
        id = ds_id,
        name = d$name %||% ds_id,
        dataType = d$dataType %||% d$fs_dataType,
        parentModule = d$parentModule,
        parentDatasetId = d$parentDatasetId,
        isInline = d$isInline,
        parsedData = d$parsedData,
        columns = d$columns,
        clinicalParsedData = d$clinicalParsedData,
        clinicalColumns = d$clinicalColumns,
        clinicalGroupCol = d$clinicalGroupCol %||% saved_group_col %||% "",
        clinicalSampleIdCol = d$clinicalSampleIdCol %||% saved_sample_id_col %||% "",
        positiveClass = d$positiveClass %||% d$fs_positiveClass %||% c_meta$positiveClass %||% c_meta$fs_positiveClass %||% "",
        negativeClass = d$negativeClass %||% d$fs_negativeClass %||% c_meta$negativeClass %||% c_meta$fs_negativeClass %||% "",
        fs_positiveClass = d$positiveClass %||% d$fs_positiveClass %||% c_meta$positiveClass %||% c_meta$fs_positiveClass %||% "",
        fs_negativeClass = d$negativeClass %||% d$fs_negativeClass %||% c_meta$negativeClass %||% c_meta$fs_negativeClass %||% "",
        fs_trainRatio = d$fs_trainRatio %||% d$trainRatio %||% saved_fs_meta$trainRatio %||% 0.7,
        fs_datasetPurpose = d$fs_datasetPurpose %||% d$datasetPurpose %||% saved_fs_meta$datasetPurpose %||% "train",
        fs_validationStrategy = d$fs_validationStrategy %||% d$validationStrategy %||% saved_fs_meta$validationStrategy %||% "train-test-split",
        fs_isInternalValidation = if (!is.null(d$fs_isInternalValidation)) isTRUE(d$fs_isInternalValidation) else if (!is.null(saved_fs_meta$isInternalValidation)) isTRUE(saved_fs_meta$isInternalValidation) else TRUE
      )
    })

    for (i in seq_along(datasets)) {
      dataset_id <- datasets[[i]]$id
      if (is.null(datasets[[i]]$parsedData) || length(datasets[[i]]$parsedData) == 0) {
        dp_file <- sprintf("tmp/%s_dp_results.csv", dataset_id)
        if (!file.exists(dp_file)) dp_file <- "tmp/dp_results.csv"
        
        if (file.exists(dp_file)) {
          df <- read.csv(dp_file, stringsAsFactors = FALSE, check.names = FALSE)
          datasets[[i]]$columns <- colnames(df)
          datasets[[i]]$parsedData <- lapply(1:nrow(df), function(r) unname(as.character(unlist(df[r, ]))))
          if (any(grepl("^Sample", colnames(df), ignore.case = TRUE))) {
            datasets[[i]]$fs_featuresOrientation <- "rows"
          } else {
            datasets[[i]]$fs_featuresOrientation <- "headers"
          }
        }
      }
    }
    
    first_ds_id <- if (length(datasets) > 0) datasets[[1]]$id else "fs"
    module_val  <- get_backend_datasets(first_ds_id)$module %||% "fs"

    testing_payload <- list(
      models = models,
      datasets = datasets,
      multi_dataset_mode = multi_dataset_mode,
      user_id = user_id,
      module = module_val
    )
    saveRDS(testing_payload, in_path)
    registry_entry$testing_payload <- testing_payload
  }

  if (requireNamespace("callr", quietly = TRUE)) {
    backend_dir <- getwd()
    eff_c <- get_effective_cores()
    worker_env <- get_worker_env(eff_c)
    p <- callr::r_bg(
      func = function(in_path, out_path, backend_dir, kind) {
        setwd(backend_dir)
        Sys.unsetenv("PORT")
        Sys.setenv(R_PARALLEL_PORT = "random")
        source("shared_utils.R")
        setup_threading_environment()
        source("processing.R")
        source("report_finalize.R")
        # Source heavy files only when the job kind requires them
        if (kind %in% c("de", "inline_de", "meta", "inline_de_meta")) source("de_analysis.R")
        if (kind %in% c("ea", "inline_ea", "annotation")) source("enrichment.R")
        if (kind %in% c("fs", "inline_fs", "cv", "testing", "refit", "fs_refit")) source("feature_selection.R")
        input <- readRDS(in_path)
        res <- tryCatch({
          if (identical(kind, "meta")) {
            raw_res <- run_meta_compute(input$datasets_by_class, input$params$method_val,
                             input$params$pvalue_method, input$params$eff_model,
                             input$params$votes, input$params$pval_thresh, input$params$logfc_thresh)
            finalize_meta_results(raw_res, input$datasets_by_class, input$params$method_val,
                                  input$params$pvalue_method, input$params$eff_model,
                                  input$params$votes, input$params$pval_thresh, input$params$logfc_thresh,
                                  input$params$meta_ds_id, input$params$user_id, input$params$module)
          } else if (identical(kind, "de")) {
            raw_res <- run_de_analysis(method = input$params$fallback_method,
                            pval_thresh = input$params$pval_thresh,
                            logfc_thresh = input$params$logfc_thresh,
                            adjust_method = input$params$adjust_method,
                            datasets = input$datasets)
            finalize_de_results(raw_res, input$datasets, input$params$pval_thresh, input$params$logfc_thresh,
                                input$params$adjust_method, input$params$fallback_method)
          } else if (identical(kind, "upload")) {
            raw_res <- run_upload_datasets(input$datasets_list)
            finalize_upload_datasets(raw_res)
          } else if (identical(kind, "annotation")) {
            raw_res <- annotate_genes(input$strategy, input$source, input$organisms, input$biotypes, input$datasets)
            finalize_annotation(raw_res, input$datasets, input$strategy, input$organisms, input$biotypes)
          } else if (identical(kind, "processing")) {
            raw_res <- process_datasets(input$payload)
            finalize_processing(raw_res, input$payload)
          } else if (identical(kind, "normalization")) {
            raw_res <- normalize_datasets(input$datasets, input$method_val, input$transform_type, input$prior_count)
            finalize_normalization(raw_res, input$datasets, input$method_val, input$transform_type, input$prior_count)
          } else if (identical(kind, "batch")) {
            raw_res <- correct_batch_effects(input$method_val, input$method_others, input$datasets)
            finalize_batch_correction(raw_res, input$datasets, input$method_val, input$method_others)
          } else if (identical(kind, "pca")) {
            raw_res <- compute_pca(input$datasets)
            finalize_pca(raw_res)
          } else if (identical(kind, "ea")) {
            raw_res <- run_ea_compute(input$config)
            finalize_ea(raw_res, input$config)
          } else if (identical(kind, "skip")) {
            run_skip_step(input$step, input$datasetIds)
          } else if (identical(kind, "redo")) {
            run_redo_step(input$step, input$datasetIds)
          } else if (identical(kind, "inline_de")) {
            run_inline_de_analysis(input$payload)
          } else if (identical(kind, "inline_de_meta")) {
            run_inline_de_meta(input$payload)
          } else if (identical(kind, "inline_ea")) {
            run_inline_ea_compute(input$payload)
          } else if (identical(kind, "supplement_clinical")) {
            run_supplement_clinical(input$payload)
          } else if (kind %in% c("refit", "fs_refit")) {
            raw_res <- run_refit_features(
              datasets = input$input_data$datasets,
              models = input$input_data$models,
              split_ratio = input$input_data$split_ratio,
              parameters = input$input_data$parameters,
              selection_method = input$selection_method,
              percentage_val = input$percentage_val,
              max_features_val = input$max_features_val
            )
            finalize_fs_refit(raw_res, input$orig_out_path, input$input_data, input$selection_method, input$percentage_val, input$max_features_val, input$user_id, input$module)
          } else if (identical(kind, "cv")) {
            raw_res <- run_cross_validation(
              models = input$models,
              cv_method = input$cv_method,
              folds = input$folds,
              datasets = input$datasets,
              multi_dataset_mode = input$multi_dataset_mode
            )
            finalize_cv(raw_res, input$datasets, input$cv_method, input$folds, input$user_id, input$module)
          } else if (identical(kind, "testing")) {
            raw_res <- run_testing(
              models = input$models,
              datasets = input$datasets,
              multi_dataset_mode = input$multi_dataset_mode
            )
            finalize_testing(raw_res, input$datasets, input$models, input$user_id, input$module)
          } else {
            stop("Unknown compute-job kind inside worker")
          }
        },
        error = function(e) list(.error = conditionMessage(e))
        )
        saveRDS(res, out_path)
      },
      args = list(in_path, out_path, backend_dir, kind),
      env = worker_env,
      stdout = log_path, stderr = "2>&1", supervise = TRUE
    )
    registry_entry$process <- p
    assign(job_id, registry_entry, envir = .fs_jobs)
    cat(sprintf("[COMPUTE-JOB] started %s (kind=%s) in %s\n", job_id, kind, jobs_dir))
    return(json_response(list(jobId = job_id, status = "running")))
  } else {
    # synchronous fallback
    res <- tryCatch({
      if (identical(kind, "meta")) {
        raw_res <- run_meta_compute(datasets_by_class, method_val, pvalue_method, eff_model, votes, pval_thresh, logfc_thresh)
        finalize_meta_results(raw_res, datasets_by_class, method_val, pvalue_method, eff_model, votes, pval_thresh, logfc_thresh, meta_ds_id, user_id, module)
      } else if (identical(kind, "de")) {
        raw_res <- run_de_analysis(fallback_method, pval_thresh, logfc_thresh, adjust_method, datasets)
        finalize_de_results(raw_res, datasets, pval_thresh, logfc_thresh, adjust_method, fallback_method)
      } else if (identical(kind, "upload")) {
        raw_res <- run_upload_datasets(datasets_list)
        finalize_upload_datasets(raw_res)
      } else if (identical(kind, "annotation")) {
        raw_res <- annotate_genes(strategy, source, organisms, biotypes, datasets)
        finalize_annotation(raw_res, datasets, strategy, organisms, biotypes)
      } else if (identical(kind, "processing")) {
        raw_res <- process_datasets(payload)
        finalize_processing(raw_res, payload)
      } else if (identical(kind, "normalization")) {
        raw_res <- normalize_datasets(datasets, method_val, transform_type, prior_count)
        finalize_normalization(raw_res, datasets, method_val, transform_type, prior_count)
      } else if (identical(kind, "batch")) {
        raw_res <- correct_batch_effects(method_val, method_others, datasets)
        finalize_batch_correction(raw_res, datasets, method_val, method_others)
      } else if (identical(kind, "pca")) {
        raw_res <- compute_pca(datasets)
        finalize_pca(raw_res)
      } else if (identical(kind, "ea")) {
        raw_res <- run_ea_compute(config)
        finalize_ea(raw_res, config)
      } else if (identical(kind, "skip")) {
        run_skip_step(payload$step, payload$datasetIds)
      } else if (identical(kind, "redo")) {
        run_redo_step(payload$step, payload$datasetIds)
      } else if (identical(kind, "inline_de")) {
        run_inline_de_analysis(payload)
      } else if (identical(kind, "inline_de_meta")) {
        run_inline_de_meta(payload)
      } else if (identical(kind, "inline_ea")) {
        run_inline_ea_compute(payload)
      } else if (identical(kind, "supplement_clinical")) {
        run_supplement_clinical(payload)
      } else if (kind %in% c("refit", "fs_refit")) {
        raw_res <- run_refit_features(
          datasets = orig_input_data$datasets,
          models = orig_input_data$models,
          split_ratio = orig_input_data$split_ratio,
          parameters = orig_input_data$parameters,
          selection_method = selection_method,
          percentage_val = percentage_val,
          max_features_val = max_features_val
        )
        finalize_fs_refit(raw_res, refit_out_path, orig_input_data, selection_method, percentage_val, max_features_val, user_id, module_val)
      } else if (identical(kind, "cv")) {
        raw_res <- run_cross_validation(
          models = models,
          cv_method = cv_method,
          folds = folds,
          datasets = datasets,
          multi_dataset_mode = multi_dataset_mode
        )
        finalize_cv(raw_res, datasets, cv_method, folds, user_id, module_val)
      } else if (identical(kind, "testing")) {
        raw_res <- run_testing(
          models = models,
          datasets = datasets,
          multi_dataset_mode = multi_dataset_mode
        )
        finalize_testing(raw_res, datasets, models, user_id, module_val)
      } else {
        stop("Unknown compute-job kind inside worker")
      }
    }, error = function(e) list(.error = conditionMessage(e)))
    saveRDS(res, out_path)
    registry_entry$process <- NULL
    assign(job_id, registry_entry, envir = .fs_jobs)
    return(json_response(list(jobId = job_id, status = "done")))
  }
}

# Main routing and endpoint handling
api_router <- function(req) {
  # Handle CORS preflight (OPTIONS)
  if (req$REQUEST_METHOD == "OPTIONS") {
    return(httpResponse(
      status = 204,
      content_type = "text/plain",
      headers = list(
        "Access-Control-Allow-Origin" = "*",
        "Access-Control-Allow-Methods" = "POST, GET, OPTIONS, DELETE",
        "Access-Control-Allow-Headers" = "Content-Type, Authorization"
      ),
      content = ""
    ))
  }

  path <- req$PATH_INFO
  method <- req$REQUEST_METHOD

  # Parse POST body if present
  body_data <- list()
  if (method == "POST" && !is.null(req$rook.input)) {
    body_bytes <- req$rook.input$read()
    if (length(body_bytes) > 0) {
      body_str <- rawToChar(body_bytes)
      tryCatch({
        body_data <- jsonlite::fromJSON(body_str, simplifyVector = TRUE, simplifyDataFrame = FALSE)
      }, error = function(e) {
        cat("[ERROR] Failed to parse JSON body:", e$message, "\n")
      })
    }
  }

  # Wrap execution in tryCatch to return detailed error messages to client
  response <- tryCatch({
    # ----------------------------------------------------
    # Route 0: ANY /api/shutdown (Cleanly exits the server and removes all tmp files)
    # ----------------------------------------------------
    if (path == "/shutdown") {
      cat("[LOG] Shutdown requested via", method, ". Cleaning up tmp files...\n")
      tryCatch({
        if (dir.exists("tmp")) {
          unlink("tmp", recursive = TRUE, force = TRUE)
          cat("[LOG] Successfully removed all tmp files of all users.\n")
        }
      }, error = function(e) {
        cat("[ERROR] Failed to clean up tmp files during shutdown:", e$message, "\n")
      })
      
      # Exit R process after sending the response
      later::later(function() {
        cat("[LOG] Exiting R process.\n")
        q(save = "no")
      }, 0.5)
      
      return(httpResponse(
        status = 200,
        content_type = "application/json",
        headers = list(
          "Access-Control-Allow-Origin" = "*",
          "Access-Control-Allow-Methods" = "POST, GET, OPTIONS",
          "Access-Control-Allow-Headers" = "Content-Type"
        ),
        content = '{"status":"ok","message":"Server is shutting down..."}'
      ))
    }

    # ----------------------------------------------------
    # Route 1: GET /api/export-file
    # ----------------------------------------------------
    if (method == "GET" && grepl("^/export-file", path)) {
      query_list <- parseQueryString(req$QUERY_STRING)
      ext     <- tolower(query_list$ext %||% "csv")
      user_id <- query_list$userId %||% ""
      file_type <- query_list$type
      ds_id     <- query_list$dsId
      model     <- query_list$model

      if (!is.null(file_type) && !is.null(ds_id)) {
        cat(sprintf("[LOG] API /export-file: type=%s, dsId=%s, model=%s, ext=%s\n",
                    file_type, ds_id, model %||% "", ext))
        res <- resolve_export_file(file_type, ds_id, model, ext, user_id)
        if (is.null(res) || is.null(res$file_path) || !file.exists(res$file_path)) {
          return(httpResponse(
            status = 404,
            content_type = "application/json",
            headers = list("Access-Control-Allow-Origin" = "*"),
            content = sprintf('{"status":"error","message":"The %s file is not available yet. Run the analysis step first."}', file_type)
          ))
        }
        con <- file(res$file_path, "rb")
        content_bytes <- readBin(con, "raw", n = file.info(res$file_path)$size)
        close(con)
        return(httpResponse(
          status = 200,
          content_type = res$content_type,
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Content-Disposition" = sprintf('attachment; filename="%s"', res$clean_filename)
          ),
          content = content_bytes
        ))
      }
      return(httpResponse(
        status = 400,
        content_type = "application/json",
        headers = list("Access-Control-Allow-Origin" = "*"),
        content = '{"status":"error","message":"Missing type or dsId parameter."}'
      ))
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

    # ----------------------------------------------------
    # Route 1b: POST /api/export-report (async PDF / report export)
    # ----------------------------------------------------
    if (method == "POST" && path == "/export-report") {
      user_id    <- body_data$userId
      module_val <- body_data$module
      format_val <- body_data$format %||% "pdf"

      if (is_empty_str(user_id)) {
        return(json_response(list(status = "error", message = "Missing userId"), 400))
      }

      if (!is_empty_str(module_val)) {
        report_fname <- sprintf("analysis_report_%s.md", tolower(safe_str(module_val)))
      } else {
        report_fname <- "analysis_report.md"
      }

      report_path <- file.path("tmp/user_sessions", user_id, report_fname)
      if (!file.exists(report_path)) {
        return(json_response(list(status = "error", message = "No analysis report found yet for this module. Complete at least one analysis step first."), 404))
      }

      if (tolower(format_val) == "pdf") {
        if (!requireNamespace("callr", quietly = TRUE)) {
          pdf_path <- tempfile(fileext = ".pdf")
          success <- convert_md_to_pdf(report_path, pdf_path)
          if (!success || !file.exists(pdf_path)) {
            return(json_response(list(status = "error", message = "Failed to compile PDF report."), 500))
          }
          return(json_response(list(status = "done", downloadUrl = sprintf("/api/export-report?userId=%s&module=%s&format=pdf", user_id, module_val %||% ""))))
        }

        fs_jobs_cleanup()
        job_id   <- paste0("reportjob_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
        jobs_dir <- sprintf("tmp/user_sessions/%s/jobs", user_id)
        dir.create(jobs_dir, showWarnings = FALSE, recursive = TRUE)
        pdf_path <- file.path(jobs_dir, paste0(job_id, ".pdf"))
        out_path <- file.path(jobs_dir, paste0(job_id, "_out.rds"))
        log_path <- file.path(jobs_dir, paste0(job_id, ".log"))
        backend_dir <- getwd()

        p <- callr::r_bg(
          func = function(report_path, pdf_path, out_path, backend_dir, module_val) {
            setwd(backend_dir)
            source("shared_utils.R")
            source("processing.R")
            source("report_finalize.R")
            res <- tryCatch({
              ok <- convert_md_to_pdf(report_path, pdf_path)
              if (!ok || !file.exists(pdf_path) || file.info(pdf_path)$size[1] == 0) {
                list(status = "error", message = "Failed to compile PDF report. Please download the Markdown format instead.")
              } else {
                date_prefix <- format(Sys.Date(), "%y%m%d")
                filename_val <- sprintf("%s_analysis_report.pdf", date_prefix)
                if (!is_empty_str(module_val)) {
                  filename_val <- sprintf("%s_analysis_report_%s.pdf", date_prefix, tolower(safe_str(module_val)))
                }
                list(status = "done", filePath = pdf_path, filename = filename_val)
              }
            }, error = function(e) {
              list(status = "error", message = conditionMessage(e))
            })
            saveRDS(res, out_path)
          },
          args = list(report_path, pdf_path, out_path, backend_dir, module_val),
          env = get_worker_env(1L),
          stdout = log_path, stderr = "2>&1", supervise = TRUE
        )

        assign(job_id, list(
          out = out_path, log = log_path, process = p,
          kind = "report", user_id = user_id, pdf_path = pdf_path,
          module = module_val, started = Sys.time()
        ), envir = .fs_jobs)

        cat(sprintf("[REPORT-JOB] Started background PDF export job %s for user %s\n", job_id, user_id))
        return(json_response(list(jobId = job_id, status = "running")))
      } else {
        date_prefix <- format(Sys.Date(), "%y%m%d")
        filename_val <- sprintf("%s_analysis_report.md", date_prefix)
        if (!is_empty_str(module_val)) {
          filename_val <- sprintf("%s_analysis_report_%s.md", date_prefix, tolower(safe_str(module_val)))
        }
        return(json_response(list(
          status = "done",
          downloadUrl = sprintf("/api/export-report?userId=%s&module=%s&format=md", user_id, module_val %||% ""),
          filename = filename_val
        )))
      }
    }

    # ----------------------------------------------------
    # Route 1c: GET /api/export-report
    # Supports polling by id (GET /api/export-report?id=<jobId>)
    # or direct stream (GET /api/export-report?userId=<userId>&format=...)
    # ----------------------------------------------------
    if (method == "GET" && path == "/export-report") {
      query_list <- parseQueryString(req$QUERY_STRING)
      job_id     <- query_list$id

      # 1. Job polling branch
      if (!is.null(job_id) && nzchar(job_id)) {
        if (!exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
          return(json_response(list(status = "error", message = "Export report job not found or expired."), 404))
        }

        job <- get(job_id, envir = .fs_jobs)

        # Check if job completed
        if (file.exists(job$out)) {
          res <- tryCatch(readRDS(job$out), error = function(e) NULL)
          if (is.null(res)) {
            return(json_response(list(status = "running", jobId = job_id)))
          }
          if (identical(res$status, "error")) {
            fs_job_remove(job_id)
            return(json_response(list(status = "error", message = res$message %||% "PDF compilation failed"), 500))
          }
          if (identical(res$status, "done")) {
            pdf_file <- res$filePath
            if (!file.exists(pdf_file) || file.info(pdf_file)$size == 0) {
              fs_job_remove(job_id)
              return(json_response(list(status = "error", message = "Compiled PDF file missing or empty."), 500))
            }
            con <- file(pdf_file, "rb")
            content_bytes <- readBin(con, "raw", n = file.info(pdf_file)$size)
            close(con)
            unlink(pdf_file)
            unlink(job$out)
            fs_job_remove(job_id)

            return(httpResponse(200, "application/pdf",
              headers = list(
                "Access-Control-Allow-Origin" = "*",
                "Content-Disposition" = sprintf('attachment; filename="%s"', res$filename)
              ),
              content = content_bytes))
          }
        }

        # Check if process is still running
        is_alive <- !is.null(job$process) && isTRUE(tryCatch(job$process$is_alive(), error = function(e) FALSE))
        if (is_alive) {
          return(json_response(list(status = "running", jobId = job_id)))
        }

        # Process exited without writing output
        fs_job_remove(job_id)
        return(json_response(list(status = "error", message = "PDF generation process terminated unexpectedly."), 500))
      }

      # 2. Legacy direct stream branch
      user_id    <- query_list$userId
      module_val <- query_list$module
      format_val <- query_list$format %||% "md"
      
      if (is_empty_str(user_id)) {
        return(httpResponse(400, "application/json",
          headers = list("Access-Control-Allow-Origin" = "*"),
          content = '{"status":"error","message":"Missing userId"}'))
      }
      
      if (!is_empty_str(module_val)) {
        report_fname <- sprintf("analysis_report_%s.md", tolower(safe_str(module_val)))
      } else {
        report_fname <- "analysis_report.md"
      }
      
      report_path <- file.path("tmp/user_sessions", user_id, report_fname)
      if (!file.exists(report_path)) {
        return(httpResponse(404, "application/json",
          headers = list("Access-Control-Allow-Origin" = "*"),
          content = '{"status":"error","message":"No analysis report found yet for this module. Complete at least one analysis step first."}'))
      }
      
      date_prefix <- format(Sys.Date(), "%y%m%d")
      
      if (tolower(format_val) == "pdf") {
        pdf_path <- tempfile(fileext = ".pdf")
        success <- convert_md_to_pdf(report_path, pdf_path)
        if (!success || !file.exists(pdf_path)) {
          return(httpResponse(500, "application/json",
            headers = list("Access-Control-Allow-Origin" = "*"),
            content = '{"status":"error","message":"Failed to compile PDF report. Please download the Markdown format instead."}'))
        }
        
        content_bytes <- readBin(file(pdf_path, "rb"), "raw", n = file.info(pdf_path)$size)
        unlink(pdf_path)
        
        filename_val <- sprintf("%s_analysis_report.pdf", date_prefix)
        if (!is_empty_str(module_val)) {
          filename_val <- sprintf("%s_analysis_report_%s.pdf", date_prefix, tolower(safe_str(module_val)))
        }
        
        return(httpResponse(200, "application/pdf",
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Content-Disposition" = sprintf('attachment; filename="%s"', filename_val)
          ),
          content = content_bytes))
      } else {
        content_bytes <- readBin(file(report_path, "rb"), "raw", n = file.info(report_path)$size)
        filename_val <- sprintf("%s_analysis_report.md", date_prefix)
        if (!is_empty_str(module_val)) {
          filename_val <- sprintf("%s_analysis_report_%s.md", date_prefix, tolower(safe_str(module_val)))
        }
        
        return(httpResponse(200, "text/markdown",
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Content-Disposition" = sprintf('attachment; filename="%s"', filename_val)
          ),
          content = content_bytes))
      }
    }

    # ----------------------------------------------------
    # Route 2a: POST /api/export-job or POST /api/export-file (async export job)
    # Route 2b: GET  /api/export-job?id=<jobId> (poll and download when ready)
    # Route 2c: DELETE /api/export-job?id=<jobId> (cancel export)
    # ----------------------------------------------------
    if (method == "POST" && (path == "/export-job" || path == "/export-file")) {
      cat(sprintf("[LOG] API %s POST called\n", path))
      query_list_ej   <- parseQueryString(req$QUERY_STRING)
      user_id_ej      <- if (!is.null(body_data$userId))     body_data$userId     else (query_list_ej$userId     %||% NULL)
      ds_param_ej     <- if (!is.null(body_data$datasetIds)) body_data$datasetIds else (query_list_ej$datasetIds %||% "")
      module_param_ej <- if (!is.null(body_data$module))     body_data$module     else (query_list_ej$module     %||% "dp")
      file_type_ej    <- if (!is.null(body_data$type))       body_data$type       else (query_list_ej$type       %||% NULL)
      ds_id_ej        <- if (!is.null(body_data$dsId))       body_data$dsId       else (query_list_ej$dsId       %||% NULL)
      model_ej        <- if (!is.null(body_data$model))      body_data$model      else (query_list_ej$model      %||% NULL)
      ext_ej          <- tolower(if (!is.null(body_data$ext)) body_data$ext        else (query_list_ej$ext        %||% "csv"))

      if (is_empty_str(user_id_ej)) {
        return(json_response(list(status = "error",
          message = "Missing session id; cannot export results."), 400))
      }
      export_dir_ej <- file.path("tmp/user_sessions", user_id_ej)
      if (!dir.exists(export_dir_ej)) {
        return(json_response(list(status = "error",
          message = "No results to export yet. Run an analysis first."), 404))
      }

      job_id_ej <- paste0("exportjob_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
      is_single_file <- !is_empty_str(file_type_ej) && !is_empty_str(ds_id_ej)

      if (is_single_file) {
        enqueue_export_job(job_id_ej, list(
          type   = file_type_ej,
          dsId   = ds_id_ej,
          model  = model_ej,
          ext    = ext_ej,
          userId = user_id_ej
        ))
        cat(sprintf("[EXPORT-JOB] Enqueued single-file job %s (type=%s, dsId=%s, model=%s, ext=%s)\n",
                    job_id_ej, file_type_ej, ds_id_ej, model_ej %||% "none", ext_ej))
        return(json_response(list(jobId = job_id_ej, status = "running")))
      } else {
        # ZIP archive compilation in callr background process
        if (!requireNamespace("callr", quietly = TRUE)) {
          return(json_response(list(status = "error",
            message = "callr package not available for ZIP export."), 503))
        }

        fs_jobs_cleanup()
        jobs_dir_ej <- sprintf("tmp/user_sessions/%s/jobs", user_id_ej)
        dir.create(jobs_dir_ej, showWarnings = FALSE, recursive = TRUE)
        out_path_ej <- file.path(jobs_dir_ej, paste0(job_id_ej, "_out.rds"))
        log_path_ej <- file.path(jobs_dir_ej, paste0(job_id_ej, ".log"))
        backend_dir_ej <- getwd()

        p_ej <- callr::r_bg(
          func = function(export_dir, user_id, ds_param, module_param, out_path, backend_dir) {
            setwd(backend_dir)
            source("shared_utils.R")
            source("processing.R")
            source("report_finalize.R")

            tryCatch({
              req_ds_ids <- if (!is_empty_str(ds_param)) trimws(strsplit(as.character(ds_param), ",")[[1]]) else NULL
              date_pfx <- format(Sys.Date(), "%y%m%d")
              jobs_dir_stage <- normalizePath(dirname(out_path), mustWork = FALSE)
              dir.create(jobs_dir_stage, recursive = TRUE, showWarnings = FALSE)
              stage_ej <- chartr("\\", "/", file.path(jobs_dir_stage, paste0("stage_", as.integer(Sys.time()), "_", sample.int(1e6, 1))))
              dir.create(stage_ej, recursive = TRUE, showWarnings = FALSE)

              # Manifest-driven ZIP build
              entries <- build_zip_manifest(user_id, module = module_param, ds_ids = req_ds_ids)

              if (length(entries) > 0) {
                for (item in entries) {
                  e <- item$entry
                  src <- item$src_path
                  tdir <- item$target_dir
                  dest_folder <- chartr("\\", "/", file.path(stage_ej, tdir))
                  dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)

                  base_id <- get_base_id(e$dsId)
                  res <- materialize_manifest_entry(e, src, ext = e$ext, uid = user_id, base_id = base_id, file_type = e$key)
                  if (!is.null(res) && !is.null(res$file_path) && isTRUE(file.exists(res$file_path))) {
                    dest_file <- chartr("\\", "/", file.path(dest_folder, res$clean_filename))
                    cnt <- 1L
                    while (file.exists(dest_file)) {
                      ne <- sub("\\.[^.]+$", "", res$clean_filename)
                      ex <- sub(".*\\.([^.]+)$", "\\1", res$clean_filename)
                      dest_file <- chartr("\\", "/", file.path(dest_folder, sprintf("%s_%d.%s", ne, cnt, ex)))
                      cnt <- cnt + 1L
                    }
                    file.copy(res$file_path, dest_file)
                  }
                }

                # Include markdown analysis report strictly for this module
                target_rep_fname <- if (!is_empty_str(module_param)) sprintf("analysis_report_%s.md", tolower(safe_str(module_param))) else "analysis_report.md"
                target_rep_path <- file.path(export_dir, target_rep_fname)
                if (file.exists(target_rep_path)) {
                  dest_rep_fname <- if (!is_empty_str(module_param)) sprintf("%s_analysis_report_%s.md", date_pfx, tolower(safe_str(module_param))) else sprintf("%s_analysis_report.md", date_pfx)
                  file.copy(target_rep_path, file.path(stage_ej, dest_rep_fname))
                }
              }

              staged_files <- list.files(stage_ej, full.names = FALSE, recursive = TRUE)
              if (length(staged_files) == 0) {
                saveRDS(list(.error = "No result files found to export."), out_path)
                unlink(stage_ej, recursive = TRUE)
                return(invisible())
              }

              zip_path_ej <- chartr("\\", "/", normalizePath(file.path(jobs_dir_stage, sprintf("%s_all_results_%d_%d.zip", date_pfx, as.integer(Sys.time()), sample.int(1e6, 1))), mustWork = FALSE))
              zipped_ej <- create_zip_archive(zip_path_ej, stage_ej)
              unlink(stage_ej, recursive = TRUE)

              if (!zipped_ej || !file.exists(zip_path_ej)) {
                saveRDS(list(.error = "Failed to create zip archive."), out_path)
                return(invisible())
              }

              saveRDS(list(
                status       = "done",
                zip_path     = zip_path_ej,
                filename     = sprintf("%s_all_results.zip", date_pfx),
                content_type = "application/zip",
                size_bytes   = file.info(zip_path_ej)$size
              ), out_path)
            }, error = function(e) {
              saveRDS(list(.error = conditionMessage(e)), out_path)
            })
          },
          args = list(export_dir_ej, user_id_ej, ds_param_ej, module_param_ej,
                      out_path_ej, backend_dir_ej),
          env = get_worker_env(1L),
          stdout = log_path_ej, stderr = "2>&1", supervise = TRUE
        )

        registry_ej <- list(
          out = out_path_ej, log = log_path_ej, process = p_ej,
          kind = "export", user_id = user_id_ej, started = Sys.time()
        )
        assign(job_id_ej, registry_ej, envir = .fs_jobs)
        cat(sprintf("[EXPORT-JOB] Started background ZIP job %s for user %s\n", job_id_ej, user_id_ej))
        return(json_response(list(jobId = job_id_ej, status = "running")))
      }
    }

    if (method == "GET" && path == "/export-job") {
      q_ej      <- shiny::parseQueryString(req$QUERY_STRING)
      job_id_ej <- q_ej$id
      if (is.null(job_id_ej)) {
        return(json_response(list(status = "error", message = "Missing export job id"), 400))
      }

      # 1. Check in-process single-file queue (.export_queue)
      if (exists(job_id_ej, envir = .export_queue, inherits = FALSE)) {
        job_q <- get(job_id_ej, envir = .export_queue)
        if (job_q$status %in% c("queued", "running")) {
          return(json_response(list(status = "running")))
        }
        if (job_q$status == "error") {
          return(json_response(list(status = "error", message = job_q$message %||% "Export failed"), 500))
        }
        if (job_q$status == "done" && !is.null(job_q$result)) {
          res <- job_q$result
          if (is.null(res$file_path) || !file.exists(res$file_path)) {
            return(json_response(list(status = "error", message = "Export file missing or expired."), 500))
          }
          content_type_ej <- res$content_type %||% "application/octet-stream"
          con_ej <- file(res$file_path, "rb")
          content_bytes_ej <- readBin(con_ej, "raw", n = file.info(res$file_path)$size)
          close(con_ej)

          return(httpResponse(
            status       = 200,
            content_type = content_type_ej,
            headers      = list(
              "Access-Control-Allow-Origin" = "*",
              "Content-Disposition" = sprintf('attachment; filename="%s"', res$clean_filename)
            ),
            content = content_bytes_ej
          ))
        }
      }

      # 2. Check background ZIP export jobs (.fs_jobs)
      if (exists(job_id_ej, envir = .fs_jobs, inherits = FALSE)) {
        job_ej <- get(job_id_ej, envir = .fs_jobs)

        if (!is.null(job_ej$process) && isTRUE(tryCatch(job_ej$process$is_alive(), error = function(e) FALSE))) {
          return(json_response(list(
            status  = "running",
            elapsed = round(as.numeric(difftime(Sys.time(), job_ej$started, units = "secs")), 1)
          )))
        }

        if (is.null(job_ej$out) || !file.exists(job_ej$out)) {
          log_tail_ej <- if (!is.null(job_ej$log) && file.exists(job_ej$log))
            paste(utils::tail(readLines(job_ej$log, warn = FALSE), 25), collapse = "\n") else ""
          return(json_response(list(status = "error",
            message = "Export job produced no output", log = log_tail_ej), 500))
        }
        out_ej <- tryCatch(readRDS(job_ej$out), error = function(e) list(.error = conditionMessage(e)))
        if (!is.null(out_ej$.error)) {
          return(json_response(list(status = "error", message = out_ej$.error), 500))
        }

        target_file_ej <- out_ej$file_path %||% out_ej$zip_path
        if (is.null(target_file_ej) || !file.exists(target_file_ej)) {
          return(json_response(list(status = "error", message = "Export file missing or expired."), 500))
        }

        content_type_ej <- out_ej$content_type %||% "application/zip"
        con_ej           <- file(target_file_ej, "rb")
        content_bytes_ej <- readBin(con_ej, "raw", n = file.info(target_file_ej)$size)
        close(con_ej)

        if (!is.null(out_ej$zip_path) || grepl("^/tmp|export_stage|tmp.*temp", target_file_ej)) {
          tryCatch(unlink(target_file_ej), error = function(e) NULL)
        }
        fs_job_remove(job_id_ej)

        return(httpResponse(
          status       = 200,
          content_type = content_type_ej,
          headers      = list(
            "Access-Control-Allow-Origin" = "*",
            "Content-Disposition" = sprintf('attachment; filename="%s"', out_ej$filename)
          ),
          content = content_bytes_ej
        ))
      }

      return(json_response(list(status = "error", message = "Unknown export job id"), 404))
    }

    if (method == "DELETE" && path == "/export-job") {
      q_ej      <- shiny::parseQueryString(req$QUERY_STRING)
      job_id_ej <- q_ej$id
      if (!is.null(job_id_ej)) {
        if (exists(job_id_ej, envir = .export_queue, inherits = FALSE)) {
          rm(list = job_id_ej, envir = .export_queue)
        }
        if (exists(job_id_ej, envir = .fs_jobs, inherits = FALSE)) {
          job_ej <- get(job_id_ej, envir = .fs_jobs)
          if (!is.null(job_ej$process)) tryCatch(job_ej$process$kill(), error = function(e) NULL)
          out_ej <- tryCatch(readRDS(job_ej$out), error = function(e) NULL)
          if (!is.null(out_ej$zip_path) && file.exists(out_ej$zip_path)) unlink(out_ej$zip_path)
          if (!is.null(out_ej$file_path) && grepl("^/tmp|export_stage|tmp.*temp", out_ej$file_path) && file.exists(out_ej$file_path)) unlink(out_ej$file_path)
          fs_job_remove(job_id_ej)
        }
      }
      return(json_response(list(status = "cancelled")))
    }

    # ----------------------------------------------------
    # Route 2d: GET / POST /api/dataset-info or /api/dataset-summary
    # Lightweight endpoint to retrieve authoritative dataset statistics & shared features
    # ----------------------------------------------------
    if (path %in% c("/dataset-info", "/dataset-summary", "/api/dataset-info", "/api/dataset-summary")) {
      query_list <- parseQueryString(req$QUERY_STRING)
      ds_param <- if (!is.null(body_data$datasetIds)) body_data$datasetIds else (if (!is.null(body_data$datasets)) body_data$datasets else (query_list$datasetIds %||% query_list$ids %||% ""))
      module_req <- if (!is.null(body_data$module)) body_data$module else (query_list$module %||% "dp")

      req_ids <- character(0)
      if (is.list(ds_param)) {
        req_ids <- sapply(ds_param, function(x) if (is.list(x)) (x$datasetId %||% x$id %||% "") else as.character(x))
      } else if (is.character(ds_param) && nzchar(ds_param)) {
        req_ids <- trimws(strsplit(ds_param, ",")[[1]])
      }
      req_ids <- req_ids[nzchar(req_ids)]

      res_datasets <- list()
      all_feature_sets <- list()

      for (d_id in req_ids) {
        parsed_ds <- get_backend_datasets(d_id)
        base_id   <- parsed_ds$base_id

        # Load expression matrix (try processed first, then original)
        parsed_expr <- tryCatch(get_backend_dataset(d_id, original = FALSE), error = function(e) NULL)
        if (is.null(parsed_expr) || is.null(parsed_expr$expr)) {
          parsed_expr <- tryCatch(get_backend_dataset(d_id, original = TRUE), error = function(e) NULL)
        }

        # Load metadata
        meta_path <- get_session_path(base_id, "%s_upload_expr_metadata.rds")
        if (!file.exists(meta_path)) meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
        meta_expr <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL

        # Load clinical data
        clin_path <- get_clinical_path(d_id)
        clin_meta_path <- get_clin_metadata_path(d_id)
        clin_df <- NULL
        clin_meta <- if (file.exists(clin_meta_path)) tryCatch(readRDS(clin_meta_path), error = function(e) NULL) else NULL
        if (file.exists(clin_path)) {
          raw_clin <- tryCatch(read_csv_preserve_id(clin_path), error = function(e) NULL)
          if (!is.null(raw_clin)) {
            s_col <- if (!is.null(clin_meta$sampleIdCol) && length(clin_meta$sampleIdCol) > 0) clin_meta$sampleIdCol[1] else colnames(raw_clin)[1]
            clin_df <- tryCatch(parse_clinical_data(raw_clin, colnames(raw_clin), s_col), error = function(e) NULL)
          }
        }

        n_feats <- 0L
        n_samps <- 0L
        sample_names <- character(0)
        gene_ids <- character(0)
        col_names <- character(0)

        if (!is.null(parsed_expr) && !is.null(parsed_expr$expr)) {
          mat <- parsed_expr$expr
          n_feats <- as.integer(nrow(mat))
          n_samps <- as.integer(ncol(mat))
          sample_names <- colnames(mat) %||% character(0)
          gene_ids <- rownames(mat) %||% character(0)
          col_names <- c((parsed_expr$geneIdCol %||% "Feature")[1], sample_names)
          if (length(gene_ids) > 0) {
            all_feature_sets[[d_id]] <- gene_ids
          }
        }

        clin_sample_names <- character(0)
        group_vals <- character(0)
        clin_columns <- character(0)

        if (!is.null(clin_df)) {
          clin_columns <- colnames(clin_df)
          clin_sample_names <- rownames(clin_df)
          g_col <- if (!is.null(clin_meta$groupCol) && length(clin_meta$groupCol) > 0) clin_meta$groupCol[1] else ""
          if (nzchar(g_col) && g_col %in% colnames(clin_df)) {
            group_vals <- unique(as.character(clin_df[[g_col]]))
            group_vals <- group_vals[!is.na(group_vals) & group_vals != ""]
          }
        }

        matched_samps <- if (length(clin_sample_names) > 0 && length(sample_names) > 0) intersect(sample_names, clin_sample_names) else sample_names
        missing_c <- if (length(clin_sample_names) > 0) setdiff(sample_names, clin_sample_names) else character(0)
        c_no_expr <- if (length(sample_names) > 0) setdiff(clin_sample_names, sample_names) else character(0)

        res_datasets[[d_id]] <- list(
          datasetId             = d_id,
          baseId                = base_id,
          module                = (parsed_ds$module %||% meta_expr$module %||% "dp")[1],
          parentModule          = (parsed_ds$parentModule %||% meta_expr$parentModule %||% "dp")[1],
          isInline              = isTRUE(any(c(parsed_ds$isInline, meta_expr$isInline))),
          nSamples              = n_samps,
          nFeatures             = n_feats,
          columns               = col_names,
          sampleIds             = sample_names,
          matchingSamples       = matched_samps,
          missingClinSamples    = missing_c,
          clinicalNoExprSamples = c_no_expr,
          groups                = group_vals,
          clinicalColumns       = clin_columns,
          dataType              = (meta_expr$dataType %||% parsed_expr$dataType %||% "readcounts")[1],
          isNormalized          = isTRUE(any(c(meta_expr$isNormalized, parsed_expr$isNormalized)))
        )
      }

      shared_feats <- NULL
      shared_feats_count <- 0L
      if (length(all_feature_sets) > 1) {
        shared_feats <- Reduce(intersect, all_feature_sets)
        shared_feats_count <- length(shared_feats)
      } else if (length(all_feature_sets) == 1) {
        shared_feats_count <- length(all_feature_sets[[1]])
      }

      return(json_response(list(
        status              = "success",
        datasets            = res_datasets,
        sharedFeaturesCount = shared_feats_count,
        sharedFeatures      = if (!is.null(shared_feats) && length(shared_feats) <= 1000) shared_feats else NULL
      )))
    }

    # ----------------------------------------------------
    # Route 3: DELETE /api/dataset/<id>/<scope>
    # ----------------------------------------------------
    if (method == "DELETE" && grepl("^/dataset/", path)) {
      parts      <- strsplit(substring(path, 10), "/")[[1]]
      dataset_id <- parts[1]
      scope      <- if (length(parts) >= 2) parts[2] else "all"
      user_id    <- get_user_id(dataset_id)
      cat(sprintf("[LOG] API /dataset DELETE called: datasetId=%s, scope=%s\n", dataset_id, scope))

      # 1. Immediately terminate any active background worker process for this dataset
      terminate_jobs_for_dataset(dataset_id, user_id)

      # 2. Asynchronously perform file cleanup and report removal
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          tryCatch(perform_dataset_file_cleanup(dataset_id, scope, user_id), error = function(e) {
            cat(sprintf("[WARNING] perform_dataset_file_cleanup error: %s\n", conditionMessage(e)))
          })
        }, 0)
      } else {
        perform_dataset_file_cleanup(dataset_id, scope, user_id)
      }

      return(json_response(list(status = "success",
                                message = sprintf("Cleared %s data for dataset %s and cancelled active jobs", scope, dataset_id))))
    }

    # ----------------------------------------------------
    # Route 4: POST /api/sample-data
    # ----------------------------------------------------
    if (method == "POST" && path == "/sample-data") {
      type <- body_data$type
      cat(sprintf("[LOG] API /sample-data called: requesting type='%s'\n", type))
      
      file_map <- list(
        expression = "sample_data/expression.csv",
        clinical = "sample_data/clinical.csv",
        enrichment = "sample_data/gene_list_logfc.csv",
        gene_list_logfc = "sample_data/gene_list_logfc.csv",
        gene_list = "sample_data/gene_list.csv"
      )
      
      file_path <- file_map[[type]]
      if (is.null(file_path) || !file.exists(file_path)) {
        return(json_response(list(detail = "[ERROR] Sample data type not found"), status_code = 404))
      }
      
      df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      columns <- colnames(df)
      parsedData <- lapply(1:nrow(df), function(i) {
        unname(as.character(unlist(df[i, ])))
      })
      
      return(json_response(list(columns = columns, parsedData = parsedData)))
    }

    # ----------------------------------------------------
    # Route 5: POST /api/annotation
    # ----------------------------------------------------
    if (method == "POST" && path == "/annotation") {
      return(submit_async_compute_job("annotation", body_data))
    }

    # ----------------------------------------------------
    # Route 6: POST /api/processing
    # ----------------------------------------------------
    if (method == "POST" && path == "/processing") {
      return(submit_async_compute_job("processing", body_data))
    }

    # ----------------------------------------------------
    # Route 7: POST /api/normalization
    # ----------------------------------------------------
    if (method == "POST" && path == "/normalization") {
      return(submit_async_compute_job("normalization", body_data))
    }

    # ----------------------------------------------------
    # Route 8: POST /api/pca
    # ----------------------------------------------------
    if (method == "POST" && path == "/pca") {
      return(submit_async_compute_job("pca", body_data))
    }

    # ----------------------------------------------------
    # Route 9: POST /api/batch-correction
    # ----------------------------------------------------
    if (method == "POST" && path == "/batch-correction") {
      return(submit_async_compute_job("batch", body_data))
    }

    # ----------------------------------------------------
    # Route 10: POST /api/de
    # ----------------------------------------------------
    if (method == "POST" && path == "/de") {
      return(submit_async_compute_job("de", body_data))
    }

    # ----------------------------------------------------
    # Route 11: POST /api/ea
    # ----------------------------------------------------
    if (method == "POST" && path == "/ea") {
      return(submit_async_compute_job("ea", body_data))
    }

    # ----------------------------------------------------
    # Route 12: POST /api/feature-selection-job  (async: start job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/feature-selection-job") {
      # Sweep expired jobs, then reject if too many are already running (OOM guard).
      fs_jobs_cleanup()
      max_concurrent <- get_max_concurrent_jobs()
      if (fs_jobs_running() >= max_concurrent) {
        return(json_response(list(status = "error",
          message = sprintf("Server busy: %d feature-selection jobs already running (max: %d). Please retry shortly.",
                            fs_jobs_running(), max_concurrent)), 429))
      }
      models     <- body_data$models
      split_ratio <- body_data$splitRatio
      datasets   <- body_data$datasets
      parameters <- body_data$parameters
      multi_dataset_mode <- body_data$multiDatasetMode %||% "combine"
      max_features_select <- if (!is.null(body_data$maxFeaturesSelect)) as.numeric(body_data$maxFeaturesSelect) else 10

      if (is.list(datasets)) {
        for (d in datasets) {
          sync_dataset_metadata(d, module = "fs")
        }
      }

      # Resolve user session dir from the first dataset id
      ds_id_raw <- if (length(datasets) > 0) datasets[[1]]$id else ""
      user_id   <- get_user_id(ds_id_raw)
      if (!is.null(user_id) && nzchar(user_id)) {
        jobs_dir <- base::sprintf("tmp/user_sessions/%s/jobs", user_id)
      } else {
        jobs_dir <- "tmp/jobs"
      }
      base::dir.create(jobs_dir, showWarnings = FALSE, recursive = TRUE)

      job_id   <- paste0("fsjob_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
      in_path  <- file.path(jobs_dir, paste0(job_id, "_in.rds"))
      out_path <- file.path(jobs_dir, paste0(job_id, "_out.rds"))
      log_path <- file.path(jobs_dir, paste0(job_id, ".log"))
      prog_path <- file.path(jobs_dir, paste0(job_id, "_progress.json"))
      ds_id    <- if (length(datasets) > 0) datasets[[1]]$id else "dataset"
      saveRDS(list(datasets = datasets, models = models, split_ratio = split_ratio, parameters = parameters, max_features_select = max_features_select, multi_dataset_mode = multi_dataset_mode), in_path)

      if (requireNamespace("callr", quietly = TRUE)) {
        backend_dir <- getwd()
        eff_c <- get_effective_cores()
        worker_env <- get_worker_env(eff_c)
        p <- callr::r_bg(
          func = function(in_path, out_path, backend_dir, progress_path) {
            setwd(backend_dir)
            Sys.unsetenv("PORT")
            Sys.setenv(R_PARALLEL_PORT = "random")
            # Skip annotation packages that FS never uses — faster start, less RAM.
            Sys.setenv(FS_WORKER_LITE = "1")
            source("shared_utils.R")
            setup_threading_environment()
            # Only source the libraries required by feature_selection.R itself
            source("processing.R"); source("feature_selection.R")
            input <- readRDS(in_path)
            res <- tryCatch(
              run_feature_selection(input$datasets, input$models, input$split_ratio, input$parameters, input$max_features_select, progress_file = progress_path, multi_dataset_mode = input$multi_dataset_mode %||% "combine"),
              error = function(e) list(.error = conditionMessage(e))
            )
            saveRDS(res, out_path)
          },
          args = list(in_path, out_path, backend_dir, prog_path),
          env = worker_env,
          stdout = log_path, stderr = "2>&1", supervise = TRUE
        )
        assign(job_id, list(process = p, out = out_path, log = log_path, progress = prog_path,
                            models = models, ds_id = ds_id, started = Sys.time()), envir = .fs_jobs)
        cat(base::sprintf("[FS-JOB] started %s in %s (models: %s)\n", job_id, jobs_dir, paste(models, collapse = ", ")))
        return(json_response(list(jobId = job_id, status = "running")))
      } else {
        # callr not installed -> run synchronously so the poll endpoint still works.
        cat("[FS-JOB] callr not installed; running synchronously.\n")
        res <- tryCatch(
          run_feature_selection(datasets, models, split_ratio, parameters, max_features_select, multi_dataset_mode = multi_dataset_mode),
          error = function(e) list(.error = conditionMessage(e))
        )
        saveRDS(res, out_path)
        assign(job_id, list(process = NULL, out = out_path, log = log_path,
                            models = models, ds_id = ds_id, started = Sys.time()), envir = .fs_jobs)
        return(json_response(list(jobId = job_id, status = "done")))
      }
    }

    # ----------------------------------------------------
    # Route 12c: GET /api/feature-selection-job?id=...  (async: poll)
    # ----------------------------------------------------
    if (method == "GET" && path == "/feature-selection-job") {
      q <- shiny::parseQueryString(req$QUERY_STRING)
      job_id <- q$id
      if (is.null(job_id) || !exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
        return(json_response(list(status = "error", message = "Unknown job id"), 404))
      }
      job <- get(job_id, envir = .fs_jobs)

      # Still running?
      if (!is.null(job$process) && job$process$is_alive()) {
        prog <- NULL
        if (!is.null(job$progress) && file.exists(job$progress)) {
          prog <- tryCatch(jsonlite::fromJSON(readLines(job$progress, warn = FALSE)), error = function(e) NULL)
        }
        return(json_response(list(
          status = "running",
          elapsed = round(as.numeric(difftime(Sys.time(), job$started, units = "secs")), 1),
          progress = prog
        )))
      }

      # Finished: read the result.
      if (file.exists(job$out)) {
        res <- readRDS(job$out)
        if (!is.null(res$.error)) {
          return(json_response(list(status = "error", message = res$.error)))
        }
        # Persist CSVs for the export module (parity with the sync route).
        for (d_id in names(res)) {
          for (m in job$models) {
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
        
        # Persist training results RDS to the session directory!
        if (length(names(res)) > 0) {
          for (d_id in names(res)) {
            base_id <- get_base_id(d_id)
            training_results_path <- get_session_path(base_id, "%s_fs_training_results.rds")
            saveRDS(res, training_results_path)
          }
        }
        
        # Append step to report
        tryCatch({
          in_path <- ""
          standard_in <- file.path("tmp/jobs", paste0(job_id, "_in.rds"))
          if (file.exists(standard_in)) {
            in_path <- standard_in
          } else {
            user_dirs <- list.dirs("tmp/user_sessions", recursive = FALSE)
            for (ud in user_dirs) {
              path_check <- file.path(ud, "jobs", paste0(job_id, "_in.rds"))
              if (file.exists(path_check)) {
                in_path <- path_check
                break
              }
            }
          }
          if (nzchar(in_path) && file.exists(in_path)) {
            input_data <- readRDS(in_path)
            ds_list <- input_data$datasets
            job_models <- input_data$models
            tr_ratio <- input_data$split_ratio %||% input_data$train_ratio %||% 0.7
            m_mode <- input_data$multi_dataset_mode %||% "combine"
            
            ds_id_raw <- if (length(ds_list) > 0) ds_list[[1]]$id else ""
            user_id_val <- get_user_id(ds_id_raw)
            
              module_val <- get_backend_datasets(ds_id_raw)$module %||% "fs"
              sel_method <- input_data$parameters$selectionMethod %||% "breakoff"
              pct_val <- input_data$parameters$percentageCutoff %||% 80
              max_feat_val <- input_data$parameters$maxFeaturesSelect %||% 10
              finalize_fs(
                datasets = ds_list,
                models = job_models,
                train_ratio = tr_ratio,
                multi_dataset_mode = m_mode,
                selection_method = sel_method,
                percentage_val = pct_val,
                max_features_val = max_feat_val,
                fs_results = res,
                user_id = user_id_val,
                module = module_val,
                parameters = input_data$parameters
              )
          }
        }, error = function(e) {
          cat("[WARNING] Failed to append FS report in polling route:", e$message, "\n")
        })
        
        return(json_response(list(status = "done", result = res)))
      }

      # Process ended without output -> failed; surface the log tail.
      log_tail <- if (file.exists(job$log)) paste(tail(readLines(job$log, warn = FALSE), 25), collapse = "\n") else ""
      return(json_response(list(status = "error", message = paste0("Job failed.\n", log_tail))))
    }

    # ----------------------------------------------------
    # Route 12d: DELETE /api/feature-selection-job?id=...  (cancel a running job)
    # Shared by the standalone and inline job flows (same .fs_jobs registry).
    # ----------------------------------------------------
    if (method == "DELETE" && (path == "/feature-selection-job" || path == "/inline-fs-job")) {
      q <- shiny::parseQueryString(req$QUERY_STRING)
      job_id <- q$id
      if (is.null(job_id) || !exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
        return(json_response(list(status = "error", message = "Unknown job id"), 404))
      }
      job <- get(job_id, envir = .fs_jobs)
      if (!is.null(job$process)) {
        tryCatch(job$process$kill(), error = function(e) NULL)
      }
      fs_job_remove(job_id)
      cat(sprintf("[FS-JOB] cancelled %s\n", job_id))
      return(json_response(list(status = "cancelled", jobId = job_id)))
    }

    # ----------------------------------------------------
    # Route 12e: POST /api/refit-features  (async: refit features job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/refit-features") {
      cat("[REFIT-FEATURES] Feature selection refitting requested asynchronously.\n")
      return(submit_async_compute_job("refit", body_data))
    }

    # ----------------------------------------------------
    # Route 13: POST /api/cross-validation  (async: cross-validation job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/cross-validation") {
      cat("[CROSS-VALIDATION] Cross-validation requested asynchronously.\n")
      return(submit_async_compute_job("cv", body_data))
    }

    # ----------------------------------------------------
    # Route 14: POST /api/testing  (async: independent validation testing job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/testing") {
      cat("[TESTING] Independent validation testing requested asynchronously.\n")
      return(submit_async_compute_job("testing", body_data))
    }

    if (method == "POST" && path == "/compute-job") {
      kind <- if (!is.null(body_data$kind)) body_data$kind else ""
      payload <- if (!is.null(body_data$payload)) body_data$payload else body_data
      return(submit_async_compute_job(kind, payload))
    }

    if (method == "GET" && path == "/compute-job") {
      q <- shiny::parseQueryString(req$QUERY_STRING)
      job_id <- q$id
      if (is.null(job_id) || !exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
        return(json_response(list(status = "error", message = "Unknown job id"), 404))
      }
      job <- get(job_id, envir = .fs_jobs)
      if (!is.null(job$process) && job$process$is_alive()) {
        return(json_response(list(status = "running",
          elapsed = round(as.numeric(difftime(Sys.time(), job$started, units = "secs")), 1))))
      }
      if (is.null(job$out) || !file.exists(job$out)) {
        log_tail <- if (!is.null(job$log) && file.exists(job$log)) paste(utils::tail(readLines(job$log, warn = FALSE), 25), collapse = "\n") else ""
        return(json_response(list(status = "error", message = "Job produced no output", log = log_tail), 500))
      }
      out <- tryCatch(readRDS(job$out), error = function(e) list(.error = conditionMessage(e)))
      if (!is.null(out$.error)) {
        return(json_response(list(status = "error", message = out$.error)))
      }
      if (is.null(out)) {
        return(json_response(list(status = "error", message = paste(job$kind, "finalisation failed."))))
      }
      if (identical(job$kind, "upload")) {
        return(json_response(list(status = "done", result = list(status = "success", datasets = out))))
      }
      return(json_response(list(status = "done", result = out)))
    }

    if (method == "DELETE" && path == "/compute-job") {
      q <- shiny::parseQueryString(req$QUERY_STRING)
      job_id <- q$id
      if (!is.null(job_id) && exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
        job <- get(job_id, envir = .fs_jobs)
        if (!is.null(job$process)) tryCatch(job$process$kill(), error = function(e) NULL)
        fs_job_remove(job_id)
      }
      return(json_response(list(status = "cancelled")))
    }

    if (method == "POST" && path == "/meta-analysis") {
      # Legacy meta-analysis route - now proxied through async compute-job worker.
      return(submit_async_compute_job("meta", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/upload-chunk
    # Receives chunked file uploads and concatenates them when complete
    # ----------------------------------------------------
    if (method == "POST" && path == "/upload-chunk") {
      upload_id <- body_data$uploadId
      chunk_idx <- as.integer(body_data$chunkIndex)
      total_chunks <- as.integer(body_data$totalChunks)
      chunk_data <- body_data$chunkData
      user_id <- body_data$userId
      dataset_id <- body_data$datasetId
      file_type <- if (!is.null(body_data$fileType)) body_data$fileType else "expression"

      if (is.null(upload_id) || is.na(chunk_idx) || is.na(total_chunks) || is.null(chunk_data)) {
        return(json_response(list(status = "error", message = "Missing chunk upload parameters"), status_code = 400))
      }

      upload_dir <- if (!is.null(user_id) && nzchar(user_id)) {
        file.path("tmp", "user_sessions", user_id, "uploads", upload_id)
      } else {
        file.path("tmp", "uploads", upload_id)
      }
      if (!dir.exists(upload_dir)) {
        dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
      }

      part_file <- file.path(upload_dir, sprintf("part_%05d", chunk_idx))
      writeChar(chunk_data, part_file, eos = NULL, useBytes = TRUE)

      # Check if all parts are received
      existing_parts <- list.files(upload_dir, pattern = "^part_", full.names = TRUE)
      if (length(existing_parts) == total_chunks) {
        parent_dir <- dirname(upload_dir)
        final_file <- file.path(parent_dir, paste0(upload_id, ".csv"))

        if (total_chunks == 1) {
          # Instant atomic move for single-chunk uploads
          file.rename(part_file, final_file)
          unlink(upload_dir, recursive = TRUE, force = TRUE)
        } else if (requireNamespace("callr", quietly = TRUE)) {
          # Offload multi-chunk assembly to background worker so main HTTP thread is never blocked
          callr::r_bg(
            func = function(upload_dir, final_file, total_chunks) {
              out_con <- file(final_file, "wb")
              for (i in 0:(total_chunks - 1)) {
                p_path <- file.path(upload_dir, sprintf("part_%05d", i))
                if (file.exists(p_path)) {
                  p_data <- readBin(p_path, "raw", file.info(p_path)$size)
                  writeBin(p_data, out_con)
                }
              }
              close(out_con)
              unlink(upload_dir, recursive = TRUE, force = TRUE)
            },
            args = list(upload_dir, final_file, total_chunks),
            supervise = TRUE
          )
        } else {
          # Fallback synchronous concatenation if callr is unavailable
          out_con <- file(final_file, "wb")
          for (i in 0:(total_chunks - 1)) {
            p_path <- file.path(upload_dir, sprintf("part_%05d", i))
            if (file.exists(p_path)) {
              p_data <- readBin(p_path, "raw", file.info(p_path)$size)
              writeBin(p_data, out_con)
            }
          }
          close(out_con)
          unlink(upload_dir, recursive = TRUE, force = TRUE)
        }

        return(json_response(list(
          status = "success",
          uploadId = upload_id,
          completed = TRUE,
          receivedChunks = total_chunks,
          totalChunks = total_chunks,
          filePath = final_file
        )))
      }

      return(json_response(list(
        status = "success",
        uploadId = upload_id,
        completed = FALSE,
        receivedChunks = length(existing_parts),
        totalChunks = total_chunks
      )))
    }

    # ----------------------------------------------------
    # Route 16: POST /api/upload-datasets  (Standardised multi-dataset upload)
    # ----------------------------------------------------
    if (method == "POST" && path == "/upload-datasets") {
      return(submit_async_compute_job("upload", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/supplement-clinical (async compute job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/supplement-clinical") {
      cat("[SUPPLEMENT-CLINICAL] Submitting async supplement clinical job.\n")
      return(submit_async_compute_job("supplement_clinical", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/inline-de
    # ----------------------------------------------------
    if (method == "POST" && path == "/inline-de") {
      cat("[INLINE-DE] Submitting async inline DE analysis.\n")
      return(submit_async_compute_job("inline_de", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/inline-de-meta
    # Meta-analysis for inline (DP pipeline) flow. Retrieve server-side DE results of each dataset
    # ----------------------------------------------------
    if (method == "POST" && path == "/inline-de-meta") {
      cat("[INLINE-DE-META] Inline DE meta-analysis requested asynchronously.\n")
      return(submit_async_compute_job("inline_de_meta", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/inline-fs-job  (async: start inline FS job)
    # ----------------------------------------------------
    if (method == "POST" && path == "/inline-fs-job") {
      cat("[INLINE-FS-JOB] Inline feature selection job requested.\n")
      # Sweep expired jobs, then reject if too many are already running (OOM guard).
      fs_jobs_cleanup()
      max_concurrent <- get_max_concurrent_jobs()
      if (fs_jobs_running() >= max_concurrent) {
        return(json_response(list(status = "error",
          message = sprintf("Server busy: %d feature-selection jobs already running (max: %d). Please retry shortly.",
                            fs_jobs_running(), max_concurrent)), 429))
      }
      first_obj          <- body_data$datasets[[1]]
      models             <- body_data$models
      train_ratio        <- if (!is.null(body_data$splitRatio)) body_data$splitRatio else 0.7
      parameters         <- if (!is.null(body_data$parameters)) body_data$parameters else list()
      multi_dataset_mode <- body_data$multiDatasetMode %||% "combine"
      max_features_select <- if (!is.null(body_data$maxFeaturesSelect)) as.numeric(body_data$maxFeaturesSelect) else 10

      # Resolve user session dir from the first dataset id
      ds_id_raw <- if (!is.null(first_obj$datasetId)) first_obj$datasetId else ""
      user_id   <- get_user_id(ds_id_raw)
      if (!is.null(user_id) && nzchar(user_id)) {
        jobs_dir <- base::sprintf("tmp/user_sessions/%s/jobs", user_id)
      } else {
        jobs_dir <- "tmp/jobs"
      }
      base::dir.create(jobs_dir, showWarnings = FALSE, recursive = TRUE)

      # Resolve datasets from the inline stack NOW (in the main process) and
      # serialise them so the background worker can call run_feature_selection
      # without needing access to the in-memory stack environments.
      resolved_datasets <- lapply(body_data$datasets, function(d) {
        ds_id <- d$datasetId

        latest_m <- get_latest_main_stack(ds_id)
        expr_mat <- NULL
        if (!is.null(latest_m)) {
          expr_mat <- latest_m$data
          cat(base::sprintf("[INLINE-FS-JOB] %s: resolved matrix from main stack (step: %s).\n", ds_id, latest_m$step))
        }
        if (is.null(expr_mat)) {
          parsed_expr <- get_backend_dataset(ds_id)
          if (!is.null(parsed_expr)) {
            expr_mat <- parsed_expr$expr
            cat(base::sprintf("[INLINE-FS-JOB] %s: fell back to parsed expression dataset.\n", ds_id))
          }
        }

        clin_path      <- get_clinical_path(ds_id, fallback = TRUE)
        clin_meta_path <- get_clin_metadata_path(ds_id, fallback = TRUE)
        clin_df <- NULL
        clin_sample_id_col <- ""
        clin_group_col     <- ""
        clin_batch_col     <- ""
        pos_class          <- ""
        neg_class          <- ""
        if (file.exists(clin_path)) {
          raw_clin  <- read_csv_preserve_id(clin_path)
          meta_clin <- if (file.exists(clin_meta_path)) tryCatch(readRDS(clin_meta_path), error = function(e) NULL) else NULL
          
          clin_sample_id_col <- if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) {
            d$clinicalSampleIdCol
          } else if (!is.null(meta_clin$sampleIdCol) && nzchar(meta_clin$sampleIdCol)) {
            meta_clin$sampleIdCol
          } else if (ncol(raw_clin) > 0) {
            colnames(raw_clin)[1]
          } else {
            ""
          }
          
          clin_group_col <- if (!is.null(d$clinicalGroupCol) && nzchar(d$clinicalGroupCol)) {
            d$clinicalGroupCol
          } else if (!is.null(meta_clin$groupCol) && nzchar(meta_clin$groupCol)) {
            meta_clin$groupCol
          } else {
            ""
          }
          
          clin_batch_col <- if (!is.null(d$clinicalBatchCol) && nzchar(d$clinicalBatchCol)) {
            d$clinicalBatchCol
          } else if (!is.null(meta_clin$batchCol) && nzchar(meta_clin$batchCol)) {
            meta_clin$batchCol
          } else {
            ""
          }
          
          pos_class <- if (!is.null(d$positiveClass) && nzchar(d$positiveClass)) {
            d$positiveClass
          } else if (!is.null(d$fs_positiveClass) && nzchar(d$fs_positiveClass)) {
            d$fs_positiveClass
          } else if (!is.null(meta_clin$positiveClass)) {
            meta_clin$positiveClass
          } else {
            ""
          }
          neg_class <- if (!is.null(d$negativeClass) && nzchar(d$negativeClass)) {
            d$negativeClass
          } else if (!is.null(d$fs_negativeClass) && nzchar(d$fs_negativeClass)) {
            d$fs_negativeClass
          } else if (!is.null(meta_clin$negativeClass)) {
            meta_clin$negativeClass
          } else {
            ""
          }
          
          if (nzchar(clin_group_col) || nzchar(clin_sample_id_col) || nzchar(clin_batch_col) || nzchar(pos_class) || nzchar(neg_class)) {
            updated_meta <- list(
              sampleIdCol     = clin_sample_id_col,
              groupCol        = clin_group_col,
              batchCol        = clin_batch_col,
              otherCovariates = meta_clin$otherCovariates %||% list(),
              positiveClass   = pos_class,
              negativeClass   = neg_class
            )
            tryCatch(saveRDS(updated_meta, file = clin_meta_path), error = function(e) NULL)
            tryCatch(saveRDS(updated_meta, file = sprintf("tmp/%s_clin_metadata.rds", ds_id)), error = function(e) NULL)
          }
          
          clin_df <- parse_clinical_data(raw_clin, colnames(raw_clin), clin_sample_id_col)
        }

        if (is.null(expr_mat)) {
          cat(base::sprintf("[INLINE-FS-JOB] WARNING: no expression matrix for %s.\n", ds_id))
          return(NULL)
        }

        samples  <- colnames(expr_mat)
        if (!is.null(clin_df)) {
          common   <- intersect(samples, rownames(clin_df))
          expr_mat <- expr_mat[, common, drop = FALSE]
        }
        gene_id_col <- if (!is.null(d$geneIdCol) && d$geneIdCol != "") d$geneIdCol else "GeneID"
        cols        <- c(gene_id_col, colnames(expr_mat))

        list(
          id                  = ds_id,
          name                = d$name,
          dataType            = d$dataType %||% "readcounts",
          module              = "fs",
          parentModule        = "dp",
          isInline            = TRUE,
          parentDatasetId     = ds_id,
          parsedData          = list(),
          columns             = cols,
          clinicalParsedData  = if (!is.null(clin_df)) lapply(1:nrow(clin_df), function(r) unname(as.list(clin_df[r,]))) else list(),
          clinicalColumns     = if (!is.null(clin_df)) colnames(clin_df) else character(0),
          clinicalSampleIdCol = clin_sample_id_col,
          clinicalGroupCol    = clin_group_col,
          clinicalBatchCol    = clin_batch_col,
          positiveClass       = pos_class,
          negativeClass       = neg_class,
          fs_positiveClass    = pos_class,
          fs_negativeClass    = neg_class,
          fs_trainRatio           = if (!is.null(d$trainRatio)) d$trainRatio else train_ratio,
          fs_datasetPurpose       = if (!is.null(d$datasetPurpose)) d$datasetPurpose else "train-and-test",
          fs_isInternalValidation = isTRUE(d$isInternalValidation)
        )
      })
      resolved_datasets <- Filter(Negate(is.null), resolved_datasets)

      if (length(resolved_datasets) == 0) {
        return(json_response(list(status = "error", message = "No expression data found for the requested datasets. Ensure the DP pipeline has been run first."), 400))
      }

      job_id   <- paste0("inline_fsjob_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
      in_path  <- file.path(jobs_dir, paste0(job_id, "_in.rds"))
      out_path <- file.path(jobs_dir, paste0(job_id, "_out.rds"))
      log_path <- file.path(jobs_dir, paste0(job_id, ".log"))
      prog_path <- file.path(jobs_dir, paste0(job_id, "_progress.json"))
      ds_id    <- resolved_datasets[[1]]$id
      saveRDS(list(datasets = resolved_datasets, models = models, split_ratio = train_ratio,
                   parameters = parameters, max_features_select = max_features_select, multi_dataset_mode = multi_dataset_mode), in_path)

      if (requireNamespace("callr", quietly = TRUE)) {
        backend_dir <- getwd()
        eff_c <- get_effective_cores()
        worker_env <- get_worker_env(eff_c)
        p <- callr::r_bg(
          func = function(in_path, out_path, backend_dir, progress_path) {
            setwd(backend_dir)
            Sys.unsetenv("PORT")
            Sys.setenv(R_PARALLEL_PORT = "random")
            # Skip annotation packages that FS never uses — faster start, less RAM.
            Sys.setenv(FS_WORKER_LITE = "1")
            source("shared_utils.R")
            setup_threading_environment()
            # Only source the libraries required by feature_selection.R itself
            source("processing.R"); source("feature_selection.R")
            input <- readRDS(in_path)
            res <- tryCatch(
              run_feature_selection(input$datasets, input$models, input$split_ratio, input$parameters, input$max_features_select, progress_file = progress_path, multi_dataset_mode = input$multi_dataset_mode %||% "combine"),
              error = function(e) list(.error = conditionMessage(e))
            )
            saveRDS(res, out_path)
          },
          args = list(in_path, out_path, backend_dir, prog_path),
          env = worker_env,
          stdout = log_path, stderr = "2>&1", supervise = TRUE
        )
        assign(job_id, list(process = p, out = out_path, log = log_path, progress = prog_path,
                            models = models, ds_id = ds_id, started = Sys.time()), envir = .fs_jobs)
        cat(base::sprintf("[INLINE-FS-JOB] started %s in %s (models: %s)\n", job_id, jobs_dir, paste(models, collapse = ", ")))
        return(json_response(list(jobId = job_id, status = "running")))
      } else {
        cat("[INLINE-FS-JOB] callr not installed; running synchronously.\n")
        res <- tryCatch(
          run_feature_selection(resolved_datasets, models, train_ratio, parameters, max_features_select, multi_dataset_mode = multi_dataset_mode),
          error = function(e) list(.error = conditionMessage(e))
        )
        saveRDS(res, out_path)
        assign(job_id, list(process = NULL, out = out_path, log = log_path,
                            models = models, ds_id = ds_id, started = Sys.time()), envir = .fs_jobs)
        return(json_response(list(jobId = job_id, status = "done")))
      }
    }

    # ----------------------------------------------------
    # Route: GET /api/inline-fs-job?id=...  (async: poll inline FS job)
    # Shares the same .fs_jobs registry as the standalone job.
    # ----------------------------------------------------
    if (method == "GET" && path == "/inline-fs-job") {
      q      <- shiny::parseQueryString(req$QUERY_STRING)
      job_id <- q$id
      if (is.null(job_id) || !exists(job_id, envir = .fs_jobs, inherits = FALSE)) {
        return(json_response(list(status = "error", message = "Unknown inline FS job id"), 404))
      }
      job <- get(job_id, envir = .fs_jobs)

      if (!is.null(job$process) && job$process$is_alive()) {
        prog <- NULL
        if (!is.null(job$progress) && file.exists(job$progress)) {
          prog <- tryCatch(jsonlite::fromJSON(readLines(job$progress, warn = FALSE)), error = function(e) NULL)
        }
        return(json_response(list(
          status  = "running",
          elapsed = round(as.numeric(difftime(Sys.time(), job$started, units = "secs")), 1),
          progress = prog
        )))
      }

      if (file.exists(job$out)) {
        res <- readRDS(job$out)
        if (!is.null(res$.error)) {
          return(json_response(list(status = "error", message = res$.error)))
        }
        for (m in job$models) {
          tryCatch({
            feats_to_save <- res[[job$ds_id]]$model_features[[m]] %||% res[[job$ds_id]]$features
            if (is.null(feats_to_save)) feats_to_save <- res$features
            if (m %in% c("stabl", "boruta")) {
              feats_to_save <- lapply(feats_to_save, function(item) {
                item$importance <- NULL
                item
              })
            }
            save_list_to_csv(feats_to_save, base::sprintf("tmp/feature_importance_%s_%s.csv", m, job$ds_id))
          }, error = function(e) NULL)
        }
        
        # Persist training results RDS to the session directory!
        base_id <- get_base_id(job$ds_id)
        training_results_path <- get_session_path(base_id, "%s_fs_training_results.rds")
        saveRDS(res, training_results_path)
        
        # Append step to report
        tryCatch({
          in_path <- sub("_out\\.rds$", "_in\\.rds", job$out)
          if (file.exists(in_path)) {
            input_data <- readRDS(in_path)
            ds_list <- input_data$datasets
            job_models <- input_data$models
            tr_ratio <- input_data$split_ratio %||% input_data$train_ratio %||% 0.7
            m_mode <- input_data$multi_dataset_mode %||% "combine"
            
            ds_id_raw <- if (length(ds_list) > 0) ds_list[[1]]$id else ""
            user_id_val <- get_user_id(ds_id_raw)
            
            if (!is.null(user_id_val) && nzchar(user_id_val)) {
              module_val <- get_backend_datasets(ds_id_raw)$module %||% "fs"
              sel_method <- input_data$parameters$selectionMethod %||% "breakoff"
              pct_val <- input_data$parameters$percentageCutoff %||% 80
              max_feat_val <- input_data$parameters$maxFeaturesSelect %||% 10
              finalize_fs(
                datasets = ds_list,
                models = job_models,
                train_ratio = tr_ratio,
                multi_dataset_mode = m_mode,
                selection_method = sel_method,
                percentage_val = pct_val,
                max_features_val = max_feat_val,
                fs_results = res,
                user_id = user_id_val,
                module = module_val,
                parameters = input_data$parameters
              )
            }
          }
        }, error = function(e) {
          cat("[WARNING] Failed to append inline FS report:", e$message, "\n")
        })
        
        return(json_response(list(status = "done", result = res)))
      }

      log_tail <- if (file.exists(job$log)) paste(tail(readLines(job$log, warn = FALSE), 25), collapse = "\n") else ""
      return(json_response(list(status = "error", message = paste0("Inline FS job failed.\n", log_tail))))
    }

    # ----------------------------------------------------
    # Route: POST /api/inline-ea
    # ----------------------------------------------------
    if (method == "POST" && path == "/inline-ea") {
      cat("[INLINE-EA] Inline enrichment analysis requested asynchronously.\n")
      return(submit_async_compute_job("inline_ea", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/redo
    # ----------------------------------------------------
    if (method == "POST" && path == "/redo") {
      cat("[API] Redo requested asynchronously.\n")
      return(submit_async_compute_job("redo", body_data))
    }

    # ----------------------------------------------------
    # Route: POST /api/skip
    # ----------------------------------------------------
    if (method == "POST" && path == "/skip") {
      cat("[API] Skip step requested asynchronously.\n")
      return(submit_async_compute_job("skip", body_data))
    }

    # ----------------------------------------------------
    # Route: GET /api/microarray-platforms
    # ----------------------------------------------------
    if (method == "GET" && path == "/microarray-platforms") {
      platforms_path <- "sample_data/microarray_platforms.json"
      if (file.exists(platforms_path)) {
        platforms_data <- jsonlite::read_json(platforms_path)
        return(json_response(platforms_data))
      }
      return(json_response(list(affymetrix = list(), illumina = list())))
    }

    # ----------------------------------------------------
    # Route: POST /api/clear-downstream
    # ----------------------------------------------------
    if (method == "POST" && path == "/clear-downstream") {
      cat("[API] Clear downstream results requested.\n")
      step <- body_data$step
      dataset_ids <- body_data$datasetIds
      
      for (ds_id_full in dataset_ids) {
        user_id <- get_user_id(ds_id_full)
        terminate_jobs_for_dataset(ds_id_full, user_id)
      }
      
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          for (ds_id_full in dataset_ids) {
            tryCatch(delete_downstream_files(ds_id_full, step, is_redo = FALSE), error = function(e) NULL)
          }
        }, 0)
      } else {
        for (ds_id_full in dataset_ids) {
          delete_downstream_files(ds_id_full, step, is_redo = FALSE)
        }
      }
      return(json_response(list(status = "success", message = "Cleared downstream results")))
    }

    # ----------------------------------------------------
    # Route 22: POST /api/cleanup
    # ----------------------------------------------------
    if (method == "POST" && path == "/cleanup") {
      query_list <- parseQueryString(req$QUERY_STRING)
      user_id    <- query_list$userId
      cat(sprintf("[API] Cleanup requested. Cleaning files for user_id='%s'...\n", user_id))
      
      if (!is.null(user_id) && nzchar(user_id)) {
        # 1. Immediately terminate any active background jobs for this user
        terminate_jobs_for_dataset(NULL, user_id = user_id)
        
        # 2. Asynchronously delete user session directory and temp files
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            if (dir.exists("tmp")) {
              tryCatch({
                user_sess_dir <- file.path("tmp/user_sessions", user_id)
                if (dir.exists(user_sess_dir)) {
                  unlink(user_sess_dir, recursive = TRUE)
                }
                all_files <- list.files("tmp", full.names = TRUE)
                user_files <- all_files[grepl(user_id, basename(all_files)) & !dir.exists(all_files)]
                if (length(user_files) > 0) {
                  file.remove(user_files)
                }
                cat(sprintf("[API] Successfully cleaned files for user_id='%s'.\n", user_id))
              }, error = function(e) {
                cat("[WARNING] Failed to clean files for user:", e$message, "\n")
              })
            }
          }, 0)
        } else {
          if (dir.exists("tmp")) {
            tryCatch({
              user_sess_dir <- file.path("tmp/user_sessions", user_id)
              if (dir.exists(user_sess_dir)) unlink(user_sess_dir, recursive = TRUE)
              all_files <- list.files("tmp", full.names = TRUE)
              user_files <- all_files[grepl(user_id, basename(all_files)) & !dir.exists(all_files)]
              if (length(user_files) > 0) file.remove(user_files)
            }, error = function(e) NULL)
          }
        }
      } else {
        cat("[WARNING] Cleanup requested without a valid userId. No files deleted to prevent wiping all users' data.\n")
      }
      gc(verbose = FALSE)
      return(json_response(list(success = TRUE)))
    }

    # Default fall-through 404
    cat(sprintf("[WARNING] Endpoint not found: Method=%s, Path=%s\n", method, path))
    return(httpResponse(
      status = 404,
      content_type = "text/html",
      headers = list("Access-Control-Allow-Origin" = "*"),
      content = "<h1>404 Not Found</h1>"
    ))
  }, error = function(e) {
    # If any actual bioinformatic/ML routine fails, log it and return 500 error code with details to the client
    cat(sprintf("[CRITICAL ERROR] Failed during request execution: %s\n", e$message))
    return(json_response(list(detail = paste0("[ERROR] ", e$message)), status_code = 500))
  })

  return(response)
}

# Register the api route handler under the prefix "/api"
shiny:::handlerManager$addHandler(
  shiny:::routeHandler("/api", api_router),
  "easyomifun_api"
)

# Custom route handler to serve raw index.html and static assets with corrected MIME types (bypassing Windows Registry MIME-type bugs)
static_handler <- function(req) {
  path <- req$PATH_INFO
  
  # Handle root "/" or empty ""
  if (path == "/" || path == "") {
    index_path <- "www/index.html"
    if (file.exists(index_path)) {
      size <- file.info(index_path)$size
      if (size > 0) {
        conn <- file(index_path, "rb")
        content_bytes <- readBin(conn, "raw", n = size)
        close(conn)
        return(httpResponse(
          status = 200,
          content_type = "text/html; charset=UTF-8",
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Cache-Control" = "no-store, no-cache, must-revalidate, max-age=0"
          ),
          content = content_bytes
        ))
      }
    }
    return(NULL)
  }
  
  # Handle other static assets in www/
  file_path <- file.path("www", substring(path, 2))
  file_path <- gsub("/", .Platform$file.sep, file_path)
  
  if (file.exists(file_path) && !dir.exists(file_path)) {
    ext <- tolower(tools::file_ext(file_path))
    
    mime_type <- switch(ext,
      "js"   = "application/javascript; charset=UTF-8",
      "css"  = "text/css; charset=UTF-8",
      "html" = "text/html; charset=UTF-8",
      "png"  = "image/png",
      "tiff" = "image/tiff",
      "tif"  = "image/tiff",
      "jpg"  = "image/jpeg",
      "jpeg" = "image/jpeg",
      "gif"  = "image/gif",
      "svg"  = "image/svg+xml",
      "ico"  = "image/x-icon",
      "json" = "application/json; charset=UTF-8",
      "woff" = "font/woff",
      "woff2"= "font/woff2",
      "ttf"  = "font/ttf",
      "otf"  = "font/otf",
      "exe"  = "application/octet-stream",
      "zip"  = "application/zip",
      "md"   = "text/markdown; charset=UTF-8",
      NULL
    )
    
    if (!is.null(mime_type)) {
      size <- file.info(file_path)$size
      if (size > 0) {
        conn <- file(file_path, "rb")
        content_bytes <- readBin(conn, "raw", n = size)
        close(conn)
        
        # Add caching for assets since their filenames have hashes and won't change
        cache_control <- if (startsWith(path, "/assets/")) {
          "public, max-age=31536000, immutable"
        } else {
          "no-store, no-cache, must-revalidate, max-age=0"
        }
        
        return(httpResponse(
          status = 200,
          content_type = mime_type,
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Cache-Control" = cache_control
          ),
          content = content_bytes
        ))
      }
    }
  }

  # SPA Fallback for client-side routing (HTML5 History API)
  # If request is not an API call or websocket and not a static asset, serve index.html
  if (!startsWith(path, "/api") && !startsWith(path, "/websocket") && !startsWith(path, "/downloads")) {
    index_path <- "www/index.html"
    if (file.exists(index_path)) {
      size <- file.info(index_path)$size
      if (size > 0) {
        conn <- file(index_path, "rb")
        content_bytes <- readBin(conn, "raw", n = size)
        close(conn)
        return(httpResponse(
          status = 200,
          content_type = "text/html; charset=UTF-8",
          headers = list(
            "Access-Control-Allow-Origin" = "*",
            "Cache-Control" = "no-store, no-cache, must-revalidate, max-age=0"
          ),
          content = content_bytes
        ))
      }
    }
  }

  return(NULL)
}

# Register the static asset handler
shiny:::handlerManager$addHandler(
  static_handler,
  "easyomifun_static"
)

# Launch minimal server with no UI (as fallback when www/index.html doesn't exist)
ui <- fluidPage(
  tags$head(
    tags$title("EasyOmiFun Backend Server"),
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; background-color: #f3f4f6; color: #1f2937; margin: 0; padding: 40px; display: flex; justify-content: center; align-items: center; height: 100vh; }
      .container { text-align: center; background: white; padding: 40px 60px; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06); }
      h1 { color: #3b82f6; font-size: 2.5rem; margin-bottom: 10px; }
      p { font-size: 1.1rem; color: #4b5563; }
      .badge { display: inline-block; background-color: #e0f2fe; color: #0369a1; padding: 6px 12px; border-radius: 9999px; font-weight: 600; font-size: 0.875rem; margin-top: 15px; }
    "))
  ),
  div(class = "container",
    h1("EasyOmiFun R Backend"),
    p("API server successfully running and listening for requests..."),
    span(class = "badge", "Active on Port 8080")
  )
)

# Background Garbage Collector to clean up stale user session directories (older than 4 hours)
start_garbage_collector <- function(interval_seconds = 3600, max_age_hours = 4) {
  gc_task <- function() {
    tryCatch({
      session_base_dir <- "tmp/user_sessions"
      if (dir.exists(session_base_dir)) {
        user_dirs <- list.dirs(session_base_dir, recursive = FALSE)
        for (u_dir in user_dirs) {
          files <- list.files(u_dir, full.names = TRUE, recursive = TRUE)
          if (length(files) == 0) {
            unlink(u_dir, recursive = TRUE)
            cat(sprintf("[GC] Cleaned up empty session directory: %s\n", basename(u_dir)))
            next
          }
          
          mtimes <- file.info(files)$mtime
          age_hours <- difftime(Sys.time(), mtimes, units = "hours")
          if (all(age_hours > max_age_hours)) {
            unlink(u_dir, recursive = TRUE)
            cat(sprintf("[GC] Cleaned up stale session directory: %s\n", basename(u_dir)))
          }
        }
      }
    }, error = function(e) {
      cat("[GC WARNING] Garbage collection failed:", e$message, "\n")
    })
    
    later::later(gc_task, interval_seconds)
  }
  later::later(gc_task, interval_seconds)
}

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    cat("[SESSION] User session closed. Running garbage collector...\n")
    tryCatch({
      if (dir.exists("tmp/user_sessions")) {
        user_dirs <- list.dirs("tmp/user_sessions", recursive = FALSE)
        for (u_dir in user_dirs) {
          files <- list.files(u_dir, recursive = TRUE)
          if (length(files) == 0) {
            unlink(u_dir, recursive = TRUE)
          }
        }
      }
    }, error = function(e) {
      cat("[WARNING] Clean up failed:", e$message, "\n")
    })
    gc(verbose = FALSE)
  })
}

# Start the background garbage collector
start_garbage_collector()

# Register global shutdown hook to clean up the temporary directory
onStop(function() {
  cat("[SERVER SHUTDOWN] Cleaning up temporary directory 'tmp'...\n")
  tryCatch({
    if (dir.exists("tmp")) {
      unlink("tmp", recursive = TRUE)
      cat("[SERVER SHUTDOWN] Successfully deleted temporary directory 'tmp'.\n")
    }
  }, error = function(e) {
    cat("[SERVER SHUTDOWN WARNING] Failed to clean up 'tmp' directory:", e$message, "\n")
  })
})

# Start the Shiny app
.app_port <- as.integer(Sys.getenv("EASYOMIFUN_PORT", Sys.getenv("PORT", "8080")))
.app_host <- Sys.getenv("SHINY_HOST", "0.0.0.0")
cat(sprintf("Starting EasyOmiFun Shiny REST API server on http://%s:%d\n", .app_host, .app_port))
# Unset PORT so socket-based parallel clusters (snow / BiocParallel / DESeq2) do not collide with this HTTP server
Sys.unsetenv("PORT")
Sys.setenv(R_PARALLEL_PORT = "random")
shinyApp(ui = ui, server = server, options = list(port = .app_port, host = .app_host))
