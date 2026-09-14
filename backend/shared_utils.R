# ── Central Resource & Mode Management ─────────────────────────────────────────
# Server mode is the default (VPS multi-tenant safety).
# Electron / packaged standalone releases explicitly set EASYOMIFUN_APP_MODE="desktop".
get_app_mode <- function() {
  mode <- tolower(trimws(Sys.getenv("EASYOMIFUN_APP_MODE", "")))
  if (nzchar(mode)) {
    if (mode %in% c("desktop", "local", "standalone")) return("desktop")
    return("server")
  }
  if (identical(Sys.getenv("EASYOMIFUN_DESKTOP"), "1")) {
    return("desktop")
  }
  return("server")
}

get_resource_limits <- function(user_id = NULL) {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  total_cores <- if (is.na(dc) || dc < 1L) 1L else as.integer(dc)
  mode <- get_app_mode()
  
  if (identical(mode, "desktop")) {
    max_c <- max(1L, total_cores)
    max_m <- Inf
    max_jobs <- max(1L, total_cores)
  } else {
    env_cap <- suppressWarnings(as.integer(Sys.getenv("EASYOMIFUN_MAX_CORES_PER_USER", "2")))
    user_cap <- if (!is.na(env_cap) && env_cap > 0) env_cap else 2L
    max_c <- min(total_cores, user_cap)
    
    env_mem <- suppressWarnings(as.integer(Sys.getenv("EASYOMIFUN_MAX_MEM_MB", "4096")))
    max_m <- if (!is.na(env_mem) && env_mem > 0) env_mem else 4096

    env_jobs_raw <- Sys.getenv("EASYOMIFUN_MAX_CONCURRENT_JOBS", Sys.getenv("FS_MAX_CONCURRENT", ""))
    env_jobs <- suppressWarnings(as.integer(env_jobs_raw))
    if (!is.na(env_jobs) && env_jobs > 0) {
      max_jobs <- env_jobs
    } else {
      # In server mode: default concurrency to total_cores / cores_per_user (at least 1)
      max_jobs <- max(1L, as.integer(floor(total_cores / max_c)))
    }
  }
  
  list(
    mode = mode,
    totalCores = total_cores,
    maxCores = max_c,
    maxMemoryMb = max_m,
    maxConcurrentJobs = max_jobs
  )
}

get_effective_cores <- function(requested_cores = NULL, user_id = NULL) {
  limits <- get_resource_limits(user_id = user_id)
  avail <- limits$maxCores
  if (!is.null(requested_cores) && !is.na(requested_cores)) {
    req <- as.integer(requested_cores)
    if (req > 0) return(min(req, avail))
  }
  return(avail)
}

get_local_r_lib <- function() {
  env_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (nzchar(env_lib)) {
    if (!dir.exists(env_lib)) dir.create(env_lib, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(env_lib, winslash = "/", mustWork = FALSE))
  }
  
  # Check if current .libPaths() has a writable local R-lib directory
  cur_paths <- .libPaths()
  for (p in cur_paths) {
    if (grepl("R-lib", p, ignore.case = TRUE)) {
      if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  
  # Fallback to backend/R-lib
  local_lib <- file.path(getwd(), "R-lib")
  if (!dir.exists(local_lib)) {
    dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
  }
  return(normalizePath(local_lib, winslash = "/", mustWork = FALSE))
}

setup_threading_environment <- function(eff_cores = NULL) {
  # Synchronize local R library path into .libPaths() and system environment
  local_lib <- get_local_r_lib()
  if (dir.exists(local_lib)) {
    if (!(local_lib %in% .libPaths())) {
      .libPaths(unique(c(local_lib, .libPaths())))
    }
    Sys.setenv(R_LIBS_USER = local_lib)
    Sys.setenv(R_LIBS_SITE = local_lib)
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  }

  if (is.null(eff_cores)) eff_cores <- get_effective_cores()
  eff_c <- max(1L, as.integer(eff_cores))
  eff_cores_str <- as.character(eff_c)
  
  Sys.setenv(OMP_NUM_THREADS = eff_cores_str)
  Sys.setenv(OMP_THREAD_LIMIT = eff_cores_str)
  Sys.setenv(OPENBLAS_NUM_THREADS = eff_cores_str)
  Sys.setenv(MKL_NUM_THREADS = eff_cores_str)
  Sys.setenv(VECLIB_MAXIMUM_THREADS = eff_cores_str)
  Sys.setenv(NUMEXPR_NUM_THREADS = eff_cores_str)
  Sys.setenv(PYTHONNUMEXPR_MAX_THREADS = eff_cores_str)
  Sys.setenv(GOTO_NUM_THREADS = eff_cores_str)
  Sys.setenv(BLIS_NUM_THREADS = eff_cores_str)
  Sys.setenv(LOKY_MAX_CPU_COUNT = eff_cores_str)
  Sys.setenv(JOBLIB_CPU_COUNT = eff_cores_str)
  Sys.setenv(R_DATATABLE_NUM_PROCS_PERCENT = "100")
  # Prevent socket clusters from inheriting HTTP server PORT (e.g. 8080) which causes socket deadlocks
  Sys.unsetenv("PORT")
  Sys.setenv(R_PARALLEL_PORT = "random")
  # macOS multiprocessing / fork safety
  Sys.setenv(OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "YES")
  Sys.setenv(MPLBACKEND = "Agg")
  
  options(Ncpus = eff_c)
  # Windows cannot fork: a global mc.cores > 1 makes any mclapply-based code (ours or a
  # dependency) throw "'mc.cores' > 1 is not supported on Windows". Force 1 on Windows.
  options(mc.cores = if (.Platform$OS.type == "windows") 1L else eff_c)
  
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    tryCatch({
      RhpcBLASctl::blas_set_num_threads(eff_c)
      RhpcBLASctl::omp_set_num_threads(eff_c)
    }, error = function(e) NULL)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    tryCatch(data.table::setDTthreads(eff_c), error = function(e) NULL)
  }
}

get_worker_env <- function(eff_cores = NULL) {
  if (is.null(eff_cores)) eff_cores <- get_effective_cores()
  eff_c <- max(1L, as.integer(eff_cores))
  eff_c_str <- as.character(eff_c)
  mode_val <- get_app_mode()

  base_env <- if (requireNamespace("callr", quietly = TRUE)) callr::rcmd_safe_env() else character(0)

  custom_env <- c(
    EASYOMIFUN_APP_MODE           = mode_val,
    EASYOMIFUN_DESKTOP            = if (identical(mode_val, "desktop")) "1" else "0",
    EASYOMIFUN_MAX_CORES_PER_USER = eff_c_str,
    OMP_NUM_THREADS               = eff_c_str,
    OMP_THREAD_LIMIT              = eff_c_str,
    OPENBLAS_NUM_THREADS          = eff_c_str,
    MKL_NUM_THREADS               = eff_c_str,
    VECLIB_MAXIMUM_THREADS        = eff_c_str,
    NUMEXPR_NUM_THREADS           = eff_c_str,
    PYTHONNUMEXPR_MAX_THREADS     = eff_c_str,
    GOTO_NUM_THREADS              = eff_c_str,
    BLIS_NUM_THREADS              = eff_c_str,
    LOKY_MAX_CPU_COUNT            = eff_c_str,
    JOBLIB_CPU_COUNT              = eff_c_str,
    R_DATATABLE_NUM_PROCS_PERCENT = "100",
    R_PARALLEL_PORT               = "random",
    OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "YES",
    MPLBACKEND                    = "Agg",
    STABL_PYTHON                  = Sys.getenv("STABL_PYTHON", ""),
    STABL_CONDA_ENV               = Sys.getenv("STABL_CONDA_ENV", ""),
    STABL_NJOBS                   = eff_c_str,
    CONDA_PREFIX                  = Sys.getenv("CONDA_PREFIX", ""),
    RETICULATE_PYTHON             = Sys.getenv("RETICULATE_PYTHON", ""),
    RETICULATE_MINICONDA_PATH     = Sys.getenv("RETICULATE_MINICONDA_PATH", ""),
    EASYOMIFUN_ELECTRON_ARCH      = Sys.getenv("EASYOMIFUN_ELECTRON_ARCH", ""),
    R_LIBS_USER                   = Sys.getenv("R_LIBS_USER", ""),
    R_LIBS                        = Sys.getenv("R_LIBS", ""),
    R_LIBS_SITE                   = Sys.getenv("R_LIBS_SITE", "")
  )

  custom_env <- custom_env[nzchar(custom_env)]
  merged <- c(base_env[!names(base_env) %in% names(custom_env)], custom_env)
  return(merged)
}

get_bioc_parallel_param <- function(eff_cores = NULL) {
  if (is.null(eff_cores)) eff_cores <- get_effective_cores()
  eff_c <- max(1L, as.integer(eff_cores))
  if (!requireNamespace("BiocParallel", quietly = TRUE) || eff_c <= 1L) {
    if (requireNamespace("BiocParallel", quietly = TRUE)) {
      return(BiocParallel::SerialParam())
    }
    return(NULL)
  }
  
  if (.Platform$OS.type == "windows") {
    cur_lp <- .libPaths()
    if (length(cur_lp) > 0 && nzchar(cur_lp[1])) {
      Sys.setenv(R_LIBS_USER = cur_lp[1])
      Sys.setenv(R_LIBS = cur_lp[1])
      Sys.setenv(R_LIBS_SITE = cur_lp[1])
    }
    # BiocParallel SnowParam checks Sys.getenv("PORT") before choosing random ports.
    # Unsetting PORT and setting R_PARALLEL_PORT="random" prevents socket collisions with Plumber/Shiny (port 8080).
    Sys.unsetenv("PORT")
    Sys.setenv(R_PARALLEL_PORT = "random")
    return(BiocParallel::SnowParam(workers = eff_c, progressbar = FALSE))
  } else {
    return(BiocParallel::MulticoreParam(workers = eff_c, progressbar = FALSE))
  }
}

# ── Unified Cross-Platform Parallel Lapply ─────────────────────────────────────
# Uses POSIX mclapply on Linux/macOS and robust snow / PSOCK socket clusters on Windows.
run_parallel_lapply <- function(X, FUN, ..., var_list = NULL, pkg_list = NULL, cores = NULL, user_id = NULL, envir = parent.frame()) {
  eff_cores <- get_effective_cores(cores, user_id = user_id)
  
  if (eff_cores <= 1L || length(X) <= 1L) {
    return(lapply(X, FUN, ...))
  }
  
  is_windows <- .Platform$OS.type == "windows"
  
  if (!is_windows) {
    # POSIX: Fast fork-based execution
    res <- tryCatch({
      parallel::mclapply(X, FUN, ..., mc.cores = eff_cores)
    }, error = function(e) {
      cat(sprintf("[Parallel] mclapply error: %s. Falling back to serial lapply.\n", conditionMessage(e)))
      lapply(X, FUN, ...)
    })
    return(res)
  }
  
  # Windows: Robust socket cluster using snow / parallel
  num_workers <- min(eff_cores, length(X))
  # Each Windows socket worker is a fresh R process that must source the backend + load
  # packages before it can compute. Spawning one per core (e.g. 32) makes that startup
  # overhead dwarf the actual work and looks like "no CPU usage". Cap to a modest count
  # so parallelism helps heavy tasks instead of hurting. Override via EASYOMIFUN_WIN_MAX_WORKERS.
  win_cap <- suppressWarnings(as.integer(Sys.getenv("EASYOMIFUN_WIN_MAX_WORKERS", "8")))
  if (is.na(win_cap) || win_cap < 1L) win_cap <- 8L
  num_workers <- min(num_workers, win_cap)
  if (num_workers <= 1L) {
    return(lapply(X, FUN, ...))
  }
  cat(sprintf("[Parallel] Windows: launching %d socket workers for %d tasks (cap=%d, snow=%s)...\n",
              num_workers, length(X), win_cap, requireNamespace("snow", quietly = TRUE)))
  
  cur_lp <- .libPaths()
  if (length(cur_lp) > 0 && nzchar(cur_lp[1])) {
    Sys.setenv(R_LIBS_USER = cur_lp[1])
    Sys.setenv(R_LIBS = cur_lp[1])
    Sys.setenv(R_LIBS_SITE = cur_lp[1])
  }
  Sys.unsetenv("PORT")
  Sys.setenv(R_PARALLEL_PORT = "random")
  cl <- tryCatch({
    if (requireNamespace("snow", quietly = TRUE)) {
      snow::makeCluster(num_workers, type = "SOCK")
    } else {
      parallel::makePSOCKcluster(num_workers)
    }
  }, error = function(e) {
    cat(sprintf("[Parallel] Cluster creation failed: %s. Falling back to serial lapply.\n", conditionMessage(e)))
    NULL
  })
  
  if (is.null(cl)) {
    return(lapply(X, FUN, ...))
  }
  
  # Ensure socket cluster is always stopped
  on.exit({
    tryCatch({
      if (requireNamespace("snow", quietly = TRUE)) {
        snow::stopCluster(cl)
      } else {
        parallel::stopCluster(cl)
      }
    }, error = function(e) NULL)
    invisible(gc(verbose = FALSE))
  }, add = TRUE)
  
  # Crucial on Windows: Synchronize libPaths, working directory and backend scripts across all socket worker processes
  cur_wd <- getwd()
  tryCatch({
    parallel::clusterCall(cl, function(lp, wd) {
      .libPaths(lp)
      setwd(wd)
      if (length(lp) > 0 && nzchar(lp[1])) {
        Sys.setenv(R_LIBS_USER = lp[1])
        Sys.setenv(R_LIBS = lp[1])
        Sys.setenv(R_LIBS_SITE = lp[1])
      }
      Sys.unsetenv("PORT")
      Sys.setenv(R_PARALLEL_PORT = "random")
      Sys.setenv(OMP_NUM_THREADS = "1")
      Sys.setenv(OPENBLAS_NUM_THREADS = "1")
      Sys.setenv(MKL_NUM_THREADS = "1")
      options(Ncpus = 1L, mc.cores = 1L)
      if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
        tryCatch({
          RhpcBLASctl::blas_set_num_threads(1L)
          RhpcBLASctl::omp_set_num_threads(1L)
        }, error = function(e) NULL)
      }
      if (file.exists("shared_utils.R")) try(source("shared_utils.R"), silent = TRUE)
      if (file.exists("processing.R")) try(source("processing.R"), silent = TRUE)
      if (file.exists("feature_selection.R")) try(source("feature_selection.R"), silent = TRUE)
      if (file.exists("de_analysis.R")) try(source("de_analysis.R"), silent = TRUE)
      if (file.exists("enrichment.R")) try(source("enrichment.R"), silent = TRUE)
      if (file.exists("report_finalize.R")) try(source("report_finalize.R"), silent = TRUE)
    }, cur_lp, cur_wd)
  }, error = function(e) NULL)
  
  # Worker setup (package loading + variable export) and the parallel run are wrapped in a
  # single guard: ANY failure on the socket cluster (dead worker, missing export, package not
  # found on a worker) falls back to serial lapply instead of propagating an error. This keeps
  # the app working (serially) on environments where the socket cluster cannot run.
  res <- tryCatch({
    # Load required packages on worker nodes
    if (!is.null(pkg_list) && length(pkg_list) > 0) {
      parallel::clusterCall(cl, function(pkgs) {
        for (p in pkgs) suppressWarnings(suppressPackageStartupMessages(require(p, character.only = TRUE)))
      }, pkg_list)
    }
    # Export environment variables/functions to worker nodes
    if (!is.null(var_list) && length(var_list) > 0) {
      parallel::clusterExport(cl, varlist = var_list, envir = envir)
    }
    # Execute parallel workload
    out <- if (requireNamespace("snow", quietly = TRUE)) {
      snow::parLapply(cl, X, FUN, ...)
    } else {
      parallel::parLapply(cl, X, FUN, ...)
    }
    cat(sprintf("[Parallel] Windows: OK, ran %d tasks on %d socket workers.\n", length(X), num_workers))
    out
  }, error = function(e) {
    cat(sprintf("[Parallel] Windows socket cluster run failed: %s. Falling back to serial lapply.\n", conditionMessage(e)))
    lapply(X, FUN, ...)
  })

  return(res)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

is_empty_str <- function(s) {
  if (is.null(s) || length(s) == 0) return(TRUE)
  val <- unlist(s)
  if (is.null(val) || length(val) == 0) return(TRUE)
  val1 <- val[1]
  if (is.na(val1)) return(TRUE)
  val_str <- as.character(val1)
  if (length(val_str) == 0 || is.na(val_str) || !nzchar(trimws(val_str))) return(TRUE)
  return(FALSE)
}

safe_str <- function(s, default = "") {
  if (is.null(s) || length(s) == 0) return(default)
  val <- unlist(s)
  if (is.null(val) || length(val) == 0) return(default)
  val1 <- val[1]
  if (is.na(val1)) return(default)
  val_str <- as.character(val1)
  if (length(val_str) == 0 || is.na(val_str)) return(default)
  return(val_str)
}

create_zip_archive <- function(zip_path, stage_dir) {
  if (!grepl("^(/|[A-Za-z]:)", zip_path)) zip_path <- file.path(getwd(), zip_path)
  if (!grepl("^(/|[A-Za-z]:)", stage_dir)) stage_dir <- file.path(getwd(), stage_dir)
  zip_path <- chartr("\\", "/", normalizePath(zip_path, mustWork = FALSE))
  stage_dir <- chartr("\\", "/", normalizePath(stage_dir, mustWork = FALSE))
  dir.create(dirname(zip_path), showWarnings = FALSE, recursive = TRUE)
  staged_files <- list.files(stage_dir, full.names = FALSE, recursive = TRUE)
  if (length(staged_files) == 0) return(FALSE)
  
  # Method 1: zip R package if available
  if (requireNamespace("zip", quietly = TRUE)) {
    ok <- tryCatch({
      zip::zip(zip_path, files = staged_files, root = stage_dir)
      file.exists(zip_path) && (file.info(zip_path)$size[1] > 0)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
  }

  is_win <- .Platform$OS.type == "windows" || Sys.info()["sysname"] == "Windows"

  # Method 2: On Windows, use built-in PowerShell Compress-Archive (no Rtools required)
  if (is_win) {
    win_stage <- chartr("/", "\\", stage_dir)
    win_zip <- chartr("/", "\\", zip_path)
    ps_cmd <- sprintf('powershell -NoProfile -NonInteractive -Command "Compress-Archive -Path \'%s\\*\' -DestinationPath \'%s\' -Force"', win_stage, win_zip)
    tryCatch(system(ps_cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE), error = function(e) NULL)
    if (file.exists(zip_path) && (file.info(zip_path)$size[1] > 0)) return(TRUE)

    # Windows 10/11 built-in bsdtar (handles .zip natively without Rtools)
    tar_bin <- Sys.which("tar")
    if (nzchar(tar_bin)) {
      old_wd <- getwd()
      setwd(stage_dir)
      tryCatch(system2(tar_bin, args = c("-a", "-c", "-f", win_zip, "*"), stdout = FALSE, stderr = FALSE), error = function(e) NULL)
      setwd(old_wd)
      if (file.exists(zip_path) && (file.info(zip_path)$size[1] > 0)) return(TRUE)
    }
  }

  # Method 3: System zip binary on Linux / macOS / Unix
  zip_bin <- Sys.which("zip")
  if (nzchar(zip_bin)) {
    old_wd <- getwd()
    setwd(stage_dir)
    ok <- tryCatch({
      system2(zip_bin, args = c("-q", "-r", zip_path, "."), stdout = FALSE, stderr = FALSE)
      file.exists(zip_path) && (file.info(zip_path)$size[1] > 0)
    }, error = function(e) FALSE)
    setwd(old_wd)
    if (isTRUE(ok)) return(TRUE)
  }
  
  # Method 4: utils::zip fallback
  old_wd <- getwd()
  setwd(stage_dir)
  tryCatch(utils::zip(zip_path, staged_files), error = function(e) NULL)
  setwd(old_wd)
  return(file.exists(zip_path) && (file.info(zip_path)$size[1] > 0))
}

fast_write_csv <- function(df, file, row.names = FALSE) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    tryCatch({
      data.table::fwrite(df, file = file, row.names = row.names)
      return(invisible(TRUE))
    }, error = function(e) NULL)
  }
  write.csv(df, file = file, row.names = row.names)
}

safe_numeric_matrix <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(matrix(numeric(0), nrow = if (is.null(df)) 0 else nrow(df), ncol = if (is.null(df)) 0 else ncol(df)))
  }
  if (is.matrix(df)) {
    if (is.numeric(df)) return(df)
    mat <- matrix(NA_real_, nrow = nrow(df), ncol = ncol(df), dimnames = dimnames(df))
    for (j in seq_len(ncol(df))) {
      mat[, j] <- suppressWarnings(as.numeric(trimws(as.character(df[, j]))))
    }
    return(mat)
  }
  mat <- matrix(NA_real_, nrow = nrow(df), ncol = ncol(df), dimnames = list(rownames(df), colnames(df)))
  for (j in seq_len(ncol(df))) {
    col_val <- df[[j]]
    if (is.numeric(col_val)) {
      mat[, j] <- as.numeric(col_val)
    } else {
      mat[, j] <- suppressWarnings(as.numeric(trimws(as.character(col_val))))
    }
  }
  mat
}

read_csv_preserve_id <- function(file, ...) {
  # 1. Ultra-fast multithreaded fread with character first column
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_res <- tryCatch({
      df <- data.table::fread(file, header = TRUE, data.table = FALSE, strip.white = TRUE, showProgress = FALSE, check.names = FALSE)
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0 && ncol(df) > 0) {
        colnames(df) <- as.character(colnames(df))
        df[[1]] <- as.character(df[[1]])
        if (ncol(df) > 1) {
          for (j in 2:ncol(df)) {
            if (is.character(df[[j]])) {
              num_val <- suppressWarnings(as.numeric(trimws(df[[j]])))
              non_empty <- nzchar(trimws(df[[j]]))
              if (sum(non_empty) > 0 && sum(!is.na(num_val[non_empty])) >= (0.8 * sum(non_empty))) {
                df[[j]] <- num_val
              }
            }
          }
        }
        df
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (!is.null(dt_res)) return(dt_res)
  }

  # 2. Fallback to smart delimiter detection
  delim <- ","
  first_lines <- tryCatch(readLines(file, n = 5, warn = FALSE), error = function(e) character(0))
  if (length(first_lines) > 0) {
    sample_line <- first_lines[nzchar(trimws(first_lines))][1]
    if (!is.na(sample_line) && nzchar(sample_line)) {
      n_tab   <- lengths(regmatches(sample_line, gregexpr("\t", sample_line)))
      n_comma <- lengths(regmatches(sample_line, gregexpr(",", sample_line)))
      n_semi  <- lengths(regmatches(sample_line, gregexpr(";", sample_line)))
      if (n_tab > n_comma && n_tab > n_semi) {
        delim <- "\t"
      } else if (n_semi > n_comma && n_semi > n_tab) {
        delim <- ";"
      }
    }
  }

  h <- tryCatch(
    read.table(file, header = TRUE, sep = delim, nrows = 5, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = ""),
    error = function(e) {
      read.delim(file, header = TRUE, sep = delim, nrows = 5, check.names = FALSE, stringsAsFactors = FALSE)
    }
  )
  n_cols <- ncol(h)
  df_res <- if (!is.null(n_cols) && n_cols > 0) {
    colClasses <- c("character", rep(NA, n_cols - 1))
    tryCatch(
      read.table(file, header = TRUE, sep = delim, colClasses = colClasses, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = "", ...),
      error = function(e) {
        read.delim(file, header = TRUE, sep = delim, colClasses = colClasses, check.names = FALSE, stringsAsFactors = FALSE, ...)
      }
    )
  } else {
    read.delim(file, header = TRUE, sep = delim, check.names = FALSE, stringsAsFactors = FALSE, ...)
  }
  
  if (!is.null(df_res) && is.data.frame(df_res) && ncol(df_res) > 0) {
    colnames(df_res) <- as.character(colnames(df_res))
    df_res[[1]] <- as.character(df_res[[1]])
    if (ncol(df_res) > 1) {
      for (j in 2:ncol(df_res)) {
        if (is.character(df_res[[j]])) {
          num_val <- suppressWarnings(as.numeric(trimws(df_res[[j]])))
          non_empty <- nzchar(trimws(df_res[[j]]))
          if (sum(non_empty) > 0 && sum(!is.na(num_val[non_empty])) >= (0.8 * sum(non_empty))) {
            df_res[[j]] <- num_val
          }
        }
      }
    }
  }
  return(df_res)
}

get_backend_datasets <- function(ds_id) {
  if (is_empty_str(ds_id)) {
    return(list(user_id = NULL, base_id = "", module = NULL, step = NULL,
                parentModule = NULL, isInline = FALSE, parentDatasetId = NULL))
  }
  ds_str <- safe_str(ds_id)
  
  # Robustly extract user_id using regex since ds_id might contain prefixes like processed_matrix_
  user_id <- NULL
  m <- regexpr("usr_[a-zA-Z0-9]+", ds_str)
  if (isTRUE(m != -1)) {
    user_id <- regmatches(ds_str, m)
  }

  # Parses `{user id}_{dataset id}_{module}_{step}` or `{user id}_{dataset id}_{module}`
  parts <- strsplit(ds_str, "_")[[1]]
  
  # Look for the module token: "dp", "de", "fs", "enrichment", "en", "ea"
  module_idx <- which(parts %in% c("dp", "de", "fs", "enrichment", "en", "ea"))
  base_id <- ds_str
  module  <- NULL
  step    <- NULL

  if (length(module_idx) > 0) {
    m_idx <- module_idx[length(module_idx)] # Last match
    if (is.null(user_id)) {
      user_id <- paste(parts[1:min(2, m_idx)], collapse = "_")
    }
    base_id <- paste(parts[1:m_idx], collapse = "_")
    mod_raw <- parts[m_idx]
    module  <- switch(mod_raw, "enrichment" = "ea", "en" = "ea", mod_raw)
    if (m_idx < length(parts)) {
      step <- paste(parts[(m_idx+1):length(parts)], collapse = "_")
    }
  } else if (length(parts) >= 4) {
    # Fallback: 4-part pattern {user_id}_{dataset_id}_{module}_{step}
    if (is.null(user_id)) {
      user_id <- parts[1]
    }
    dataset_id <- parts[2]
    module <- parts[3]
    step <- paste(parts[4:length(parts)], collapse = "_")
    base_id <- sprintf("%s_%s_%s", user_id, dataset_id, module)
  } else if (length(parts) == 3) {
    # Fallback if 3 parts: {user_id}_{dataset_id}_{module}
    if (is.null(user_id)) {
      user_id <- parts[1]
    }
    dataset_id <- parts[2]
    module <- parts[3]
    base_id <- sprintf("%s_%s_%s", user_id, dataset_id, module)
  }

  result <- list(
    user_id         = user_id,
    base_id         = base_id,
    module          = module,
    step            = step,
    parentModule    = module,
    isInline        = FALSE,
    parentDatasetId = NULL
  )

  # RDS metadata lookup (authoritative metadata source)
  if (!is.null(result$user_id) && nzchar(result$user_id) && !is.null(result$base_id) && nzchar(result$base_id)) {
    sess_dir <- file.path("tmp/user_sessions", result$user_id)
    cands <- c(
      file.path(sess_dir, paste0(result$base_id, "_upload_expr_metadata.rds")),
      file.path(sess_dir, paste0(result$base_id, "_expr_metadata.rds")),
      file.path("tmp", paste0(result$base_id, "_upload_expr_metadata.rds")),
      file.path("tmp", paste0(result$base_id, "_expr_metadata.rds"))
    )
    for (rp in cands) {
      if (file.exists(rp)) {
        meta <- tryCatch(readRDS(rp), error = function(e) NULL)
        if (!is.null(meta) && is.list(meta)) {
          if (!is.null(meta$module) && nzchar(meta$module))             result$module          <- meta$module
          if (!is.null(meta$parentModule) && nzchar(meta$parentModule)) result$parentModule    <- meta$parentModule
          if (!is.null(meta$isInline))                                  result$isInline        <- isTRUE(meta$isInline)
          if (!is.null(meta$parentDatasetId))                           result$parentDatasetId <- meta$parentDatasetId
          break
        }
      }
    }
  }

  return(result)
}

# Helper to synchronize/persist dataset metadata sidecars with explicit lineage
sync_dataset_metadata <- function(d, module = NULL) {
  ds_id <- d$id %||% d$datasetId
  if (is.null(ds_id) || !nzchar(ds_id)) return(NULL)
  
  base_id <- get_base_id(ds_id)
  if (!nzchar(base_id)) return(NULL)
  
  parsed <- get_backend_datasets(ds_id)
  cur_mod <- module %||% parsed$module %||% "dp"
  
  parent_mod <- d$parentModule %||% parsed$parentModule
  is_inline  <- if (!is.null(d$isInline)) isTRUE(d$isInline) else isTRUE(parsed$isInline)
  parent_ds  <- d$parentDatasetId %||% parsed$parentDatasetId
  
  if (is.null(parent_mod) || !nzchar(parent_mod)) {
    parent_mod <- cur_mod
  }
  
  if (is.null(parent_ds) || !nzchar(parent_ds)) {
    if (parent_mod != cur_mod) {
      parent_ds <- sub(paste0("_", cur_mod, "$"), paste0("_", parent_mod), base_id)
    }
  }
  
  expr_meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
  clin_meta_path <- get_session_path(base_id, "%s_clin_metadata.rds")
  
  # Check if parent metadata exists to inherit defaults from
  parent_expr_meta <- NULL
  parent_clin_meta <- NULL
  if (!is.null(parent_ds) && nzchar(parent_ds) && parent_ds != base_id) {
    p_base <- get_base_id(parent_ds)
    p_expr_path <- get_session_path(p_base, "%s_expr_metadata.rds")
    if (!file.exists(p_expr_path)) p_expr_path <- get_session_path(p_base, "%s_upload_expr_metadata.rds")
    if (file.exists(p_expr_path)) {
      parent_expr_meta <- tryCatch(readRDS(p_expr_path), error = function(e) NULL)
    }
    p_clin_path <- get_session_path(p_base, "%s_clin_metadata.rds")
    if (file.exists(p_clin_path)) {
      parent_clin_meta <- tryCatch(readRDS(p_clin_path), error = function(e) NULL)
    }
  }
  
  # Prepare expr metadata
  existing_expr_meta <- if (file.exists(expr_meta_path)) tryCatch(readRDS(expr_meta_path), error = function(e) NULL) else NULL
  expr_meta <- existing_expr_meta %||% parent_expr_meta %||% list()
  
  expr_meta$datasetId       <- base_id
  expr_meta$datasetName     <- d$name %||% d$datasetName %||% expr_meta$datasetName %||% base_id
  expr_meta$module          <- cur_mod
  expr_meta$parentModule    <- parent_mod
  expr_meta$isInline        <- is_inline
  expr_meta$parentDatasetId <- parent_ds
  if (!is.null(d$dataType) && nzchar(d$dataType)) expr_meta$dataType <- d$dataType
  if (!is.null(d$isNormalized)) expr_meta$isNormalized <- isTRUE(d$isNormalized)
  if (!is.null(d$platform) && nzchar(d$platform)) expr_meta$platform <- d$platform
  if (!is.null(d$geneIdCol) && nzchar(d$geneIdCol)) expr_meta$geneIdCol <- d$geneIdCol
  if (!is.null(d$geneIdType) && nzchar(d$geneIdType)) expr_meta$geneIdType <- d$geneIdType
  if (!is.null(d$organism) && nzchar(d$organism)) expr_meta$organism <- d$organism
  
  tryCatch(saveRDS(expr_meta, expr_meta_path), error = function(e) NULL)
  
  # Prepare clin metadata
  existing_clin_meta <- if (file.exists(clin_meta_path)) tryCatch(readRDS(clin_meta_path), error = function(e) NULL) else NULL
  clin_meta <- existing_clin_meta %||% parent_clin_meta %||% list()
  
  clin_meta$datasetId       <- base_id
  clin_meta$module          <- cur_mod
  clin_meta$parentModule    <- parent_mod
  clin_meta$isInline        <- is_inline
  clin_meta$parentDatasetId <- parent_ds
  if (!is.null(d$clinicalSampleIdCol) && nzchar(d$clinicalSampleIdCol)) clin_meta$sampleIdCol <- d$clinicalSampleIdCol
  if (!is.null(d$sampleIdCol) && nzchar(d$sampleIdCol)) clin_meta$sampleIdCol <- d$sampleIdCol
  if (!is.null(d$clinicalGroupCol) && nzchar(d$clinicalGroupCol)) clin_meta$groupCol <- d$clinicalGroupCol
  if (!is.null(d$groupCol) && nzchar(d$groupCol)) clin_meta$groupCol <- d$groupCol
  if (!is.null(d$clinicalBatchCol) && nzchar(d$clinicalBatchCol)) clin_meta$batchCol <- d$clinicalBatchCol
  if (!is.null(d$batchCol) && nzchar(d$batchCol)) clin_meta$batchCol <- d$batchCol
  if (!is.null(d$referenceGroup) && nzchar(d$referenceGroup)) clin_meta$referenceGroup <- d$referenceGroup
  if (!is.null(d$de_referenceGroup) && nzchar(d$de_referenceGroup)) clin_meta$referenceGroup <- d$de_referenceGroup
  if (!is.null(d$comparisonGroup) && nzchar(d$comparisonGroup)) clin_meta$comparisonGroup <- d$comparisonGroup
  if (!is.null(d$de_comparisonGroup) && nzchar(d$de_comparisonGroup)) clin_meta$comparisonGroup <- d$de_comparisonGroup
  if (!is.null(d$positiveClass) && nzchar(d$positiveClass)) clin_meta$positiveClass <- d$positiveClass
  if (!is.null(d$fs_positiveClass) && nzchar(d$fs_positiveClass)) clin_meta$positiveClass <- d$fs_positiveClass
  if (!is.null(d$negativeClass) && nzchar(d$negativeClass)) clin_meta$negativeClass <- d$negativeClass
  if (!is.null(d$fs_negativeClass) && nzchar(d$fs_negativeClass)) clin_meta$negativeClass <- d$fs_negativeClass
  
  tryCatch(saveRDS(clin_meta, clin_meta_path), error = function(e) NULL)
  
  invisible(list(expr = expr_meta, clin = clin_meta))
}

get_base_id <- function(ds_id) {
  if (is_empty_str(ds_id)) return("")
  parsed <- get_backend_datasets(ds_id)
  return(safe_str(parsed$base_id, ""))
}

get_user_id <- function(ds_id) {
  if (is_empty_str(ds_id)) return("user")
  parsed <- get_backend_datasets(ds_id)
  return(safe_str(parsed$user_id, "user"))
}

get_dataset_name <- function(base_id) {
  meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
  if (file.exists(meta_path)) {
    meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$datasetName)) {
      clean_name <- gsub("[^a-zA-Z0-9_-]", "", gsub(" ", "_", meta$datasetName))
      if (nzchar(clean_name)) return(clean_name)
    }
  }
  return(NULL)
}

get_dataset_data_class <- function(ds_id) {
  parsed <- get_backend_datasets(ds_id)
  base_id <- parsed$base_id
  meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
  meta_info <- if (file.exists(meta_path)) tryCatch(readRDS(meta_path), error = function(e) NULL) else NULL
  dtype <- if (!is.null(meta_info$dataType)) meta_info$dataType else "readcounts"
  
  if (dtype %in% c("readcounts", "microarray")) {
    return("transcriptomics")
  } else if (dtype == "proteomics") {
    return("proteomics")
  } else {
    return("others")
  }
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

find_uploaded_file <- function(upload_id, user_id = NULL) {
  if (is.null(upload_id) || !nzchar(upload_id)) return(NULL)
  cands <- c()
  if (!is.null(user_id) && nzchar(user_id)) {
    cands <- c(cands, file.path("tmp", "user_sessions", user_id, "uploads", paste0(upload_id, ".csv")))
  }
  cands <- c(
    cands,
    file.path("tmp", "uploads", paste0(upload_id, ".csv")),
    list.files("tmp/user_sessions", pattern = paste0("^", upload_id, "\\.csv$"), recursive = TRUE, full.names = TRUE)
  )

  part_dirs <- c()
  if (!is.null(user_id) && nzchar(user_id)) {
    part_dirs <- c(part_dirs, file.path("tmp", "user_sessions", user_id, "uploads", upload_id))
  }
  part_dirs <- c(part_dirs, file.path("tmp", "uploads", upload_id))

  # If part directory exists, wait up to 5 seconds for background assembly to finish
  for (iter in 1:50) {
    for (c_path in cands) {
      if (file.exists(c_path) && file.info(c_path)$size > 0 && !any(dir.exists(part_dirs))) {
        return(c_path)
      }
    }
    if (any(dir.exists(part_dirs))) {
      Sys.sleep(0.1)
    } else {
      break
    }
  }

  for (c_path in cands) {
    if (file.exists(c_path)) return(c_path)
  }
  return(NULL)
}

get_session_path <- function(ds_id, filename_pattern, fallback = FALSE) {
  parsed <- get_backend_datasets(ds_id)
  user_id <- parsed$user_id
  base_id <- parsed$base_id
  
  dir_path <- "tmp"
  if (!is.null(user_id) && user_id != "") {
    dir_path <- base::sprintf("tmp/user_sessions/%s", user_id)
    dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  }
  
  if (grepl("%s", filename_pattern)) {
    filename <- base::sprintf(filename_pattern, base_id)
  } else {
    filename <- filename_pattern
  }
  file_path <- file.path(dir_path, filename)
  
  # Fallback ONLY when explicitly requested AND dataset is an inline/inherited dataset
  if (fallback && !file.exists(file_path)) {
    is_inherited <- isTRUE(parsed$isInline)
    if (is_inherited) {
      parent_ds_id <- parsed$parentDatasetId
      parent_mod <- parsed$parentModule
      if (!is.null(parent_ds_id) && nzchar(parent_ds_id) && parent_ds_id != base_id) {
        parent_base_id <- get_base_id(parent_ds_id)
        alt_filename <- if (grepl("%s", filename_pattern)) base::sprintf(filename_pattern, parent_base_id) else filename_pattern
        alt_file_path <- file.path(dir_path, alt_filename)
        if (file.exists(alt_file_path)) {
          return(alt_file_path)
        }
      }
      if (!is.null(parent_mod) && nzchar(parent_mod) && !is.null(parsed$module) && parent_mod != parsed$module) {
        alt_base_id <- sub(paste0("_", parsed$module, "$"), paste0("_", parent_mod), base_id)
        alt_filename <- if (grepl("%s", filename_pattern)) base::sprintf(filename_pattern, alt_base_id) else filename_pattern
        alt_file_path <- file.path(dir_path, alt_filename)
        if (file.exists(alt_file_path)) {
          return(alt_file_path)
        }
      }
    }
    # General module suffix fallback within same session directory
    for (alt_mod in c("dp", "fs", "de", "ea")) {
      alt_base_id <- sub("_(dp|fs|de|ea|en|enrichment)$", paste0("_", alt_mod), base_id)
      if (alt_base_id != base_id) {
        alt_filename <- if (grepl("%s", filename_pattern)) base::sprintf(filename_pattern, alt_base_id) else filename_pattern
        alt_file_path <- file.path(dir_path, alt_filename)
        if (file.exists(alt_file_path)) {
          return(alt_file_path)
        }
      }
    }
  }
  
  return(file_path)
}

# ── Smart Caching Helpers ────────────────────────────────────────────────────

# Compute a fast fingerprint for a matrix or data.frame.
# Uses dim, sorted rownames checksum, and sorted colnames checksum.
# NEVER call register_export_file() on files produced by these helpers.
get_matrix_fingerprint <- function(mat) {
  if (is.null(mat)) return(NULL)
  list(
    nrow      = nrow(mat),
    ncol      = ncol(mat),
    rn_digest = digest::digest(sort(rownames(mat)), algo = "xxhash32"),
    cn_digest = digest::digest(sort(colnames(mat)), algo = "xxhash32")
  )
}

# Save a cache metadata sidecar (always use _cache_meta suffix; never register_export_file).
save_step_cache_meta <- function(ds_id, step, meta_list) {
  path <- get_session_path(ds_id, sprintf("%%s_%s_cache_meta.rds", step))
  tryCatch(saveRDS(meta_list, path), error = function(e) {
    cat(sprintf("[CACHE] Could not save %s cache meta for %s: %s\n", step, ds_id, e$message))
  })
}

# Read a cache metadata sidecar. Returns NULL if missing or unreadable.
get_step_cache_meta <- function(ds_id, step) {
  path <- get_session_path(ds_id, sprintf("%%s_%s_cache_meta.rds", step))
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

get_clinical_path <- function(ds_id, fallback = TRUE) {
  return(get_session_path(ds_id, "%s_clinical.csv", fallback = fallback))
}

get_clin_metadata_path <- function(ds_id, fallback = TRUE) {
  return(get_session_path(ds_id, "%s_clin_metadata.rds", fallback = fallback))
}

# Custom sprintf override to implement session directory isolation and clean target base path mapping
sprintf <- function(fmt, ...) {
  if (startsWith(fmt, "tmp/user_sessions/")) {
    return(do.call(base::sprintf, list(fmt, ...)))
  }
  if (startsWith(fmt, "tmp/")) {
    args <- list(...)
    if (length(args) > 0) {
      user_id <- NULL
      base_id <- NULL
      ds_arg_idx <- NULL
      
      for (idx in seq_along(args)) {
        arg <- args[[idx]]
        if (is.character(arg) && length(arg) == 1) {
          parsed <- get_backend_datasets(arg)
          if (!is.null(parsed$user_id) && parsed$user_id != "") {
            user_id <- parsed$user_id
            base_id <- parsed$base_id
            ds_arg_idx <- idx
            break
          }
        }
      }
      
      if (!is.null(user_id) && user_id != "") {
        dir_path <- base::sprintf("tmp/user_sessions/%s", user_id)
        base::dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
        new_fmt <- sub("^tmp/", paste0(dir_path, "/"), fmt)
        args[[ds_arg_idx]] <- base_id
        return(do.call(base::sprintf, c(list(new_fmt), args)))
      } else {
        # Try to use standard first argument behavior as fallback
        ds_id <- args[[1]]
        parsed <- get_backend_datasets(ds_id)
        user_id <- parsed$user_id
        base_id <- parsed$base_id
        
        if (!is.null(user_id) && user_id != "") {
          dir_path <- base::sprintf("tmp/user_sessions/%s", user_id)
          base::dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
          new_fmt <- sub("^tmp/", paste0(dir_path, "/"), fmt)
          args[[1]] <- base_id
          return(do.call(base::sprintf, c(list(new_fmt), args)))
        }
      }
    }
  }
  base::sprintf(fmt, ...)
}

detect_gene_id_type <- function(val) {
  if (is.null(val) || length(val) == 0) return("genename")
  val_1 <- val[1]
  if (is.na(val_1) || trimws(val_1) == "") return("genename")
  val_clean <- trimws(val_1)
  
  if (grepl("^ENS[A-Z]*G\\d+", val_clean, ignore.case = TRUE)) {
    return("ensembl")
  } else if (grepl("^ILMN_\\d+", val_clean, ignore.case = TRUE)) {
    return("illumina")
  } else if (grepl("^AFFX-", val_clean, ignore.case = TRUE)) {
    return("affymetrix")
  } else if (grepl("^\\d+$", val_clean)) {
    return("entrez")
  } else {
    return("genename")
  }
}

get_formatted_matrix_df <- function(ds_id, expr_mat, default_col_name = "GeneName") {
  if (is.null(expr_mat)) {
    df_out <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(df_out) <- default_col_name
    return(df_out)
  }

  resolved_mapping_path <- get_session_path(ds_id, "%s_resolved_mapping.rds")
  if (file.exists(resolved_mapping_path)) {
    resolved_mapping <- tryCatch(readRDS(resolved_mapping_path), error = function(e) NULL)
    if (!is.null(resolved_mapping)) {
      common_ids <- intersect(rownames(expr_mat), rownames(resolved_mapping))
      if (length(common_ids) > 0) {
        expr_aligned <- expr_mat[common_ids, , drop = FALSE]
        mapping_aligned <- resolved_mapping[common_ids, ]
        df_out <- data.frame(
          entrez_id = mapping_aligned$entrez_id,
          gene_symbol = mapping_aligned$gene_symbol,
          expr_aligned,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        colnames(df_out)[1] <- "entrez_id"
        colnames(df_out)[2] <- "gene_symbol"
        return(df_out)
      }
    }
  }
  
  # Fallback
  rnames <- rownames(expr_mat)
  if (is.null(rnames)) {
    rnames <- character(nrow(expr_mat))
  }
  df_out <- data.frame(rnames, expr_mat, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(df_out) > 0) {
    colnames(df_out)[1] <- default_col_name
  }
  return(df_out)
}

# Custom file.remove override to prevent warnings when files do not exist
file.remove <- function(...) {
  files <- unlist(list(...))
  if (length(files) == 0) return(invisible(logical(0)))
  # Remove empty/null/NA elements
  files <- files[nzchar(files) & !is.na(files)]
  if (length(files) == 0) return(invisible(logical(0)))
  
  existing <- files[file.exists(files)]
  if (length(existing) > 0) {
    base::file.remove(existing)
  } else {
    invisible(logical(0))
  }
}

write_table_by_ext <- function(df, file_path, ext) {
  ext <- tolower(ext)
  if (ext == "tsv" || ext == "txt") {
    write.table(df, file = file_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  } else if (ext == "xlsx") {
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(df, path = file_path)
    } else if (requireNamespace("openxlsx", quietly = TRUE)) {
      openxlsx::write.xlsx(df, file = file_path)
    } else {
      write.csv(df, file = file_path, row.names = FALSE)
    }
  } else {
    # Default to CSV
    write.csv(df, file = file_path, row.names = FALSE)
  }
}

save_list_to_csv <- function(data_list, file_path, headers = NULL) {
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  if (is.null(data_list) || length(data_list) == 0) {
    if (!is.null(headers)) {
      df <- as.data.frame(matrix(ncol = length(headers), nrow = 0))
      colnames(df) <- headers
    } else {
      df <- data.frame(Message = "No significant terms found.", stringsAsFactors = FALSE)
    }
    write.csv(df, file = file_path, row.names = FALSE)
    return(TRUE)
  }
  
  tryCatch({
    # If it's already a data.frame, use it directly
    if (is.data.frame(data_list)) {
      df <- data_list
    } else if (is.list(data_list) && length(data_list) > 0 && is.list(data_list[[1]])) {
      # If it's a list of lists, convert each sub-list to a named row
      # Flatten each element to a simple named vector
      rows <- lapply(data_list, function(item) {
        # Recursively unlist but keep names
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
      df <- do.call(rbind, rows)
    } else if (is.list(data_list) && !is.null(names(data_list)) && length(data_list) > 0 && !is.list(data_list[[1]])) {
      # Named simple list -> single row
      flat <- lapply(names(data_list), function(nm) {
        val <- data_list[[nm]]
        if (is.null(val)) return(setNames(list(NA_character_), nm))
        if (length(val) > 1) setNames(list(paste(val, collapse=";")), nm)
        else setNames(list(as.character(val[1])), nm)
      })
      df <- as.data.frame(do.call(c, flat), stringsAsFactors = FALSE)
    } else {
      # Fallback: try direct coercion
      json_str <- jsonlite::toJSON(data_list, auto_unbox = TRUE)
      df <- jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE, flatten = TRUE)
      if (is.list(df) && !is.data.frame(df)) {
        df <- as.data.frame(t(unlist(df)), stringsAsFactors = FALSE)
      }
    }

    if (!is.null(headers) && length(headers) > 0 && is.data.frame(df)) {
      for (h in headers) {
        if (!h %in% colnames(df)) {
          df[[h]] <- NA
        }
      }
      df <- df[, headers, drop = FALSE]
    }

    write.csv(df, file = file_path, row.names = FALSE)
    return(TRUE)
  }, error = function(e) {
    cat("[ERROR] Failed to save list to CSV:", e$message, "\n")
    return(FALSE)
  })
}

session_file_path <- function(ds_id, filename) {
  uid <- get_user_id(ds_id)
  dir_path <- if (!is.null(uid) && nzchar(uid)) file.path("tmp/user_sessions", uid) else "tmp"
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  
  base_fn <- basename(filename)
  ext <- tools::file_ext(base_fn)
  fn_no_ext <- tools::file_path_sans_ext(base_fn)
  clean_fn <- if (nzchar(ext)) {
    sprintf("%s.%s", gsub("[:/\\\\?*\"<>| ]", "_", fn_no_ext), ext)
  } else {
    gsub("[:/\\\\?*\"<>| ]", "_", base_fn)
  }
  file.path(dir_path, clean_fn)
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

get_downstream_steps <- function(step) {
  if (is.null(step)) return(character(0))
  step_clean <- tolower(trimws(as.character(step)))
  if (step_clean %in% c("upload", "all-datasets", "all_datasets")) {
    return(c("annotation", "processing", "normalization", "normalization-counts", "normalization-others", "batch", "de", "meta", "fs", "cv", "testing", "ea"))
  } else if (step_clean == "annotation") {
    return(c("processing", "normalization", "normalization-counts", "normalization-others", "batch", "de", "meta", "fs", "cv", "testing", "ea"))
  } else if (step_clean == "processing") {
    return(c("normalization", "normalization-counts", "normalization-others", "batch", "de", "meta", "fs", "cv", "testing", "ea"))
  } else if (step_clean %in% c("normalization", "normalization-counts", "normalization-others", "normalization_counts", "normalization_others")) {
    return(c("batch", "de", "meta", "fs", "cv", "testing", "ea"))
  } else if (step_clean == "batch") {
    return(c("de", "meta", "fs", "cv", "testing", "ea"))
  } else if (step_clean %in% c("de", "inline-de", "inline_de", "de-analysis", "de_analysis", "analysis")) {
    return(c("meta", "ea"))
  } else if (step_clean %in% c("meta", "inline-de-meta", "inline_de_meta", "de-meta", "de_meta", "meta-analysis", "meta_analysis")) {
    return(c("ea"))
  } else if (step_clean %in% c("fs", "inline-fs", "inline_fs", "feature-selection", "feature_selection", "model-selection", "model_selection")) {
    return(c("cv", "testing"))
  } else if (step_clean %in% c("cv", "cross-validation", "cross_validation")) {
    return(c("testing"))
  } else if (step_clean %in% c("testing", "test")) {
    return(character(0))
  } else if (step_clean %in% c("ea", "inline-ea", "inline_ea", "inline-enrichment", "inline_enrichment", "enrichment", "enrichment-analysis", "enrichment_analysis")) {
    return(character(0))
  }
  return(character(0))
}

truncate_stacks_to_step <- function(ds_id, target_step, is_redo = FALSE) {
  base_id <- get_base_id(ds_id)
  target_clean <- tolower(trimws(as.character(target_step)))
  
  step_levels <- c(
    "upload" = 1,
    "all-datasets" = 1,
    "all_datasets" = 1,
    "annotation" = 2,
    "processing" = 3,
    "normalization" = 4,
    "normalization-counts" = 4,
    "normalization-others" = 4,
    "normalization_counts" = 4,
    "normalization_others" = 4,
    "batch" = 5
  )
  
  target_lvl <- step_levels[target_clean]
  if (is.na(target_lvl)) {
    return(TRUE)
  }
  
  # 1. Main stack
  main_stack_file <- get_session_path(base_id, "%s_main_stack.rds")
  if (file.exists(main_stack_file)) {
    stack <- tryCatch(readRDS(main_stack_file), error = function(e) list())
    if (is.list(stack) && length(stack) > 0) {
      new_stack <- list()
      for (entry in stack) {
        entry_step <- tolower(trimws(as.character(entry$step)))
        entry_lvl <- step_levels[entry_step]
        if (is.na(entry_lvl)) entry_lvl <- 99
        
        if (target_clean %in% c("upload", "all-datasets", "all_datasets")) {
          if (entry_lvl <= 1) {
            new_stack[[length(new_stack) + 1]] <- entry
          }
        } else if (is_redo) {
          if (entry_lvl < target_lvl) {
            new_stack[[length(new_stack) + 1]] <- entry
          }
        } else {
          if (entry_lvl <= target_lvl) {
            new_stack[[length(new_stack) + 1]] <- entry
          }
        }
      }
      
      if (length(new_stack) == 0 && length(stack) > 0 && identical(stack[[1]]$step, "upload")) {
        new_stack <- list(stack[[1]])
      }
      saveRDS(new_stack, main_stack_file, compress = FALSE)
    }
  }
  
  # 2. Counts stack
  counts_stack_file <- get_session_path(base_id, "%s_counts_stack.rds")
  if (file.exists(counts_stack_file)) {
    counts_stack <- tryCatch(readRDS(counts_stack_file), error = function(e) list())
    if (is.list(counts_stack) && length(counts_stack) > 0) {
      new_counts_stack <- list()
      for (entry in counts_stack) {
        entry_step <- tolower(trimws(as.character(entry$step)))
        entry_lvl <- step_levels[entry_step]
        if (is.na(entry_lvl)) entry_lvl <- 99
        
        if (target_clean %in% c("upload", "all-datasets", "all_datasets")) {
          if (entry_lvl <= 1) {
            new_counts_stack[[length(new_counts_stack) + 1]] <- entry
          }
        } else if (is_redo) {
          if (entry_lvl < target_lvl) {
            new_counts_stack[[length(new_counts_stack) + 1]] <- entry
          }
        } else {
          if (entry_lvl <= target_lvl) {
            new_counts_stack[[length(new_counts_stack) + 1]] <- entry
          }
        }
      }
      if (length(new_counts_stack) == 0 && length(counts_stack) > 0 && identical(counts_stack[[1]]$step, "upload")) {
        new_counts_stack <- list(counts_stack[[1]])
      }
      saveRDS(new_counts_stack, counts_stack_file, compress = FALSE)
    }
  }
  
  # 3. DE stack
  de_stack_file <- get_session_path(base_id, "%s_de_stack.rds")
  if (file.exists(de_stack_file)) {
    de_stack <- tryCatch(readRDS(de_stack_file), error = function(e) list())
    if (is.list(de_stack) && length(de_stack) > 0) {
      new_de_stack <- list()
      for (entry in de_stack) {
        if (identical(entry$step, "upload")) {
          new_de_stack[[length(new_de_stack) + 1]] <- entry
        }
      }
      saveRDS(new_de_stack, de_stack_file, compress = FALSE)
    }
  }
  
  return(TRUE)
}

truncate_stack_to_step <- function(ds_id, step_name) {
  truncate_stacks_to_step(ds_id, step_name, is_redo = TRUE)
}

delete_downstream_files <- function(ds_id_full, target_step, is_redo = FALSE) {
  ds_id <- get_base_id(ds_id_full)
  user_id <- get_user_id(ds_id_full)
  if (is.null(user_id) || !nzchar(user_id)) user_id <- "user"
  
  target_step_clean <- tolower(trimws(as.character(target_step)))
  module_val <- get_backend_datasets(ds_id_full)$module
  if (is.null(module_val) || !nzchar(module_val)) {
    module_val <- if (grepl("_dp", ds_id_full)) "dp" else (if (grepl("_de", ds_id_full)) "de" else (if (grepl("_fs", ds_id_full)) "fs" else "ea"))
  }
  
  downstream_steps <- get_downstream_steps(target_step_clean)
  invalidated_steps <- downstream_steps
  if (is_redo && !target_step_clean %in% c("upload", "all-datasets", "all_datasets")) {
    invalidated_steps <- unique(c(target_step_clean, downstream_steps))
  }
  
  # Remove sections from report
  for (st in invalidated_steps) {
    remove_sections_from_report(user_id, st, ds_id_full, module_val)
  }
  if (is_redo && !target_step_clean %in% c("upload", "all-datasets", "all_datasets")) {
    remove_sections_from_report(user_id, target_step_clean, ds_id_full, module_val)
  }
  
  # Build regex patterns of filenames to remove for this dataset
  patterns_to_remove <- character(0)
  
  # Annotation files
  if ("annotation" %in% invalidated_steps) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(annotation_results|expr_matrix_annotated|annotated_matrix|gene_mapper|raw_mapping|resolved_mapping|unmapped_results|annotated_raw_counts)\\."),
      paste0("^annotation_results_", ds_id, "\\."),
      paste0("^annotation_mapping_", ds_id, "\\."),
      paste0("^", ds_id, "_annotation_cache_meta\\.rds$")
    )
  }
  
  # Processing files
  if ("processing" %in% invalidated_steps) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(processed_expr|processed|processing|processed_matrix|dp_results|processing_dp_results|non_normalized_expr|processing_non_normalized_expr|deseq2_obj|processing_deseq2_obj|edgeR_obj|processing_edgeR_obj|processed_raw_counts)\\."),
      paste0("^", ds_id, "_processing_"),
      paste0("^processed_matrix_", ds_id, "\\."),
      paste0("^", ds_id, "_processing_cache_meta\\.rds$")
    )
  }
  
  # Normalization files
  if (any(c("normalization", "normalization-counts", "normalization-others", "normalization_counts", "normalization_others") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(normalized_expr|normalized_expr_actual|normalized|normalization|normalized_matrix|norm_config|normalization_dp_results)\\."),
      paste0("^", ds_id, "_normalization_"),
      paste0("^boxplot_(before|after).*_", ds_id, "\\."),
      paste0("^", ds_id, "_boxplot_"),
      paste0("^normalized_matrix_", ds_id, "\\."),
      paste0("^", ds_id, "_normalization_cache_meta\\.rds$")
    )
  }
  
  # Batch files
  if ("batch" %in% invalidated_steps) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(batch_expr|batch_expr_actual|batch|batch_corrected_matrix|batch_raw_counts|batch_deseq2_obj|batch_edgeR_obj)\\."),
      paste0("^", ds_id, "_batch_"),
      paste0("^pca_(before|after).*_", ds_id, "\\."),
      paste0("^", ds_id, "_pca_"),
      paste0("^batch_corrected_matrix_", ds_id, "\\."),
      paste0("^", ds_id, "_batch_cache_meta\\.rds$")
    )
  }
  
  # DE files
  if (any(c("de", "inline-de", "inline_de", "de-analysis", "de_analysis", "analysis") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(de_results|de_sig_results|de_raw_table|inline_de_main_stack|inline_de_counts_stack)\\."),
      paste0("^", ds_id, "_de_"),
      paste0("^(de_results|de_sig_results|de_volcano|de_ma|de_heatmap_top10|volcano_plot|ma_plot|heatmap_top10).*_", ds_id, "\\."),
      paste0("^", ds_id, "_(de_volcano|de_ma|de_heatmap|volcano_plot|ma_plot|heatmap)"),
      paste0("^", ds_id, "_de_cache_meta\\.rds$")
    )
  }
  
  # Meta-analysis files
  if (any(c("meta", "inline-de-meta", "inline_de_meta", "de-meta", "de_meta", "meta-analysis", "meta_analysis") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(meta_results|de_meta_results|de_meta_result|dp_meta_results|dp_meta_result|meta_analyzed_results|meta_analyzed_result|heterogeneity_report|forest_plots|meta_volcano_plots)\\."),
      paste0("^(forest_plots|heterogeneity_report|meta_volcano_plots|meta_forest_plots|meta_analyzed_results).*_", ds_id, "\\."),
      paste0("^", user_id, "_(meta_results|de_meta_results|dp_meta_results|meta_analyzed_results|heterogeneity_report|forest_plots|meta_volcano_plots)\\."),
      "^(heterogeneity_report|forest_plots|meta_volcano_plots|meta_forest_plots|meta_analyzed_results|meta_results)\\."
    )
  }
  
  # Feature Selection files
  if (any(c("fs", "inline-fs", "inline_fs", "feature-selection", "feature_selection", "model-selection", "model_selection") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(fs_training_results|fs_train_split|fs_test_split|fs_parameters|fs_meta|fs_train_expr|fs_train_clin|fs_test_expr|fs_test_clin|fs_full_importances|fs_top_features)"),
      paste0("^(feature_importance|selected_features|stabl_path|fdr_path|roc_train).*_", ds_id),
      paste0("^", ds_id, "_(feature_importance|selected_features|stabl_path|fdr_path|roc_train)"),
      paste0("^", ds_id, "_fs_discovery_cache_meta_[^.]+\\.rds$")
    )
  }
  if (any(c("cv", "cross-validation", "cross_validation") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(fs_cv_results|cv_results)\\."),
      paste0("^(cv_results|roc_cv|cv_curves|roc_all_models_cv).*_", ds_id),
      paste0("^", ds_id, "_(cv_results|roc_cv|cv_curves|roc_all_models_cv)")
    )
  }
  if (any(c("testing", "test") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^", ds_id, "_(fs_testing_results|testing_results|model_performance|all_models_performance|confusion_matrix)\\."),
      paste0("^(testing_results|model_performance|all_models_performance|confusion_matrix|roc_test|roc_all_models_train|roc_test_all).*_", ds_id),
      paste0("^", ds_id, "_(testing_results|model_performance|all_models_performance|confusion_matrix|roc_test|roc_all_models_train|roc_test_all)")
    )
  }
  
  # Enrichment Analysis files
  if (any(c("ea", "inline-ea", "inline_ea", "inline-enrichment", "inline_enrichment", "enrichment", "enrichment-analysis", "enrichment_analysis") %in% invalidated_steps)) {
    patterns_to_remove <- c(patterns_to_remove,
      paste0("^(ora_results|gsea_results|inline_ora_results|inline_gsea_results|ora_dotplot|gsea_dotplot|gsea_ridgeplot|gsea_esplot|gsea_ranked_list|gsea_leading_edge).*_", ds_id),
      paste0("^", ds_id, "_(ora_results|gsea_results|inline_ora_results|inline_gsea_results|ora_dotplot|gsea_dotplot|gsea_ridgeplot|gsea_esplot|gsea_ranked_list|gsea_leading_edge)"),
      "^(ora_results|gsea_results|inline_ora_results|inline_gsea_results|ora_dotplot|gsea_dotplot|gsea_ridgeplot|gsea_esplot|gsea_ranked_list|gsea_leading_edge)"
    )
  }
  
  search_dirs <- unique(c(file.path("tmp/user_sessions", user_id), "tmp"))
  search_dirs <- search_dirs[dir.exists(search_dirs)]
  
  if (length(patterns_to_remove) > 0) {
    for (s_dir in search_dirs) {
      f_list <- list.files(s_dir, full.names = TRUE)
      for (f in f_list) {
        bn <- basename(f)
        # CRITICAL SAFETY: Never delete base uploaded raw files or backups!
        if (grepl("(_expression\\.csv|_expression_original_backup\\.csv|_clinical\\.csv|_expr_metadata\\.rds|_upload_expr_metadata\\.rds|_clin_metadata\\.rds|_original_parsed\\.rds|_main_stack_original_backup\\.rds|_counts_stack_original_backup\\.rds)$", bn, ignore.case = TRUE)) {
          next
        }
        for (pat in patterns_to_remove) {
          if (grepl(pat, bn, ignore.case = TRUE)) {
            tryCatch(unlink(f, force = TRUE), error = function(e) NULL)
            break
          }
        }
      }
    }
  }
  
  # Truncate stacks
  truncate_stacks_to_step(ds_id, target_step_clean, is_redo = is_redo)
  
  # Restore expr_matrix.rds
  p_main <- get_session_path(ds_id, "%s_expr_matrix.rds")
  latest_main <- get_latest_main_stack(ds_id)
  if (!is.null(latest_main) && !is.null(latest_main$data)) {
    saveRDS(latest_main$data, p_main, compress = FALSE)
  } else {
    backup_main_f <- get_session_path(ds_id, "%s_main_stack_original_backup.rds")
    if (file.exists(backup_main_f)) {
      backup_stack <- tryCatch(readRDS(backup_main_f), error = function(e) list())
      if (length(backup_stack) > 0 && !is.null(backup_stack[[1]]$data)) {
        saveRDS(backup_stack[[1]]$data, p_main, compress = FALSE)
        main_stack_f <- get_session_path(ds_id, "%s_main_stack.rds")
        saveRDS(list(backup_stack[[1]]), main_stack_f, compress = FALSE)
      }
    } else {
      parsed_orig <- get_backend_dataset(ds_id, original = TRUE)
      if (!is.null(parsed_orig) && !is.null(parsed_orig$expr)) {
        saveRDS(parsed_orig$expr, p_main, compress = FALSE)
      }
    }
  }
  
  ensure_sequential_matrices(ds_id)
  return(TRUE)
}

append_step_to_report <- function(user_id, section_title, content_md, module = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) {
    user_id <- "user"
  }
  
  # Determine report file name
  report_fname <- "analysis_report.md" # fallback
  if (!is.null(module) && nzchar(module)) {
    report_fname <- sprintf("analysis_report_%s.md", tolower(module))
  }
  
  dir_path <- file.path("tmp/user_sessions", user_id)
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  report_path <- file.path(dir_path, report_fname)
  
  header <- NULL
  remaining_sections <- list()
  
  if (file.exists(report_path)) {
    lines <- readLines(report_path, warn = FALSE)
    content <- paste(lines, collapse = "\n")
    if (nzchar(content)) {
      sections <- strsplit(content, "\n\n---\n\n", fixed = TRUE)[[1]]
      if (length(sections) > 0) {
        header <- sections[1]
        if (length(sections) > 1) {
          remaining_sections <- as.list(sections[-1])
        }
      }
    }
  }
  
  if (is.null(header)) {
    title_suffix <- if (!is.null(module)) {
      switch(tolower(module),
        "dp" = "Data Processing Module",
        "de" = "Differential Expression Module",
        "ea" = "Enrichment Analysis Module",
        "fs" = "Feature Selection Module",
        ""
      )
    } else {
      ""
    }
    header <- paste0(
      "# EasyOmiFun Analysis Report\n",
      if (nzchar(title_suffix)) paste0("## Module: ", title_suffix, "\n") else "",
      "_Report generated on: ", format(Sys.Date(), "%B %d, %Y"), "_\n\n"
    )
  }
  
  new_section_sec <- paste0(
    "## ", section_title, "\n\n",
    content_md, "\n\n",
    "_Recorded at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "_"
  )
  
  found <- FALSE
  target_title <- paste0("## ", section_title)
  
  if (length(remaining_sections) > 0) {
    for (i in 1:length(remaining_sections)) {
      sec_trimmed <- trimws(remaining_sections[[i]])
      if (startsWith(sec_trimmed, target_title)) {
        remaining_sections[[i]] <- new_section_sec
        found <- TRUE
        break
      }
    }
  }
  
  if (!found) {
    remaining_sections[[length(remaining_sections) + 1]] <- new_section_sec
  }
  
  new_content <- paste(c(header, remaining_sections), collapse = "\n\n---\n\n")
  new_content <- paste0(new_content, "\n\n---\n\n")
  
  writeLines(new_content, report_path)
  cat(sprintf("[REPORT] Appended/Updated section '%s' for user '%s' in module '%s'.\n", section_title, user_id, module %||% "default"))
  return(TRUE)
}

remove_sections_from_report <- function(user_id, step, ds_id = NULL, module = NULL) {
  if (is.null(user_id) || !nzchar(user_id)) {
    user_id <- if (!is.null(ds_id)) get_user_id(ds_id) else "user"
  }
  if (is.null(user_id) || !nzchar(user_id)) user_id <- "user"
  
  # Determine report file name
  report_fname <- "analysis_report.md" # fallback
  if (!is.null(module) && nzchar(module)) {
    report_fname <- sprintf("analysis_report_%s.md", tolower(module))
  }
  
  report_path <- file.path("tmp/user_sessions", user_id, report_fname)
  if (!file.exists(report_path)) {
    report_path_tmp <- file.path("tmp", report_fname)
    if (file.exists(report_path_tmp)) {
      report_path <- report_path_tmp
    } else {
      return(FALSE)
    }
  }
  
  # Read the report content
  lines <- readLines(report_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  if (!nzchar(content)) return(FALSE)
  
  # Split sections using the separator "\n\n---\n\n"
  sections <- strsplit(content, "\n\n---\n\n", fixed = TRUE)[[1]]
  if (length(sections) <= 1) {
    return(FALSE)
  }
  
  # The first element is the header
  header <- sections[1]
  remaining_sections <- sections[-1]
  
  # Collect all possible identifier strings for this dataset
  id_tokens <- character(0)
  if (!is.null(ds_id) && nzchar(ds_id)) {
    base_id <- get_base_id(ds_id)
    ds_name <- get_dataset_name(base_id)
    if (!is.null(ds_name) && nzchar(ds_name)) id_tokens <- c(id_tokens, ds_name)
    
    meta_path <- get_session_path(base_id, "%s_expr_metadata.rds")
    if (!file.exists(meta_path)) {
      meta_path <- sprintf("tmp/%s_expr_metadata.rds", base_id)
    }
    if (file.exists(meta_path)) {
      meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
      if (!is.null(meta$datasetName) && nzchar(meta$datasetName)) {
        id_tokens <- c(id_tokens, meta$datasetName)
      }
      if (!is.null(meta$name) && nzchar(meta$name)) {
        id_tokens <- c(id_tokens, meta$name)
      }
    }
    short_base <- sub("^usr_[^_]+_", "", base_id)
    id_tokens <- c(id_tokens, ds_id, base_id, short_base,
                   sub("_(dp|de|fs|ea)$", "", base_id),
                   sub("_(dp|de|fs|ea)$", "", short_base),
                   gsub("_", " ", short_base),
                   sub("^(dp|de|fs|ea)_", "", base_id))
    id_tokens <- unique(id_tokens[nzchar(id_tokens)])
  }
  
  step_clean <- tolower(trimws(as.character(step)))
  patterns <- character(0)
  if (step_clean %in% c("annotation")) {
    patterns <- c("## Gene Annotation & Identifier Mapping")
  } else if (step_clean %in% c("processing")) {
    patterns <- c("## Data Filtering & Missing Value Imputation")
  } else if (step_clean %in% c("normalization", "normalization-counts", "normalization-others", "normalization_counts", "normalization_others")) {
    patterns <- c("## Expression Normalization")
  } else if (step_clean %in% c("batch")) {
    patterns <- c("## Batch Effect Correction")
  } else if (step_clean %in% c("de", "inline-de", "inline_de", "de-analysis", "de_analysis")) {
    patterns <- c("## Differential Expression Analysis")
  } else if (step_clean %in% c("meta", "inline-de-meta", "inline_de_meta", "de-meta", "de_meta", "meta-analysis", "meta_analysis")) {
    patterns <- c("## Differential Expression Meta-Analysis")
  } else if (step_clean %in% c("fs", "inline-fs", "inline_fs", "feature-selection", "feature_selection", "cv", "cross-validation", "cross_validation", "testing")) {
    patterns <- c("## Feature Selection")
  } else if (step_clean %in% c("ea", "inline-ea", "inline_ea", "inline-enrichment", "enrichment", "enrichment-analysis", "enrichment_analysis")) {
    patterns <- c("## Enrichment Analysis")
  }
  
  if (length(patterns) == 0) {
    return(FALSE)
  }
  
  keep_idx <- sapply(remaining_sections, function(sec) {
    sec_trimmed <- trimws(sec)
    for (pat in patterns) {
      if (startsWith(sec_trimmed, pat)) {
        # If no dataset IDs were specified, or this is a global / multi-dataset section without dash:
        if (length(id_tokens) == 0 || !grepl(" — ", sec_trimmed, fixed = TRUE)) {
          return(FALSE)
        }
        
        # Check if title line matches any dataset token
        title_line <- strsplit(sec_trimmed, "\n")[[1]][1]
        for (token in id_tokens) {
          if (grepl(tolower(token), tolower(title_line), fixed = TRUE)) {
            return(FALSE)
          }
        }
      }
    }
    return(TRUE)
  })
  
  filtered_sections <- remaining_sections[keep_idx]
  
  # Reconstruct report
  new_content <- paste(c(header, filtered_sections), collapse = "\n\n---\n\n")
  new_content <- paste0(new_content, "\n\n---\n\n")
  
  writeLines(new_content, report_path)
  cat(sprintf("[REPORT] Removed %d sections for step '%s' from report %s\n", sum(!keep_idx), step, report_fname))
  return(TRUE)
}

save_temp_csv <- function(dataset_id, type, columns, parsed_data) {
  dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
  file_path <- sprintf("tmp/%s_%s.csv", dataset_id, type)
  
  if (is.null(parsed_data) || length(parsed_data) == 0) {
    return(list(status = "success", message = "No data to write"))
  }
  
  if (is.matrix(parsed_data)) {
    df <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
  } else {
    max_len <- max(sapply(parsed_data, length))
    rows <- lapply(parsed_data, function(x) {
      x_char <- as.character(sapply(x, function(val) if (is.null(val)) "" else val))
      if (length(x_char) < max_len) c(x_char, rep("", max_len - length(x_char))) else x_char
    })
    mat <- matrix(unlist(rows), nrow = length(rows), byrow = TRUE)
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
  }
  
  if (!is.null(columns) && length(columns) == ncol(df)) {
    colnames(df) <- columns
  }
  
  write.csv(df, file = file_path, row.names = FALSE)
  return(list(status = "success", file = file_path))
}

ensure_sequential_matrices <- function(ds_id) {
  ds_id <- get_base_id(ds_id)
  processed_path <- get_session_path(ds_id, "%s_processed_expr.rds")
  normalized_path <- get_session_path(ds_id, "%s_normalized_expr.rds")
  normalized_actual <- get_session_path(ds_id, "%s_normalized_expr_actual.rds")
  batch_path <- get_session_path(ds_id, "%s_batch_expr.rds")
  batch_actual <- get_session_path(ds_id, "%s_batch_expr_actual.rds")
  expr_path <- get_session_path(ds_id, "%s_expr_matrix.rds")
  annotated_path <- get_session_path(ds_id, "%s_expr_matrix_annotated.rds")
  
  stack_file <- get_session_path(ds_id, "%s_main_stack.rds")
  stack <- if (file.exists(stack_file)) tryCatch(readRDS(stack_file), error = function(e) list()) else list()
  if (!is.list(stack)) stack <- list()
  
  if (length(stack) == 0) {
    backup_stack_file <- get_session_path(ds_id, "%s_main_stack_original_backup.rds")
    if (file.exists(backup_stack_file)) {
      b_stack <- tryCatch(readRDS(backup_stack_file), error = function(e) list())
      if (is.list(b_stack) && length(b_stack) > 0 && !is.null(b_stack[[1]]$data)) {
        stack <- list(b_stack[[1]])
        saveRDS(stack, stack_file, compress = FALSE)
      }
    }
  }
  
  if (is.list(stack) && length(stack) > 0) {
    upload_data <- NULL
    normalization_data <- NULL
    annotation_data <- NULL
    processing_data <- NULL
    batch_data <- NULL
    
    for (entry in stack) {
      if (identical(entry$step, "upload")) upload_data <- entry$data
      if (identical(entry$step, "normalization")) normalization_data <- entry$data
      if (identical(entry$step, "annotation")) annotation_data <- entry$data
      if (identical(entry$step, "processing")) processing_data <- entry$data
      if (identical(entry$step, "batch")) batch_data <- entry$data
    }
    
    dataType <- "readcounts"
    meta_path <- get_session_path(ds_id, "%s_expr_metadata.rds")
    if (file.exists(meta_path)) {
      meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
      if (!is.null(meta) && !is.null(meta$dataType)) {
        dataType <- tolower(meta$dataType)
      }
    }
    
    current_data <- if (!is.null(upload_data)) upload_data else stack[[1]]$data
    
    final_normalization_data <- NULL
    final_annotation_data <- NULL
    final_processing_data <- NULL
    final_batch_data <- NULL
    
    if (dataType == "microarray") {
      # Order: upload -> normalization -> annotation -> processing -> batch
      if (!is.null(normalization_data)) {
        current_data <- normalization_data
        saveRDS(normalization_data, normalized_actual)
      }
      final_normalization_data <- current_data
      
      if (!is.null(annotation_data)) {
        current_data <- annotation_data
      }
      final_annotation_data <- current_data
      
      if (!is.null(processing_data)) {
        current_data <- processing_data
      }
      final_processing_data <- current_data
      
      if (!is.null(batch_data)) {
        current_data <- batch_data
        saveRDS(batch_data, batch_actual)
      }
      final_batch_data <- current_data
    } else {
      # Order: upload -> annotation -> processing -> normalization -> batch
      if (!is.null(annotation_data)) {
        current_data <- annotation_data
      }
      final_annotation_data <- current_data
      
      if (!is.null(processing_data)) {
        current_data <- processing_data
      }
      final_processing_data <- current_data
      
      if (!is.null(normalization_data)) {
        current_data <- normalization_data
        saveRDS(normalization_data, normalized_actual)
      }
      final_normalization_data <- current_data
      
      if (!is.null(batch_data)) {
        current_data <- batch_data
        saveRDS(batch_data, batch_actual)
      }
      final_batch_data <- current_data
    }
    
    uid <- get_user_id(ds_id)
    if (!is.null(annotation_data)) {
      saveRDS(final_annotation_data, annotated_path)
      annotated_csv_path <- get_session_path(ds_id, "%s_annotated_matrix.csv")
      tryCatch(write.csv(get_formatted_matrix_df(ds_id, final_annotation_data), annotated_csv_path, row.names = FALSE), error = function(e) NULL)
      register_export_file(uid, "annotated_matrix", ds_id, annotated_path, "dp", "annotation", ext = "csv")
      
      anno_res_path <- get_session_path(ds_id, "%s_annotation_results.rds")
      if (file.exists(anno_res_path)) {
        register_export_file(uid, "annotation_results", ds_id, anno_res_path, "dp", "annotation", ext = "csv")
      } else {
        anno_res_csv <- get_session_path(ds_id, "%s_annotation_results.csv")
        if (file.exists(anno_res_csv)) {
          register_export_file(uid, "annotation_results", ds_id, anno_res_csv, "dp", "annotation", ext = "csv")
        }
      }

      unmapped_res_path <- get_session_path(ds_id, "%s_unmapped_results.rds")
      if (file.exists(unmapped_res_path)) {
        register_export_file(uid, "unmapped_results", ds_id, unmapped_res_path, "dp", "annotation", ext = "csv")
      } else {
        unmapped_res_csv <- get_session_path(ds_id, "%s_unmapped_results.csv")
        if (file.exists(unmapped_res_csv)) {
          register_export_file(uid, "unmapped_results", ds_id, unmapped_res_csv, "dp", "annotation", ext = "csv")
        }
      }
    } else {
      if (file.exists(annotated_path)) file.remove(annotated_path)
      annotated_csv_path <- get_session_path(ds_id, "%s_annotated_matrix.csv")
      if (file.exists(annotated_csv_path)) file.remove(annotated_csv_path)
      anno_res_path <- get_session_path(ds_id, "%s_annotation_results.rds")
      if (file.exists(anno_res_path)) file.remove(anno_res_path)
      anno_res_csv <- get_session_path(ds_id, "%s_annotation_results.csv")
      if (file.exists(anno_res_csv)) file.remove(anno_res_csv)
      unmapped_res_path <- get_session_path(ds_id, "%s_unmapped_results.rds")
      if (file.exists(unmapped_res_path)) file.remove(unmapped_res_path)
      unmapped_res_csv <- get_session_path(ds_id, "%s_unmapped_results.csv")
      if (file.exists(unmapped_res_csv)) file.remove(unmapped_res_csv)
    }
    if (!is.null(processing_data)) {
      saveRDS(final_processing_data, processed_path)
      processed_csv_path <- get_session_path(ds_id, "%s_processed_matrix.csv")
      tryCatch(write.csv(get_formatted_matrix_df(ds_id, final_processing_data), processed_csv_path, row.names = FALSE), error = function(e) NULL)
      register_export_file(uid, "processed_matrix", ds_id, processed_path, "dp", "processing", ext = "csv")
    } else {
      if (file.exists(processed_path)) file.remove(processed_path)
      processed_csv_path <- get_session_path(ds_id, "%s_processed_matrix.csv")
      if (file.exists(processed_csv_path)) file.remove(processed_csv_path)
    }
    if (!is.null(normalization_data)) {
      saveRDS(final_normalization_data, normalized_path)
      normalized_csv_path <- get_session_path(ds_id, "%s_normalized_matrix.csv")
      tryCatch(write.csv(get_formatted_matrix_df(ds_id, final_normalization_data), normalized_csv_path, row.names = FALSE), error = function(e) NULL)
      register_export_file(uid, "normalized_matrix", ds_id, normalized_path, "dp", "normalization", ext = "csv")
    } else {
      if (file.exists(normalized_path)) file.remove(normalized_path)
      if (file.exists(normalized_actual)) file.remove(normalized_actual)
      normalized_csv_path <- get_session_path(ds_id, "%s_normalized_matrix.csv")
      if (file.exists(normalized_csv_path)) file.remove(normalized_csv_path)
    }
    if (!is.null(batch_data)) {
      saveRDS(final_batch_data, batch_path)
      batch_csv_path <- get_session_path(ds_id, "%s_batch_corrected_matrix.csv")
      tryCatch(write.csv(get_formatted_matrix_df(ds_id, final_batch_data), batch_csv_path, row.names = FALSE), error = function(e) NULL)
      register_export_file(uid, "batch_corrected_matrix", ds_id, batch_path, "dp", "batch", ext = "csv")
    } else {
      if (file.exists(batch_path)) file.remove(batch_path)
      if (file.exists(batch_actual)) file.remove(batch_actual)
      batch_csv_path <- get_session_path(ds_id, "%s_batch_corrected_matrix.csv")
      if (file.exists(batch_csv_path)) file.remove(batch_csv_path)
    }
    
    saveRDS(current_data, expr_path)
    register_export_file(uid, "final_matrix", ds_id, expr_path, "dp", "final", ext = "csv")
  } else {
    # Fallback: if no stack exists, do NOT create processed/normalized/batch files
    # because they only exist when the steps are performed. Clean them up.
    if (file.exists(processed_path)) file.remove(processed_path)
    if (file.exists(annotated_path)) file.remove(annotated_path)
    if (file.exists(normalized_path)) file.remove(normalized_path)
    if (file.exists(normalized_actual)) file.remove(normalized_actual)
    if (file.exists(batch_path)) file.remove(batch_path)
    if (file.exists(batch_actual)) file.remove(batch_actual)
  }
}

apply_direction_filter <- function(df, dir) {
  if (is.null(dir) || dir == "all" || is.null(df) || nrow(df) == 0) return(df)
  dir_lower <- tolower(dir)
  if ("direction" %in% colnames(df)) {
    return(df[tolower(as.character(df$direction)) == dir_lower, ])
  }
  if ("dir" %in% colnames(df)) {
    return(df[tolower(as.character(df$dir)) == dir_lower, ])
  }
  
  # Check effect size / logFC columns where > 0 is up, < 0 is down
  effect_cols <- c(
    "logFC", "LogFC", "log2FoldChange", "fc", "FC",
    "Combined Effects Size", "Combined.Effects.Size",
    "Combined Effect Size", "Combined.Effect.Size",
    "Combined LogFC", "Combined.LogFC",
    "hedges_g", "combined_hedges_g", "g", "d", "stat", "score", "EA_Rank_Score"
  )
  for (col in effect_cols) {
    if (col %in% colnames(df)) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      if (sum(!is.na(vals)) > 0) {
        if (dir_lower == "up")   return(df[!is.na(vals) & vals > 0, ])
        if (dir_lower == "down") return(df[!is.na(vals) & vals < 0, ])
      }
    }
  }
  
  # Check FoldChange column (ratio where > 1 is up, < 1 is down)
  fc_cols <- c("FoldChange", "Fold Change", "fold_change")
  for (col in fc_cols) {
    if (col %in% colnames(df)) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      if (sum(!is.na(vals)) > 0) {
        if (dir_lower == "up")   return(df[!is.na(vals) & vals > 1, ])
        if (dir_lower == "down") return(df[!is.na(vals) & vals < 1 & vals > 0, ])
      }
    }
  }
  
  return(df)
}

find_de_files <- function(ds_id, b_id, target_is_proteomics, user_prefix, parent_module = NULL) {
  files <- c()
  
  is_dp <- grepl("_dp", ds_id) || grepl("_dp", b_id)
  target_module <- parent_module %||% (if (is_dp) "dp" else "de")

  # 1. First check if the requested ds_id itself has a single DE results file.
  check_paths <- c()
  if (target_module == "dp") {
    check_paths <- c(
      get_session_path(ds_id, "%s_dp_de_results.csv"),
      get_session_path(ds_id, "%s_de_results.csv"),
      sprintf("tmp/%s_dp_de_results.csv", ds_id),
      sprintf("tmp/%s_de_results.csv", ds_id)
    )
    if (b_id != "" && b_id != ds_id) {
      check_paths <- c(check_paths,
        get_session_path(b_id, "%s_dp_de_results.csv"),
        get_session_path(b_id, "%s_de_results.csv"),
        sprintf("tmp/%s_dp_de_results.csv", b_id),
        sprintf("tmp/%s_de_results.csv", b_id)
      )
    }
  } else {
    check_paths <- c(
      get_session_path(ds_id, "%s_de_results.csv"),
      sprintf("tmp/%s_de_results.csv", ds_id)
    )
    if (b_id != "" && b_id != ds_id) {
      check_paths <- c(check_paths,
        get_session_path(b_id, "%s_de_results.csv"),
        sprintf("tmp/%s_de_results.csv", b_id)
      )
    }
  }
  
  for (f in check_paths) {
    if (file.exists(f)) files <- c(files, f)
  }
  if (length(files) > 0 && !grepl("_meta$", ds_id) && !grepl("_meta$", b_id)) {
    if (target_module == "dp") {
      files <- files[grepl("_dp", basename(files))]
    } else {
      files <- files[!grepl("_dp", basename(files))]
    }
    if (length(files) > 0) return(unique(files))
  }
  
  # 2. If meta-dataset or single dataset not found by exact path, search ONLY files belonging to this specific module and user
  user_sess_dir <- sprintf("tmp/user_sessions/%s", user_prefix)
  all_meta_files <- c()
  if (dir.exists(user_sess_dir)) {
    all_meta_files <- list.files(user_sess_dir, pattern = sprintf("^%s_.*_%s_expr_metadata\\.rds$", user_prefix, target_module), full.names = TRUE)
    if (length(all_meta_files) == 0 && target_module == "dp") {
      all_meta_files <- list.files(user_sess_dir, pattern = sprintf("^%s_.*_expr_metadata\\.rds$", user_prefix), full.names = TRUE)
      all_meta_files <- all_meta_files[!grepl("_upload_", all_meta_files) & !grepl("_(de|fs|ea|meta)_", all_meta_files)]
    }
  }
  
  valid_ds_ids <- c()
  for (meta_f in all_meta_files) {
    m_info <- tryCatch(readRDS(meta_f), error = function(e) NULL)
    if (!is.null(m_info)) {
      m_dtype <- if (!is.null(m_info$dataType)) m_info$dataType else "readcounts"
      is_prot <- (m_dtype == "proteomics")
      if (is_prot == target_is_proteomics) {
        fname <- basename(meta_f)
        ds_id_extracted <- gsub("_expr_metadata\\.rds$", "", fname)
        valid_ds_ids <- c(valid_ds_ids, ds_id_extracted)
      }
    }
  }
  valid_ds_ids <- unique(valid_ds_ids)
  
  for (v_id in valid_ds_ids) {
    v_paths <- c(
      get_session_path(v_id, "%s_de_results.csv"),
      get_session_path(v_id, "%s_dp_de_results.csv"),
      sprintf("tmp/%s_de_results.csv", v_id),
      sprintf("tmp/%s_dp_de_results.csv", v_id)
    )
    for (f in v_paths) {
      if (file.exists(f)) files <- c(files, f)
    }
  }
  
  # Module-strict filtering: if target_module is "dp", exclude any standalone DE files without _dp
  files <- unique(files)
  if (target_module == "dp") {
    files <- files[grepl("_dp", basename(files))]
  } else {
    files <- files[!grepl("_dp", basename(files))]
  }
  
  return(unique(files))
}

find_gene_col <- function(df, pref = NULL) {
  if (is.null(df) || ncol(df) == 0) return("gene")
  if (!is.null(pref) && pref != "" && pref %in% colnames(df)) return(pref)
  if ("Feature" %in% colnames(df)) return("Feature")
  if ("Features" %in% colnames(df)) return("Features")
  if ("gene" %in% colnames(df)) return("gene")
  if ("Gene" %in% colnames(df)) return("Gene")
  return(colnames(df)[1])
}

# Lazy package loader - loads packages globally in the current R session
load_packages_globally <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("[LAZY LOAD] Optional/required package '%s' is not installed in current library path.\n", pkg))
      next
    }
    if (!paste0("package:", pkg) %in% search()) {
      cat(sprintf("[LAZY LOAD] Loading package globally: %s...\n", pkg))
      suppressMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    }
  }
}

generate_meta_plots <- function(full_rows, meta_ds_id, pval_thresh, logfc_thresh, data_class = NULL) {
  tryCatch({
    load_packages_globally("ggplot2")
    if (is.null(full_rows) || length(full_rows) == 0) return(NULL)
    
    # Convert full_rows to data.frame if it is a list
    if (is.list(full_rows) && !is.data.frame(full_rows)) {
      df_meta <- do.call(rbind, lapply(full_rows, function(r) {
        # Ensure we don't have NULL values in the list elements
        row_list <- lapply(r, function(x) if (is.null(x)) NA else x)
        as.data.frame(row_list, stringsAsFactors = FALSE)
      }))
    } else {
      df_meta <- as.data.frame(full_rows)
    }
    
    if (is.null(df_meta) || nrow(df_meta) == 0) return(NULL)
    
    # Sort by pval/qval
    df_sorted <- df_meta[order(as.numeric(df_meta$pval)), ]
    top10 <- head(df_sorted, 10)
    
    if (is.null(data_class) || !nzchar(as.character(data_class))) {
      data_class <- get_dataset_data_class(meta_ds_id)
      if (data_class == "others" || is.null(data_class)) {
        u_id <- if (exists("get_user_id", mode = "function")) get_user_id(meta_ds_id) else ""
        data_class <- get_session_data_class(u_id)
      }
    }
    is_proteomics <- identical(as.character(data_class), "proteomics")
    y_lab <- if (is_proteomics) "Protein" else "Gene"
    y_face <- if (is_proteomics) "plain" else "italic"

    es_col <- if ("hedges_g" %in% colnames(df_meta)) "hedges_g" else 
              if ("g" %in% colnames(df_meta)) "g" else 
              if ("d" %in% colnames(df_meta)) "d" else NULL
              
    if (nrow(top10) > 0) {
      es_col_t10 <- if ("hedges_g" %in% colnames(top10)) "hedges_g" else 
                    if ("g" %in% colnames(top10)) "g" else 
                    if ("d" %in% colnames(top10)) "d" else NULL
                    
      df_forest <- data.frame(
        Gene = as.character(top10$gene),
        Estimate = if (!is.null(es_col_t10)) as.numeric(top10[[es_col_t10]]) else NA_real_,
        Lower = if (!is.null(top10$ci_lower)) as.numeric(top10$ci_lower) else {
          est <- if (!is.null(es_col_t10)) as.numeric(top10[[es_col_t10]]) else NA_real_
          est - 1.96 * as.numeric(top10$se)
        },
        Upper = if (!is.null(top10$ci_upper)) as.numeric(top10$ci_upper) else {
          est <- if (!is.null(es_col_t10)) as.numeric(top10[[es_col_t10]]) else NA_real_
          est + 1.96 * as.numeric(top10$se)
        },
        stringsAsFactors = FALSE
      )
      
      p_forest <- ggplot(df_forest, aes(x = Estimate, y = reorder(Gene, Estimate))) +
        geom_point(size = 3, color = "#2563eb") +
        geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "#1e293b", size = 0.8) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#94a3b8") +
        theme_minimal() +
        labs(x = "Combined Effect Size (Hedges' g)", y = y_lab) +
        theme(axis.text.y = element_text(face = y_face))
        
      forest_path <- get_session_path(meta_ds_id, "%s_forest_plots.pdf")
      ggplot2::ggsave(forest_path, p_forest, width = 7, height = 5)
      cat(sprintf("[META] Forest plot saved to %s [DataClass: %s | Omics Type: %s | y-axis: '%s' | font: '%s']\n",
                  forest_path,
                  data_class,
                  if (is_proteomics) "PROTEOMICS" else "TRANSCRIPTOMICS",
                  y_lab,
                  y_face))
    }
    
    # 2. Volcano Plot
    if (!is.null(es_col)) {
      df_meta$significant <- as.numeric(df_meta$qval) < pval_thresh & abs(as.numeric(df_meta[[es_col]])) >= logfc_thresh
      df_meta$direction <- ifelse(df_meta$significant, ifelse(as.numeric(df_meta[[es_col]]) > 0, "up", "down"), "ns")
      
      p_volc <- ggplot(df_meta, aes(x = as.numeric(.data[[es_col]]), y = -log10(as.numeric(qval)), color = direction)) +
        geom_point(size = 1.5, alpha = 0.7) +
        scale_color_manual(values = c("up" = "#ef4444", "down" = "#3b82f6", "ns" = "#94a3b8")) +
        geom_vline(xintercept = c(-logfc_thresh, logfc_thresh), linetype = "dashed", color = "grey") +
        geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed", color = "grey") +
        theme_minimal() + 
        labs(x = "Combined Hedges' g", y = "-log10(FDR)") +
        theme(
          legend.position = "right"
        )
        
      volcano_path <- get_session_path(meta_ds_id, "%s_volcano_plots.pdf")
      ggplot2::ggsave(volcano_path, p_volc, width = 7, height = 5)
      cat(sprintf("[META] Volcano plot saved to %s\n", volcano_path))
    }
    
  }, error = function(e) {
    cat(sprintf("[META][WARNING] Failed to generate meta-analysis plots: %s\n", e$message))
  })
}

# ----------------------------------------------------
# Plot Export Conversion Helper (pdftools)
# ----------------------------------------------------

convert_pdf_to_image <- function(src_pdf, dest_img, target_format = "png", dpi = 300) {
  if (!file.exists(src_pdf) || file.info(src_pdf)$size == 0) return(FALSE)

  target_format <- tolower(target_format)
  if (target_format == "jpg") target_format <- "jpeg"
  if (target_format == "tif") target_format <- "tiff"

  dest_dir <- dirname(dest_img)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

  # Primary converter: pdftools (cross-platform, wraps Poppler on Linux/macOS/Windows)
  if (requireNamespace("pdftools", quietly = TRUE)) {
    ok <- tryCatch({
      suppressWarnings(
        pdftools::pdf_convert(src_pdf, format = target_format, dpi = dpi,
                              filenames = dest_img, pages = 1, verbose = FALSE)
      )
      file.exists(dest_img) && file.info(dest_img)$size > 0
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
  }

  # Fallback 1: magick package if available
  if (requireNamespace("magick", quietly = TRUE)) {
    ok <- tryCatch({
      img <- magick::image_read_pdf(src_pdf, pages = 1, density = dpi)
      magick::image_write(img, path = dest_img, format = target_format)
      file.exists(dest_img) && file.info(dest_img)$size > 0
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
  }

  return(FALSE)
}

# ----------------------------------------------------
# Structured Manifest Registry Helpers
# ----------------------------------------------------

clean_manifest_field <- function(v) {
  if (is_empty_str(v)) return(NULL)
  v_un <- unlist(v)
  if (is.null(v_un) || length(v_un) == 0) return(NULL)
  v1 <- v_un[[1]]
  if (is.null(v1) || is.na(v1)) return(NULL)
  v_str <- trimws(as.character(v1))
  if (!nzchar(v_str) || v_str == "NA" || v_str == "null" || v_str == "NULL") return(NULL)
  v_str <- gsub("[:/\\\\?*\"<>| ]", "_", v_str)
  return(v_str)
}

register_export_file <- function(user_id, key, ds_id, path, module, step,
                                 model = NULL, db = NULL, ext = NULL,
                                 parentModule = NULL, isInline = NULL) {
  if (is_empty_str(user_id) || user_id == "user") {
    user_id <- get_user_id(ds_id)
  }
  if (is_empty_str(user_id)) return(FALSE)

  canonical_ds_id <- if (!is_empty_str(ds_id)) get_base_id(ds_id) else ""

  # Resolve parentModule and isInline from dataset metadata if not explicitly provided
  parsed_meta <- if (nzchar(canonical_ds_id)) get_backend_datasets(canonical_ds_id) else list()
  parent_mod_val <- parentModule %||% parsed_meta$parentModule %||% module
  is_inline_val  <- if (!is.null(isInline)) isTRUE(isInline) else isTRUE(parsed_meta$isInline)

  dir_path <- file.path("tmp/user_sessions", user_id)
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(dir_path, "manifest.json")

  manifest <- if (file.exists(manifest_path)) {
    tryCatch(jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
             error = function(e) list(version = 1, entries = list()))
  } else {
    list(version = 1, entries = list())
  }
  if (is.null(manifest$entries)) manifest$entries <- list()

  table_matrix_keys <- c(
    "annotated_matrix", "processed_matrix", "normalized_matrix", "batch_corrected_matrix", "final_matrix",
    "annotation_results", "unmapped_results", "unmapped_features",
    "de_all_results", "de_sig_results", "meta_results", "heterogeneity_report",
    "fs_training_results", "fs_testing_results", "cv_results", "all_models_performance_train",
    "all_models_performance_test", "all_models_performance_cv", "performance_metrics",
    "confusion_matrix", "fs_train_clin", "fs_test_clin", "clinical"
  )
  if (key %in% table_matrix_keys || grepl("(matrix|results|report|importance|features|performance)", key)) {
    ext_val <- ext %||% "csv"
    if (ext_val == "rds") ext_val <- "csv"
  } else {
    ext_val <- ext %||% tools::file_ext(path)
  }
  if (is_empty_str(ext_val)) ext_val <- "csv"

  norm_model <- clean_manifest_field(model)
  norm_db    <- clean_manifest_field(db)

  clean_rel_path <- basename(safe_str(path))
  clean_ext <- tools::file_ext(clean_rel_path)
  clean_no_ext <- tools::file_path_sans_ext(clean_rel_path)
  if (nzchar(clean_ext)) {
    clean_rel_path <- sprintf("%s.%s", gsub("[:/\\\\?*\"<>| ]", "_", clean_no_ext), clean_ext)
  } else {
    clean_rel_path <- gsub("[:/\\\\?*\"<>| ]", "_", clean_rel_path)
  }

  new_entry <- list(
    key          = key,
    dsId         = if (nzchar(canonical_ds_id)) canonical_ds_id else jsonlite::unbox(NA),
    model        = if (!is.null(norm_model)) norm_model else jsonlite::unbox(NA),
    db           = if (!is.null(norm_db)) norm_db else jsonlite::unbox(NA),
    path         = clean_rel_path,
    ext          = ext_val,
    module       = module,
    parentModule = parent_mod_val,
    isInline     = is_inline_val,
    step         = step
  )

  # Filter out existing duplicate entry matching key + canonical dsId + model + db
  manifest$entries <- Filter(function(e) {
    e_ds    <- get_base_id(e$dsId)
    e_model <- clean_manifest_field(e$model)
    e_db    <- clean_manifest_field(e$db)

    !(identical(e$key, key) && (identical(safe_str(e$dsId), canonical_ds_id) || identical(e_ds, canonical_ds_id)) &&
      identical(e_model, norm_model) && identical(e_db, norm_db))
  }, manifest$entries)

  manifest$entries[[length(manifest$entries) + 1]] <- new_entry
  tryCatch({
    jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = FALSE)
    TRUE
  }, error = function(e) FALSE)
}

load_manifest <- function(uid) {
  if (is_empty_str(uid)) return(list(version = 1, entries = list()))
  p <- file.path("tmp/user_sessions", uid, "manifest.json")
  if (!file.exists(p)) return(list(version = 1, entries = list()))
  tryCatch({
    res <- jsonlite::fromJSON(p, simplifyVector = FALSE)
    if (is.null(res$entries)) res$entries <- list()
    res
  }, error = function(e) list(version = 1, entries = list()))
}

find_manifest_entry <- function(manifest, key, ds_id, model = NULL, db = NULL) {
  if (is.null(manifest) || is.null(manifest$entries) || length(manifest$entries) == 0) return(NULL)
  base_id <- get_base_id(ds_id)

  target_model <- clean_manifest_field(model)
  target_db    <- clean_manifest_field(db)

  for (e in manifest$entries) {
    if (!identical(e$key, key)) next

    e_ds <- safe_str(e$dsId, "")
    e_base <- get_base_id(e_ds)
    ds_match <- (nzchar(ds_id) && (identical(e_ds, ds_id) || identical(e_base, base_id) ||
                identical(e_ds, base_id) || identical(e_base, ds_id))) ||
                (!nzchar(ds_id) && !nzchar(e_ds))
    if (!ds_match) next

    m_val <- clean_manifest_field(e$model)
    d_val <- clean_manifest_field(e$db)

    if (!is.null(target_model)) {
      m_match <- identical(m_val, target_model) || identical(d_val, target_model)
      if (!m_match) next
    }

    if (!is.null(target_db)) {
      d_match <- identical(d_val, target_db) || identical(m_val, target_db)
      if (!d_match) next
    }

    return(e)
  }
  NULL
}
