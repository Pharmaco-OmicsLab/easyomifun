#!/usr/bin/env Rscript
# install_packages.R
# Installs all R packages required by EasyOmiFun backend.
# Run once before starting the app: Rscript install_packages.R
#
# Custom Installation Order:
#   1. Pure-R prerequisites & reticulate
#   2. BiocManager
#   3. CRAN R packages (pinned versions)
#   4. Bioconductor packages (Bioc 3.20 pinned versions)

cat("=== EasyOmiFun: Installing R dependencies ===\n")


# ── Global options ────────────────────────────────────────────────────────────
options(download.file.method = "libcurl")
# 1-hour download timeout — Miniconda/packages can be slow on poor connections
options(timeout = 3600)
# ── Platform & Architecture Detection (arm64 vs x86_64 / x64) ─────────────────
sys_name <- Sys.info()[["sysname"]]
machine  <- Sys.info()[["machine"]]
r_arch   <- R.version$arch
is_mac   <- sys_name == "Darwin"
is_win   <- .Platform$OS.type == "windows"
is_linux <- sys_name == "Linux"
is_arm64 <- identical(machine, "arm64") || identical(machine, "aarch64") || identical(r_arch, "aarch64")

cat(sprintf("Platform: %s | System Arch: %s | R Platform: %s\n", sys_name, machine, R.version$platform))

# Configure binary repository preferences and source compilation per architecture
if (is_win) {
  cat("  [arch] Detected Windows (x64) — using Windows binary packages.\n")
  options(pkgType = "win.binary")
  options(install.packages.compile.from.source = "both")
} else if (is_mac) {
  if (is_arm64) {
    cat("  [arch] Detected Apple Silicon (arm64) macOS — using arm64 binaries with source fallback.\n")
    options(pkgType = "mac.binary.big-sur-arm64")
  } else {
    cat("  [arch] Detected Intel (x86_64) macOS — using x86_64 binaries with source fallback.\n")
    options(pkgType = "mac.binary.big-sur-x86_64")
  }
  options(install.packages.compile.from.source = "both")
} else {
  cat(sprintf("  [arch] Detected Linux (%s) — compiling from source or distribution binaries.\n", machine))
  options(pkgType = "source")
  options(install.packages.compile.from.source = "always")
}
options(install.packages.ask = FALSE)
options(menu.graphics = FALSE)
options(device.ask.default = FALSE)
Sys.setenv(DEBIAN_FRONTEND = "noninteractive")
# Enable parallel downloads and package compilation/extraction where supported by R
options(Ncpus = max(1, parallel::detectCores()))

# ── CRAN mirror pool ─────────────────────────────────────────────────────────
# PPM (Posit Package Manager) is listed FIRST — it serves pre-built Windows/Mac/Linux
# binaries for virtually every CRAN package. Setting EASYOMIFUN_CRAN_SNAPSHOT (e.g. "2024-11-01")
# locks the repository to an exact historical date across all platforms.
snapshot_date <- Sys.getenv("EASYOMIFUN_CRAN_SNAPSHOT", unset = "")
ppm_mirror <- if (nzchar(snapshot_date)) {
  sprintf("https://packagemanager.posit.co/cran/%s", snapshot_date)
} else {
  "https://packagemanager.posit.co/cran/latest"
}

cran_mirrors <- unique(c(
  ppm_mirror,
  "https://packagemanager.posit.co/cran/latest",  # PPM pre-built binary pool
  "https://cloud.r-project.org",                  # Official CRAN CDN (Mirror 0 HTTPS - most reliable)
  "https://cran.rstudio.com"                      # Posit/RStudio CDN (Fast global fallback)
))

# ── Connectivity pre-check: promote the first reachable mirror ───────────────
# Probes each mirror with a lightweight connection. The first one that responds
# is promoted to slot #1 so installs don't waste time on dead endpoints.
cat("Checking CRAN mirror reachability...\n"); flush.console()
best_mirror <- NULL

# Temporarily lower connection timeout for pre-check (avoids hanging on dead URLs)
old_timeout <- getOption("timeout")
options(timeout = 5)

for (.m in cran_mirrors) {
  .ok <- tryCatch({
    con <- url(paste0(.m, "/src/contrib/PACKAGES"), open = "r")
    close(con)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (.ok) {
    best_mirror <- .m
    cat(sprintf("  [mirror] Using: %s\n", best_mirror)); flush.console()
    break
  } else {
    cat(sprintf("  [mirror] Unreachable: %s\n", .m)); flush.console()
  }
}

# Restore standard timeout for actual package downloads
options(timeout = old_timeout)

if (is.null(best_mirror)) {
  cat("  [mirror] WARNING: No CRAN mirror reachable — will try all in order.\n")
  flush.console()
  best_mirror <- cran_mirrors[1]
}

# Put the working mirror first; keep the full list as ordered fallbacks.
cran_mirrors <- unique(c(best_mirror, cran_mirrors))
options(repos = setNames(cran_mirrors, c("CRAN", paste0("CRAN_", seq_along(cran_mirrors[-1])))))

# ── Local writable R library path ─────────────────────────────────────────────
# 1. EASYOMIFUN_RLIB takes highest precedence (explicit build/target path).
# 2. In desktop mode (Electron), R_LIBS_USER points to userData/R-lib.
# 3. Otherwise (packaging / local scripts), default strictly to script-relative R-lib/.
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name   <- "--file="
script.name     <- sub(file.arg.name, "",
                       initial.options[grep(file.arg.name, initial.options)])
script.dir <- if (length(script.name) > 0) dirname(normalizePath(script.name)) else getwd()

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
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
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

cat(sprintf("Using R library: %s\n", local_lib))
flush.console()

# ── Package Definitions (CRAN & Bioconductor pinned versions) ───────────────
# Pinned to easyomifun conda env / Bioconductor 3.20
cran_pkgs <- c(
    "shiny",          # 1.14.0
    "jsonlite",       # 2.0.0
    "yaml",           # 2.3.12
    "callr",          # 3.7.6
    "later",          # 1.4.8
    "rmarkdown",      # 2.31  (pulls in markdown + commonmark as dependencies)
    # DE analysis
    "metapro",        # 1.5.11
    "metafor",        # 5.0.1
    # Feature selection
    "caret",          # 7.0.1
    "Boruta",         # 9.0.0
    "randomForest",   # 4.7.1.2
    "e1071",          # 1.7.17
    "glmnet",         # 5.0
    "MASS",           # 7.3.66
    "ranger",         # 0.18.0
    "gbm",            # 2.3.1
    "pROC",           # 1.19.0.1
    "ggthemes",       # 5.2.0
    "segmented",      # 2.2.1
    # Plotting / utilities / formats
    "ggplot2",        # 3.5.2
    "ggridges",       # 0.5.7
    "matrixStats",    # 1.5.0
    "base64enc",      # 0.1.6
    "zip",            # 3.0.2
    "writexl",        # 2.0.0
    "openxlsx",       # 4.2.9
    "pdftools",       # 3.9.0
    "snow",           # 0.4.4
    "doParallel",     # 1.0.17
    "foreach",        # 1.5.2
    "iterators",      # 1.0.14
    "pheatmap"        # 1.0.13
  )

  # Named list of pinned CRAN versions.
  cran_versions <- list(
    "shiny"        = "1.14.0",
    "jsonlite"     = "2.0.0",
    "yaml"         = "2.3.12",
    "callr"        = "3.8.0",
    "later"        = "1.4.8",
    "rmarkdown"    = "2.32",
    "metapro"      = "1.5.11",
    "metafor"      = "5.0.1",
    "caret"        = "7.0.1",
    "Boruta"       = "9.0.0",
    "randomForest" = "4.7.1.2",
    "e1071"        = "1.7.17",
    "glmnet"       = "5.0",
    "MASS"         = "7.3.64",
    "ranger"       = "0.18.0",
    "gbm"          = "2.3.1",
    "pROC"         = "1.19.1",
    "ggthemes"     = "6.0.0",
    "segmented"    = "2.2.1",
    "ggplot2"      = "4.0.3",
    "ggridges"     = "0.5.7",
    "matrixStats"  = "1.5.0",
    "base64enc"    = "0.1.6",
    "zip"          = "3.0.2",
    "writexl"      = "2.0.1",
    "openxlsx"     = "4.2.9",
    "pdftools"     = "3.9.1",
    "snow"         = "0.4.4",
    "doParallel"   = "1.0.17",
    "foreach"      = "1.5.2",
    "iterators"    = "1.0.14",
    "pheatmap"     = "1.0.13",
    "reticulate"   = "1.47.0"
  )

  # ── Pinned Bioconductor versions (locked to easyomifun conda env) ──────────
  bioc_pkgs <- c(
    "AnnotationDbi",      # 1.68.0
    "DESeq2",             # 1.46.0
    "edgeR",              # 4.4.2
    "limma",              # 3.62.2
    "clusterProfiler",    # 4.14.0
    "enrichplot",         # 1.26.1
    "ReactomePA",        # 1.50.0  — Reactome pathway enrichment
    "reactome.db",       # 1.89.0  — Reactome annotation DB (required by ReactomePA)
    #"GO.db",             # 3.20.0  — Gene Ontology annotation DB
    "msigdbr",           # 26.1.0  — MSigDB gene sets (hallmark, KEGG, etc.)
    "org.Hs.eg.db",       # 3.20.0  — Homo sapiens
    "org.Mm.eg.db",      # 3.20.0  — Mus musculus
    "org.Rn.eg.db",      # 3.20.0  — Rattus norvegicus
    "org.Ss.eg.db",      # 3.20.0  — Sus scrofa (pig)
    "org.Gg.eg.db",      # 3.20.0  — Gallus gallus (chicken)
    # Processing dependencies
    "preprocessCore",     # 1.68.0
    "vsn",
    "SummarizedExperiment", # 1.36.0
    "sva",                # 3.54.0
    "impute"
  )

  # Named list of pinned Bioc versions
  bioc_versions <- list(
    "AnnotationDbi"        = "1.68.0",
    "DESeq2"               = "1.46.0",
    "edgeR"                = "4.4.2",
    "limma"                = "3.62.2",
    "clusterProfiler"      = "4.14.6",
    "enrichplot"           = "1.26.6",
    # Optional pathway / annotation packages
    "ReactomePA"           = "1.50.0",
    "reactome.db"          = "1.89.0",
    "GO.db"                = "3.20.0",
    "msigdbr"              = "26.1.1",
    # Organism annotation DBs
    "org.Hs.eg.db"         = "3.20.0",
    "org.Mm.eg.db"         = "3.20.0",
    "org.Rn.eg.db"         = "3.20.0",
    "org.Ss.eg.db"         = "3.20.0",
    "org.Gg.eg.db"         = "3.20.0",
    # Processing dependencies
    "preprocessCore"       = "1.68.0",
    "SummarizedExperiment" = "1.36.0",
    "sva"                  = "3.54.0"
  )

# ── Helper: version string normalization ──────────────────────────────────────
normalize_ver <- function(v) {
  if (is.null(v) || is.na(v)) return("")
  gsub("-", ".", v, fixed = TRUE)
}

# ── Helper: get installed package version ─────────────────────────────────────
get_pkg_version <- function(pkg) {
  tryCatch({
    as.character(utils::packageVersion(pkg, lib.loc = .libPaths()))
  }, error = function(e) NA_character_)
}

# ── Helper: get first-level dependencies from DESCRIPTION file ────────────────
get_all_deps <- function(pkg) {
  pkg_path <- tryCatch(find.package(pkg, lib.loc = .libPaths(), quiet = TRUE), error = function(e) NULL)
  if (is.null(pkg_path) || length(pkg_path) == 0) {
    return(NULL)
  }
  
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return(NULL)
  }
  
  desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  if (is.null(desc)) return(NULL)
  
  deps <- c()
  for (field in c("Depends", "Imports", "LinkingTo")) {
    if (field %in% colnames(desc)) {
      val <- desc[1, field]
      if (!is.na(val) && nzchar(val)) {
        pkgs <- gsub("\\([^\\)]+\\)", "", val)
        pkgs <- unlist(strsplit(pkgs, ","))
        pkgs <- trimws(pkgs)
        pkgs <- pkgs[pkgs != "R" & nzchar(pkgs)]
        deps <- c(deps, pkgs)
      }
    }
  }
  deps
}

# ── Helper: check a package is installed and all its dependencies are also installed ──
is_pkg_ok <- function(pkg) {
  visited <- c()
  to_visit <- c(pkg)
  
  while (length(to_visit) > 0) {
    curr <- to_visit[1]
    to_visit <- to_visit[-1]
    
    if (curr %in% visited) next
    
    # Check if this package itself is installed and has a valid, parseable DESCRIPTION
    pkg_path <- tryCatch(find.package(curr, lib.loc = .libPaths(), quiet = TRUE), error = function(e) NULL)
    if (is.null(pkg_path) || length(pkg_path) == 0) {
      return(FALSE)
    }
    desc_path <- file.path(pkg_path, "DESCRIPTION")
    if (!file.exists(desc_path)) {
      return(FALSE)
    }
    desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
    if (is.null(desc)) {
      return(FALSE)
    }
    
    visited <- c(visited, curr)
    
    # Get dependencies
    deps <- get_all_deps(curr)
    # Exclude base R packages
    base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", "utils")
    deps <- deps[!(deps %in% base_pkgs)]
    
    # Add new dependencies to visit
    new_deps <- deps[!(deps %in% visited) & !(deps %in% to_visit)]
    to_visit <- c(to_visit, new_deps)
  }
  
  TRUE
}

# ── Helper: check package is installed, healthy AND matches target version ────
is_pkg_version_ok <- function(pkg) {
  if (!is_pkg_ok(pkg)) return(FALSE)
  
  desired <- if (pkg %in% bioc_pkgs && !is.null(bioc_versions[[pkg]])) {
    bioc_versions[[pkg]]
  } else if (!is.null(cran_versions[[pkg]])) {
    cran_versions[[pkg]]
  } else {
    NULL
  }
  
  if (is.null(desired) || desired == "(any)") return(TRUE)
  
  inst_ver <- get_pkg_version(pkg)
  if (is.na(inst_ver)) return(FALSE)
  
  normalize_ver(inst_ver) == normalize_ver(desired)
}

# ── Helper: check a package can actually be loaded (for final verification) ───
is_pkg_loadable <- function(pkg) {
  tryCatch({ loadNamespace(pkg); TRUE }, error = function(e) FALSE)
}

# ── Helper: download file with retry ──────────────────────────────────────────
download_with_retry <- function(url, dest, mode = "wb", max_attempts = 3) {
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      cat(sprintf("  Retrying download attempt %d/%d for %s...\n", attempt, max_attempts, basename(url)))
      flush.console()
    }
    
    status <- tryCatch({
      download.file(url, dest, mode = mode, quiet = FALSE)
      TRUE
    }, error = function(e) {
      cat(sprintf("  [WARN] Download attempt %d failed: %s\n", attempt, conditionMessage(e)))
      flush.console()
      FALSE
    })
    
    if (isTRUE(status)) {
      return(TRUE)
    }
    
    if (attempt < max_attempts) {
      Sys.sleep(3)
    }
  }
  stop(sprintf("Failed to download file from %s after %d attempts.", url, max_attempts))
}

## ── Helper: install a single CRAN package with retry ──────────────────────────
# If `cran_versions[[pkg]]` is set, installs the exact pinned version.
# On Windows/macOS, binary installation from PPM/CRAN is preferred to avoid Rtools dependency.
install_cran <- function(pkg, max_attempts = 3) {
  # Fast path: already installed and loadable with dependencies
  if (is_pkg_ok(pkg)) {
    return(TRUE)
  }

  pinned_ver <- cran_versions[[pkg]]   # NULL if not pinned

  if (!is.null(pinned_ver)) {
    cat(sprintf("  [pin] %s == %s (pinned)\n", pkg, pinned_ver)); flush.console()
  }

  # Check if a native compiler is available for building from source on Windows
  has_compiler <- !(.Platform$OS.type == "windows") || nzchar(Sys.which("make")) || nzchar(Sys.which("gcc"))

  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      cat(sprintf("  Retry attempt %d/%d for CRAN package '%s'...\n", attempt, max_attempts, pkg))
      flush.console()
    }

    # Rotate mirrors on each attempt: attempt 1 = best_mirror first,
    # attempt 2 = second mirror first, etc. — ensures we don't hammer a
    # dead endpoint on every retry.
    n      <- length(cran_mirrors)
    offset <- (attempt - 1) %% n
    rotated_mirrors  <- cran_mirrors[c((offset + 1):n, if (offset > 0) 1:offset)]
    repos  <- setNames(rotated_mirrors, c("CRAN", paste0("CRAN_", seq_along(rotated_mirrors[-1]))))
    # Include configured active mirrors (like Bioconductor) so dependencies can be resolved
    repos  <- unique(c(repos, getOption("repos")))

    # On Windows without Rtools, try binary install first even if pinned to avoid compilation errors
    if (!is.null(pinned_ver) && has_compiler) {
      # ── Pinned install: fetch the exact archive tarball from CRAN ────────
      # We try each mirror's /src/contrib/Archive/<pkg>/<pkg>_<ver>.tar.gz
      installed_ok <- FALSE
      for (mirror in rotated_mirrors) {
        archive_url <- sprintf("%s/src/contrib/Archive/%s/%s_%s.tar.gz",
                               mirror, pkg, pkg, pinned_ver)
        # Also try the current (non-archived) path as a fallback
        current_url <- sprintf("%s/src/contrib/%s_%s.tar.gz",
                               mirror, pkg, pinned_ver)
        for (url in c(archive_url, current_url)) {
          tmp_tar <- tempfile(fileext = ".tar.gz")
          dl_ok <- tryCatch({
            download_with_retry(url, tmp_tar, mode = "wb")
            TRUE
          }, error = function(e) FALSE)
          if (dl_ok) {
            tryCatch(
              suppressWarnings(
                install.packages(tmp_tar, lib = local_lib, repos = NULL,
                                 type = "source", dependencies = NA, quiet = FALSE)
              ),
              error = function(e) {
                cat(sprintf("  [WARN] Pinned source install failed for '%s' %s: %s\n",
                            pkg, pinned_ver, conditionMessage(e)))
                flush.console()
              }
            )
            unlink(tmp_tar)
            if (is_pkg_ok(pkg)) { installed_ok <- TRUE; break }
          }
        }
        if (installed_ok) break
      }
      if (installed_ok) return(TRUE)
      cat(sprintf("  [WARN] Pinned source install failed for '%s' %s — falling back to binary repository.\n",
                  pkg, pinned_ver)); flush.console()
    }

    # ── Binary / Unpinned (or fallback) install ───────────────────────────
    pkg_type_opt <- getOption("pkgType", if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else "source")
    tryCatch(
      suppressWarnings(
        install.packages(pkg,
                         lib          = local_lib,
                         repos        = repos,
                         type         = pkg_type_opt,
                         dependencies = NA,   # Depends+Imports+LinkingTo only — skip Suggests
                         quiet        = FALSE)
      ),
      error   = function(e) {
        cat(sprintf("  [WARN] Attempt %d failed for CRAN package '%s': %s\n", attempt, pkg, conditionMessage(e)))
        flush.console()
      }
    )

    if (is_pkg_ok(pkg)) {
      return(TRUE)
    }

    if (attempt < max_attempts) {
      Sys.sleep(3)
    }
  }

  # Source fallback: handles pure-R packages with no binary for this R version
  # (e.g. withr 3.0.3 has no binary for R 4.4 — source compiles without Rtools)
  cat(sprintf("  [FALLBACK] Binary install failed for '%s' — trying source...\n", pkg))
  flush.console()
  tryCatch(
    suppressWarnings(
      install.packages(pkg,
                       lib          = local_lib,
                       repos        = getOption("repos"),
                       type         = "source",
                       dependencies = NA,
                       quiet        = FALSE)
    ),
    error   = function(e) {
      cat(sprintf("  [WARN] Source fallback failed for '%s': %s\n", pkg, conditionMessage(e)))
      flush.console()
    }
  )

  return(is_pkg_ok(pkg))
}


# ── Helper: install a single Bioconductor package with retry ──────────────────
# Pins the Bioconductor release via BiocManager::install(version = "3.20")
install_bioc <- function(pkg, max_attempts = 3) {
  # Fast path: already installed and loadable with dependencies
  if (is_pkg_ok(pkg)) {
    return(TRUE)
  }

  bioc_release <- "3.20"
  pinned_pkg_ver <- bioc_versions[[pkg]]   # Expected package version in Bioc 3.20

  if (!is.null(pinned_pkg_ver)) {
    cat(sprintf("  [pin] %s == %s (Bioconductor %s)\n", pkg, pinned_pkg_ver, bioc_release)); flush.console()
  }

  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      cat(sprintf("  Retry attempt %d/%d for Bioconductor package '%s'...\n", attempt, max_attempts, pkg))
      flush.console()
    }

    tryCatch(
      suppressWarnings(
        BiocManager::install(pkg, lib = local_lib, ask = FALSE, update = FALSE,
                             quiet = FALSE, version = bioc_release)
      ),
      error   = function(e) {
        cat(sprintf("  [WARN] Attempt %d failed for Bioconductor package '%s': %s\n", attempt, pkg, conditionMessage(e)))
        flush.console()
      }
    )

    if (is_pkg_ok(pkg)) {
      return(TRUE)
    }

    if (attempt < max_attempts) {
      Sys.sleep(3)
    }
  }
  return(FALSE)
}

# ── Helper: topological sort of packages based on dependencies ────────────────
topo_sort <- function(pkgs, db) {
  visited <- c()
  temp_visited <- c()
  result <- c()
  
  visit <- function(pkg) {
    if (pkg %in% temp_visited) {
      # Cycle detected, ignore or warn
      return()
    }
    if (pkg %in% visited) {
      return()
    }
    
    temp_visited <<- c(temp_visited, pkg)
    
    # Get dependencies from db
    if (pkg %in% rownames(db)) {
      val <- db[pkg, "Imports"]
      val_dep <- db[pkg, "Depends"]
      val_link <- db[pkg, "LinkingTo"]
      
      deps <- c()
      for (val_str in c(val, val_dep, val_link)) {
        if (!is.na(val_str) && nzchar(val_str)) {
          pkgs_parsed <- gsub("\\([^\\)]+\\)", "", val_str)
          pkgs_parsed <- unlist(strsplit(pkgs_parsed, ","))
          pkgs_parsed <- trimws(pkgs_parsed)
          pkgs_parsed <- pkgs_parsed[pkgs_parsed != "R" & nzchar(pkgs_parsed)]
          deps <- c(deps, pkgs_parsed)
        }
      }
      
      # Filter to those in our target list and not base R packages
      base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", "utils")
      deps <- deps[!(deps %in% base_pkgs)]
      
      for (dep in deps) {
        if (dep %in% pkgs) {
          visit(dep)
        }
      }
    }
    
    temp_visited <<- temp_visited[temp_visited != pkg]
    visited <<- c(visited, pkg)
    result <<- c(result, pkg)
  }
  
  for (pkg in pkgs) {
    visit(pkg)
  }
  
  result
}

# ── Helper: install packages and all their recursive dependencies with retry ──
install_with_retry_and_deps <- function(pkgs) {
  if (length(pkgs) == 0) return(TRUE)
  
  # Fast path: check if all requested packages are already fully installed and OK
  # (avoids calling available.packages entirely if we have nothing to install)
  is_ok <- sapply(pkgs, is_pkg_ok)
  if (all(is_ok)) {
    cat("All requested packages and their dependencies are already installed and OK. Skipping database query.\n")
    flush.console()
    return(TRUE)
  }
  
  cat("Querying available packages database...\n")
  flush.console()
  
  preferred_type <- getOption("pkgType", "source")
  if (preferred_type == "source") {
    db <- tryCatch(available.packages(type = "source"), error = function(e) NULL)
  } else {
    # Query binary packages first on Windows/macOS
    db_bin <- tryCatch(available.packages(type = "binary"), error = function(e) NULL)
    db <- db_bin
    
    needed_pkgs <- pkgs[!is_ok]
    needs_src <- is.null(db_bin) || !all(needed_pkgs %in% rownames(db_bin))
    
    if (needs_src) {
      cat("Querying available source packages database (fallback)...\n")
      flush.console()
      db_src <- tryCatch(available.packages(type = "source"), error = function(e) NULL)
      
      if (is.null(db)) {
        db <- db_src
      } else if (!is.null(db_src)) {
        missing_pkgs <- setdiff(rownames(db_src), rownames(db))
        if (length(missing_pkgs) > 0) {
          db <- rbind(db, db_src[missing_pkgs, ])
        }
      }
    }
  }
  
  if (is.null(db)) {
    cat("[ERROR] Could not query available packages database.\n")
    return(FALSE)
  }
  
  # Resolve all recursive dependencies (excluding base packages)
  visited <- c()
  to_visit <- pkgs
  
  while (length(to_visit) > 0) {
    curr <- to_visit[1]
    to_visit <- to_visit[-1]
    
    if (curr %in% visited) next
    
    visited <- c(visited, curr)
    
    # Check if package is in available packages database
    if (!(curr %in% rownames(db))) {
      # It might be a base package or already installed package not in repos
      next
    }
    
    # Get its dependencies
    val <- db[curr, "Imports"]
    val_dep <- db[curr, "Depends"]
    val_link <- db[curr, "LinkingTo"]
    
    deps <- c()
    for (val_str in c(val, val_dep, val_link)) {
      if (!is.na(val_str) && nzchar(val_str)) {
        pkgs_parsed <- gsub("\\([^\\)]+\\)", "", val_str)
        pkgs_parsed <- unlist(strsplit(pkgs_parsed, ","))
        pkgs_parsed <- trimws(pkgs_parsed)
        pkgs_parsed <- pkgs_parsed[pkgs_parsed != "R" & nzchar(pkgs_parsed)]
        deps <- c(deps, pkgs_parsed)
      }
    }
    
    # Exclude base R packages
    base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", "utils")
    deps <- deps[!(deps %in% base_pkgs)]
    
    # Add new dependencies to visit
    new_deps <- deps[!(deps %in% visited) & !(deps %in% to_visit)]
    to_visit <- c(to_visit, new_deps)
  }
  
  # Now we have all recursive packages needed in 'visited'.
  # Filter to those that are NOT already installed and ok (is_pkg_ok == FALSE)
  to_install <- visited[!sapply(visited, is_pkg_ok)]
  
  if (length(to_install) == 0) {
    cat("All packages and dependencies already installed.\n")
    return(TRUE)
  }
  
  cat("Packages to download and install (including missing dependencies):\n")
  cat(paste("  -", to_install, collapse = "\n"), "\n")
  flush.console()
  
  # Create a temporary directory for downloads under R-lib
  env_lib   <- Sys.getenv("R_LIBS_USER", unset = "")
  local_lib <- if (nzchar(env_lib)) env_lib else "R-lib"
  tmp_dir   <- file.path(local_lib, "pkg-downloads")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE, force = TRUE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Download all packages with retry
  downloaded_files <- c()
  for (pkg in to_install) {
    if (!(pkg %in% rownames(db))) {
      cat(sprintf("  [WARN] Package '%s' not found in repositories.\n", pkg))
      next
    }
    
    repo_url <- db[pkg, "Repository"]
    is_src <- grepl("src/contrib", repo_url, fixed = TRUE)
    
    pkg_ext <- if (is_src) {
      ".tar.gz"
    } else {
      if (getOption("pkgType") == "binary") {
        if (.Platform$OS.type == "windows") ".zip" else ".tgz"
      } else {
        ".tar.gz"
      }
    }
    
    filename <- paste0(pkg, "_", db[pkg, "Version"], pkg_ext)
    url <- paste0(repo_url, "/", filename)
    dest <- file.path(tmp_dir, filename)
    
    if (pkg == "Boruta" && !is.null(cran_versions[["Boruta"]]) && cran_versions[["Boruta"]] == "9.0.0") {
      if (.Platform$OS.type == "windows" && getOption("pkgType") == "binary") {
        url <- "https://cloud.r-project.org/bin/windows/contrib/4.4/Boruta_9.0.0.zip"
        filename <- "Boruta_9.0.0.zip"
        dest <- file.path(tmp_dir, filename)
        is_src <- FALSE
      } else if (Sys.info()[["sysname"]] == "Darwin" && getOption("pkgType") == "binary") {
        arch <- if (grepl("arm|aarch64", Sys.info()["machine"])) "big-sur-arm64" else "big-sur-x86_64"
        url <- sprintf("https://cloud.r-project.org/bin/macosx/%s/contrib/4.4/Boruta_9.0.0.tgz", arch)
        filename <- "Boruta_9.0.0.tgz"
        dest <- file.path(tmp_dir, filename)
        is_src <- FALSE
      }
    }
    
    cat(sprintf("Downloading package '%s' (type: %s) from %s...\n", pkg, if (is_src) "source" else "binary", url))
    flush.console()
    
    dl_ok <- tryCatch({
      download_with_retry(url, dest)
      TRUE
    }, error = function(e) {
      cat(sprintf("  [ERROR] Failed to download '%s': %s\n", pkg, conditionMessage(e)))
      FALSE
    })
    
    if (dl_ok) {
      downloaded_files <- c(downloaded_files, dest)
    } else {
      # Try fallback to source if binary download failed and we are not on Windows
      if (!is_src && getOption("pkgType") == "binary") {
        # Check if source package is available
        # But since wininet should succeed with retries, we stop on error
        stop(sprintf("Failed to download required package dependency: %s", pkg))
      }
    }
  }
  
  # Sort packages in topological order so dependencies are installed first
  to_install_sorted <- topo_sort(to_install, db)
  
  cat("Installing downloaded packages in topological order...\n")
  flush.console()
  
  for (pkg in to_install_sorted) {
    file_matches <- list.files(tmp_dir, pattern = paste0("^", pkg, "_"), full.names = TRUE)
    if (length(file_matches) == 0) next
    
    local_file <- file_matches[1]
    is_local_src <- grepl("\\.tar\\.gz$", basename(local_file))
    cat(sprintf("Installing local package: %s (type: %s)...\n", basename(local_file), if (is_local_src) "source" else "binary"))
    flush.console()
    
    tryCatch({
      install.packages(local_file, lib = local_lib, repos = NULL, type = if (is_local_src) "source" else getOption("pkgType"), quiet = FALSE)
    }, error = function(e) {
      cat(sprintf("  [WARN] First-pass installation failed for %s: %s\n", pkg, conditionMessage(e)))
    })
  }
  
  # Clean up temp files
  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  
  # Verify that all target packages are now ok
  all_ok <- all(sapply(pkgs, is_pkg_ok))
  return(all_ok)
}

# ── Optional: Synchronize targets from renv.lock if present ───────────────────
sync_from_renv_lock <- function() {
  lock_candidates <- unique(c(
    file.path(getwd(), "renv.lock"),
    file.path(script.dir, "renv.lock"),
    file.path(dirname(script.dir), "renv.lock")
  ))
  lockfile_renv <- lock_candidates[file.exists(lock_candidates)][1]

  if (is.na(lockfile_renv)) {
    cat("[renv] No renv.lock file found in project root; using built-in pinned package versions.\n")
    flush.console()
    return(FALSE)
  }

  cat(sprintf("\n[renv] Discovered lockfile: %s\n", lockfile_renv))

  # Ensure jsonlite is available to parse the JSON lockfile
  if (!is_pkg_ok("jsonlite")) {
    cat("[renv] Installing jsonlite to parse lockfile...\n")
    flush.console()
    install_cran("jsonlite")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("[WARN] Failed to load jsonlite; falling back to built-in pinned versions.\n")
    flush.console()
    return(FALSE)
  }

  tryCatch({
    lock_data <- jsonlite::fromJSON(lockfile_renv, simplifyVector = FALSE)
    if (is.null(lock_data$Packages) || length(lock_data$Packages) == 0) {
      cat("[renv] No package entries found in lockfile.\n")
      flush.console()
      return(FALSE)
    }

    cat(sprintf("[renv] Parsing %d locked package specifications from %s...\n",
                length(lock_data$Packages), basename(lockfile_renv)))
    flush.console()

    new_cran_pkgs <- character(0)
    new_bioc_pkgs <- character(0)

    for (p in names(lock_data$Packages)) {
      rec <- lock_data$Packages[[p]]
      v <- rec$Version
      if (is.null(v) || !nzchar(v)) next
      v_norm <- normalize_ver(v)

      src  <- if (!is.null(rec$Source)) as.character(rec$Source) else ""
      repo <- if (!is.null(rec$Repository)) as.character(rec$Repository) else ""

      is_bioc <- identical(src, "Bioconductor") ||
                 grepl("bioc", repo, ignore.case = TRUE) ||
                 (p %in% names(bioc_versions))

      if (is_bioc) {
        bioc_versions[[p]] <<- v_norm
        new_bioc_pkgs <- c(new_bioc_pkgs, p)
      } else {
        cran_versions[[p]] <<- v_norm
        new_cran_pkgs <- c(new_cran_pkgs, p)
      }
    }

    cran_pkgs <<- unique(c(cran_pkgs, new_cran_pkgs))
    bioc_pkgs <<- unique(c(bioc_pkgs, new_bioc_pkgs))

    arch_label <- if (is_arm64) "arm64 (Apple Silicon)" else if (is_win) "Windows x64" else if (is_mac) "Intel x86_64" else "Linux"
    cat(sprintf("[renv] Successfully synchronized locked versions (CRAN: %d, Bioconductor: %d).\n",
                length(cran_pkgs), length(bioc_pkgs)))
    cat(sprintf("  [arch] Target architecture: %s — binary packages preferred with source archive fallback.\n",
                arch_label))
    flush.console()
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("[WARN] Could not parse %s: %s — continuing with built-in versions.\n",
                lockfile_renv, conditionMessage(e)))
    flush.console()
    return(FALSE)
  })
}

# Run synchronization if renv.lock is present
sync_from_renv_lock()

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1 — Bootstrap: pre-install pure-R packages with no R 4.4 binary
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== STEP 1: Bootstrapping pure-R dependencies ===\n"); flush.console()
bootstrap_pkgs <- c("withr", "png", "Matrix", "RcppTOML")  # prerequisites for reticulate
for (pkg in bootstrap_pkgs) {
  if (!is_pkg_ok(pkg)) {
    cat(sprintf("  [bootstrap] Installing '%s' from source (no binary for R %s)...\n",
                pkg, paste(R.version$major, R.version$minor, sep = ".")))
    flush.console()
    tryCatch(
      install.packages(pkg,
                       repos        = c(CRAN = "https://cloud.r-project.org"),
                       dependencies = NA,
                       quiet        = FALSE),
      error   = function(e) {
        cat(sprintf("  [bootstrap] [WARN] Failed for '%s': %s\n", pkg, conditionMessage(e)))
        flush.console()
      }
    )
    if (is_pkg_ok(pkg)) {
      cat(sprintf("  [bootstrap] '%s' OK\n", pkg)); flush.console()
    } else {
      cat(sprintf("  [bootstrap] [WARN] '%s' still not loadable after source install\n", pkg))
      flush.console()
    }
  } else {
    cat(sprintf("  [bootstrap] '%s' already available.\n", pkg)); flush.console()
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2 — Install reticulate (prerequisite for Python integration)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== STEP 2: Installing reticulate ===\n"); flush.console()
if (!is_pkg_ok("reticulate")) {
  cat("Installing CRAN package: reticulate...\n"); flush.console()
  install_cran("reticulate")
} else {
  cat("reticulate already installed.\n"); flush.console()
}

if (!is_pkg_ok("reticulate")) {
  stop("Failed to install required package: reticulate.")
}


# ═══════════════════════════════════════════════════════════════════════════════
# STEP 3 — Install BiocManager
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== STEP 3: Installing BiocManager ===\n"); flush.console()
if (!is_pkg_ok("BiocManager")) {
  cat("Installing CRAN package: BiocManager...\n"); flush.console()
  install_cran("BiocManager")
} else {
  cat("BiocManager already installed.\n"); flush.console()
}

if (!is_pkg_ok("BiocManager")) {
  stop("Failed to install required package: BiocManager.")
}

# ── Configure BiocManager to use Posit Package Manager ───────────────────────
# ── Configure Bioconductor repositories ───────────────────────────────────────
# We use official bioconductor.org for software & annotation binaries (R 4.4 Windows .zip / macOS .tgz)
# and Posit Package Manager for CRAN packages.
bioc_version <- tryCatch(as.character(BiocManager::version()), error = function(e) "3.20")
cat(sprintf("Configuring Bioconductor repositories (Bioconductor %s)...\n", bioc_version))
flush.console()

cran_pref <- getOption("repos")["CRAN"]
if (length(cran_pref) == 0 || !nzchar(cran_pref)) {
  cran_pref <- "https://packagemanager.posit.co/cran/latest"
}

bioc_repos <- c(
  CRAN       = as.character(cran_pref),
  CRAN_cloud = "https://cloud.r-project.org",
  BioCsoft   = sprintf("https://bioconductor.org/packages/%s/bioc", bioc_version),
  BioCann    = sprintf("https://bioconductor.org/packages/%s/data/annotation", bioc_version),
  BioCexp    = sprintf("https://bioconductor.org/packages/%s/data/experiment", bioc_version)
)
options(repos = bioc_repos)
cat("  Bioconductor official binary repositories configured.\n"); flush.console()


# ═══════════════════════════════════════════════════════════════════════════════
# STEP 4 — Install Packages and Dependencies (CRAN & Bioconductor)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n=== STEP 4: Installing packages and all dependencies ===\n"); flush.console()
tryCatch({
  install_with_retry_and_deps(c(cran_pkgs, bioc_pkgs))
}, error = function(e) {
  cat(sprintf("[ERROR] Installation wrapper failed: %s\n", conditionMessage(e)))
})


# ── Final Verification Step ───────────────────────────────────────────────────
cat("\n===============================================================================\n")
cat("=== Final Verification: Checking Installed Packages & Desired Versions ===\n")
cat("===============================================================================\n\n")
flush.console()

get_pkg_version <- function(pkg) {
  tryCatch({
    as.character(utils::packageVersion(pkg, lib.loc = .libPaths()))
  }, error = function(e) NA_character_)
}

# Determine all packages that should be verified
all_pkgs_to_verify <- unique(c(
  "reticulate",
  "BiocManager",
  cran_pkgs,
  bioc_pkgs
))

# Collect verification records
verification_results <- data.frame(
  Package   = character(0),
  Type      = character(0),
  Desired   = character(0),
  Installed = character(0),
  Status    = character(0),
  stringsAsFactors = FALSE
)

failed_pkgs <- c()
version_diffs <- c()

for (pkg in all_pkgs_to_verify) {
  is_bioc <- pkg %in% bioc_pkgs
  pkg_type <- if (is_bioc) "Bioc (3.20)" else if (pkg %in% c("reticulate", "BiocManager")) "CRAN (Core)" else "CRAN"
  
  desired <- if (is_bioc && !is.null(bioc_versions[[pkg]])) {
    bioc_versions[[pkg]]
  } else if (!is.null(cran_versions[[pkg]])) {
    cran_versions[[pkg]]
  } else {
    "(any)"
  }
  
  loadable <- is_pkg_loadable(pkg)
  inst_ver <- get_pkg_version(pkg)
  
  normalize_ver <- function(v) {
    if (is.null(v) || is.na(v)) return("")
    gsub("-", ".", v, fixed = TRUE)
  }
  
  status <- "UNKNOWN"
  if (!loadable || is.na(inst_ver)) {
    status <- "FAIL (Unloadable/Missing)"
    failed_pkgs <- c(failed_pkgs, pkg)
  } else if (desired != "(any)") {
    if (normalize_ver(inst_ver) == normalize_ver(desired)) {
      status <- "OK (Exact match)"
    } else {
      status <- sprintf("OK (Diff: %s != %s)", inst_ver, desired)
      version_diffs <- c(version_diffs, sprintf("%s (installed: %s, desired: %s)", pkg, inst_ver, desired))
    }
  } else {
    status <- sprintf("OK (%s)", inst_ver)
  }
  
  verification_results <- rbind(verification_results, data.frame(
    Package   = pkg,
    Type      = pkg_type,
    Desired   = desired,
    Installed = if (is.na(inst_ver)) "MISSING" else inst_ver,
    Status    = status,
    stringsAsFactors = FALSE
  ))
}

# Print a structured, cleanly aligned summary table
col_w_pkg  <- max(c(nchar("Package"), nchar(verification_results$Package)), na.rm = TRUE) + 2
col_w_type <- max(c(nchar("Type"), nchar(verification_results$Type)), na.rm = TRUE) + 2
col_w_des  <- max(c(nchar("Desired"), nchar(verification_results$Desired)), na.rm = TRUE) + 2
col_w_inst <- max(c(nchar("Installed"), nchar(verification_results$Installed)), na.rm = TRUE) + 2

header_fmt <- sprintf("%%-%ds %%-%ds %%-%ds %%-%ds %%s\n", col_w_pkg, col_w_type, col_w_des, col_w_inst)
divider_len <- max(80, col_w_pkg + col_w_type + col_w_des + col_w_inst + 30)
divider <- paste(rep("-", divider_len), collapse = "")

cat(divider, "\n")
cat(sprintf(header_fmt, "Package", "Type", "Desired", "Installed", "Status"))
cat(divider, "\n")

for (i in seq_len(nrow(verification_results))) {
  row <- verification_results[i, ]
  cat(sprintf(header_fmt, row$Package, row$Type, row$Desired, row$Installed, row$Status))
}
cat(divider, "\n\n")
flush.console()

# Report any version differences as informative notes
if (length(version_diffs) > 0) {
  cat("[NOTE] The following packages are installed and functional, but have version differences from the target lock list:\n")
  for (vd in version_diffs) {
    cat(sprintf("  - %s\n", vd))
  }
  cat("\n")
  flush.console()
}

# Fatal stop if any required package could not be loaded or is missing
if (length(failed_pkgs) > 0) {
  err_msg <- sprintf(
    "Installation FAILED: The following %d required package(s) could not be loaded or are missing:\n%s\n",
    length(failed_pkgs),
    paste0("  - ", failed_pkgs, collapse = "\n")
  )
  cat(err_msg)
  flush.console()
  stop("R dependencies installation was incomplete. See details above.")
}

cat("=== Installation complete. All R packages verified and loaded successfully! ===\n")
flush.console()
