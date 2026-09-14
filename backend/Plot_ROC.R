# =============================================================================
# ROC CURVE TOOLKIT
# =============================================================================
# COVERED INPUTS (auto-detected by `as_roc_set()`):
#   - result object of rf_/svm_/logit_/plsda_perform_std_Revised()   [nested CV]
#   - list of pROC::roc objects                                      [any CV]
#   - a single pROC::roc object                                      [single split]
#   - per-fold predicted-probability data.frames + per-fold truth    [manual CV]
#   - one probability vector + truth vector                          [hold-out /
#                                                                     external val]
#   - a caret::train object with savePredictions = "final"           [flat CV]
#   - a long data.frame (truth / prob / fold / rep columns)          [anything]
#
# COVERED AGGREGATIONS (`average =`):
#   "fpr"       vertical averaging: TPR interpolated on a fixed FPR grid.
#               Statistically standard (Fawcett 2006); use for new figures.
#   "threshold" threshold averaging: sens/spec interpolated on a probability grid.
#   "pooled"    concatenate all out-of-fold predictions into ONE roc
#               ("micro" / pooled ROC) -> single curve + DeLong CI.
#   "none"      no averaging; draw every fold separately.
#
# COVERED BANDS (`band =`):
#   "sd" (lab default, mean +/- 1 SD), "se" (SD/sqrt(k)), "ci" (bootstrap
#   percentile band, or pROC::ci.se when there is only one curve), "none".
#
# MAIN ENTRY POINTS
#   plot_roc(x, ...)                 one ROC figure, lab style
#   plot_roc_multi(list_of_x, ...)   several curves overlaid (models / omics /
#                                    cohorts), one colour each
#   plot_roc_folds(x, ...)           mean curve + thin per-fold "spaghetti"
#   roc_stats(x, ...)                AUC + Se/Sp/BalAcc table, no plotting
#   roc_compare(x, y, ...)           DeLong test between two ROCs
#   roc_curve_data(x, ...)           tidy data.frame of the plotted curve (CSV)
#   save_roc(p, file)                300-dpi square PNG/PDF at lab figure size
#
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("pROC", quietly = TRUE)) library(pROC)
  if (requireNamespace("ggplot2", quietly = TRUE)) library(ggplot2)
  if (requireNamespace("ggthemes", quietly = TRUE)) library(ggthemes)
  if (requireNamespace("grid", quietly = TRUE)) library(grid)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 1. THEME AND STYLE
# =============================================================================

# Publication theme, verbatim from ML_Function_Refactored.R -------------------
theme_Publication <- function(base_size = 14, base_family = "sans") {
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.2, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      legend.title = ggplot2::element_text(face = "italic"),
      plot.margin = grid::unit(c(3, 3, 3, 3), "mm"),
      strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    ))
}

# Default colours for overlaying several curves -------------------------------
roc_palette_lab <- c("#366993", "#D1495B", "#EDAE49", "#00798C",
                     "#8D6A9F", "#66A182", "#B75D3F", "#3C3C3C")

# All cosmetic knobs in one place; override any field ------------------------
#   style = roc_style(line_color = "#D1495B", band_alpha = 0.15)
roc_style <- function(line_color   = "#366993",
                      band_color   = "steelblue",
                      band_alpha   = 0.2,
                      line_size    = 1.18,
                      diag_color   = "grey",
                      fold_color   = "grey60",
                      fold_alpha   = 0.45,
                      fold_size    = 0.45,
                      axis_title_size = 19,
                      axis_text_size  = 17,
                      title_size      = 23,
                      base_size       = 14,
                      palette      = roc_palette_lab) {
  as.list(environment())
}

# =============================================================================
# 2. INGEST LAYER -- turn anything into a "roc_set"
# =============================================================================
# A roc_set is: list(rocs = <list of pROC::roc>, classname = c(pos, neg),
#                    label = <chr>, source = <chr describing what was detected>)
# -----------------------------------------------------------------------------

# Build one roc with the lab's pinned orientation:
#   positive class = classname[1] = the "case"; predictor = P(positive class).
.roc_pinned <- function(truth, prob, classname) {
  truth <- factor(as.character(truth), levels = classname)
  keep  <- !is.na(truth) & !is.na(prob)
  truth <- truth[keep]; prob <- prob[keep]
  if (length(unique(truth)) < 2L) return(NULL)   # degenerate fold -> skipped
  pROC::roc(response = truth, predictor = as.numeric(prob),
            levels = rev(classname), direction = "<", quiet = TRUE)
}

# Pull the positive-class probability column out of a predict(type="prob") frame
.prob_column <- function(prob, classname) {
  if (is.null(dim(prob))) return(as.numeric(prob))
  if (classname[1] %in% colnames(prob)) return(as.numeric(prob[[classname[1]]]))
  # fall back to the 2nd column, the convention used in the Stabl_cox variant
  as.numeric(prob[[min(2L, ncol(prob))]])
}

# Recover (truth, prob) from an existing pROC::roc, for pooling / re-plotting
.roc_to_preds <- function(r) {
  pos <- r$levels[2]                      # pROC stores levels = c(control, case)
  data.frame(truth = factor(as.character(r$response),
                            levels = c(pos, setdiff(r$levels, pos))),
             prob  = as.numeric(r$predictor),
             stringsAsFactors = FALSE)
}

.classname_from_roc <- function(r) c(r$levels[2], r$levels[1])

# Trapezoidal AUC, for curves that arrive as bare FPR/TPR points
.auc_trapezoid <- function(fpr, tpr) {
  o <- order(fpr, tpr); f <- fpr[o]; t <- tpr[o]
  sum(diff(f) * (utils::head(t, -1) + utils::tail(t, -1)) / 2)
}

# A roc_set that carries geometry only (no raw predictions behind it).
# Used for results that were already reduced to a curve: the web app's
# roc_data, and pipeline results where only avg_AUC/polygon_SD_AUC survived.
.roc_set_precomputed <- function(curve, band = NULL, auc_tot = NULL,
                                 auc_mean = NULL, auc_sd = NULL,
                                 classname = NULL, label = NULL,
                                 source = "pre-computed curve", n_curves = 1L) {
  structure(list(rocs = NULL, classname = classname, label = label,
                 source = source, n_curves = n_curves,
                 precomputed = list(curve = curve, band = band,
                                    auc_tot = auc_tot,
                                    auc_mean = auc_mean %||% mean(auc_tot),
                                    auc_sd = auc_sd %||%
                                      (if (length(auc_tot) > 1) stats::sd(auc_tot)
                                       else NA_real_))),
            class = "roc_set")
}

# Does x look like the web app's list(fpr = , tpr = , auc = )?
.is_fpr_tpr_list <- function(x) {
  is.list(x) && !is.data.frame(x) &&
    all(c("fpr", "tpr") %in% tolower(names(x))) &&
    is.numeric(x[[which(tolower(names(x)) == "fpr")[1]]])
}
.get_ci <- function(x, nm) x[[which(tolower(names(x)) == nm)[1]]]

#' Detect the input format and return a roc_set
#'
#' @param x         see the COVERED INPUTS list at the top of this file
#' @param truth     truth vector, or list of per-fold truth vectors (when x is
#'                  probabilities)
#' @param classname c(positive, negative). Inferred when possible; supply it
#'                  whenever you want to be sure which class is "positive".
#' @param fold_col,truth_col,prob_col,rep_col  column names when x is a data.frame
as_roc_set <- function(x, truth = NULL, classname = NULL, label = NULL,
                       fold_col = "fold", truth_col = "truth",
                       prob_col = "prob", rep_col = NULL) {

  mk <- function(rocs, cn, src) {
    rocs <- Filter(Negate(is.null), rocs)
    if (!length(rocs)) stop("No usable ROC could be built (all folds degenerate?).")
    structure(list(rocs = rocs, classname = cn,
                   label = label, source = src, n_curves = length(rocs)),
              class = "roc_set")
  }

  # -- already a roc_set ------------------------------------------------------
  if (inherits(x, "roc_set")) {
    if (!is.null(label)) x$label <- label
    return(x)
  }

  # -- single pROC::roc -------------------------------------------------------
  if (inherits(x, "roc")) {
    return(mk(list(x), classname %||% .classname_from_roc(x), "single pROC::roc"))
  }

  # -- web app: evaluate_predictions() / roc_data[[m]] = list(fpr, tpr, auc) --
  # (data_processing_web_app/backend/feature_selection.R). Geometry only: no
  # raw predictions, so no SD band, no DeLong CI -- the AUC is taken as given.
  if (.is_fpr_tpr_list(x)) {
    f <- as.numeric(.get_ci(x, "fpr")); t <- as.numeric(.get_ci(x, "tpr"))
    o <- order(f, t)
    a <- if ("auc" %in% tolower(names(x))) as.numeric(.get_ci(x, "auc"))
         else .auc_trapezoid(f, t)
    return(.roc_set_precomputed(
      curve = data.frame(fpr = f[o], tpr = t[o]), auc_tot = a,
      classname = classname, label = label,
      source = "web-app curve: list(fpr, tpr, auc)"))
  }

  # -- named list of web-app curves (the whole roc_data object) ---------------
  if (is.list(x) && !is.data.frame(x) && length(x) &&
      all(vapply(x, .is_fpr_tpr_list, logical(1)))) {
    stop("This looks like the web app's `roc_data` (one curve per model).\n",
         "  Overlay them:  plot_roc_multi(roc_data)\n",
         "  Or one model:  plot_roc(roc_data[[\"randomforest\"]])")
  }

  # -- caret::train with saved resample predictions ---------------------------
  if (inherits(x, "train")) {
    pred <- x$pred
    if (is.null(pred))
      stop("caret::train object has no $pred; re-train with ",
           "trainControl(savePredictions = \"final\", classProbs = TRUE).")
    # keep only the winning hyper-parameter rows
    if (!is.null(x$bestTune) && all(names(x$bestTune) %in% names(pred))) {
      keep <- rep(TRUE, nrow(pred))
      for (nm in names(x$bestTune)) keep <- keep & pred[[nm]] == x$bestTune[[nm]]
      pred <- pred[keep, , drop = FALSE]
    }
    cn <- classname %||% levels(pred$obs)
    sp <- split(pred, pred$Resample)
    rocs <- lapply(sp, function(d) .roc_pinned(d$obs, d[[cn[1]]], cn))
    return(mk(rocs, cn, sprintf("caret::train, %d resamples", length(sp))))
  }

  # -- long data.frame --------------------------------------------------------
  if (is.data.frame(x)) {

    # web app's roc_df: columns FPR / TPR (+ optional Model). Geometry only.
    nml <- tolower(names(x))
    if (all(c("fpr", "tpr") %in% nml) && !all(c(truth_col, prob_col) %in% names(x))) {
      mcol <- names(x)[nml %in% c("model", "curve", "group")][1]
      if (!is.na(mcol) && length(unique(x[[mcol]])) > 1)
        stop("This data.frame holds several curves (column '", mcol, "').\n",
             "  Overlay them:  plot_roc_multi(split(df, df$", mcol, "))")
      f <- as.numeric(x[[names(x)[nml == "fpr"][1]]])
      t <- as.numeric(x[[names(x)[nml == "tpr"][1]]])
      o <- order(f, t)
      return(.roc_set_precomputed(
        curve = data.frame(fpr = f[o], tpr = t[o]),
        auc_tot = .auc_trapezoid(f, t), classname = classname,
        label = label %||% (if (!is.na(mcol)) as.character(x[[mcol]][1])),
        source = "web-app curve: data.frame(FPR, TPR)"))
    }

    if (!all(c(truth_col, prob_col) %in% names(x)))
      stop(sprintf("data.frame needs columns '%s' and '%s'.", truth_col, prob_col))
    cn <- classname %||% {
      tv <- x[[truth_col]]
      if (is.factor(tv)) levels(tv) else sort(unique(as.character(tv)))
    }
    if (length(cn) != 2L)
      stop("Binary ROC needs exactly 2 classes; got: ", paste(cn, collapse = ", "),
           ". For >2 classes use roc_ovr().")
    grp <- NULL
    if (!is.null(rep_col) && rep_col %in% names(x) && fold_col %in% names(x)) {
      grp <- interaction(x[[rep_col]], x[[fold_col]], drop = TRUE)
    } else if (fold_col %in% names(x)) {
      grp <- x[[fold_col]]
    }
    if (is.null(grp)) {
      rocs <- list(.roc_pinned(x[[truth_col]], x[[prob_col]], cn))
      return(mk(rocs, cn, "data.frame, no fold column -> 1 curve"))
    }
    sp <- split(x, grp, drop = TRUE)
    rocs <- lapply(sp, function(d) .roc_pinned(d[[truth_col]], d[[prob_col]], cn))
    return(mk(rocs, cn, sprintf("data.frame, %d folds", length(sp))))
  }

  # -- pipeline result of *_perform_std_Revised() -----------------------------
  if (is.list(x) && !is.null(x$AUCROC_fit) && !is.null(x$test)) {
    cn <- classname %||% levels(x$test[[1]]$Label)
    rocs <- lapply(seq_along(x$AUCROC_fit), function(i) {
      pr <- stats::predict(x$AUCROC_fit[[i]], newdata = x$test[[i]], type = "prob")
      .roc_pinned(x$test[[i]]$Label, .prob_column(pr, cn), cn)
    })
    return(mk(rocs, cn, sprintf("pipeline result, %d outer folds",
                                length(x$AUCROC_fit))))
  }

  # -- pipeline result already summarised (models dropped, avg_AUC kept) ------
  if (is.list(x) && !is.null(x$avg_AUC) && !is.null(x$polygon_SD_AUC)) {
    return(.roc_set_precomputed(
      curve = data.frame(fpr = 1 - x$avg_AUC$roc_mean_spe,
                         tpr = x$avg_AUC$roc_mean_sen),
      band  = data.frame(x = 1 - x$polygon_SD_AUC$x, y = x$polygon_SD_AUC$y),
      auc_tot = x$auc_tot, auc_mean = x$auc_mean, auc_sd = x$auc_sd,
      classname = classname, label = label %||% x$Comparision,
      source = "pre-summarised pipeline result (avg_AUC + polygon_SD_AUC)",
      n_curves = length(x$auc_tot %||% 1)))
  }

  # -- list of pROC::roc ------------------------------------------------------
  if (is.list(x) && length(x) && all(vapply(x, inherits, logical(1), "roc"))) {
    cn <- classname %||% .classname_from_roc(x[[1]])
    return(mk(x, cn, sprintf("list of %d pROC::roc", length(x))))
  }

  # -- list of per-fold probability frames + list of per-fold truth -----------
  if (is.list(x) && !is.null(truth) && is.list(truth)) {
    if (length(x) != length(truth))
      stop("prob list and truth list must have the same length.")
    cn <- classname %||% {
      tv <- truth[[1]]; if (is.factor(tv)) levels(tv) else sort(unique(as.character(tv)))
    }
    rocs <- lapply(seq_along(x), function(i)
      .roc_pinned(truth[[i]], .prob_column(x[[i]], cn), cn))
    return(mk(rocs, cn, sprintf("per-fold probabilities, %d folds", length(x))))
  }

  # -- one probability vector / one-row-per-sample frame + truth vector -------
  if (!is.null(truth) && (is.numeric(x) || is.data.frame(x) || is.matrix(x))) {
    cn <- classname %||% {
      if (is.factor(truth)) levels(truth) else sort(unique(as.character(truth)))
    }
    if (length(cn) != 2L)
      stop("Binary ROC needs exactly 2 classes; for >2 use roc_ovr().")
    return(mk(list(.roc_pinned(truth, .prob_column(x, cn), cn)), cn,
              "single prediction vector (hold-out / external validation)"))
  }

  stop("Unrecognised input. Pass a pipeline result, a pROC::roc (or list of), ",
       "predicted probabilities + `truth`, a caret::train, or a long data.frame.")
}

print.roc_set <- function(x, ...) {
  cat("<roc_set>\n")
  cat("  detected  :", x$source, "\n")
  cat("  curves    :", x$n_curves, "\n")
  if (!is.null(x$classname))
    cat("  classes   :", x$classname[1], "(positive) vs", x$classname[2], "\n")
  if (!is.null(x$rocs)) {
    a <- vapply(x$rocs, function(r) as.numeric(r$auc), numeric(1))
    cat("  AUC       :", sprintf("%.3f +/- %.3f  [%s]", mean(a),
                                 if (length(a) > 1) stats::sd(a) else NA_real_,
                                 paste(sprintf("%.3f", a), collapse = ", ")), "\n")
    cat("  raw preds : yes  (all `average=` and `band=` options available)\n")
  } else {
    pc <- x$precomputed
    cat("  AUC       :", sprintf("%.3f", pc$auc_mean), "(as supplied)\n")
    cat("  raw preds : no   (geometry only: `average=`/`band=` are ignored)\n")
  }
  invisible(x)
}

# =============================================================================
# 2b. roc_inspect() -- "I do not know what my colleague handed me"
# =============================================================================
#' Say what an object is and print the exact call to plot it.
#' Never throws: on an unknown object it lists what it did find instead.
roc_inspect <- function(x, truth = NULL, classname = NULL, ...) {
  cat("---------------------------------------------------------------\n")
  cat("roc_inspect()\n")
  cat("---------------------------------------------------------------\n")
  cat("R class     :", paste(class(x), collapse = ", "), "\n")
  if (is.list(x) && !is.null(names(x)))
    cat("names       :", paste(utils::head(names(x), 20), collapse = ", "),
        if (length(names(x)) > 20) sprintf(" ... (%d total)", length(names(x))) else "", "\n")

  rs <- tryCatch(as_roc_set(x, truth = truth, classname = classname, ...),
                 error = function(e) e)

  if (inherits(rs, "error")) {
    cat("\nNOT RECOGNISED. Message:\n  ", conditionMessage(rs), "\n\n")
    cat("What the toolkit accepts, and how to get there:\n")
    cat("  * pipeline result  -> plot_roc(res)\n")
    cat("  * caret::train     -> plot_roc(fit)          [needs savePredictions='final']\n")
    cat("  * pROC::roc (list) -> plot_roc(rocs)\n")
    cat("  * probs + truth    -> plot_roc(prob, truth = y, classname = c('Pos','Neg'))\n")
    cat("  * long data.frame  -> plot_roc(df, truth_col=, prob_col=, fold_col=)\n")
    cat("  * web-app roc_data -> plot_roc_multi(roc_data)\n")
    if (is.list(x)) {
      cat("\nTop-level element types, to help you find the predictions:\n")
      for (nm in utils::head(names(x), 25))
        cat(sprintf("    $%-24s %s\n", nm, paste(class(x[[nm]]), collapse = "/")))
    }
    return(invisible(NULL))
  }

  print(rs)
  cat("\nSuggested calls:\n")
  if (is.null(rs$rocs)) {
    cat("  plot_roc(x)                       # curve as supplied\n")
    cat("  # geometry only -- to get an SD band or a CI you need the raw\n")
    cat("  # per-fold probabilities, not just the finished fpr/tpr points.\n")
  } else if (rs$n_curves > 1) {
    cat("  plot_roc(x)                       # lab standard, mean +/- 1 SD\n")
    cat("  plot_roc(x, average = 'fpr')      # statistically standard averaging\n")
    cat("  plot_roc(x, show_folds = TRUE)    # + per-fold curves\n")
    cat("  plot_roc(x, average = 'pooled', auc_label = 'pooled_ci')  # DeLong CI\n")
    cat("  roc_stats(x, threshold = 'youden')\n")
  } else {
    cat("  plot_roc(x, band = 'ci')          # single curve + bootstrap CI band\n")
    cat("  roc_stats(x, threshold = 'youden')\n")
  }
  invisible(rs)
}

# =============================================================================
# 2c. WEB-APP BRIDGE (data_processing_web_app/backend/feature_selection.R)
# =============================================================================
#' Turn one dataset's result block from run_ml_training() / run_testing() into
#' something plottable. Accepts the whole block (it looks for $roc_data) or the
#' roc_data list itself.
#'
#'   res <- run_ml_training(...)          # or the JSON parsed back into R
#'   plot_roc_app(res[[1]])               # all models overlaid, lab style
#'   plot_roc_app(res[[1]], model = "randomforest")   # just one
plot_roc_app <- function(x, model = NULL, title = NULL, ...) {
  rd <- if (!is.null(x$roc_data)) x$roc_data else x
  if (!is.list(rd) || !length(rd))
    stop("No `roc_data` found. Pass the per-dataset result block, or roc_data itself.")
  if (!is.null(model)) {
    if (!model %in% names(rd))
      stop("Model '", model, "' not in roc_data. Available: ",
           paste(names(rd), collapse = ", "))
    return(plot_roc(rd[[model]], title = title %||% toupper(model), ...))
  }
  if (length(rd) == 1L)
    return(plot_roc(rd[[1]], title = title %||% toupper(names(rd)[1]), ...))
  names(rd) <- toupper(names(rd))
  plot_roc_multi(rd, title = title, ...)
}

# =============================================================================
# 3. AGGREGATION -- turn a roc_set into a mean curve + band
# =============================================================================
# Every method returns:
#   list(curve = data.frame(fpr, tpr),
#        mat   = matrix of per-curve tpr on the shared grid (NULL for pooled),
#        grid  = shared x grid,
#        pooled_roc = <roc> or NULL)
# -----------------------------------------------------------------------------

# -- "lab": pipeline maths, index grid ----------------------------------------
# Each fold's sens/spec vector is re-sampled on its own threshold INDEX rescaled
# to [0,1] (not on FPR). Kept because it reproduces already-published numbers.
.agg_lab <- function(rocs, n_grid = 100) {
  common_x <- seq(0, 1, length.out = n_grid)
  sen <- spe <- matrix(NA_real_, nrow = n_grid, ncol = length(rocs))
  for (i in seq_along(rocs)) {
    s <- rocs[[i]]$sensitivities; p <- rocs[[i]]$specificities
    ox <- seq(0, 1, length.out = length(s))
    sen[, i] <- stats::approx(ox, s, xout = common_x, rule = 2)$y
    spe[, i] <- stats::approx(ox, p, xout = common_x, rule = 2)$y
  }
  list(sen_mat = sen, spe_mat = spe, grid = common_x, mode = "lab")
}

# -- "fpr": vertical averaging (TPR at fixed FPR) -----------------------------
.agg_fpr <- function(rocs, n_grid = 100) {
  grid <- seq(0, 1, length.out = n_grid)
  tpr <- matrix(NA_real_, nrow = n_grid, ncol = length(rocs))
  for (i in seq_along(rocs)) {
    f <- 1 - rocs[[i]]$specificities
    t <- rocs[[i]]$sensitivities
    o <- order(f, t)
    tpr[, i] <- stats::approx(f[o], t[o], xout = grid, rule = 2, ties = max)$y
  }
  list(sen_mat = tpr, spe_mat = matrix(rep(1 - grid, length(rocs)), ncol = length(rocs)),
       grid = grid, mode = "fpr")
}

# -- "threshold": sens/spec at a fixed probability grid -----------------------
.agg_threshold <- function(rocs, n_grid = 100) {
  grid <- seq(0, 1, length.out = n_grid)
  sen <- spe <- matrix(NA_real_, nrow = n_grid, ncol = length(rocs))
  for (i in seq_along(rocs)) {
    th <- rocs[[i]]$thresholds
    th[is.infinite(th) & th < 0] <- 0
    th[is.infinite(th) & th > 0] <- 1
    o <- order(th)
    sen[, i] <- stats::approx(th[o], rocs[[i]]$sensitivities[o], xout = grid,
                              rule = 2, ties = mean)$y
    spe[, i] <- stats::approx(th[o], rocs[[i]]$specificities[o], xout = grid,
                              rule = 2, ties = mean)$y
  }
  list(sen_mat = sen, spe_mat = spe, grid = grid, mode = "threshold")
}

# -- "pooled": one roc from all out-of-fold predictions -----------------------
.agg_pooled <- function(rocs, classname, n_grid = 100) {
  preds <- do.call(rbind, lapply(rocs, .roc_to_preds))
  cn <- classname %||% levels(preds$truth)
  r <- .roc_pinned(preds$truth, preds$prob, cn)
  list(pooled_roc = r, mode = "pooled",
       sen_mat = matrix(r$sensitivities, ncol = 1),
       spe_mat = matrix(r$specificities, ncol = 1),
       grid = NULL)
}

# # =============================================================================
# 3b. FAST BOOTSTRAP FOR AUC CI & ROC CONFIDENCE BAND
# =============================================================================
#' Compute both AUC 95% CI and pointwise ROC confidence band via pROC bootstrap
#'
#' @param roc_obj    pROC::roc object
#' @param n_boot     number of stratified bootstrap replicates (default 200)
#' @param conf.level confidence level (default 0.95)
#' @param n_grid     number of specificity grid points (default 100)
#' @param seed       random seed (default 42)
#' @return list(ci_auc = c(lower, upper), band_df = data.frame(x, y))
compute_bootstrap_roc <- function(roc_obj, n_boot = 200, conf.level = 0.95, n_grid = 100, seed = 42) {
  if (!inherits(roc_obj, "roc")) return(NULL)
  
  # Return cached bootstrap if already present on the object
  if (!is.null(roc_obj$ci_auc) && !is.null(roc_obj$band_df)) {
    return(list(ci_auc = roc_obj$ci_auc, band_df = roc_obj$band_df))
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  sp <- seq(0, 1, length.out = n_grid)
  
  # 1. Pointwise sensitivity CI across specificity grid via pROC ci.se
  cse <- tryCatch(suppressWarnings(
    pROC::ci.se(roc_obj, specificities = sp, boot.n = n_boot, conf.level = conf.level, boot.stratified = TRUE)
  ), error = function(e) NULL)
  
  # 2. Bootstrap AUC 95% CI via pROC ci.auc
  b_auc <- tryCatch(suppressWarnings(
    pROC::ci.auc(roc_obj, method = "bootstrap", boot.n = n_boot, conf.level = conf.level, boot.stratified = TRUE)
  ), error = function(e) {
    tryCatch(as.numeric(pROC::ci.auc(roc_obj, conf.level = conf.level))[c(1, 3)], error = function(e) c(NA_real_, NA_real_))
  })
  
  ci_auc <- as.numeric(b_auc)[c(1, 3)]
  
  band_df <- if (!is.null(cse)) {
    data.frame(
      x = 1 - c(sp, rev(sp)),
      y = c(cse[, 1], rev(cse[, 3]))
    )
  } else NULL
  
  list(ci_auc = ci_auc, band_df = band_df)
}

# =============================================================================
# 4. THE WORKHORSE -- roc_summary()
# =============================================================================
#' Aggregate a roc_set into everything the plot needs
#'
#' @param average "lab" | "fpr" | "threshold" | "pooled" | "none"
#' @param band    "sd" | "se" | "ci" | "none"
#' @param n_grid  interpolation points (lab standard = 100)
#' @param n_boot  bootstrap replicates when band = "ci"
#' @return list(curve, band, folds, auc, ...) -- see fields below
roc_summary <- function(x, truth = NULL, classname = NULL,
                        average = c("lab", "fpr", "threshold", "pooled", "none"),
                        band = c("sd", "se", "ci", "none"),
                        n_grid = 100, ci_level = 0.95, n_boot = 200,
                        seed = 42, ...) {

  average <- match.arg(average)
  band    <- match.arg(band)
  rs <- as_roc_set(x, truth = truth, classname = classname, ...)

  # ---- geometry-only input: pass the stored curve straight through -----------
  # (web-app roc_data, or a pipeline result whose models were dropped). There
  # are no raw predictions, so `average` and `band` cannot be honoured.
  if (!is.null(rs$precomputed)) {
    pc <- rs$precomputed
    if (average != "lab")
      warning("Input carries only a finished curve; `average = \"", average,
              "\"` ignored.", call. = FALSE)
    return(list(
      curve = pc$curve,
      band  = if (band == "none") NULL else pc$band,
      folds = NULL,
      auc = list(per_curve = pc$auc_tot, mean = pc$auc_mean, sd = pc$auc_sd,
                 se = NA_real_, ci = NULL, pooled = NULL, pooled_ci = NULL),
      average = "pre-computed", band_type = if (is.null(pc$band)) "none" else "sd",
      n_curves = rs$n_curves, classname = rs$classname, label = rs$label,
      source = rs$source, roc_set = rs, pooled_roc = NULL))
  }

  rocs <- rs$rocs
  k <- length(rocs)
  auc_tot <- vapply(rocs, function(r) as.numeric(r$auc), numeric(1))

  # ---- per-fold curves, always available for the spaghetti layer -------------
  folds_df <- do.call(rbind, lapply(seq_along(rocs), function(i) {
    data.frame(fold = i,
               fpr = 1 - rocs[[i]]$specificities,
               tpr = rocs[[i]]$sensitivities)
  }))
  folds_df <- folds_df[order(folds_df$fold, folds_df$fpr, folds_df$tpr), ]

  # ---- pooled AUC + Bootstrap CI, computed whenever we have raw predictions -----
  pooled_roc <- NULL; pooled_auc <- NULL; pooled_ci <- NULL
  pooled_ok <- tryCatch({
    pr <- .agg_pooled(rocs, rs$classname)$pooled_roc; !is.null(pr)
  }, error = function(e) FALSE)
  if (isTRUE(pooled_ok)) {
    pooled_roc <- .agg_pooled(rocs, rs$classname)$pooled_roc
    pooled_auc <- as.numeric(pooled_roc$auc)
    if (k == 1 && !is.null(rocs[[1]]$ci_auc)) {
      pooled_ci <- rocs[[1]]$ci_auc
    } else {
      b_tmp <- tryCatch(compute_bootstrap_roc(pooled_roc, n_boot = n_boot,
                                              conf.level = ci_level, n_grid = n_grid, seed = seed),
                        error = function(e) NULL)
      pooled_ci <- if (!is.null(b_tmp)) b_tmp$ci_auc else NULL
    }
  }

  # ---- aggregate -------------------------------------------------------------
  agg <- switch(average,
    lab       = .agg_lab(rocs, n_grid),
    fpr       = .agg_fpr(rocs, n_grid),
    threshold = .agg_threshold(rocs, n_grid),
    pooled    = .agg_pooled(rocs, rs$classname, n_grid),
    none      = NULL)

  # ---- "none": no mean curve, the folds ARE the figure -----------------------
  if (average == "none") {
    return(list(curve = NULL, band = NULL, folds = folds_df,
                auc = list(per_curve = auc_tot, mean = mean(auc_tot),
                           sd = stats::sd(auc_tot), se = stats::sd(auc_tot) / sqrt(k),
                           ci = NULL, pooled = pooled_auc, pooled_ci = pooled_ci),
                average = average, band_type = "none", n_curves = k,
                classname = rs$classname, label = rs$label, source = rs$source,
                roc_set = rs, pooled_roc = pooled_roc))
  }

  # ---- mean curve ------------------------------------------------------------
  if (average == "pooled") {
    mean_sen <- agg$sen_mat[, 1]; mean_spe <- agg$spe_mat[, 1]
    sd_sen <- sd_spe <- rep(0, length(mean_sen))
  } else {
    mean_sen <- apply(agg$sen_mat, 1, mean)
    mean_spe <- apply(agg$spe_mat, 1, mean)
    sd_sen   <- if (k > 1) apply(agg$sen_mat, 1, stats::sd) else rep(0, n_grid)
    sd_spe   <- if (k > 1) apply(agg$spe_mat, 1, stats::sd) else rep(0, n_grid)
  }

  curve <- data.frame(fpr = rev(1 - mean_spe), tpr = rev(mean_sen))
  rownames(curve) <- NULL

  # ---- band ------------------------------------------------------------------
  band_df <- NULL
  if (band != "none" && average != "pooled" && k > 1) {
    mult <- switch(band, sd = 1, se = 1 / sqrt(k), ci = 1)
    if (band == "ci") {
      # percentile band across folds (bootstrap over folds, not over samples)
      set.seed(seed)
      a <- (1 - ci_level) / 2
      B <- min(n_boot, 2000)
      idx <- matrix(sample(seq_len(k), B * k, replace = TRUE), nrow = k)
      bs_sen <- apply(idx, 2, function(j) rowMeans(agg$sen_mat[, j, drop = FALSE]))
      bs_spe <- apply(idx, 2, function(j) rowMeans(agg$spe_mat[, j, drop = FALSE]))
      lo_sen <- apply(bs_sen, 1, stats::quantile, probs = a)
      hi_sen <- apply(bs_sen, 1, stats::quantile, probs = 1 - a)
      lo_spe <- apply(bs_spe, 1, stats::quantile, probs = a)
      hi_spe <- apply(bs_spe, 1, stats::quantile, probs = 1 - a)
    } else {
      lo_sen <- mean_sen - mult * sd_sen; hi_sen <- mean_sen + mult * sd_sen
      lo_spe <- mean_spe - mult * sd_spe; hi_spe <- mean_spe + mult * sd_spe
    }
    # polygon exactly as the pipeline builds it (lower path, then upper reversed)
    band_df <- data.frame(x = 1 - c(lo_spe, rev(hi_spe)),
                          y =     c(lo_sen, rev(hi_sen)))
  } else if (band == "ci" && k == 1 && !is.null(pooled_roc)) {
    # Check if pre-computed band and CI exist
    if (!is.null(rocs[[1]]$band_df)) {
      band_df <- rocs[[1]]$band_df
      if (is.null(pooled_ci) && !is.null(rocs[[1]]$ci_auc)) {
        pooled_ci <- rocs[[1]]$ci_auc
      }
    } else {
      b_res <- tryCatch(compute_bootstrap_roc(rocs[[1]], n_boot = n_boot,
                                              conf.level = ci_level, n_grid = n_grid, seed = seed),
                        error = function(e) NULL)
      if (!is.null(b_res)) {
        band_df <- b_res$band_df
        if (is.null(pooled_ci)) pooled_ci <- b_res$ci_auc
      }
    }
  }

  auc_ci <- if (k > 1) {
    z <- stats::qnorm(1 - (1 - ci_level) / 2)
    mean(auc_tot) + c(-1, 1) * z * stats::sd(auc_tot) / sqrt(k)
  } else if (!is.null(rocs[[1]]$ci_auc)) {
    rocs[[1]]$ci_auc
  } else pooled_ci

  list(curve = curve, band = band_df, folds = folds_df,
       auc = list(per_curve = auc_tot, mean = mean(auc_tot),
                  sd = if (k > 1) stats::sd(auc_tot) else NA_real_,
                  se = if (k > 1) stats::sd(auc_tot) / sqrt(k) else NA_real_,
                  ci = auc_ci, pooled = pooled_auc, pooled_ci = pooled_ci),
       average = average, band_type = band, n_curves = k,
       classname = rs$classname, label = rs$label, source = rs$source,
       roc_set = rs, pooled_roc = pooled_roc, grid = agg$grid,
       sen_mat = agg$sen_mat, spe_mat = agg$spe_mat)
}

# =============================================================================
# 5. AUC LABEL TEXT
# =============================================================================
.auc_text <- function(s, auc_label = c("auto", "mean_sd", "mean_ci",
                                       "pooled_ci", "auc", "none")) {
  auc_label <- match.arg(auc_label)
  f <- function(v, d = 2) formatC(v, format = "f", digits = d)
  if (auc_label == "auto")
    auc_label <- if (s$average == "pooled") "pooled_ci"
                 else if (s$n_curves > 1) "mean_sd" else "auc"
  switch(auc_label,
    none      = NULL,
    auc       = paste("AUC:", f(s$auc$pooled %||% s$auc$mean)),
    mean_sd   = paste("AUC:", f(s$auc$mean), "±", f(s$auc$sd)),
    mean_ci   = sprintf("AUC: %s (%s–%s)", f(s$auc$mean),
                        f(s$auc$ci[1]), f(s$auc$ci[2])),
    pooled_ci = if (!is.null(s$auc$pooled_ci))
                  sprintf("AUC: %s (95%% CI %s–%s)", f(s$auc$pooled),
                          f(s$auc$pooled_ci[1]), f(s$auc$pooled_ci[2]))
                else paste("AUC:", f(s$auc$pooled %||% s$auc$mean)))
}

# =============================================================================
# 6. plot_roc() -- ONE CURVE, LAB STYLE
# =============================================================================
#' @param x          anything as_roc_set() understands
#' @param average    see roc_summary(); default "lab" = pipeline-identical
#' @param band       "sd" (lab default) | "se" | "ci" | "none"
#' @param show_folds draw thin per-fold curves underneath
#' @param mark_best  mark the Youden-optimal operating point (pooled curve)
#' @param partial_auc c(lo, hi) specificity range to shade, or NULL
#' @param legend_box draw the lab's manual "Mean ROC / Standard Deviation" keys
plot_roc <- function(x, truth = NULL, classname = NULL, title = NULL,
                     average = c("lab", "fpr", "threshold", "pooled", "none"),
                     band = c("sd", "se", "ci", "none"),
                     auc_label = c("auto", "mean_sd", "mean_ci", "pooled_ci",
                                   "auc", "none"),
                     show_folds = FALSE, mark_best = FALSE, partial_auc = NULL,
                     legend_box = TRUE, style = roc_style(),
                     n_grid = 100, ci_level = 0.95, n_boot = 200, ...) {

  average   <- match.arg(average)
  band      <- match.arg(band)
  auc_label <- match.arg(auc_label)

  s <- roc_summary(x, truth = truth, classname = classname, average = average,
                   band = band, n_grid = n_grid, ci_level = ci_level,
                   n_boot = n_boot, ...)

  title <- title %||% s$label %||%
    if (!is.null(s$classname)) paste(s$classname, collapse = " vs ") else NULL

  p <- ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                alpha = 1, colour = style$diag_color)

  # partial-AUC shading (specificity window), drawn under everything ----------
  if (!is.null(partial_auc) && !is.null(s$curve)) {
    lo <- 1 - max(partial_auc); hi <- 1 - min(partial_auc)
    p <- p + ggplot2::annotate("rect", xmin = lo, xmax = hi, ymin = 0, ymax = 1,
                      fill = "grey60", alpha = 0.12)
  }

  if (!is.null(s$band))
    p <- p + ggplot2::geom_polygon(data = s$band, ggplot2::aes(x = x, y = y),
                          fill = style$band_color, alpha = style$band_alpha)

  if (show_folds && !is.null(s$folds))
    p <- p + ggplot2::geom_line(data = s$folds, ggplot2::aes(x = fpr, y = tpr, group = fold),
                       colour = style$fold_color, alpha = style$fold_alpha,
                       linewidth = style$fold_size)

  if (!is.null(s$curve)) {
    p <- p + ggplot2::geom_line(data = s$curve, ggplot2::aes(x = fpr, y = tpr),
                       colour = style$line_color, linewidth = style$line_size)
  } else {
    # average = "none": every fold is a curve of its own
    p <- p + ggplot2::geom_line(data = s$folds, ggplot2::aes(x = fpr, y = tpr, group = fold),
                       colour = style$line_color, alpha = 0.8,
                       linewidth = style$fold_size * 1.6)
  }

  if (mark_best && !is.null(s$pooled_roc)) {
    b <- roc_best_threshold(s$pooled_roc)
    p <- p + ggplot2::annotate("point", x = 1 - b$specificity, y = b$sensitivity,
                      size = 3, colour = style$line_color) +
             ggplot2::annotate("text", x = 1 - b$specificity + 0.03, y = b$sensitivity - 0.05,
                      hjust = 0, size = 4.2,
                      label = sprintf("thr = %.2f\nSe %.2f / Sp %.2f",
                                      b$threshold, b$sensitivity, b$specificity))
  }

  p <- p +
    theme_Publication(base_size = style$base_size) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate", title = title) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0.02, 0.02),
                       limits = c(-0.02, 1.02)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0.02, 0.02),
                       limits = c(-0.02, 1.02)) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = style$axis_title_size, face = "bold"),
          axis.text  = ggplot2::element_text(size = style$axis_text_size),
          plot.title = ggplot2::element_text(size = style$title_size, face = "bold"))

  # AUC annotation + manual legend keys, at the pipeline's coordinates --------
  lab <- .auc_text(s, auc_label)
  if (!is.null(lab))
    p <- p + ggplot2::annotate("text", x = 0.72, y = 0.16, vjust = 0, size = 7, label = lab)

  if (legend_box && !is.null(s$curve)) {
    p <- p + ggplot2::annotate("text", x = 0.848, y = 0.09, vjust = 0, size = 5,
                      label = if (s$average == "pooled") "Pooled ROC"
                              else if (s$n_curves > 1) "Mean ROC" else "ROC") +
             ggplot2::annotate("segment", x = 0.64, xend = 0.72, y = 0.106, yend = 0.106,
                      colour = style$line_color, linewidth = style$line_size)
    if (!is.null(s$band)) {
      key <- switch(s$band_type,
                    sd = "Standard Deviation",
                    se = "Standard Error",
                    ci = sprintf("%g%% CI", 100 * ci_level), "")
      p <- p + ggplot2::annotate("text", x = 0.772, y = 0.02, vjust = 0, size = 5, label = key) +
               ggplot2::annotate("rect", xmin = 0.48, xmax = 0.56, ymin = 0.02, ymax = 0.05,
                        alpha = .3, fill = style$band_color)
    }
  }

  attr(p, "roc_summary") <- s
  p
}

# =============================================================================
# 7. plot_roc_multi() -- SEVERAL CURVES ON ONE AXIS
# =============================================================================
#' @param xs   NAMED list; each element is anything as_roc_set() understands
#'             (models, omics layers, cohorts, feature sets, ...)
#' @param truth,classname  recycled to every element when they need it
#' @param bands draw the SD/CI ribbon for every curve too (busy but honest)
plot_roc_multi <- function(xs, truth = NULL, classname = NULL, title = NULL,
                           average = c("lab", "fpr", "threshold", "pooled"),
                           band = c("sd", "se", "ci", "none"),
                           bands = FALSE, show_auc_in_legend = TRUE,
                           auc_digits = 2, legend_title = NULL,
                           style = roc_style(), n_grid = 100,
                           ci_level = 0.95, n_boot = 100, ...) {

  average <- match.arg(average)
  band    <- match.arg(band)
  if (is.null(names(xs)) || any(names(xs) == ""))
    stop("`xs` must be a NAMED list, e.g. list(RF = res_rf, SVM = res_svm).")

  sums <- lapply(seq_along(xs), function(i)
    roc_summary(xs[[i]], truth = truth, classname = classname,
                average = average, band = if (bands) band else "none",
                n_grid = n_grid, ci_level = ci_level, n_boot = n_boot, ...))
  names(sums) <- names(xs)

  keys <- vapply(names(sums), function(nm) {
    s <- sums[[nm]]
    if (!show_auc_in_legend) return(nm)
    f <- function(v) formatC(v, format = "f", digits = auc_digits)
    if (!is.null(s$auc$ci) && !any(is.na(s$auc$ci)))
      sprintf("%s (AUC %s, 95%% CI [%s, %s])", nm, f(s$auc$pooled %||% s$auc$mean), f(s$auc$ci[1]), f(s$auc$ci[2]))
    else if (s$n_curves > 1 && !is.na(s$auc$sd))
      sprintf("%s (AUC %s ± %s)", nm, f(s$auc$mean), f(s$auc$sd))
    else sprintf("%s (AUC %s)", nm, f(s$auc$pooled %||% s$auc$mean))
  }, character(1), USE.NAMES = FALSE)

  curve_df <- do.call(rbind, lapply(seq_along(sums), function(i) {
    d <- sums[[i]]$curve; rownames(d) <- NULL
    if (is.null(d) || nrow(d) == 0) {
      data.frame(fpr = numeric(0), tpr = numeric(0), Curve = character(0), stringsAsFactors = FALSE)
    } else {
      data.frame(d, Curve = keys[i], stringsAsFactors = FALSE)
    }
  }))
  curve_df$Curve <- factor(curve_df$Curve, levels = keys)

  cols <- rep(style$palette, length.out = length(keys))
  names(cols) <- keys

  # Map specific colors to models so STABL is always orange (#EDAE49)
  model_colors <- c(
    "STABL" = "#edd249",
    "Boruta" = "#f07e45",
    "GBM" = "#D1495B",
    "RF" = "#366993", 
    "Logistic Regression" = "#8D6A9F",
    "SVM" = "#66A182"
  )
  for (i in seq_along(keys)) {
    for (m_name in names(model_colors)) {
      if (grepl(m_name, keys[i], fixed = TRUE)) {
        cols[i] <- model_colors[m_name]
        break
      }
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = style$diag_color)

  if (bands) {
    band_df <- do.call(rbind, lapply(seq_along(sums), function(i) {
      b <- sums[[i]]$band; if (is.null(b)) return(NULL)
      rownames(b) <- NULL
      if (nrow(b) == 0) {
        data.frame(x = numeric(0), y = numeric(0), Curve = character(0), stringsAsFactors = FALSE)
      } else {
        data.frame(b, Curve = keys[i], stringsAsFactors = FALSE)
      }
    }))
    if (!is.null(band_df))
      p <- p + ggplot2::geom_polygon(data = band_df, ggplot2::aes(x = x, y = y, fill = Curve),
                            alpha = style$band_alpha, colour = NA) +
               ggplot2::scale_fill_manual(values = cols, guide = "none")
  }

  p +
    ggplot2::geom_line(data = curve_df, ggplot2::aes(x = fpr, y = tpr, colour = Curve),
              linewidth = style$line_size) +
    ggplot2::scale_colour_manual(values = cols, name = legend_title %||% "", guide = ggplot2::guide_legend(ncol = 2)) +
    theme_Publication(base_size = style$base_size) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate", title = title) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), expand = c(0.02, 0.02),
                       limits = c(-0.02, 1.02)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0.02, 0.02),
                       limits = c(-0.02, 1.02)) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = style$axis_title_size, face = "bold"),
          axis.text  = ggplot2::element_text(size = style$axis_text_size),
          plot.title = ggplot2::element_text(size = style$title_size, face = "bold"),
          legend.position = "bottom", legend.direction = "horizontal",
          legend.text = ggplot2::element_text(size = 16),
          legend.key.size = grid::unit(0.4, "cm")) -> pp

  attr(pp, "roc_summaries") <- sums
  pp
}

# =============================================================================
# 8. plot_roc_folds() -- MEAN CURVE + PER-FOLD SPAGHETTI
# =============================================================================
# The honest version of the lab figure: shows how variable the outer folds are.
plot_roc_folds <- function(x, ..., show_folds = TRUE) {
  plot_roc(x, ..., show_folds = show_folds)
}

# =============================================================================
# 9. NUMBERS: roc_stats(), roc_best_threshold(), roc_compare()
# =============================================================================

#' AUC + operating-point metrics, per fold and aggregated
#'
#' @param threshold  fixed cut-off, or "youden" / "closest.topleft" to optimise
#'                   per fold, or 0.5 for the default caret rule
roc_stats <- function(x, truth = NULL, classname = NULL, threshold = 0.5,
                      average = "lab", ci_level = 0.95, ...) {
  s <- roc_summary(x, truth = truth, classname = classname,
                   average = average, band = "none", ci_level = ci_level, ...)
  rocs <- s$roc_set$rocs
  if (is.null(rocs))
    return(data.frame(Metric = c("AUC, Mean", "AUC, SD"),
                      Value = c(s$auc$mean, s$auc$sd)))

  per <- do.call(rbind, lapply(seq_along(rocs), function(i) {
    r <- rocs[[i]]
    co <- if (is.character(threshold))
      pROC::coords(r, "best", best.method = threshold, ret = c("threshold",
                   "sensitivity", "specificity"), transpose = FALSE)[1, ]
    else
      pROC::coords(r, threshold, input = "threshold",
                   ret = c("threshold", "sensitivity", "specificity"),
                   transpose = FALSE)[1, ]
    data.frame(fold = i, AUC = as.numeric(r$auc),
               Threshold = as.numeric(co[["threshold"]]),
               Sensitivity = as.numeric(co[["sensitivity"]]),
               Specificity = as.numeric(co[["specificity"]]),
               Balanced_Accuracy = (as.numeric(co[["sensitivity"]]) +
                                    as.numeric(co[["specificity"]])) / 2)
  }))

  agg <- data.frame(
    Metric = c("AUC", "Sensitivity", "Specificity", "Balanced Accuracy"),
    Mean = c(mean(per$AUC), mean(per$Sensitivity),
             mean(per$Specificity), mean(per$Balanced_Accuracy)),
    SD = c(stats::sd(per$AUC), stats::sd(per$Sensitivity),
           stats::sd(per$Specificity), stats::sd(per$Balanced_Accuracy)))
  attr(agg, "per_fold") <- per
  attr(agg, "pooled_auc") <- s$auc$pooled
  attr(agg, "pooled_auc_ci") <- s$auc$pooled_ci
  agg
}

#' Youden-optimal (or other) operating point of a single roc
roc_best_threshold <- function(r, method = "youden") {
  co <- pROC::coords(r, "best", best.method = method,
                     ret = c("threshold", "sensitivity", "specificity"),
                     transpose = FALSE)
  list(threshold = as.numeric(co[1, "threshold"]),
       sensitivity = as.numeric(co[1, "sensitivity"]),
       specificity = as.numeric(co[1, "specificity"]))
}

#' DeLong (or bootstrap) test between two ROCs
#'
#' Use on POOLED curves. `paired = TRUE` requires the two predictors to be for
#' the same samples in the same order (e.g. two models on one test set).
roc_compare <- function(x, y, truth = NULL, classname = NULL,
                        paired = FALSE, method = c("delong", "bootstrap"), ...) {
  method <- match.arg(method)
  sx <- roc_summary(x, truth = truth, classname = classname,
                    average = "pooled", band = "none", ...)
  sy <- roc_summary(y, truth = truth, classname = classname,
                    average = "pooled", band = "none", ...)
  pROC::roc.test(sx$pooled_roc, sy$pooled_roc, paired = paired, method = method)
}

# =============================================================================
# 10. MULTICLASS: ONE-VS-REST
# =============================================================================
#' Build a named list of one-vs-rest roc_sets, ready for plot_roc_multi()
#'
#' @param prob   data.frame of class probabilities (one column per class)
#' @param truth  vector of true class labels
roc_ovr <- function(prob, truth, classes = NULL) {
  classes <- classes %||% (if (is.factor(truth)) levels(truth)
                           else sort(unique(as.character(truth))))
  out <- lapply(classes, function(cl) {
    bin <- factor(ifelse(as.character(truth) == cl, cl, "Rest"),
                  levels = c(cl, "Rest"))
    as_roc_set(as.numeric(prob[[cl]]), truth = bin, classname = c(cl, "Rest"),
               label = paste(cl, "vs rest"))
  })
  names(out) <- classes
  out
}

# =============================================================================
# 11. EXPORT
# =============================================================================
#' Tidy data.frame of the plotted geometry -- put this next to the figure so the
#' curve is reproducible without re-running the models.
roc_curve_data <- function(x, ..., include_folds = FALSE) {
  s <- if (inherits(x, "ggplot")) attr(x, "roc_summary") else roc_summary(x, ...)
  out <- if (!is.null(s$curve)) data.frame(s$curve, part = "mean") else NULL
  if (!is.null(s$band))
    out <- rbind(out, data.frame(fpr = s$band$x, tpr = s$band$y, part = "band"))
  if (include_folds && !is.null(s$folds))
    out <- rbind(out, data.frame(fpr = s$folds$fpr, tpr = s$folds$tpr,
                                 part = paste0("fold", s$folds$fold)))
  out
}

#' Save at the lab's figure size (square, 300 dpi, white background)
save_roc <- function(p, file, width = 10, height = 7, dpi = 300) {
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height,
                  dpi = dpi, bg = "white")
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}
