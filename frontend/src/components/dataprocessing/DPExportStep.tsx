import { useState } from "react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import SharedExportStep, { type ExportGroup, type StatChip } from "../shared/SharedExportStep";
import { getHierarchyGroup } from "../../dataObject";
import { useToast } from "../../hooks/use-toast";
import { downloadReportAPI } from "../../lib/api";

interface Props {
  datasetId: string;
}

export default function DPExportStep({ datasetId }: Props) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const [loadingReport, setLoadingReport] = useState(false);

  const handleDownloadReport = async (format: "md" | "pdf") => {
    setLoadingReport(true);
    try {
      const userId = getSessionUserId();
      const { blob, filename } = await downloadReportAPI(userId, "dp", format);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename;
      a.click();
      URL.revokeObjectURL(url);
    } catch (e: any) {
      console.error("Report download failed:", e);
      toast({
        title: "Report Download Failed",
        description: e.message || "Failed to download the analysis report.",
        variant: "destructive",
      });
    } finally {
      setLoadingReport(false);
    }
  };

  const allDpDs = Array.isArray(state.dpDatasets) ? state.dpDatasets : [];
  const datasetsToExport = datasetId === "all"
    ? allDpDs
    : allDpDs.filter((d) => d && d.id === datasetId);

  if (datasetsToExport.length === 0) return null;
  const ds = datasetsToExport[0];

  const multipleDs = allDpDs.length > 1;
  const totalSamples = multipleDs
    ? allDpDs.reduce((a, d) => a + (d?.nSamples || 0), 0)
    : (ds?.nSamples || 0);

  const getModelLabel = (m: string) => {
    switch (m) {
      case "logistic": return "Logistic Regression";
      case "svm": return "Support Vector Machine";
      case "randomforest": return "Random Forest";
      case "gbm": return "Gradient Boosting Model";
      case "stabl": return "STABL";
      case "boruta": return "Boruta";
      default: return m;
    }
  };

  // ── Determine FS pooling mode ──────────────────────────────────────────────
  const isAllSameType = allDpDs.length > 1 &&
    new Set(allDpDs.map(d => d?.dataType || "readcounts")).size === 1 &&
    (allDpDs[0]?.dataType || "readcounts") !== "others";
  const isCombinedPool = allDpDs.length === 1 ||
    (isAllSameType && (state.fsConfig.multiDatasetMode ?? "combine") === "combine");

  const cvEnabled = state.fsConfig.cvEnabled;

  const groups: ExportGroup[] = [];

  datasetsToExport.forEach((d) => {
    const isOthers = d.dataType === "others";
    const dsSuffix = multipleDs ? ` of ${d.name}` : "";
    const dpGroupName = multipleDs ? `Data Processing Results for ${d.name}` : "Data Processing";
    const deGroupName = multipleDs ? `DE Analysis Results for ${d.name}` : "DE Analysis";

    // 1. Organism
    const organism = state.dpAnnotationConfig.selectOrganism || "Human (Homo sapiens)";

    // 2. Processing (Filter + Missing Method)
    const methodsList = (state.dpProcessingConfig.filterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
    const filterParts: string[] = [];
    if (methodsList.includes("cpm")) {
      filterParts.push(`above the ${state.dpProcessingConfig.cpmThreshold} CPM value in at least ${state.dpProcessingConfig.minSamples} number of samples`);
    }
    if (methodsList.includes("min_count")) {
      filterParts.push(`above the ${state.dpProcessingConfig.countThreshold} sum counts across all samples`);
    }
    if (methodsList.includes("variance")) {
      filterParts.push(`above the ${state.dpProcessingConfig.varianceThreshold}% variance across all samples`);
    }
    const filterLabel = filterParts.length > 0 ? filterParts.join(" and ") : "No Filtering";
    const missingLabel = state.dpProcessingConfig.missingMethod === "knn" ? "kNN" : state.dpProcessingConfig.missingMethod;

    // 3. Normalization Method
    const getNormMethodLabel = (m: string) => {
      switch (m?.toLowerCase()) {
        case "tmm": return "Trimmed-mean of M values method";
        case "rle": return "Regularized log transformation";
        case "upperquantile": return "Upper Quartile method";
        case "cpm": return "Counts per million method";
        case "vsn": return "VSN";
        case "none": return "None";
        default: return m || "Trimmed-mean of M values method";
      }
    };

    // 4. Batch Method
    const getBatchMethodLabel = () => {
      const method = isOthers ? state.dpBatchConfig.methodOthers : state.dpBatchConfig.method;
      switch (method) {
        case "combat_seq": return "ComBat-seq using sva R package";
        case "combat": return "ComBat using sva R package";
        case "limma_removebatch": return "removeBatchEffect using sva R package";
        default: return method || "ComBat-seq using sva R package";
      }
    };
    const batchMethodLabel = getBatchMethodLabel();

    // 5. DE Method
    const getDeMethodLabel = () => {
      const deMethod = state.deConfig.method || "deseq2";
      switch (deMethod) {
        case "deseq2": return "with DESeq2 R package";
        case "edger": return "with edgeR R package";
        case "limma_voom": return "with limma-voom R package";
        case "limma": return "with limma R package";
        default: return deMethod;
      }
    };
    const deMethodLabel = getDeMethodLabel();

    // Build files for Data Processing group
    const dpFiles = [
      {
        id: `final_matrix|${d.id}`,
        label: `Final Expression Matrix${dsSuffix}`,
        ext: "CSV",
        desc: `The final, fully processed expression matrix ready for downstream analysis${dsSuffix}`,
      },
      {
        id: `final_clinical|${d.id}`,
        label: `Final Clinical Data${dsSuffix}`,
        ext: "CSV",
        desc: `The final clinical metadata aligned and matching the expression matrix${dsSuffix}`,
      },
      ...(!d.batchDone
        ? [
            {
              id: `pca_plots|${d.id}`,
              label: `PCA Plots${dsSuffix}`,
              ext: "PDF",
              desc: `Principal component analysis plots of the expression data${dsSuffix}`,
            },
          ]
        : []),
      ...(!isOthers && d.annotationDone
        ? [
            {
              id: `annotated_matrix|${d.id}`,
              label: `Annotated Expression Data${dsSuffix}`,
              ext: "CSV",
              desc: `Expression data that was mapped with ${organism} as reference${dsSuffix}`,
            },
            {
              id: `annotation_results|${d.id}`,
              label: `Annotation Results${dsSuffix}`,
              ext: "CSV",
              desc: `Detailed annotation results${dsSuffix}`,
            },
          ]
        : []),
      ...(d.processingDone
        ? [
            {
              id: `processed_matrix|${d.id}`,
              label: `Processed Expression Data${dsSuffix}`,
              ext: "CSV",
              desc: filterLabel === "No Filtering"
                ? `Processed Data with No Filtering${d.hasNA ? ` and handling missing value by ${missingLabel}` : ""}${dsSuffix}`
                : `Processed Data with filtering: ${filterLabel}${d.hasNA ? ` and handling missing value by ${missingLabel}` : ""}${dsSuffix}`,
            },
          ]
        : []),
      ...(d.normalizationDone
        ? [
            {
              id: `boxplot_before|${d.id}`,
              label: `Normalization Box Plot (Before)${dsSuffix}`,
              ext: "PDF",
              desc: `Box plot showing sample distribution before normalization${dsSuffix}`,
            },
            {
              id: `boxplot_after|${d.id}`,
              label: `Normalization Box Plot (After)${dsSuffix}`,
              ext: "PDF",
              desc: `Box plot showing sample distribution after normalization${dsSuffix}`,
            },
            {
              id: `normalized_matrix|${d.id}`,
              label: `Normalized Expression Data${dsSuffix}`,
              ext: "CSV",
              desc: `Normalized Data using ${getNormMethodLabel(d.normalizationMethod || state.dpNormConfig.method)}${dsSuffix}`,
            },
          ]
        : [
            {
              id: `boxplot_before|${d.id}`,
              label: `Sample Box Plots${dsSuffix}`,
              ext: "PDF",
              desc: `Box plot showing per-sample expression distribution${dsSuffix}`,
            },
          ]),
      ...(d.batchDone
        ? [
            {
              id: `batch_corrected_matrix|${d.id}`,
              label: `Batch-Corrected Expression Data${dsSuffix}`,
              ext: "CSV",
              desc: `Batch-corrected Data using ${batchMethodLabel}${dsSuffix}`,
            },
            {
              id: `pca_plots_before_batch|${d.id}`,
              label: `PCA Plots (Before Batch Correction)${dsSuffix}`,
              ext: "PDF",
              desc: `PCA plot showing sample distribution before batch correction${dsSuffix}`,
            },
            {
              id: `pca_plots_after_batch|${d.id}`,
              label: `PCA Plots (After Batch Correction)${dsSuffix}`,
              ext: "PDF",
              desc: `PCA plot showing sample distribution after batch correction${dsSuffix}`,
            },
          ]
        : []),
    ];

    groups.push({
      name: dpGroupName,
      badge: "Data Processing",
      badgeColor: "hsl(214 60% 35%)",
      datasetId: d.id,
      datasetLabel: d.name,
      files: dpFiles,
    });

    if (state.dpInlineDeDone) {
      const deFiles = [
        {
          id: `de_all_results|${d.id}`,
          label: `All Features Table${dsSuffix}`,
          ext: "CSV",
          desc: `Complete DE results matrix including all genes performed by using ${deMethodLabel} method (adjusted p-value cutoff: ${state.deConfig.pValueThreshold}, adjustment method: ${state.deConfig.adjustMethod}, log₂FC threshold: |${state.deConfig.logFcThreshold}|)${dsSuffix}`,
        },
        {
          id: `de_sig_results|${d.id}`,
          label: `Significant Features Table${dsSuffix}`,
          ext: "CSV",
          desc: `Significant gene sets filtered by thresholds using ${deMethodLabel} method (adjusted p-value cutoff: ${state.deConfig.pValueThreshold}, adjustment method: ${state.deConfig.adjustMethod}, log₂FC threshold: |${state.deConfig.logFcThreshold}|)${dsSuffix}`,
        },
        {
          id: `de_volcano|${d.id}`,
          label: `Volcano Plot${dsSuffix}`,
          ext: "PDF",
          desc: `Volcano plot visualization highlighting log₂ Fold Change vs Statistical Significance (-log₁₀ p-value) for ${d.name}`,
        },
        {
          id: `de_ma|${d.id}`,
          label: `MA Plot${dsSuffix}`,
          ext: "PDF",
          desc: `MA plot mapping Average Log Expression vs Log Fold Change for ${d.name}`,
        },
        {
          id: `de_heatmap_top10|${d.id}`,
          label: `Top 10 Significant Genes Heatmap${dsSuffix}`,
          ext: "PDF",
          desc: `Clustered heatmap of the top 10 most significant differentially expressed genes${dsSuffix}, showing expression patterns across all samples`,
        },
      ];

      groups.push({
        name: deGroupName,
        badge: "DE Analysis",
        badgeColor: "hsl(0 65% 55%)",
        datasetId: d.id,
        datasetLabel: d.name,
        files: deFiles,
      });
    }
  });

  // ── Inline FS Export Groups ────────────────────────────────────────────────
  if (state.dpInlineFsDone) {
    const selectedModels = Array.isArray(state.fsConfig.selectedModels) ? state.fsConfig.selectedModels : [];

    const trainingDs = allDpDs.filter(
      d => d && (d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" ||
           d.fs_datasetPurpose === "train-and-cv" || d.fs_datasetPurpose === "train-cv-test" || !d.fs_datasetPurpose)
    );
    const testingDs = allDpDs.filter(d => d && d.fs_datasetPurpose === "test");

    const processedTrainDss = (() => {
      const mode = state.fsConfig.multiDatasetMode ?? "combine";
      if (mode !== "combine" || trainingDs.length <= 1) {
        return trainingDs.map(d => ({
          id: d.id,
          name: d.name,
          isPooled: false,
          dataType: d.dataType || "readcounts",
          originalDatasets: [d],
        }));
      }

      const typeGroups: Record<string, typeof trainingDs> = {};
      trainingDs.forEach(d => {
        const dt = d.dataType || "readcounts";
        if (!typeGroups[dt]) typeGroups[dt] = [];
        typeGroups[dt].push(d);
      });

      const result: { id: string; name: string; isPooled: boolean; dataType: string; originalDatasets: typeof trainingDs }[] = [];
      Object.keys(typeGroups).forEach(dt => {
        const group = typeGroups[dt];
        if (dt !== "others" && group.length > 1) {
          const firstDsId = group[0].id;
          const userId = firstDsId.match(/usr_[a-zA-Z0-9]+/)?.[0] || "";
          const mergedId = userId ? `${userId}_merged_${dt}_training_fs` : `merged_${dt}_training_fs`;
          result.push({
            id: mergedId,
            name: `Pooled ${dt.toUpperCase()} Training Datasets`,
            isPooled: true,
            dataType: dt,
            originalDatasets: group,
          });
        } else {
          group.forEach(d => {
            result.push({
              id: d.id,
              name: d.name,
              isPooled: false,
              dataType: d.dataType || "readcounts",
              originalDatasets: [d],
            });
          });
        }
      });

      return result;
    })();

    processedTrainDss.forEach((processedTrain) => {
      const hasInternal = processedTrain.originalDatasets.some(d =>
        d.fs_datasetPurpose === "train-and-test" ||
        d.fs_datasetPurpose === "train-cv-test" ||
        ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
      );

      const processedTrainHier = getHierarchyGroup(processedTrain.dataType);
      const matchedTestDs = testingDs.filter(
        t => getHierarchyGroup(t.dataType || "readcounts") === processedTrainHier
      );
      const hasExternalTest = matchedTestDs.length > 0;

      // 1. Pooled Expression Matrix (only if this group is pooled)
      if (processedTrain.isPooled) {
        groups.push({
          name: `Pooled Expression Matrix — ${processedTrain.name}`,
          badge: "Pooled Data",
          badgeColor: "hsl(214 55% 45%)",
          datasetId: processedTrain.id,
          files: [
            {
              id: `pooled_expression_matrix|${processedTrain.id}`,
              label: `Pooled Expression Matrix (Batch-Corrected) — ${processedTrain.dataType.toUpperCase()}`,
              ext: "CSV",
              desc: `ComBat/limma batch-corrected expression matrix from pooling all same-type ${processedTrain.dataType.toUpperCase()} datasets`,
            },
            {
              id: `pooled_clinical_data|${processedTrain.id}`,
              label: `Pooled Clinical Data — ${processedTrain.dataType.toUpperCase()}`,
              ext: "CSV",
              desc: `Clinical metadata (SampleID + Group) for the pooled batch-corrected ${processedTrain.dataType.toUpperCase()} dataset`,
            },
          ],
        });
      }

      // 2. Per-model Feature Selection results
      selectedModels.forEach((model) => {
        const modelLabel = getModelLabel(model);
        groups.push({
          name: `${modelLabel} Feature Selection — ${processedTrain.name}`,
          badge: `${modelLabel} FS`,
          badgeColor: "hsl(214 65% 52%)",
          datasetId: processedTrain.id,
          datasetLabel: processedTrain.name,
          modelId: model,
          modelLabel,
          files: [
            {
              id: `selected_features|${processedTrain.id}|${model}`,
              label: `Selected Features (${modelLabel}) — ${processedTrain.name}`,
              ext: "CSV",
              desc: `Selected features with importance scores for ${modelLabel} on ${processedTrain.name}`,
            },
            ...(model !== "boruta" && model !== "stabl"
              ? [
                  {
                    id: `feature_importance|${processedTrain.id}|${model}`,
                    label: `Feature Importance Scores (${modelLabel}) — ${processedTrain.name}`,
                    ext: "CSV",
                    desc: `All features ranked by importance for ${modelLabel} on ${processedTrain.name}`,
                  },
                ]
              : []),
          ],
        });
      });

      // 3. Training & Evaluation Cards
      if (hasExternalTest) {
        // Card 1: Training (Self)
        groups.push({
          name: `All-Models Performance — Training (Self) — ${processedTrain.name}`,
          badge: "Training (Self)",
          badgeColor: "hsl(214 55% 40%)",
          datasetId: processedTrain.id,
          datasetLabel: processedTrain.name,
          files: [
            {
              id: `all_models_performance_train|${processedTrain.id}`,
              label: `All-Models Performance — Training (Self) — ${processedTrain.name}`,
              ext: "CSV",
              desc: `Combined performance metrics for all models on training split of ${processedTrain.name}`,
            },
            {
              id: `roc_all_models_train|${processedTrain.id}`,
              label: `All-Models ROC — Training (Self) — ${processedTrain.name}`,
              ext: "PDF",
              desc: `Combined ROC plot for all models on training split of ${processedTrain.name}`,
            },
          ],
        });

        // Card 2: Evaluation (Self)
        if (cvEnabled) {
          groups.push({
            name: `All-Models Performance — Evaluation (Self) — ${processedTrain.name}`,
            badge: "Evaluation (Self)",
            badgeColor: "hsl(214 60% 35%)",
            datasetId: processedTrain.id,
            datasetLabel: processedTrain.name,
            files: [
              {
                id: `cv_results|${processedTrain.id}`,
                label: `All-Models Performance — Evaluation (Self) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `Cross-validation scores per model on ${processedTrain.name}`,
              },
              {
                id: `roc_cv|${processedTrain.id}`,
                label: `All-Models ROC — Evaluation (Self) — ${processedTrain.name}`,
                ext: "PDF",
                desc: `Combined ROC from cross-validation for all models on ${processedTrain.name}`,
              },
            ],
          });
        } else if (hasInternal) {
          groups.push({
            name: `All-Models Performance — Evaluation (Self) — ${processedTrain.name}`,
            badge: "Evaluation (Self)",
            badgeColor: "hsl(150 55% 35%)",
            datasetId: processedTrain.id,
            datasetLabel: processedTrain.name,
            files: [
              {
                id: `testing_results|${processedTrain.id}`,
                label: `All-Models Performance — Evaluation (Self) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `AUC, Accuracy, PPV, NPV on internal test split for all models on ${processedTrain.name}`,
              },
              {
                id: `roc_test_all|${processedTrain.id}`,
                label: `All-Models ROC — Evaluation (Self) — ${processedTrain.name}`,
                ext: "PDF",
                desc: `Combined ROC curves plot on internal test split of ${processedTrain.name}`,
              },
              {
                id: `confusion_matrix|${processedTrain.id}`,
                label: `Confusion Matrix — Evaluation (Self) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `Aggregated confusion matrix for all models on held-out internal test split of ${processedTrain.name}`,
              },
            ],
          });
        }

        // Card 3 & Card 4 for matched external test datasets
        matchedTestDs.forEach((t) => {
          // Card 3: Refit Training (Test)
          groups.push({
            name: `All-Models Performance — Refit Training (Test) — ${t.name}`,
            badge: "Refit Training",
            badgeColor: "hsl(280 50% 45%)",
            datasetId: t.id,
            datasetLabel: t.name,
            files: [
              {
                id: `all_models_performance_train|${t.id}`,
                label: `All-Models Performance — Refit Training (Test) — ${t.name}`,
                ext: "CSV",
                desc: `Refit model performance on ${t.name} using selected features from ${processedTrain.name}`,
              },
              {
                id: `roc_all_models_train|${t.id}`,
                label: `All-Models ROC — Refit Training (Test) — ${t.name}`,
                ext: "PDF",
                desc: `Combined ROC curves plot for refit models on ${t.name}`,
              },
            ],
          });

          // Card 4: Evaluation (Test)
          groups.push({
            name: `All-Models Performance — Evaluation (Test) — ${t.name}`,
            badge: "Evaluation (Test)",
            badgeColor: "hsl(25 70% 40%)",
            datasetId: t.id,
            datasetLabel: t.name,
            files: [
              {
                id: `testing_results|${t.id}`,
                label: `All-Models Performance — Evaluation (Test) — ${t.name}`,
                ext: "CSV",
                desc: `AUC, Balanced Accuracy, PPV, NPV on ${t.name} (features from ${processedTrain.name})`,
              },
              {
                id: `roc_test_all|${t.id}`,
                label: `All-Models ROC — Evaluation (Test) — ${t.name}`,
                ext: "PDF",
                desc: `Combined ROC on ${t.name} (features from ${processedTrain.name})`,
              },
              {
                id: `confusion_matrix|${t.id}`,
                label: `Confusion Matrix — Evaluation (Test) — ${t.name}`,
                ext: "CSV",
                desc: `Aggregated confusion matrix for all models on ${t.name}`,
              },
            ],
          });
        });

      } else {
        // Standard Mode: Training + Evaluation (CV) / Evaluation (Test)
        groups.push({
          name: `All-Models Performance — Training — ${processedTrain.name}`,
          badge: "Training",
          badgeColor: "hsl(214 55% 40%)",
          datasetId: processedTrain.id,
          datasetLabel: processedTrain.name,
          files: [
            {
              id: `all_models_performance_train|${processedTrain.id}`,
              label: `All-Models Performance — Training — ${processedTrain.name}`,
              ext: "CSV",
              desc: `Combined performance metrics for all models on training split of ${processedTrain.name}`,
            },
            {
              id: `roc_all_models_train|${processedTrain.id}`,
              label: `All-Models ROC — Training — ${processedTrain.name}`,
              ext: "PDF",
              desc: `Combined ROC plot for all models on training split of ${processedTrain.name}`,
            },
          ],
        });

        if (cvEnabled) {
          groups.push({
            name: `All-Models Performance — Evaluation (CV) — ${processedTrain.name}`,
            badge: "Evaluation (CV)",
            badgeColor: "hsl(214 60% 35%)",
            datasetId: processedTrain.id,
            datasetLabel: processedTrain.name,
            files: [
              {
                id: `cv_results|${processedTrain.id}`,
                label: `All-Models Performance — Evaluation (CV) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `Cross-validation scores per model on ${processedTrain.name}`,
              },
              {
                id: `roc_cv|${processedTrain.id}`,
                label: `All-Models ROC — Evaluation (CV) — ${processedTrain.name}`,
                ext: "PDF",
                desc: `Combined ROC from cross-validation for all models on ${processedTrain.name}`,
              },
            ],
          });
        }

        if (hasInternal) {
          groups.push({
            name: `All-Models Performance — Evaluation (Test) — ${processedTrain.name}`,
            badge: "Evaluation (Test)",
            badgeColor: "hsl(150 55% 35%)",
            datasetId: processedTrain.id,
            datasetLabel: processedTrain.name,
            files: [
              {
                id: `testing_results|${processedTrain.id}`,
                label: `All-Models Performance — Evaluation (Test) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `AUC, Balanced Accuracy, PPV, NPV on internal test split for all models on ${processedTrain.name}`,
              },
              {
                id: `roc_test_all|${processedTrain.id}`,
                label: `All-Models ROC — Evaluation (Test) — ${processedTrain.name}`,
                ext: "PDF",
                desc: `Combined ROC curves plot on internal test split of ${processedTrain.name}`,
              },
              {
                id: `confusion_matrix|${processedTrain.id}`,
                label: `Confusion Matrix — Evaluation (Test) — ${processedTrain.name}`,
                ext: "CSV",
                desc: `Aggregated confusion matrix for all models on held-out internal test split of ${processedTrain.name}`,
              },
            ],
          });
        }
      }
    });
  }

  if (state.dpDeMetaMethod) {
    const userId = getSessionUserId();
    const metaDsId = `${userId}_dp_meta`;
    const metaFiles = [
      {
        id: `meta_results|${metaDsId}`,
        label: "Meta-analysis Results Table",
        ext: "CSV",
        desc: "Aggregated differential expression statistics across all cohorts",
      },
      ...(state.dpDeMetaMethod === "effect_size"
        ? [
            {
              id: `forest_plots|${metaDsId}`,
              label: "Forest Plots",
              ext: "PDF",
              desc: "Forest plots showing effect sizes and confidence intervals across cohorts",
            },
            {
              id: `de_volcano|${metaDsId}`,
              label: "Meta-analysis Volcano Plot",
              ext: "PDF",
              desc: "Volcano plot mapping combined effect size (Cohen's d) against statistical significance (-log₁₀ FDR)",
            },
            {
              id: `heterogeneity_report|${metaDsId}`,
              label: "Heterogeneity Report",
              ext: "CSV",
              desc: "Cochran's Q test statistics and I² heterogeneity metrics results (Cochran's Q statistic evaluating null hypothesis of homogeneity, I² statistic quantifying percentage of total variation across studies due to heterogeneity rather than chance)",
            },
          ]
        : []),
    ];

    groups.push({
      name: "Meta-analysis Results",
      badge: "Meta-analysis",
      badgeColor: "hsl(38 70% 35%)",
      files: metaFiles,
    });
  }

  if (state.dpInlineEaDone) {
    const isMetaActive = state.dpDatasets.length > 1 && !state.dpDeMetaSkipped && !!state.dpDeMetaMethod;
    const userId = getSessionUserId();
    const metaDsId = `${userId}_dp_meta`;

    const processCache = (cacheKey: string, dsId: string, dsSuffix: string, dsName: string) => {
      const inlineCache = state.dpInlineEnResultsCache[cacheKey];
      if (!inlineCache) return;

      const submittedCfg = inlineCache.submittedCfg;
      const activeMethods = Array.isArray(submittedCfg?.methods) ? submittedCfg.methods : (Array.isArray(state.enConfig.methods) ? state.enConfig.methods : []);
      const isGsea = activeMethods.includes("gsea");
      const isOra = activeMethods.includes("ora");

      if (isGsea) {
        const dbs = Array.isArray(submittedCfg?.gseaDatabase) ? submittedCfg.gseaDatabase : (Array.isArray(state.enConfig.gseaDatabase) ? state.enConfig.gseaDatabase : []);
        dbs.forEach(db => {
          const dbLabel = db.toUpperCase();
          const hasSigGsea = Array.isArray(inlineCache?.gseaResults) && inlineCache.gseaResults.filter((r: any) => r && r.database === db).length > 0;
          const dbFiles: any[] = [
            {
              id: `gsea_results|${dsId}|${db}`,
              label: `${dbLabel} GSEA Results Table${dsSuffix}`,
              ext: "CSV",
              desc: `Significant gene sets with NES, p-value, and FDR q-value computed using ${dbLabel} database${dsSuffix}.`,
            },
          ];

          if (hasSigGsea) {
            dbFiles.push(
              {
                id: `gsea_dotplot|${dsId}|${db}`,
                label: `${dbLabel} GSEA Dot Plot${dsSuffix}`,
                ext: "PDF",
                desc: `Bubble visualization highlighting top enriched GSEA terms by NES using ${dbLabel}${dsSuffix}.`,
              },
              {
                id: `gsea_ridgeplot|${dsId}|${db}`,
                label: `${dbLabel} GSEA Ridge Plot${dsSuffix}`,
                ext: "PDF",
                desc: `Ridge plot showing expression distributions of core enrichment genes for top terms using ${dbLabel}${dsSuffix}.`,
              },
              {
                id: `gsea_esplot_up|${dsId}|${db}`,
                label: `${dbLabel} GSEA Enrichment Score Plot (Top Up-regulated)${dsSuffix}`,
                ext: "PDF",
                desc: `Running enrichment score profile for the top up-regulated pathway using ${dbLabel}${dsSuffix}.`,
              },
              {
                id: `gsea_esplot_down|${dsId}|${db}`,
                label: `${dbLabel} GSEA Enrichment Score Plot (Top Down-regulated)${dsSuffix}`,
                ext: "PDF",
                desc: `Running enrichment score profile for the top down-regulated pathway using ${dbLabel}${dsSuffix}.`,
              },
              {
                id: `gsea_ranked_list|${dsId}|${db}`,
                label: `${dbLabel} GSEA Ranked Gene List${dsSuffix}`,
                ext: "CSV",
                desc: `Full ranked gene list with calculated cross-study meta-metrics using ${dbLabel}${dsSuffix}.`,
              },
              {
                id: `gsea_leading_edge|${dsId}|${db}`,
                label: `${dbLabel} GSEA Leading Edge Genes${dsSuffix}`,
                ext: "CSV",
                desc: `Core-enrichment leading edge genes extracted for significant terms using ${dbLabel}${dsSuffix}.`,
              }
            );
          }

          groups.push({
            name: `${dbLabel} (GSEA)${dsSuffix}`,
            badge: "GSEA",
            badgeColor: "hsl(270 55% 40%)",
            datasetId: dsId,
            datasetLabel: dsName,
            files: dbFiles,
          });
        });
      }

      if (isOra) {
        const dbs = Array.isArray(submittedCfg?.oraDatabase) ? submittedCfg.oraDatabase : (Array.isArray(state.enConfig.oraDatabase) ? state.enConfig.oraDatabase : []);
        dbs.forEach(db => {
          const dbLabel = db.toUpperCase();
          const oraList = Array.isArray(inlineCache?.oraResults) ? inlineCache.oraResults.filter((r: any) => r && r.database === db) : [];
          const hasSigOraUp = oraList.some((r: any) => r && r.direction && r.direction.toLowerCase().includes("up"));
          const hasSigOraDown = oraList.some((r: any) => r && r.direction && r.direction.toLowerCase().includes("down"));
          const hasSigOraGeneric = oraList.length > 0 && !hasSigOraUp && !hasSigOraDown;

          const dbFiles: any[] = [
            {
              id: `ora_results|${dsId}|${db}`,
              label: `${dbLabel} ORA Results Table${dsSuffix}`,
              ext: "CSV",
              desc: `Gene sets with fold enrichment and hyper-geometric p-values using ${dbLabel} database${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_results-${db}-${fmt}`, `btn-download-ora_results-${db}-${fmt.toUpperCase()}`]
            },
          ];

          if (hasSigOraUp) {
            dbFiles.push({
              id: `ora_dotplot_up|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot - Up${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms for up-regulated genes using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot_up-${db}-${fmt}`, `btn-download-ora_dotplot_up-${db}-${fmt.toUpperCase()}`]
            });
          }

          if (hasSigOraDown) {
            dbFiles.push({
              id: `ora_dotplot_down|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot - Down${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms for down-regulated genes using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot_down-${db}-${fmt}`, `btn-download-ora_dotplot_down-${db}-${fmt.toUpperCase()}`]
            });
          }

          if (hasSigOraGeneric) {
            dbFiles.push({
              id: `ora_dotplot|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms by ratio using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot-${db}-${fmt}`, `btn-download-ora_dotplot-${db}-${fmt.toUpperCase()}`]
            });
          }

          groups.push({
            name: `${dbLabel} (ORA)${dsSuffix}`,
            badge: "ORA",
            badgeColor: "hsl(270 55% 40%)",
            datasetId: dsId,
            datasetLabel: dsName,
            files: dbFiles,
          });
        });
      }
    };

    if (isMetaActive) {
      const key = state.dpInlineEnResultsCache["proteomics"] ? "proteomics" : "transcriptomics";
      processCache(key, metaDsId, " (Meta-analysis)", "Meta-analysis");
    } else {
      (state.dpDatasets || []).forEach(d => {
        const cacheKey = (d.dataType === "readcounts" || d.dataType === "microarray") ? "transcriptomics" : "proteomics";
        processCache(cacheKey, d.id, ` (${d.name})`, d.name);
      });
    }
  }

  // Build stats chips
  const stats: StatChip[] = [
    { label: "Datasets", value: String(multipleDs ? state.dpDatasets.length : 1) },
    { label: "Total Samples", value: String(totalSamples) },
    { label: "Normalization", value: state.dpNormConfig.method.toUpperCase() },
  ];
  if (multipleDs) {
    stats.push({ label: "Batch Correction", value: state.dpBatchConfig.method.toUpperCase() });
  }
  if (state.dpInlineFsDone) {
    stats.push({ label: "FS Mode", value: allDpDs.length === 1 ? "Single" : (isCombinedPool ? "Pooled" : "Individual") });
    stats.push({ label: "Models", value: String(state.fsConfig.selectedModels?.length || 0) });
  }

  return (
    <SharedExportStep
      title="Export Summary"
      subtitle="Processing pipeline complete. Download your results below."
      stats={stats}
      groups={groups}
      onBack={() => dispatch({ type: "DP_SET_STEP", step: "module-select" })}
      onReset={() => dispatch({ type: "RESET_STORE", pipelineMode: true })}
      resetLabel="Start New Analysis"
      resetTestId="btn-new-analysis"
      onDownloadReport={handleDownloadReport}
      loadingReport={loadingReport}
      module="dp"
    />
  );
}
