import { useState } from "react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import SharedExportStep, { type ExportGroup, type StatChip } from "../shared/SharedExportStep";
import { getHierarchyGroup } from "../../dataObject";
import { useToast } from "../../hooks/use-toast";
import { downloadReportAPI } from "../../lib/api";

// Helper to extract feature ids from dataset
const getFsDatasetFeatureIds = (d: any): Set<string> => {
  if (d.dataMode === "metadata_features") {
    const sampleIdCol = d.clinicalSampleIdCol || d.geneIdCol || "SampleID";
    const infoCols = d.geneInfoCols || [];
    const featureCols = (d.columns || []).filter((c: string) => c !== sampleIdCol && !infoCols.includes(c));
    return new Set(featureCols);
  }
  if (!d.parsedData || d.parsedData.length === 0) return new Set();
  const geneIdIdx = d.columns ? d.columns.indexOf(d.geneIdCol) : -1;
  if (geneIdIdx === -1) return new Set(d.parsedData.map((row: any) => String(row[0])));
  return new Set(d.parsedData.map((row: any) => String(row[geneIdIdx])));
};

// Helper to compute intersection size
const computeOverlapCount = (setA: Set<string>, setB: Set<string>): number => {
  let intersection = new Set();
  for (let elem of setB) {
    if (setA.has(elem)) {
      intersection.add(elem);
    }
  }
  return intersection.size;
};

// Helper to compute intersection of features in a list of datasets
const getSharedFeatureCountForGroup = (groupDatasets: any[]): number => {
  if (groupDatasets.length <= 1) return 0;
  if (groupDatasets.some(d => !d.parsedData || d.parsedData.length === 0)) return 0;
  let intersection = getFsDatasetFeatureIds(groupDatasets[0]);
  for (let i = 1; i < groupDatasets.length; i++) {
    const next = getFsDatasetFeatureIds(groupDatasets[i]);
    intersection = new Set([...intersection].filter(x => next.has(x)));
  }
  return intersection.size;
};

export default function FSExportStep() {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const [loadingReport, setLoadingReport] = useState(false);

  const handleDownloadReport = async (format: "md" | "pdf") => {
    setLoadingReport(true);
    try {
      const userId = getSessionUserId();
      const { blob, filename } = await downloadReportAPI(userId, "fs", format);
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

  const groups: ExportGroup[] = [];
  const selectedModels = Array.isArray(state.fsConfig.selectedModels) ? state.fsConfig.selectedModels : [];
  const allDs = Array.isArray(state.fsDatasets) ? state.fsDatasets : [];
  const cvEnabled = state.fsConfig.cvEnabled;

  // ── Determine pooling mode ─────────────────────────────────────────────────
  const isSingleDataset = allDs.length === 1;
  const isAllSameType = allDs.length > 1 &&
    new Set(allDs.map(d => d?.dataType || "readcounts")).size === 1 &&
    (allDs[0]?.dataType || "readcounts") !== "others";

  // Group datasets by exact type to check pooling eligibility
  const typeGroups: Record<string, any[]> = {};
  allDs.forEach(d => {
    if (!d) return;
    const type = d.dataType || "readcounts";
    if (type !== "others") {
      if (!typeGroups[type]) typeGroups[type] = [];
      typeGroups[type].push(d);
    }
  });

  // Pooling selection card is visible only when we have same-type datasets with >= 10 shared features
  const hasPoolableTypesAndSufficientFeatures = Object.values(typeGroups).some(group => {
    if (!Array.isArray(group) || group.length < 2) return false;
    const count = getSharedFeatureCountForGroup(group);
    return count >= 10;
  });

  const isCombinedPool = isSingleDataset || (
    isAllSameType &&
    (state.fsConfig.multiDatasetMode ?? "combine") === "combine" &&
    hasPoolableTypesAndSufficientFeatures
  );

  const trainingDs = (allDs || []).filter(
    d => d && (d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" ||
         d.fs_datasetPurpose === "train-and-cv" || d.fs_datasetPurpose === "train-cv-test" || !d.fs_datasetPurpose)
  );
  const testingDs = (allDs || []).filter(d => d && d.fs_datasetPurpose === "test");
  const hasInternalTest = trainingDs.some(d =>
    d && (d.fs_datasetPurpose === "train-and-test" ||
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation))
  );
  // Exclude "test" datasets with cv-only strategy — they are evaluated in CV, not a separate testing step
  const hasAnyTesting = testingDs.some(
    d => d && d.fs_validationStrategy !== "cv-only" && d.fs_validationStrategy !== "train-and-cv"
  ) || hasInternalTest;

  // Unify pooling groups
  const processedTrainDss = (() => {
    if (!isCombinedPool || trainingDs.length <= 1) {
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

  // ── Populate Export Groups ────────────────────────────────────────────────
  processedTrainDss.forEach((processedTrain) => {
    const hasInternal = (processedTrain.originalDatasets || []).some(d =>
      d && (d.fs_datasetPurpose === "train-and-test" ||
      d.fs_datasetPurpose === "train-cv-test" ||
      ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation))
    );

    const processedTrainHier = getHierarchyGroup(processedTrain.dataType);
    const matchedTestDs = testingDs.filter(
      t => t && getHierarchyGroup(t.dataType || "readcounts") === processedTrainHier
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
            ext: "csv",
            desc: `ComBat/limma batch-corrected expression matrix from pooling all same-type ${processedTrain.dataType.toUpperCase()} datasets`,
          },
          {
            id: `pooled_clinical_data|${processedTrain.id}`,
            label: `Pooled Clinical Data — ${processedTrain.dataType.toUpperCase()}`,
            ext: "csv",
            desc: `Clinical metadata (SampleID + Group) for the pooled batch-corrected ${processedTrain.dataType.toUpperCase()} dataset`,
          },
        ],
      });
    }

    // 2. Per-model Feature Selection results (selected features & feature importance only, per-model ROC is ZIP only)
    selectedModels.forEach((model) => {
      const modelLabel = getModelLabel(model);
      groups.push({
        name: `${modelLabel} Feature Selection — ${processedTrain.name}`,
        badge: modelLabel,
        badgeColor: "hsl(214 65% 52%)",
        datasetId: processedTrain.id,
        datasetLabel: processedTrain.name,
        modelId: model,
        modelLabel,
        files: [
          {
            id: `selected_features|${processedTrain.id}|${model}`,
            label: `Selected Features (${modelLabel}) — ${processedTrain.name}`,
            ext: "csv",
            desc: `Selected features with importance scores for ${modelLabel} on ${processedTrain.name}`,
          },
          ...(model !== "boruta" && model !== "stabl"
            ? [
                {
                  id: `feature_importance|${processedTrain.id}|${model}`,
                  label: `Feature Importance Scores (${modelLabel}) — ${processedTrain.name}`,
                  ext: "csv",
                  desc: `All features ranked by importance for ${modelLabel} on ${processedTrain.name}`,
                },
              ]
            : []),
        ],
      });
    });

    // 3. Training & Evaluation Cards
    if (hasExternalTest) {
      // ── CASE C: 4 Cards ──────────────────────────────────────────────────
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
            ext: "csv",
            desc: `Combined performance metrics for all models on training split of ${processedTrain.name}`,
          },
          {
            id: `roc_all_models_train|${processedTrain.id}`,
            label: `All-Models ROC — Training (Self) — ${processedTrain.name}`,
            ext: "pdf",
            desc: `Combined ROC plot for all models on training split of ${processedTrain.name}`,
          },
        ],
      });

      // Card 2: Evaluation (Self) — CV or Internal Test on Training Dataset
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
              ext: "csv",
              desc: `Cross-validation scores per model on ${processedTrain.name}`,
            },
            {
              id: `roc_cv|${processedTrain.id}`,
              label: `All-Models ROC — Evaluation (Self) — ${processedTrain.name}`,
              ext: "pdf",
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
              ext: "csv",
              desc: `AUC, Accuracy, PPV, NPV on internal test split for all models on ${processedTrain.name}`,
            },
            {
              id: `roc_test_all|${processedTrain.id}`,
              label: `All-Models ROC — Evaluation (Self) — ${processedTrain.name}`,
              ext: "pdf",
              desc: `Combined ROC curves plot on internal test split of ${processedTrain.name}`,
            },
            {
              id: `confusion_matrix|${processedTrain.id}`,
              label: `Confusion Matrix — Evaluation (Self) — ${processedTrain.name}`,
              ext: "csv",
              desc: `Aggregated confusion matrix for all models on held-out internal test split of ${processedTrain.name}`,
            },
          ],
        });
      }

      // Card 3 & Card 4 for each external test dataset
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
              ext: "csv",
              desc: `Refit model performance on ${t.name} using selected features from ${processedTrain.name}`,
            },
            {
              id: `roc_all_models_train|${t.id}`,
              label: `All-Models ROC — Refit Training (Test) — ${t.name}`,
              ext: "pdf",
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
              ext: "csv",
              desc: `AUC, Balanced Accuracy, PPV, NPV on ${t.name} (features from ${processedTrain.name})`,
            },
            {
              id: `roc_test_all|${t.id}`,
              label: `All-Models ROC — Evaluation (Test) — ${t.name}`,
              ext: "pdf",
              desc: `Combined ROC on ${t.name} (features from ${processedTrain.name})`,
            },
            {
              id: `confusion_matrix|${t.id}`,
              label: `Confusion Matrix — Evaluation (Test) — ${t.name}`,
              ext: "csv",
              desc: `Aggregated confusion matrix for all models on ${t.name}`,
            },
          ],
        });
      });

    } else {
      // ── Standard Mode (Single dataset / Internal Test / CV only) ─────────
      // Card 1: Training
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
            ext: "csv",
            desc: `Combined performance metrics for all models on training split of ${processedTrain.name}`,
          },
          {
            id: `roc_all_models_train|${processedTrain.id}`,
            label: `All-Models ROC — Training — ${processedTrain.name}`,
            ext: "pdf",
            desc: `Combined ROC plot for all models on training split of ${processedTrain.name}`,
          },
        ],
      });

      // Card 2: Evaluation (CV)
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
              ext: "csv",
              desc: `Cross-validation scores per model on ${processedTrain.name}`,
            },
            {
              id: `roc_cv|${processedTrain.id}`,
              label: `All-Models ROC — Evaluation (CV) — ${processedTrain.name}`,
              ext: "pdf",
              desc: `Combined ROC from cross-validation for all models on ${processedTrain.name}`,
            },
          ],
        });
      }

      // Card 3: Evaluation (Test)
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
              ext: "csv",
              desc: `AUC, Balanced Accuracy, PPV, NPV on internal test split for all models on ${processedTrain.name}`,
            },
            {
              id: `roc_test_all|${processedTrain.id}`,
              label: `All-Models ROC — Evaluation (Test) — ${processedTrain.name}`,
              ext: "pdf",
              desc: `Combined ROC curves plot on internal test split of ${processedTrain.name}`,
            },
            {
              id: `confusion_matrix|${processedTrain.id}`,
              label: `Confusion Matrix — Evaluation (Test) — ${processedTrain.name}`,
              ext: "csv",
              desc: `Aggregated confusion matrix for all models on held-out internal test split of ${processedTrain.name}`,
            },
          ],
        });
      }
    }
  });

  // ── Stats chips ────────────────────────────────────────────────────────────
  const exportStats: StatChip[] = [
    { label: "Datasets", value: String(allDs.length) },
    { label: "FS Mode", value: isCombinedPool ? (isSingleDataset ? "Single" : "Pooled") : "Individual" },
    { label: "Models", value: String(selectedModels.length) },
    { label: "CV", value: cvEnabled ? `${state.fsConfig.cvFolds === -1 ? "LOOCV" : `${state.fsConfig.cvFolds}-fold`}` : "Off" },
  ];

  return (
    <SharedExportStep
      title="Feature Selection Complete"
      subtitle={`${selectedModels.length} model(s) · ${cvEnabled ? `${state.fsConfig.cvFolds === -1 ? "LOOCV" : `${state.fsConfig.cvFolds}-fold CV`}` : "No CV"} · ${isCombinedPool ? (isSingleDataset ? "Single dataset" : "Pooled datasets") : "Individual datasets"}`}
      stats={exportStats}
      groups={groups}
      onBack={() => {
        if (hasAnyTesting) {
          dispatch({ type: "FS_SET_STEP", step: "testing" });
        } else if (cvEnabled) {
          dispatch({ type: "FS_SET_STEP", step: "cross-validation" });
        } else {
          dispatch({ type: "FS_SET_STEP", step: "model-selection" });
        }
      }}
      onReset={() => dispatch({ type: "FS_SET_STEP", step: "upload" })}
      resetLabel="Start New Analysis"
      onDownloadReport={handleDownloadReport}
      loadingReport={loadingReport}
      module="fs"
    />
  );
}
