import { useState, useEffect, useMemo } from "react";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import { runTestingAPI, type TestingResult } from "../../lib/api";
import { formatMetric, formatCi } from "../../lib/utils";
import { toast } from "../../hooks/use-toast";

interface Props {
  mode?: "dp" | "fs";
  onBack?: () => void;
  onContinue?: () => void;
}

const MODEL_COLORS: Record<string, string> = {
  logistic: "#2563eb",
  svm: "#16a34a",
  randomforest: "#ea580c",
  gbm: "#7c3aed",
  stabl: "#8b5cf6",
  boruta: "#059669",
};

const MODEL_LABELS: Record<string, string> = {
  logistic: "Logistic Regression",
  svm: "Support Vector Machine",
  randomforest: "Random Forest",
  gbm: "Gradient Boosting Model",
  stabl: "STABL (Stability Selection)",
  boruta: "Boruta (All-Relevant Selection)",
};

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

export default function TestingStep({ mode = "fs", onBack, onContinue }: Props) {
  const { state, dispatch } = useAppStore();
  const datasets = mode === "dp" ? state.dpDatasets : state.fsDatasets;
  const fsConfig = state.fsConfig;
  const selectedModels = fsConfig.selectedModels;

  // Fetch evaluation metrics from API on mount
  const [loading, setLoading] = useState(true);
  const [evalMetrics, setEvalMetrics] = useState<Record<string, Record<string, TestingResult>>>({});

  const isSingleDataset = datasets.length === 1;
  const isAllSameType = datasets.length > 1 && 
    new Set(datasets.map(d => d.dataType || "readcounts")).size === 1 && 
    (datasets[0]?.dataType || "readcounts") !== "others";

  // Group datasets by exact type to check pooling eligibility
  const typeGroups: Record<string, any[]> = {};
  datasets.forEach(d => {
    const type = d.dataType || "readcounts";
    if (type !== "others") {
      if (!typeGroups[type]) typeGroups[type] = [];
      typeGroups[type].push(d);
    }
  });

  // Pooling selection card is visible only when we have same-type datasets with >= 10 shared features
  const hasPoolableTypesAndSufficientFeatures = Object.values(typeGroups).some(group => {
    if (group.length < 2) return false;
    const count = getSharedFeatureCountForGroup(group);
    return count >= 10;
  });

  const isCombinedPool = isSingleDataset || (
    isAllSameType && 
    (fsConfig.multiDatasetMode ?? "combine") === "combine" && 
    hasPoolableTypesAndSufficientFeatures
  );

  // Filter datasets that are used for testing
  const evalDatasets = useMemo(() => {
    const trainingDs = datasets.filter(d => {
      if (isSingleDataset) return true;
      const isTrain = d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || d.fs_datasetPurpose === "train-and-cv" || !d.fs_datasetPurpose;
      return isTrain && (d.fs_isInternalValidation || d.fs_datasetPurpose === "train-and-test" || (!d.fs_datasetPurpose && (d.fs_trainRatio ?? 0.7) < 1));
    });

    const testingDs = datasets.filter(d => {
      if (d.fs_datasetPurpose === "test") {
        return d.fs_validationStrategy !== "cv-only" && d.fs_validationStrategy !== "train-and-cv";
      }
      return false;
    });

    let effectiveTrainDs = trainingDs;
    if (effectiveTrainDs.length === 0 && testingDs.length === 0 && datasets.length > 0) {
      effectiveTrainDs = datasets;
    }

    const processedTrainList: { id: string; name: string; isPooled: boolean; color?: string; fs_datasetPurpose?: string; fs_isInternalValidation?: boolean }[] = [];

    if (!isCombinedPool || effectiveTrainDs.length <= 1) {
      effectiveTrainDs.forEach(d => {
        processedTrainList.push({
          id: d.id,
          name: d.name,
          isPooled: false,
          color: d.color,
          fs_datasetPurpose: d.fs_datasetPurpose,
          fs_isInternalValidation: d.fs_isInternalValidation
        });
      });
    } else {
      const typeGroups: Record<string, typeof effectiveTrainDs> = {};
      effectiveTrainDs.forEach(d => {
        const dt = d.dataType || "readcounts";
        if (!typeGroups[dt]) typeGroups[dt] = [];
        typeGroups[dt].push(d);
      });

      Object.keys(typeGroups).forEach(dt => {
        const group = typeGroups[dt];
        if (dt !== "others" && group.length > 1) {
          const firstDsId = group[0].id;
          const userId = firstDsId.match(/usr_[a-zA-Z0-9]+/)?.[0] || "";
          const mergedId = userId ? `${userId}_merged_${dt}_training_fs` : `merged_${dt}_training_fs`;
          processedTrainList.push({
            id: mergedId,
            name: `Pooled ${dt.toUpperCase()} Training Datasets`,
            isPooled: true,
            color: group[0].color,
            fs_datasetPurpose: "train",
            fs_isInternalValidation: true
          });
        } else {
          group.forEach(d => {
            processedTrainList.push({
              id: d.id,
              name: d.name,
              isPooled: false,
              color: d.color,
              fs_datasetPurpose: d.fs_datasetPurpose,
              fs_isInternalValidation: d.fs_isInternalValidation
            });
          });
        }
      });
    }

    return [
      ...processedTrainList,
      ...testingDs.map(d => ({
        id: d.id,
        name: d.name,
        isPooled: false,
        color: d.color,
        fs_datasetPurpose: d.fs_datasetPurpose,
        fs_isInternalValidation: d.fs_isInternalValidation
      }))
    ];
  }, [datasets, fsConfig.multiDatasetMode, isSingleDataset, isCombinedPool]);

  useEffect(() => {
    let active = true;
    
    const cache = mode === "fs" ? state.fsTestingResultsCache : state.dpTestingResultsCache;
    if (cache && cache.selectedModels && JSON.stringify(cache.selectedModels) === JSON.stringify(selectedModels)) {
      setEvalMetrics(cache.metrics);
      setLoading(false);
      return;
    }

    const runEval = async () => {
      setLoading(true);
      try {
        const metrics = await runTestingAPI(selectedModels, datasets, isCombinedPool ? "combine" : "individual");
        if (active) {
          setEvalMetrics(metrics);
          if (mode === "fs") {
            dispatch({ type: "FS_SET_TESTING_CACHE", results: { metrics, selectedModels } });
          } else {
            dispatch({ type: "DP_SET_TESTING_CACHE", results: { metrics, selectedModels } });
          }
        }
      } catch (err: any) {
        console.error("Evaluation error:", err);
        if (active) {
          toast({
            title: "Model Evaluation Failed",
            description: err.message || "Failed to evaluate models on test data.",
            variant: "destructive"
          });
        }
      } finally {
        if (active) setLoading(false);
      }
    };
    runEval();
    return () => { active = false; };
  }, [selectedModels, datasets, mode, state.fsTestingResultsCache, state.dpTestingResultsCache, isCombinedPool]);

  const getMetricsForDataset = (dsId: string) => {
    if (!evalMetrics) return null;
    if (evalMetrics[dsId]) return evalMetrics[dsId];
    
    // Check for merged training dataset key
    const mergedKey = Object.keys(evalMetrics).find(k => k.includes("merged_") && k.includes("_training"));
    if (mergedKey && evalMetrics[mergedKey]) return evalMetrics[mergedKey];

    // Fuzzy matching fallback for dataset ID variations
    const cleanId = (id: string) => {
      let prev;
      let current = id;
      do {
        prev = current;
        current = current.replace(/_(upload|parsed|raw|dp|fs|de|en|ea)$/g, "");
      } while (current !== prev);
      return current;
    };
    const targetClean = cleanId(dsId);
    const matchedKey = Object.keys(evalMetrics).find(k => {
      const kClean = cleanId(k);
      return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
    });
    if (matchedKey && evalMetrics[matchedKey]) return evalMetrics[matchedKey];

    // Fallback: if single dataset in evalMetrics, return it
    const keys = Object.keys(evalMetrics);
    if (keys.length === 1 && evalMetrics[keys[0]]) {
      return evalMetrics[keys[0]];
    }

    return null;
  };

  const getMetrics = (modelId: string, dsId: string) => {
    const dsMetrics = getMetricsForDataset(dsId);
    if (dsMetrics?.[modelId]) {
      return dsMetrics[modelId];
    }
    return {
      auc: 0,
      acc: 0,
      ppv: 0,
      npv: 0,
      fpr: [],
      tpr: []
    };
  };

  const getTestRocPlotForDataset = (dsId: string): string | null => {
    const dsMetrics = getMetricsForDataset(dsId);
    if (dsMetrics && typeof dsMetrics.roc_plot === "string" && dsMetrics.roc_plot.length > 0) {
      return dsMetrics.roc_plot;
    }
    return null;
  };

  const handleBackClick = () => {
    if (onBack) {
      onBack();
      return;
    }
    const step = fsConfig.cvEnabled ? "cross-validation" : "model-selection";
    dispatch({
      type: mode === "dp" ? "DP_SET_STEP" : "FS_SET_STEP",
      step: step as never
    });
  };

  const handleContinueClick = () => {
    if (onContinue) {
      onContinue();
      return;
    }
    dispatch({ type: "FS_SET_STEP", step: "export" });
  };

  return (
    <>
      {loading && (
        <Spinner
          label="Evaluating models on test data…"
          sublabel={`Testing ${selectedModels.length} model(s) — estimated ~${Math.max(3, selectedModels.length * 2)}s`}
        />
      )}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 4 }}>Model Testing & Evaluation</div>
        <div className="card-sub">Evaluate selected models on internal validation splits and external test datasets.</div>
      </div>

      <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
        {evalDatasets.map(d => {
          const isTrain = d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || d.fs_datasetPurpose === "train-and-cv" || !d.fs_datasetPurpose;
          const displayName = isCombinedPool && !isSingleDataset && isTrain ? "Pooled Datasets" : d.name;
          const badgeText = isSingleDataset
            ? "Training & Testing"
            : (isTrain ? "Internal Validation" : "External Test");
          const badgeBg = isSingleDataset || isTrain ? "hsl(150 50% 95%)" : "hsl(270 50% 95%)";
          const badgeColor = isSingleDataset || isTrain ? "hsl(150 65% 28%)" : "hsl(270 65% 35%)";
          const badgeBorder = isSingleDataset || isTrain ? "hsl(150 45% 78%)" : "hsl(270 45% 78%)";
          const rocPlot = getTestRocPlotForDataset(d.id);

          return (
            <div key={d.id} className="card" style={{ marginBottom: 14 }}>
              <div className="card-title" style={{ marginBottom: 10, display: "flex", alignItems: "center", justifyContent: "space-between" }}>
                <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <div style={{ width: 10, height: 10, borderRadius: "50%", background: d.color || "var(--primary)", flexShrink: 0 }} />
                  <span style={{ fontWeight: 700, fontSize: 13, color: "hsl(220 25% 12%)" }}>
                    Model Testing Results — {displayName}
                  </span>
                </div>
                <span style={{
                  fontSize: 10, fontWeight: 700, padding: "3px 8px", borderRadius: 4,
                  background: badgeBg, color: badgeColor, border: `1px solid ${badgeBorder}`
                }}>
                  {badgeText}
                </span>
              </div>

              <div className="banner success" style={{ marginBottom: 12 }}>✅ Model evaluation complete.</div>

              {rocPlot ? (
                <div style={{ display: "flex", justifyContent: "center", marginBottom: 14 }}>
                  <img src={`data:image/png;base64,${rocPlot}`} style={{ width: 600, height: 400, objectFit: "contain" }} alt="ROC Curves" />
                </div>
              ) : (
                <div style={{ height: 160, display: "flex", alignItems: "center", justifyContent: "center", border: "1px dashed var(--border)", borderRadius: 6, background: "var(--background)", marginBottom: 14 }}>
                  <div style={{ color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>No ROC curve available for this evaluation split.</div>
                </div>
              )}

              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12, marginBottom: 14 }}>
                <thead>
                  <tr style={{ background: "hsl(214 50% 96%)" }}>
                    <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Model</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Test Score (AUC)</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)", minWidth: 120 }}>95% CI</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Balanced Accuracy</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>PPV</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>NPV</th>
                  </tr>
                </thead>
                <tbody>
                  {selectedModels.map((m, i) => {
                    const metrics = getMetrics(m, d.id);
                    return (
                      <tr key={m} style={{ borderBottom: "1px solid hsl(214 20% 92%)", background: i % 2 === 0 ? "white" : "hsl(214 50% 98%)" }}>
                        <td style={{ padding: "8px 10px", textTransform: "capitalize", fontWeight: 500 }}>
                          <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
                            <span>{MODEL_LABELS[m] || m}</span>
                          </div>
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 700, color: "hsl(150 55% 30%)" }}>{formatMetric(metrics?.auc, 0, 3)}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 48%)", whiteSpace: "nowrap" }}>{formatCi(metrics?.ci, metrics?.ci_lower, metrics?.ci_upper)}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{formatMetric(metrics?.acc, 0, 3)}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{formatMetric(metrics?.ppv, 0, 3)}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{formatMetric(metrics?.npv, 0, 3)}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>

              {/* Performance metrics explanation card */}
              <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                <div style={{
                  padding: "10px 12px",
                  background: "hsl(214 50% 98%)", border: "1px solid hsl(214 20% 89%)",
                  borderRadius: 8, fontSize: 11, color: "var(--muted-foreground)",
                  lineHeight: "1.4"
                }}>
                  <div style={{ fontWeight: 600, fontSize: 13, color: "var(--foreground)", marginBottom: 6 }}>
                    Classification Statistics Reference Guide
                  </div>
                  <div style={{ display: "flex", flexDirection: "column", gap: 5 }}>
                    <div>
                      <strong>AUC (Area Under ROC Curve):</strong> Measures the classifier's overall discriminative ability (0.5 is random guessing, 1.0 is perfect classification)
                    </div>
                    <div>
                      <strong>Balanced Accuracy:</strong> The average recall across each class
                    </div>
                    <div>
                      <strong>PPV (Positive Predictive Value):</strong> TP / (TP + FP). The proportion of predicted positive cases that are true positives (empirical or prevalence-adjusted). Abbreviations: TP: True Positive; FP: False Positive.
                    </div>
                    <div>
                      <strong>NPV (Negative Predictive Value):</strong> TN / (TN + FN). The proportion of predicted negative cases that are true negatives (empirical or prevalence-adjusted). Abbreviations: TN: True Negative; FN: False Negative.
                    </div>
                  </div>
                </div>

                {/* AUC Interpretability Guide card */}
                <div style={{
                  padding: "10px 12px",
                  background: "hsl(214 50% 98%)", border: "1px solid hsl(214 20% 89%)",
                  borderRadius: 8, fontSize: 11, color: "var(--muted-foreground)",
                  lineHeight: "1.4"
                }}>
                  <div style={{ fontWeight: 600, fontSize: 13, color: "var(--foreground)", marginBottom: 6 }}>
                    AUC Interpretability Guide
                  </div>
                  <div style={{ display: "flex", flexDirection: "column", gap: 5 }}>
                    <div>
                      <strong>0.90 - 1.00:</strong> Excellent classification performance
                    </div>
                    <div>
                      <strong>0.80 - 0.90:</strong> Very good classification performance
                    </div>
                    <div>
                      <strong>0.70 - 0.80:</strong> Good classification performance
                    </div>
                    <div>
                      <strong>0.60 - 0.70:</strong> Acceptable classification performance
                    </div>
                    <div>
                      <strong>0.50:</strong> No classification performance (equivalent to random guessing)
                    </div>
                  </div>
                </div>
              </div>
            </div>
          );
        })}
      </div>

      <div className="action-row">
        <button className="btn btn-default" onClick={handleBackClick}>← Back</button>
        <button className="btn btn-primary" onClick={handleContinueClick}>Continue →</button>
      </div>
    </>
  );
}
