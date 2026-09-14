import { useState, useEffect, useMemo } from "react";
import { useLocation } from "wouter";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import { type CVMethod, getHierarchyGroup } from "../../dataObject";
import { runCrossValidationAPI, redoStepAPI, type CVResult } from "../../lib/api";
import { formatCi } from "../../lib/utils";
import { toast } from "../../hooks/use-toast";

const CV_METHODS: { id: CVMethod; label: string; desc: string }[] = [
  {
    id: "k_fold", label: "K-Fold Cross Validation",
    desc: "Splits the dataset into k folds (5 or 10 folds) to evaluate model performance across different subsets. Each fold is used as a validation set once while the remaining folds are used for training, providing a robust performance estimate."
  },
  {
    id: "loocv", label: "Leave-One-Out CV (LOOCV)",
    desc: "Trains the model N times, leaving one sample out each time. Recommended for very small datasets."
  }
];



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

export default function CrossValidationStep() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const fsConfig = state.fsConfig;
  const [loading, setLoading] = useState(false);
  const [done, setDone] = useState(state.fsCvResultsCache ? true : false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  // Stable CV results — only updated when Run is clicked
  const [cvResults, setCvResults] = useState<CVResult[]>(state.fsCvResultsCache || []);

  const isSingleDataset = state.fsDatasets.length === 1;
  const isAllSameType = state.fsDatasets.length > 1 && 
    new Set(state.fsDatasets.map(d => d.dataType || "readcounts")).size === 1 && 
    (state.fsDatasets[0]?.dataType || "readcounts") !== "others";

  // Group datasets by exact type to check pooling eligibility
  const typeGroups: Record<string, any[]> = {};
  state.fsDatasets.forEach(d => {
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
    (state.fsConfig.multiDatasetMode ?? "combine") === "combine" && 
    hasPoolableTypesAndSufficientFeatures
  );

  const cvTrackSizes = useMemo(() => {
    const sizes: number[] = [];
    const trainingDs = state.fsDatasets.filter(d => 
      d.fs_datasetPurpose === "train" || 
      d.fs_datasetPurpose === "train-and-test" || 
      d.fs_datasetPurpose === "train-and-cv" || 
      !d.fs_datasetPurpose
    );
    const testingDsWithCv = state.fsDatasets.filter(d => 
      d.fs_datasetPurpose === "test" && 
      (d.fs_validationStrategy === "cv-only" || d.fs_validationStrategy === "cv-and-test")
    );

    if (isCombinedPool) {
      if (trainingDs.length > 0) {
        const totalSamples = trainingDs.reduce((sum, d) => sum + (d.nSamples || 0), 0);
        const firstPurpose = trainingDs[0]?.fs_datasetPurpose || "train-and-test";
        const ratio = trainingDs[0]?.fs_trainRatio ?? 0.7;
        const cvSize = (firstPurpose === "train-and-cv") ? totalSamples : Math.round(totalSamples * ratio);
        sizes.push(cvSize);
      }
    } else {
      trainingDs.forEach(d => {
        const ratio = d.fs_trainRatio ?? 0.7;
        const cvSize = (d.fs_datasetPurpose === "train-and-cv") ? d.nSamples : Math.round(d.nSamples * ratio);
        sizes.push(cvSize);
      });
    }

    testingDsWithCv.forEach(d => {
      const ratio = d.fs_trainRatio ?? 0.7;
      const cvSize = (d.fs_validationStrategy === "cv-only") ? d.nSamples : Math.round(d.nSamples * ratio);
      sizes.push(cvSize);
    });

    return sizes;
  }, [state.fsDatasets, isCombinedPool]);

  const trainSampleSize = useMemo(() => {
    if (cvTrackSizes.length === 0) return 0;
    return Math.min(...cvTrackSizes);
  }, [cvTrackSizes]);

  const availableFolds = useMemo(() => {
    if (trainSampleSize < 80) return [];
    if (trainSampleSize <= 150) return [5];
    return [5, 10];
  }, [trainSampleSize]);

  const availableMethods = useMemo(() => {
    return CV_METHODS.filter(m => {
      if (trainSampleSize < 80) {
        return m.id === "loocv";
      } else {
        return m.id === "k_fold";
      }
    });
  }, [trainSampleSize]);

  useEffect(() => {
    if (trainSampleSize < 80) {
      if (fsConfig.cvMethod !== "loocv") {
        dispatch({ type: "FS_SET_CONFIG", patch: { cvMethod: "loocv" } });
      }
      if (fsConfig.cvFolds !== -1) {
        dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: -1 } });
      }
    } else {
      if (fsConfig.cvMethod !== "k_fold") {
        dispatch({ type: "FS_SET_CONFIG", patch: { cvMethod: "k_fold" } });
      }
      if (trainSampleSize <= 150) {
        if (fsConfig.cvFolds !== 5) {
          dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: 5 } });
        }
      } else {
        if (fsConfig.cvFolds !== 5 && fsConfig.cvFolds !== 10) {
          dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: 5 } });
        }
      }
    }
  }, [trainSampleSize, fsConfig.cvMethod, fsConfig.cvFolds, dispatch]);

  const [submittedCfg, setSubmittedCfg] = useState<{
    cvMethod: CVMethod;
    cvFolds: number;
  } | null>(
    state.standaloneFsDone ? {
      cvMethod: fsConfig.cvMethod,
      cvFolds: fsConfig.cvFolds,
    } : null
  );



  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.cvMethod !== fsConfig.cvMethod ||
    submittedCfg.cvFolds !== fsConfig.cvFolds
  );

  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const downstreamDone = !!state.fsTestingResultsCache;
  const completedModules = ["Model Evaluation (Testing)"];

  const handleRedo = () => {
    if (downstreamDone) {
      setShowDiscardModal(true);
    } else {
      runRedoCV();
    }
  };

  const runRedoCV = async () => {
    setLoading(true);
    dispatch({ type: "FS_SET_CV_CACHE", results: null });
    dispatch({
      type: "RESET_DOWNSTREAM_STEPS",
      datasetId: state.fsCurrentDatasetId,
      fromStep: "cross-validation"
    } as any);
    try {
      await redoStepAPI("cv", state.fsDatasets.map(d => d.id));
    } catch (e) {
      console.error(e);
    }
    await triggerRunCV();
  };

  const handleRun = async () => {
    setLoading(true);
    await triggerRunCV();
  };

  const triggerRunCV = async () => {
    try {
      const results = await runCrossValidationAPI(
        fsConfig.selectedModels,
        fsConfig.cvMethod,
        fsConfig.cvFolds,
        state.fsDatasets
      );

      setSubmittedCfg({
        cvMethod: fsConfig.cvMethod,
        cvFolds: fsConfig.cvFolds,
      });
      setCvResults(results);
      dispatch({ type: "FS_SET_CV_CACHE", results });
      dispatch({ type: "FS_SUBMIT_CONFIG" });
      dispatch({ type: "SET_STANDALONE_FS_DONE", done: true });
      setDone(true);
    } catch (err: any) {
      console.error("Cross validation error:", err);
      toast({
        title: "Cross-Validation Failed",
        description: err.message || "Failed to perform cross-validation.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const hasAnyTesting = state.fsDatasets.some(d => 
    (d.fs_datasetPurpose === "test" &&
     d.fs_validationStrategy !== "cv-only" &&
     d.fs_validationStrategy !== "train-and-cv") || 
    d.fs_datasetPurpose === "train-and-test" || 
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
  );

  const proceedToNextStep = () => {
    if (hasAnyTesting) {
      dispatch({ type: "FS_SET_STEP", step: "testing" });
    } else {
      dispatch({ type: "FS_SET_STEP", step: "export" });
    }
  };

  const handleContinue = () => {
    if (hasSettingsChanged) {
      setShowWarnModal(true);
      return;
    }
    proceedToNextStep();
  };

  const cleanId = (id: string) => {
    let prev;
    let current = id;
    do {
      prev = current;
      current = current.replace(/_(upload|parsed|raw|dp|fs|de|en|ea)$/g, "");
    } while (current !== prev);
    return current;
  };

  const getDatasetCVResults = (dsId: string) => {
    if (!cvResults) return [];
    let datasetObj = null;
    if (cvResults && !Array.isArray(cvResults)) {
      if (cvResults[dsId]) {
        datasetObj = cvResults[dsId];
      } else {
        const mergedKey = Object.keys(cvResults).find(k => k.includes("merged_") && k.includes("_training"));
        if (mergedKey && cvResults[mergedKey]) {
          datasetObj = cvResults[mergedKey];
        } else {
          // Fuzzy matching fallback
          const targetClean = cleanId(dsId);
          const matchedKey = Object.keys(cvResults).find(k => {
            const kClean = cleanId(k);
            return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
          });
          if (matchedKey && cvResults[matchedKey]) {
            datasetObj = cvResults[matchedKey];
          }
        }
      }
    }
    if (datasetObj) {
      if (Array.isArray(datasetObj)) return datasetObj;
      if (datasetObj.metrics) return datasetObj.metrics;
    }
    return Array.isArray(cvResults) ? cvResults : [];
  };

  const getCVRocPlotForDataset = (dsId: string): string | null => {
    if (!cvResults || Array.isArray(cvResults)) return null;
    let datasetObj = cvResults[dsId];
    if (!datasetObj) {
      const mergedKey = Object.keys(cvResults).find(k => k.includes("merged_") && k.includes("_training"));
      if (mergedKey) {
        datasetObj = cvResults[mergedKey];
      } else {
        // Fuzzy matching fallback
        const targetClean = cleanId(dsId);
        const matchedKey = Object.keys(cvResults).find(k => {
          const kClean = cleanId(k);
          return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
        });
        if (matchedKey && cvResults[matchedKey]) {
          datasetObj = cvResults[matchedKey];
        }
      }
    }
    if (datasetObj && !Array.isArray(datasetObj) && datasetObj.roc_plot) {
      return datasetObj.roc_plot;
    }
    return null;
  };

  const getDatasetTrainingResults = (dsId: string) => {
    const cache = state.fsTrainingResultsCache;
    if (!cache) return null;
    if (cache[dsId]) return cache[dsId];
    const mergedKey = Object.keys(cache).find(k => k.includes("merged_") && k.includes("_training"));
    if (mergedKey && cache[mergedKey]) return cache[mergedKey];
    
    // Fuzzy matching fallback
    const targetClean = cleanId(dsId);
    const matchedKey = Object.keys(cache).find(k => {
      const kClean = cleanId(k);
      return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
    });
    if (matchedKey && cache[matchedKey]) return cache[matchedKey];

    return null;
  };

  const processedCVList = useMemo(() => {
    const trainingDs = state.fsDatasets.filter(
      d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" ||
           d.fs_datasetPurpose === "train-and-cv" || !d.fs_datasetPurpose
    );
    const testingDs = state.fsDatasets.filter(
      d => d.fs_datasetPurpose === "test" && (d.fs_validationStrategy === "cv-only" || d.fs_validationStrategy === "cv-and-test")
    );

    const mode = state.fsConfig.multiDatasetMode ?? "combine";
    const processedTrain: { id: string; name: string; isPooled: boolean; isTest?: boolean }[] = [];

    if (!isCombinedPool || trainingDs.length <= 1) {
      trainingDs.forEach(d => {
        processedTrain.push({ id: d.id, name: d.name, isPooled: false });
      });
    } else {
      const typeGroups: Record<string, typeof trainingDs> = {};
      trainingDs.forEach(d => {
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
          processedTrain.push({
            id: mergedId,
            name: `Pooled ${dt.toUpperCase()} Training Datasets`,
            isPooled: true
          });
        } else {
          group.forEach(d => {
            processedTrain.push({ id: d.id, name: d.name, isPooled: false });
          });
        }
      });
    }

    return [
      ...processedTrain,
      ...testingDs.map(d => ({ id: d.id, name: d.name, isPooled: false, isTest: true }))
    ];
  }, [state.fsDatasets, state.fsConfig.multiDatasetMode]);

  return (
    <>
      {loading && <Spinner label="Running cross-validation…" sublabel="Searching parameter space…" />}

      {/* CV Method */}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 4 }}>Cross-Validation Method</div>
        <div className="card-sub">Choose a hyperparameter search strategy.</div>
        <hr className="card-divider" />
        <div style={{ display: "flex", flexDirection: "column", gap: 8, marginBottom: 16 }}>
          {availableMethods.map(m => (
            <label key={m.id} style={{
              display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8, cursor: "pointer",
              border: `1px solid ${fsConfig.cvMethod === m.id ? "var(--primary)" : "var(--border)"}`,
              background: fsConfig.cvMethod === m.id ? "var(--selected-bg)" : "white",
            }}>
              <input type="radio" checked={fsConfig.cvMethod === m.id}
                onChange={() => dispatch({ type: "FS_SET_CONFIG", patch: { cvMethod: m.id } })}
                style={{ accentColor: "var(--primary)", marginTop: 2 }} />
              <div>
                <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
              </div>
            </label>
          ))}
        </div>

        {/* ========================================================== */}
        {/* CV Parameters */}
        {/* ========================================================== */}
        {(fsConfig.cvMethod === "k_fold") && (
          <div style={{ display: "flex", flexDirection: "column", gap: 16, padding: "14px 16px", background: "var(--sub-panel-bg, #f9fafb)", borderRadius: 8, border: "1px solid var(--border)", marginTop: 12 }}>
            <div>
              <label style={{ fontSize: 12, fontWeight: 600, display: "block", marginBottom: 6, color: "var(--foreground)" }}>CV Folds</label>
              <div style={{ display: "flex", gap: 10 }}>
                {availableFolds.map(val => {
                  const label = `${val} Folds`;
                  const desc = val === 5 ? "Standard, balanced" : "More robust, slower";
                  return (
                    <label key={val} style={{
                      flex: 1, display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8, cursor: "pointer",
                      border: `1px solid ${fsConfig.cvFolds === val ? "var(--primary)" : "var(--border)"}`,
                      background: fsConfig.cvFolds === val ? "var(--selected-bg)" : "white",
                    }}>
                      <input type="radio" name="cvFolds_rand" checked={fsConfig.cvFolds === val}
                        onChange={() => dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: val } })}
                        style={{ accentColor: "var(--primary)", marginTop: 2 }} />
                      <div>
                        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{label}</div>
                        <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{desc}</div>
                      </div>
                    </label>
                  );
                })}
              </div>
            </div>
          </div>
        )}
      </div>

      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}

      {done && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
          {processedCVList.map(d => {
            const displayName = d.name;
            const dsCV = getDatasetCVResults(d.id);
            if (!dsCV || dsCV.length === 0) return null;
            return (
              <div key={d.id} style={{ display: "flex", flexDirection: "column", gap: 14 }}>
                <div className="card" style={{ marginBottom: 14, opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
                  <div className="card-title" style={{ marginBottom: 10 }}>
                  Cross-Validation Results - {displayName} {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated - settings changed)</span>}
                </div>

                <div className="banner success" style={{ marginBottom: 12 }}>✅ Cross-validation complete.</div>

                {getCVRocPlotForDataset(d.id) && (
                  <div style={{ display: "flex", justifyContent: "center", marginBottom: 14 }}>
                    <img src={`data:image/png;base64,${getCVRocPlotForDataset(d.id)}`} style={{ width: 600, height: 400, objectFit: "contain" }} />
                  </div>
                )}

                <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12, marginBottom: 14 }}>
                  <thead>
                    <tr style={{ background: "hsl(214 50% 96%)" }}>
                      <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Model</th>
                      <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Best CV Score (AUC)</th>
                      <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)", minWidth: 120 }}>95% CI</th>
                      <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>Balanced Accuracy</th>
                      <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>PPV</th>
                      <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "hsl(220 15% 30%)", borderBottom: "2px solid hsl(214 20% 85%)" }}>NPV</th>
                    </tr>
                  </thead>
                  <tbody>
                    {dsCV.map((r, i) => (
                      <tr key={r.model} style={{ borderBottom: "1px solid hsl(214 20% 92%)", background: i % 2 === 0 ? "white" : "hsl(214 50% 98%)" }}>
                        <td style={{ padding: "8px 10px", textTransform: "capitalize", fontWeight: 500 }}>{r.model}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 700, color: "hsl(150 55% 30%)" }}>{r.bestScore}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 48%)", whiteSpace: "nowrap" }}>{formatCi(r.ci, r.ci_lower, r.ci_upper)}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.accuracy}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.ppv}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.npv}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>

                {/* Explanation cards for reference */}
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
            </div>
          );
          })}
        </div>
      )}



      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => dispatch({ type: "FS_SET_STEP", step: "model-selection" })}>← Back</button>
        </div>

        <div style={{ display: "flex", gap: 8 }}>
          {done && hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={handleRedo} data-testid="btn-redo-cv">
              🔄 Redo Cross-Validation
            </button>
          )}

          {!done ? (
            <button className="btn btn-primary" onClick={handleRun} data-testid="btn-run-cv">Run Cross-Validation</button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-next-cv" disabled={!!hasSettingsChanged}>
              {hasAnyTesting ? "Continue to Testing →" : "Continue to Export →"}
            </button>
          )}
        </div>
      </div>

      {showDiscardModal && (
        <DiscardWarnModal
          stepId="cv"
          datasetIds={state.fsDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            await runRedoCV();
          }}
        />
      )}
    </>
  );
}
