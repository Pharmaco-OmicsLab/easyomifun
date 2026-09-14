import { useState, useEffect, useMemo } from "react";
import { useLocation } from "wouter";
import { ChevronDown, ChevronRight, Layers, AlertTriangle, CheckCircle } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import { type MLModel, getHierarchyGroup, canPoolDatasets } from "../../dataObject";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import { runFeatureSelectionAPI, redoStepAPI, cancelFSJobAPI, pollFSJobAPI, type FSProgress, type TrainingResponse, refitFeaturesAPI, fetchDatasetInfoAPI } from "../../lib/api";
import { formatMetric, formatCi } from "../../lib/utils";
import { toast } from "../../hooks/use-toast";

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
  stabl: "STABL",
  boruta: "Boruta",
};

interface ModelParam {
  key: string;
  label: string;
  default: string | number;
  type?: "text" | "number" | "select";
  options?: string[];
  min?: number;
  max?: number;
  step?: number;
}

const ML_MODEL_DEFS: { id: MLModel; label: string; desc: string; params: ModelParam[] }[] = [
  {
    id: "stabl", label: "STABL",
    desc: "Selects a sparse, reproducible signature by fitting a sparse base model over many bootstraps and controlling the false discovery rate with artificial (decoy) features. Robust for high-dimensional omics, but computationally heavier.",
    params: [
      { key: "base_estimator", label: "Base Estimator", type: "select", options: ["Logistic L1", "Logistic Elastic Net"], default: "Logistic L1" },
      { key: "alpha", label: "L1/L2 Ratio", type: "number", default: 1, min: 0, max: 1, step: 0.05 },
      { key: "max_iter", label: "Max Iterations", type: "select", options: ["1000", "2000", "5000", "10000", "100000"], default: "1000" },
      { key: "n_bootstraps", label: "Bootstrap Runs", type: "number", default: 300 },
      { key: "artificial_type", label: "Artificial Type", type: "select", options: ["random_permutation", "knockoff"], default: "random_permutation" },
      { key: "artificial_proportion", label: "Artificial Proportion", type: "number", default: 1.0, min: 0, max: 1, step: 0.1 },
      { key: "refit_max_depth", label: "Refit Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "refit_n_estimators", label: "Refit Number of trees", type: "number", default: 500, min: 1, step: 1 },
    ],
  },
  {
    id: "boruta", label: "Boruta",
    desc: "Iteratively compares each feature's importance against randomized shadow features using a Random Forest, keeping every feature that is confirmed relevant. Captures all-relevant features, but does not enforce sparsity.",
    params: [
      { key: "max_runs", label: "Max Runs", type: "number", default: 100, min: 11, step: 1 },
      { key: "p_value", label: "P-value cutoff", type: "number", default: 0.01, min: 0, max: 1},
      { key: "max_depth", label: "Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "n_estimators", label: "Number of Trees", type: "number", default: 500, min: 1, step: 1 },
      { key: "keep_tentative", label: "Keep Tentative", type: "select", options: ["no", "yes"], default: "no" },
    ],
  },
  {
    id: "gbm", label: "Gradient Boosting Model",
    desc: "Builds an ensemble of boosted decision trees. Often achieves high predictive accuracy, but requires more parameter tuning.",
    params: [
      { key: "n.trees", label: "Boosting Iterations", type: "select", options: ["100", "300", "500", "1000"], default: "500" },
      { key: "interaction.depth", label: "Max Depth", type: "number", default: 3, min: 1, max: 20, step: 1 },
      { key: "shrinkage", label: "Learning Rate", type: "select", options: ["0.01", "0.05", "0.1", "0.3"], default: "0.1" },
      { key: "n.minobsinnode", label: "Min. Node Size", type: "number", default: 10, min: 1, max: 10, step: 1 },
    ],
  },
  {
    id: "randomforest", label: "Random Forest",
    desc: "Combines multiple decision trees for classification. Provides robust performance and feature importance, but may be less interpretable than simpler models.",
    params: [
      { key: "max_depth", label: "Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "num_trees", label: "Number of Trees", type: "number", default: 500, min: 1, step: 1 },
    ],
  },
  {
    id: "logistic", label: "Logistic Regression",
    desc: "Models the relationship between features and classes. Fast, interpretable, and a strong baseline, but may not capture complex nonlinear patterns.",
    params: [
      { key: "lambda", label: "Regularization Lambda", type: "number", default: 0.1, min: 0.0001, step: 0.01 },
      { key: "alpha", label: "L1/L2 Ratio", type: "number", default: 1.0, min: 0, max: 1, step: 0.05 },
      { key: "max_iter", label: "Max Iterations", type: "select", options: ["100", "500", "1000", "1500", "2000"], default: "1000" },
    ],
  },
  {
    id: "svm", label: "Support Vector Machine",
    desc: "Finds the optimal boundary between classes. Performs well on high-dimensional data, but can be slower on large datasets.",
    params: [
      { key: "C", label: "Regularization C", type: "number", default: 1.0, min: 0 },
      { key: "weight", label: "Class Weight", type: "number", default: 1, min: 1, max: 20, step: 1 },
    ],
  },
];

function buildFullModelParams(
  selectedModels: MLModel[],
  userParams: Record<string, Record<string, string | number>>
): Record<string, Record<string, string | number | null>> {
  const result: Record<string, Record<string, string | number | null>> = {};
  for (const mId of selectedModels) {
    const def = ML_MODEL_DEFS.find(d => d.id === mId);
    const paramsMap: Record<string, string | number | null> = {};
    if (def) {
      def.params.forEach(p => {
        paramsMap[p.key] = p.default;
      });
    }
    if (userParams[mId]) {
      Object.assign(paramsMap, userParams[mId]);
    }
    
    // No conditional overrides needed for STABL — fdr_threshold_range is always used
    
    result[mId] = paramsMap;
  }
  return result;
}

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

export default function ModelSelectionStep() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const fsConfig = state.fsConfig;

  // Global Multi-Dataset and Pooling determination
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
  // NOTE: backendSharedCounts is populated below via useEffect; this will reactively update
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

  const [expandedModels, setExpandedModels] = useState<Set<MLModel>>(new Set());
  const [modelParams, setModelParams] = useState<Record<string, Record<string, string | number>>>({});
  const [fsResults, setFsResults] = useState<any>(state.fsTrainingResultsCache);
  const [selectionMethod, setSelectionMethod] = useState<"breakoff" | "percentage" | "max_features" | "overlap">("breakoff");
  const [percentageValue, setPercentageValue] = useState<number>(80);
  const [maxFeaturesValue, setMaxFeaturesValue] = useState<number>(10);
  const [selectionJobId, setSelectionJobId] = useState<string>(state.fsTrainingResultsCache?.jobId || "");
  const [isRefitting, setIsRefitting] = useState<boolean>(false);
  const [isRefitted, setIsRefitted] = useState<boolean>(() => Boolean(state.fsTrainingResultsCache?.isRefitted));
  const [submittedSelectionParams, setSubmittedSelectionParams] = useState<{
    method: string;
    percentage: number;
    maxFeatures: number;
  } | null>(() => {
    if (state.fsTrainingResultsCache?.isRefitted) {
      return {
        method: "breakoff",
        percentage: 80,
        maxFeatures: 10
      };
    }
    return null;
  });
  const [selectedModelTab, setSelectedModelTab] = useState<Record<string, string>>({});

  const trainingDatasets = state.fsDatasets.filter(
    d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || d.fs_datasetPurpose === "train-and-cv" || d.fs_datasetPurpose === "train-cv-test" || !d.fs_datasetPurpose
  );

  const getDatasetResults = (dsId: string) => {
    if (!fsResults) return null;
    if (fsResults[dsId]) return fsResults[dsId];
    // Check if fsResults is itself the dataset result directly
    if (fsResults.features || fsResults.model_features || fsResults.performance_metrics) {
      return fsResults;
    }
    // Check for merged training dataset key
    const mergedKey = Object.keys(fsResults).find(k => k.includes("merged_") && k.includes("_training"));
    if (mergedKey && fsResults[mergedKey]) return fsResults[mergedKey];

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
    const matchedKey = Object.keys(fsResults).find(k => {
      if (k === "jobId" || k === "status" || k === "message") return false;
      const kClean = cleanId(k);
      return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
    });
    if (matchedKey && fsResults[matchedKey]) return fsResults[matchedKey];

    // Fallback: return the first dataset result with features or metrics
    const validKeys = Object.keys(fsResults).filter(k => !k.startsWith(".") && k !== "jobId" && k !== "status" && k !== "message" && typeof fsResults[k] === "object" && fsResults[k] !== null);
    if (validKeys.length > 0) {
      for (const k of validKeys) {
        if (fsResults[k] && (fsResults[k].features || fsResults[k].model_features || fsResults[k].performance_metrics)) {
          return fsResults[k];
        }
      }
      return fsResults[validKeys[0]];
    }
    return null;
  };

  const anyDatasetHasFeatures = trainingDatasets.some(d => {
    const dsRes = getDatasetResults(d.id);
    return Boolean(
      dsRes && (
        (dsRes.features && dsRes.features.length > 0) ||
        (dsRes.model_features && Object.values(dsRes.model_features).some((arr: any) => Array.isArray(arr) && arr.length > 0))
      )
    );
  }) || Boolean(
    fsResults && (
      (fsResults.features && fsResults.features.length > 0) ||
      (fsResults.model_features && Object.values(fsResults.model_features).some((arr: any) => Array.isArray(arr) && arr.length > 0)) ||
      Object.values(fsResults).some((val: any) => val && typeof val === "object" && ((val.features && val.features.length > 0) || (val.model_features && Object.values(val.model_features).some((arr: any) => Array.isArray(arr) && arr.length > 0))))
    )
  );

  const hasOverlapOption = fsConfig.selectedModels.length > 1 && (trainingDatasets.some(d => {
    const dsRes = getDatasetResults(d.id);
    return dsRes && dsRes.overlap_count !== undefined && dsRes.overlap_count > 0;
  }) || (fsResults && (fsResults.overlap_count !== undefined && fsResults.overlap_count > 0)));

  const isOnlyBorutaStabl = fsConfig.selectedModels.length > 0 && fsConfig.selectedModels.every(m => m === "boruta" || m === "stabl");
  const hasAnyRankModel = fsConfig.selectedModels.some(m => m !== "boruta" && m !== "stabl");

  const showFeatureSelectionMethodCard = Boolean(
    fsResults && (anyDatasetHasFeatures || Object.keys(fsResults).length > 0) && hasAnyRankModel
  );

  const totalSamples = state.fsDatasets.reduce((sum, d) => sum + (d.nSamples || 0), 0);

  const cvTrackSizes = useMemo(() => {
    const sizes: number[] = [];
    const trainingDs = state.fsDatasets.filter(d => 
      d.fs_datasetPurpose === "train" || 
      d.fs_datasetPurpose === "train-and-test" || 
      d.fs_datasetPurpose === "train-and-cv" || 
      d.fs_datasetPurpose === "train-cv-test" ||
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

  const trainSize = useMemo(() => {
    if (cvTrackSizes.length === 0) return 0;
    return Math.min(...cvTrackSizes);
  }, [cvTrackSizes]);

  const cvAllowed = trainSize >= 4;

  useEffect(() => {
    if (!cvAllowed && fsConfig.cvEnabled) {
      dispatch({ type: "FS_SET_CONFIG", patch: { cvEnabled: false } });
    }
  }, [cvAllowed, fsConfig.cvEnabled, dispatch]);



  const toggleExpanded = (id: MLModel) => {
    setExpandedModels(prev => {
      const next = new Set(prev);
      if (next.has(id)) next.delete(id); else next.add(id);
      return next;
    });
  };

  const toggleModel = (id: MLModel) => {
    const next = fsConfig.selectedModels.includes(id)
      ? fsConfig.selectedModels.filter(m => m !== id)
      : [...fsConfig.selectedModels, id];
    dispatch({ type: "FS_SET_CONFIG", patch: { selectedModels: next } });
    if (!fsConfig.selectedModels.includes(id)) {
      // Auto-expand when selecting
      setExpandedModels(prev => new Set([...prev, id]));
    }
  };

  const setParam = (modelId: MLModel, key: string, value: string | number) => {
    setModelParams(prev => {
      const modelPrev = prev[modelId] ?? {};
      let val = value;

      if (modelId === "stabl" && key === "base_estimator") {
        if (val === "Logistic L1") {
          return {
            ...prev,
            [modelId]: {
              ...modelPrev,
              [key]: val,
              alpha: 1
            }
          };
        }
      }

      if ((modelId === "stabl" || modelId === "logistic") && key === "alpha" && typeof val === "number") {
        if (val < 0) val = 0;
        if (val > 1) val = 1;
      }

      if (modelId === "logistic" && key === "lambda" && typeof val === "number") {
        if (val <= 0) val = 0.0001;
      }

      if (modelId === "svm" && key === "weight" && typeof val === "number") {
        if (val < 1) val = 1;
        if (val > 20) val = 20;
      }

      if (modelId === "gbm" && key === "interaction.depth" && typeof val === "number") {
        val = Math.max(1, Math.min(20, Math.round(val)));
      }

      if ((modelId === "randomforest" || modelId === "boruta") && key === "max_depth" && typeof val === "number") {
        val = Math.max(1, Math.min(20, Math.round(val)));
      }

      if (modelId === "gbm" && key === "n.minobsinnode" && typeof val === "number") {
        val = Math.max(1, Math.min(10, Math.round(val)));
      }

      return {
        ...prev,
        [modelId]: {
          ...modelPrev,
          [key]: val
        }
      };
    });
  };

  const getParam = (modelId: MLModel, key: string, def: string | number) => {
    const val = modelParams[modelId]?.[key] ?? def;
    if (modelId === "stabl" && key === "base_estimator") {
      if (val === "lasso" || val === "logistic_l1") return "Logistic L1";
      if (val === "elasticnet") return "Logistic Elastic Net";
    }
    return val;
  };

  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const [loadingTraining, setLoadingTraining] = useState(false);
  const [trainElapsed, setTrainElapsed] = useState(0);
  const [submittedModelParams, setSubmittedModelParams] = useState<Record<string, Record<string, string | number>>>({});
  const [trainInfo, setTrainInfo] = useState<FSProgress | null>(null);
  const usesSelector = fsConfig.selectedModels.some(m => m === "stabl" || m === "boruta");

  const FS_JOB_LS_KEY = "fs_pending_job_standalone";

  // Shared success handling (used by both a fresh run and a resumed-after-reload run).
  const applyFSResult = (res: TrainingResponse, isRefit = false) => {
    if (isRefit && res) {
      (res as any).isRefitted = true;
      setSubmittedSelectionParams({
        method: selectionMethod,
        percentage: percentageValue,
        maxFeatures: maxFeaturesValue
      });
    } else {
      setSubmittedSelectionParams(null);
    }
    setSelectionJobId((res as any).jobId || selectionJobId || "");
    setFsResults(res);
    dispatch({ type: "FS_SET_TRAINING_CACHE", results: res });
    setSubmittedModelParams(modelParams);
    setLoadingTraining(false);
    dispatch({ type: "FS_SUBMIT_CONFIG" });
    dispatch({ type: "SET_STANDALONE_FS_DONE", done: true });
    try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }

    const allWarnings: string[] = [];
    if (res) {
      Object.values(res).forEach((val: any) => {
        if (val && val.warnings && Array.isArray(val.warnings) && val.warnings.length > 0) {
          allWarnings.push(...val.warnings);
        }
      });
    }
    if (allWarnings.length > 0) {
      toast({
        title: "Model Execution Notice",
        description: allWarnings.join("\n"),
        variant: "destructive"
      });
    }
  };

  const handleFSProgress = (info: FSProgress) => {
    setTrainElapsed(info.elapsed);
    setTrainInfo(info);
    // Persist the jobId so a page reload can re-attach to the still-running job.
    if (info.jobId) {
      try { localStorage.setItem(FS_JOB_LS_KEY, JSON.stringify({ jobId: info.jobId })); } catch (_) { /* ignore */ }
    }
  };

  const cancelTraining = async () => {
    if (trainInfo?.jobId) await cancelFSJobAPI(trainInfo.jobId, false);
    setLoadingTraining(false);
    setTrainInfo(null);
    try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }
  };

  // Resume a still-running FS job after a page reload (the background job keeps going).
  useEffect(() => {
    let stored: { jobId?: string } | null = null;
    try { stored = JSON.parse(localStorage.getItem(FS_JOB_LS_KEY) || "null"); } catch (_) { stored = null; }
    if (!stored?.jobId || fsResults) return;
    let cancelled = false;
    setLoadingTraining(true);
    pollFSJobAPI(stored.jobId, false, handleFSProgress)
      .then(res => { if (!cancelled) applyFSResult(res); })
      .catch(() => {
        if (!cancelled) {
          setLoadingTraining(false);
          try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }
        }
      });
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const done = state.standaloneFsDone;
  const submittedCfg = state.fsSubmittedConfig;
  const hasTrainingSettingsChanged = done && submittedCfg && (
    state.fsDatasets.some(d => {
      const submittedRatio = submittedCfg.fs_trainRatios?.[d.id] ?? 0.7;
      const currentRatio = d.fs_trainRatio ?? 0.7;
      return submittedRatio !== currentRatio;
    }) ||
    JSON.stringify(submittedCfg.selectedModels) !== JSON.stringify(fsConfig.selectedModels) ||
    JSON.stringify(submittedModelParams) !== JSON.stringify(modelParams)
  );

  const hasSelectionMethodChanged = Boolean(isRefitted && submittedSelectionParams && (
    submittedSelectionParams.method !== selectionMethod ||
    (selectionMethod === "percentage" && submittedSelectionParams.percentage !== percentageValue) ||
    (selectionMethod === "max_features" && submittedSelectionParams.maxFeatures !== maxFeaturesValue)
  ));

  const hasSettingsChanged = hasTrainingSettingsChanged;

  const completedModules: string[] = [];
  if (state.standaloneFsDone) completedModules.push("Cross-Validation / Feature Selection Run");
  const downstreamDone = completedModules.length > 0;

  const handleRefit = async () => {
    if (!selectionJobId) {
      toast({
        title: "Cannot Refit",
        description: "Job ID is missing. Please train models first.",
        variant: "destructive"
      });
      return;
    }

    setIsRefitting(true);
    try {
      const refitResponse = await refitFeaturesAPI(
        selectionJobId,
        selectionMethod,
        selectionMethod === "percentage" ? percentageValue : null,
        selectionMethod === "max_features" ? maxFeaturesValue : null
      );
      if (refitResponse) {
        (refitResponse as any).isRefitted = true;
        (refitResponse as any).jobId = selectionJobId;
        applyFSResult(refitResponse, true);
        setIsRefitted(true);
        setSubmittedSelectionParams({
          method: selectionMethod,
          percentage: selectionMethod === "percentage" ? (percentageValue || 80) : 80,
          maxFeatures: selectionMethod === "max_features" ? (maxFeaturesValue || 10) : 10
        });
        toast({
          title: "Features Refitted",
          description: "Models have been successfully retrained on the selected features."
        });
      }
    } finally {
      setIsRefitting(false);
    }
  };

  const dataTypes = state.fsDatasets.map(d => d.dataType || "readcounts");
  const typeCounts: Record<string, number> = {};
  dataTypes.forEach(t => { typeCounts[t] = (typeCounts[t] || 0) + 1; });
  
  // Find group datasets of same hierarchy
  const transcriptomicsDatasets = state.fsDatasets.filter(
    d => getHierarchyGroup(d.dataType || "readcounts") === "transcriptomics"
  );
  const proteomicsDatasets = state.fsDatasets.filter(
    d => getHierarchyGroup(d.dataType || "readcounts") === "proteomics"
  );

  const sharedTranscriptomicsCount = getSharedFeatureCountForGroup(transcriptomicsDatasets);
  const sharedProteomicsCount = getSharedFeatureCountForGroup(proteomicsDatasets);

  // Authoritative shared feature counts from the backend (full expression matrix, not 100-row preview)
  const [backendSharedCounts, setBackendSharedCounts] = useState<{ transcriptomics?: number; proteomics?: number }>({});

  useEffect(() => {
    let cancelled = false;
    const fetchCounts = async () => {
      const groups: Array<{ key: "transcriptomics" | "proteomics"; ds: typeof state.fsDatasets }> = [
        { key: "transcriptomics", ds: transcriptomicsDatasets },
        { key: "proteomics",      ds: proteomicsDatasets },
      ];
      for (const { key, ds } of groups) {
        if (ds.length < 2) continue;
        try {
          const res = await fetchDatasetInfoAPI(ds.map(d => d.id), "fs");
          if (!cancelled && res?.sharedFeaturesCount !== undefined) {
            setBackendSharedCounts(prev => ({ ...prev, [key]: res.sharedFeaturesCount }));
          }
        } catch (_) { /* keep parsedData fallback */ }
      }
    };
    fetchCounts();
    return () => { cancelled = true; };
  }, [state.fsDatasets]);

  // Use backend count when available, else fall back to parsedData intersection (100-row preview only)
  const sharedTranscriptomicsCountFinal = backendSharedCounts.transcriptomics ?? sharedTranscriptomicsCount;
  const sharedProteomicsCountFinal = backendSharedCounts.proteomics ?? sharedProteomicsCount;

  const combinedPurpose = state.fsDatasets[0]?.fs_datasetPurpose === "train-and-cv" ? "train-and-cv" : "train-and-test";
  const combinedRatio = state.fsDatasets[0]?.fs_trainRatio ?? 0.7;
  const combinedHierarchy = getHierarchyGroup(state.fsDatasets[0]?.dataType || "readcounts");

  const testingDatasets = state.fsDatasets.filter(d => d.fs_datasetPurpose === "test");

  // Force validations and strategy selections based on sample sizes (< 100) and defaults
  useEffect(() => {
    // 1. Check CV by default
    if (fsConfig.cvEnabled === undefined) {
      dispatch({ type: "FS_SET_CONFIG", patch: { cvEnabled: true } });
    }
  }, [fsConfig.cvEnabled, dispatch]);

  useEffect(() => {
    if (!isSingleDataset && hasPoolableTypesAndSufficientFeatures && fsConfig.multiDatasetMode !== "combine") {
      dispatch({ type: "FS_SET_CONFIG", patch: { multiDatasetMode: "combine" } });
    }
  }, [isSingleDataset, hasPoolableTypesAndSufficientFeatures, fsConfig.multiDatasetMode, dispatch]);

  useEffect(() => {
    if (isCombinedPool) {
      if (totalSamples < 100 && combinedPurpose !== "train-and-cv") {
        state.fsDatasets.forEach(d => {
          dispatch({
            type: "FS_UPDATE_DATASET", id: d.id,
            patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
          });
        });
      }
    } else {
      // Individual mode: check each dataset
      state.fsDatasets.forEach(d => {
        if (d.nSamples < 100) {
          const currentPurpose = d.fs_datasetPurpose || "train-and-test";
          if (currentPurpose === "test") {
            if (d.fs_validationStrategy !== "cv-only") {
              dispatch({
                type: "FS_UPDATE_DATASET", id: d.id,
                patch: { fs_validationStrategy: "cv-only", fs_trainRatio: 1.0 }
              });
            }
          } else {
            if (currentPurpose !== "train-and-cv") {
              dispatch({
                type: "FS_UPDATE_DATASET", id: d.id,
                patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
              });
            }
          }
        }
      });
    }
  }, [isCombinedPool, totalSamples, combinedPurpose, state.fsDatasets, dispatch]);



  const hasCvOnly = state.fsDatasets.some(d => 
    d.fs_datasetPurpose === "train-and-cv" || 
    (d.fs_datasetPurpose === "test" && d.fs_validationStrategy === "cv-only")
  );

  const hasInternalTest = state.fsDatasets.some(d => 
    d.fs_datasetPurpose === "train-and-test" || 
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
  );

  const hasAnyTesting = state.fsDatasets.some(d => 
    (d.fs_datasetPurpose === "test" &&
     d.fs_validationStrategy !== "cv-only" &&
     d.fs_validationStrategy !== "train-and-cv") || 
    d.fs_datasetPurpose === "train-and-test" || 
    d.fs_datasetPurpose === "train-cv-test" ||
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
  );

  let allocationError: string | null = null;
  if (trainingDatasets.length === 0) {
    allocationError = "You must assign at least 1 dataset for Training.";
  } else if (testingDatasets.length > 0) {
    for (const testDs of testingDatasets) {
      const testHierarchy = getHierarchyGroup(testDs.dataType || "readcounts");
      const hasMatchingTrain = trainingDatasets.some(trainDs => getHierarchyGroup(trainDs.dataType || "readcounts") === testHierarchy);
      if (!hasMatchingTrain) {
        allocationError = `External testing dataset "${testDs.name}" (${testHierarchy}) requires at least 1 training dataset from the same hierarchy group.`;
        break;
      }
    }
  }
  if (!allocationError && !hasInternalTest && !hasCvOnly && !fsConfig.cvEnabled && testingDatasets.length === 0) {
    allocationError = "Training requires at least an Internal Test split, Cross-Validation, or an assigned External Testing dataset.";
  }

  const getSubmittedContinueLabel = () => {
    if (!submittedCfg) return "Continue";
    const subCvEnabled = submittedCfg.cvEnabled;
    const subHasCvOnly = state.fsDatasets.some(d => 
      submittedCfg.fs_datasetPurposes?.[d.id] === "train-and-cv" || 
      (submittedCfg.fs_datasetPurposes?.[d.id] === "test" && submittedCfg.fs_validationStrategies?.[d.id] === "cv-only")
    );
    const subHasAnyTesting = state.fsDatasets.some(d => 
      (submittedCfg.fs_datasetPurposes?.[d.id] === "test" &&
       submittedCfg.fs_validationStrategies?.[d.id] !== "cv-only") || 
      submittedCfg.fs_datasetPurposes?.[d.id] === "train-and-test" || 
      ((submittedCfg.fs_datasetPurposes?.[d.id] === "train" || !submittedCfg.fs_datasetPurposes?.[d.id]) && submittedCfg.fs_isInternalValidations?.[d.id])
    );
    
    if (subCvEnabled || subHasCvOnly) {
      return "Continue to Cross Validation";
    } else if (subHasAnyTesting) {
      return "Continue to Testing →";
    } else {
      return "Continue to Export →";
    }
  };

  const revertToSubmittedSettings = () => {
    if (!submittedCfg) return;
    dispatch({
      type: "FS_SET_CONFIG",
      patch: {
        selectedModels: [...submittedCfg.selectedModels],
        multiDatasetMode: submittedCfg.multiDatasetMode,
        cvEnabled: submittedCfg.cvEnabled,
        cvMethod: submittedCfg.cvMethod,
        cvFolds: submittedCfg.cvFolds,
      }
    });
    state.fsDatasets.forEach(d => {
      const trainRatio = submittedCfg.fs_trainRatios?.[d.id] ?? 0.7;
      const datasetPurpose = submittedCfg.fs_datasetPurposes?.[d.id] || "train-and-test";
      const validationStrategy = submittedCfg.fs_validationStrategies?.[d.id] || "train-test-split";
      const isInternalValidation = submittedCfg.fs_isInternalValidations?.[d.id] ?? false;
      dispatch({
        type: "FS_UPDATE_DATASET",
        id: d.id,
        patch: {
          fs_trainRatio: trainRatio,
          fs_datasetPurpose: datasetPurpose as any,
          fs_validationStrategy: validationStrategy as any,
          fs_isInternalValidation: isInternalValidation
        }
      });
    });
    setModelParams(JSON.parse(JSON.stringify(submittedModelParams)));
  };

  const handleContinue = async () => {
    if (allocationError) {
      toast({ title: "Dataset Allocation Issue", description: allocationError, variant: "destructive" });
      return;
    }
    if (hasTrainingSettingsChanged) {
      revertToSubmittedSettings();
      const subCvEnabled = submittedCfg.cvEnabled;
      const subHasCvOnly = state.fsDatasets.some(d => 
        submittedCfg.fs_datasetPurposes?.[d.id] === "train-and-cv" || 
        (submittedCfg.fs_datasetPurposes?.[d.id] === "test" && submittedCfg.fs_validationStrategies?.[d.id] === "cv-only")
      );
      const subHasAnyTesting = state.fsDatasets.some(d => 
        (submittedCfg.fs_datasetPurposes?.[d.id] === "test" &&
         submittedCfg.fs_validationStrategies?.[d.id] !== "cv-only") ||
        submittedCfg.fs_datasetPurposes?.[d.id] === "train-and-test" ||
        ((submittedCfg.fs_datasetPurposes?.[d.id] === "train" || !submittedCfg.fs_datasetPurposes?.[d.id]) && submittedCfg.fs_isInternalValidations?.[d.id])
      );
      const isCV = subCvEnabled || subHasCvOnly;
      if (isCV) {
        dispatch({ type: "FS_SET_STEP", step: "cross-validation" });
      } else if (subHasAnyTesting) {
        dispatch({ type: "FS_SET_STEP", step: "testing" });
      } else {
        dispatch({ type: "FS_SET_STEP", step: "export" });
      }
      return;
    }
    if (!fsResults) {
      proceedToNextStep();
    } else if (hasAnyRankModel && (!isRefitted || hasSelectionMethodChanged)) {
      await handleRefit();
    } else {
      const isCV = fsConfig.cvEnabled || hasCvOnly;
      if (isCV) {
        dispatch({ type: "FS_SET_STEP", step: "cross-validation" });
      } else if (hasAnyTesting) {
        dispatch({ type: "FS_SET_STEP", step: "testing" });
      } else {
        dispatch({ type: "FS_SET_STEP", step: "export" });
      }
    }
  };

  const proceedToNextStep = async () => {
    setShowWarnModal(false);
    setTrainElapsed(0);
    setLoadingTraining(true);
    setIsRefitted(false);
    try {
      const fullParams = buildFullModelParams(fsConfig.selectedModels, modelParams);
      const res = await runFeatureSelectionAPI(
        state.fsDatasets,
        fsConfig.selectedModels,
        state.fsConfig.trainRatio || 0.7,
        fullParams,
        undefined,
        handleFSProgress,
        fsConfig.multiDatasetMode ?? "combine"
      );
      if (res) {
        setSelectionJobId((res as any).jobId || selectionJobId || "");
        setFsResults(res);
        dispatch({ type: "FS_SET_TRAINING_CACHE", results: res });
        setSubmittedModelParams(modelParams);
        setLoadingTraining(false);
        dispatch({ type: "FS_SUBMIT_CONFIG" });
        dispatch({ type: "SET_STANDALONE_FS_DONE", done: true });
        try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }

        const allWarnings: string[] = [];
        Object.values(res).forEach((val: any) => {
          if (val && val.warnings && Array.isArray(val.warnings) && val.warnings.length > 0) {
            allWarnings.push(...val.warnings);
          }
        });
        if (allWarnings.length > 0) {
          toast({
            title: "Model Execution Notice",
            description: allWarnings.join("\n"),
            variant: "destructive"
          });
        }
      }
    } catch (err: any) {
      console.error("Feature selection error:", err);
      setLoadingTraining(false);
      try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }
      toast({
        title: "Model Training Failed",
        description: err.message || "Failed to train machine learning models.",
        variant: "destructive"
      });
    }
  };

  const handleRedo = () => {
    if (downstreamDone) {
      setShowDiscardModal(true);
    } else {
      runRedoAction();
    }
  };

  const runRedoAction = async (skipAPI = false) => {
    if (!skipAPI) {
      try {
        await redoStepAPI("fs", state.fsDatasets.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    setFsResults(null);
    setIsRefitted(false);
    setSubmittedSelectionParams(null);
    dispatch({ type: "FS_SET_TRAINING_CACHE", results: null });
    dispatch({
      type: "RESET_DOWNSTREAM_STEPS",
      datasetId: state.fsCurrentDatasetId,
      fromStep: "model-selection"
    } as any);
    setTrainElapsed(0);
    setLoadingTraining(true);
    try {
      const fullParams = buildFullModelParams(fsConfig.selectedModels, modelParams);
      const res = await runFeatureSelectionAPI(
        state.fsDatasets,
        fsConfig.selectedModels,
        state.fsConfig.trainRatio || 0.7,
        fullParams,
        undefined,
        handleFSProgress
      );
      applyFSResult(res);
    } catch (err: any) {
      console.error("Feature selection error:", err);
        toast({
          title: "Model Training Failed",
          description: err.message || "Failed to train machine learning models.",
          variant: "destructive"
        });
      } finally {
        setLoadingTraining(false);
        try { localStorage.removeItem(FS_JOB_LS_KEY); } catch (_) { /* ignore */ }
      }
  };

  return (
    <>
      {loadingTraining && (
        <Spinner
          label={usesSelector ? "Running feature selection…" : "Training models…"}
          sublabel={(() => {
            const stage = trainInfo?.current && trainInfo?.total
              ? `Model ${trainInfo.current}/${trainInfo.total}${trainInfo.model ? ` (${trainInfo.model})` : ""} — `
              : "";
            const time = trainElapsed > 0 ? `${Math.round(trainElapsed)}s elapsed` : "starting…";
            const hint = usesSelector ? " · STABL/Boruta scan the full feature set, this can take minutes" : "";
            return `${stage}${time}${hint}`;
          })()}
          onCancel={cancelTraining}
        />
      )}
      {isRefitting && (
        <Spinner
          label="Refitting features…"
          sublabel="Retraining models on selected feature subset…"
        />
      )}
      {/* Unified Dataset Purpose & Validation Strategy Panel */}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 4 }}>Dataset Purpose & Validation Strategy</div>
        <div className="card-sub">
          {isSingleDataset
            ? "Choose how your dataset will be partitioned and evaluated during model training."
            : "Assign the role and evaluation strategy for each dataset across training and testing."}
        </div>
        <hr className="card-divider" />

        {/* Dynamic Multi-Dataset Mode Banner (When >= 2 Datasets) */}
        {!isSingleDataset && (
          <div style={{ display: "flex", gap: 12, marginBottom: 16, flexWrap: "wrap" }}>
            {transcriptomicsDatasets.length >= 2 && (
              <div className="stat-chip" style={{ display: "flex", flexDirection: "column", background: "white", border: "1px solid var(--border)", borderRadius: 8, padding: "8px 12px", minWidth: 140 }}>
                <span className="stat-chip-val" style={{ fontSize: 18, fontWeight: 700, color: sharedTranscriptomicsCountFinal >= 10 ? "hsl(150 60% 32%)" : "hsl(0 72% 50%)" }}>
                  {sharedTranscriptomicsCountFinal.toLocaleString()}
                </span>
                <span className="stat-chip-lbl" style={{ fontSize: 10, color: "var(--muted-foreground)", fontWeight: 600, marginTop: 2 }}>
                  Shared Features (Transcriptomics)
                </span>
              </div>
            )}
            {proteomicsDatasets.length >= 2 && (
              <div className="stat-chip" style={{ display: "flex", flexDirection: "column", background: "white", border: "1px solid var(--border)", borderRadius: 8, padding: "8px 12px", minWidth: 140 }}>
                <span className="stat-chip-val" style={{ fontSize: 18, fontWeight: 700, color: sharedProteomicsCountFinal >= 10 ? "hsl(150 60% 32%)" : "hsl(0 72% 50%)" }}>
                  {sharedProteomicsCountFinal.toLocaleString()}
                </span>
                <span className="stat-chip-lbl" style={{ fontSize: 10, color: "var(--muted-foreground)", fontWeight: 600, marginTop: 2 }}>
                  Shared Features (Proteomics)
                </span>
              </div>
            )}
          </div>
        )}

        {!isSingleDataset && hasPoolableTypesAndSufficientFeatures && (
          <div style={{ marginBottom: 16, padding: "12px 14px", border: "1px solid var(--border)", borderRadius: 8, background: "#fafafa" }}>
            <div style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 4 }}>
              <Layers size={15} style={{ color: "var(--primary)" }} />
              Multi-Dataset Handling Strategy
            </div>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 8 }}>
              Multiple datasets of the same data type detected.
            </div>
            <div className="banner info" style={{ marginTop: 0, fontSize: 12 }}>
              ℹ️ <strong>Automatic Pooling:</strong> Datasets of the same type and hierarchy will be pooled and batch-corrected using ComBat/limma before feature selection.
            </div>
          </div>
        )}

        {/* Dataset Purpose & Validation Strategy Body */}
        <div style={{ display: "flex", flexDirection: "column", gap: 14, marginBottom: 16 }}>
          {isCombinedPool ? (
            // Combined Pool / Single Dataset: Render unified purpose selection and single split ratio options
            <div style={{ border: "1px solid var(--border)", borderRadius: 10, padding: 14, background: "#fafafa" }}>
              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 12, flexWrap: "wrap", gap: 8 }}>
                <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <div style={{ width: 10, height: 10, borderRadius: "50%", background: state.fsDatasets[0]?.color || "var(--primary)" }} />
                  <span style={{ fontWeight: 600, fontSize: 14, color: "var(--foreground)" }}>
                    {isSingleDataset
                      ? `${state.fsDatasets[0]?.name} (${totalSamples} samples)`
                      : `Pooled Dataset (${state.fsDatasets.length} Datasets · ${totalSamples} Total Samples)`}
                  </span>
                </div>
                <div style={{ display: "flex", alignItems: "center", gap: 6, flexWrap: "wrap" }}>
                  {!isSingleDataset && state.fsDatasets.map(d => (
                    <span key={d.id} style={{ fontSize: 11, padding: "1px 6px", borderRadius: 4, background: "white", border: "1px solid var(--border)" }}>
                      {d.name} ({d.nSamples})
                    </span>
                  ))}
                  <span style={{
                    fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                    background: combinedHierarchy === "transcriptomics" ? "hsl(214 50% 93%)" : (combinedHierarchy === "proteomics" ? "hsl(140 40% 93%)" : "hsl(30 50% 93%)"),
                    color: combinedHierarchy === "transcriptomics" ? "hsl(214 58% 30%)" : (combinedHierarchy === "proteomics" ? "hsl(140 50% 25%)" : "hsl(30 60% 30%)"),
                    border: "1px solid var(--border)"
                  }}>
                    {combinedHierarchy.toUpperCase()} · {state.fsDatasets[0]?.dataType || "readcounts"}
                  </span>
                </div>
              </div>

              {/* 2 Options for Combined Pool / Single Dataset */}
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                <label style={{
                  display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8,
                  cursor: totalSamples < 100 ? "default" : "pointer",
                  border: `1px solid ${combinedPurpose === "train-and-test" ? "var(--primary)" : "var(--border)"}`,
                  background: combinedPurpose === "train-and-test" ? "var(--selected-bg)" : "white",
                  opacity: totalSamples < 100 ? 0.5 : 1
                }}>
                  <input
                    type="radio"
                    name="fs-combined-purpose"
                    value="train-and-test"
                    disabled={totalSamples < 100}
                    checked={combinedPurpose === "train-and-test"}
                    onChange={() => {
                      state.fsDatasets.forEach(d => {
                        dispatch({
                          type: "FS_UPDATE_DATASET", id: d.id,
                          patch: { fs_datasetPurpose: "train-and-test", fs_validationStrategy: "train-test-split", fs_isInternalValidation: true, fs_trainRatio: combinedRatio }
                        });
                      });
                    }}
                    style={{ accentColor: "var(--primary)", marginTop: 2 }}
                  />
                  <div style={{ flex: 1 }}>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "flex", alignItems: "center", gap: 8 }}>
                      <span>Train and Test (Internal Split)</span>
                      <span className="banner success" style={{ margin: 0, padding: "2px 6px", fontSize: 10 }}>Recommend</span>
                      {totalSamples < 100 && <span style={{ fontSize: 10, color: "hsl(0 72% 50%)", fontWeight: 500 }}>{"(Requires >= 100 samples)"}</span>}
                    </div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>
                      Discovers features on the training partition and evaluates performance on a held-out test partition.
                    </div>
                  </div>
                </label>

                <label style={{
                  display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8, cursor: "pointer",
                  border: `1px solid ${combinedPurpose === "train-and-cv" ? "var(--primary)" : "var(--border)"}`,
                  background: combinedPurpose === "train-and-cv" ? "var(--selected-bg)" : "white"
                }}>
                  <input
                    type="radio"
                    name="fs-combined-purpose"
                    value="train-and-cv"
                    checked={combinedPurpose === "train-and-cv"}
                    onChange={() => {
                      state.fsDatasets.forEach(d => {
                        dispatch({
                          type: "FS_UPDATE_DATASET", id: d.id,
                          patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
                        });
                      });
                    }}
                    style={{ accentColor: "var(--primary)", marginTop: 2 }}
                  />
                  <div style={{ flex: 1 }}>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>
                      Train and Cross-Validation Only
                    </div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>
                      Uses 100% of pooled data for training and hyperparameter tuning via Cross-Validation (no held-out test set).
                    </div>
                  </div>
                </label>
              </div>

              {/* Single Train/Test Split Ratio Option */}
              {(combinedPurpose === "train-and-test" || combinedPurpose === "train-cv-test") && (
                <div style={{ marginTop: 10, paddingTop: 10, borderTop: "1px dashed var(--border)", display: "flex", flexDirection: "column", gap: 6 }}>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>Train / Test Split Ratio:</div>
                  <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
                    {[
                      { value: 0.7, label: "70 / 30 Split" },
                      { value: 0.8, label: "80 / 20 Split" }
                    ].map(opt => {
                      const trainN = Math.round(totalSamples * opt.value);
                      const testN = totalSamples - trainN;
                      return (
                        <label key={opt.value} style={{
                          display: "flex", alignItems: "center", gap: 6, padding: "6px 10px", borderRadius: 6, cursor: "pointer",
                          border: `1px solid ${combinedRatio === opt.value ? "var(--primary)" : "var(--border)"}`,
                          background: combinedRatio === opt.value ? "var(--selected-bg)" : "white", fontSize: 12
                        }}>
                          <input
                            type="radio"
                            name="fs-combined-split-ratio"
                            value={opt.value}
                            checked={combinedRatio === opt.value}
                            onChange={() => {
                              state.fsDatasets.forEach(d => {
                                dispatch({ type: "FS_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } });
                              });
                            }}
                            style={{ accentColor: "var(--primary)" }}
                          />
                          <span><strong>{opt.label}</strong> ({trainN} train / {testN} test)</span>
                        </label>
                      );
                    })}
                  </div>
                </div>
              )}
            </div>
          ) : (
            // Different data types or Individual mode: Render per-dataset selection cards
            state.fsDatasets.map((d, index) => {
              const hierarchy = getHierarchyGroup(d.dataType || "readcounts");
              const currentPurpose = d.fs_datasetPurpose || "train-and-test";
              const currentRatio = d.fs_trainRatio ?? 0.7;
              const isSmall = d.nSamples < 100;

              return (
                <div key={d.id} style={{ border: "1px solid var(--border)", borderRadius: 10, padding: 14, background: "#fafafa" }}>
                  <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 10, flexWrap: "wrap", gap: 8 }}>
                    <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                      <div style={{ width: 10, height: 10, borderRadius: "50%", background: d.color }} />
                      <span style={{ fontWeight: 600, fontSize: 14, color: "var(--foreground)" }}>
                        Dataset {index + 1}: {d.name} ({d.nSamples} samples)
                      </span>
                    </div>
                    <span style={{
                      fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                      background: hierarchy === "transcriptomics" ? "hsl(214 50% 93%)" : (hierarchy === "proteomics" ? "hsl(140 40% 93%)" : "hsl(30 50% 93%)"),
                      color: hierarchy === "transcriptomics" ? "hsl(214 58% 30%)" : (hierarchy === "proteomics" ? "hsl(140 50% 25%)" : "hsl(30 60% 30%)"),
                      border: "1px solid var(--border)"
                    }}>
                      {hierarchy.toUpperCase()} · {d.dataType || "readcounts"}
                    </span>
                  </div>

                  {/* Multi-dataset: 4 options (Train and Test, Train and CV, Train, Test) */}
                  <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                    <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))", gap: 8 }}>
                      <label style={{
                        display: "flex", alignItems: "center", gap: 8, padding: "8px 10px", borderRadius: 7,
                        cursor: isSmall ? "default" : "pointer",
                        border: `1px solid ${currentPurpose === "train-and-test" ? "var(--primary)" : "var(--border)"}`,
                        background: currentPurpose === "train-and-test" ? "var(--selected-bg)" : "white",
                        opacity: isSmall ? 0.5 : 1
                      }}>
                        <input
                          type="radio"
                          name={`purpose-${d.id}`}
                          value="train-and-test"
                          disabled={isSmall}
                          checked={currentPurpose === "train-and-test"}
                          onChange={() => dispatch({
                            type: "FS_UPDATE_DATASET", id: d.id,
                            patch: { fs_datasetPurpose: "train-and-test", fs_validationStrategy: "train-test-split", fs_isInternalValidation: true, fs_trainRatio: 0.7 }
                          })}
                          style={{ accentColor: "var(--primary)" }}
                        />
                        <div>
                          <div style={{ fontSize: 12, fontWeight: 600 }}>Train and Test {isSmall && " (>=100)"}</div>
                          <div style={{ fontSize: 10, color: "var(--muted-foreground)" }}>Train + internal test split</div>
                        </div>
                      </label>

                      <label style={{
                        display: "flex", alignItems: "center", gap: 8, padding: "8px 10px", borderRadius: 7, cursor: "pointer",
                        border: `1px solid ${currentPurpose === "train-and-cv" ? "var(--primary)" : "var(--border)"}`,
                        background: currentPurpose === "train-and-cv" ? "var(--selected-bg)" : "white"
                      }}>
                        <input
                          type="radio"
                          name={`purpose-${d.id}`}
                          value="train-and-cv"
                          checked={currentPurpose === "train-and-cv"}
                          onChange={() => dispatch({
                            type: "FS_UPDATE_DATASET", id: d.id,
                            patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
                          })}
                          style={{ accentColor: "var(--primary)" }}
                        />
                        <div>
                          <div style={{ fontSize: 12, fontWeight: 600 }}>Train and CV</div>
                          <div style={{ fontSize: 10, color: "var(--muted-foreground)" }}>100% data, CV evaluated</div>
                        </div>
                      </label>



                      <label style={{
                        display: "flex", alignItems: "center", gap: 8, padding: "8px 10px", borderRadius: 7, cursor: "pointer",
                        border: `1px solid ${currentPurpose === "test" ? "var(--primary)" : "var(--border)"}`,
                        background: currentPurpose === "test" ? "var(--selected-bg)" : "white"
                      }}>
                        <input
                          type="radio"
                          name={`purpose-${d.id}`}
                          value="test"
                          checked={currentPurpose === "test"}
                          onChange={() => dispatch({
                            type: "FS_UPDATE_DATASET", id: d.id,
                            patch: { fs_datasetPurpose: "test", fs_validationStrategy: isSmall ? "cv-only" : "train-test-split", fs_isInternalValidation: false, fs_trainRatio: isSmall ? 1.0 : 0.7 }
                          })}
                          style={{ accentColor: "var(--primary)" }}
                        />
                        <div>
                          <div style={{ fontSize: 12, fontWeight: 600 }}>Test (Validation)</div>
                          <div style={{ fontSize: 10, color: "var(--muted-foreground)" }}>External testing dataset</div>
                        </div>
                      </label>
                    </div>
                  </div>

                  {/* Sub-options for Train & Test split */}
                  {(currentPurpose === "train-and-test" || currentPurpose === "train-cv-test") && (
                    <div style={{ marginTop: 10, paddingTop: 10, borderTop: "1px dashed var(--border)", display: "flex", flexDirection: "column", gap: 6 }}>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>Train / Test Split Ratio:</div>
                      <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
                        {[
                          { value: 0.7, label: "70 / 30 Split" },
                          { value: 0.8, label: "80 / 20 Split" }
                        ].map(opt => {
                          const trainN = Math.round(d.nSamples * opt.value);
                          const testN = d.nSamples - trainN;
                          return (
                            <label key={opt.value} style={{
                              display: "flex", alignItems: "center", gap: 6, padding: "6px 10px", borderRadius: 6, cursor: "pointer",
                              border: `1px solid ${currentRatio === opt.value ? "var(--primary)" : "var(--border)"}`,
                              background: currentRatio === opt.value ? "var(--selected-bg)" : "white", fontSize: 12
                            }}>
                              <input
                                type="radio"
                                name={`ratio-${d.id}`}
                                value={opt.value}
                                checked={currentRatio === opt.value}
                                onChange={() => dispatch({ type: "FS_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } })}
                                style={{ accentColor: "var(--primary)" }}
                              />
                              <span><strong>{opt.label}</strong> ({trainN} train / {testN} test)</span>
                            </label>
                          );
                        })}
                      </div>
                    </div>
                  )}

                  {/* Sub-options for External Test dataset validation strategy */}
                  {currentPurpose === "test" && (
                    <div style={{ marginTop: 10, paddingTop: 10, borderTop: "1px dashed var(--border)", display: "flex", flexDirection: "column", gap: 6 }}>
                      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>Testing Validation Strategy:</div>
                      <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
                        <label style={{
                          display: "flex", alignItems: "center", gap: 6, padding: "6px 10px", borderRadius: 6,
                          cursor: isSmall ? "default" : "pointer",
                          border: `1px solid ${d.fs_validationStrategy === "train-test-split" ? "var(--primary)" : "var(--border)"}`,
                          background: d.fs_validationStrategy === "train-test-split" ? "var(--selected-bg)" : "white", fontSize: 12,
                          opacity: isSmall ? 0.5 : 1
                        }}>
                          <input
                            type="radio"
                            name={`val-strat-${d.id}`}
                            value="train-test-split"
                            disabled={isSmall}
                            checked={d.fs_validationStrategy === "train-test-split"}
                            onChange={() => dispatch({ type: "FS_UPDATE_DATASET", id: d.id, patch: { fs_validationStrategy: "train-test-split", fs_trainRatio: 0.7 } })}
                            style={{ accentColor: "var(--primary)" }}
                          />
                          <span><strong>Refit with Train/Test Split</strong> {isSmall && " (>= 100 samples)"}</span>
                        </label>

                        <label style={{
                          display: "flex", alignItems: "center", gap: 6, padding: "6px 10px", borderRadius: 6, cursor: "pointer",
                          border: `1px solid ${d.fs_validationStrategy === "cv-only" ? "var(--primary)" : "var(--border)"}`,
                          background: d.fs_validationStrategy === "cv-only" ? "var(--selected-bg)" : "white", fontSize: 12
                        }}>
                          <input
                            type="radio"
                            name={`val-strat-${d.id}`}
                            value="cv-only"
                            checked={d.fs_validationStrategy === "cv-only"}
                            onChange={() => dispatch({ type: "FS_UPDATE_DATASET", id: d.id, patch: { fs_validationStrategy: "cv-only", fs_trainRatio: 1.0 } })}
                            style={{ accentColor: "var(--primary)" }}
                          />
                          <span><strong>Refit with Train & CV</strong></span>
                        </label>


                      </div>

                      {/* Split Ratio sub-option if Refit with Train/Test Split is chosen */}
                      {d.fs_validationStrategy === "train-test-split" && (
                        <div style={{ marginTop: 6, paddingLeft: 14, display: "flex", flexDirection: "column", gap: 6 }}>
                          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--foreground)" }}>Refit Split Ratio:</div>
                          <div style={{ display: "flex", gap: 8 }}>
                            {[
                              { value: 0.7, label: "70 / 30 Split" },
                              { value: 0.8, label: "80 / 20 Split" }
                            ].map(opt => {
                              const trainN = Math.round(d.nSamples * opt.value);
                              const testN = d.nSamples - trainN;
                              return (
                                <label key={opt.value} style={{
                                  display: "flex", alignItems: "center", gap: 6, padding: "4px 8px", borderRadius: 5, cursor: "pointer",
                                  border: `1px solid ${currentRatio === opt.value ? "var(--primary)" : "var(--border)"}`,
                                  background: currentRatio === opt.value ? "var(--selected-bg)" : "white", fontSize: 11
                                }}>
                                  <input
                                    type="radio"
                                    name={`refit-ratio-${d.id}`}
                                    value={opt.value}
                                    checked={currentRatio === opt.value}
                                    onChange={() => dispatch({ type: "FS_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } })}
                                    style={{ accentColor: "var(--primary)" }}
                                  />
                                  <span><strong>{opt.label}</strong> ({trainN} train / {testN} test)</span>
                                </label>
                              );
                            })}
                          </div>
                        </div>
                      )}
                    </div>
                  )}
                </div>
              );
            })
          )}
        </div>

        {/* Global Cross-Validation Toggle */}
        <label style={{
          display: "flex", alignItems: "center", gap: 10, padding: "10px 14px", borderRadius: 8,
          cursor: hasCvOnly ? "default" : "pointer",
          border: `1px solid ${(fsConfig.cvEnabled || hasCvOnly) ? "var(--primary)" : "var(--border)"}`,
          background: (fsConfig.cvEnabled || hasCvOnly) ? "var(--selected-bg)" : "white",
        }}>
          <input
            type="checkbox"
            checked={fsConfig.cvEnabled || hasCvOnly}
            disabled={hasCvOnly}
            onChange={e => dispatch({ type: "FS_SET_CONFIG", patch: { cvEnabled: e.target.checked } })}
            style={{ accentColor: "var(--primary)", width: 16, height: 16 }}
            data-testid="check-cv-enabled"
          />
          <div>
            <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Enable Cross-Validation</div>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
              Adds a CV step to tune hyperparameters and calculate cross-validated performance metrics.
            </div>
          </div>
          {(fsConfig.cvEnabled || hasCvOnly) && (
            <span style={{
              marginLeft: "auto", fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
              background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--primary)",
            }}>
              {hasCvOnly ? "CV Step Required (Train & CV)" : "CV Step Added"}
            </span>
          )}
        </label>

        {/* Allocation Error Banner if any */}
        {allocationError && (
          <div style={{ marginTop: 12, display: "flex", alignItems: "center", gap: 8, padding: "10px 12px", background: "hsl(0 70% 97%)", border: "1px solid hsl(0 70% 85%)", borderRadius: 8, color: "hsl(0 70% 35%)", fontSize: 12 }}>
            <AlertTriangle size={16} />
            <span>{allocationError}</span>
          </div>
        )}
      </div>

      {/* Model Cards */}
      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Machine Learning Models</div>
        <div className="card-sub">Click a model card to select it and expand its parameters.</div>
        <hr className="card-divider" />

        <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
          {ML_MODEL_DEFS.map(model => {
            const isSelected = fsConfig.selectedModels.includes(model.id);
            const isExpanded = expandedModels.has(model.id);
            return (
              <div
                key={model.id}
                style={{
                  border: `1px solid ${isSelected ? "var(--primary)" : "var(--border)"}`,
                  borderRadius: 10,
                  background: isSelected ? "var(--selected-bg)" : "white",
                  overflow: "visible",
                  transition: "all .15s",
                  position: "relative"
                }}
              >
                {/* Card header - always visible */}
                <div
                  style={{ display: "flex", alignItems: "center", gap: 12, padding: "12px 14px", cursor: "pointer" }}
                  onClick={() => toggleModel(model.id)}
                >
                  <div style={{ flex: 1, minWidth: 0 }}>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 2, display: "flex", alignItems: "center", gap: 8 }}>
                      <span>{model.label}</span>
                      {(model.id === "stabl" || model.id === "boruta") && (
                        <div className="banner success" style={{ margin: 0, padding: "2px 8px", fontSize: 10, display: "inline-flex", width: "fit-content" }}>Recommend</div>
                      )}
                    </div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{model.desc}</div>
                  </div>
                  <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                    {isSelected && (
                      <span style={{
                        fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                        background: "hsl(150 50% 93%)", color: "hsl(150 58% 28%)",
                        border: "1px solid hsl(150 35% 76%)",
                      }}>Selected</span>
                    )}
                    <button
                      onClick={e => { e.stopPropagation(); toggleExpanded(model.id); }}
                      style={{ background: "none", border: "none", cursor: "pointer", color: "var(--muted-foreground)", padding: 4 }}
                      title={isExpanded ? "Collapse" : "Expand parameters"}
                      data-testid={`btn-expand-${model.id}`}
                    >
                      {isExpanded ? <ChevronDown size={15} /> : <ChevronRight size={15} />}
                    </button>
                  </div>
                </div>

                {/* Params - shown when expanded */}
                {isExpanded && (
                  <div style={{ borderTop: "1px solid var(--border)", padding: "12px 14px", background: "var(--muted)", borderRadius: "0 0 10px 10px" }}>
                    <div style={{ fontSize: 11, fontWeight: 700, textTransform: "uppercase", letterSpacing: ".04em", color: "var(--muted-foreground)", marginBottom: 10 }}>
                      Hyperparameters
                    </div>
                    <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))", gap: 10 }}>
                      {model.params.map(p => {
                        const rawVal = getParam(model.id, p.key, p.default);
                        const currentValue = String(rawVal);
                        const isDisabled = (() => {
                          if (model.id === "stabl") {
                            if (p.key === "alpha") {
                              return getParam("stabl", "base_estimator", "Logistic L1") === "Logistic L1";
                            }
                          }
                          return false;
                        })();
                        return (
                          <div key={p.key}>
                            <label style={{ fontSize: 11, fontWeight: 500, color: "var(--foreground)", display: "block", marginBottom: 4 }}>{p.label}</label>
                            {p.type === "select" ? (
                              <details 
                                style={{ 
                                  width: "100%", 
                                  position: "relative",
                                  opacity: isDisabled ? 0.6 : 1,
                                  pointerEvents: isDisabled ? "none" : "auto"
                                }} 
                                data-testid={`details-${model.id}-${p.key}`}
                              >
                                <summary
                                  onClick={e => {
                                    if (isDisabled) {
                                      e.preventDefault();
                                    }
                                  }}
                                  style={{
                                    display: "flex",
                                    justifyContent: "space-between",
                                    alignItems: "center",
                                    padding: "6px 8px",
                                    border: "1px solid var(--border)",
                                    borderRadius: 6,
                                    fontSize: 12,
                                    background: isDisabled ? "#f3f4f6" : "#fff",
                                    cursor: isDisabled ? "not-allowed" : "pointer",
                                    listStyle: "none",
                                    userSelect: "none"
                                  }}
                                  data-testid={`select-${model.id}-${p.key}`}
                                >
                                  <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                                    {isDisabled ? "N/A" : currentValue}
                                  </span>
                                  <span style={{ fontSize: 10, color: "var(--muted-foreground)", marginLeft: 4 }}>▼</span>
                                </summary>

                                <div
                                  style={{
                                    position: "absolute",
                                    top: "100%",
                                    left: 0,
                                    right: 0,
                                    zIndex: 50,
                                    marginTop: 4,
                                    border: "1px solid var(--border)",
                                    borderRadius: 6,
                                    padding: "4px 0",
                                    maxHeight: 160,
                                    overflowY: "auto",
                                    background: "#fff",
                                    boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                                  }}
                                >
                                  {p.options?.map(o => (
                                    <div
                                      key={o}
                                      onClick={e => {
                                        setParam(model.id, p.key, o);
                                        const details = (e.target as HTMLElement).closest("details");
                                        if (details) details.removeAttribute("open");
                                      }}
                                      style={{
                                        padding: "6px 8px",
                                        fontSize: 12,
                                        cursor: "pointer",
                                        background: currentValue === String(o) ? "var(--selected-bg)" : "transparent",
                                        fontWeight: currentValue === String(o) ? 600 : 400
                                      }}
                                      onMouseEnter={e => (e.currentTarget.style.background = "var(--selected-bg)")}
                                      onMouseLeave={e =>
                                        (e.currentTarget.style.background = currentValue === String(o) ? "var(--selected-bg)" : "transparent")
                                      }
                                    >
                                      {o}
                                    </div>
                                  ))}
                                </div>
                              </details>
                            ) : (
                              <input
                                type="number"
                                min={p.min}
                                max={p.max}
                                step={p.step}
                                disabled={isDisabled}
                                value={isDisabled ? "" : rawVal}
                                placeholder={isDisabled ? "N/A" : ""}
                                onChange={e => setParam(model.id, p.key, e.target.value === "" ? "" : Number(e.target.value))}
                                style={{
                                  width: "100%",
                                  padding: "6px 8px",
                                  border: "1px solid var(--border)",
                                  borderRadius: 6,
                                  fontSize: 12,
                                  background: isDisabled ? "#f3f4f6" : "#fff",
                                  cursor: isDisabled ? "not-allowed" : "text"
                                }}
                                data-testid={`input-${model.id}-${p.key}`}
                              />
                            )}
                          </div>
                        );
                      })}
                    </div>
                  </div>
                )}
              </div>
            );
          })}
        </div>

        {fsConfig.selectedModels.length === 0 && (
          <div className="banner warn" style={{ marginTop: 12 }}>⚠ Select at least one model to proceed.</div>
        )}
        {fsConfig.selectedModels.length > 0 && (
          <div className="banner info" style={{ marginTop: 12 }}>
            ✓ <strong>{fsConfig.selectedModels.length} model(s) selected</strong>: {fsConfig.selectedModels.join(", ")}.
            {fsConfig.cvEnabled && " Cross-validation will tune their hyperparameters."}
          </div>
        )}
      </div>

      {fsResults && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16, marginTop: 16 }}>
          {(isCombinedPool ? trainingDatasets.slice(0, 1) : trainingDatasets).map(d => {
            const dsRes = getDatasetResults(d.id);
            if (!dsRes) return null;
            const dsWarnings: string[] = Array.isArray(dsRes.warnings) ? dsRes.warnings : [];
            const displayName = isCombinedPool && !isSingleDataset ? "Pooled Datasets" : d.name;
            return (
              <div key={d.id} style={{ display: "flex", flexDirection: "column", gap: 14 }}>
                {dsWarnings.length > 0 && dsWarnings.map((w, i) => (
                  <div key={i} className="banner warn" style={{ marginTop: 0 }}>⚠ {w}</div>
                ))}
                {dsRes.features && dsRes.features.length > 0 && (
                  <div className="card">
                    <div className="card-title" style={{ marginBottom: 4, fontSize: 13 }}>Selected Features & Importance Rank - {displayName}</div>
                    <div className="card-sub">The machine learning models ranked genes by order of their training feature importance.</div>
                    <hr className="card-divider" />
                    
                    {fsConfig.selectedModels.length > 1 && (
                      <div style={{ display: "flex", gap: 8, marginBottom: 14 }}>
                        {[...fsConfig.selectedModels].sort((a, b) => {
                          const order = ["stabl", "boruta", "gbm", "randomforest", "logistic", "svm"];
                          return order.indexOf(a) - order.indexOf(b);
                        }).map(mId => {
                          const currentTab = selectedModelTab[d.id] || fsConfig.selectedModels[0];
                          const isActive = currentTab === mId;
                          return (
                            <button
                              key={mId}
                              onClick={() => setSelectedModelTab(prev => ({ ...prev, [d.id]: mId }))}
                              style={{
                                padding: "6px 12px",
                                fontSize: 12,
                                fontWeight: 600,
                                borderRadius: 6,
                                border: `1px solid ${isActive ? "var(--primary)" : "var(--border)"}`,
                                background: isActive ? "var(--primary)" : "white",
                                color: isActive ? "white" : "var(--foreground)",
                                cursor: "pointer",
                                outline: "none",
                                transition: "all 0.15s ease"
                              }}
                            >
                              {MODEL_LABELS[mId] || mId}
                            </button>
                          );
                        })}
                      </div>
                    )}

                    {(() => {
                      const currentTab = selectedModelTab[d.id] || fsConfig.selectedModels[0];
                      const isBorutaOrStabl = currentTab === "boruta" || currentTab === "stabl";
                      const showImportance = !isBorutaOrStabl;
                      const showRank = !isBorutaOrStabl;
                      const colSpan = (showRank ? 1 : 0) + 1 + (showImportance ? 1 : 0);
                      return (
                        <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                          <thead>
                            <tr style={{ textAlign: "left", borderBottom: "2px solid var(--border)", color: "var(--muted-foreground)" }}>
                              {showRank && <th style={{ padding: "8px 10px", fontWeight: 600 }}>Rank</th>}
                              <th style={{ padding: "8px 10px", fontWeight: 600 }}>Feature Name</th>
                              {showImportance && (
                                <th style={{ padding: "8px 10px", fontWeight: 600, textAlign: "right" }}>Importance Score</th>
                              )}
                            </tr>
                          </thead>
                          <tbody>
                            {(() => {
                              const featuresToShow = dsRes.model_features?.[currentTab] || dsRes.features || [];
                              if (featuresToShow.length === 0) {
                                  return (
                                    <tr>
                                      <td colSpan={colSpan} style={{ padding: "20px", fontStyle: "italic", textAlign: "center", color: "var(--muted-foreground)" }}>
                                        No features selected by this model.
                                      </td>
                                    </tr>
                                  );
                              }
                              return featuresToShow.slice(0, 10).map((r: any, i: number) => (
                                <tr key={r.gene} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                                  {showRank && <td style={{ padding: "7px 10px", color: "var(--muted-foreground)" }}>#{r.rank}</td>}
                                  <td style={{ padding: "7px 10px", fontWeight: 600 }}>{r.gene}</td>
                                  {showImportance && (
                                    <td style={{ padding: "7px 10px", textAlign: "right" }}>
                                        <div style={{ display: "flex", alignItems: "center", gap: 6, justifyContent: "flex-end" }}>
                                          <div style={{ width: 60, height: 6, borderRadius: 3, background: "var(--border)", overflow: "hidden" }}>
                                            <div style={{ width: `${(Number(Array.isArray(r.importance) ? r.importance[0] : r.importance) || 0) * 100}%`, height: "100%", background: "var(--primary)", borderRadius: 3 }} />
                                          </div>
                                          <span style={{ fontWeight: 600, color: "var(--foreground)" }}>{formatMetric(r.importance, 0, 3)}</span>
                                        </div>
                                    </td>
                                  )}
                                </tr>
                              ));
                            })()}
                          </tbody>
                        </table>
                      );
                    })()}
                  </div>
                )}

                {/* Feature Selection Method card removed from here to render once globally */}

                {/* Only render Model Training Performance card after refitting with selected features (or immediately for STABL/Boruta) */}
                {(isRefitted || isOnlyBorutaStabl) && !hasSelectionMethodChanged && !hasTrainingSettingsChanged && (
                  <div className="card">
                    <div className="card-title" style={{ marginBottom: 4, fontSize: 13 }}>Model Training Performance - {displayName}</div>
                    <div className="card-sub">Performance metrics evaluated on the training validation split.</div>
                    <hr className="card-divider" />

                    <div style={{ display: "grid", gridTemplateColumns: "1fr", gap: 20 }}>
                      {/* ROC Curve Graph */}
                      <div style={{ display: "flex", flexDirection: "column", gap: 8, border: "1px solid var(--border)", borderRadius: 8, padding: 12, background: "var(--muted)" }}>
                        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>ROC Curves</div>
                        {dsRes.roc_plot ? (
                          <div style={{ display: "flex", justifyContent: "center" }}>
                            <img src={`data:image/png;base64,${dsRes.roc_plot}`} style={{ width: 700, height: 400, objectFit: "contain", borderRadius: 8 }} alt="ROC Curves" />
                          </div>
                        ) : (
                          <div style={{ height: 210, display: "flex", alignItems: "center", justifyContent: "center", border: "1px dashed var(--border)", borderRadius: 6, background: "var(--background)" }}>
                            <div style={{ padding: 40, color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>Errors occured. Couldn't generate ROC Curves...</div>
                          </div>
                        )}
                      </div>

                      {/* Metrics Tables */}
                      <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
                        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>Classification Performance Metrics</div>
                        <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                          {[...fsConfig.selectedModels].sort((a, b) => {
                            const order = ["stabl", "boruta", "gbm", "randomforest", "logistic", "svm"];
                            return order.indexOf(a) - order.indexOf(b);
                          }).map(m => {
                            const metrics = dsRes.performance_metrics?.[m] || { auc: 0.5, acc: 0.5, ppv: 0.5, npv: 0.5 };
                            const modelName = MODEL_LABELS[m] || m;
                            return (
                              <div key={m} style={{ padding: "8px 12px", border: "1px solid var(--border)", borderRadius: 8, background: "white" }}>
                                <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 8 }}>
                                  <div style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 12, fontWeight: 600 }}>
                                    <div style={{ width: 6, height: 6, background: MODEL_COLORS[m], borderRadius: "50%" }} />
                                    {modelName}
                                  </div>
                                </div>
                                <div className="chips-row">
                                  <div className="stat-chip">
                                    <div className="stat-chip-val" style={{ color: "var(--primary)" }}>{formatMetric(metrics?.auc, 0.5, 3)}</div>
                                    <div className="stat-chip-lbl">AUC</div>
                                  </div>
                                  <div className="stat-chip" style={{ minWidth: 160 }}>
                                    <div className="stat-chip-val">{formatCi(metrics?.ci, metrics?.ci_lower, metrics?.ci_upper)}</div>
                                    <div className="stat-chip-lbl">95% CI</div>
                                  </div>
                                  {/* NOTE: The backend reports Balanced Accuracy in metrics.acc to prevent bias on imbalanced datasets */}
                                  <div className="stat-chip">
                                    <div className="stat-chip-val">{formatMetric(metrics?.acc, 0.5, 3)}</div>
                                    <div className="stat-chip-lbl">Balanced Accuracy</div>
                                  </div>
                                  <div className="stat-chip">
                                    <div className="stat-chip-val">{formatMetric(metrics?.ppv, 0.5, 3)}</div>
                                    <div className="stat-chip-lbl">PPV</div>
                                  </div>
                                  <div className="stat-chip">
                                    <div className="stat-chip-val">{formatMetric(metrics?.npv, 0.5, 3)}</div>
                                    <div className="stat-chip-lbl">NPV</div>
                                  </div>
                                </div>
                              </div>
                            );
                          })}
                        </div>

                        {/* Performance metrics explanation card */}
                        <div style={{
                          marginTop: 4, padding: "10px 12px",
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
                          marginTop: 4, padding: "10px 12px",
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
                )}
              </div>
            );
          })}
          {showFeatureSelectionMethodCard && (
            <div
              className="card"
              style={{
                marginTop: 14,
                opacity: hasTrainingSettingsChanged ? 0.5 : 1,
                pointerEvents: hasTrainingSettingsChanged ? "none" : "auto",
                transition: "opacity 0.2s ease"
              }}
            >
              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 4 }}>
                <div className="card-title" style={{ fontSize: 13 }}>Feature Selection Method</div>
                {hasTrainingSettingsChanged && (
                  <span style={{ fontSize: 11, color: "var(--warning-text)", background: "var(--warning-bg)", border: "1px solid var(--warning-border)", padding: "2px 8px", borderRadius: 4, fontWeight: 500 }}>
                    Model settings changed — Redo training first
                  </span>
                )}
              </div>
              <div className="card-sub">Select how the final subset of features should be determined for model retraining.</div>
              <hr className="card-divider" />
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                
                {/* Option 1: Break-off Point */}
                <div
                  style={{
                    border: `1px solid ${selectionMethod === "breakoff" ? "var(--primary)" : "var(--border)"}`,
                    borderRadius: 10,
                    background: selectionMethod === "breakoff" ? "var(--selected-bg)" : "white",
                    overflow: "hidden",
                    transition: "all .15s",
                  }}
                >
                  <div style={{ display: "flex", flexDirection: "column", padding: "12px 14px" }}>
                    <div
                      style={{ display: "flex", alignItems: "center", gap: 12, cursor: "pointer" }}
                      onClick={() => { setSelectionMethod("breakoff"); setIsRefitted(false); }}
                    >
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 2 }}>
                          Break-off Point
                        </div>
                        <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
                          Identify the inflection point of the importance score curve (using the segmented R package).
                        </div>
                      </div>
                      {selectionMethod === "breakoff" && (
                        <span style={{
                          fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                          background: "hsl(150 50% 93%)", color: "hsl(150 58% 28%)",
                          border: "1px solid hsl(150 35% 76%)",
                        }}>Selected</span>
                      )}
                    </div>
                  </div>
                </div>

                {/* Option 2: Percentage Threshold */}
                <div
                  style={{
                    border: `1px solid ${selectionMethod === "percentage" ? "var(--primary)" : "var(--border)"}`,
                    borderRadius: 10,
                    background: selectionMethod === "percentage" ? "var(--selected-bg)" : "white",
                    overflow: "hidden",
                    transition: "all .15s",
                  }}
                >
                  <div style={{ display: "flex", flexDirection: "column", padding: "12px 14px" }}>
                    <div
                      style={{ display: "flex", alignItems: "center", gap: 12, cursor: "pointer" }}
                      onClick={() => { setSelectionMethod("percentage"); setIsRefitted(false); }}
                    >
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 2 }}>
                          Percentage Threshold
                        </div>
                        <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
                          Select features capturing a percentage of the total importance score.
                        </div>
                      </div>
                      {selectionMethod === "percentage" && (
                        <span style={{
                          fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                          background: "hsl(150 50% 93%)", color: "hsl(150 58% 28%)",
                          border: "1px solid hsl(150 35% 76%)",
                        }}>Selected</span>
                      )}
                    </div>
                    {selectionMethod === "percentage" && (
                      <div style={{ marginTop: 10, display: "flex", alignItems: "center", gap: 8 }}>
                        <span style={{ fontSize: 12, fontWeight: 500 }}>Percentage (1-100):</span>
                        <input
                          type="number"
                          min={1}
                          max={100}
                          placeholder="80"
                          value={percentageValue}
                          onChange={(e) => { setPercentageValue(Number(e.target.value)); setIsRefitted(false); }}
                          style={{ width: 80, padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12 }}
                        />
                        <span style={{ fontSize: 12, color: "var(--muted-foreground)" }}>% of importance score</span>
                      </div>
                    )}
                  </div>
                </div>

                {/* Option 3: Maximum Features */}
                <div
                  style={{
                    border: `1px solid ${selectionMethod === "max_features" ? "var(--primary)" : "var(--border)"}`,
                    borderRadius: 10,
                    background: selectionMethod === "max_features" ? "var(--selected-bg)" : "white",
                    overflow: "hidden",
                    transition: "all .15s",
                  }}
                >
                  <div style={{ display: "flex", flexDirection: "column", padding: "12px 14px" }}>
                    <div
                      style={{ display: "flex", alignItems: "center", gap: 12, cursor: "pointer" }}
                      onClick={() => { setSelectionMethod("max_features"); setIsRefitted(false); }}
                    >
                      <div style={{ flex: 1, minWidth: 0 }}>
                        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 2 }}>
                          Maximum Features
                        </div>
                        <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
                          Select a fixed maximum number of top features.
                        </div>
                      </div>
                      {selectionMethod === "max_features" && (
                        <span style={{
                          fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                          background: "hsl(150 50% 93%)", color: "hsl(150 58% 28%)",
                          border: "1px solid hsl(150 35% 76%)",
                        }}>Selected</span>
                      )}
                    </div>
                    {selectionMethod === "max_features" && (
                      <div style={{ marginTop: 10, display: "flex", alignItems: "center", gap: 8 }}>
                        <span style={{ fontSize: 12, fontWeight: 500 }}>Maximum count:</span>
                        <input
                          type="number"
                          min={1}
                          max={200}
                          placeholder="10"
                          value={maxFeaturesValue}
                          onChange={(e) => { setMaxFeaturesValue(Number(e.target.value)); setIsRefitted(false); }}
                          style={{ width: 80, padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12 }}
                        />
                        <span style={{ fontSize: 12, color: "var(--muted-foreground)" }}>features</span>
                      </div>
                    )}
                  </div>
                </div>

                {/* Option 4: Overlap between models */}
                {hasOverlapOption && (
                  <div
                    style={{
                      border: `1px solid ${selectionMethod === "overlap" ? "var(--primary)" : "var(--border)"}`,
                      borderRadius: 10,
                      background: selectionMethod === "overlap" ? "var(--selected-bg)" : "white",
                      overflow: "hidden",
                      transition: "all .15s",
                    }}
                  >
                    <div style={{ display: "flex", flexDirection: "column", padding: "12px 14px" }}>
                      <div
                        style={{ display: "flex", alignItems: "center", gap: 12, cursor: "pointer" }}
                        onClick={() => { setSelectionMethod("overlap"); setIsRefitted(false); }}
                      >
                        <div style={{ flex: 1, minWidth: 0 }}>
                          <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 2 }}>
                            Overlap between models
                          </div>
                          <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
                            Select features that are consistently chosen by all trained models (intersection of Elbow Point selections).
                          </div>
                        </div>
                        {selectionMethod === "overlap" && (
                          <span style={{
                            fontSize: 10, fontWeight: 700, padding: "2px 8px", borderRadius: 4,
                            background: "hsl(150 50% 93%)", color: "hsl(150 58% 28%)",
                            border: "1px solid hsl(150 35% 76%)",
                          }}>Selected</span>
                        )}
                      </div>
                    </div>
                  </div>
                )}
              </div>
            </div>
          )}
        </div>
      )}

      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="fs"
          datasetIds={state.fsDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={() => {
            setShowDiscardModal(false);
            runRedoAction(true);
          }}
        />
      )}

      <div className="action-row">
        <button
          className="btn btn-default"
          onClick={() => {
            const multipleDs = state.fsDatasets.length > 1;
            const isViewingAll = state.fsSelectedContext === "all" && multipleDs;
            dispatch({ type: "FS_SET_STEP", step: isViewingAll ? "all-datasets" : "upload" });
          }}
        >
          ← Back
        </button>

        {/* Level 1: Redo Model Training (overrules feature selection redo) */}
        {(state.fsTrainingResultsCache || fsResults) && hasTrainingSettingsChanged && (
          <button
            className="btn btn-default"
            style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }}
            onClick={handleRedo}
            data-testid="btn-redo-model-selection"
          >
            🔄 Redo Model Training
          </button>
        )}

        {/* Level 2: Redo Selecting Features (only when training settings have NOT changed and models have rank) */}
        {!hasTrainingSettingsChanged && fsResults && hasAnyRankModel && hasSelectionMethodChanged && (
          <button
            className="btn btn-default"
            style={{ borderColor: "var(--primary)", color: "var(--primary)" }}
            disabled={isRefitting || loadingTraining}
            onClick={handleRefit}
            data-testid="btn-redo-selecting-features"
          >
            {isRefitting ? "Refitting Features…" : "🔄 Redo Selecting Features"}
          </button>
        )}

        <button
          className="btn btn-primary"
          disabled={fsConfig.selectedModels.length === 0 || isRefitting || loadingTraining || !!allocationError}
          onClick={handleContinue}
          data-testid="btn-continue-model-selection"
        >
          {loadingTraining
            ? "Training Models…"
            : isRefitting
            ? "Refitting Features…"
            : !fsResults
            ? "Train Selected Models"
            : hasTrainingSettingsChanged
            ? getSubmittedContinueLabel()
            : hasAnyRankModel && (!isRefitted || hasSelectionMethodChanged)
            ? "Refit Models on Selected Features"
            : (fsConfig.cvEnabled || hasCvOnly)
            ? "Continue to Cross Validation"
            : hasAnyTesting
            ? "Continue to Testing →"
            : "Continue to Export →"}
        </button>
      </div>
    </>
  );
}
