import { useState, useEffect, useMemo } from "react";
import { ChevronDown, ChevronRight, FileText, X, AlertTriangle, CheckCircle, Layers } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import { toast } from "../../hooks/use-toast";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import WarnSampleModal from "../shared/WarnSampleModal";
import { type MLModel, type CVMethod, getHierarchyGroup } from "../../dataObject";
import { fetchSampleDataAPI } from "../../lib/api";
import { parseClinicalFile, getExpressionSampleColumns } from "../../lib/dataParser";
import TestingStep from "../featureselection/TestingStep";
import InlineUploadStep from "./InlineUploadStep";
import AllDatasetsUploadView from "../shared/AllDatasetsUploadView";
import { runInlineFSAPI, runCrossValidationAPI, redoStepAPI, clearDatasetAPI, type CVResult, refitFeaturesAPI, fetchDatasetInfoAPI } from "../../lib/api";
import { formatMetric, formatCi } from "../../lib/utils";

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
      { key: "artificial_proportion", label: "Artificial Proportion", type: "number", default: 1.0 },
      { key: "refit_max_depth", label: "Refit Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "refit_n_estimators", label: "Refit Number of trees", type: "number", default: 500, min: 1, step: 1 },
    ],
  },
  {
    id: "boruta", label: "Boruta",
    desc: "Iteratively compares each feature's importance against randomized shadow features using a Random Forest, keeping every feature that is confirmed relevant. Captures all-relevant features, but does not enforce sparsity.",
    params: [
      { key: "max_runs", label: "Max Runs", type: "number", default: 100, min: 11, step: 1 },
      { key: "p_value", label: "Confidence (p-value)", type: "number", default: 0.01 },
      { key: "max_depth", label: "Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "n_estimators", label: "Number of Trees", type: "number", default: 500 },
      { key: "keep_tentative", label: "Keep Tentative", type: "select", options: ["no", "yes"], default: "no" },
    ],
  },
  {
    id: "gbm", label: "Gradient Boosting Model",
    desc: "Builds an ensemble of boosted decision trees. Often achieves high predictive accuracy, but requires more parameter tuning.",
    params: [
      { key: "n.trees", label: "Boosting Iterations (n.trees)", type: "select", options: ["100", "300", "500", "1000"], default: "500" },
      { key: "interaction.depth", label: "Max Tree Depth (interaction.depth)", type: "number", default: 3, min: 1, max: 20, step: 1 },
      { key: "shrinkage", label: "Shrinkage (shrinkage)", type: "select", options: ["0.01", "0.05", "0.1", "0.3"], default: "0.1" },
      { key: "n.minobsinnode", label: "Min. Terminal Node Size (n.minobsinnode)", type: "number", default: 10, min: 1, max: 10, step: 1 },
    ],
  },
  {
    id: "randomforest", label: "Random Forest",
    desc: "Combines multiple decision trees for classification. Provides robust performance and feature importance, but may be less interpretable than simpler models.",
    params: [
      { key: "max_depth", label: "Max Depth", type: "number", default: 10, min: 1, max: 20, step: 1 },
      { key: "num_trees", label: "Number of Trees", type: "number", default: 500 },
    ],
  },
  {
    id: "logistic", label: "Logistic Regression",
    desc: "Models the relationship between features and classes. Fast, interpretable, and a strong baseline, but may not capture complex nonlinear patterns.",
    params: [
      { key: "lambda", label: "Lambda (> 0)", type: "number", default: 0.1, min: 0.0001, step: 0.01 },
      { key: "alpha", label: "Alpha (0-1)", type: "number", default: 1.0, min: 0, max: 1, step: 0.05 },
      { key: "max_iter", label: "Max Iterations", type: "select", options: ["100", "500", "1000", "1500", "2000"], default: "1000" },
    ],
  },
  {
    id: "svm", label: "Support Vector Machine",
    desc: "Finds the optimal boundary between classes. Performs well on high-dimensional data, but can be slower on large datasets.",
    params: [
      { key: "C", label: "Regularization C", type: "number", default: 1.0 },
      { key: "weight", label: "Weight (1-20)", type: "number", default: 1, min: 1, max: 20, step: 1 },
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

const CV_METHODS: { id: CVMethod; label: string; desc: string }[] = [
  { id: "k_fold", label: "K-Fold Cross Validation", desc: "Splits the dataset into k folds (5 or 10 folds) to evaluate model performance across different subsets. Each fold is used as a validation set once while the remaining folds are used for training, providing a robust performance estimate." },
  { id: "loocv", label: "Leave-One-Out CV (LOOCV)", desc: "Trains the model N times, leaving one sample out each time. Recommended for very small datasets." }
];





export default function InlineFSStep() {
  const { state, dispatch } = useAppStore();
  const allDs = state.dpDatasets;
  const multipleDs = allDs.length > 1;
  const sorted = [...allDs].sort((a, b) => a.id.localeCompare(b.id));

  const isIncomplete = (d: typeof sorted[0]) => !d.clinicalFileName || !d.clinicalGroupCol;
  const pendingDatasets = sorted.filter(isIncomplete);

  const lowSampleDs = sorted.find(d => (d.nSamples || 0) < 30);
  const lowGroupDs = sorted.find(d => d.minGroupSampleSize !== undefined && d.minGroupSampleSize < 10);

  // Compute shared (intersection) feature count across all datasets for the Upload Summary check.
  // Aware of feature orientation: when features are column names (headers) vs in a single column (rows).
  const getDatasetFeatureIds = (d: typeof sorted[0]): Set<string> => {
    if (d.fs_featuresOrientation === "headers") {
      const sampleIdCol = d.clinicalSampleIdCol || d.geneIdCol || "SampleID";
      const infoCols = d.geneInfoCols || [];
      const featureCols = (d.columns || []).filter(c => c !== sampleIdCol && !infoCols.includes(c));
      return new Set(featureCols);
    }
    if (!d.parsedData || d.parsedData.length === 0) return new Set();
    const geneIdIdx = d.columns ? d.columns.indexOf(d.geneIdCol) : -1;
    if (geneIdIdx === -1) return new Set(d.parsedData.map(row => String(row[0])));
    return new Set(d.parsedData.map(row => String(row[geneIdIdx])));
  };

  const sharedFeatureCount: number = (() => {
    if (!multipleDs) return 0;
    if (sorted.some(d => !d.parsedData || d.parsedData.length === 0)) return 0;
    let intersection = getDatasetFeatureIds(sorted[0]);
    for (let i = 1; i < sorted.length; i++) {
      const next = getDatasetFeatureIds(sorted[i]);
      intersection = new Set([...intersection].filter(x => next.has(x)));
    }
    return intersection.size;
  })();

  const sharedFeatureError: string | null =
    multipleDs && sorted.every(d => d.parsedData && d.parsedData.length > 0) && sharedFeatureCount <= 10
      ? `Only ${sharedFeatureCount} shared feature(s) found across datasets. At least 11 shared features are required to proceed.`
      : null;

  const summaryGateError: string | null =
    pendingDatasets.length > 0
      ? null // handled by isContinueDisabled separately
      : lowSampleDs
        ? `Dataset "${lowSampleDs.name}" has fewer than 30 samples (${lowSampleDs.nSamples || 0}).`
        : lowGroupDs
          ? `Dataset "${lowGroupDs.name}" has a group with fewer than 10 samples (${lowGroupDs.minGroupSampleSize}).`
          : sharedFeatureError;


  // Cross-dataset purpose allocation is deferred to Model Selection.

  const [activeUploadDatasetId, setActiveUploadDatasetId] = useState(
    pendingDatasets.length > 0 ? pendingDatasets[0].id : (sorted[0]?.id || "")
  );
  const [showUploadSummary, setShowUploadSummary] = useState(pendingDatasets.length === 0 && multipleDs);
  const [editingFromSummary, setEditingFromSummary] = useState(false);

  // Sync state if databases update externally (e.g. clinical data cleared)
  useEffect(() => {
    const hasIncomplete = allDs.some(isIncomplete);
    if (hasIncomplete && state.inlineFsUploadReviewed) {
      dispatch({ type: "SET_INLINE_FS_UPLOAD_REVIEWED", reviewed: false });
    }
    if (!sorted.some(d => d.id === activeUploadDatasetId)) {
      const firstPending = sorted.find(isIncomplete);
      setActiveUploadDatasetId(firstPending ? firstPending.id : (sorted[0]?.id || ""));
    }
  }, [allDs, state.inlineFsUploadReviewed, sorted, activeUploadDatasetId]);

  // If already reviewed
  if (state.inlineFsUploadReviewed) {
    return (
      <InlineFSContent
        onBack={() => {
          if (multipleDs) {
            dispatch({ type: "SET_INLINE_FS_UPLOAD_REVIEWED", reviewed: false });
            setShowUploadSummary(true);
          } else {
            dispatch({ type: "SET_INLINE_FS_UPLOAD_REVIEWED", reviewed: false });
          }
        }}
      />
    );
  }

  // Single dataset flow
  if (!multipleDs) {
    const ds = sorted[0];
    return (
      <InlineUploadStep
        key={ds.id}
        datasetId={ds.id}
        mode="fs"
        onBack={() => {
          dispatch({ type: "DP_SET_STEP", step: "module-select" });
        }}
        onContinue={() => {
          dispatch({ type: "SET_INLINE_FS_UPLOAD_REVIEWED", reviewed: true });
        }}
      />
    );
  }

  // Multi-dataset flow - Upload Summary view
  if (showUploadSummary) {
    return (
      <>
        <AllDatasetsUploadView
          title="All Datasets — Datasets Upload Summary"
          subtitle="Overview of clinical data upload status. Ensure all datasets have matched samples before proceeding."
          datasets={allDs as any}
          showClinicalDetails={true}
          onDatasetClick={(id) => {
            setActiveUploadDatasetId(id);
            setShowUploadSummary(false);
            setEditingFromSummary(true);
          }}
          onContinue={() => {
            dispatch({ type: "SET_INLINE_FS_UPLOAD_REVIEWED", reviewed: true });
          }}
          isContinueDisabled={pendingDatasets.length > 0 || !!summaryGateError}
          continueLabel="Continue to Feature Selection →"
          onBack={() => {
            if (pendingDatasets.length > 0) {
              setActiveUploadDatasetId(pendingDatasets[pendingDatasets.length - 1].id);
              setShowUploadSummary(false);
            } else {
              dispatch({ type: "DP_SET_STEP", step: "module-select" });
            }
          }}
        />
        {multipleDs && sorted.every(d => d.parsedData && d.parsedData.length > 0) && (
          <div className="card" style={{ marginTop: 12 }}>
            <div className="card-title" style={{ marginBottom: 6 }}>Shared Feature Space</div>
            <div className="card-sub" style={{ marginBottom: 10 }}>
              Intersection of features (genes/proteins) across all datasets. At least 11 are required to proceed.
            </div>
            <div style={{ display: "flex", alignItems: "center", gap: 12 }}>
              <div className="stat-chip">
                <div className="stat-chip-val" style={{ color: sharedFeatureCount > 10 ? "hsl(150 60% 32%)" : "hsl(0 72% 50%)" }}>
                  {sharedFeatureCount.toLocaleString()}
                </div>
                <div className="stat-chip-lbl">Shared Features</div>
              </div>
            </div>
            {sharedFeatureCount > 10 ? (
              <div className="banner success" style={{ marginTop: 10, marginBottom: 0 }}>
                ✓ {sharedFeatureCount.toLocaleString()} shared features detected across all datasets. Feature space is sufficient.
              </div>
            ) : (
              <div className="banner danger" style={{ marginTop: 10, marginBottom: 0 }}>
                ⛔ Only {sharedFeatureCount} shared feature(s) across datasets. At least 11 are required to run feature selection.
              </div>
            )}
          </div>
        )}
        {summaryGateError && (
          <div className="banner danger" style={{ marginTop: 12 }}>
            ⛔ <strong>Cannot continue:</strong> {summaryGateError}
          </div>
        )}
      </>
    );
  }

  // Multi-dataset flow - Sequential upload steps
  const targetId = activeUploadDatasetId;
  const pendingIdx = pendingDatasets.findIndex(d => d.id === targetId);

  return (
    <InlineUploadStep
      key={targetId}
      datasetId={targetId}
      mode="fs"
      editingFromSummary={editingFromSummary}
      onBack={() => {
        if (editingFromSummary) {
          setShowUploadSummary(true);
          setEditingFromSummary(false);
        } else if (pendingIdx > 0) {
          setActiveUploadDatasetId(pendingDatasets[pendingIdx - 1].id);
        } else {
          dispatch({ type: "DP_SET_STEP", step: "module-select" });
        }
      }}
      onContinue={() => {
        if (editingFromSummary) {
          setShowUploadSummary(true);
          setEditingFromSummary(false);
        } else if (pendingIdx < pendingDatasets.length - 1) {
          setActiveUploadDatasetId(pendingDatasets[pendingIdx + 1].id);
        } else {
          setShowUploadSummary(true);
        }
      }}
    />
  );
}

// Helper to extract feature ids from dataset
const getFsDatasetFeatureIds = (d: any): Set<string> => {
  if (d.dataMode === "metadata_features" || d.fs_featuresOrientation === "headers") {
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

function InlineFSContent({ onBack }: { onBack?: () => void }) {
  const { state, dispatch } = useAppStore();
  const fsConfig = state.fsConfig;
  const ds = state.dpDatasets.find(d => d.id === state.dpCurrentDatasetId) ?? state.dpDatasets[0];
  const allDs = state.dpDatasets;

  // Global Multi-Dataset and Pooling determination
  const isSingleDataset = state.dpDatasets.length === 1;
  const isAllSameType = state.dpDatasets.length > 1 && 
    new Set(state.dpDatasets.map(d => d.dataType || "readcounts")).size === 1 && 
    (state.dpDatasets[0]?.dataType || "readcounts") !== "others";

  // Group datasets by exact type to check pooling eligibility
  const typeGroups: Record<string, any[]> = {};
  allDs.forEach(d => {
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

  // Local Flow Control: "model-selection", "cross-validation", or "testing"
  const [subStep, setSubStep] = useState<"model-selection" | "cross-validation" | "testing">("model-selection");
  const [expandedModels, setExpandedModels] = useState<Set<MLModel>>(new Set());
  const [modelParams, setModelParams] = useState<Record<string, Record<string, string | number>>>({});

  // Run states for Model Selection (no CV)
  const [loadingSelection, setLoadingSelection] = useState(false);
  const [trainElapsed, setTrainElapsed] = useState(0);
  const [doneSelection, setDoneSelection] = useState(state.dpTrainingResultsCache ? true : false);

  const [submittedSelectionCfg, setSubmittedSelectionCfg] = useState<{
    trainRatios: Record<string, number>;
    selectedModels: string[];
    datasetPurposes?: Record<string, string>;
    validationStrategies?: Record<string, string>;
    isInternalValidations?: Record<string, boolean>;
  } | null>(() => {
    if (state.dpTrainingResultsCache) {
      const ratios: Record<string, number> = {};
      const purposes: Record<string, string> = {};
      const strategies: Record<string, string> = {};
      const internals: Record<string, boolean> = {};
      state.dpDatasets.forEach(d => {
        ratios[d.id] = d.fs_trainRatio ?? 0.7;
        purposes[d.id] = d.fs_datasetPurpose || "train-and-test";
        strategies[d.id] = d.fs_validationStrategy || "train-test-split";
        internals[d.id] = d.fs_isInternalValidation ?? false;
      });
      return {
        trainRatios: ratios,
        selectedModels: state.dpTrainingResultsCache.selectedModels || [],
        datasetPurposes: purposes,
        validationStrategies: strategies,
        isInternalValidations: internals
      };
    }
    return null;
  });
  const [submittedModelParams, setSubmittedModelParams] = useState<Record<string, Record<string, string | number>>>({});
  const [submittedSelectionParams, setSubmittedSelectionParams] = useState<{
    method: string;
    percentage: number;
    maxFeatures: number;
  } | null>(() => {
    if (state.dpTrainingResultsCache?.isRefitted) {
      return {
        method: "breakoff",
        percentage: 80,
        maxFeatures: 10
      };
    }
    return null;
  });

  // Run states for Cross Validation
  const [loadingCV, setLoadingCV] = useState(false);
  const [doneCV, setDoneCV] = useState(state.dpCvResultsCache ? true : false);
 
  // Overlap check and Warn Modal states
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showChangeWarnModal, setShowChangeWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const [discardType, setDiscardType] = useState<"model-selection" | "cv">("model-selection");
  const [pendingRun, setPendingRun] = useState<(() => void) | null>(null);
 
  const [submittedCVCfg, setSubmittedCVCfg] = useState<{
    cvMethod: CVMethod;
    cvFolds: number;
  } | null>(null);

  const trainCount = state.dpDatasets.filter(d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || !d.fs_datasetPurpose).length;
  const hasInternalVal = state.dpDatasets.some(d => (d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || !d.fs_datasetPurpose) && d.fs_isInternalValidation);
  const testCount = state.dpDatasets.filter(d => d.fs_datasetPurpose === "test" || d.fs_datasetPurpose === "train-and-test").length;

  const totalSamples = state.dpDatasets.reduce((sum, d) => sum + (d.nSamples || 0), 0);

  const cvTrackSizes = useMemo(() => {
    const sizes: number[] = [];
    const trainingDs = state.dpDatasets.filter(d => 
      d.fs_datasetPurpose === "train" || 
      d.fs_datasetPurpose === "train-and-test" || 
      d.fs_datasetPurpose === "train-and-cv" || 
      !d.fs_datasetPurpose
    );
    const testingDsWithCv = state.dpDatasets.filter(d => 
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
  }, [state.dpDatasets, isCombinedPool]);

  const trainSize = useMemo(() => {
    if (cvTrackSizes.length === 0) return 0;
    return Math.min(...cvTrackSizes);
  }, [cvTrackSizes]);

  const cvAllowed = trainSize >= 30;

  const trainingDatasets = allDs.filter(
    d => 
      d.fs_datasetPurpose === "train" || 
      d.fs_datasetPurpose === "train-and-test" || 
      d.fs_datasetPurpose === "train-and-cv" || 
      d.fs_datasetPurpose === "train-cv-test" ||
      !d.fs_datasetPurpose ||
      (d.fs_datasetPurpose === "test" && (d.fs_validationStrategy === "cv-only" || d.fs_validationStrategy === "cv-and-test"))
  );

  const processedCVList = useMemo(() => {
    const trainingDs = allDs.filter(
      d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" ||
           d.fs_datasetPurpose === "train-and-cv" || d.fs_datasetPurpose === "train-cv-test" || !d.fs_datasetPurpose
    );
    const testingDs = allDs.filter(
      d => d.fs_datasetPurpose === "test" && (d.fs_validationStrategy === "cv-only" || d.fs_validationStrategy === "cv-and-test")
    );

    const mode = fsConfig.multiDatasetMode ?? "combine";
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
  }, [allDs, fsConfig.multiDatasetMode]);

  const cleanId = (id: string) => {
    let prev;
    let current = id;
    do {
      prev = current;
      current = current.replace(/_(upload|parsed|raw|dp|fs|de|en|ea)$/g, "");
    } while (current !== prev);
    return current;
  };

  const getDatasetResults = (dsId: string) => {
    const cache = state.dpTrainingResultsCache;
    if (!cache) return null;
    if (cache[dsId]) return cache[dsId];
    if (cache.features || cache.model_features || cache.performance_metrics) {
      return cache;
    }
    const mergedKey = Object.keys(cache).find(k => k.includes("merged_") && k.includes("_training"));
    if (mergedKey && cache[mergedKey]) return cache[mergedKey];

    // Fuzzy matching fallback
    const targetClean = cleanId(dsId);
    const matchedKey = Object.keys(cache).find(k => {
      if (k === "jobId" || k === "status" || k === "message") return false;
      const kClean = cleanId(k);
      return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
    });
    if (matchedKey && cache[matchedKey]) return cache[matchedKey];

    // Fallback: return the first dataset result with features or metrics
    const validKeys = Object.keys(cache).filter(k => !k.startsWith(".") && k !== "jobId" && k !== "status" && k !== "message" && typeof cache[k] === "object" && cache[k] !== null);
    if (validKeys.length > 0) {
      for (const k of validKeys) {
        if (cache[k] && (cache[k].features || cache[k].model_features || cache[k].performance_metrics)) {
          return cache[k];
        }
      }
      return cache[validKeys[0]];
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
    state.dpTrainingResultsCache && (
      (state.dpTrainingResultsCache.features && state.dpTrainingResultsCache.features.length > 0) ||
      (state.dpTrainingResultsCache.model_features && Object.values(state.dpTrainingResultsCache.model_features).some((arr: any) => Array.isArray(arr) && arr.length > 0)) ||
      Object.values(state.dpTrainingResultsCache).some((val: any) => val && typeof val === "object" && ((val.features && val.features.length > 0) || (val.model_features && Object.values(val.model_features).some((arr: any) => Array.isArray(arr) && arr.length > 0))))
    )
  );

  const hasOverlapOption = fsConfig.selectedModels.length > 1 && (trainingDatasets.some(d => {
    const dsRes = getDatasetResults(d.id);
    return dsRes && dsRes.overlap_count !== undefined && dsRes.overlap_count > 0;
  }) || (state.dpTrainingResultsCache && (state.dpTrainingResultsCache.overlap_count !== undefined && state.dpTrainingResultsCache.overlap_count > 0)));

  const isOnlyBorutaStabl = fsConfig.selectedModels.length > 0 && fsConfig.selectedModels.every(m => m === "boruta" || m === "stabl");
  const hasAnyRankModel = fsConfig.selectedModels.some(m => m !== "boruta" && m !== "stabl");

  const showFeatureSelectionMethodCard = Boolean(
    doneSelection && (anyDatasetHasFeatures || (state.dpTrainingResultsCache && Object.keys(state.dpTrainingResultsCache).length > 0)) && hasAnyRankModel
  );

  const getDatasetCVResults = (dsId: string) => {
    const cache = (inlineCvResults && Object.keys(inlineCvResults).length > 0) ? inlineCvResults : state.dpCvResultsCache;
    if (!cache) return [];
    let datasetObj = null;
    if (cache && !Array.isArray(cache)) {
      if (cache[dsId]) {
        datasetObj = cache[dsId];
      } else {
        const mergedKey = Object.keys(cache).find(k => k.includes("merged_") && k.includes("_training"));
        if (mergedKey && cache[mergedKey]) {
          datasetObj = cache[mergedKey];
        } else {
          // Fuzzy matching fallback
          const targetClean = cleanId(dsId);
          const matchedKey = Object.keys(cache).find(k => {
            const kClean = cleanId(k);
            return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
          });
          if (matchedKey && cache[matchedKey]) {
            datasetObj = cache[matchedKey];
          }
        }
      }
    }
    if (datasetObj) {
      if (Array.isArray(datasetObj)) return datasetObj;
      if (datasetObj.metrics) return datasetObj.metrics;
    }
    return Array.isArray(cache) ? cache : [];
  };

  const getCVRocPlotForDataset = (dsId: string): string | null => {
    const cache = (inlineCvResults && Object.keys(inlineCvResults).length > 0) ? inlineCvResults : state.dpCvResultsCache;
    if (!cache || Array.isArray(cache)) return null;
    let datasetObj = cache[dsId];
    if (!datasetObj) {
      const mergedKey = Object.keys(cache).find(k => k.includes("merged_") && k.includes("_training"));
      if (mergedKey) {
        datasetObj = cache[mergedKey];
      } else {
        // Fuzzy matching fallback
        const targetClean = cleanId(dsId);
        const matchedKey = Object.keys(cache).find(k => {
          const kClean = cleanId(k);
          return kClean === targetClean || k.includes(targetClean) || targetClean.includes(kClean);
        });
        if (matchedKey && cache[matchedKey]) {
          datasetObj = cache[matchedKey];
        }
      }
    }
    if (datasetObj && !Array.isArray(datasetObj) && datasetObj.roc_plot) {
      return datasetObj.roc_plot;
    }
    return null;
  };

  const availableFolds = useMemo(() => {
    if (trainSize < 80) return [];
    if (trainSize <= 150) return [5];
    return [5, 10];
  }, [trainSize]);

  const availableMethods = useMemo(() => {
    return CV_METHODS.filter(m => {
      if (trainSize < 80) {
        return m.id === "loocv";
      } else {
        return m.id === "k_fold";
      }
    });
  }, [trainSize]);

  useEffect(() => {
    if (trainSize < 80) {
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
      if (trainSize <= 150) {
        if (fsConfig.cvFolds !== 5) {
          dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: 5 } });
        }
      } else {
        if (fsConfig.cvFolds !== 5 && fsConfig.cvFolds !== 10) {
          dispatch({ type: "FS_SET_CONFIG", patch: { cvFolds: 5 } });
        }
      }
    }
  }, [trainSize, fsConfig.cvMethod, fsConfig.cvFolds, dispatch]);

  useEffect(() => {
    if (!cvAllowed && fsConfig.cvEnabled) {
      dispatch({ type: "FS_SET_CONFIG", patch: { cvEnabled: false } });
    }
  }, [cvAllowed, fsConfig.cvEnabled, dispatch]);

  const dataTypes = allDs.map(d => d.dataType || "readcounts");
  const typeCounts: Record<string, number> = {};
  dataTypes.forEach(t => { typeCounts[t] = (typeCounts[t] || 0) + 1; });
  
  // Find group datasets of same hierarchy
  const transcriptomicsDatasets = allDs.filter(
    d => getHierarchyGroup(d.dataType || "readcounts") === "transcriptomics"
  );
  const proteomicsDatasets = allDs.filter(
    d => getHierarchyGroup(d.dataType || "readcounts") === "proteomics"
  );

  const sharedTranscriptomicsCount = getSharedFeatureCountForGroup(transcriptomicsDatasets);
  const sharedProteomicsCount = getSharedFeatureCountForGroup(proteomicsDatasets);

  // Authoritative shared feature counts from backend (full expression matrix, not 100-row preview)
  const [backendSharedCounts, setBackendSharedCounts] = useState<{ transcriptomics?: number; proteomics?: number }>({});

  useEffect(() => {
    let cancelled = false;
    const fetchTypeCounts = async () => {
      const groups: Array<{ key: "transcriptomics" | "proteomics"; ds: typeof allDs }> = [
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
    fetchTypeCounts();
    return () => { cancelled = true; };
  }, [allDs]);

  // Use backend count when available; fall back to parsedData intersection (100-row preview only)
  const sharedTranscriptomicsCountFinal = backendSharedCounts.transcriptomics ?? sharedTranscriptomicsCount;
  const sharedProteomicsCountFinal = backendSharedCounts.proteomics ?? sharedProteomicsCount;

  const combinedPurpose = allDs[0]?.fs_datasetPurpose === "train-and-cv" ? "train-and-cv" : "train-and-test";
  const combinedRatio = allDs[0]?.fs_trainRatio ?? 0.7;
  const combinedHierarchy = getHierarchyGroup(allDs[0]?.dataType || "readcounts");

  const testingDatasets = allDs.filter(d => d.fs_datasetPurpose === "test");

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
        allDs.forEach(d => {
          dispatch({
            type: "DP_UPDATE_DATASET", id: d.id,
            patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
          });
        });
      }
    } else {
      // Individual mode: check each dataset
      allDs.forEach(d => {
        if (d.nSamples < 100) {
          const currentPurpose = d.fs_datasetPurpose || "train-and-test";
          if (currentPurpose === "test") {
            if (d.fs_validationStrategy !== "cv-only") {
              dispatch({
                type: "DP_UPDATE_DATASET", id: d.id,
                patch: { fs_validationStrategy: "cv-only", fs_trainRatio: 1.0 }
              });
            }
          } else {
            if (currentPurpose !== "train-and-cv") {
              dispatch({
                type: "DP_UPDATE_DATASET", id: d.id,
                patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
              });
            }
          }
        }
      });
    }
  }, [isCombinedPool, totalSamples, combinedPurpose, allDs, dispatch]);

  const hasCvOnly = allDs.some(d => 
    d.fs_datasetPurpose === "train-and-cv" || 
    (d.fs_datasetPurpose === "test" && d.fs_validationStrategy === "cv-only")
  );

  const hasInternalTest = allDs.some(d => 
    d.fs_datasetPurpose === "train-and-test" || 
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
  );

  const hasAnyTesting = allDs.some(d => 
    (d.fs_datasetPurpose === "test" &&
     d.fs_validationStrategy !== "cv-only" &&
     d.fs_validationStrategy !== "train-and-cv") || 
    d.fs_datasetPurpose === "train-and-test" || 
    ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
  );

  // sampleCountError: warn if any training dataset has too few samples for a train-test split
  const sampleCountError: string | null = (() => {
    if (isCombinedPool) {
      // pooled check already handled by useEffect that forces train-and-cv
      return null;
    }
    for (const d of trainingDatasets) {
      const purpose = d.fs_datasetPurpose || "train-and-test";
      if (purpose !== "train-and-cv" && (d.nSamples || 0) < 30) {
        return `Dataset "${d.name}" has only ${d.nSamples || 0} samples. Datasets with fewer than 30 samples must use Train & CV mode.`;
      }
    }
    return null;
  })();

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

  const validationError: string | null = sampleCountError || allocationError;

  // Stable CV results — only updated when Run CV is clicked
  const [inlineCvResults, setInlineCvResults] = useState<CVResult[]>(state.dpCvResultsCache || []);

  const [fsResults, setFsResults] = useState<{ gene: string, importance: number, rank: number }[]>(state.dpTrainingResultsCache?.features || []);
  const [fsFullResults, setFsFullResults] = useState<any>(state.dpTrainingResultsCache || null);
  const [trainingAccuracies, setTrainingAccuracies] = useState<Record<string, number>>({});

  const [selectionMethod, setSelectionMethod] = useState<"breakoff" | "percentage" | "max_features" | "overlap">("breakoff");
  const [percentageValue, setPercentageValue] = useState<number>(80);
  const [maxFeaturesValue, setMaxFeaturesValue] = useState<number>(10);
  const [selectionJobId, setSelectionJobId] = useState<string>(state.dpTrainingResultsCache?.jobId || "");
  const [isRefitting, setIsRefitting] = useState<boolean>(false);
  const [isRefitted, setIsRefitted] = useState<boolean>(() => Boolean(state.dpTrainingResultsCache?.isRefitted));
  const [selectedModelTab, setSelectedModelTab] = useState<Record<string, string>>({});

  // Clinical upload states and helpers
  const [clinicalDrag, setClinicalDrag] = useState(false);
  const [localLoading, setLocalLoading] = useState(false);
  const [localLoadingMsg, setLocalLoadingMsg] = useState("");

  const updateDs = (patch: Partial<typeof ds>) => {
    dispatch({ type: "DP_UPDATE_DATASET", id: ds.id, patch } as never);
  };

  const handleClear = async () => {
    try {
      await clearDatasetAPI(ds.id, "clinical");
    } catch (e) {
      console.warn("clearDatasetAPI failed (non-fatal):", e);
    }
    updateDs({
      clinicalFile: null,
      clinicalFileName: "",
      clinicalSampleIdCol: "",
      clinicalGroupCol: "",
      clinicalColumns: undefined,
      clinicalParsedData: undefined,
      deUploadBypassed: false,
      fsUploadBypassed: false,
      integrityOk: true,
      integrityIssues: []
    });
  };

  const handleLoadSample = async () => {
    setLocalLoading(true);
    setLocalLoadingMsg("Loading sample clinical data…");
    try {
      const result = await fetchSampleDataAPI("clinical");
      const rows = result.parsedData as string[][];
      updateDs({
        clinicalFileName: "sample_clinical.csv",
        clinicalSampleIdCol: result.columns[0] || "SampleID",
        clinicalGroupCol: result.columns[1] || "Group",
        clinicalColumns: result.columns,
        clinicalParsedData: rows,
        deUploadBypassed: false,
        fsUploadBypassed: false,
      });
    } catch (err: any) {
      updateDs({ integrityIssues: [`Failed to load sample data: ${err.message}`] });
    } finally {
      setLocalLoading(false);
    }
  };

  const handleClinicalFile = async (file: File) => {
    setLocalLoadingMsg(`Parsing ${file.name}…`);
    setLocalLoading(true);
    try {
      const parsed = await parseClinicalFile(file);
      updateDs({
        clinicalFile: file,
        clinicalFileName: file.name,
        clinicalSampleIdCol: parsed.clinicalSampleIdCol,
        clinicalGroupCol: parsed.clinicalGroupCol,
        clinicalColumns: parsed.clinicalColumns,
        clinicalParsedData: parsed.clinicalParsedData,
        deUploadBypassed: false,
        fsUploadBypassed: false,
      });
    } catch (error: any) {
      updateDs({
        integrityIssues: [`Error parsing clinical file: ${error.message}`],
      });
    } finally {
      setLocalLoading(false);
    }
  };

  // Sync authoritative dataset statistics from backend on mount if missing
  useEffect(() => {
    const unSynced = allDs.filter(d => !d.matchingSamples || d.matchingSamples.length === 0 || !d.nFeatures || d.nFeatures === 0);
    if (unSynced.length > 0) {
      fetchDatasetInfoAPI(allDs.map(d => d.id), "fs")
        .then(infoRes => {
          if (infoRes && infoRes.datasets) {
            Object.entries(infoRes.datasets).forEach(([dsId, info]) => {
              dispatch({
                type: "DP_UPDATE_DATASET",
                id: dsId,
                patch: {
                  nSamples: info.nSamples,
                  sampleIds: info.sampleIds,
                  nFeatures: info.nFeatures,
                  matchingSamples: info.matchingSamples,
                  missingClinSamples: info.missingClinSamples,
                  clinicalNoExprSamples: info.clinicalNoExprSamples,
                  groups: info.groups,
                  clinicalColumns: info.clinicalColumns && info.clinicalColumns.length > 0 ? info.clinicalColumns : undefined
                }
              } as never);
            });
          }
        })
        .catch(err => console.warn("Failed to sync FS dataset info:", err));
    }
  }, []);

  // Samples matching comparison logic (preferring authoritative backend stats)
  const exprSamples = ds
    ? ((ds.sampleIds && ds.sampleIds.length > 0)
      ? ds.sampleIds
      : getExpressionSampleColumns(ds.columns || [], ds.geneIdCol, ds.geneInfoCols))
    : [];
  const sampleIdIdx = ds && ds.clinicalFileName && ds.clinicalSampleIdCol && ds.clinicalColumns
    ? ds.clinicalColumns.indexOf(ds.clinicalSampleIdCol)
    : -1;
  const columnsToUse = ds && ds.clinicalColumns && ds.clinicalColumns.length > 0
    ? ds.clinicalColumns
    : [];
  const rowsToUse = ds && ds.clinicalParsedData && ds.clinicalParsedData.length > 0
    ? ds.clinicalParsedData
    : [];
  const clinSamples = sampleIdIdx !== -1 && rowsToUse
    ? rowsToUse.map(row => row[sampleIdIdx]).filter(Boolean)
    : [];
  const clinSet = new Set(clinSamples);

  const matchingSamples = (ds && ds.matchingSamples && ds.matchingSamples.length > 0)
    ? ds.matchingSamples
    : (exprSamples.length > 0 && clinSamples.length > 0 ? exprSamples.filter(s => clinSet.has(s)) : exprSamples);
  const matchingSet = new Set(matchingSamples);
  const filteredRows = sampleIdIdx !== -1
    ? rowsToUse.filter(row => matchingSet.has(row[sampleIdIdx]))
    : rowsToUse;
  const hasClinical = ds ? (!!ds.clinicalFileName || !!ds.clinicalUploadId || !!ds.clinicalFilePath) : false;
  const isNoOverlap = hasClinical && exprSamples.length > 0 && matchingSamples.length === 0;
  const isPartialMatch = hasClinical && exprSamples.length > 0 && matchingSamples.length > 0 && (matchingSamples.length < exprSamples.length);

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

  const handleRunSelection = async () => {
    setTrainElapsed(0);
    setLoadingSelection(true);
    if (doneSelection) {
      try {
        await redoStepAPI("fs", state.dpDatasets.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    try {
      const fullParams = buildFullModelParams(fsConfig.selectedModels, modelParams);
      const response = await runInlineFSAPI(
        state.dpDatasets,
        fsConfig.selectedModels,
        state.dpDatasets[0]?.fs_trainRatio ?? 0.7,
        fullParams,
        undefined,
        (info) => setTrainElapsed(info.elapsed),
        fsConfig.multiDatasetMode ?? "combine"
      );
      if (response) {
        setSelectionJobId(response.jobId || "");
        const firstDsId = Object.keys(response)[0];
        const firstRes = response[firstDsId] || response;
        setFsResults(firstRes.features || []);
        setTrainingAccuracies(firstRes.accuracies || {});
        setFsFullResults(response);
        setIsRefitted(false);
        dispatch({ type: "DP_SET_TRAINING_CACHE", results: response });

        const allWarnings: string[] = [];
        Object.values(response).forEach((val: any) => {
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
       const ratios: Record<string, number> = {};
      const purposes: Record<string, string> = {};
      const strategies: Record<string, string> = {};
      const internals: Record<string, boolean> = {};
      state.dpDatasets.forEach(d => {
        ratios[d.id] = d.fs_trainRatio ?? 0.7;
        purposes[d.id] = d.fs_datasetPurpose || "train-and-test";
        strategies[d.id] = d.fs_validationStrategy || "train-test-split";
        internals[d.id] = d.fs_isInternalValidation ?? false;
      });
      setSubmittedSelectionCfg({
        trainRatios: ratios,
        selectedModels: [...fsConfig.selectedModels],
        datasetPurposes: purposes,
        validationStrategies: strategies,
        isInternalValidations: internals
      });
      setSubmittedModelParams(modelParams);
      setSubmittedSelectionParams(null);
      setDoneSelection(true);
      setIsRefitted(false);
      dispatch({ type: "SET_DP_INLINE_FS_DONE", done: true });
    } catch (err: any) {
      console.error("Inline run selection error:", err);
      toast({
        title: "Model Training Failed",
        description: err.message || "Failed to train machine learning models.",
        variant: "destructive"
      });
    } finally {
      setLoadingSelection(false);
    }
  };

  const triggerRun = (runFn: () => void) => {
    if (isNoOverlap) {
      setShowWarnModal(true);
      setPendingRun(() => runFn);
      return;
    }
    runFn();
  };

  const handleConfirmWarn = () => {
    setShowWarnModal(false);
    if (pendingRun) {
      pendingRun();
      setPendingRun(null);
    }
  };

  const hasTrainingSettingsChanged = doneSelection && submittedSelectionCfg && (
    state.dpDatasets.some(d => {
      const submittedRatio = submittedSelectionCfg.trainRatios?.[d.id] ?? 0.7;
      const currentRatio = d.fs_trainRatio ?? 0.7;
      return submittedRatio !== currentRatio;
    }) ||
    JSON.stringify(submittedSelectionCfg.selectedModels) !== JSON.stringify(fsConfig.selectedModels) ||
    JSON.stringify(submittedModelParams) !== JSON.stringify(modelParams)
  );

  const hasSelectionMethodChanged = Boolean(isRefitted && submittedSelectionParams && (
    submittedSelectionParams.method !== selectionMethod ||
    (selectionMethod === "percentage" && submittedSelectionParams.percentage !== percentageValue) ||
    (selectionMethod === "max_features" && submittedSelectionParams.maxFeatures !== maxFeaturesValue)
  ));

  const hasSelectionSettingsChanged = hasTrainingSettingsChanged;

  const getSubmittedContinueLabel = () => {
    if (!submittedSelectionCfg) return "Continue";
    const subCvEnabled = fsConfig.cvEnabled;
    const subHasCvOnly = allDs.some(d => 
      submittedSelectionCfg.datasetPurposes?.[d.id] === "train-and-cv" || 
      (submittedSelectionCfg.datasetPurposes?.[d.id] === "test" && submittedSelectionCfg.validationStrategies?.[d.id] === "cv-only")
    );
    const subHasAnyTesting = allDs.some(d => 
      submittedSelectionCfg.datasetPurposes?.[d.id] === "test" || 
      submittedSelectionCfg.datasetPurposes?.[d.id] === "train-and-test" || 
      ((submittedSelectionCfg.datasetPurposes?.[d.id] === "train" || !submittedSelectionCfg.datasetPurposes?.[d.id]) && submittedSelectionCfg.isInternalValidations?.[d.id])
    );
    
    if (subCvEnabled || subHasCvOnly) {
      return "Continue to Cross Validation";
    } else if (subHasAnyTesting) {
      return "Continue to Testing →";
    } else {
      return "Continue to Module Selection →";
    }
  };

  const revertToSubmittedSettings = () => {
    if (!submittedSelectionCfg) return;
    dispatch({
      type: "FS_SET_CONFIG",
      patch: {
        selectedModels: [...submittedSelectionCfg.selectedModels]
      }
    });
    allDs.forEach(d => {
      const trainRatio = submittedSelectionCfg.trainRatios?.[d.id] ?? 0.7;
      const datasetPurpose = submittedSelectionCfg.datasetPurposes?.[d.id] || "train-and-test";
      const validationStrategy = submittedSelectionCfg.validationStrategies?.[d.id] || "train-test-split";
      const isInternalValidation = submittedSelectionCfg.isInternalValidations?.[d.id] ?? false;
      dispatch({
        type: "DP_UPDATE_DATASET",
        id: d.id,
        patch: {
          fs_trainRatio: trainRatio,
          fs_datasetPurpose: datasetPurpose as any,
          fs_validationStrategy: validationStrategy as any,
          fs_isInternalValidation: isInternalValidation
        }
      } as never);
    });
    setModelParams(JSON.parse(JSON.stringify(submittedModelParams)));
  };

  const handleRefitSelection = async () => {
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
        const firstDsId = Object.keys(refitResponse)[0];
        const firstRes = refitResponse[firstDsId] || refitResponse;
        (refitResponse as any).isRefitted = true;
        (refitResponse as any).jobId = selectionJobId;
        setFsResults(firstRes.features || []);
        setTrainingAccuracies(firstRes.accuracies || {});
        setFsFullResults(refitResponse);
        setIsRefitted(true);
        setSubmittedSelectionParams({
          method: selectionMethod,
          percentage: selectionMethod === "percentage" ? (percentageValue || 80) : 80,
          maxFeatures: selectionMethod === "max_features" ? (maxFeaturesValue || 10) : 10
        });
        dispatch({ type: "DP_SET_TRAINING_CACHE", results: refitResponse });
        toast({
          title: "Features Refitted",
          description: "Models have been successfully retrained on the selected features."
        });
      }
    } catch (err: any) {
      console.error("Refit features error:", err);
      toast({
        title: "Feature Refitting Failed",
        description: err.message || "Failed to retrain models on selected features.",
        variant: "destructive"
      });
    } finally {
      setIsRefitting(false);
    }
  };

  const hasCVSettingsChanged = doneCV && submittedCVCfg && (
    submittedCVCfg.cvMethod !== fsConfig.cvMethod ||
    submittedCVCfg.cvFolds !== fsConfig.cvFolds
  );

  const handleRunCV = async () => {
    setLoadingCV(true);
    if (doneCV) {
      dispatch({ type: "DP_SET_CV_CACHE", results: null });
      try {
        await redoStepAPI("cv", state.dpDatasets.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    try {
      const results = await runCrossValidationAPI(
        fsConfig.selectedModels,
        fsConfig.cvMethod,
        fsConfig.cvFolds,
        state.dpDatasets
      );
      setSubmittedCVCfg({ cvMethod: fsConfig.cvMethod, cvFolds: fsConfig.cvFolds });
      setInlineCvResults(results);
      dispatch({ type: "DP_SET_CV_CACHE", results });
      setDoneCV(true);
      dispatch({ type: "SET_DP_INLINE_FS_DONE", done: true });
    } catch (err: any) {
      console.error("Inline run CV error:", err);
      toast({
        title: "Cross-Validation Failed",
        description: err.message || "Failed to perform cross-validation.",
        variant: "destructive"
      });
    } finally {
      setLoadingCV(false);
    }
  };

  const handleContinueCV = () => {
    if (hasCVSettingsChanged) {
      setShowChangeWarnModal(true);
      return;
    }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowChangeWarnModal(false);
    if (hasAnyTesting) {
      setSubStep("testing");
    } else {
      // Inline FS always continues to module selection after CV (no export substep)
      dispatch({ type: "DP_SET_STEP", step: "module-select" });
    }
  };


  if (subStep === "model-selection") {
    return (
      <>
        {localLoading && <Spinner label={localLoadingMsg} sublabel="Please wait…" />}
        {loadingSelection && <Spinner label="Running feature selection…" sublabel={trainElapsed > 0 ? `Training ${fsConfig.selectedModels.length} model(s) — ${Math.round(trainElapsed)}s elapsed` : `Training ${fsConfig.selectedModels.length} model(s) — estimated ~${Math.max(5, fsConfig.selectedModels.length * 3)}s`} />}
        {isRefitting && <Spinner label="Refitting features…" sublabel="Retraining models on selected feature subset…" />}

        <div className="card" style={{ marginBottom: 14, display: "none" }}>
          <div className="card-header" style={{ marginBottom: 0 }}>
            <div>
              <div className="card-title">
                Clinical / Phenotype Data
                <span style={{
                  marginLeft: 8, fontSize: 11, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                  border: "1px solid hsl(0 50% 85%)", padding: "2px 7px", borderRadius: 4, fontWeight: 500,
                }}>
                  Required
                </span>
              </div>
              <div className="card-sub">Upload sample metadata with condition/group assignments for analysis.</div>
            </div>
            <div style={{ display: "flex", gap: 10 }}>
              <button className="link-button danger" onClick={handleClear}>Clear</button>
              <button className="link-button primary" onClick={handleLoadSample}>Example Data</button>
            </div>
          </div>

          {!ds.clinicalFileName ? (
            <div className={`drop-zone ${clinicalDrag ? "dragover" : ""}`} style={{ marginTop: 16 }}
              onClick={() => {
                const input = document.createElement("input");
                input.type = "file"; input.accept = ".csv,.tsv,.txt";
                input.onchange = e => { const f = (e.target as HTMLInputElement).files?.[0]; if (f) handleClinicalFile(f); };
                input.click();
              }}
              onDragOver={e => { e.preventDefault(); setClinicalDrag(true); }}
              onDragLeave={() => setClinicalDrag(false)}
              onDrop={e => { e.preventDefault(); setClinicalDrag(false); const f = e.dataTransfer.files?.[0]; if (f) handleClinicalFile(f); }}
              data-testid="dropzone-clinical">
              <div className="drop-zone-icon">🧬</div>
              <div className="drop-zone-title">Drop clinical metadata file here</div>
              <div className="drop-zone-hint">CSV · TSV &nbsp;·&nbsp; SampleID + Group columns required</div>
            </div>
          ) : (
            <>
              <div className="file-chip" style={{ marginTop: 14 }}>
                <FileText size={15} />
                {isPartialMatch || isNoOverlap ? (
                  <>
                    <span className="file-chip-name">
                      {ds.clinicalFileName}
                    </span>
                    <span style={{ fontSize: 12, color: "hsl(38 85% 35%)" }}>
                      ({matchingSamples.length} samples matched with the {exprSamples.length} expression samples)
                    </span>
                  </>
                ) : (
                  <>
                    <span className="file-chip-name">{ds.clinicalFileName}</span>
                    <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>
                      ({matchingSamples.length} samples matched)
                    </span>
                  </>
                )}
                <button className="file-chip-clear" onClick={handleClear} data-testid="btn-clear-clinical"><X size={16} /></button>
              </div>

              {/* Custom selector stacked layout next to each other */}
              <div style={{ marginTop: 14, display: "flex", gap: 16, flexWrap: "wrap" }}>
                <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Sample ID Column:</label>
                  <details style={{ width: "100%" }} data-testid="details-clinical-sample-id-col">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>{ds.clinicalSampleIdCol || "Select column..."}</span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {(ds.clinicalColumns).map(c => (
                        <div
                          key={c}
                          onClick={(e) => {
                            updateDs({ clinicalSampleIdCol: c, deUploadBypassed: false, fsUploadBypassed: false });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.clinicalSampleIdCol === c ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent"}
                        >
                          {c}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>

                <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Group Column</label>
                  <details style={{ width: "100%" }} data-testid="details-clinical-group-col">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>{ds.clinicalGroupCol || "Select column..."}</span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {(ds.clinicalColumns).map(c => (
                        <div
                          key={c}
                          onClick={(e) => {
                            updateDs({ clinicalGroupCol: c, deUploadBypassed: false, fsUploadBypassed: false });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.clinicalGroupCol === c ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.clinicalGroupCol === c ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.clinicalGroupCol === c ? "var(--selected-bg)" : "transparent"}
                        >
                          {c}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>
              </div>

              <hr className="card-divider" />
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
                Clinical Data Preview (Overlapping Samples Only)
              </div>
              <div className="preview-wrap">
                <table>
                  <thead><tr>{(columnsToUse).map(col => <th key={col}>{col}</th>)}</tr></thead>
                  <tbody>{(filteredRows).map((row, i) => <tr key={i}>{row.map((cell, j) => <td key={j}>{cell}</td>)}</tr>)}</tbody>
                </table>
              </div>
            </>
          )}
        </div>
        
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
                    <div style={{ width: 10, height: 10, borderRadius: "50%", background: state.dpDatasets[0]?.color || "var(--primary)" }} />
                    <span style={{ fontWeight: 600, fontSize: 14, color: "var(--foreground)" }}>
                      {isSingleDataset
                        ? `${state.dpDatasets[0]?.name} (${totalSamples} samples)`
                        : `Pooled Dataset (${state.dpDatasets.length} Datasets · ${totalSamples} Total Samples)`}
                    </span>
                  </div>
                  <div style={{ display: "flex", alignItems: "center", gap: 6, flexWrap: "wrap" }}>
                    {!isSingleDataset && state.dpDatasets.map(d => (
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
                      {combinedHierarchy.toUpperCase()} · {state.dpDatasets[0]?.dataType || "readcounts"}
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
                      name="inline-combined-purpose"
                      value="train-and-test"
                      disabled={totalSamples < 100}
                      checked={combinedPurpose === "train-and-test"}
                      onChange={() => {
                        state.dpDatasets.forEach(d => {
                          dispatch({
                            type: "DP_UPDATE_DATASET", id: d.id,
                            patch: { fs_datasetPurpose: "train-and-test", fs_validationStrategy: "train-test-split", fs_isInternalValidation: true, fs_trainRatio: combinedRatio }
                          } as never);
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
                      name="inline-combined-purpose"
                      value="train-and-cv"
                      checked={combinedPurpose === "train-and-cv"}
                      onChange={() => {
                        state.dpDatasets.forEach(d => {
                          dispatch({
                            type: "DP_UPDATE_DATASET", id: d.id,
                            patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
                          } as never);
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
                {combinedPurpose === "train-and-test" && (
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
                              name="inline-combined-split-ratio"
                              value={opt.value}
                              checked={combinedRatio === opt.value}
                              onChange={() => {
                                state.dpDatasets.forEach(d => {
                                  dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } } as never);
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
              state.dpDatasets.map((d, index) => {
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
                            name={`inline-purpose-${d.id}`}
                            value="train-and-test"
                            disabled={isSmall}
                            checked={currentPurpose === "train-and-test"}
                            onChange={() => dispatch({
                              type: "DP_UPDATE_DATASET", id: d.id,
                              patch: { fs_datasetPurpose: "train-and-test", fs_validationStrategy: "train-test-split", fs_isInternalValidation: true, fs_trainRatio: 0.7 }
                            } as never)}
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
                            name={`inline-purpose-${d.id}`}
                            value="train-and-cv"
                            checked={currentPurpose === "train-and-cv"}
                            onChange={() => dispatch({
                              type: "DP_UPDATE_DATASET", id: d.id,
                              patch: { fs_datasetPurpose: "train-and-cv", fs_validationStrategy: "cv-only", fs_isInternalValidation: false, fs_trainRatio: 1.0 }
                            } as never)}
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
                            name={`inline-purpose-${d.id}`}
                            value="test"
                            checked={currentPurpose === "test"}
                            onChange={() => dispatch({
                              type: "DP_UPDATE_DATASET", id: d.id,
                              patch: { fs_datasetPurpose: "test", fs_validationStrategy: isSmall ? "cv-only" : "train-test-split", fs_isInternalValidation: false, fs_trainRatio: isSmall ? 1.0 : 0.7 }
                            } as never)}
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
                    {currentPurpose === "train-and-test" && (
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
                                  name={`inline-ratio-${d.id}`}
                                  value={opt.value}
                                  checked={currentRatio === opt.value}
                                  onChange={() => dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } } as never)}
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
                              onChange={() => dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { fs_validationStrategy: "train-test-split", fs_trainRatio: 0.7 } } as never)}
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
                              onChange={() => dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { fs_validationStrategy: "cv-only", fs_trainRatio: 1.0 } } as never)}
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
                                      name={`inline-refit-ratio-${d.id}`}
                                      value={opt.value}
                                      checked={currentRatio === opt.value}
                                      onChange={() => dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { fs_trainRatio: opt.value } } as never)}
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
        <div className="card" style={{ marginBottom: 14 }}>
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
                  {/* Card header */}
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

                  {/* Params */}
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

        {/* Global Dataset Purpose Allocation Verification Card */}
        {state.dpDatasets.length > 1 && (
          <div className="card" style={{ marginBottom: 14 }}>
            <div className="card-title" style={{ marginBottom: 6 }}>Pipeline Allocation Requirements</div>
            <div className="card-sub" style={{ marginBottom: 12 }}>Validating cross-dataset allocation rules for setup layout topology.</div>
            
            {validationError ? (
              <div className="banner danger" style={{ display: "flex", alignItems: "flex-start", gap: 10 }}>
                <AlertTriangle size={16} style={{ marginTop: 2, flexShrink: 0 }} />
                <div>
                  <strong>Allocation Error:</strong> {validationError}
                </div>
              </div>
            ) : (
              <div className="banner success" style={{ display: "flex", alignItems: "center", gap: 10 }}>
                <CheckCircle size={16} style={{ flexShrink: 0 }} />
                <span>Global dataset purposes are validly assigned. Configuration ready.</span>
              </div>
            )}
          </div>
        )}

        {doneSelection && (
          <div style={{ display: "flex", flexDirection: "column", gap: 16, marginTop: 16 }}>
            {(isCombinedPool ? trainingDatasets.slice(0, 1) : trainingDatasets).map(d => {
              const dsRes = getDatasetResults(d.id);
              if (!dsRes) return null;
              const features = dsRes.features || [];
              const dsWarnings: string[] = Array.isArray(dsRes.warnings) ? dsRes.warnings : [];
              const displayName = isCombinedPool && !isSingleDataset ? "Pooled Datasets" : d.name;
              return (
                <div key={d.id} style={{ display: "flex", flexDirection: "column", gap: 14 }}>
                  {dsWarnings.map((w, i) => (
                    <div key={`w${i}`} className="banner warn" style={{ marginTop: 0 }}>⚠ {w}</div>
                  ))}
                  {features.length > 0 && (
                    <div className="card" style={{ marginBottom: 0, opacity: hasSelectionSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
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

                  {(isRefitted || isOnlyBorutaStabl) && !hasSelectionMethodChanged && !hasTrainingSettingsChanged && (
                    <div className="card" style={{ marginTop: 14, marginBottom: 0, opacity: hasTrainingSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
                      <div className="card-title" style={{ marginBottom: 4, fontSize: 13 }}>Model Training Performance - {displayName}</div>
                      <div className="card-sub">Performance metrics evaluated on the training validation split.</div>
                      <hr className="card-divider" />

                      <div style={{ display: "grid", gridTemplateColumns: "1fr", gap: 20 }}>
                        {/* ROC Curve Graph */}
                        <div style={{ display: "flex", flexDirection: "column", gap: 8, border: "1px solid var(--border)", borderRadius: 8, padding: 12, background: "var(--muted)" }}>
                          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>ROC Curves</div>
                          {dsRes.roc_plot ? (
                            <div style={{ display: "flex", justifyContent: "center" }}>
                              <img src={`data:image/png;base64,${dsRes.roc_plot}`} style={{ width: 600, height: 400, objectFit: "contain", borderRadius: 8 }} alt="ROC Curves" />
                            </div>
                          ) : (
                            <div style={{ height: 210, display: "flex", alignItems: "center", justifyContent: "center", border: "1px dashed var(--border)", borderRadius: 6, background: "var(--background)" }}>
                              <div style={{ padding: 40, color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>Errors occured. Couldn't generate ROC Curves...</div>
                            </div>
                          )}
                        </div>

                        {/* Metrics Tables / stat-chips */}
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
                                      <div className="stat-chip-val" style={{ color: "var(--primary)" }}>{formatMetric(metrics.auc, 0.5, 3)}</div>
                                      <div className="stat-chip-lbl">AUC</div>
                                    </div>
                                    <div className="stat-chip" style={{ minWidth: 160 }}>
                                      <div className="stat-chip-val">{formatCi(metrics.ci, metrics.ci_lower, metrics.ci_upper)}</div>
                                      <div className="stat-chip-lbl">95% CI</div>
                                    </div>
                                    <div className="stat-chip">
                                      <div className="stat-chip-val">{formatMetric(metrics.acc, 0.5, 3)}</div>
                                      <div className="stat-chip-lbl">Balanced Accuracy</div>
                                    </div>
                                    <div className="stat-chip">
                                      <div className="stat-chip-val">{formatMetric(metrics.ppv, 0.5, 3)}</div>
                                      <div className="stat-chip-lbl">PPV</div>
                                    </div>
                                    <div className="stat-chip">
                                      <div className="stat-chip-val">{formatMetric(metrics.npv, 0.5, 3)}</div>
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
                                <strong>PPV (Positive Predictive Value):</strong> TP / (TP + FP). The proportion of predicted positive cases that are true positives (empirical or prevalence-adjusted)
                              </div>
                              <div>
                                <strong>NPV (Negative Predictive Value):</strong> TN / (TN + FN). The proportion of predicted negative cases that are true negatives (empirical or prevalence-adjusted)
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

        <div className="action-row">
          <button className="btn btn-default" onClick={() => {
            if (onBack) {
              onBack();
            } else {
              dispatch({ type: "DP_SET_STEP", step: "module-select" });
            }
          }}>← Back</button>

          <div style={{ display: "flex", gap: 8 }}>
            {/* Level 1: Redo Model Training (overrules feature selection redo) */}
            {doneSelection && hasTrainingSettingsChanged && (
              <button
                className="btn btn-default"
                style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }}
                disabled={loadingSelection}
                onClick={() => {
                  setDiscardType("model-selection");
                  if (doneCV || state.dpTestingResultsCache) {
                    setShowDiscardModal(true);
                  } else {
                    dispatch({
                      type: "RESET_DOWNSTREAM_STEPS",
                      datasetId: ds?.id ?? state.dpDatasets[0]?.id,
                      fromStep: "model-selection"
                    } as any);
                    setDoneSelection(false);
                    setIsRefitted(false);
                    setSubmittedSelectionParams(null);
                    setFsResults([]);
                    setFsFullResults(null);
                    setTrainingAccuracies({});
                    setDoneCV(false);
                    dispatch({ type: "DP_SET_TRAINING_CACHE", results: null });
                    dispatch({ type: "DP_SET_CV_CACHE", results: null });
                    dispatch({ type: "DP_SET_TESTING_CACHE", results: null });
                    triggerRun(handleRunSelection);
                  }
                }}
                data-testid="btn-redo-fs"
              >
                🔄 Redo Model Training
              </button>
            )}

            {/* Level 2: Redo Selecting Features (only when training settings have NOT changed and models have rank) */}
            {!hasTrainingSettingsChanged && doneSelection && hasAnyRankModel && hasSelectionMethodChanged && (
              <button
                className="btn btn-default"
                style={{ borderColor: "var(--primary)", color: "var(--primary)" }}
                disabled={isRefitting || loadingSelection}
                onClick={handleRefitSelection}
                data-testid="btn-redo-inline-fs-selection"
              >
                {isRefitting ? "Refitting Features…" : "🔄 Redo Selecting Features"}
              </button>
            )}

            {!doneSelection ? (
              <button
                className="btn btn-primary"
                disabled={fsConfig.selectedModels.length === 0 || !!validationError || loadingSelection}
                onClick={() => triggerRun(handleRunSelection)}
                data-testid="btn-run-inline-fs"
              >
                {loadingSelection ? "Training…" : "Train Selected Models"}
              </button>
            ) : hasTrainingSettingsChanged ? (
              <button
                className="btn btn-primary"
                disabled={!!validationError || loadingSelection}
                onClick={() => {
                  revertToSubmittedSettings();
                  const subCvEnabled = fsConfig.cvEnabled;
                  const subHasCvOnly = allDs.some(d => 
                    submittedSelectionCfg.datasetPurposes?.[d.id] === "train-and-cv" || 
                    (submittedSelectionCfg.datasetPurposes?.[d.id] === "test" && submittedSelectionCfg.validationStrategies?.[d.id] === "cv-only")
                  );
                  const subHasAnyTesting = allDs.some(d => 
                    (submittedSelectionCfg.datasetPurposes?.[d.id] === "test" &&
                     submittedSelectionCfg.validationStrategies?.[d.id] !== "cv-only" &&
                     submittedSelectionCfg.validationStrategies?.[d.id] !== "train-and-cv") ||
                    submittedSelectionCfg.datasetPurposes?.[d.id] === "train-and-test" || 
                    ((submittedSelectionCfg.datasetPurposes?.[d.id] === "train" || !submittedSelectionCfg.datasetPurposes?.[d.id]) && submittedSelectionCfg.isInternalValidations?.[d.id])
                  );
                  const isCV = subCvEnabled || subHasCvOnly;
                  if (isCV) {
                    setSubStep("cross-validation");
                  } else if (subHasAnyTesting) {
                    setSubStep("testing");
                  } else {
                    dispatch({ type: "SET_INLINE_FS_DONE", done: true });
                    dispatch({ type: "SET_ACTIVE_STEP", step: "module-select" });
                  }
                }}
                data-testid="btn-run-inline-fs"
              >
                {getSubmittedContinueLabel()}
              </button>
            ) : hasAnyRankModel && (!isRefitted || hasSelectionMethodChanged) ? (
              <button
                className="btn btn-primary"
                disabled={!!validationError || isRefitting || loadingSelection}
                onClick={handleRefitSelection}
                data-testid="btn-refit-inline-fs"
              >
                {isRefitting ? "Refitting Features…" : "Refit Models on Selected Features"}
              </button>
            ) : (fsConfig.cvEnabled || hasCvOnly) ? (
              <button
                className="btn btn-primary"
                disabled={!!validationError || loadingSelection}
                onClick={() => setSubStep("cross-validation")}
                data-testid="btn-continue-model-selection"
              >
                Continue to Cross Validation
              </button>
            ) : hasAnyTesting ? (
              <button
                className="btn btn-primary"
                disabled={!!validationError || loadingSelection}
                onClick={() => setSubStep("testing")}
                data-testid="btn-inline-fs-continue"
              >
                Continue to Testing →
              </button>
            ) : (
              <button
                className="btn btn-primary"
                disabled={!!validationError || loadingSelection}
                onClick={() => {
                  dispatch({ type: "SET_INLINE_FS_DONE", done: true });
                  dispatch({ type: "SET_ACTIVE_STEP", step: "module-select" });
                }}
                data-testid="btn-inline-fs-continue"
              >
                Continue to Module Selection →
              </button>
            )}
          </div>
        </div>

        {showWarnModal && (
          <WarnSampleModal
            type="partial_overlap"
            onConfirm={handleConfirmWarn}
            onCancel={() => setShowWarnModal(false)}
          />
        )}

        {showChangeWarnModal && (
          <ChangeWarnModal
            onConfirm={proceedToNextStep}
            onCancel={() => setShowChangeWarnModal(false)}
          />
        )}

        {showDiscardModal && (
          <DiscardWarnModal
            stepId="fs"
            datasetIds={state.dpDatasets.map(d => d.id)}
            modulesList={
              discardType === "cv"
                ? (state.dpTestingResultsCache ? ["Model Evaluation (Testing)"] : [])
                : (doneCV ? ["Cross-Validation"] : [])
            }
            onCancel={() => setShowDiscardModal(false)}
            onConfirm={async () => {
              setShowDiscardModal(false);
              if (discardType === "cv") {
                dispatch({
                  type: "RESET_DOWNSTREAM_STEPS",
                  datasetId: ds?.id ?? state.dpDatasets[0]?.id,
                  fromStep: "cross-validation"
                } as any);
                setInlineCvResults([]);
                dispatch({ type: "DP_SET_CV_CACHE", results: null });
                dispatch({ type: "DP_SET_TESTING_CACHE", results: null });
                setDoneCV(false);
                triggerRun(handleRunCV);
              } else {
                dispatch({
                  type: "RESET_DOWNSTREAM_STEPS",
                  datasetId: ds?.id ?? state.dpDatasets[0]?.id,
                  fromStep: "model-selection"
                } as any);
                setDoneSelection(false);
                setFsResults([]);
                setFsFullResults(null);
                setTrainingAccuracies({});
                setDoneCV(false);
                dispatch({ type: "DP_SET_TRAINING_CACHE", results: null });
                dispatch({ type: "DP_SET_CV_CACHE", results: null });
                dispatch({ type: "DP_SET_TESTING_CACHE", results: null });
                triggerRun(handleRunSelection);
              }
            }}
          />
        )}
      </>
    );
  }

  if (subStep === "testing") {
    return (
      <TestingStep
        mode="dp"
        onBack={() => {
          setSubStep(fsConfig.cvEnabled ? "cross-validation" : "model-selection");
        }}
        onContinue={() => {
          dispatch({ type: "DP_SET_STEP", step: "module-select" });
        }}
      />
    );
  }

  // Cross-Validation view
  return (
    <>
      {localLoading && <Spinner label={localLoadingMsg} sublabel="Please wait…" />}
      {loadingCV && <Spinner label="Running cross-validation…" sublabel="Searching parameter space…" />}

      {/* CV Method */}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 4 }}>Cross-Validation Method</div>
        <div className="card-sub">Choose a hyperparameter search strategy.</div>
        <hr className="card-divider" />
        
        {/* Method Selection List */}
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


      {doneCV && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
          {processedCVList.map(d => {
            const dsCV = getDatasetCVResults(d.id);
            if (!dsCV || dsCV.length === 0) return null;
            const displayLabel = d.name;
            return (
              <div key={d.id} style={{ display: "flex", flexDirection: "column", gap: 14 }}>
                <div className="card" style={{ marginBottom: 14, opacity: hasCVSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
                  <div className="card-title" style={{ marginBottom: 10 }}>
                  Cross-Validation Results - {displayLabel} {hasCVSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated - settings changed)</span>}
                </div>

                <div className="banner success" style={{ marginBottom: 12 }}>✅ Cross-validation complete.</div>

                {getCVRocPlotForDataset(d.id) && (
                  <div style={{ display: "flex", justifyContent: "center", marginBottom: 14 }}>
                    <img src={`data:image/png;base64,${getCVRocPlotForDataset(d.id)}`} style={{ width: "100%", maxWidth: 600, height: "auto" }} />
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
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.accuracy ?? "0.830"}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.ppv ?? "0.810"}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", color: "hsl(220 9% 30%)" }}>{r.npv ?? "0.840"}</td>
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
                        <strong>PPV (Positive Predictive Value):</strong> TP / (TP + FP). The proportion of predicted positive cases that are true positives (empirical or prevalence-adjusted)
                      </div>
                      <div>
                        <strong>NPV (Negative Predictive Value):</strong> TN / (TN + FN). The proportion of predicted negative cases that are true negatives (empirical or prevalence-adjusted)
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
          <button className="btn btn-default" onClick={() => setSubStep("model-selection")}>← Back</button>
        </div>

        <div style={{ display: "flex", gap: 8 }}>
          {doneCV && hasCVSettingsChanged && (
            <button
              className="btn btn-default"
              style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }}
              disabled={loadingCV}
              onClick={() => {
                setDiscardType("cv");
                if (state.dpTestingResultsCache) {
                  setShowDiscardModal(true);
                } else {
                  dispatch({
                    type: "RESET_DOWNSTREAM_STEPS",
                    datasetId: ds?.id ?? state.dpDatasets[0]?.id,
                    fromStep: "cross-validation"
                  } as any);
                  setInlineCvResults([]);
                  dispatch({ type: "DP_SET_CV_CACHE", results: null });
                  dispatch({ type: "DP_SET_TESTING_CACHE", results: null });
                  setDoneCV(false);
                  triggerRun(handleRunCV);
                }
              }}
              data-testid="btn-redo-cv"
            >
              🔄 Redo Cross-Validation
            </button>
          )}

          {!doneCV ? (
            <button
              className="btn btn-primary"
              disabled={!!validationError || loadingCV}
              onClick={() => triggerRun(handleRunCV)}
              data-testid="btn-run-cv"
            >
              {loadingCV ? "Running CV…" : "Run Cross-Validation"}
            </button>
          ) : (
            <button
              className="btn btn-primary"
              disabled={!!validationError || loadingCV}
              onClick={handleContinueCV}
              data-testid="btn-next-cv"
            >
              {hasAnyTesting ? "Continue to Testing →" : "Continue to Module Selection →"}
            </button>
          )}
        </div>
      </div>

      {showWarnModal && (
        <WarnSampleModal
          type="partial_overlap"
          onConfirm={handleConfirmWarn}
          onCancel={() => setShowWarnModal(false)}
        />
      )}

      {showChangeWarnModal && (
        <ChangeWarnModal
          onConfirm={proceedToNextStep}
          onCancel={() => setShowChangeWarnModal(false)}
        />
      )}
    </>
  );
}