import { useLocation } from "wouter";
import { PanelRightClose, PanelRight, Home } from "lucide-react";
import { useAppStore } from "../store/appStore";
import { computeDPSteps, isMetaEligible } from "../dataObject";
import PlotFootnote from "../components/shared/PlotFootnote";
import ReloadButton from "../components/shared/ReloadButton";
import DatasetPanel from "../components/DatasetPanel";
import StepRibbon from "../components/StepRibbon";
import ModuleNav from "../components/shared/ModuleNav";
import DataUploadStep from "../components/dataprocessing/DataUploadStep";
import AllDatasetsUploadView from "../components/shared/AllDatasetsUploadView";
import AnnotationStep from "../components/dataprocessing/AnnotationStep";
import ProcessingStep from "../components/dataprocessing/ProcessingStep";
import CountNormalizationStep from "../components/dataprocessing/CountsNormalizationStep";
import MicroarrayNormalizationStep from "../components/dataprocessing/MicroarrayNormalizationStep";
import BatchEffectsStep from "../components/dataprocessing/BatchEffectsStep";
import ModuleSelectStep from "../components/dataprocessing/ModuleSelectStep";
import InlineDEAnalysisStep from "../components/inline/InlineDEAnalysisStep";
import InlineDEMeta from "../components/inline/InlineDEMeta";
import InlineFSStep from "../components/inline/InlineFSStep";
import DPInlineEAStep from "../components/inline/DPInlineEAStep";
import DPExportStep from "../components/dataprocessing/DPExportStep";

export default function DataProcessing() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();

  const datasets = state.dpDatasets;
  const multipleDs = datasets.length > 1;
  const currentDs = datasets.find(d => d.id === state.dpCurrentDatasetId) ?? datasets[0];

  // Use shared computeDPSteps for ribbon
  const steps = computeDPSteps(datasets);

  // --- Completed steps ---
  const hasMicroarray = datasets.some(d => d.dataType === "microarray");
  const hasReadcounts = datasets.some(d => d.dataType === "readcounts");
  const hasOthers = datasets.some(d => d.dataType === "others");
  const isMixed = multipleDs && new Set(datasets.map(d => d.dataType)).size > 1;

  const completedSteps: string[] = [];
  if (datasets.every(d => d.uploadDone)) {
    completedSteps.push("upload");
    if (multipleDs) completedSteps.push("all-datasets");
  }
  // normalization step: microarray normalization only
  const normTargets = datasets.filter(d => d.dataType === "microarray");
  if (normTargets.length > 0 && normTargets.every(d => d.normalizationDone || d.isNormalized)) {
    completedSteps.push("normalization");
    completedSteps.push("normalization-others"); // legacy compat
  }
  // annotation: readcounts + microarray only (proteomics has no annotation step)
  const annotTargets = datasets.filter(d => {
    if (multipleDs) {
      return d.dataType === "microarray" || d.dataType === "readcounts";
    } else {
      // Single dataset: only microarray and readcounts have annotation
      return d.dataType === "microarray" || d.dataType === "readcounts";
    }
  });
  if (annotTargets.length > 0 && annotTargets.every(d => d.annotationDone)) {
    completedSteps.push("annotation");
  }
  // processing: all types
  if (datasets.every(d => d.processingDone)) completedSteps.push("processing");
  // counts normalization: readcounts, proteomics, and others
  const countsTargets = datasets.filter(
    d => d.dataType === "readcounts" || d.dataType === "proteomics" || d.dataType === "others"
  );
  if (countsTargets.length > 0 && countsTargets.every(d => d.normalizationDone || d.isNormalized)) {
    completedSteps.push("normalization-counts");
  }
  // batch
  if (datasets.every(d => d.batchDone)) completedSteps.push("batch");
  // module select
  if (state.dpInlineDeDone || state.dpInlineFsDone || state.dpInlineEaDone || state.dpStep === "export" || (state.dpSelectedModule !== undefined && state.dpSelectedModule !== null)) {
    completedSteps.push("module-select");
  }

  // --- Step after upload summary ---
  const getNextDPStepAfterUpload = (): string => {
    const activeSteps = computeDPSteps(datasets);
    const summaryIdx = activeSteps.findIndex(s => s.id === "all-datasets");
    if (summaryIdx !== -1 && summaryIdx < activeSteps.length - 1) {
      return activeSteps[summaryIdx + 1].id;
    }
    const uploadIdx = activeSteps.findIndex(s => s.id === "upload");
    return activeSteps[uploadIdx + 1]?.id || "module-select";
  };

  const getDPStepLabel = (step: string) => {
    const found = steps.find(s => s.id === step);
    return found?.label ?? step;
  };

  // --- Viewing context ---
  // For multi-dataset: always "all"
  // For single dataset: current dataset
  const activeDatasetId = currentDs?.id ?? "";
  const displayDatasetId = multipleDs ? "all" : activeDatasetId;

  // Active ribbon step (inline steps map to module-select)
  const inlineSteps = ["inline-de", "inline-de-meta", "inline-fs", "inline-enrichment"];
  const ribbonActiveStep = inlineSteps.includes(state.dpStep) ? "module-select" : state.dpStep;

  return (
    <div style={{ display: "flex", flexDirection: "column", minHeight: "100vh", background: "#eaefff" }}>
      {/* Top ribbon */}
      <div className="ribbon">
        <button
          onClick={() => navigate("/")}
          style={{
            background: "none", border: "none", color: "#101a36",
            cursor: "pointer", display: "flex", alignItems: "center", gap: 6, fontSize: 12,
          }}
          data-testid="btn-home"
        >
          <Home size={14} /> Home
        </button>
        <div className="ribbon-divider" />
        <ModuleNav active="dp" />
        <div className="ribbon-divider" />
        <StepRibbon
          steps={steps}
          currentStep={ribbonActiveStep}
          completedSteps={completedSteps}
          onStepClick={(step) => dispatch({ type: "DP_SET_STEP", step: step as typeof state.dpStep })}
          visitedSteps={state.dpVisitedSteps}
        />
        <div className="ribbon-divider" />
        <ReloadButton />
        <div className="ribbon-divider" />
        <button
          className="panel-toggle"
          onClick={() => dispatch({ type: "DP_TOGGLE_PANEL" })}
          title={state.dpPanelOpen ? "Hide datasets" : "Show datasets"}
          data-testid="btn-toggle-panel"
        >
          {state.dpPanelOpen ? <PanelRightClose size={15} /> : <PanelRight size={15} />}
        </button>
      </div>

      {/* Body */}
      <div className="app-body">
        <div className="content-area">
          <div className="content-inner">

            {/* Viewing bar — always "All Datasets" for multi-ds, otherwise single ds name */}
            {multipleDs ? (
              <div className="dataset-select-bar" style={{ display: "flex", gap: 12, alignItems: "center" }}>
                <span className="dataset-select-label">Viewing:</span>
                <span style={{ fontSize: 13, fontWeight: 600 }}>All Datasets</span>
                <span style={{
                  fontSize: 11, padding: "3px 9px", borderRadius: 5,
                  background: "hsl(214 50% 94%)", color: "hsl(214 58% 30%)",
                  border: "1px solid hsl(214 35% 78%)", fontWeight: 500,
                }}>
                  Settings applied to all {datasets.length} datasets
                </span>
              </div>
            ) : (
              <div className="dataset-select-bar">
                <span className="dataset-select-label">Viewing:</span>
                <span style={{ fontSize: 13, fontWeight: 600 }}>{currentDs?.name ?? "Dataset 1"}</span>
              </div>
            )}

            {/* === Step routing === */}

            {state.dpStep === "upload" && (() => {
              if (multipleDs) {
                const pendingDs = datasets.find(d => !d.uploadDone);
                if (pendingDs) {
                  return <DataUploadStep key={pendingDs.id} datasetId={pendingDs.id} showGeneIdType />;
                }
                // All uploaded — show last dataset or navigate to summary
                return <DataUploadStep key={activeDatasetId} datasetId={activeDatasetId} showGeneIdType />;
              }
              return <DataUploadStep key={activeDatasetId} datasetId={activeDatasetId} showGeneIdType />;
            })()}

            {state.dpStep === "all-datasets" && (() => {
              const nextStep = getNextDPStepAfterUpload();
              return (
                <AllDatasetsUploadView
                  title="All Datasets — Upload Summary"
                  subtitle={`Overview of all ${datasets.length} Data Processing datasets. Review uploads before continuing.`}
                  datasets={datasets}
                  onDatasetClick={(id) => {
                    dispatch({ type: "DP_SELECT_DATASET", id });
                    dispatch({ type: "DP_SET_CONTEXT", id });
                    dispatch({ type: "DP_SET_STEP", step: "upload" });
                  }}
                  onContinue={() => dispatch({ type: "DP_SET_STEP", step: nextStep as typeof state.dpStep })}
                  continueLabel={`Continue to ${getDPStepLabel(nextStep)} →`}
                  onBack={() => {
                    const sorted = [...datasets].sort((a, b) => a.id.localeCompare(b.id));
                    const lastDs = sorted[sorted.length - 1];
                    dispatch({ type: "DP_SELECT_DATASET", id: lastDs.id });
                    dispatch({ type: "DP_SET_CONTEXT", id: lastDs.id });
                    dispatch({ type: "DP_SET_STEP", step: "upload" });
                  }}
                  hasStepMismatch={
                    datasets.some(d => d.annotationDone || d.processingDone) &&
                    datasets.some(d => !d.annotationDone && !d.processingDone)
                  }
                  stepId="upload"
                  datasetIds={datasets.map(d => d.id)}
                  onConfirmDiscard={() => {
                    dispatch({ type: "RESET_DOWNSTREAM_STEPS", datasetId: "all", fromStep: "upload" });
                    dispatch({ type: "DP_SET_STEP", step: nextStep as typeof state.dpStep });
                  }}
                />
              );
            })()}

            {/* Microarray/Others Normalization — used for both mixed and single-type */}
            {(state.dpStep === "normalization" || state.dpStep === "normalization-others") && (
              <MicroarrayNormalizationStep key={displayDatasetId} datasetId={displayDatasetId} mode="dp" />
            )}

            {state.dpStep === "annotation" && (
              <AnnotationStep key={displayDatasetId} datasetId={displayDatasetId} mode="dp" />
            )}

            {state.dpStep === "processing" && (
              <ProcessingStep key={displayDatasetId} datasetId={displayDatasetId} mode="dp" />
            )}

            {state.dpStep === "normalization-counts" && (
              <CountNormalizationStep key={displayDatasetId} datasetId={displayDatasetId} mode="dp" />
            )}

            {state.dpStep === "batch" && (
              <BatchEffectsStep key={displayDatasetId} datasetId={displayDatasetId} mode="dp" />
            )}

            {state.dpStep === "module-select" && <ModuleSelectStep />}

            {state.dpStep === "inline-de" && <InlineDEAnalysisStep />}

            {state.dpStep === "inline-fs" && <InlineFSStep />}

            {state.dpStep === "inline-de-meta" && isMetaEligible(datasets) && <InlineDEMeta />}

            {state.dpStep === "inline-enrichment" && <DPInlineEAStep />}

            {state.dpStep === "export" && (
              <DPExportStep datasetId={displayDatasetId} />
            )}
          </div>
        </div>
        {state.dpPanelOpen && <DatasetPanel mode="dp" />}
      </div>
      <PlotFootnote style={{ borderRadius: "0px", marginTop: "0px", borderTop: "1px solid rgba(255, 255, 255, 0.1)" }} />
    </div>
  );
}