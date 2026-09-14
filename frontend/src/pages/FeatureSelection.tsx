import { useLocation } from "wouter";
import { Home, PanelRightClose, PanelRight } from "lucide-react";
import { useAppStore } from "../store/appStore";
import PlotFootnote from "../components/shared/PlotFootnote";
import ReloadButton from "../components/shared/ReloadButton";
import DatasetPanel from "../components/DatasetPanel";
import StepRibbon from "../components/StepRibbon";
import ModuleNav from "../components/shared/ModuleNav";
import FSUploadStep from "../components/featureselection/FSUploadStep";
import ModelSelectionStep from "../components/featureselection/ModelSelectionStep";
import CrossValidationStep from "../components/featureselection/CrossValidationStep";
import FSExportStep from "../components/featureselection/FSExportStep";
import AllDatasetsUploadView from "../components/shared/AllDatasetsUploadView";
import TestingStep from "../components/featureselection/TestingStep";
import { validateFSSamplesLocal } from "../lib/dataParser";
import type {FSStep} from "../dataObject";


export default function FeatureSelection() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();

  const currentDs = state.fsDatasets.find(d => d.id === state.fsCurrentDatasetId) ?? state.fsDatasets[0];
  const activeDatasetId = currentDs?.id ?? "";
  const isViewingAll = state.fsSelectedContext === "all" && state.fsDatasets.length > 1;
  const displayDatasetId = isViewingAll ? "all" : (state.fsSelectedContext || activeDatasetId);

  const getDynamicSteps = () => {
    const base: { id: FSStep; label: string }[] = [
      { id: "upload", label: "Data Upload" }
    ];
    if (state.fsDatasets.length > 1) {
      base.push({ id: "all-datasets", label: "Upload Summary" });
    }
    base.push({ id: "model-selection", label: "Model Selection" });
    if (state.fsConfig.cvEnabled) {
      base.push({ id: "cross-validation", label: "Cross-Validation" });
    }
    
    // Only count "test" datasets that will actually produce a testing step —
    // datasets with cv-only strategy are evaluated during CV, not a separate testing step.
    const testingDatasets = state.fsDatasets.filter(
      d => d.fs_datasetPurpose === "test" &&
           d.fs_validationStrategy !== "cv-only" &&
           d.fs_validationStrategy !== "train-and-cv"
    );
    const hasInternalTest = state.fsDatasets.some(d => 
      d.fs_datasetPurpose === "train-and-test" || 
      ((d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation)
    );
    const hasAnyTesting = testingDatasets.length > 0 || hasInternalTest;
    
    if (hasAnyTesting) {
      base.push({ id: "testing", label: "Testing" });
    }
    
    base.push({ id: "export", label: "Export" });
    return base;
  };
  const steps = getDynamicSteps();
  const submittedCfg = state.fsSubmittedConfig;
  const hasStepMismatch = Boolean(
    state.standaloneFsDone && (
      !submittedCfg ||
      state.fsDatasets.length !== Object.keys(submittedCfg.fs_trainRatios || {}).length ||
      state.fsDatasets.some(d => !d.uploadDone || !submittedCfg.fs_trainRatios?.[d.id])
    )
  );

  const completedSteps: string[] = [];
  const fsDatasets = state.fsDatasets;

  if (fsDatasets.length > 0 && fsDatasets.every(d => d.uploadDone)) {
    completedSteps.push("upload");
    if (fsDatasets.length > 1) {
      completedSteps.push("all-datasets");
    }
  }
  if (state.fsStep === "cross-validation" || state.fsStep === "testing" || state.fsStep === "export" || state.standaloneFsDone) completedSteps.push("model-selection");
  if (state.fsStep === "testing" || state.fsStep === "export") {
    if (state.fsConfig.cvEnabled) completedSteps.push("cross-validation");
  }
  if (state.fsStep === "export") {
    // Only mark "testing" as completed if it exists in the ribbon
    if (steps.some(s => s.id === "testing")) completedSteps.push("testing");
  }

  // Export configurations delegated to FSExportStep

  return (
    <div style={{ display: "flex", flexDirection: "column", minHeight: "100vh", background: "#eaefff" }}>
      {/* Top ribbon */}
      <div className="ribbon">
        <button
          onClick={() => navigate("/")}
          style={{ background: "none", border: "none", color: "#101a36", cursor: "pointer", display: "flex", alignItems: "center", gap: 6, fontSize: 12 }}
          data-testid="btn-home-fs"
        >
          <Home size={14} /> Home
        </button>
        <div className="ribbon-divider" />

        <ModuleNav active="fs" />

        <div className="ribbon-divider" />

        <StepRibbon
          steps={steps}
          currentStep={state.fsStep}
          completedSteps={completedSteps}
          onStepClick={(step) => dispatch({ type: "FS_SET_STEP", step: step as typeof state.fsStep })}
          visitedSteps={state.fsVisitedSteps}
        />

        <div className="ribbon-divider" />
        <ReloadButton />
        <div className="ribbon-divider" />
        <button
          className="panel-toggle"
          onClick={() => dispatch({ type: "FS_TOGGLE_PANEL" })}
          title={state.fsPanelOpen ? "Hide datasets" : "Show datasets"}
          data-testid="btn-toggle-panel-fs"
        >
          {state.fsPanelOpen ? <PanelRightClose size={15} /> : <PanelRight size={15} />}
        </button>
      </div>

      {/* Body */}
      <div className="app-body">
        <div className="content-area">
          <div className="content-inner">
            {/* Dataset context selector */}
            {state.fsDatasets.length > 1 ? (
              <div className="dataset-select-bar" style={{ display: "flex", gap: 12, alignItems: "center" }}>
                <span className="dataset-select-label">Viewing:</span>
                <span style={{ fontSize: 13, fontWeight: 600 }}>All Datasets</span>
                <span style={{
                  fontSize: 11, padding: "3px 9px", borderRadius: 5,
                  background: "hsl(214 50% 94%)", color: "hsl(214 58% 30%)",
                  border: "1px solid hsl(214 35% 78%)", fontWeight: 500,
                }}>
                  Settings applied to all {state.fsDatasets.length} datasets
                </span>
              </div>
            ) : (
              <div className="dataset-select-bar">
                <span className="dataset-select-label">Viewing:</span>
                <select
                  value={state.fsSelectedContext}
                  onChange={e => dispatch({ type: "FS_SET_CONTEXT", id: e.target.value })}
                  style={{ padding: "6px 10px", border: "1px solid hsl(214 20% 82%)", borderRadius: 7, fontSize: 13, background: "white" }}
                  data-testid="select-dataset-context-fs"
                >
                  {state.fsDatasets.map(d => (
                    <option key={d.id} value={d.id}>{d.name}</option>
                  ))}
                </select>
              </div>
            )}

            {/* Step routing */}
            {state.fsStep === "upload" && (() => {
              const pendingDs = state.fsDatasets.find(d => !d.uploadDone);
              if (pendingDs && isViewingAll) {
                return <FSUploadStep key={pendingDs.id} datasetId={pendingDs.id} />;
              }
              const fallbackId = displayDatasetId === "all" ? activeDatasetId : displayDatasetId;
              return <FSUploadStep key={fallbackId} datasetId={fallbackId}/>;
            })()}
            {state.fsStep === "all-datasets" && (
              <AllDatasetsUploadView
                title="All Datasets — Upload Summary"
                subtitle={`Overview of all ${state.fsDatasets.length} Feature Selection datasets. Select an individual dataset from the panel to upload or configure it.`}
                datasets={state.fsDatasets}
                onDatasetClick={(id) => {
                  dispatch({ type: "FS_SELECT_DATASET", id });
                  dispatch({ type: "FS_SET_CONTEXT", id });
                  dispatch({ type: "FS_SET_STEP", step: "upload" });
                }}
                onContinue={async () => {
                  try {
                    const valRes = validateFSSamplesLocal(state.fsDatasets, state.fsConfig.trainRatio || 0.7);
                    if (!valRes.sufficient) {
                      alert(valRes.message || "Insufficient sample size to continue to Model Selection.");
                      return;
                    }
                  } catch (e) {
                    console.error("FS Validation failed:", e);
                  }
                  dispatch({ type: "FS_SET_STEP", step: "model-selection" });
                }}
                continueLabel="Continue to Model Selection →"
                onBack={() => {
                  const sorted = [...state.fsDatasets].sort((a, b) => a.id.localeCompare(b.id));
                  const lastDs = sorted[sorted.length - 1];
                  dispatch({ type: "FS_SELECT_DATASET", id: lastDs.id });
                  dispatch({ type: "FS_SET_CONTEXT", id: lastDs.id });
                  dispatch({ type: "FS_SET_STEP", step: "upload" });
                }}
                hasStepMismatch={hasStepMismatch}
                stepId="upload"
                datasetIds={state.fsDatasets.map(d => d.id)}
                onConfirmDiscard={() => {
                  dispatch({ type: "RESET_DOWNSTREAM_STEPS", datasetId: "all", fromStep: "model-selection" });
                  dispatch({ type: "FS_SET_STEP", step: "model-selection" });
                }}
              />
            )}
            {state.fsStep === "model-selection" && (
              <ModelSelectionStep />
            )}
            {state.fsStep === "cross-validation" && (
              <CrossValidationStep />
            )}
            {state.fsStep === "testing" && (
              <TestingStep mode="fs" />
            )}
            {state.fsStep === "export" && (
              <FSExportStep />
            )}
          </div>
        </div>
        {state.fsPanelOpen && <DatasetPanel mode="fs" />}
      </div>
      <PlotFootnote style={{ borderRadius: "0px", marginTop: "0px", borderTop: "1px solid rgba(255, 255, 255, 0.1)" }} />
    </div>
  );
}
