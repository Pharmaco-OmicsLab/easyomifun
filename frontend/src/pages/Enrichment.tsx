import { useLocation } from "wouter";
import { Home, PanelRightClose, PanelRight } from "lucide-react";
import { useAppStore } from "../store/appStore";
import PlotFootnote from "../components/shared/PlotFootnote";
import ReloadButton from "../components/shared/ReloadButton";
import DatasetPanel from "../components/DatasetPanel";
import StepRibbon from "../components/StepRibbon";
import ModuleNav from "../components/shared/ModuleNav";
import EnrichmentUploadStep from "../components/enrichment/EnrichmentUploadStep";
import EnrichmentAnalysisStep from "../components/enrichment/EnrichmentAnalysisStep";
import EAExportStep from "../components/enrichment/EAExportStep";
import AllDatasetsUploadView from "../components/shared/AllDatasetsUploadView";
import { EN_STEPS } from "../dataObject";

export default function Enrichment() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();

  const enDatasets = state.enDatasets;
  const currentDs = enDatasets.find(d => d.id === state.enCurrentDatasetId) ?? enDatasets[0];
  const activeDatasetId = currentDs?.id ?? "";
  const isViewingAll = state.enSelectedContext === "all" && enDatasets.length > 1;
  const displayDatasetId = isViewingAll ? "all" : (state.enSelectedContext || activeDatasetId);

  const steps = EN_STEPS;

  const completedSteps: string[] = [];
  if (enDatasets.every(d => d.uploadDone)) completedSteps.push("upload");
  if (state.enStep === "export") completedSteps.push("analysis");

  // Export configurations delegated to EAExportStep

  return (
    <div style={{ display: "flex", flexDirection: "column", minHeight: "100vh", background: "#eaefff" }}>
      {/* Top ribbon */}
      <div className="ribbon">
        <button
          onClick={() => navigate("/")}
          style={{ background: "none", border: "none", color: "#101a36", cursor: "pointer", display: "flex", alignItems: "center", gap: 6, fontSize: 12 }}
          data-testid="btn-home-enrichment"
        >
          <Home size={14} /> Home
        </button>
        <div className="ribbon-divider" />

        <ModuleNav active="ea" />

        <div className="ribbon-divider" />

        <StepRibbon
          steps={steps}
          currentStep={state.enStep}
          completedSteps={completedSteps}
          onStepClick={(step) => dispatch({ type: "EN_SET_STEP", step: step as typeof state.enStep })}
          visitedSteps={state.enVisitedSteps}
        />

        <div className="ribbon-divider" />
        <ReloadButton />
        <div className="ribbon-divider" />
        <button
          className="panel-toggle"
          onClick={() => dispatch({ type: "EN_TOGGLE_PANEL" })}
          title={state.enPanelOpen ? "Hide datasets" : "Show datasets"}
          data-testid="btn-toggle-panel-en"
        >
          {state.enPanelOpen ? <PanelRightClose size={15} /> : <PanelRight size={15} />}
        </button>
      </div>

      {/* Body */}
      <div className="app-body">
        <div className="content-area">
          <div className="content-inner">
            <div className="dataset-select-bar">
              <span className="dataset-select-label">Viewing:</span>
              <span style={{ fontSize: 13, fontWeight: 600 }}>{state.enDatasets[0]?.name || "Dataset"}</span>
            </div>

            {/* Step content */}
            {state.enStep === "upload" && (() => {
              const pendingDs = state.enDatasets.find(d => !d.uploadDone);
              if (pendingDs && isViewingAll) {
                return <EnrichmentUploadStep key={pendingDs.id} datasetId={pendingDs.id} />;
              } else if (isViewingAll) {
                return (
                  <AllDatasetsUploadView
                    title="All Datasets — Upload Summary"
                    subtitle={`Overview of all ${state.enDatasets.length} Enrichment Analysis datasets. Select an individual dataset from the panel to upload or configure it.`}
                    datasets={state.enDatasets}
                    onDatasetClick={(id) => {
                      dispatch({ type: "EN_SELECT_DATASET", id });
                      dispatch({ type: "EN_SET_CONTEXT", id });
                    }}
                    onContinue={() => {
                      dispatch({ type: "EN_SET_STEP", step: "analysis" });
                    }}
                    continueLabel="Continue to Analysis →"
                    onBack={() => {
                      const sorted = [...state.enDatasets].sort((a, b) => a.id.localeCompare(b.id));
                      const lastDs = sorted[sorted.length - 1];
                      dispatch({ type: "EN_SELECT_DATASET", id: lastDs.id });
                      dispatch({ type: "EN_SET_CONTEXT", id: lastDs.id });
                    }}
                  />
                );
              }
              return <EnrichmentUploadStep key={displayDatasetId} datasetId={displayDatasetId} />;
            })()}
            {state.enStep === "analysis" && (
              <EnrichmentAnalysisStep />
            )}
            {state.enStep === "export" && (
              <EAExportStep />
            )}
          </div>
        </div>
        {state.enPanelOpen && <DatasetPanel mode="en" />}
      </div>
      <PlotFootnote style={{ borderRadius: "0px", marginTop: "0px", borderTop: "1px solid rgba(255, 255, 255, 0.1)" }} />
    </div>
  );
}
