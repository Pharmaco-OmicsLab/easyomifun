import { useEffect } from "react";
import { useLocation } from "wouter";
import { Home, PanelRightClose, PanelRight } from "lucide-react";
import { useAppStore } from "../store/appStore";
import PlotFootnote from "../components/shared/PlotFootnote";
import ReloadButton from "../components/shared/ReloadButton";
import DatasetPanel from "../components/DatasetPanel";
import StepRibbon from "../components/StepRibbon";
import ModuleNav from "../components/shared/ModuleNav";
import DEUploadStep from "../components/de/DEUploadStep";
import DEAnalysisStep from "../components/de/DEAnalysisStep";
import MetaAnalysisStep from "../components/de/MetaAnalysisStep";
import DEModuleSelectStep from "../components/de/DEModuleSelectStep";
import DEInlineEAStep from "../components/inline/DEInlineEAStep";
import DEExportStep from "../components/de/DEExportStep";
import AllDatasetsUploadView from "../components/shared/AllDatasetsUploadView";
import { DE_STEPS, DE_STEPS_MULTI, isMetaEligible } from "../dataObject";

export default function DEAnalysis() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();

  const multipleDs = state.deDatasets.length > 1;
  const isViewingAll = state.deSelectedContext === "all" && multipleDs;
  
  const hasSameTypeMultiple = isMetaEligible(state.deDatasets);

  useEffect(() => {
    if (state.deStep === "meta" && !hasSameTypeMultiple) {
      dispatch({ type: "DE_SET_STEP", step: "module-select" });
    }
  }, [state.deStep, hasSameTypeMultiple, dispatch]);

  const baseSteps = multipleDs
    ? (hasSameTypeMultiple ? DE_STEPS_MULTI : DE_STEPS_MULTI.filter(s => s.id !== "meta"))
    : DE_STEPS;
  const steps = baseSteps.filter(s => !(state.deMetaSkipped && s.id === "module-select"));

  const currentDs = state.deDatasets.find(d => d.id === state.deCurrentDatasetId) ?? state.deDatasets[0];
  const activeDatasetId = currentDs?.id ?? state.deDatasets[0]?.id ?? "";
  const displayDatasetId = isViewingAll ? "all" : (state.deSelectedContext || activeDatasetId);

  const isDeDoneForDataset = (targetDs: any) => {
    if (!targetDs || !state.deResults) return false;
    if (state.deResults[targetDs.id]) return true;
    const baseId = targetDs.id.replace(/_(dp|de|fs|enrichment|en|ea)(.*)$/, "");
    return !!Object.entries(state.deResults).find(([k, v]) => {
      const kBaseId = k.replace(/_(dp|de|fs|enrichment|en|ea)(.*)$/, "");
      return kBaseId === baseId && v;
    });
  };

  const completedSteps: string[] = [];
  const deDatasets = state.deDatasets;
  if (isViewingAll) {
    if (deDatasets.length > 0 && deDatasets.every(d => d.uploadDone)) {
      completedSteps.push("upload");
      completedSteps.push("all-datasets");
    }
    if (deDatasets.length > 0 && deDatasets.every(isDeDoneForDataset)) {
      completedSteps.push("analysis");
    }
    if (state.deMetaMethod) {
      completedSteps.push("meta");
    }
  } else {
    const ds = deDatasets.find(d => d.id === displayDatasetId);
    if (ds) {
      if (ds.uploadDone) {
        completedSteps.push("upload");
      }
      if (isDeDoneForDataset(ds)) {
        completedSteps.push("analysis");
      }
    }
  }

  if (state.deStep === "export" || state.deStep === "inline-enrichment" || (state.deSelectedModule !== undefined && state.deSelectedModule !== null)) {
    completedSteps.push("module-select");
  }



  let ribbonActiveStep = state.deStep === "inline-enrichment" ? "module-select" : state.deStep;
  if (ribbonActiveStep === "meta" && !hasSameTypeMultiple) {
    ribbonActiveStep = "module-select";
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", minHeight: "100vh", background: "#eaefff" }}>
      {/* Top ribbon */}
      <div className="ribbon">
        <button
          onClick={() => navigate("/")}
          style={{ background: "none", border: "none", color: "#101a36", cursor: "pointer", display: "flex", alignItems: "center", gap: 6, fontSize: 12 }}
          data-testid="btn-home-de"
        >
          <Home size={14} /> Home
        </button>
        <div className="ribbon-divider" />

        <ModuleNav active="de" />

        <div className="ribbon-divider" />

        <StepRibbon
          steps={steps}
          currentStep={ribbonActiveStep}
          completedSteps={completedSteps}
          skippedSteps={(hasSameTypeMultiple && state.deMetaSkipped) ? ["meta"] : []}
          onStepClick={(step) => dispatch({ type: "DE_SET_STEP", step: step as typeof state.deStep })}
          visitedSteps={state.deVisitedSteps}
        />

        <div className="ribbon-divider" />
        <ReloadButton />
        <div className="ribbon-divider" />
        <button
          className="panel-toggle"
          onClick={() => dispatch({ type: "DE_TOGGLE_PANEL" })}
          title={state.dePanelOpen ? "Hide datasets" : "Show datasets"}
          data-testid="btn-toggle-panel-de"
        >
          {state.dePanelOpen ? <PanelRightClose size={15} /> : <PanelRight size={15} />}
        </button>
      </div>

      {/* Body */}
      <div className="app-body">
        <div className="content-area">
          <div className="content-inner">
            {multipleDs ? (
              <div className="dataset-select-bar" style={{ display: "flex", gap: 12, alignItems: "center" }}>
                <span className="dataset-select-label">Viewing:</span>
                <span style={{ fontSize: 13, fontWeight: 600 }}>All Datasets</span>
                <span style={{
                  fontSize: 11, padding: "3px 9px", borderRadius: 5,
                  background: "hsl(150 50% 94%)", color: "hsl(150 58% 28%)",
                  border: "1px solid hsl(150 35% 78%)", fontWeight: 500,
                }}>
                  Settings applied to all {state.deDatasets.length} datasets
                </span>
                {hasSameTypeMultiple ? (
                  <span style={{
                    fontSize: 11, padding: "3px 9px", borderRadius: 5,
                    background: "hsl(38 70% 96%)", color: "hsl(38 65% 30%)",
                    border: "1px solid hsl(38 50% 78%)", fontWeight: 500,
                  }}>
                    ⚡ Multiple datasets — Meta-analysis enabled
                  </span>
                ) : (
                  <span style={{
                    fontSize: 11, padding: "3px 9px", borderRadius: 5,
                    background: "hsl(210 20% 94%)", color: "hsl(210 20% 30%)",
                    border: "1px solid hsl(210 20% 78%)", fontWeight: 500,
                  }}>
                    ⚡ Multiple datasets — Meta-analysis not available (different tech types)
                  </span>
                )}
              </div>
            ) : (
              <div className="dataset-select-bar">
                <span className="dataset-select-label">Viewing:</span>
                <select
                  value={state.deSelectedContext}
                  onChange={e => dispatch({ type: "DE_SET_CONTEXT", id: e.target.value })}
                  style={{ padding: "6px 10px", border: "1px solid hsl(214 20% 82%)", borderRadius: 7, fontSize: 13, background: "white" }}
                  data-testid="select-dataset-context-de"
                >
                  {state.deDatasets.map(d => (
                    <option key={d.id} value={d.id}>{d.name}</option>
                  ))}
                </select>
              </div>
            )}

            {/* Step content */}
            {state.deStep === "upload" && (() => {
              const pendingDs = state.deDatasets.find(d => !d.uploadDone);
              if (pendingDs && isViewingAll) {
                return <DEUploadStep key={pendingDs.id} datasetId={pendingDs.id} />;
              }
              const fallbackId = displayDatasetId === "all" ? activeDatasetId : displayDatasetId;
              return <DEUploadStep key={fallbackId} datasetId={fallbackId} />;
            })()}
            {state.deStep === "all-datasets" && (
              <AllDatasetsUploadView
                title="All Datasets — Upload Summary"
                subtitle={`Overview of all ${state.deDatasets.length} DE Analysis datasets. Select an individual dataset from the panel to upload or configure it.`}
                datasets={state.deDatasets}
                onDatasetClick={(id) => {
                  dispatch({ type: "DE_SELECT_DATASET", id });
                  dispatch({ type: "DE_SET_CONTEXT", id });
                  dispatch({ type: "DE_SET_STEP", step: "upload" });
                }}
                onContinue={() => {
                  dispatch({ type: "DE_SET_STEP", step: "analysis" });
                }}
                continueLabel="Continue to Analysis →"
                onBack={() => {
                  const sorted = [...state.deDatasets].sort((a, b) => a.id.localeCompare(b.id));
                  const lastDs = sorted[sorted.length - 1];
                  dispatch({ type: "DE_SELECT_DATASET", id: lastDs.id });
                  dispatch({ type: "DE_SET_CONTEXT", id: lastDs.id });
                  dispatch({ type: "DE_SET_STEP", step: "upload" });
                }}
              />
            )}
            {state.deStep === "analysis" && (
              <DEAnalysisStep key={displayDatasetId} datasetId={displayDatasetId} />
            )}
            {state.deStep === "meta" && multipleDs && hasSameTypeMultiple && (
              <MetaAnalysisStep />
            )}
            {state.deStep === "module-select" && !state.deMetaSkipped && (
              <DEModuleSelectStep />
            )}
            {state.deStep === "inline-enrichment" && (
              <DEInlineEAStep />
            )}
            {state.deStep === "export" && (
              <DEExportStep />
            )}
          </div>
        </div>
        {state.dePanelOpen && <DatasetPanel mode="de" />}
      </div>
      <PlotFootnote style={{ borderRadius: "0px", marginTop: "0px", borderTop: "1px solid rgba(255, 255, 255, 0.1)" }} />
    </div>
  );
}
