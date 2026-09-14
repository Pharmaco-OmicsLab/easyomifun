import { useAppStore } from "../../store/appStore";
import { computeDPSteps, isEAEligible } from "../../dataObject";
import type { SelectedModule } from "../../dataObject";

export default function ModuleSelectStep() {
  const { state, dispatch } = useAppStore();
  const selected = state.dpSelectedModule;

  const ds = state.dpDatasets.find(d => d.id === state.dpCurrentDatasetId) ?? state.dpDatasets[0];
  const allDs = state.dpDatasets;
  const isViewingAll = state.dpSelectedContext === "all" && allDs.length > 1;
  const targetDatasets = isViewingAll ? allDs : (ds ? [ds] : []);
  
  const totalSamples = targetDatasets.reduce((sum, d) => sum + (d.nSamples || 0), 0);
  const canRenderFS = totalSamples >= 30;

  // Dynamically build the list of available modules

  const modules = [
    {
      id: null as SelectedModule, icon: "📦", title: "Export Only",
      desc: "Export processed and normalized expression data directly. No downstream analysis.",
    },
    {
      id: "de" as SelectedModule, icon: "📊", title: "Add Differential Expression",
      desc: "Run Differential Expression Analysis on the processed data without re-uploading",
      badge: "DE Analysis",
    },
    // 1. Conditionally insert Enrichment Analysis right after DE
    ...(state.dpInlineDeDone && isEAEligible(state.dpDatasets, state.dpInlineMetaDone) ? [{
      id: "enrichment" as SelectedModule, icon: "🧬", title: "Add Enrichment Analysis",
      desc: "Run pathway enrichment analysis (ORA / GSEA) on the differentially expressed genes",
      badge: "Enrichment Analysis",
    }] : []),
    // 2. Feature Selection conditionally goes last
    ...(canRenderFS ? [{
      id: "feature-selection" as SelectedModule, icon: "🔍", title: "Add Feature Selection",
      desc: "Apply machine learning model to select relevant features of the processed data",
      badge: "Feature Selection",
    }] : []),
  ];

  const handleContinue = () => {
    if (selected === "de") {
      dispatch({ type: "DP_SET_STEP", step: "inline-de" });
    } else if (selected === "feature-selection") {
      dispatch({ type: "DP_SET_STEP", step: "inline-fs" });
    } else if (selected === "enrichment") {
      dispatch({ type: "DP_SET_STEP", step: "inline-enrichment" });
    } else {
      dispatch({ type: "DP_SET_STEP", step: "export" });
    }
  };

  const getButtonText = () => {
    if (selected === "de") {
      return state.dpInlineDeDone ? "Redo DE analysis" : "Configure DE Analysis →";
    }
    if (selected === "feature-selection") {
      return state.dpInlineFsDone ? "Redo Feature Selection" : "Configure Feature Selection →";
    }
    if (selected === "enrichment") {
      return state.dpInlineEaDone ? "Redo Enrichment Analysis" : "Configure Enrichment Analysis →";
    }
    return "Continue to Export →";
  };

  return (
    <>
      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Choose An Analysis Module</div>
        <div className="card-sub">
          Select an optional downstream analysis to run on your processed data. The export package will include all results.
        </div>
        <hr className="card-divider" />

        <div className="module-choice-grid">
          {modules.map(m => (
            <div
              key={String(m.id)}
              className={`module-choice-card ${selected === m.id ? "selected" : ""}`}
              onClick={() => dispatch({ type: "DP_SET_MODULE", module: m.id })}
              data-testid={`module-card-${m.id ?? "export"}`}
            >
              <div className="icon">{m.icon}</div>
              <div className="title">{m.title}</div>
              {m.badge && (
                <div style={{
                  display: "inline-block", marginBottom: 8, fontSize: 10, fontWeight: 700,
                  padding: "2px 8px", borderRadius: 4,
                  background: "var(--selected-bg)", color: "var(--foreground)",
                  border: "1px solid var(--border)",
                }}>
                  {m.badge}
                </div>
              )}
              {m.id === "de" && state.dpInlineDeDone && (
                <div style={{
                  display: "inline-block", marginBottom: 8, fontSize: 10, fontWeight: 700,
                  padding: "2px 8px", borderRadius: 4, marginLeft: 6,
                  background: "hsl(142 70% 93%)", color: "hsl(142 72% 29%)",
                  border: "1px solid hsl(142 70% 80%)",
                }} data-testid="badge-de-done">
                  ✓ Run Completed
                </div>
              )}
              {m.id === "enrichment" && state.dpInlineEaDone && (
                <div style={{
                  display: "inline-block", marginBottom: 8, fontSize: 10, fontWeight: 700,
                  padding: "2px 8px", borderRadius: 4, marginLeft: 6,
                  background: "hsl(142 70% 93%)", color: "hsl(142 72% 29%)",
                  border: "1px solid hsl(142 70% 80%)",
                }} data-testid="badge-en-done">
                  ✓ Run Completed
                </div>
              )}
              {m.id === "feature-selection" && state.dpInlineFsDone && (
                <div style={{
                  display: "inline-block", marginBottom: 8, fontSize: 10, fontWeight: 700,
                  padding: "2px 8px", borderRadius: 4, marginLeft: 6,
                  background: "hsl(142 70% 93%)", color: "hsl(142 72% 29%)",
                  border: "1px solid hsl(142 70% 80%)",
                }} data-testid="badge-fs-done">
                  ✓ Run Completed
                </div>
              )}
              <div className="desc">{m.desc}</div>
              {selected === m.id && (
                <div style={{ marginTop: 12, fontSize: 12, color: "var(--foreground)", fontWeight: 600 }}>✓ Selected</div>
              )}
            </div>
          ))}
        </div>

        {selected === "de" && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>DE Analysis</strong> - Configure group comparisons and run statistical testing on your processed expression data. No re-upload needed.
          </div>
        )}
        {selected === "enrichment" && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>Enrichment Analysis</strong> - Run Over-Representation Analysis (ORA) or Gene Set Enrichment Analysis (GSEA) on your DE results.
          </div>
        )}
        {selected === "feature-selection" && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>Feature Selection</strong>- Select ML models and configure parameters for feature selection. Uses your normalized expression matrix directly.
          </div>
        )}
        {selected === null && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>Export Only</strong> - Your export will include: intermediate files and final processed matrix.
          </div>
        )}
      </div>

      <div className="action-row">
        <button
          className="btn btn-default"
          onClick={() => {
            const activeSteps = computeDPSteps(state.dpDatasets);
            const currentIdx = activeSteps.findIndex(s => s.id === "module-select");
            const prevStep = currentIdx !== -1 ? activeSteps[currentIdx - 1]?.id : null;
            dispatch({
              type: "DP_SET_STEP",
              step: (prevStep || (state.dpDatasets.length > 1 ? "batch" : "normalization")) as any
            });
          }}
        >
          ← Back
        </button>
        <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-continue-to-export">
          {getButtonText()}
        </button>
      </div>
    </>
  );
}
