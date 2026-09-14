import { useAppStore } from "../../store/appStore";
import { isMetaEligible, isEAEligible } from "../../dataObject";

export default function DEModuleSelectStep() {
  const { state, dispatch } = useAppStore();
  const selected = state.deSelectedModule;

  const metaDone = !state.deMetaSkipped && isMetaEligible(state.deDatasets);
  const eaEligible = isEAEligible(state.deDatasets, metaDone);

  // Compute whether meta-analysis is available (same-type multiple datasets exist)
  const hasSameTypeMultiple = isMetaEligible(state.deDatasets);
  const multipleDs = state.deDatasets.length > 1;

  const options = [
    {
      id: null,
      icon: "📦",
      title: "Export Only",
      desc: "Export the differential expression results directly. No further downstream pathway analysis.",
    },
    ...(eaEligible ? [{
      id: "enrichment",
      icon: "🧬",
      title: "Add Enrichment Analysis",
      desc: "Run pathway enrichment analysis (ORA / GSEA) on the differentially expressed genes identified from this DE analysis.",
      badge: "Enrichment Analysis",
    }] : []),
  ];

  const handleContinue = () => {
    if (selected === "enrichment") {
      dispatch({ type: "DE_SET_STEP", step: "inline-enrichment" });
    } else {
      dispatch({ type: "DE_SET_STEP", step: "export" });
    }
  };

  const getButtonText = () => {
    if (selected === "enrichment") {
      return state.deInlineEaDone ? "Redo Enrichment Analysis" : "Configure Enrichment Analysis →";
    }
    return "Continue to Export →";
  };


  return (
    <>
      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Choose An Analysis Module</div>
        <div className="card-sub">
          Select an optional downstream analysis to run on your DE results. The export package will include all results.
        </div>
        <hr className="card-divider" />

        <div className="module-choice-grid">
          {options.map(m => (
            <div
              key={String(m.id)}
              className={`module-choice-card ${selected === m.id ? "selected" : ""}`}
              onClick={() => dispatch({ type: "DE_SET_MODULE", module: m.id as any })}
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
              {m.id === "enrichment" && state.deInlineEaDone && (
                <div style={{
                  display: "inline-block", marginBottom: 8, fontSize: 10, fontWeight: 700,
                  padding: "2px 8px", borderRadius: 4, marginLeft: 6,
                  background: "hsl(142 70% 93%)", color: "hsl(142 72% 29%)",
                  border: "1px solid hsl(142 70% 80%)",
                }} data-testid="badge-en-done">
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

        {selected === "enrichment" && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>Enrichment Analysis</strong> - Run Over-Representation Analysis (ORA) or Gene Set Enrichment Analysis (GSEA) on your DE results.
          </div>
        )}
        {selected === null && (
          <div className="banner info" style={{ marginTop: 16, marginBottom: 0 }}>
            <strong>Export Only</strong> - Your export will include the DE analysis results and gene lists.
          </div>
        )}
      </div>

      <div className="action-row">
        <button
          className="btn btn-default"
          onClick={() => dispatch({ type: "DE_SET_STEP", step: (multipleDs && hasSameTypeMultiple) ? "meta" : "analysis" })}
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
