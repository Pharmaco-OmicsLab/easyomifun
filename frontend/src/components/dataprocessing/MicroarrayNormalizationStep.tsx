import { useState, useEffect } from "react";
import { useAppStore } from "../../store/appStore";
import { computeDPSteps } from "../../dataObject";
import Spinner from "../Spinner";
import SkipModal from "../shared/SkipModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import type { NormMethod } from "../../dataObject";
import { normalizeDatasetAPI, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";


interface Props { datasetId: string; mode?: "dp" | "de" }

// Always Quantile + VSN + No Normalization — regardless of platform or dataType
const NORM_METHODS: { value: NormMethod; label: string; desc: string }[] = [
  { value: "quantile", label: "Quantile Normalization", desc: "Forces sample intensity distributions to be identical — standard for microarray and high-throughput data" },
  { value: "vsn", label: "VSN (Variance Stabilizing Normalization)", desc: "Fits a model combining variance stabilization and calibration" },
];

function NormalizationResultCard({ targetDs, submittedCfg }: { targetDs: any; submittedCfg: any }) {
  return (
    <div className="result-ds-card" style={{ margin: 0 }}>
      <div className="result-ds-card-header" style={{ marginBottom: 12 }}>
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: targetDs.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>
          {targetDs.name} Normalization Results
        </div>
      </div>
      
      <div className="banner success" style={{ marginBottom: 12 }}>
        ✅ Normalization complete using <strong>{NORM_METHODS.find(m => m.value === submittedCfg?.method)?.label || "TMM"}</strong>{submittedCfg?.logTransform ? ` with ${submittedCfg?.transformationType === "log10" ? "log₁₀" : "log₂"} transformation` : ""}.
      </div>
      
      <div className="chips-row">
        <div className="stat-chip">
          <div className="stat-chip-val">{(targetDs.nFeatures ?? 0).toLocaleString()}</div>
          <div className="stat-chip-lbl">Features</div>
        </div>
        <div className="stat-chip">
          <div className="stat-chip-val">{(targetDs.nSamples ?? 0).toLocaleString()}</div>
          <div className="stat-chip-lbl">Samples</div>
        </div>
      </div>
    </div>
  );
}

export default function MicroarrayNormalizationStep({ datasetId, mode = "dp" }: Props) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const cfg = state.dpNormConfig;
  const [loading, setLoading] = useState(false);
  const [showSkipModal, setShowSkipModal] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const datasets = mode === "dp" ? state.dpDatasets : state.deDatasets;
  const ds = datasets.find(d => d.id === datasetId);
  const multipleDs = datasets.length > 1;

  // Which datasets need normalization here (microarray ONLY - others/proteomics now go through CountsNormalizationStep)
  const allTarget = (datasetId === "all" ? datasets : (ds ? [ds] : []));
  // Only show cards for unnormalized microarray datasets
  const maForDisplay = allTarget.filter(d => d.dataType === "microarray" && !d.isNormalized);
  // For targeting normalization run
  const maTarget = maForDisplay;

  // All target datasets for done-checking (microarray only)
  const normTargets = [...maTarget];
  const done = normTargets.length === 0 || normTargets.every(d => d.normalizationDone);

  // Default to "quantile" for microarray normalization, with logTransform enabled by default
  const [maMethod, setMaMethod] = useState<NormMethod>("quantile");
  const [maPrior, setMaPrior] = useState(0.5);

  const [transformEnabled, setTransformEnabled] = useState(() => {
    if (state.dpSubmittedNormConfig) {
      return state.dpSubmittedNormConfig.logTransform;
    }
    return true;
  });
  const [transformType, setTransformType] = useState<"log2" | "log10">(() => {
    if (state.dpSubmittedNormConfig?.transformationType) {
      return state.dpSubmittedNormConfig.transformationType as "log2" | "log10";
    }
    return "log2";
  });

  const submittedCfg = state.dpSubmittedNormConfig;
  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.method !== maMethod ||
    submittedCfg.logTransform !== transformEnabled ||
    (transformEnabled && submittedCfg.transformationType !== transformType) ||
    (transformEnabled && submittedCfg.priorCount !== maPrior)
  );

  const completedModules: string[] = [];
  normTargets.forEach(d => {
    if (d.annotationDone && !completedModules.includes("Annotation")) completedModules.push("Annotation");
    if (d.batchDone && !completedModules.includes("Batch Correction")) completedModules.push("Batch Correction");
    if ((state.dpInlineDeDone || (state.dpDeResults[d.id] && state.dpDeResults[d.id].length > 0)) && !completedModules.includes("DE Analysis")) completedModules.push("DE Analysis");
  });
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");
  const downstreamDone = completedModules.length > 0;

  const [activeTabId, setActiveTabId] = useState<string>("");

  useEffect(() => {
    if (normTargets.length > 0) {
      if (!activeTabId || !normTargets.some(d => d.id === activeTabId)) {
        setActiveTabId(normTargets[0].id);
      }
    }
  }, [normTargets, activeTabId]);

  useEffect(() => {
    if (done && !state.dpSubmittedNormConfig) {
      dispatch({ type: "DP_SUBMIT_NORM" });
    }
  }, [done, state.dpSubmittedNormConfig, dispatch]);

  const getNextAndPrevSteps = () => {
    const activeSteps = computeDPSteps(datasets);
    const currentStepId = state.dpStep === "normalization-others" ? "normalization-others" : "normalization";
    const currentIdx = activeSteps.findIndex(s => s.id === currentStepId || s.id === "normalization");
    const next = activeSteps[currentIdx + 1]?.id || "module-select";
    let prev = activeSteps[currentIdx - 1]?.id || "upload";
    if (prev === "upload" && multipleDs) prev = "all-datasets";
    return { next, prev };
  };

  const runNorm = async () => {
    setLoading(true);
    try {
      const isBuiltIn = maMethod === "vsn";
      const activeConfig = {
        method: maMethod,
        logTransform: !isBuiltIn && transformEnabled,
        transformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
        priorCount: isBuiltIn ? 0 : maPrior
      };
      const results = await normalizeDatasetAPI(normTargets, activeConfig as any);

      dispatch({ type: "DP_SET_NORM", patch: activeConfig });
      dispatch({ type: "DP_SUBMIT_NORM" });
      results.forEach(res => {
        const targetDs = normTargets.find(d => d.id === res.datasetId);
        dispatch({
          type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
          id: res.datasetId,
          patch: {
            normalizationDone: true,
            normalizationMethod: maMethod,
            normalizationLogTransform: !isBuiltIn && transformEnabled,
            normalizationTransformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
            parsedData: res.parsedData || undefined,
            columns: res.columns || undefined,
            sampleIds: res.sampleIds || undefined,
            boxplotBefore: res.boxplotBefore || undefined,
            boxplotAfter: res.boxplotAfter || undefined,
            nFeatures: res.nFeatures || res.retainedFeatures || (res.parsedData && res.parsedData.length > 0 ? res.parsedData.length : (targetDs?.nFeatures || 0)),
            nSamples: res.nSamples !== undefined ? res.nSamples : (targetDs?.nSamples || 0),
          }
        });
      });
    } catch (err: any) {
      console.error("Normalization error:", err);
      toast({
        title: "Normalization Error",
        description: err.message || "An error occurred during microarray normalization.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const handleContinue = () => {
    if (hasSettingsChanged) { setShowWarnModal(true); return; }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowWarnModal(false);
    normTargets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { normalizationDone: true } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const handleSkip = () => {
    normTargets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { normalizationDone: false } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const cardTitle = `Microarray Normalization for ${multipleDs ? "Datasets" : "Dataset"}`;

  return (
    <>
      {showSkipModal && <SkipModal stepName="Normalization" stepId="normalization" datasetIds={normTargets.map(d => d.id)} onConfirm={handleSkip} onCancel={() => setShowSkipModal(false)} />}
      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="normalization"
          datasetIds={normTargets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({ type: "RESET_DOWNSTREAM_STEPS", datasetId, fromStep: "normalization" } as any);
            await runNorm();
          }}
        />
      )}
      {loading && <Spinner label="Applying normalization…" sublabel={`Method: ${NORM_METHODS.find(m => m.value === maMethod)?.label || "Loading"}`} />}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>{cardTitle}</div>
        <div className="card-sub">
          Preprocessing normalization and transformation for microarray data.
          {allTarget.some(d => d.isNormalized) && <span style={{ marginLeft: 8, color: "hsl(38 70% 40%)", fontWeight: 500 }}>⚠ Some datasets are pre-normalized.</span>}
        </div>
        <hr className="card-divider" style={{ marginBottom: 16 }} />

        {/* Transformation Section */}
        <div style={{
          border: "1px solid var(--border)",
          borderRadius: 10,
          padding: "14px 16px",
          background: "hsl(210 20% 98%)",
          marginBottom: 16
        }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 10, flexWrap: "wrap", gap: 8 }}>
            <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)" }}>Transformation</div>
            {maMethod === "vsn" ? (
              <div style={{ fontSize: 12, fontWeight: 500, color: "var(--muted-foreground)" }}>
                managed automatically by VSN
              </div>
            ) : (
              <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 12, fontWeight: 600, cursor: "pointer" }}>
                <input
                  type="checkbox"
                  checked={transformEnabled}
                  onChange={(e) => setTransformEnabled(e.target.checked)}
                  style={{ accentColor: "var(--primary)" }}
                  data-testid="check-transform-enable"
                />
                Enable Data Transformation
              </label>
            )}
          </div>
          
          {transformEnabled && maMethod !== "vsn" && (
            <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
              <div style={{ display: "flex", gap: 16, alignItems: "center", flexWrap: "wrap" }}>
                <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, cursor: "pointer" }}>
                  <input
                    type="radio"
                    name="transform-type-microarray"
                    value="log2"
                    checked={transformType === "log2"}
                    onChange={() => setTransformType("log2")}
                    style={{ accentColor: "var(--primary)" }}
                    data-testid="radio-transform-log2"
                  />
                  Log₂ transformation
                </label>
                <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, cursor: "pointer" }}>
                  <input
                    type="radio"
                    name="transform-type-microarray"
                    value="log10"
                    checked={transformType === "log10"}
                    onChange={() => setTransformType("log10")}
                    style={{ accentColor: "var(--primary)" }}
                    data-testid="radio-transform-log10"
                  />
                  Log₁₀ transformation
                </label>
              </div>
              
              {maMethod === "quantile" && (
                <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <label style={{ fontSize: 12, fontWeight: 500 }}>Prior count:</label>
                  <input
                    type="number"
                    value={maPrior}
                    min={0}
                    step={0.5}
                    onChange={e => setMaPrior(Number(e.target.value))}
                    style={{ width: 80, padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 13 }}
                    data-testid="input-prior-count-microarray"
                  />
                </div>
              )}
            </div>
          )}
          {maMethod === "vsn" && (
            <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
              Data transformation is automatically managed by VSN.
            </div>
          )}
          {!transformEnabled && maMethod !== "vsn" && (
            <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
              No transformation will be applied to the expression values.
            </div>
          )}
        </div>

        {/* Single-type: microarray */}
        <>
          <div style={{ display: "flex", flexDirection: "column", gap: 8, marginBottom: 16 }}>
            {NORM_METHODS.map(m => (
              <label key={m.value} style={{
                display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 14px", borderRadius: 8, cursor: "pointer",
                border: `1px solid ${maMethod === m.value ? "var(--primary)" : "var(--border)"}`,
                background: maMethod === m.value ? "var(--selected-bg)" : "white", transition: "all .12s",
              }}>
                <input type="radio" name="normMethod" value={m.value} checked={maMethod === m.value}
                  onChange={() => setMaMethod(m.value)}
                  style={{ accentColor: "var(--primary)", marginTop: 2 }}
                  data-testid={`radio-norm-${m.value}`} />
                <div>
                  <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                  <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
                </div>
              </label>
            ))}
          </div>
        </>
      </div>

      {/* Results */}
      {done && normTargets.length > 0 && (
        <div className="card" style={{ opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Normalization Summary
            {hasSettingsChanged && (
              <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>
                (Outdated - settings changed)
              </span>
            )}
          </div>
          
          <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
            {normTargets
              .filter(d => d.normalizationDone)
              .map(targetDs => (
                <NormalizationResultCard 
                  key={targetDs.id} 
                  targetDs={targetDs} 
                  submittedCfg={submittedCfg} 
                />
              ))}
          </div>
        </div>
      )}

      {done && normTargets.length > 0 && (
        <div className="card" style={{ marginTop: 14, opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Quality Control Box Plots (Before vs After Normalization)
          </div>
          
          {normTargets.length > 1 && (
            <div style={{ display: "flex", gap: 8, marginBottom: 16, borderBottom: "1px solid var(--border)", paddingBottom: 10 }}>
              {normTargets.map(d => {
                const isActive = activeTabId === d.id || (!activeTabId && normTargets[0]?.id === d.id);
                return (
                  <button
                    key={d.id}
                    onClick={() => setActiveTabId(d.id)}
                    style={{
                      padding: "6px 12px",
                      fontSize: 12,
                      fontWeight: 600,
                      borderRadius: 6,
                      border: `1px solid ${isActive ? "var(--primary)" : "var(--border)"}`,
                      background: isActive ? "var(--primary)" : "white",
                      color: isActive ? "white" : "var(--foreground)",
                      cursor: "pointer",
                      transition: "all 0.15s ease"
                    }}
                  >
                    {d.name}
                  </button>
                );
              })}
            </div>
          )}

          {(() => {
            const activeDs = normTargets.find(d => d.id === (activeTabId || normTargets[0]?.id));
            if (!activeDs || (!activeDs.boxplotBefore && !activeDs.boxplotAfter)) {
              return <div style={{ fontSize: 13, color: "var(--muted-foreground)" }}>No Box Plots available for this dataset.</div>;
            }
            return (
              <div style={{ display: "flex", gap: 16, flexWrap: "wrap", justifyContent: "center" }}>
                {activeDs.boxplotBefore && (
                  <div style={{ flex: "1 1 300px", minWidth: 280, textAlign: "center" }}>
                    <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 2, color: "var(--foreground)" }}>Before Normalization</div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6 }}>{activeDs.name}</div>
                    <img 
                      src={`data:image/png;base64,${activeDs.boxplotBefore}`} 
                      alt="Before Normalization Boxplot" 
                      style={{ width: "100%", maxWidth: 500, height: "auto", border: "1px solid var(--border)", borderRadius: 6 }} 
                    />
                  </div>
                )}
                {activeDs.boxplotAfter && (
                  <div style={{ flex: "1 1 300px", minWidth: 280, textAlign: "center" }}>
                    <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 2, color: "var(--foreground)" }}>After Normalization</div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6 }}>{activeDs.name}</div>
                    <img 
                      src={`data:image/png;base64,${activeDs.boxplotAfter}`} 
                      alt="After Normalization Boxplot" 
                      style={{ width: "100%", maxWidth: 500, height: "auto", border: "1px solid var(--border)", borderRadius: 6 }} 
                    />
                  </div>
                )}
              </div>
            );
          })()}
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: getNextAndPrevSteps().prev } as never)}>← Back</button>
          <button className="btn btn-default" onClick={() => setShowSkipModal(true)} data-testid="btn-skip-normalization" style={{ color: "var(--muted-foreground)" }}>Skip</button>
        </div>
        <div style={{ display: "flex", gap: 8 }}>
          {hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }}
              onClick={async () => {
                if (downstreamDone) {
                  setShowDiscardModal(true);
                } else {
                  try {
                    await redoStepAPI("normalization", normTargets.map(d => d.id));
                  } catch (e) {
                    console.error("Failed to redo normalization:", e);
                  }
                  runNorm();
                }
              }}
              data-testid="btn-redo-norm">
              🔄 Redo Normalization
            </button>
          )}
          {!done ? (
            <button className="btn btn-primary" onClick={runNorm} data-testid="btn-run-norm">
              Apply Normalization
            </button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-next-norm">
              Continue →
            </button>
          )}
        </div>
      </div>
    </>
  );
}