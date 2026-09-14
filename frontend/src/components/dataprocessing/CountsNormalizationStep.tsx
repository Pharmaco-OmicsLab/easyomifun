import { useState, useEffect } from "react";
import { useLocation } from "wouter";
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

const isBuiltInTransformMethod = (method: string) => {
  return ["rlog", "vst", "vsn"].includes((method || "").toLowerCase());
};

const COUNT_METHODS: { value: NormMethod; label: string; desc: string }[] = [
  { value: "tmm", label: "TMM", desc: "Adjusts for the differences in library size and RNA composition between samples with trimmed-mean of M values. Common for read counts" },
  { value: "vst", label: "Variance-Stablizing Transformation (VST)", desc: "Transforms counts to stabilize variance across expression levels." },
  { value: "rlog", label: "Regularized Log Normalization", desc: "Regularized log transformation to reduce variance, especially for low counts. Performs well with small sample sizes, but is slower on large datasets." },
  { value: "cpm", label: "CPM / RPKM", desc: "Normalizes counts by library size. Enables expression comparison, but does not account for composition bias." },
  { value: "uq", label: "Upper Quartile Normalization", desc: "Scales counts using the upper quartile of gene expression. Reduces the effect of highly expressed genes, but may not correct all composition biases." },
];

const PROTEOMICS_METHODS: { value: NormMethod; label: string; desc: string }[] = [
  { value: "vsn", label: "VSN (Variance Stabilizing Normalization)", desc: "Fits a model combining variance stabilization and calibration. Recommended for proteomics data." },
  { value: "quantile", label: "Quantile Normalization", desc: "Forces sample intensity distributions to be identical — standard for high-throughput continuous proteomics data." },
];

const OTHER_METHODS: { value: NormMethod; label: string; desc: string }[] = [
  { value: "quantile", label: "Quantile Normalization", desc: "Forces sample intensity distributions to be identical — standard for microarray and general continuous data." },
  { value: "none", label: "No Normalization", desc: "Keep original scales. Use if the dataset is pre-normalized." },
];

interface NormalizationCardProps {
  title: string;
  methods: { value: NormMethod; label: string; desc: string }[];
  selectedMethod: NormMethod;
  onMethodChange: (m: NormMethod) => void;
  transformEnabled: boolean;
  dataTestIdPrefix: string;
  nSamples?: number;
}

function NormalizationCard({
  title,
  methods,
  selectedMethod,
  onMethodChange,
  transformEnabled,
  dataTestIdPrefix,
  nSamples = 0,
}: NormalizationCardProps) {
  const effectiveMethod = methods.find(m => m.value === selectedMethod) ? selectedMethod : methods[0]?.value;

  return (
    <div style={{ border: "1px solid var(--border)", borderRadius: 10, padding: "14px 16px", background: "white", marginBottom: 12 }}>
      <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", marginBottom: 10 }}>{title}</div>
      
      <div style={{ display: "flex", flexDirection: "column", gap: 8, marginBottom: 16 }}>
        {methods.map(m => (
          <label key={m.value} style={{
            display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 14px", borderRadius: 8, cursor: "pointer",
            border: `1px solid ${effectiveMethod === m.value ? "var(--primary)" : "var(--border)"}`,
            background: effectiveMethod === m.value ? "var(--selected-bg)" : "white", transition: "all .12s",
          }}>
            <input 
              type="radio" 
              name={`normMethod-${dataTestIdPrefix}`} 
              value={m.value} 
              checked={effectiveMethod === m.value}
              onChange={() => onMethodChange(m.value)}
              style={{ accentColor: "var(--primary)", marginTop: 2 }} 
              data-testid={`radio-norm-${dataTestIdPrefix}-${m.value}`} 
            />
            <div style={{ flex: 1 }}>
              <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
              <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
              
              {m.value === "rlog" && effectiveMethod === "rlog" && nSamples > 30 && (
                <div className="banner warn" style={{ marginTop: 8, marginBottom: 0, fontSize: 11, padding: "8px 10px" }}>
                  ⚠ Sample size detected to be more than 30 ({nSamples} samples). We recommend using VST normalization for faster computation.
                </div>
              )}
            </div>
          </label>
        ))}
      </div>
    </div>
  );
}

function NormalizationResultCard({ targetDs, submittedCfg }: { targetDs: any; submittedCfg: any }) {
  const allMethods = [...COUNT_METHODS, ...PROTEOMICS_METHODS, ...OTHER_METHODS];
  return (
    <div className="result-ds-card" style={{ margin: 0 }}>
      <div className="result-ds-card-header" style={{ marginBottom: 12 }}>
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: targetDs.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>
          {targetDs.name} Normalization Results
        </div>
      </div>
      
      <div className="banner success" style={{ marginBottom: 12 }}>
        {targetDs.isNormalized ? (
          <>✅ Dataset is pre-normalized.</>
        ) : (
          <>✅ Normalization complete using <strong>{allMethods.find(m => m.value === (targetDs.normalizationMethod || submittedCfg?.method))?.label || "TMM"}</strong>{(targetDs.normalizationLogTransform !== undefined ? targetDs.normalizationLogTransform : submittedCfg?.logTransform) ? ` with ${targetDs.normalizationTransformationType === "log10" || submittedCfg?.transformationType === "log10" ? "log₁₀" : "log₂"} transformation` : ""}.</>
        )}
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

export default function CountNormalizationStep({ datasetId, mode = "dp" }: Props) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const [, navigate] = useLocation();
  const cfg = state.dpNormConfig;
  const [loading, setLoading] = useState(false);
  const [showSkipModal, setShowSkipModal] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  // Dedicated state for Proteomics parameters
  const [proteomicsMethod, setProteomicsMethod] = useState<NormMethod>("vsn");
  const [proteomicsLog, setProteomicsLog] = useState(false);

  // Dedicated state for Others parameters
  const [othersMethod, setOthersMethod] = useState<NormMethod>("quantile");
  const [othersLog, setOthersLog] = useState(false);

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

  useEffect(() => {
    dispatch({
      type: "DP_SET_NORM",
      patch: {
        logTransform: transformEnabled,
        transformationType: transformEnabled ? transformType : "none"
      }
    });
  }, [transformEnabled, transformType, dispatch]);

  useEffect(() => {
    setProteomicsLog(transformEnabled);
    setOthersLog(transformEnabled);
  }, [transformEnabled]);

  const datasets = mode === "dp" ? state.dpDatasets : state.deDatasets;
  const ds = datasets.find(d => d.id === datasetId);

  const allScope = datasetId === "all" ? datasets : (ds ? [ds] : []);
  const rcDatasetsAll = allScope.filter(d => d.dataType === "readcounts");
  const proteomicsDatasetsAll = allScope.filter(d => d.dataType === "proteomics");
  const othersDatasetsAll = allScope.filter(d => d.dataType === "others");

  const rcDatasets = rcDatasetsAll.filter(d => !d.isNormalized);
  const proteomicsDatasets = proteomicsDatasetsAll.filter(d => !d.isNormalized);
  const othersDatasets = othersDatasetsAll.filter(d => !d.isNormalized);

  const targetDatasets = [...rcDatasetsAll, ...proteomicsDatasetsAll, ...othersDatasetsAll];
  const multipleDs = mode === "dp" ? state.dpDatasets.length > 1 : state.deDatasets.length > 1;
  const done = targetDatasets.length > 0 && targetDatasets.every(d => d.normalizationDone || d.isNormalized);

  const hasRc = rcDatasets.length > 0;
  const hasProteomics = proteomicsDatasets.length > 0;
  const hasOthers = othersDatasets.length > 0;
  
  // Flag to adjust main card titles/subheaders if multiple types exist
  const hasMultipleTypes = (hasRc ? 1 : 0) + (hasProteomics ? 1 : 0) + (hasOthers ? 1 : 0) > 1;

  const activeMethods: string[] = [];
  if (hasRc) activeMethods.push(cfg.method);
  if (hasProteomics) activeMethods.push(proteomicsMethod);
  if (hasOthers) activeMethods.push(othersMethod);

  const allBuiltInTransform = activeMethods.length > 0 && activeMethods.every(isBuiltInTransformMethod);

  const [activeTabId, setActiveTabId] = useState<string>("");

  useEffect(() => {
    if (targetDatasets.length > 0) {
      if (!activeTabId || !targetDatasets.some(d => d.id === activeTabId)) {
        setActiveTabId(targetDatasets[0].id);
      }
    }
  }, [targetDatasets, activeTabId]);

  useEffect(() => {
    if (done && !state.dpSubmittedNormConfig) {
      dispatch({ type: "DP_SUBMIT_NORM" });
    }
  }, [done, state.dpSubmittedNormConfig, dispatch]);

  useEffect(() => {
    const isValid = COUNT_METHODS.some(m => m.value === cfg.method);
    if (!isValid && hasRc) {
      dispatch({ type: "DP_SET_NORM", patch: { method: "tmm" } });
    }
  }, [cfg.method, dispatch, hasRc]);

  const getNextAndPrevSteps = () => {
    const activeSteps = computeDPSteps(datasets);
    const currentIdx = activeSteps.findIndex(s => s.id === "normalization-counts");
    const next = activeSteps[currentIdx + 1]?.id || "module-select";
    let prev = activeSteps[currentIdx - 1]?.id || "upload";
    if (prev === "upload" && multipleDs) prev = "all-datasets";
    return { next, prev };
  };

  const submittedCfg = state.dpSubmittedNormConfig;

  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.method !== cfg.method ||
    submittedCfg.logTransform !== cfg.logTransform ||
    (cfg.logTransform && submittedCfg.priorCount !== cfg.priorCount)
  );

  const completedModules: string[] = [];
  targetDatasets.forEach(d => {
    if (d.batchDone && !completedModules.includes("Batch Correction")) completedModules.push("Batch Correction");
    if ((state.dpInlineDeDone || (d && state.dpDeResults[d.id] && state.dpDeResults[d.id].length > 0)) && !completedModules.includes("DE Analysis")) completedModules.push("DE Analysis");
  });
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");

  const downstreamDone = completedModules.length > 0;

  const runNorm = async () => {
    setLoading(true);
    try {
      if (rcDatasets.length > 0) {
        const isBuiltIn = isBuiltInTransformMethod(cfg.method);
        const activeRcConfig = {
          ...cfg,
          logTransform: !isBuiltIn && transformEnabled,
          transformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
          priorCount: isBuiltIn ? 0 : cfg.priorCount
        };
        const results = await normalizeDatasetAPI(rcDatasets, activeRcConfig);
        dispatch({ type: "DP_SUBMIT_NORM" });
        results.forEach(res => {
          const targetDs = rcDatasets.find(d => d.id === res.datasetId);
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: res.datasetId,
            patch: {
              normalizationDone: true,
              normalizationMethod: cfg.method,
              normalizationLogTransform: !isBuiltIn && transformEnabled,
              normalizationTransformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
              parsedData: res.parsedData || undefined,
              columns: res.columns || undefined,
              sampleIds: res.sampleIds || undefined,
              nFeatures: res.nFeatures || res.retainedFeatures || (res.parsedData && res.parsedData.length > 0 ? res.parsedData.length : (targetDs?.nFeatures || 0)),
              nSamples: res.nSamples !== undefined ? res.nSamples : (targetDs?.nSamples || 0),
              boxplotBefore: res.boxplotBefore || undefined,
              boxplotAfter: res.boxplotAfter || undefined
            }
          });
        });
      }

      if (proteomicsDatasets.length > 0) {
        const isBuiltIn = isBuiltInTransformMethod(proteomicsMethod);
        const proteomicsCfg = { 
          method: proteomicsMethod, 
          logTransform: !isBuiltIn && transformEnabled, 
          transformationType: (!isBuiltIn && transformEnabled) ? transformType : "none", 
          priorCount: 0 
        };
        const results = await normalizeDatasetAPI(proteomicsDatasets, proteomicsCfg as any);
        if (!rcDatasets.length && !othersDatasets.length) {
          dispatch({ type: "DP_SET_NORM", patch: proteomicsCfg });
          dispatch({ type: "DP_SUBMIT_NORM" });
        }
        results.forEach(res => {
          const targetDs = proteomicsDatasets.find(d => d.id === res.datasetId);
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: res.datasetId,
            patch: {
              normalizationDone: true,
              normalizationMethod: proteomicsMethod,
              normalizationLogTransform: !isBuiltIn && transformEnabled,
              normalizationTransformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
              parsedData: res.parsedData || undefined,
              columns: res.columns || undefined,
              sampleIds: res.sampleIds || undefined,
              nFeatures: res.nFeatures || res.retainedFeatures || (res.parsedData && res.parsedData.length > 0 ? res.parsedData.length : (targetDs?.nFeatures || 0)),
              nSamples: res.nSamples !== undefined ? res.nSamples : (targetDs?.nSamples || 0),
              boxplotBefore: res.boxplotBefore || undefined,
              boxplotAfter: res.boxplotAfter || undefined
            }
          });
        });
      }

      if (othersDatasets.length > 0) {
        const isBuiltIn = isBuiltInTransformMethod(othersMethod);
        const othersCfg = { 
          method: othersMethod, 
          logTransform: !isBuiltIn && transformEnabled, 
          transformationType: (!isBuiltIn && transformEnabled) ? transformType : "none", 
          priorCount: 0 
        };
        const results = await normalizeDatasetAPI(othersDatasets, othersCfg as any);
        if (!rcDatasets.length && !proteomicsDatasets.length) {
          dispatch({ type: "DP_SET_NORM", patch: othersCfg });
          dispatch({ type: "DP_SUBMIT_NORM" });
        }
        results.forEach(res => {
          const targetDs = othersDatasets.find(d => d.id === res.datasetId);
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: res.datasetId,
            patch: {
              normalizationDone: true,
              normalizationMethod: othersMethod,
              normalizationLogTransform: !isBuiltIn && transformEnabled,
              normalizationTransformationType: (!isBuiltIn && transformEnabled) ? transformType : "none",
              parsedData: res.parsedData || undefined,
              columns: res.columns || undefined,
              sampleIds: res.sampleIds || undefined,
              nFeatures: res.nFeatures || res.retainedFeatures || (res.parsedData && res.parsedData.length > 0 ? res.parsedData.length : (targetDs?.nFeatures || 0)),
              nSamples: res.nSamples !== undefined ? res.nSamples : (targetDs?.nSamples || 0),
              boxplotBefore: res.boxplotBefore || undefined,
              boxplotAfter: res.boxplotAfter || undefined
            }
          });
        });
      }
    } catch (err: any) {
      console.error("Normalization error:", err);
      toast({
        title: "Normalization Error",
        description: err.message || "An error occurred during normalization.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const handleContinue = () => {
    if (hasSettingsChanged) {
      setShowWarnModal(true);
      return;
    }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowWarnModal(false);
    targetDatasets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { normalizationDone: true } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const handleSkip = () => {
    targetDatasets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { normalizationDone: false } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  if (targetDatasets.length === 0) return null;

  const sectionTitle = hasMultipleTypes
    ? "Dataset Normalization Settings"
    : hasRc
      ? `Read Counts Normalization ${multipleDs ? "Datasets" : "Dataset"}`
      : hasProteomics
        ? `Proteomics Normalization ${multipleDs ? "Datasets" : "Dataset"}`
        : `Normalization Configuration ${multipleDs ? "Datasets" : "Dataset"}`;

  return (
    <>
      {showSkipModal && <SkipModal stepName="Normalization" stepId="normalization" datasetIds={targetDatasets.map(d => d.id)} onConfirm={handleSkip} onCancel={() => setShowSkipModal(false)} />}
      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="normalization"
          datasetIds={targetDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: datasetId,
              fromStep: "normalization"
            } as any);
            await runNorm();
          }}
        />
      )}
      {loading && <Spinner label="Applying normalization…" sublabel={`Processing datasets…`} />}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>{sectionTitle}</div>
        <div className="card-sub" style={{ marginBottom: 16 }}>
          Select an appropriate normalization and transformation strategy for your data.
          {targetDatasets.some(d => d.isNormalized) && <span style={{ marginLeft: 8, color: "hsl(38 70% 40%)", fontWeight: 500 }}>⚠ Some datasets are pre-normalized.</span>}
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
            {allBuiltInTransform ? (
              <div style={{ fontSize: 12, fontWeight: 500, color: "var(--muted-foreground)" }}>
                managed automatically
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
          
          {transformEnabled && !allBuiltInTransform && (
            <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
              <div style={{ display: "flex", gap: 16, alignItems: "center", flexWrap: "wrap" }}>
                <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, cursor: "pointer" }}>
                  <input
                    type="radio"
                    name="transform-type-counts"
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
                    name="transform-type-counts"
                    value="log10"
                    checked={transformType === "log10"}
                    onChange={() => setTransformType("log10")}
                    style={{ accentColor: "var(--primary)" }}
                    data-testid="radio-transform-log10"
                  />
                  Log₁₀ transformation
                </label>
              </div>
              {cfg.priorCount !== undefined && (
                <div style={{ display: "flex", alignItems: "center", gap: 8, marginTop: 4 }}>
                  <label style={{ fontSize: 12, fontWeight: 500 }}>Prior count:</label>
                  <input
                    type="number"
                    value={cfg.priorCount}
                    min={0}
                    step={0.5}
                    onChange={(e) => dispatch({ type: "DP_SET_NORM", patch: { priorCount: Number(e.target.value) } })}
                    style={{ width: 80, padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 13 }}
                    data-testid="input-prior-count-transform"
                  />
                </div>
              )}
            </div>
          )}
          {allBuiltInTransform && (
            <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
              Data transformation is automatically managed by the selected normalization method(s) (VST, rlog, or VSN).
            </div>
          )}
          {!transformEnabled && !allBuiltInTransform && (
            <div style={{ fontSize: 11, color: "var(--muted-foreground)" }}>
              No transformation will be applied to the expression values.
            </div>
          )}
        </div>

        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          {/* Read Counts Sub-Card */}
          {hasRc && (
            <NormalizationCard
              title="Read Counts Data Normalization"
              methods={COUNT_METHODS}
              selectedMethod={cfg.method}
              onMethodChange={(m) => dispatch({ type: "DP_SET_NORM", patch: { method: m } })}
              transformEnabled={transformEnabled && !isBuiltInTransformMethod(cfg.method)}
              dataTestIdPrefix="rc"
              nSamples={rcDatasets[0]?.nSamples ?? 0}
            />
          )}

          {/* Proteomics Sub-Card */}
          {hasProteomics && (
            <NormalizationCard
              title="Proteomics Data Normalization"
              methods={PROTEOMICS_METHODS}
              selectedMethod={proteomicsMethod}
              onMethodChange={(m) => setProteomicsMethod(m)}
              transformEnabled={transformEnabled && !isBuiltInTransformMethod(proteomicsMethod)}
              dataTestIdPrefix="proteomics"
            />
          )}

          {/* Others Sub-Card */}
          {hasOthers && (
            <NormalizationCard
              title="Others Data Normalization"
              methods={OTHER_METHODS}
              selectedMethod={othersMethod}
              onMethodChange={(m) => setOthersMethod(m)}
              transformEnabled={transformEnabled && !isBuiltInTransformMethod(othersMethod)}
              dataTestIdPrefix="others"
            />
          )}
        </div>
      </div>

      {done && (
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
            {targetDatasets.map(targetDs => (
              <NormalizationResultCard 
                key={targetDs.id} 
                targetDs={targetDs} 
                submittedCfg={submittedCfg} 
              />
            ))}
          </div>
        </div>
      )}

      {done && (
        <div className="card" style={{ marginTop: 14, opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Quality Control Box Plots (Before vs After Normalization)
          </div>
          
          {targetDatasets.length > 1 && (
            <div style={{ display: "flex", gap: 8, marginBottom: 16, borderBottom: "1px solid var(--border)", paddingBottom: 10 }}>
              {targetDatasets.map(d => {
                const isActive = activeTabId === d.id || (!activeTabId && targetDatasets[0]?.id === d.id);
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
            const activeDs = targetDatasets.find(d => d.id === (activeTabId || targetDatasets[0]?.id));
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
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={async () => {
              if (downstreamDone) {
                setShowDiscardModal(true);
              } else {
                try {
                  await redoStepAPI("normalization", targetDatasets.map(d => d.id));
                } catch (e) {
                  console.error("Failed to redo normalization:", e);
                }
                runNorm();
              }
            }} data-testid="btn-redo-norm">
              🔄 Redo Normalization
            </button>
          )}

          {!done ? (
            <button className="btn btn-primary" onClick={runNorm} data-testid="btn-run-norm">
              Apply Normalization
            </button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-next-norm">
              {getNextAndPrevSteps().next === "batch"
                ? "Continue to Batch Correction →"
                : "Continue to Analysis Module →"}
            </button>
          )}
        </div>
      </div>
    </>
  );
}