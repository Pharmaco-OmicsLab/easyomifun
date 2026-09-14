import { useState, useEffect } from "react";
import { useLocation } from "wouter";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import type { MetaMethod } from "../../dataObject";
import { isMetaEligible } from "../../dataObject";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import { runMetaAnalysisAPI, redoStepAPI, fetchDatasetInfoAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";
import { AlertTriangle, CheckCircle } from "lucide-react";
import {
  ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip,
  ReferenceLine, ResponsiveContainer, Cell,
} from "recharts";

// Recharts plots for custom rendering
function VolcanoPlot({ results, pThresh, fcThresh }: { results: any[]; pThresh: number; fcThresh: number }) {
  const data = results.map(r => ({
    x: r.logFC,
    y: -Math.log10((r.pValue || r.pval || 0) + 1e-10),
    gene: r.gene,
    sig: r.significant,
    dir: r.direction,
  }));
  return (
    <ResponsiveContainer width="100%" height={260}>
      <ScatterChart margin={{ top: 10, right: 20, bottom: 20, left: 20 }}>
        <CartesianGrid strokeDasharray="3 3" stroke="hsl(214 20% 92%)" />
        <XAxis dataKey="x" name="log₂FC" tick={{ fontSize: 11 }} label={{ value: "log₂ Fold Change", position: "insideBottom", offset: -10, fontSize: 11 }} />
        <YAxis dataKey="y" name="-log₁₀(p)" tick={{ fontSize: 11 }} label={{ value: "-log₁₀(p)", angle: -90, position: "insideLeft", fontSize: 11 }} />
        <Tooltip
          cursor={{ strokeDasharray: "3 3" }}
          content={({ payload }) => {
            if (!payload?.length) return null;
            const d = payload[0].payload;
            return (
              <div style={{ background: "white", border: "1px solid hsl(214 20% 82%)", borderRadius: 6, padding: "6px 10px", fontSize: 11 }}>
                <strong>{d.gene}</strong><br />
                log₂FC: {d.x.toFixed(3)}<br />
                -log₁₀(p): {d.y.toFixed(3)}
              </div>
            );
          }}
        />
        <ReferenceLine x={fcThresh} stroke="hsl(38 70% 55%)" strokeDasharray="4 2" strokeWidth={1.5} />
        <ReferenceLine x={-fcThresh} stroke="hsl(38 70% 55%)" strokeDasharray="4 2" strokeWidth={1.5} />
        <ReferenceLine y={-Math.log10(pThresh)} stroke="hsl(38 70% 55%)" strokeDasharray="4 2" strokeWidth={1.5} />
        <Scatter data={data} opacity={0.8}>
          {data.map((entry, i) => (
            <Cell
              key={i}
              fill={entry.dir === "up" ? "hsl(0 65% 55%)" : entry.dir === "down" ? "hsl(214 65% 52%)" : "hsl(220 9% 72%)"}
              r={entry.sig ? 4.5 : 2.5}
            />
          ))}
        </Scatter>
      </ScatterChart>
    </ResponsiveContainer>
  );
}

function MAPlot({ results }: { results: any[] }) {
  const data = results.map(r => ({
    x: Math.log10((r.baseMean || 0) + 1),
    y: r.logFC,
    gene: r.gene,
    sig: r.significant,
    dir: r.direction,
  }));
  return (
    <ResponsiveContainer width="100%" height={260}>
      <ScatterChart margin={{ top: 10, right: 20, bottom: 20, left: 20 }}>
        <CartesianGrid strokeDasharray="3 3" stroke="hsl(214 20% 92%)" />
        <XAxis dataKey="x" name="log₁₀(Mean Expression)" tick={{ fontSize: 11 }} label={{ value: "log₁₀ Mean Expression", position: "insideBottom", offset: -10, fontSize: 11 }} />
        <YAxis dataKey="y" name="log₂FC" tick={{ fontSize: 11 }} label={{ value: "log₂FC", angle: -90, position: "insideLeft", fontSize: 11 }} />
        <Tooltip
          cursor={{ strokeDasharray: "3 3" }}
          content={({ payload }) => {
            if (!payload?.length) return null;
            const d = payload[0].payload;
            return (
              <div style={{ background: "white", border: "1px solid hsl(214 20% 82%)", borderRadius: 6, padding: "6px 10px", fontSize: 11 }}>
                <strong>{d.gene}</strong><br />
                Mean: {Math.pow(10, d.x).toFixed(0)}<br />
                log₂FC: {d.y.toFixed(3)}
              </div>
            );
          }}
        />
        <ReferenceLine y={0} stroke="hsl(220 9% 55%)" strokeDasharray="4 2" strokeWidth={1.5} />
        <Scatter data={data} opacity={0.8}>
          {data.map((entry, i) => (
            <Cell
              key={i}
              fill={entry.dir === "up" ? "hsl(0 65% 55%)" : entry.dir === "down" ? "hsl(214 65% 52%)" : "hsl(220 9% 72%)"}
              r={entry.sig ? 4.5 : 2.5}
            />
          ))}
        </Scatter>
      </ScatterChart>
    </ResponsiveContainer>
  );
}


interface CombinePvalRow {
  gene: string;
  pval: number;
  qval: number;
  dir: string;
}

interface EffectSizeRow {
  gene: string;
  fc: number;
  se: number;
  ci: string;
  pval: number;
  qval: number;
  d: number;
}

interface VoteCountingRow {
  gene: string;
  votes: number;
  total: number;
  dir: string;
}

interface SharedGenesRow {
  gene: string;
  presence: string;
  dir: string;
  range: string;
}

const META_OPTIONS: { id: MetaMethod; label: string; desc: string}[] = [
  {
    id: "combine_pvalue",
    label: "Combining P-values",
    desc: "Aggregate p-values from multiple studies using statistical combination techniques.",
  },
  {
    id: "effect_size",
    label: "Effect Size Models",
    desc: "Combine effect sizes (log fold-changes) using fixed- or random-effects meta-analysis.",
  },
  {
    id: "vote_counting",
    label: "Vote Counting",
    desc: "Count the number of studies in which a gene is significantly differentially expressed.",
  },
  {
    id: "shared_genes",
    label: "Shared Gene Set",
    desc: "Identify genes consistently DE across all or a selected subset of datasets.",
  },
];

export default function MetaAnalysisStep() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const { toast } = useToast();
  const [loading, setLoading] = useState(false);
  const [done, setDone] = useState(!!state.deMetaMethod);

  const selected = state.deMetaMethod;

  // Local state parameters for methods
  const [pvalueMethod, setPvalueMethod] = useState<"fisher" | "stouffer">("fisher");
  const [effectSizeModel, setEffectSizeModel] = useState<"fixed" | "random">("random");

  // Track the detailed run configurations inside submittedCfg snapshot
  const [submittedCfg, setSubmittedCfg] = useState<{ 
    method: MetaMethod | null; 
    votes: number;
    pvalueMethod: "fisher" | "stouffer";
    effectSizeModel: "fixed" | "random";
  } | null>(
    state.deMetaMethod ? { 
      method: state.deMetaMethod, 
      votes: state.deMetaVotes,
      pvalueMethod: "fisher",
      effectSizeModel: "random"
    } : null
  );
  
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  // Expanded validation rule to catch sub-selection adjustments
  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.method !== selected ||
    (selected === "vote_counting" && submittedCfg.votes !== state.deMetaVotes) ||
    (selected === "combine_pvalue" && submittedCfg.pvalueMethod !== pvalueMethod) ||
    (selected === "effect_size" && submittedCfg.effectSizeModel !== effectSizeModel)
  );

  const completedModules: string[] = [];
  if (state.deInlineEaDone) completedModules.push("Enrichment Analysis");
  const downstreamDone = completedModules.length > 0;
  
  const [runDatasetNames, setRunDatasetNames] = useState<string[]>(
    state.deDatasets.map(d => d.name)
  );

  const toArray = (val: any): any[] => {
    if (!val) return [];
    if (Array.isArray(val)) return val;
    if (typeof val === "object") {
      const values = Object.values(val);
      if (values.length > 0 && typeof values[0] === "object") {
        return values;
      }
    }
    return [];
  };

  // ─── Frozen Result Row Snapshots ─────────────────────────────────────────
  const getStoredRows = () => {
    const firstDs = state.deDatasets[0];
    const dataClass = (firstDs?.dataType === "readcounts" || firstDs?.dataType === "microarray") ? "transcriptomics" : "proteomics";
    const data = state.deResults[`meta_${dataClass}`] || state.deResults["meta"] || [];
    if (!data) return [];
    if (Array.isArray(data)) return data;
    const fromFull = toArray(data.full_results);
    if (fromFull.length > 0) return fromFull;
    const fromResults = toArray(data.results);
    if (fromResults.length > 0) return fromResults;
    const fromTop10 = toArray(data.top10);
    if (fromTop10.length > 0) return fromTop10;
    const fromComb = toArray(data.comb_pval);
    if (fromComb.length > 0) return fromComb;
    return toArray(data);
  };

  const storedRows = getStoredRows();

  // Snapshot stored plot images so going back doesn't clear them
  const getStoredPlots = () => {
    const metaObj = state.deResults["meta"] as any;
    return {
      volcanoPlot: metaObj?.volcanoPlot || null,
      forestPlot: metaObj?.forestPlot || null,
    };
  };
  const storedPlots = getStoredPlots();

  const [snapshotVolcanoPlot, setSnapshotVolcanoPlot] = useState<string | null>(storedPlots.volcanoPlot);
  const [snapshotForestPlot, setSnapshotForestPlot] = useState<string | null>(storedPlots.forestPlot);

  const [snapshotCombineRows, setSnapshotCombineRows] = useState<CombinePvalRow[]>(
    selected === "combine_pvalue" ? toArray(storedRows).slice(0, 10) : []
  );
  const [snapshotEffectRows, setSnapshotEffectRows] = useState<EffectSizeRow[]>(
    selected === "effect_size" ? toArray(storedRows).slice(0, 10) : []
  );
  const [snapshotVoteRows, setSnapshotVoteRows] = useState<VoteCountingRow[]>(
    selected === "vote_counting" ? toArray(storedRows).slice(0, 10) : []
  );
  const [snapshotSharedRows, setSnapshotSharedRows] = useState<SharedGenesRow[]>(
    selected === "shared_genes" ? toArray(storedRows).slice(0, 10) : []
  );

  const runMeta = async () => {
    try {
      setLoading(true);
      const metaRes = await runMetaAnalysisAPI(selected!, {
        pvalueMethod: selected === "combine_pvalue" ? pvalueMethod : "",
        effectSizeModel: selected === "effect_size" ? effectSizeModel : "",
        votes: selected === "vote_counting" ? state.deMetaVotes : "",
        pValueThreshold: state.deConfig.pValueThreshold,
        logFcThreshold: state.deConfig.logFcThreshold
      }, state.deDatasets);
      const rawRows = toArray(metaRes?.full_results).length > 0
        ? toArray(metaRes.full_results)
        : (toArray(metaRes?.results).length > 0
            ? toArray(metaRes.results)
            : (toArray(metaRes?.top10).length > 0
                ? toArray(metaRes.top10)
                : toArray(metaRes)));
      
      const targetDatasetNames = state.deDatasets.map(d => d.name);
      
      // Freeze current selection parameters into snapshot values
      setSubmittedCfg({
        method: selected,
        votes: state.deMetaVotes,
        pvalueMethod: pvalueMethod,
        effectSizeModel: effectSizeModel
      });
      setRunDatasetNames(targetDatasetNames);

      // Snapshot plot images from result so they persist on back navigation
      if (metaRes && typeof metaRes === "object" && !Array.isArray(metaRes)) {
        setSnapshotVolcanoPlot((metaRes as any).volcanoPlot || null);
        setSnapshotForestPlot((metaRes as any).forestPlot || null);
      }

      if (selected === "combine_pvalue") {
        setSnapshotCombineRows(rawRows.slice(0, 10));
      } else if (selected === "effect_size") {
        setSnapshotEffectRows(rawRows.slice(0, 10));
      } else if (selected === "vote_counting") {
        setSnapshotVoteRows(rawRows.slice(0, 10));
      } else if (selected === "shared_genes") {
        setSnapshotSharedRows(rawRows.slice(0, 10));
      }

      const firstDs = state.deDatasets[0];
      const dataClass = (firstDs?.dataType === "readcounts" || firstDs?.dataType === "microarray") ? "transcriptomics" : "proteomics";
      dispatch({
        type: "DE_SET_RESULTS",
        id: `meta_${dataClass}`,
        results: metaRes as any
      });
      dispatch({
        type: "DE_SET_RESULTS",
        id: "meta",
        results: metaRes as any
      });
      dispatch({ type: "DE_SET_META_SKIPPED", skipped: false });

      setDone(true);
    } catch (e: any) {
      console.error(e);
      toast({
        title: "Meta-analysis Error",
        description: e.message || "An error occurred during meta-analysis.",
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
    dispatch({ type: "DE_SET_STEP", step: "module-select" });
  };



  const metaObj = state.deResults["meta"] as any;
  const rawMetaList = Array.isArray(metaObj) ? metaObj : (metaObj?.top10 || metaObj?.results || []);
  const metaStats = metaObj?.stats || {
    totalFeatures: rawMetaList.length,
    numDatasets: state.deDatasets.length,
    overlappingGenes: 0,
    numSignificant: rawMetaList.filter((r: any) => r.significant || r.qval < 0.05).length,
    sigUp: rawMetaList.filter((r: any) => (r.significant || r.qval < 0.05) && (r.logFC > 0 || r.dir === "Up" || r.direction === "up")).length,
    sigDown: rawMetaList.filter((r: any) => (r.significant || r.qval < 0.05) && (r.logFC < 0 || r.dir === "Down" || r.direction === "down")).length,
  };
  const numDatasetsVal = metaStats.numDatasets ?? runDatasetNames.length;
  const metaSigCount = metaStats.numSignificant;
  const metaUpCount = metaStats.sigUp;
  const metaDownCount = metaStats.sigDown;

  const activeSnapshotMethod = submittedCfg?.method || selected;

  const [backendSharedCounts, setBackendSharedCounts] = useState<{ transcriptomics?: number; proteomics?: number }>({});

  const transcriptomicsDS = state.deDatasets.filter(
    d => d.dataType === "readcounts" || d.dataType === "microarray"
  );
  const proteomicsDS = state.deDatasets.filter(d => d.dataType === "proteomics");
  const othersDS = state.deDatasets.filter(d => d.dataType === "others");

  useEffect(() => {
    let cancelled = false;
    const fetchShared = async () => {
      if (transcriptomicsDS.length > 1) {
        try {
          const res = await fetchDatasetInfoAPI(transcriptomicsDS.map(d => d.id), "de");
          if (!cancelled && res?.sharedFeaturesCount !== undefined) {
            setBackendSharedCounts(prev => ({ ...prev, transcriptomics: res.sharedFeaturesCount }));
          }
        } catch (e) {
          console.error("Failed to fetch transcriptomics shared count:", e);
        }
      }
      if (proteomicsDS.length > 1) {
        try {
          const res = await fetchDatasetInfoAPI(proteomicsDS.map(d => d.id), "de");
          if (!cancelled && res?.sharedFeaturesCount !== undefined) {
            setBackendSharedCounts(prev => ({ ...prev, proteomics: res.sharedFeaturesCount }));
          }
        } catch (e) {
          console.error("Failed to fetch proteomics shared count:", e);
        }
      }
    };
    fetchShared();
    return () => { cancelled = true; };
  }, [state.deDatasets]);

  const getIntersectionSize = (dsList: typeof state.deDatasets, dataClass: "transcriptomics" | "proteomics" = "transcriptomics"): number => {
    if (dsList.length === 0) return 0;
    // Prefer authoritative backend count (from /api/dataset-info full expression matrix)
    if (backendSharedCounts[dataClass] !== undefined) {
      return backendSharedCounts[dataClass]!;
    }
    // parsedData only holds ~100 preview rows — don't intersect it.
    // Fall back to the minimum nFeatures stored from the backend upload/dataset-info response.
    const minFeatures = Math.min(...dsList.map(d => d.nFeatures || 0));
    return minFeatures > 0 ? minFeatures : 0;
  };

  const getOverlappingGeneCount = (datasets: typeof state.deDatasets): number => {
    if (datasets.length === 0) return 0;
    const tCount = transcriptomicsDS.length > 1 ? getIntersectionSize(transcriptomicsDS, "transcriptomics") : 0;
    const pCount = proteomicsDS.length > 1 ? getIntersectionSize(proteomicsDS, "proteomics") : 0;
    return tCount + pCount || Math.min(...datasets.map(d => d.nFeatures || 0));
  };

  const overlappingCount = metaStats.overlappingGenes ?? getOverlappingGeneCount(state.deDatasets);

  let hasZeroSharedFeatures = false;
  const zeroSharedGroups: string[] = [];

  if (transcriptomicsDS.length > 1 && getIntersectionSize(transcriptomicsDS, "transcriptomics") === 0) {
    hasZeroSharedFeatures = true;
    zeroSharedGroups.push("Transcriptomics");
  }
  if (proteomicsDS.length > 1 && getIntersectionSize(proteomicsDS, "proteomics") === 0) {
    hasZeroSharedFeatures = true;
    zeroSharedGroups.push("Proteomics");
  }

  const hasSameTypeMultiple = isMetaEligible(state.deDatasets);

  useEffect(() => {
    if (!hasSameTypeMultiple) {
      dispatch({ type: "DE_SET_STEP", step: "module-select" });
    }
  }, [hasSameTypeMultiple, dispatch]);

  if (!hasSameTypeMultiple) return null;

return (
    <>
      {loading && <Spinner label="Running meta-analysis…" sublabel={`Method: ${META_OPTIONS.find(m => m.id === selected)?.label ?? "—"}`} />}



      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Meta-analysis Strategy</div>
        <div className="card-sub">
          {state.deDatasets.length} datasets detected. Select a method to combine results across all datasets.
        </div>
        <hr className="card-divider" />

        {/* Strategy Grid */}
        <div className="meta-grid">
          {META_OPTIONS.map(opt => (
            <div
              key={opt.id}
              className={`meta-card ${selected === opt.id ? "selected" : ""}`}
              onClick={() => dispatch({ type: "DE_SET_META", method: opt.id })}
              data-testid={`card-meta-${opt.id}`}
              style={{ cursor: "pointer" }}
            >
              <div className="meta-card-title">{opt.label}</div>
              <div className="meta-card-desc">{opt.desc}</div>
            </div>
          ))}
        </div>

        {/* Context-Specific Parameters Panel */}
        {selected && selected !== "shared_genes" && (
          <div style={{ 
            marginTop: 20, 
            padding: "16px 20px", 
            borderRadius: 8, 
            background: "var(--muted, hsl(220 10% 97%))", 
            border: "1px solid var(--border)",
            display: "flex",
            alignItems: "center",
            gap: 16,
            flexWrap: "wrap"
          }}>
            {/* 1. Combining P-Value Extra Settings */}
            {selected === "combine_pvalue" && (
              <div style={{ display: "flex", alignItems: "center", gap: 12 }}>
                <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Combination Method:</label>
                
                <details style={{ width: 180, position: "relative" }} data-testid="details-pvalue-method">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{pvalueMethod === "fisher" ? "Fisher's Method" : "Stouffer's Method"}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {[
                      { val: "fisher", label: "Fisher's Method" },
                      { val: "stouffer", label: "Stouffer's Method" }
                    ].map(({ val, label }) => (
                      <div
                        key={val}
                        onClick={(e) => {
                          setPvalueMethod(val as any);
                          const details = (e.target as HTMLElement).closest("details");
                          if (details) details.removeAttribute("open");
                        }}
                        style={{
                          padding: "8px 10px",
                          fontSize: 12,
                          cursor: "pointer",
                          background: pvalueMethod === val ? "var(--selected-bg)" : "transparent",
                          fontWeight: pvalueMethod === val ? 600 : 400
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                        onMouseLeave={e => e.currentTarget.style.background = pvalueMethod === val ? "var(--selected-bg)" : "transparent"}
                      >
                        {label}
                      </div>
                    ))}
                  </div>
                </details>
              </div>
            )}

            {/* 2. Effect Size Extra Settings */}
            {selected === "effect_size" && (
              <>
                <div style={{ display: "flex", alignItems: "center", gap: 12 }}>
                  <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Analysis Model:</label>
                  
                  <details style={{ width: 180, position: "relative" }} data-testid="details-effect-size-model">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>{effectSizeModel === "random" ? "Random Effects Model" : "Fixed Effect Model"}</span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {[
                        { val: "random", label: "Random Effects Model" },
                        { val: "fixed", label: "Fixed Effect Model" }
                      ].map(({ val, label }) => (
                        <div
                          key={val}
                          onClick={(e) => {
                            setEffectSizeModel(val as any);
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: effectSizeModel === val ? "var(--selected-bg)" : "transparent",
                            fontWeight: effectSizeModel === val ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = effectSizeModel === val ? "var(--selected-bg)" : "transparent"}
                        >
                          {label}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>

              </>
            )}

            {/* 3. Vote Counting Extra Settings */}
            {selected === "vote_counting" && (
              <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
                <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Minimum Votes Required:</label>
                <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <input
                    type="number"
                    value={state.deMetaVotes}
                    min={1}
                    max={state.deDatasets.length}
                    onChange={e => dispatch({ type: "DE_SET_META_VOTES", n: Number(e.target.value) })}
                    style={{ width: 65, padding: "6px 10px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 13, background: "white", textAlign: "center" }}
                    data-testid="input-min-votes"
                  />
                  <span style={{ fontSize: 12, color: "hsl(220 9% 50%)", fontWeight: 500 }}>
                    out of {state.deDatasets.length} active studies
                  </span>
                </div>
              </div>
            )}
          </div>
        )}
      </div>

      {/* Integrity check */}
      <div className="card">
        <div className="card-title" style={{ marginBottom: 12 }}>Data Integrity Check</div>
        {hasZeroSharedFeatures ? (
          <div className="banner danger" style={{ marginBottom: 10 }}>
            <AlertTriangle size={15} style={{ flexShrink: 0 }} />
            <span>
              Danger: No shared features (overlapping genes) found across same-type datasets ({zeroSharedGroups.join(", ")}).
              You cannot proceed with meta-analysis.
            </span>
          </div>
        ) : (
          <div className="banner success" style={{ marginBottom: 10 }}>
            <CheckCircle size={15} style={{ flexShrink: 0 }} />
            <span>Success: All same-type datasets have overlapping features. Ready to proceed.</span>
          </div>
        )}

        {/* Display statistics */}
        <div style={{ marginTop: 14 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
            Shared Features Statistics
          </div>
          <div className="chips-row">
            {transcriptomicsDS.length > 1 && (
              <div className="stat-chip">
                <div className="stat-chip-val">{getIntersectionSize(transcriptomicsDS, "transcriptomics").toLocaleString()}</div>
                <div className="stat-chip-lbl">Transcriptomics Shared</div>
              </div>
            )}
            {proteomicsDS.length > 1 && (
              <div className="stat-chip">
                <div className="stat-chip-val">{getIntersectionSize(proteomicsDS, "proteomics").toLocaleString()}</div>
                <div className="stat-chip-lbl">Proteomics Shared</div>
              </div>
            )}
          </div>
        </div>
      </div>

      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="meta-analysis"
          datasetIds={state.deDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: "all",
              fromStep: "meta-analysis"
            } as any);
            await runMeta();
          }}
        />
      )}

      {!done ? (
        <div className="action-row">
          <div style={{ display: "flex", gap: 8 }}>
            <button className="btn btn-default" onClick={() => dispatch({ type: "DE_SET_STEP", step: "analysis" })}>
              ← Back
            </button>
            <button
              className="btn btn-default"
              onClick={() => {
                dispatch({ type: "DE_SET_META_SKIPPED", skipped: true });
                dispatch({ type: "DE_SET_STEP", step: "export" });
              }}
              style={{ color: "var(--muted-foreground)" }}
              data-testid="btn-skip-meta"
            >
              Skip
            </button>
          </div>
          <button
            className="btn btn-primary"
            disabled={!selected || hasZeroSharedFeatures}
            onClick={runMeta}
            data-testid="btn-run-meta"
          >
            Run Meta-analysis →
          </button>
        </div>
      ) : (
        <>

          {/* Results Table Section */}
          <div className="card" style={{ opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
            <div className="card-title" style={{ margin: "0 0 8px 0" }}>
              Meta-analysis Results {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated - settings changed)</span>}
            </div>
            <div className="banner success" style={{ marginBottom: 12 }}>
              ✅ Meta-analysis complete using <strong>{META_OPTIONS.find(m => m.id === activeSnapshotMethod)?.label}</strong> across {runDatasetNames.length} datasets.
            </div>
            <div className="chips-row">
              <div className="stat-chip"><div className="stat-chip-val">{numDatasetsVal}</div><div className="stat-chip-lbl">Datasets</div></div>
              <div className="stat-chip"><div className="stat-chip-val">{overlappingCount}</div><div className="stat-chip-lbl">Overlapping Genes</div></div>
              <div className="stat-chip"><div className="stat-chip-val">{metaSigCount}</div><div className="stat-chip-lbl">Consensus Sig.</div></div>
              <div className="stat-chip"><div className="stat-chip-val" style={{ color: "#dc2626" }}>{metaUpCount}</div><div className="stat-chip-lbl">Up-regulated</div></div>
              <div className="stat-chip"><div className="stat-chip-val" style={{ color: "#2563eb" }}>{metaDownCount}</div><div className="stat-chip-lbl">Down-regulated</div></div>
              {activeSnapshotMethod === "effect_size" && metaStats.i2 !== undefined && (
                <>
                  <div className="stat-chip">
                    <div className="stat-chip-val">{metaStats.tau2 !== null && metaStats.tau2 !== undefined ? Number(metaStats.tau2).toFixed(4) : "—"}</div>
                    <div className="stat-chip-lbl">τ² (median)</div>
                  </div>
                  <div className="stat-chip">
                    <div className="stat-chip-val">{metaStats.i2 !== null && metaStats.i2 !== undefined ? Number(metaStats.i2).toFixed(1) : "—"}%</div>
                    <div className="stat-chip-lbl">I² (median)</div>
                  </div>
                </>
              )}
            </div>

            {/* Volcano & Forest Plots for Meta Analysis */}
            {activeSnapshotMethod === "combine_pvalue" && (
              <div style={{ display: "flex", justifyContent: "center", marginTop: 16, marginBottom: 16 }}>
                <div className="chart-box" style={{ display: "flex", flexDirection: "column", border: "1px solid var(--border)", borderRadius: 8, padding: 12, background: "white", width: "100%", maxWidth: 600 }}>
                  <div className="chart-box-title" style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>Volcano Plot (Combined Log2FC vs FDR)</div>
                  {snapshotVolcanoPlot ? (
                    <img src={`data:image/png;base64,${snapshotVolcanoPlot}`} style={{ width: "100%", height: "auto", borderRadius: 8 }} alt="Volcano Plot" />
                  ) : (
                    <VolcanoPlot results={rawMetaList} pThresh={state.deConfig.pValueThreshold} fcThresh={state.deConfig.logFcThreshold} />
                  )}
                </div>
              </div>
            )}
            {activeSnapshotMethod === "effect_size" && (
              <div className="chart-grid" style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16, marginTop: 16, marginBottom: 16 }}>
                <div className="chart-box" style={{ display: "flex", flexDirection: "column", border: "1px solid var(--border)", borderRadius: 8, padding: 12, background: "white" }}>
                  <div className="chart-box-title" style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>Volcano Plot (Hedges' g vs FDR)</div>
                  {snapshotVolcanoPlot ? (
                    <img src={`data:image/png;base64,${snapshotVolcanoPlot}`} style={{ width: "100%", height: "auto", borderRadius: 8 }} alt="Volcano Plot" />
                  ) : (
                    <VolcanoPlot results={rawMetaList} pThresh={state.deConfig.pValueThreshold} fcThresh={state.deConfig.logFcThreshold} />
                  )}
                </div>
                <div className="chart-box" style={{ display: "flex", flexDirection: "column", border: "1px solid var(--border)", borderRadius: 8, padding: 12, background: "white" }}>
                  <div className="chart-box-title" style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>Forest Plot (Top 10)</div>
                  {snapshotForestPlot ? (
                    <img src={`data:image/png;base64,${snapshotForestPlot}`} style={{ width: "100%", height: "auto", borderRadius: 8 }} alt="Forest Plot" />
                  ) : (
                    <div style={{ display: "flex", alignItems: "center", justifyContent: "center", height: "100%", minHeight: 200, color: "var(--muted-foreground)" }}>Forest Plot not available</div>
                  )}
                </div>
              </div>
            )}

            <hr className="card-divider" />
            <div style={{ fontSize: 13, fontWeight: 600, marginBottom: 12 }}>Consolidation Strategy: {META_OPTIONS.find(m => m.id === activeSnapshotMethod)?.label}</div>
            
            <div style={{ overflowX: "auto" }}>
              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                <thead>
                  <tr style={{ background: "var(--neutral-bg)" }}>
                    {activeSnapshotMethod === "combine_pvalue" && (
                      <>
                        <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Feature</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Fold Change</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Combined LogFC</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>P-value</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>adj.P.Val.</th>
                      </>
                    )}
                    {activeSnapshotMethod === "effect_size" && (
                      <>
                        <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Feature</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Fold Change</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Combined Effects Size</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>P-value</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>adj.P.Val.</th>
                      </>
                    )}
                    {activeSnapshotMethod === "vote_counting" && (
                      <>
                        <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Feature</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Significant Votes</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Total Studies</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Vote Ratio</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Direction</th>
                      </>
                    )}
                    {activeSnapshotMethod === "shared_genes" && (
                      <>
                        <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Feature</th>
                        <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Presence in Studies</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Consensus Direction</th>
                        <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Log2FC Range</th>
                      </>
                    )}
                  </tr>
                </thead>
                <tbody>
                  {activeSnapshotMethod === "combine_pvalue" && toArray(snapshotCombineRows).slice(0, 10).map((r: any, i) => {
                    const feature = r.Feature ?? r.Features ?? r.gene ?? "";
                    const combinedVal = r["Combined LogFC"] ?? r.combined ?? r.logFC ?? 0;
                    const foldChange = r.FoldChange ?? (typeof combinedVal === "number" ? Math.pow(2, combinedVal) : 1);
                    const pValue = r["P-value"] ?? r.pval ?? r.pValue ?? 1;
                    const adjPValue = r["adj.P.Val."] ?? r.qval ?? r.adjPValue ?? 1;
                    return (
                      <tr key={feature || i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                        <td style={{ padding: "8px 10px", fontWeight: 600 }}>{feature}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof foldChange === "number" ? foldChange.toFixed(3) : foldChange}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: typeof combinedVal === "number" && combinedVal > 0 ? "var(--danger-text)" : typeof combinedVal === "number" && combinedVal < 0 ? "var(--primary)" : "inherit" }}>
                          {typeof combinedVal === "number" ? (combinedVal > 0 ? "+" : "") + combinedVal.toFixed(3) : combinedVal}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof pValue === "number" ? (pValue < 0.001 ? pValue.toExponential(2) : pValue.toFixed(4)) : String(pValue ?? "")}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof adjPValue === "number" ? (adjPValue < 0.001 ? adjPValue.toExponential(2) : adjPValue.toFixed(4)) : String(adjPValue ?? "")}
                        </td>
                      </tr>
                    );
                  })}
                  
                  {activeSnapshotMethod === "effect_size" && toArray(snapshotEffectRows).slice(0, 10).map((r: any, i) => {
                    const feature = r.Feature ?? r.Features ?? r.gene ?? "";
                    const combinedVal = r["Combined Effects Size"] ?? r["Combined Effect Size"] ?? r.combined ?? r.hedges_g ?? r.d ?? r.fc ?? 0;
                    const foldChange = r.FoldChange ?? (typeof combinedVal === "number" ? Math.pow(2, combinedVal) : 1);
                    const pValue = r["P-value"] ?? r.pval ?? r.pValue ?? 1;
                    const adjPValue = r["adj.P.Val."] ?? r.qval ?? r.adjPValue ?? 1;
                    return (
                      <tr key={feature || i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                        <td style={{ padding: "8px 10px", fontWeight: 600 }}>{feature}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof foldChange === "number" ? foldChange.toFixed(3) : foldChange}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: typeof combinedVal === "number" && combinedVal > 0 ? "var(--danger-text)" : typeof combinedVal === "number" && combinedVal < 0 ? "var(--primary)" : "inherit" }}>
                          {typeof combinedVal === "number" ? (combinedVal > 0 ? "+" : "") + combinedVal.toFixed(3) : combinedVal}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof pValue === "number" ? (pValue < 0.001 ? pValue.toExponential(2) : pValue.toFixed(4)) : String(pValue ?? "")}
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          {typeof adjPValue === "number" ? (adjPValue < 0.001 ? adjPValue.toExponential(2) : adjPValue.toFixed(4)) : String(adjPValue ?? "")}
                        </td>
                      </tr>
                    );
                  })}
                  
                  {activeSnapshotMethod === "vote_counting" && toArray(snapshotVoteRows).slice(0, 10).map((r, i) => {
                    const rawDir = typeof r.dir === "string" ? r.dir.toLowerCase() : "";
                    const dir = (rawDir === "up" || rawDir === "down" || rawDir === "ns") ? rawDir : (rawDir.includes("up") ? "up" : rawDir.includes("down") ? "down" : "ns");
                    return (
                      <tr key={r.gene} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                        <td style={{ padding: "8px 10px", fontWeight: 600 }}>{r.gene}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 700 }}>{r.votes}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.total}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>{((r.votes / r.total) * 100).toFixed(1)}%</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          <span style={{
                            fontSize: 11, fontWeight: 600, padding: "2px 7px", borderRadius: 4,
                            background: dir === "up" ? "hsl(0 60% 95%)" : dir === "down" ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                            color: dir === "up" ? "hsl(0 65% 38%)" : dir === "down" ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                          }}>
                            {dir === "up" ? "↑ Up" : dir === "down" ? "↓ Down" : "Not Significant"}
                          </span>
                        </td>
                      </tr>
                    );
                  })}
                  
                  {activeSnapshotMethod === "shared_genes" && toArray(snapshotSharedRows).slice(0, 10).map((r, i) => {
                    const rawDir = typeof r.dir === "string" ? r.dir.toLowerCase() : "";
                    const dir = (rawDir === "up" || rawDir === "down" || rawDir === "ns") ? rawDir : (rawDir.includes("up") ? "up" : rawDir.includes("down") ? "down" : "ns");
                    return (
                      <tr key={r.gene} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                        <td style={{ padding: "8px 10px", fontWeight: 600 }}>{r.gene}</td>
                        <td style={{ padding: "8px 10px", fontSize: 11 }}>{r.presence}</td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>
                          <span style={{
                            fontSize: 11, fontWeight: 600, padding: "2px 7px", borderRadius: 4,
                            background: dir === "up" ? "hsl(0 60% 95%)" : dir === "down" ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                            color: dir === "up" ? "hsl(0 65% 38%)" : dir === "down" ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                          }}>
                            {dir === "up" ? "↑ Up" : dir === "down" ? "↓ Down" : "Not Significant"}
                          </span>
                        </td>
                        <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.range}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
          
          <div className="action-row">
            <button className="btn btn-default" onClick={() => setDone(false)}>← Back</button>
            <div style={{ display: "flex", gap: 8 }}>
              {hasSettingsChanged && (
                <button
                  className="btn btn-default"
                  disabled={hasZeroSharedFeatures}
                  style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }}
                  onClick={async () => {
                    if (downstreamDone) {
                      setShowDiscardModal(true);
                    } else {
                      try {
                        await redoStepAPI("meta-analysis", state.deDatasets.map(d => d.id));
                      } catch (e) {
                        console.error("Failed to redo Meta-analysis:", e);
                      }
                      runMeta();
                    }
                  }}
                  data-testid="btn-redo-meta"
                >
                  🔄 Redo Meta-analysis
                </button>
              )}
              <button
                className="btn btn-primary"
                onClick={handleContinue}
                disabled={hasZeroSharedFeatures}
                data-testid="btn-next-meta"
              >
                Continue to Analysis Module →
              </button>
            </div>
          </div>
        </>
      )}
    </>
  );
}