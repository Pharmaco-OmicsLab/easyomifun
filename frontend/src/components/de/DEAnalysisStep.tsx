import { useState, useEffect } from "react";
import { useLocation } from "wouter";
import { useAppStore } from "../../store/appStore";
import { AlertTriangle } from "lucide-react";
import Spinner from "../Spinner";

import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import type { PValueAdjMethod } from "../../dataObject";
import { isMetaEligible } from "../../dataObject";
import { runDifferentialExpressionAPI, redoStepAPI, fetchDatasetInfoAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

interface Props { datasetId: string }


const DE_METHODS = [
  { id: "deseq2", label: "DESeq2", desc: "Using a negative binomial distribution. Robust differential expression analysis for RNA-seq." },
  { id: "edger", label: "edgeR", desc: "Using a negative binomial distribution with empirical Bayes estimation. Performs well small sample sizes." },
  { id: "limma_voom", label: "limma-voom", desc: "Using linear models with precision weights (voom) for raw read counts." },
  { id: "limma", label: "limma", desc: "Using linear models. Performs robust differential expression analysis for microarray, normalized, and other datasets." },
];

const ADJ_METHODS: { value: PValueAdjMethod; label: string }[] = [
  { value: "BH", label: "Benjamini-Hochberg" },
  { value: "BY", label: "Benjamini-Yekutieli" },
  { value: "bonferroni", label: "Bonferroni" },
  { value: "holm", label: "Holm" },
  { value: "none", label: "No adjustment" },
];

function getBaseDatasetId(id: string): string {
  if (!id) return "";
  return id.replace(/_(dp|de|fs|enrichment|en|ea)(.*)$/, "");
}

interface TargetDataset { id: string; name: string; color: string; }

function DEResultCard({
  targetDs,
  results,
  hasSettingsChanged,
}: {
  targetDs: TargetDataset;
  results: any;
  hasSettingsChanged: boolean | null;
}) {
  const isOutdated = hasSettingsChanged === true;
  const rawList = Array.isArray(results) ? results : (results?.top10 || results?.results || []);
  const stats = results?.stats || {
    totalFeatures: rawList.length,
    numSignificant: rawList.filter((r: any) => r.significant).length,
    sigUp: rawList.filter((r: any) => r.significant && (r.logFC > 0 || r.direction === "up")).length,
    sigDown: rawList.filter((r: any) => r.significant && (r.logFC < 0 || r.direction === "down")).length,
  };
  const top10 = (results?.top10 || rawList).slice(0, 10);

  return (
    <div className="result-ds-card" style={{ opacity: isOutdated ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
      <div className="result-ds-card-header">
        <div style={{ width: 8, height: 8, borderRadius: "50%", background: targetDs.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>{targetDs.name} Results</div>
      </div>
      {results?.hadDuplicates && (
        <div className="banner warn" style={{ marginTop: 8, marginBottom: 8, display: "flex", alignItems: "center", gap: 8, fontSize: 12 }}>
          <AlertTriangle size={15} style={{ color: "hsl(38 90% 40%)", flexShrink: 0 }} />
          <span>Duplicate Gene IDs were detected and collapsed (readcounts: sum, microarray/normalized: mean) before applying the DE model.</span>
        </div>
      )}
      <div className="chips-row" style={{ marginBottom: 14 }}>
        <div className="stat-chip"><div className="stat-chip-val">{stats.totalFeatures}</div><div className="stat-chip-lbl">Tested</div></div>
        <div className="stat-chip"><div className="stat-chip-val">{stats.numSignificant}</div><div className="stat-chip-lbl">Significant</div></div>
        <div className="stat-chip"><div className="stat-chip-val" style={{ color: "#dc2626" }}>{stats.sigUp}</div><div className="stat-chip-lbl">Up</div></div>
        <div className="stat-chip"><div className="stat-chip-val" style={{ color: "#2563eb" }}>{stats.sigDown}</div><div className="stat-chip-lbl">Down</div></div>
      </div>

      {/* Volcano & MA Plots from Backend */}
      <div className="chart-grid" style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16, marginBottom: 16 }}>
        <div className="chart-box" style={{ display: "flex", flexDirection: "column", border: "1px solid var(--border)", borderRadius: 8, padding: 12 }}>
          <div className="chart-box-title" style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>Volcano Plot</div>
          {results?.volcanoPlot ? (
            <img src={`data:image/png;base64,${results.volcanoPlot}`} style={{ width: "100%", height: "auto", borderRadius: 8 }} alt="Volcano Plot" />
          ) : (
            <div style={{ padding: 40, textAlign: "center", color: "var(--muted-foreground)", fontSize: 12 }}>Generating Volcano Plot...</div>
          )}
        </div>
        <div className="chart-box" style={{ display: "flex", flexDirection: "column", border: "1px solid var(--border)", borderRadius: 8, padding: 12 }}>
          <div className="chart-box-title" style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>MA Plot</div>
          {results?.maPlot ? (
            <img src={`data:image/png;base64,${results.maPlot}`} style={{ width: "100%", height: "auto", borderRadius: 8 }} alt="MA Plot" />
          ) : (
            <div style={{ padding: 40, textAlign: "center", color: "var(--muted-foreground)", fontSize: 12 }}>Generating MA Plot...</div>
          )}
        </div>
      </div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8 }}>Preview (top 10)</div>
      <div className="preview-wrap">
        <table>
          <thead><tr><th>Feature</th><th>Fold Change</th><th>Log₂FC</th><th>P-value</th><th>adj.P.Val.</th></tr></thead>
          <tbody>
            {top10.map((r: any) => {
              const feature = r.Feature ?? r.Features ?? r.gene ?? "";
              const logFC = r.logFC ?? r.log2FoldChange ?? 0;
              const foldChange = r.FoldChange ?? (Math.pow(2, logFC));
              const pValue = r["P-value"] ?? r.pValue ?? r.pval ?? 1;
              const adjPValue = r["adj.P.Val."] ?? r.adjPValue ?? r.qval ?? 1;
              const isSig = r.significant ?? (adjPValue < (state.deConfig?.pValueThreshold ?? 0.05) && Math.abs(logFC) >= (state.deConfig?.logFcThreshold ?? 1.0));
              const rawDir = typeof r.direction === "string" ? r.direction.toLowerCase() : "";
              const direction = (rawDir === "up" || rawDir === "down" || rawDir === "ns")
                ? rawDir
                : (isSig ? (logFC > 0 ? "up" : "down") : "ns");
              return (
                <tr key={feature}>
                  <td style={{ fontWeight: 600 }}>{feature}</td>
                  <td>{typeof foldChange === "number" ? foldChange.toFixed(3) : foldChange}</td>
                  <td style={{ color: direction === "up" ? "#dc2626" : direction === "down" ? "#2563eb" : "inherit" }}>
                    {logFC > 0 ? "+" : ""}{typeof logFC === "number" ? logFC.toFixed(3) : logFC}
                  </td>
                  <td>{typeof pValue === "number" ? pValue.toFixed(4) : pValue}</td>
                  <td>{typeof adjPValue === "number" ? adjPValue.toFixed(4) : adjPValue}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
}

export default function DEAnalysisStep({ datasetId }: Props) {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const { toast } = useToast();
  const cfg = state.deConfig;
  const [loading, setLoading] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const ds = state.deDatasets.find(d => d.id === datasetId);
  const allDs = state.deDatasets;
  
  const multipleDs = state.deDatasets.length > 1;
  const isViewingAll = datasetId === "all";
  const targetDatasets = isViewingAll ? state.deDatasets : (ds ? [ds] : []);
  const done = targetDatasets.length > 0 && targetDatasets.every(d => !!(state.deResults && (state.deResults[d.id] || Object.entries(state.deResults).find(([k]) => getBaseDatasetId(k) === getBaseDatasetId(d.id))?.[1])));

  const unnormReadcountsDs = targetDatasets.filter(d => d.dataType === "readcounts" && !d.isNormalized);
  const limmaDs = targetDatasets.filter(d => d.dataType === "microarray" || d.dataType === "others" || d.isNormalized);
  const hasUnnormReadcounts = unnormReadcountsDs.length > 0;
  const hasLimma = limmaDs.length > 0;
  const isMixed = hasUnnormReadcounts && hasLimma;

  const showLimmaOnly = hasLimma && !hasUnnormReadcounts;
  const method = cfg.method || (showLimmaOnly ? "limma" : "deseq2");

  const setMethod = (val: string) => {
    dispatch({ type: "DE_SET_CONFIG", patch: { method: val } });
  };

  useEffect(() => {
    if (method === "limma" && !showLimmaOnly && !isMixed) {
      setMethod("deseq2");
    } else if (method !== "limma" && showLimmaOnly) {
      setMethod("limma");
    }
  }, [showLimmaOnly, method, isMixed]);

  useEffect(() => {
    if (done && !state.deSubmittedConfig) {
      dispatch({ type: "DE_SUBMIT_CONFIG" });
    }
  }, [done, state.deSubmittedConfig, dispatch]);

  // Sync authoritative dataset statistics and clinical groups from backend on mount if missing
  useEffect(() => {
    const unSynced = allDs.filter(d => !d.matchingSamples || d.matchingSamples.length === 0 || !d.groups || d.groups.length === 0);
    if (unSynced.length > 0) {
      fetchDatasetInfoAPI(allDs.map(d => d.id), "de")
        .then(infoRes => {
          if (infoRes && infoRes.datasets) {
            Object.entries(infoRes.datasets).forEach(([dsId, info]) => {
              dispatch({
                type: "DE_UPDATE_DATASET",
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
              });
            });
          }
        })
        .catch(err => console.warn("Failed to sync DE dataset info:", err));
    }
  }, []);

  const getDatasetSampleGroups = (d: typeof state.deDatasets[0]) => {
    if (d && d.groups && d.groups.length > 0) {
      return d.groups;
    }
    const columnsToUse = d.clinicalColumns || [];
    const rowsToUse = d.clinicalParsedData || [];
    const colIndex = d.clinicalGroupCol ? columnsToUse.indexOf(d.clinicalGroupCol) : -1;
    return colIndex !== -1
      ? Array.from(new Set(rowsToUse.map(row => row[colIndex]).filter(Boolean)))
      : [];
  };

  useEffect(() => {
    state.deDatasets.forEach(d => {
      const groups = getDatasetSampleGroups(d);
      if (groups.length > 0) {
        const patch: Partial<typeof d> = {};
        if (!d.de_referenceGroup || !groups.includes(d.de_referenceGroup)) {
          patch.de_referenceGroup = groups[0];
        }
        const ref = patch.de_referenceGroup || d.de_referenceGroup || groups[0];
        const validCmpGroups = groups.filter(g => g !== ref);
        if (!d.de_comparisonGroup || !groups.includes(d.de_comparisonGroup) || d.de_comparisonGroup === ref) {
          patch.de_comparisonGroup = validCmpGroups[0] || groups[0];
        }
        if (Object.keys(patch).length > 0) {
          dispatch({ type: "DE_UPDATE_DATASET", id: d.id, patch });
        }
      }
    });
  }, [state.deDatasets, dispatch]);

  const runDE = async () => {
    setLoading(true);
    try {
      const resultsMap = await runDifferentialExpressionAPI(targetDatasets, cfg);
      targetDatasets.forEach(targetDs => {
        const resObj = resultsMap[targetDs.id] as any;
        if (!resObj) {
          throw new Error(`No DE results returned for dataset: ${targetDs.name}`);
        }
        dispatch({ type: "DE_SET_RESULTS", id: targetDs.id, results: resObj });
        dispatch({
          type: "DE_UPDATE_DATASET",
          id: targetDs.id,
          patch: {
            submitted_de_referenceGroup: targetDs.de_referenceGroup,
            submitted_de_comparisonGroup: targetDs.de_comparisonGroup,
          }
        });
      });
      dispatch({ type: "DE_SUBMIT_CONFIG" });
    } catch (err: any) {
      console.error("DE analysis error:", err);
      toast({
        title: "Differential Expression Error",
        description: err.message || "An error occurred during differential expression analysis.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const submittedCfg = state.deSubmittedConfig;

  const hasSettingsChanged = done && submittedCfg && (
    (submittedCfg.method || (showLimmaOnly ? "limma" : "deseq2")) !== method ||
    submittedCfg.pValueThreshold !== cfg.pValueThreshold ||
    submittedCfg.logFcThreshold !== cfg.logFcThreshold ||
    submittedCfg.adjustMethod !== cfg.adjustMethod ||
    targetDatasets.some(d => 
      d.de_referenceGroup !== d.submitted_de_referenceGroup || 
      d.de_comparisonGroup !== d.submitted_de_comparisonGroup
    )
  );
  const completedModules: string[] = [];
  if (state.deInlineEaDone) completedModules.push("Enrichment Analysis");
  if (state.deMetaMethod) completedModules.push("Meta-analysis");

  const downstreamDone = completedModules.length > 0;
  const handleContinue = () => {
    if (hasSettingsChanged) {
      setShowWarnModal(true);
      return;
    }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowWarnModal(false);
    handleNext();
  };

  const hasSameTypeMultiple = isMetaEligible(state.deDatasets);

  const handleNext = () => {
    dispatch({ type: "DE_SET_STEP", step: hasSameTypeMultiple ? "meta" : "module-select" });
  };

  return (
    <>
      {loading && <Spinner label="Running differential expression analysis…" sublabel={`Using ${DE_METHODS.find(m => m.id === method)?.label || "DESeq2/limma"} pipeline`} />}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Analysis Configuration</div>
        <div className="card-sub">Set comparison groups and statistical thresholds for differential expression testing.</div>
        <hr className="card-divider" />

        {/* Statistical Method */}
        {isMixed ? (
          <>
            <div style={{ marginBottom: 16 }}>
              <label style={{ fontSize: 12, fontWeight: 550, display: "block", marginBottom: 8, color: "var(--foreground)" }}>
                Statistical Method for Read Counts data
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {DE_METHODS.filter(m => m.id !== "limma").map(m => (
                  <label key={m.id} style={{
                    display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8, cursor: "pointer",
                    border: `1px solid ${method === m.id ? "var(--primary)" : "var(--border)"}`,
                    background: method === m.id ? "var(--selected-bg)" : "white",
                  }}>
                    <input type="radio" checked={method === m.id} onChange={() => setMethod(m.id)} style={{ accentColor: "var(--primary)", marginTop: 2 }} />
                    <div>
                      <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                      <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
                    </div>
                  </label>
                ))}
              </div>
            </div>

            <div style={{ marginBottom: 16 }}>
              <label style={{ fontSize: 12, fontWeight: 550, display: "block", marginBottom: 8, color: "var(--foreground)" }}>
                {(() => {
                  const hasMicroarray = targetDatasets.some(d => d.dataType === "microarray");
                  const hasOthers = targetDatasets.some(d => d.dataType === "others");
                  const hasNormCounts = targetDatasets.some(d => d.dataType === "readcounts" && d.isNormalized);
                  const list = [];
                  if (hasMicroarray) list.push("Microarray");
                  if (hasNormCounts) list.push("Normalized Counts");
                  if (hasOthers) list.push("Others");
                  return `Statistical Method for ${list.join(" / ")} data`;
                })()}
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                <label style={{
                  display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8,
                  border: "1px solid var(--primary)", background: "var(--selected-bg)", opacity: 0.85
                }}>
                  <input type="radio" checked={true} readOnly style={{ accentColor: "var(--primary)", marginTop: 2 }} />
                  <div>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>limma</div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>
                      Using linear models. Performs robust differential expression analysis for microarray, normalized, and other datasets.
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </>
        ) : (
          <div style={{ marginBottom: 16 }}>
            <label style={{ fontSize: 12, fontWeight: 550, display: "block", marginBottom: 8 }}>Statistical Method</label>
            <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
              {DE_METHODS.filter(m => showLimmaOnly ? m.id === "limma" : m.id !== "limma").map(m => (
                <label key={m.id} style={{
                  display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 12px", borderRadius: 8, cursor: "pointer",
                  border: `1px solid ${method === m.id ? "var(--primary)" : "var(--border)"}`,
                  background: method === m.id ? "var(--selected-bg)" : "white",
                }}>
                  <input type="radio" checked={method === m.id} onChange={() => setMethod(m.id)} style={{ accentColor: "var(--primary)", marginTop: 2 }} />
                  <div>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
                  </div>
                </label>
              ))}
            </div>
          </div>
        )}

        {datasetId === "all" ? (
          <div style={{ display: "flex", flexDirection: "column", gap: 12, marginBottom: 16 }}>
            <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Dataset-Specific Comparison Groups</div>
            {state.deDatasets.map(d => {
              const groups = getDatasetSampleGroups(d);
              return (
                <div key={d.id} style={{
                  display: "flex", gap: 16, flexWrap: "wrap", alignItems: "center",
                  padding: "10px 12px", border: "1px solid var(--border)", borderRadius: 8, background: "var(--selected-bg)"
                }}>
                  <div style={{ display: "flex", alignItems: "center", gap: 8, minWidth: 150 }}>
                    <div style={{ width: 8, height: 8, borderRadius: "50%", background: d.color }} />
                    <span style={{ fontWeight: 600, fontSize: 13 }}>{d.name}</span>
                  </div>
                  <div style={{ display: "flex", gap: 12, flex: 1, flexWrap: "wrap" }}>
                    {/* Reference Group Dropdown */}
                    <div style={{ display: "flex", flexDirection: "column", minWidth: 140, position: "relative" }}>
                      <span style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 4 }}>Reference Group</span>
                      <details style={{ width: "100%" }} data-testid={`details-reference-group-${d.id}`}>
                        <summary style={{
                          display: "flex", justifyContent: "space-between", alignItems: "center",
                          padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12,maxHeight: 50,
                          background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                        }}>
                          <span>{d.de_referenceGroup || "Select reference..."}</span>
                          <span style={{ fontSize: 9, color: "var(--muted-foreground)" }}>▼</span>
                        </summary>

                        <div style={{
                          position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                          border: "1px solid var(--border)", borderRadius: 6, padding: "4px 0", maxHeight: 160,
                          overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                        }}>
                          {groups.map(g => (
                            <div
                              key={g}
                              onClick={(e) => {
                                dispatch({ type: "DE_UPDATE_DATASET", id: d.id, patch: { de_referenceGroup: g } });
                                const details = (e.target as HTMLElement).closest("details");
                                if (details) details.removeAttribute("open");
                              }}
                              style={{
                                padding: "6px 8px",
                                fontSize: 12,
                                cursor: "pointer",
                                background: d.de_referenceGroup === g ? "var(--selected-bg)" : "transparent",
                                fontWeight: d.de_referenceGroup === g ? 600 : 400
                              }}
                              onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                              onMouseLeave={e => e.currentTarget.style.background = d.de_referenceGroup === g ? "var(--selected-bg)" : "transparent"}
                            >
                              {g}
                            </div>
                          ))}
                        </div>
                      </details>
                    </div>

                    {/* Comparison Group Dropdown */}
                    <div style={{ display: "flex", flexDirection: "column", minWidth: 140, position: "relative" }}>
                      <span style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 4 }}>Comparison Group</span>
                      <details style={{ width: "100%" }} data-testid={`details-comparison-group-${d.id}`}>
                        <summary style={{
                          display: "flex", justifyContent: "space-between", alignItems: "center",
                          padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12,
                          background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                        }}>
                          <span>{d.de_comparisonGroup || "Select comparison..."}</span>
                          <span style={{ fontSize: 9, color: "var(--muted-foreground)" }}>▼</span>
                        </summary>

                        <div style={{
                          position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                          border: "1px solid var(--border)", borderRadius: 6, padding: "4px 0", maxHeight: 160,
                          overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                        }}>
                          {groups.filter(g => g !== d.de_referenceGroup).map(g => (
                            <div
                              key={g}
                              onClick={(e) => {
                                dispatch({ type: "DE_UPDATE_DATASET", id: d.id, patch: { de_comparisonGroup: g } });
                                const details = (e.target as HTMLElement).closest("details");
                                if (details) details.removeAttribute("open");
                              }}
                              style={{
                                padding: "6px 8px",
                                fontSize: 12,
                                cursor: "pointer",
                                background: d.de_comparisonGroup === g ? "var(--selected-bg)" : "transparent",
                                fontWeight: d.de_comparisonGroup === g ? 600 : 400
                              }}
                              onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                              onMouseLeave={e => e.currentTarget.style.background = d.de_comparisonGroup === g ? "var(--selected-bg)" : "transparent"}
                            >
                              {g}
                            </div>
                          ))}
                        </div>
                      </details>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        ) : (
          <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 16 }}>
            {/* Single Dataset Reference Group Dropdown */}
            <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>Reference Group</label>
              <details style={{ width: "100%" }}>
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{ds?.de_referenceGroup || "Select reference..."}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>
                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {ds && getDatasetSampleGroups(ds).map(g => (
                    <div
                      key={g}
                      onClick={(e) => {
                        dispatch({ type: "DE_UPDATE_DATASET", id: ds.id, patch: { de_referenceGroup: g } });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px",
                        fontSize: 13,
                        cursor: "pointer",
                        background: ds.de_referenceGroup === g ? "var(--selected-bg)" : "transparent",
                        fontWeight: ds.de_referenceGroup === g ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = ds.de_referenceGroup === g ? "var(--selected-bg)" : "transparent"}
                    >
                      {g}
                    </div>
                  ))}
                </div>
              </details>
            </div>

            {/* Single Dataset Comparison Group Dropdown */}
            <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>Comparison Group</label>
              <details style={{ width: "100%" }}>
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{ds?.de_comparisonGroup || "Select comparison..."}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>
                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {ds && getDatasetSampleGroups(ds).filter(g => g !== ds.de_referenceGroup).map(g => (
                    <div
                      key={g}
                      onClick={(e) => {
                        dispatch({ type: "DE_UPDATE_DATASET", id: ds.id, patch: { de_comparisonGroup: g } });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px",
                        fontSize: 13,
                        cursor: "pointer",
                        background: ds.de_comparisonGroup === g ? "var(--selected-bg)" : "transparent",
                        fontWeight: ds.de_comparisonGroup === g ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = ds.de_comparisonGroup === g ? "var(--selected-bg)" : "transparent"}
                    >
                      {g}
                    </div>
                  ))}
                </div>
              </details>
            </div>
          </div>
        )}

        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 16 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>P-value threshold</label>
            <input
              type="number"
              value={cfg.pValueThreshold}
              step={0.01} min={0.001} max={0.2}
              onChange={e => dispatch({ type: "DE_SET_CONFIG", patch: { pValueThreshold: Number(e.target.value) } })}
              style={{ width: 120, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
              data-testid="input-pvalue-threshold"
            />
          </div>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>|log₂FC| threshold</label>
            <input
              type="number"
              value={cfg.logFcThreshold}
              step={0.25} min={0}
              onChange={e => dispatch({ type: "DE_SET_CONFIG", patch: { logFcThreshold: Number(e.target.value) } })}
              style={{ width: 120, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
              data-testid="input-logfc-threshold"
            />
          </div>
          {/* Adjustment Method Custom Dropdown */}
          <div style={{ display: "flex", flexDirection: "column", minWidth: 220, position: "relative" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>P-value adjustment method</label>
            <details style={{ width: "100%" }}>
              <summary style={{
                display: "flex", justifyContent: "space-between", alignItems: "center",
                padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
              }}>
                <span>{ADJ_METHODS.find(m => m.value === cfg.adjustMethod)?.label || ""}</span>
                <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
              </summary>
              <div style={{
                position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 120,
                overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
              }}>
                {ADJ_METHODS.map(m => (
                  <div
                    key={m.value}
                    onClick={(e) => {
                      dispatch({ type: "DE_SET_CONFIG", patch: { adjustMethod: m.value } });
                      const details = (e.target as HTMLElement).closest("details");
                      if (details) details.removeAttribute("open");
                    }}
                    style={{
                      padding: "8px 10px",
                      fontSize: 13,
                      cursor: "pointer",
                      background: cfg.adjustMethod === m.value ? "var(--selected-bg)" : "transparent",
                      fontWeight: cfg.adjustMethod === m.value ? 600 : 400
                    }}
                    onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                    onMouseLeave={e => e.currentTarget.style.background = cfg.adjustMethod === m.value ? "var(--selected-bg)" : "transparent"}
                  >
                    {m.label}
                  </div>
                ))}
              </div>
            </details>
          </div>
        </div>
      </div>
      
      {done && (
        <div className="card">
          <div className="card-title" style={{ marginBottom: 14 }}>
            DE Results {hasSettingsChanged === true && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated)</span>}
          </div>
          <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
            {targetDatasets.map(targetDs => {
              const res = state.deResults[targetDs.id] ?? Object.entries(state.deResults).find(([k]) => getBaseDatasetId(k) === getBaseDatasetId(targetDs.id))?.[1] ?? [];
              return (
                <DEResultCard key={targetDs.id} targetDs={targetDs} results={res} hasSettingsChanged={hasSettingsChanged} pValueThreshold={cfg.pValueThreshold} logFcThreshold={cfg.logFcThreshold} adjustMethod={cfg.adjustMethod} />
              );
            })}
          </div>
        </div>
      )}

      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="de"
          datasetIds={targetDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: datasetId,
              fromStep: "de-analysis"
            } as any);
            await runDE();
          }}
        />
      )}

      <div className="action-row">
        <button
          className="btn btn-default"
          onClick={() => {
            const isViewingAll = state.deSelectedContext === "all" && multipleDs;
            dispatch({ type: "DE_SET_STEP", step: isViewingAll ? "all-datasets" : "upload" });
          }}
        >
          ← Back
        </button>
        <div style={{ display: "flex", gap: 8 }}>
          {hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={async () => {
              if (downstreamDone) {
                setShowDiscardModal(true);
              } else {
                try {
                  await redoStepAPI("de", targetDatasets.map(d => d.id));
                } catch (e) {
                  console.error("Failed to redo DE Analysis:", e);
                }
                runDE();
              }
            }} data-testid="btn-redo-de">
              🔄 Redo DE Analysis
            </button>
          )}

          {!done ? (
            <button
              className="btn btn-primary"
              disabled={
                datasetId === "all" 
                  ? state.deDatasets.some(d => !d.de_referenceGroup || !d.de_comparisonGroup)
                  : (!ds?.de_referenceGroup || !ds?.de_comparisonGroup)
              }
              onClick={runDE}
              data-testid="btn-run-de"
            >
              Run DE Analysis
            </button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-next-de-analysis">
              {hasSameTypeMultiple ? "Continue to Meta-analysis →" : "Continue to Analysis Module →"}
            </button>
          )}
        </div>
      </div>
    </>
  );
}
