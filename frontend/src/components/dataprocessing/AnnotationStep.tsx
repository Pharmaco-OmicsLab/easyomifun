import { useState, useEffect } from "react";
import { Download, Check, Loader2 } from "lucide-react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import { computeDPSteps } from "../../dataObject";
import Spinner from "../Spinner";
import SkipModal from "../shared/SkipModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import { annotateDatasetAPI, SERVER_URL, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";


const ORGS = [
  "Human (Homo sapiens)",
  "Mouse (Mus musculus)",
  "Rat (Rattus norvegicus)",
  "Pig (Sus scrofa)",
  "Chicken (Gallus gallus)"
];
const GENE_BIOTYPES = ["Protein-coding", "All", "lncRNA", "pseudogenes", "rRNA"];
const STRATEGIES = [
  {
    id: "keep-first",
    label: "Keep first occurrence of annotation for multi-mapped genes",
    subtext: "Maintains only the first recorded match in the database, reducing redundancy but omitting alternative transcripts."
  },
  {
    id: "filter-all",
    label: "Filter all multi-mapped genes",
    subtext: "Remove genes with multiple mappings to avoid ambiguous annotations."
  }
];

interface DatasetAnnotationResult {
  id: string; name: string; color: string;
  nFeatures: number; mapped: number; unmapped: number; coverage: number;
  unique: number; multi: number; geneIdType?: string;
  retained?: number;
}

interface Props { datasetId: string; mode?: "dp" | "de" }

interface PieSlice {
  label: string;
  value: number;
  color: string;
}

function InteractivePieChart({ slices }: { slices: PieSlice[] }) {
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null);
  const total = slices.reduce((sum, s) => sum + s.value, 0);
  if (total === 0) return null;

  let accumulatedPercent = 0;
  const processedSlices = slices.map((slice, idx) => {
    const startPercent = accumulatedPercent;
    accumulatedPercent += slice.value / total;
    const endPercent = accumulatedPercent;

    if (slice.value <= 0) {
      return { ...slice, pathData: "", idx };
    }

    if (slice.value / total >= 0.9999) {
      const pathData = `M 0 -1 A 1 1 0 1 1 0 1 A 1 1 0 1 1 0 -1 Z`;
      return { ...slice, pathData, idx };
    }

    const getCoords = (percent: number) => {
      const angle = 2 * Math.PI * percent - Math.PI / 2;
      return [Math.cos(angle), Math.sin(angle)];
    };

    const [startX, startY] = getCoords(startPercent);
    const [endX, endY] = getCoords(endPercent);
    const largeArcFlag = endPercent - startPercent > 0.5 ? 1 : 0;

    const pathData = [
      `M 0 0`,
      `L ${startX} ${startY}`,
      `A 1 1 0 ${largeArcFlag} 1 ${endX} ${endY}`,
      `Z`
    ].join(" ");

    return { ...slice, pathData, idx };
  });

  return (
    <div style={{ display: "flex", gap: 32, alignItems: "center", justifyContent: "center", padding: "16px 0", flexWrap: "wrap" }}>
      <div style={{ position: "relative", width: 140, height: 140 }}>
        <svg viewBox="-1.1 -1.1 2.2 2.2" style={{ width: "100%", height: "100%" }}>
          {processedSlices.map((slice) => {
            if (slice.value <= 0 || !slice.pathData) return null;
            const isHovered = hoveredIdx === slice.idx;
            return (
              <path
                key={slice.idx}
                d={slice.pathData}
                fill={slice.color}
                stroke="#fff"
                strokeWidth={isHovered ? 0.04 : 0.01}
                style={{
                  cursor: "pointer",
                  transition: "all 0.15s ease",
                  transform: isHovered ? "scale(1.05)" : "scale(1)",
                  transformOrigin: "center"
                }}
                onMouseEnter={() => setHoveredIdx(slice.idx)}
                onMouseLeave={() => setHoveredIdx(null)}
              />
            );
          })}
        </svg>
      </div>

      <div style={{ display: "flex", flexDirection: "column", gap: 8, minWidth: 150 }}>
        {processedSlices.map((slice) => {
          const isHovered = hoveredIdx === slice.idx;
          const percentage = total > 0 ? ((slice.value / total) * 100).toFixed(1) : "0.0";
          return (
            <div
              key={slice.idx}
              style={{
                display: "flex",
                alignItems: "center",
                gap: 8,
                padding: "4px 8px",
                borderRadius: 4,
                background: isHovered ? "var(--selected-bg)" : "transparent",
                transition: "background 0.12s ease",
                cursor: "pointer"
              }}
              onMouseEnter={() => setHoveredIdx(slice.idx)}
              onMouseLeave={() => setHoveredIdx(null)}
            >
              <div style={{ width: 10, height: 10, borderRadius: 2, background: slice.color }} />
              <div style={{ fontSize: 11, fontWeight: 500, color: "var(--foreground)", flex: 1 }}>
                {slice.label}
              </div>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--muted-foreground)" }}>
                {slice.value.toLocaleString()} ({percentage}%)
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

function AnnotationResultCard({ result }: { result: DatasetAnnotationResult }) {
  const [downloaded, setDownloaded] = useState<string[]>([]);
  const [loadingFmt, setLoadingFmt] = useState<string | null>(null);
  const { state } = useAppStore();
  const { toast } = useToast();
  const slices = [
    { label: "Unique", value: result.unique, color: "#10b981" },
    { label: "Multi-mapped", value: result.multi, color: "#f59e0b" },
    { label: "Unmapped", value: result.unmapped, color: "#ef4444" }
  ];

  const handleDownload = async (fmt: string) => {
    const key = `${result.id}_${fmt.toLowerCase()}`;
    setLoadingFmt(key);
    try {
      const userId = getSessionUserId() || state.userId || "";
      const payload = {
        userId,
        type: "unmapped_results",
        dsId: result.id,
        ext: fmt.toLowerCase(),
      };

      let downloadBlob: Blob | null = null;
      let downloadFilename = "";

      const postRes = await fetch(`${SERVER_URL}/api/export-job`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });

      if (postRes.ok) {
        const postData = await postRes.json();
        if (postData.jobId) {
          const maxAttempts = 60;
          for (let i = 0; i < maxAttempts; i++) {
            await new Promise((r) => setTimeout(r, 800));
            const pollRes = await fetch(`${SERVER_URL}/api/export-job?id=${encodeURIComponent(postData.jobId)}`);
            const contentType = pollRes.headers.get("Content-Type") || "";
            if (pollRes.ok) {
              if (contentType.includes("application/json")) {
                const json = await pollRes.json();
                if (json.status === "running" || json.status === "queued") {
                  continue;
                } else if (json.status === "error" || json.status === "failed") {
                  throw new Error(json.message || "Export job failed");
                }
              } else {
                downloadBlob = await pollRes.blob();
                const disposition = pollRes.headers.get("Content-Disposition") || "";
                const match = disposition.match(/filename="?([^"]+)"?/);
                if (match) downloadFilename = match[1];
                break;
              }
            } else {
              break;
            }
          }
        }
      }

      // If export-job did not resolve or failed, fallback to direct GET /api/export-file
      if (!downloadBlob) {
        const getRes = await fetch(
          `${SERVER_URL}/api/export-file?type=unmapped_results&dsId=${encodeURIComponent(result.id)}&ext=${fmt.toLowerCase()}&userId=${encodeURIComponent(userId)}`
        );
        if (!getRes.ok) {
          let msg = `Export request failed (status ${getRes.status})`;
          try {
            const errJson = await getRes.json();
            if (errJson && errJson.message) msg = errJson.message;
          } catch (_) {}
          throw new Error(msg);
        }
        downloadBlob = await getRes.blob();
        const disposition = getRes.headers.get("Content-Disposition") || "";
        const match = disposition.match(/filename="?([^"]+)"?/);
        if (match) downloadFilename = match[1];
      }

      if (downloadBlob) {
        const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
        const fallbackName = `${datePrefix}_${result.name.replace(/[^A-Za-z0-9_-]/g, "_")}_unmapped_results.${fmt.toLowerCase()}`;
        const finalFilename = downloadFilename || fallbackName;

        const url = URL.createObjectURL(downloadBlob);
        const a = document.createElement("a");
        a.href = url;
        a.download = finalFilename;
        a.click();
        URL.revokeObjectURL(url);

        setDownloaded((prev) => [...prev, key, `${result.id}_${fmt.toUpperCase()}`]);
      }
    } catch (e: any) {
      console.error("Filtered-out IDs download failed:", e);
      toast({
        title: "Download Failed",
        description: e.message || "Failed to download filtered out gene IDs.",
        variant: "destructive",
      });
    } finally {
      setLoadingFmt(null);
    }
  };

  return (
    <div className="result-ds-card">
      <div className="result-ds-card-header">
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: result.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>{result.name}</div>
        <span style={{
          fontSize: 10, fontWeight: 700, padding: "3px 8px", borderRadius: 4,
          background: result.coverage === 0 ? "hsl(0 100% 95%)" : result.coverage >= 90 ? "hsl(150 55% 95%)" : "hsl(38 80% 95%)",
          color: result.coverage === 0 ? "hsl(0 100% 30%)" : result.coverage >= 90 ? "hsl(150 65% 28%)" : "hsl(38 70% 30%)",
          border: `1px solid ${result.coverage === 0 ? "hsl(0 100% 80%)" : result.coverage >= 90 ? "hsl(150 45% 78%)" : "hsl(38 55% 75%)"}`,
        }}>
          {result.coverage.toFixed(1)}% mapped
        </span>
      </div>
      <div className="chips-row" style={{ marginBottom: 12 }}>
        <div className="stat-chip"><div className="stat-chip-val">{result.nFeatures.toLocaleString()}</div><div className="stat-chip-lbl">Total</div></div>
        {result.retained !== undefined && result.retained !== null && (
          <div className="stat-chip"><div className="stat-chip-val">{result.retained.toLocaleString()}</div><div className="stat-chip-lbl">Features after Annotation</div></div>
        )}
        <div className="stat-chip"><div className="stat-chip-val">{result.mapped.toLocaleString()}</div><div className="stat-chip-lbl">Mapped</div></div>
        <div className="stat-chip"><div className="stat-chip-val">{result.unique.toLocaleString()}</div><div className="stat-chip-lbl">Unique</div></div>
        <div className="stat-chip"><div className="stat-chip-val">{result.multi.toLocaleString()}</div><div className="stat-chip-lbl">Multi-mapped</div></div>
        <div className="stat-chip"><div className="stat-chip-val">{result.unmapped.toLocaleString()}</div><div className="stat-chip-lbl">Unmapped</div></div>
      </div>
      <InteractivePieChart slices={slices} />
      {result.geneIdType === "genename" && (
        <div style={{
          background: "var(--selected-bg)",
          border: "1px solid var(--primary)",
          borderRadius: 8,
          padding: "10px 12px",
          marginBottom: 12,
          fontSize: 12,
          color: "var(--foreground)"
        }}>
          💡 <strong>Gene Name Input Detected:</strong> Since the input ID type is Gene Name, we queried the database for biotype information. "Mapped" shows the count of input gene names found in the reference database, "Unique" represents genes with a single biotype, and "Multi-mapped" represents genes associated with multiple biotypes.
        </div>
      )}
      <div style={{ marginTop: 12, display: "flex", flexDirection: "column", gap: 6 }}>
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--foreground)" }}>
          Download Filtered Gene IDs:
        </span>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap" }}>
          {["csv", "tsv", "xlsx"].map((fmt) => {
            const key = `${result.id}_${fmt.toLowerCase()}`;
            const isDone = downloaded.includes(key);
            const isLoader = loadingFmt === key;
            const testIds = [`btn-download-unmapped-${result.id}-${fmt}`, `btn-download-unmapped-${result.id}-${fmt.toUpperCase()}`];
            if (fmt === "csv") {
              testIds.push(`btn-download-unmapped-${result.id}`);
            }
            return (
              <button
                key={fmt}
                className="btn btn-default btn-sm"
                style={{
                  gap: 4,
                  display: "flex",
                  alignItems: "center",
                  fontSize: 11,
                  padding: "4px 8px",
                }}
                onClick={() => handleDownload(fmt)}
                disabled={loadingFmt !== null}
                data-testid={testIds.join(" ")}
              >
                {isLoader ? (
                  <Loader2 size={11} className="animate-spin" />
                ) : isDone ? (
                  <Check size={11} style={{ color: "hsl(150 55% 38%)" }} />
                ) : (
                  <Download size={11} />
                )}
                {fmt.toUpperCase()}
              </button>
            );
          })}
        </div>
      </div>
    </div>
  );
}

export default function AnnotationStep({ datasetId, mode = "dp" }: Props) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const cfg = state.dpAnnotationConfig;
  const [loading, setLoading] = useState(false);
  const [activeTabId, setActiveTabId] = useState<string>("");
  const datasets = mode === "dp" ? state.dpDatasets : state.deDatasets;

  const ds = datasets.find(d => d.id === datasetId) ?? datasets[0];
  const multipleDs = datasets.length > 1;

  // Annotation applies to readcounts + microarray (and proteomics in single mode)
  const targetDatasets = (datasetId === "all" ? datasets : (ds ? [ds] : [])).filter(d => {
    if (multipleDs) {
      return d.dataType === "microarray" || d.dataType === "readcounts";
    } else {
      return d.dataType === "microarray" || d.dataType === "readcounts" || d.dataType === "proteomics";
    }
  });
  // Separate microarray and non-microarray target datasets
  const nonMicroarrayTargets = targetDatasets.filter(d => d.dataType !== "microarray");
  const microarrayTargets    = targetDatasets.filter(d => d.dataType === "microarray");
  const showOrganismSelector = nonMicroarrayTargets.length > 0;

  // Sub-groups for multi-card layout
  const rcDatasets = targetDatasets.filter(d => d.dataType === "readcounts");
  const maDatasets = targetDatasets.filter(d => d.dataType === "microarray");
  const protDatasets = targetDatasets.filter(d => d.dataType === "proteomics");
  const isMixedAnnotation = (rcDatasets.length > 0 ? 1 : 0) + (maDatasets.length > 0 ? 1 : 0) + (protDatasets.length > 0 ? 1 : 0) > 1;

  const done = targetDatasets.length > 0 && targetDatasets.every(d => d.annotationDone);
  const [showSkipModal, setShowSkipModal] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const strategy = cfg.multiMappedStrategy || "keep-first";
  const setStrategy = (val: string) => {
    dispatch({ type: "DP_SET_ANNOTATION", patch: { multiMappedStrategy: val } });
  };

  const getAnnotationResults = () => {
    return targetDatasets
      .filter(d => d.annotationDone)
      .map(d => {
        const total = d.annotationTotal
        const mapped = d.annotationMapped
        const unique = d.annotationUnique
        const multi = d.annotationMulti
        const retained = d.annotationRetained
        return {
          id: d.id,
          name: d.name,
          color: d.color,
          nFeatures: total,
          mapped,
          unmapped: total - mapped,
          coverage: total > 0 ? (mapped / total) * 100 : 0,
          unique,
          multi,
          geneIdType: d.submittedGeneIdType || d.geneIdType,
          retained
        };
      });
  };
  const completedResults = getAnnotationResults();

  useEffect(() => {
    if (completedResults.length > 0) {
      if (!activeTabId || !completedResults.some(r => r.id === activeTabId)) {
        setActiveTabId(completedResults[0].id);
      }
    }
  }, [completedResults, activeTabId]);

  useEffect(() => {
    if (done && !state.dpSubmittedAnnotationConfig) {
      dispatch({ type: "DP_SUBMIT_ANNOTATION" });
    }
  }, [done, state.dpSubmittedAnnotationConfig, dispatch]);

  const getNextAndPrevSteps = () => {
    const activeSteps = computeDPSteps(datasets);
    const currentIdx = activeSteps.findIndex(s => s.id === "annotation");
    const next = activeSteps[currentIdx + 1]?.id || "module-select";
    let prev = activeSteps[currentIdx - 1]?.id || "upload";
    if (prev === "upload" && multipleDs) prev = "all-datasets";
    return { next, prev };
  };

  const submittedCfg = state.dpSubmittedAnnotationConfig;

  const hasSettingsChanged = done && submittedCfg && (
    (submittedCfg.selectOrganism || "") !== (cfg.selectOrganism || "") ||
    (submittedCfg.geneBiotype || "") !== (cfg.geneBiotype || "") ||
    (submittedCfg.multiMappedStrategy || "keep-first") !== (cfg.multiMappedStrategy || "keep-first")
  );

  const completedModules: string[] = [];
  targetDatasets.forEach(d => {
    if (d.processingDone && !completedModules.includes("Processing")) completedModules.push("Processing");
    if (d.normalizationDone && !completedModules.includes("Normalization")) completedModules.push("Normalization");
    if (d.batchDone && !completedModules.includes("Batch Correction")) completedModules.push("Batch Correction");
    if ((state.dpInlineDeDone || (state.dpDeResults[d.id] && state.dpDeResults[d.id].length > 0)) && !completedModules.includes("DE Analysis")) completedModules.push("DE Analysis");
  });
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");

  const downstreamDone = completedModules.length > 0;

  const handleSubmit = async () => {
    setLoading(true);
    try {
      const results = await annotateDatasetAPI(targetDatasets, cfg);
      dispatch({ type: "DP_SUBMIT_ANNOTATION" });

      if (Array.isArray(results)) {
        results.forEach(res => {
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: res.datasetId,
            patch: {
              annotationDone: true,
              annotationMapped: res.mapped,
              annotationTotal: res.total,
              annotationUnique: res.unique,
              annotationMulti: res.multi,
              annotationRetained: res.retained,
              nFeatures: res.retained,
              nSamples: res.nSamples !== undefined ? res.nSamples : undefined,
              columns: res.columns || undefined,
              parsedData: res.parsedData || undefined,
              sampleIds: res.sampleIds || undefined,
              geneIdCol: "gene_symbol",
              geneInfoCols: ["entrez_id", "gene_symbol"],
            }
          } as never);
        });
      }
    } catch (err: any) {
      console.error("Annotation error:", err);
      toast({
        title: "Annotation Error",
        description: err.message || "An error occurred during dataset annotation.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  // Intercept continue click if configurations are dirty
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
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { annotationDone: true } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const handleSkip = () => {
    targetDatasets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { annotationDone: false } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const dataTypeLabel = isMixedAnnotation
    ? "Multiple Types"
    : (rcDatasets.length > 0 ? "Readcounts" : maDatasets.length > 0 ? "Microarray" : "Proteomics");

  // Guard: no target datasets in current context
  if (targetDatasets.length === 0) return null;

  return (
    <>
      {showSkipModal && <SkipModal stepName="Annotation" stepId="annotation" datasetIds={targetDatasets.map(d => d.id)} onConfirm={handleSkip} onCancel={() => setShowSkipModal(false)} />}
      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="annotation"
          datasetIds={targetDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({ type: "RESET_DOWNSTREAM_STEPS", datasetId, fromStep: "annotation" } as any);
            await handleSubmit();
          }}
        />
      )}
      {loading && <Spinner label="Running annotation…" sublabel="Mapping gene IDs to reference genome" />}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>
          Annotation for {dataTypeLabel} {multipleDs ? "Datasets" : "Dataset"}
        </div>
        <div className="card-sub">Map gene or probe IDs to gene name using BioMart with reference genome, for easier interpretation and downstream analysis</div>
        <hr className="card-divider" />

        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 20 }}>
          
          {/* Select Organisms Dropdown */}
          {showOrganismSelector && (
            <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>Select Organisms</label>
              <details style={{ width: "100%" }} data-testid="details-select-genome">
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{cfg.selectOrganism || "Select organism..."}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>

                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {ORGS.map(g => (
                    <div
                      key={g}
                      onClick={(e) => {
                        dispatch({ type: "DP_SET_ANNOTATION", patch: { selectOrganism: g } });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px",
                        fontSize: 12,
                        cursor: "pointer",
                        background: cfg.selectOrganism === g ? "var(--selected-bg)" : "transparent",
                        fontWeight: cfg.selectOrganism === g ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = cfg.selectOrganism === g ? "var(--selected-bg)" : "transparent"}
                    >
                      {g}
                    </div>
                  ))}
                </div>
              </details>
            </div>
          )}

          {microarrayTargets.length > 0 && (
            <div style={{ display: "flex", flexDirection: "column", minWidth: 200 }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>Platform Organism</label>
              <div style={{ display: "flex", flexWrap: "wrap", gap: 8 }}>
                {microarrayTargets.map(d => (
                  <span key={d.id} style={{
                    padding: "7px 10px", borderRadius: 7, fontSize: 12,
                    background: "var(--selected-bg)", border: "1px solid var(--border)",
                    display: "inline-flex", alignItems: "center", gap: 6
                  }}>
                    <span style={{ width: 8, height: 8, borderRadius: "50%", background: d.color }} />
                    <strong>{d.name}:</strong> {d.microarrayOrganism || "Unknown organism"}
                  </span>
                ))}
              </div>
            </div>
          )}

          {/* Gene Biotypes Dropdown */}
          <div style={{ display: "flex", flexDirection: "column", minWidth: 200, position: "relative" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 6 }}>Gene Biotypes</label>
            {(() => {
              const rawBiotype = cfg.geneBiotype || "Protein-coding";
              const selectedBiotypes = (rawBiotype === "gene" || !rawBiotype) ? ["Protein-coding"] : rawBiotype.split(",");
              
              let summaryText = "";
              if (selectedBiotypes.length === 1) {
                summaryText = selectedBiotypes[0];
              } else if (selectedBiotypes.length > 1) {
                summaryText = `${selectedBiotypes.length} selected`;
              }
return (
                <details style={{ width: "100%" }}>
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{summaryText}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "8px 10px", maxHeight: 180,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {GENE_BIOTYPES.map(f => {
                      const isChecked = selectedBiotypes.includes(f);
                      return (
                        <label key={f} style={{ display: "flex", alignItems: "center", gap: 7, fontSize: 12, cursor: "pointer", padding: "4px 0" }}>
                          <input
                            type="checkbox"
                            checked={isChecked}
                            onChange={e => {
                              let next: string[];
                              if (f === "All") {
                                next = e.target.checked ? ["All"] : ["Protein-coding"];
                              } else {
                                if (e.target.checked) {
                                  next = [...selectedBiotypes.filter(x => x !== "All"), f];
                                } else {
                                  next = selectedBiotypes.filter(x => x !== f);
                                  if (next.length === 0) next = ["All"];
                                }
                              }
                              dispatch({ type: "DP_SET_ANNOTATION", patch: { geneBiotype: next.join(",") } });
                            }}
                            style={{ accentColor: "var(--primary)" }}
                            data-testid={`check-biotype-${f}`}
                          />
                          {f}
                        </label>
                      );
                    })}
                  </div>
                </details>
              );
            })()}
          </div>
        </div>

        {/* Multi-Mapped Genes Strategy Section */}
        <div style={{ display: "flex", flexDirection: "column", gap: 12, marginBottom: 20 }}>
          <label style={{ fontSize: 12, fontWeight: 500 }}>Multi-mapped Genes Strategy</label>
          <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
            {STRATEGIES.map((strat) => {
              const isSelected = strategy === strat.id;
              return (
                <label
                  key={strat.id}
                  style={{
                    display: "flex", alignItems: "flex-start", gap: 10, padding: "9px 12px",
                    borderRadius: 8, cursor: "pointer", border: `1px solid ${isSelected ? "var(--primary)" : "var(--border)"}`,
                    background: isSelected ? "var(--selected-bg)" : "white", transition: "all .12s"
                  }}
                >
                  <input
                    type="radio"
                    name="multiMappedStrategy"
                    value={strat.id}
                    checked={isSelected}
                    onChange={(e) => setStrategy(e.target.value)}
                    style={{ accentColor: "var(--primary)", marginTop: 2 }}
                  />
                  <div>
                    <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{strat.label}</div>
                    <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>
                      {strat.subtext}
                      {strat.id === "keep-first" && (
                        <div style={{ marginTop: 4, fontWeight: 500 }}>
                          {ds?.dataType === "readcounts"
                            ? "Multi-mapped gene IDs will be collapsed by taking the sum of counts."
                            : ds?.dataType === "microarray" || ds?.dataType === "proteomics"
                              ? "Multi-mapped gene IDs will be collapsed by taking the mean intensities."
                              : ""}
                        </div>
                      )}
                    </div>
                  </div>
                </label>
              );
            })}
          </div>
        </div>
      </div>

      {done && (
        <div className="card" style={{ opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Annotation Results {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated - settings changed)</span>}
          </div>

          {completedResults.length > 1 && (
            <div style={{ display: "flex", gap: 8, marginBottom: 16, borderBottom: "1px solid var(--border)", paddingBottom: 10 }}>
              {completedResults.map(res => {
                const isActive = activeTabId === res.id || (!activeTabId && completedResults[0]?.id === res.id);
                return (
                  <button
                    key={res.id}
                    onClick={() => setActiveTabId(res.id)}
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
                    {res.name}
                  </button>
                );
              })}
            </div>
          )}

          <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
            {(() => {
              const activeResult = completedResults.find(res => res.id === (activeTabId || completedResults[0]?.id));
              if (!activeResult) return null;
              return <AnnotationResultCard result={activeResult} />;
            })()}
          </div>
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: getNextAndPrevSteps().prev } as never)}>← Back</button>
          <button className="btn btn-default" onClick={() => setShowSkipModal(true)} data-testid="btn-skip-annotation" style={{ color: "var(--muted-foreground)" }}>Skip</button>
        </div>

        <div style={{ display: "flex", gap: 8 }}>
          {hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={async () => {
              if (downstreamDone) {
                setShowDiscardModal(true);
              } else {
                try {
                  await redoStepAPI("annotation", targetDatasets.map(d => d.id));
                } catch (e) {
                  console.error("Failed to redo annotation:", e);
                }
                handleSubmit();
              }
            }} data-testid="btn-redo-annotation">
              🔄 Redo Annotation
            </button>
          )}

          {!done ? (
            <button className="btn btn-primary" onClick={handleSubmit} data-testid="btn-submit-annotation">Submit</button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} disabled={completedResults.some(res => res.coverage === 0)} data-testid="btn-continue-processing">Continue →</button>
          )}
        </div>
      </div>
    </>
  );
}
