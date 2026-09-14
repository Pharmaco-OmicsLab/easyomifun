import { useState, useEffect } from "react";
import { FileText, X, AlertTriangle } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import type {PValueAdjMethod} from '../../dataObject';
import { isMetaEligible } from '../../dataObject';
import { fetchSampleDataAPI } from "../../lib/api";
import WarnSampleModal from "../shared/WarnSampleModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import { parseClinicalFile, getExpressionSampleColumns } from "../../lib/dataParser";
import InlineUploadStep from "./InlineUploadStep";
import AllDatasetsUploadView from "../shared/AllDatasetsUploadView";
import { runInlineDEAPI, redoStepAPI, clearDatasetAPI, fetchDatasetInfoAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";


const DE_METHODS = [
  { id: "deseq2", label: "DESeq2", desc: "Using a negative binomial distribution. Robust differential expression analysis for RNA-seq." },
  { id: "edger", label: "edgeR", desc: "Using a negative binomial distribution with empirical Bayes estimation. Performs well small sample sizes." },
  { id: "limma_voom", label: "limma-voom", desc: "Using linear models with precision weights (voom) for raw read counts." },
  { id: "limma", label: "limma", desc: "Using linear models. Performs robust differential expression analysis for microarray, proteomics, normalized, and other datasets." },
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

function DEResultCard({ targetDs, results, hasSettingsChanged }: { targetDs: TargetDataset; results: any; hasSettingsChanged: boolean | null }) {
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
        <div className="stat-chip"><div className="stat-chip-val" style={{ color: "hsl(0 65% 38%)" }}>{stats.sigUp}</div><div className="stat-chip-lbl">Up</div></div>
        <div className="stat-chip"><div className="stat-chip-val" style={{ color: "hsl(214 65% 30%)" }}>{stats.sigDown}</div><div className="stat-chip-lbl">Down</div></div>
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

export default function InlineDEAnalysisStep() {
  const { state, dispatch } = useAppStore();
  const allDs = state.dpDatasets;
  const multipleDs = allDs.length > 1;
  const sorted = [...allDs].sort((a, b) => a.id.localeCompare(b.id));

  const isIncomplete = (d: typeof sorted[0]) => !d.clinicalFileName || !d.clinicalGroupCol;
  const pendingDatasets = sorted.filter(isIncomplete);

  const [activeUploadDatasetId, setActiveUploadDatasetId] = useState(
    pendingDatasets.length > 0 ? pendingDatasets[0].id : (sorted[0]?.id || "")
  );
  const [showUploadSummary, setShowUploadSummary] = useState(pendingDatasets.length === 0 && multipleDs);
  const [editingFromSummary, setEditingFromSummary] = useState(false);

  // Sync state if databases update externally (e.g. clinical data cleared)
  useEffect(() => {
    const hasIncomplete = allDs.some(isIncomplete);
    if (hasIncomplete && state.inlineDeUploadReviewed) {
      dispatch({ type: "SET_INLINE_DE_UPLOAD_REVIEWED", reviewed: false });
    }
    if (!sorted.some(d => d.id === activeUploadDatasetId)) {
      const firstPending = sorted.find(isIncomplete);
      setActiveUploadDatasetId(firstPending ? firstPending.id : (sorted[0]?.id || ""));
    }
  }, [allDs, state.inlineDeUploadReviewed, sorted, activeUploadDatasetId]);

  // If already reviewed
  if (state.inlineDeUploadReviewed) {
    return (
      <InlineDEAnalysisContent
        onBack={() => {
          if (multipleDs) {
            dispatch({ type: "SET_INLINE_DE_UPLOAD_REVIEWED", reviewed: false });
            setShowUploadSummary(true);
          } else {
            dispatch({ type: "SET_INLINE_DE_UPLOAD_REVIEWED", reviewed: false });
          }
        }}
      />
    );
  }

  // Single dataset flow
  if (!multipleDs) {
    const ds = sorted[0];
    return (
      <InlineUploadStep
        key={ds.id}
        datasetId={ds.id}
        mode="de"
        onBack={() => {
          dispatch({ type: "DP_SET_STEP", step: "module-select" });
        }}
        onContinue={() => {
          dispatch({ type: "SET_INLINE_DE_UPLOAD_REVIEWED", reviewed: true });
        }}
      />
    );
  }

  // Multi-dataset flow - Upload Summary view
  if (showUploadSummary) {
    return (
      <AllDatasetsUploadView
        title="All Datasets — Datasets Upload Summary"
        subtitle="Overview of clinical data upload status. Ensure all datasets have matched samples before proceeding."
        datasets={allDs as any}
        showClinicalDetails={true}
        onDatasetClick={(id) => {
          setActiveUploadDatasetId(id);
          setShowUploadSummary(false);
          setEditingFromSummary(true);
        }}
        onContinue={() => {
          dispatch({ type: "SET_INLINE_DE_UPLOAD_REVIEWED", reviewed: true });
        }}
        isContinueDisabled={pendingDatasets.length > 0}
        continueLabel="Continue to DE Analysis →"
        onBack={() => {
          if (pendingDatasets.length > 0) {
            setActiveUploadDatasetId(pendingDatasets[pendingDatasets.length - 1].id);
            setShowUploadSummary(false);
          } else {
            dispatch({ type: "DP_SET_STEP", step: "module-select" });
          }
        }}
      />
    );
  }

  // Multi-dataset flow - Sequential upload steps
  const targetId = activeUploadDatasetId;
  const pendingIdx = pendingDatasets.findIndex(d => d.id === targetId);

  return (
    <InlineUploadStep
      key={targetId}
      datasetId={targetId}
      mode="de"
      editingFromSummary={editingFromSummary}
      onBack={() => {
        if (editingFromSummary) {
          setShowUploadSummary(true);
          setEditingFromSummary(false);
        } else if (pendingIdx > 0) {
          setActiveUploadDatasetId(pendingDatasets[pendingIdx - 1].id);
        } else {
          dispatch({ type: "DP_SET_STEP", step: "module-select" });
        }
      }}
      onContinue={() => {
        if (editingFromSummary) {
          setShowUploadSummary(true);
          setEditingFromSummary(false);
        } else if (pendingIdx < pendingDatasets.length - 1) {
          setActiveUploadDatasetId(pendingDatasets[pendingIdx + 1].id);
        } else {
          setShowUploadSummary(true);
        }
      }}
    />
  );
}

/**
 * Inner content: all useState/useEffect hooks live here, called unconditionally
 * on every render so React's hook ordering is never violated.
 */
function InlineDEAnalysisContent({ onBack }: { onBack?: () => void }) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const ds = state.dpDatasets.find(d => d.id === state.dpCurrentDatasetId) ?? state.dpDatasets[0];
  const allDs = state.dpDatasets;
  const isViewingAll = state.dpSelectedContext === "all" && allDs.length > 1;
  const targetDatasets = isViewingAll ? allDs : (ds ? [ds] : []);

  const unnormReadcountsDs = targetDatasets.filter(d => d.dataType === "readcounts" && !d.isNormalized);
  const limmaDs = targetDatasets.filter(d => d.dataType === "microarray" || d.dataType === "proteomics" || d.dataType === "others" || d.isNormalized);
  const hasUnnormReadcounts = unnormReadcountsDs.length > 0;
  const hasLimma = limmaDs.length > 0;
  const isMixed = hasUnnormReadcounts && hasLimma;

  const showLimmaOnly = hasLimma && !hasUnnormReadcounts;
  
  const [loading, setLoading] = useState(false);
  const [done, setDone] = useState(state.dpInlineDeDone);
  const [method, setMethod] = useState(showLimmaOnly ? "limma" : "deseq2");
  
  // Local thresholds state (shared)
  const [pThresh, setPThresh] = useState(0.05);
  const [fcThresh, setFcThresh] = useState(1.0);
  const [adjMethod, setAdjMethod] = useState("BH");
  
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showChangeWarnModal, setShowChangeWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const [submittedCfg, setSubmittedCfg] = useState<{
    method: string;
    pThresh: number;
    fcThresh: number;
    adjMethod: string;
  } | null>(
    state.dpInlineDeDone ? {
      method: showLimmaOnly ? "limma" : "deseq2",
      pThresh: 0.05,
      fcThresh: 1.0,
      adjMethod: "BH"
    } : null
  );

  // Clinical upload states and helpers
  const [clinicalDrag, setClinicalDrag] = useState(false);
  const [localLoading, setLocalLoading] = useState(false);
  const [localLoadingMsg, setLocalLoadingMsg] = useState("");

  const updateDs = (patch: Partial<typeof ds>) => {
    dispatch({ type: "DP_UPDATE_DATASET", id: ds.id, patch } as never);
  };

  const handleClear = async () => {
    try {
      await clearDatasetAPI(ds.id, "clinical");
    } catch (e) {
      console.warn("clearDatasetAPI failed (non-fatal):", e);
    }
    updateDs({ 
      clinicalFile: null, 
      clinicalFileName: "", 
      clinicalSampleIdCol: "",
      clinicalGroupCol: "",
      clinicalColumns: undefined,
      clinicalParsedData: undefined,
      deUploadBypassed: false,
      fsUploadBypassed: false,
      integrityOk: true,
      integrityIssues: []
    });
  };

  const handleLoadSample = async () => {
    setLocalLoading(true);
    setLocalLoadingMsg("Loading sample clinical data…");
    try {
      const result = await fetchSampleDataAPI("clinical");
      const rows = result.parsedData as string[][];
      updateDs({
        clinicalFileName: "sample_clinical.csv",
        clinicalSampleIdCol: result.columns[0],
        clinicalGroupCol: result.columns[1],
        clinicalColumns: result.columns,
        clinicalParsedData: rows,
        deUploadBypassed: false,
        fsUploadBypassed: false,
      });
    } catch (err: any) {
      updateDs({ integrityIssues: [`Failed to load sample data: ${err.message}`] });
    } finally {
      setLocalLoading(false);
    }
  };

  const handleClinicalFile = async (file: File) => {
    setLocalLoadingMsg(`Parsing ${file.name}…`);
    setLocalLoading(true);
    try {
      const parsed = await parseClinicalFile(file);
      updateDs({
        clinicalFile: file,
        clinicalFileName: file.name,
        clinicalSampleIdCol: parsed.clinicalSampleIdCol,
        clinicalGroupCol: parsed.clinicalGroupCol,
        clinicalColumns: parsed.clinicalColumns,
        clinicalParsedData: parsed.clinicalParsedData,
        deUploadBypassed: false,
        fsUploadBypassed: false,
      });
    } catch (error: any) {
      updateDs({
        integrityIssues: [`Error parsing clinical file: ${error.message}`],
      });
    } finally {
      setLocalLoading(false);
    }
  };

  // Keep method selection synchronized with dataset status
  useEffect(() => {
    if (method === "limma" && !showLimmaOnly && !isMixed) {
      setMethod("deseq2");
    } else if (method !== "limma" && showLimmaOnly) {
      setMethod("limma");
    }
  }, [showLimmaOnly, method, isMixed]);

  useEffect(() => {
    state.dpDatasets.forEach(d => {
      const columnsToUse = d.clinicalColumns && d.clinicalColumns.length > 0 ? d.clinicalColumns : [];
      const rowsToUse = d.clinicalParsedData && d.clinicalParsedData.length > 0 ? d.clinicalParsedData : [];
      const groupColName = d.clinicalGroupCol || "Group";
      const groupColIdx = columnsToUse.indexOf(groupColName);
      const groups = groupColIdx !== -1
        ? Array.from(new Set(rowsToUse.map(row => row[groupColIdx]).filter(Boolean)))
        : [];

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
          dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch } as never);
        }
      }
    });
  }, [state.dpDatasets, dispatch]);

  // Sync authoritative dataset statistics from backend on mount if missing
  useEffect(() => {
    const unSynced = allDs.filter(d => !d.matchingSamples || d.matchingSamples.length === 0 || !d.groups || d.groups.length === 0);
    if (unSynced.length > 0) {
      fetchDatasetInfoAPI(allDs.map(d => d.id), "dp")
        .then(infoRes => {
          if (infoRes && infoRes.datasets) {
            Object.entries(infoRes.datasets).forEach(([dsId, info]) => {
              dispatch({
                type: "DP_UPDATE_DATASET",
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
              } as never);
            });
          }
        })
        .catch(err => console.warn("Failed to sync dataset info:", err));
    }
  }, []);

  const getDatasetSampleGroups = (targetDs: typeof ds) => {
    if (targetDs && targetDs.groups && targetDs.groups.length > 0) {
      return targetDs.groups;
    }
    const cols = targetDs.clinicalColumns && targetDs.clinicalColumns.length > 0 ? targetDs.clinicalColumns : [];
    const rows = targetDs.clinicalParsedData && targetDs.clinicalParsedData.length > 0 ? targetDs.clinicalParsedData : [];
    const groupCol = targetDs.clinicalGroupCol || "Group";
    const groupIdx = cols.indexOf(groupCol);
    return groupIdx !== -1
      ? Array.from(new Set(rows.map(row => row[groupIdx]).filter(Boolean)))
      : [];
  };

  const columnsToUse = ds && ds.clinicalColumns && ds.clinicalColumns.length > 0
    ? ds.clinicalColumns
    : [];
  const rowsToUse = ds && ds.clinicalParsedData && ds.clinicalParsedData.length > 0
    ? ds.clinicalParsedData
    : [];

  // Calculate matching / overlap variables (preferring authoritative backend stats)
  const exprSamples = ds
    ? ((ds.sampleIds && ds.sampleIds.length > 0)
      ? ds.sampleIds
      : getExpressionSampleColumns(ds.columns || [], ds.geneIdCol, ds.geneInfoCols))
    : [];
  const sampleIdIdx = ds && ds.clinicalFileName && ds.clinicalSampleIdCol && ds.clinicalColumns
    ? ds.clinicalColumns.indexOf(ds.clinicalSampleIdCol)
    : -1;

  const clinSamples = sampleIdIdx !== -1 && rowsToUse
    ? rowsToUse.map(row => row[sampleIdIdx]).filter(Boolean)
    : [];
  const clinSet = new Set(clinSamples);

  const matchingSamples = (ds && ds.matchingSamples && ds.matchingSamples.length > 0)
    ? ds.matchingSamples
    : (exprSamples.length > 0 && clinSamples.length > 0 ? exprSamples.filter(s => clinSet.has(s)) : exprSamples);
  const matchingSet = new Set(matchingSamples);

  const filteredRows = sampleIdIdx !== -1
    ? rowsToUse.filter(row => matchingSet.has(row[sampleIdIdx]))
    : rowsToUse;

  const hasClinical = ds ? (!!ds.clinicalFileName || !!ds.clinicalUploadId || !!ds.clinicalFilePath) : false;
  const isNoOverlap = hasClinical && exprSamples.length > 0 && matchingSamples.length === 0;
  const isPartialMatch = hasClinical && exprSamples.length > 0 && matchingSamples.length > 0 && (matchingSamples.length < exprSamples.length);


  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.method !== method ||
    submittedCfg.pThresh !== pThresh ||
    submittedCfg.fcThresh !== fcThresh ||
    submittedCfg.adjMethod !== adjMethod ||
    allDs.some(d =>
      d.de_referenceGroup !== d.submitted_de_referenceGroup ||
      d.de_comparisonGroup !== d.submitted_de_comparisonGroup
    )
  );
  const completedModules: string[] = [];
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");

  const downstreamDone = completedModules.length > 0;
  const handleRun = async () => {
    setLoading(true);
    if (done) {
      try {
        await redoStepAPI("de", allDs.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    try {
      const datasetsToRun = allDs.map(d => {
        const groups = getDatasetSampleGroups(d);
        const ref = d.de_referenceGroup || groups[0] || "";
        const cmp = d.de_comparisonGroup || groups.filter(g => g !== ref)[0] || groups[0] || "";
        return {
          ...d,
          de_referenceGroup: ref,
          de_comparisonGroup: cmp
        };
      });
      const resultsMap = await runInlineDEAPI(datasetsToRun, {
        method,
        pValueThreshold: pThresh,
        logFcThreshold: fcThresh,
        adjustMethod: adjMethod as PValueAdjMethod,
        referenceGroup: "",
        comparisonGroup: ""
      });

      setSubmittedCfg({
        method,
        pThresh,
        fcThresh,
        adjMethod
      });
      
      allDs.forEach(targetDs => {
        const dsToRunObj = datasetsToRun.find(td => td.id === targetDs.id);
        dispatch({
          type: "DP_UPDATE_DATASET",
          id: targetDs.id,
          patch: {
            de_referenceGroup: dsToRunObj?.de_referenceGroup || "",
            de_comparisonGroup: dsToRunObj?.de_comparisonGroup || "",
            submitted_de_referenceGroup: dsToRunObj?.de_referenceGroup || "",
            submitted_de_comparisonGroup: dsToRunObj?.de_comparisonGroup || "",
          }
        } as never);
        
        const resObj = resultsMap[targetDs.id] as any;
        dispatch({
          type: "DP_SET_DE_RESULTS",
          id: targetDs.id,
          results: resObj
        } as never);
      });
      
      dispatch({ type: "SET_DP_INLINE_DE_DONE", done: true });
      dispatch({ type: "SET_DP_INLINE_META_DONE", done: false });
      setDone(true);
    } catch (err: any) {
      console.error("Inline DE analysis error:", err);
      toast({
        title: "DE Analysis Error",
        description: err.message || "Failed to execute differential expression analysis.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const onRunClick = () => {
    if (isPartialMatch && !done) {
      setShowWarnModal(true);
    } else {
      handleRun();
    }
  };

  const handleConfirmWarn = () => {
    setShowWarnModal(false);
    handleRun();
  };

  const handleContinue = () => {
    if (hasSettingsChanged) {
      setShowChangeWarnModal(true);
      return;
    }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowChangeWarnModal(false);
    const hasSameTypeMultiple = isMetaEligible(state.dpDatasets);
    dispatch({ type: "DP_SET_STEP", step: hasSameTypeMultiple ? "inline-de-meta" : "module-select" });
  };

  return (
    <>
      {localLoading && <Spinner label={localLoadingMsg} sublabel="Please wait…" />}
      {loading && <Spinner label="Running DE analysis…" sublabel={`Using ${DE_METHODS.find(m => m.id === method)?.label}`} />}

      {/* Clinical Data Upload */}
      <div className="card" style={{ marginBottom: 14, display: "none" }}>
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">
              Clinical / Phenotype Data
              <span style={{
                marginLeft: 8, 
                fontSize: 11, 
                background: "hsl(0 70% 96%)", 
                color: "hsl(0 65% 40%)",
                border: "1px solid hsl(0 50% 85%)", 
                padding: "2px 7px", 
                borderRadius: 4, 
                fontWeight: 500,
              }}>
                Required
              </span>
            </div>
            <div className="card-sub">Upload sample metadata with condition/group assignments for analysis.</div>
          </div>
          <div style={{ display: "flex", gap: 10 }}>
            <button className="link-button danger" onClick={handleClear}>Clear</button>
            <button className="link-button primary" onClick={handleLoadSample}>Example Data</button>
          </div>
        </div>

        {!ds.clinicalFileName ? (
          <div className={`drop-zone ${clinicalDrag ? "dragover" : ""}`} style={{ marginTop: 16 }}
            onClick={() => {
              const input = document.createElement("input");
              input.type = "file"; input.accept = ".csv,.tsv,.txt";
              input.onchange = e => { const f = (e.target as HTMLInputElement).files?.[0]; if (f) handleClinicalFile(f); };
              input.click();
            }}
            onDragOver={e => { e.preventDefault(); setClinicalDrag(true); }}
            onDragLeave={() => setClinicalDrag(false)}
            onDrop={e => { e.preventDefault(); setClinicalDrag(false); const f = e.dataTransfer.files?.[0]; if (f) handleClinicalFile(f); }}
            data-testid="dropzone-clinical">
            <div className="drop-zone-icon">🧬</div>
            <div className="drop-zone-title">Drop clinical metadata file here</div>
            <div className="drop-zone-hint">CSV · TSV &nbsp;·&nbsp; SampleID + Group columns required</div>
          </div>
        ) : (
          <>
            <div className="file-chip" style={{ marginTop: 14 }}>
              <FileText size={15} />
              {isPartialMatch || isNoOverlap ? (
                <>
                  <span className="file-chip-name">
                    {ds.clinicalFileName}
                  </span>
                  <span style={{ fontSize: 12, color: "hsl(38 85% 35%)" }}>
                    ({matchingSamples.length} samples matched with the {exprSamples.length} expression samples)
                  </span>
                </>
              ) : (
                <>
                  <span className="file-chip-name">{ds.clinicalFileName}</span>
                  <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>
                    ({matchingSamples.length} samples matched)
                  </span>
                </>
              )}
              <button className="file-chip-clear" onClick={handleClear} data-testid="btn-clear-clinical"><X size={16} /></button>
            </div>

            {/* Custom selector stacked layout next to each other */}
            <div style={{ marginTop: 14, display: "flex", gap: 16, flexWrap: "wrap" }}>
              <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Sample ID Column:</label>
                <details style={{ width: "100%" }} data-testid="details-clinical-sample-id-col">
                  <summary style={{display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"}}>
                    <span>{ds.clinicalSampleIdCol || "Select column..."}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>
                  
                  <div style={{position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4, 
                  border: "1px solid var(--border)",  borderRadius: 7,  padding: "4px 0",  maxHeight: 160,  
                  overflowY: "auto",  background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"}}>
                    {(ds.clinicalColumns || []).map(c => (
                      <div
                        key={c}
                        onClick={(e) => {
                          updateDs({ clinicalSampleIdCol: c, deUploadBypassed: false, fsUploadBypassed: false });
                          const details = (e.target as HTMLElement).closest("details");
                          if (details) details.removeAttribute("open");
                        }}
                        style={{
                          padding: "8px 10px",
                          fontSize: 12,
                          cursor: "pointer",
                          background: ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent",
                          fontWeight: ds.clinicalSampleIdCol === c ? 600 : 400
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                        onMouseLeave={e => e.currentTarget.style.background = ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent"}
                      >
                        {c}
                      </div>
                    ))}
                  </div>
                </details>
              </div>
            </div>

            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Clinical Data Preview (Overlapping Samples Only)
            </div>
            <div className="preview-wrap">
              <table>
                <thead><tr>{(columnsToUse).map(col => <th key={col}>{col}</th>)}</tr></thead>
                <tbody>{(filteredRows).map((row, i) => <tr key={i}>{row.map((cell, j) => <td key={j}>{cell}</td>)}</tr>)}</tbody>
              </table>
            </div>
          </>
        )}
      </div>

      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 4 }}>Differential Expression Configuration</div>
        <div className="card-sub">Configure group comparisons using the processed expression data. No re-upload required.</div>
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
                  const hasProteomics = targetDatasets.some(d => d.dataType === "proteomics");
                  const hasOthers = targetDatasets.some(d => d.dataType === "others");
                  const hasNormCounts = targetDatasets.some(d => d.dataType === "readcounts" && d.isNormalized);
                  const list = [];
                  if (hasMicroarray) list.push("Microarray");
                  if (hasProteomics) list.push("Proteomics");
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
                      Using linear models. Performs robust differential expression analysis for microarray, proteomics, normalized, and other datasets.
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </>
        ) : (
          <div style={{ marginBottom: 16 }}>
            <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 8 }}>Statistical Method</label>
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

        {/* Group selectors */}
        {allDs.length > 1 ? (
          <div style={{ display: "flex", flexDirection: "column", gap: 12, marginBottom: 14 }}>
            <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>Dataset-Specific Comparison Groups</div>
            {allDs.map(d => {
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
                      <details style={{ width: "100%" }}>
                        <summary style={{
                          display: "flex", justifyContent: "space-between", alignItems: "center",
                          padding: "6px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12,
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
                                const nextComp = (d.de_comparisonGroup === g) ? (groups.find(x => x !== g) || "") : d.de_comparisonGroup;
                                dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { de_referenceGroup: g, de_comparisonGroup: nextComp } } as never);
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
                      <details style={{ width: "100%" }}>
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
                          {groups.map(g => (
                            <div
                              key={g}
                              onClick={(e) => {
                                const nextRef = (d.de_referenceGroup === g) ? (groups.find(x => x !== g) || "") : d.de_referenceGroup;
                                dispatch({ type: "DP_UPDATE_DATASET", id: d.id, patch: { de_comparisonGroup: g, de_referenceGroup: nextRef } } as never);
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
                  {ds && getDatasetSampleGroups(ds).map(g => {
                    const groups = getDatasetSampleGroups(ds);
                    return (
                      <div
                        key={g}
                        onClick={(e) => {
                          const nextComp = (ds.de_comparisonGroup === g) ? (groups.find(x => x !== g) || "") : ds.de_comparisonGroup;
                          dispatch({ type: "DP_UPDATE_DATASET", id: ds.id, patch: { de_referenceGroup: g, de_comparisonGroup: nextComp } } as never);
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
                    );
                  })}
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
                  {ds && getDatasetSampleGroups(ds).map(g => {
                    const groups = getDatasetSampleGroups(ds);
                    return (
                      <div
                        key={g}
                        onClick={(e) => {
                          const nextRef = (ds.de_referenceGroup === g) ? (groups.find(x => x !== g) || "") : ds.de_referenceGroup;
                          dispatch({ type: "DP_UPDATE_DATASET", id: ds.id, patch: { de_comparisonGroup: g, de_referenceGroup: nextRef } } as never);
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
                    );
                  })}
                </div>
              </details>
            </div>
          </div>
        )}

        {/* Thresholds */}
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 14 }}>
          <div>
            <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 5 }}>p-value threshold</label>
            <input type="number" value={pThresh} min={0} max={1} step={0.01} onChange={e => setPThresh(Number(e.target.value))} style={{ width: 100, padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }} />
          </div>
          <div>
            <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 5 }}>|log₂FC| threshold</label>
            <input type="number" value={fcThresh} min={0} step={0.1} onChange={e => setFcThresh(Number(e.target.value))} style={{ width: 100, padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }} />
          </div>

          {/* Custom Dropdown for Adjustment Method */}
          <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
            <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 5 }}>Adjustment Method</label>
            <details style={{ width: "100%" }}>
              <summary style={{
                display: "flex", justifyContent: "space-between", alignItems: "center",
                padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
              }}>
                <span>{ADJ_METHODS.find(m => m.value === adjMethod)?.label || adjMethod}</span>
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
                      setAdjMethod(m.value);
                      const details = e.currentTarget.closest("details");
                      if (details) details.removeAttribute("open");
                    }}
                    style={{
                      padding: "8px 10px",
                      fontSize: 13,
                      cursor: "pointer",
                      background: adjMethod === m.value ? "var(--selected-bg)" : "transparent",
                      fontWeight: adjMethod === m.value ? 600 : 400
                    }}
                    onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                    onMouseLeave={e => e.currentTarget.style.background = adjMethod === m.value ? "var(--selected-bg)" : "transparent"}
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
              const res = state.dpDeResults[targetDs.id] ?? Object.entries(state.dpDeResults).find(([k]) => getBaseDatasetId(k) === getBaseDatasetId(targetDs.id))?.[1] ?? [];
              return (
                <DEResultCard key={targetDs.id} targetDs={targetDs} results={res} hasSettingsChanged={hasSettingsChanged} />
              );
            })}
          </div>
        </div>
      )}

      <div className="action-row">
        <button className="btn btn-default" onClick={() => {
          if (onBack) {
            onBack();
          } else {
            dispatch({ type: "DP_SET_STEP", step: "module-select" });
          }
        }}>← Back</button>
        <div style={{ display: "flex", gap: 8 }}>
          {hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={() => {
              if (downstreamDone) {
                setShowDiscardModal(true);
              } else {
                onRunClick();
              }
            }} data-testid="btn-redo-de">
              🔄 Redo DE Analysis
            </button>
          )}
          {!done ? (
            <button className="btn btn-primary" onClick={onRunClick} data-testid="btn-run-inline-de">Run DE Analysis</button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-inline-de-continue">Continue →</button>
          )}
        </div>
      </div>

      {showWarnModal && (
        <WarnSampleModal
          type="partial_overlap"
          onConfirm={handleConfirmWarn}
          onCancel={() => setShowWarnModal(false)}
        />
      )}

      {showChangeWarnModal && (
        <ChangeWarnModal
          onConfirm={proceedToNextStep}
          onCancel={() => setShowChangeWarnModal(false)}
        />
      )}

      {showDiscardModal && (
        <DiscardWarnModal
          stepId="de"
          datasetIds={[ds.id]}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: ds.id,
              fromStep: "de-analysis"
            } as any);
            dispatch({ type: "SET_DP_INLINE_DE_DONE", done: false });
            onRunClick();
          }}
        />
      )}
    </>
  );
}
