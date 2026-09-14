import { useState, useEffect, useMemo } from "react";
import { useAppStore } from "../../store/appStore";
import { computeDPSteps } from "../../dataObject";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import SkipModal from "../shared/SkipModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import type { BatchMethod} from "../../dataObject";
import InlineUploadStep from "../inline/InlineUploadStep";
import AllDatasetsUploadView from "../shared/AllDatasetsUploadView";
import { fetchPCAResultsAPI, runBatchCorrectionAPI, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

interface Props { datasetId: string; mode?: "dp" | "de" }

const BATCH_METHODS_RAW: { value: BatchMethod; label: string; desc: string }[] = [
  { value: "combat_seq", label: "ComBat-seq", desc: "Empirical Bayes-based batch correction. Specifically designed for raw RNA-seq read counts." },
];

const BATCH_METHODS_OTHERS: { value: BatchMethod; label: string; desc: string }[] = [
  { value: "combat", label: "ComBat (parametric)", desc: "Empirical Bayes-based batch correction, widely used for microarray and normalized RNA-seq." },
  { value: "limma_removebatch", label: "removeBatchEffect (limma)", desc: "Linear model-based batch effect removal, best when batch is a known factor." },
];

export default function BatchEffectsStep({ datasetId, mode = "dp" }: Props) {
  const { state, dispatch } = useAppStore();
  const datasets = mode === "dp" ? state.dpDatasets : state.deDatasets;
  const multipleDs = datasets.length > 1;
  const sorted = [...datasets].sort((a, b) => a.id.localeCompare(b.id));

  const isIncomplete = (d: typeof sorted[0]) => !d.clinicalFileName || !d.clinicalBatchCol;
  const pendingDatasets = sorted.filter(isIncomplete);

  const [activeUploadDatasetId, setActiveUploadDatasetId] = useState(
    pendingDatasets.length > 0 ? pendingDatasets[0].id : (sorted[0]?.id || "")
  );
  const [showUploadSummary, setShowUploadSummary] = useState(pendingDatasets.length === 0 && multipleDs);
  const [editingFromSummary, setEditingFromSummary] = useState(false);
  const [showSummarySkipModal, setShowSummarySkipModal] = useState(false);
  const confirmedSummary = state.inlineDpUploadReviewed;
  const setConfirmedSummary = (reviewed: boolean) => {
    dispatch({ type: "SET_INLINE_DP_UPLOAD_REVIEWED", reviewed });
  };
  // Sync state if databases update externally (e.g. clinical data cleared)
  useEffect(() => {
    const hasIncomplete = datasets.some(isIncomplete);
    if (hasIncomplete && confirmedSummary) {
      setConfirmedSummary(false);
    }
    if (!sorted.some(d => d.id === activeUploadDatasetId)) {
      const firstPending = sorted.find(isIncomplete);
      setActiveUploadDatasetId(firstPending ? firstPending.id : (sorted[0]?.id || ""));
    }
  }, [datasets, confirmedSummary, sorted, activeUploadDatasetId]);

  // If already confirmed
  if (confirmedSummary) {
    return (
      <BatchEffectsContent
        datasetId={datasetId}
        mode={mode}
        onBack={() => {
          if (multipleDs) {
            setConfirmedSummary(false);
            setShowUploadSummary(true);
          } else {
            setConfirmedSummary(false);
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
        mode="batch"
        onBack={() => {
          const activeSteps = computeDPSteps(datasets);
          const currentIdx = activeSteps.findIndex(s => s.id === "batch");
          let prev = activeSteps[currentIdx - 1]?.id || "upload";
          dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: prev as any });
        }}
        onContinue={() => {
          setConfirmedSummary(true);
        }}
      />
    );
  }

  // Multi-dataset flow - Upload Summary view
  if (showUploadSummary) {
    return (
      <>
        {showSummarySkipModal && (
          <SkipModal
            stepName="Batch Effects Removal"
            stepId="batch"
            datasetIds={datasets.map(d => d.id)}
            onConfirm={() => {
              setShowSummarySkipModal(false);
              datasets.forEach(t => {
                dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: t.id, patch: { batchDone: false } } as never);
              });
              const activeSteps = computeDPSteps(datasets);
              const currentIdx = activeSteps.findIndex(s => s.id === "batch");
              const next = activeSteps[currentIdx + 1]?.id || "module-select";
              dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
            }}
            onCancel={() => setShowSummarySkipModal(false)}
          />
        )}
        <AllDatasetsUploadView
          title="All Datasets — Datasets Upload Summary"
          subtitle="Overview of clinical data upload status. Ensure all datasets have matched samples before proceeding."
          datasets={datasets as any}
          showClinicalDetails={true}
          stepId="batch" // <-- Add this prop here
          onDatasetClick={(id) => {
            setActiveUploadDatasetId(id);
            setShowUploadSummary(false);
            setEditingFromSummary(true);
          }}
          onContinue={() => {
            setConfirmedSummary(true);
            setShowUploadSummary(false);
          }}
          isContinueDisabled={pendingDatasets.length > 0}
          onSkip={() => setShowSummarySkipModal(true)}
          continueLabel="Continue to Batch Correction →"
          onBack={() => {
            if (pendingDatasets.length > 0) {
              setActiveUploadDatasetId(pendingDatasets[pendingDatasets.length - 1].id);
              setShowUploadSummary(false);
            } else {
              const activeSteps = computeDPSteps(datasets);
              const currentIdx = activeSteps.findIndex(s => s.id === "batch");
              let prevStep = activeSteps[currentIdx - 1]?.id || "upload";
              if (prevStep === "upload" && multipleDs) prevStep = "all-datasets";
              dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: prevStep as any });
            }
          }}
        />
      </>
    );
  }

  // Multi-dataset flow - Sequential upload steps
  const targetId = activeUploadDatasetId;
  const pendingIdx = pendingDatasets.findIndex(d => d.id === targetId);

  return (
    <InlineUploadStep
      key={targetId}
      datasetId={targetId}
      mode="batch"
      editingFromSummary={editingFromSummary}
      onBack={() => {
        if (editingFromSummary) {
          setShowUploadSummary(true);
          setEditingFromSummary(false);
        } else if (pendingIdx > 0) {
          setActiveUploadDatasetId(pendingDatasets[pendingIdx - 1].id);
        } else {
          const activeSteps = computeDPSteps(datasets);
          const currentIdx = activeSteps.findIndex(s => s.id === "batch");
          let prevStep = activeSteps[currentIdx - 1]?.id || "upload";
          if (prevStep === "upload" && multipleDs) prevStep = "all-datasets";
          dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: prevStep } as never);
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

interface BatchContentProps {
  datasetId: string;
  mode?: "dp" | "de";
  onBack?: () => void;
}

// Client-side cache for PCA plots to prevent redundant API queries when navigating back/forth
let pcaPlotsCache: Record<string, any> = {};

function BatchEffectsContent({ datasetId, mode = "dp", onBack }: BatchContentProps) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const cfg = state.dpBatchConfig;
  const [loading, setLoading] = useState(false);
  const [showSkipModal, setShowSkipModal] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const [showRedoWarnModal, setShowRedoWarnModal] = useState(false);
  const [batchSelections, setBatchSelections] = useState<Record<string, string>>({});
  const [colorBy, setColorBy] = useState<"batch" | "group" | "covariate">("batch");
  const [pcaPoints, setPcaPoints] = useState<any>(null);

  const [loadingPCA, setLoadingPCA] = useState(false);

  const datasets = mode === "dp" ? state.dpDatasets : state.deDatasets;
  const ds = datasets.find(d => d.id === datasetId) ?? datasets[0];
  const allSameDataType = datasets.length > 0 && datasets.every(d => d.dataType === datasets[0].dataType);

  const cacheKey = datasets.map(d => d.id).join(",") + "_" + datasetId;

  useEffect(() => {
    let active = true;
    const loadPrePCA = async () => {
      // Return cached plots if they exist
      if (pcaPlotsCache[cacheKey]) {
        setPcaPoints(pcaPlotsCache[cacheKey]);
        return;
      }

      setLoadingPCA(true);
      try {
        const points = await fetchPCAResultsAPI(datasets);
        if (active) {
          setPcaPoints(points);
          pcaPlotsCache[cacheKey] = points;
        }
      } catch (err: any) {
        console.error("Error loading pre-correction PCA:", err);
        toast({
          title: "PCA Loading Error",
          description: err.message || "Failed to load pre-correction PCA points.",
          variant: "destructive"
        });
      } finally {
        if (active) {
          setLoadingPCA(false);
        }
      }
    };
    loadPrePCA();
    return () => { active = false; };
  }, [datasets, cacheKey]);

  const transcriptomicsDs = datasets.filter(d => d.dataType === "readcounts" || d.dataType === "microarray");
  const othersDs = datasets.filter(d => d.dataType === "others");
  const hasTranscriptomics = transcriptomicsDs.length > 0;
  const hasOthers = othersDs.length > 0;
  const isMixedBatch = hasTranscriptomics && hasOthers;

  const multipleDs = mode === "dp" ? state.dpDatasets.length > 1 : state.deDatasets.length > 1;
  const isAllDone = datasetId === "all" ? datasets.every(d => d.batchDone) : (ds?.batchDone || false);
  const [done, setDone] = useState(isAllDone);

  useEffect(() => {
    setDone(isAllDone);
  }, [isAllDone]);

  const getClinicalAttributes = (d: any): string[] => {
    if (d.clinicalColumns && d.clinicalColumns.length > 0) return d.clinicalColumns;
    const exclude = ["GeneID", "GeneName", "Chr", "GeneType", "ProbeID", "entrez_id", "gene_symbol", "gene_biotype", ...(d.geneInfoCols || [])];
    const cleanCols = (d.columns || []).filter((c: string) => !exclude.includes(c) && !c.startsWith("Sample_"));
    return cleanCols.length > 0 ? cleanCols : ["SampleID", "Group", "Batch", "Age", "Gender"];
  };

  useEffect(() => {
    if (ds?.batchDone && !state.dpSubmittedBatchConfig) {
      dispatch({ type: "DP_SUBMIT_BATCH" });
    }
  }, [ds?.batchDone, state.dpSubmittedBatchConfig, dispatch]);

  useEffect(() => {
    const initial: Record<string, string> = {};
    datasets.forEach(d => {
      const attrs = getClinicalAttributes(d);
      const batchAttr = attrs.find(a => a.toLowerCase().includes("batch")) || attrs[0] || "";
      initial[d.id] = batchAttr;
    });

    if (cfg.batchVariable) {
      const selections: Record<string, string> = {};
      cfg.batchVariable.split(";").forEach(item => {
        const [id, col] = item.split(":");
        if (id && col) {
          selections[id] = col;
        }
      });

      let mismatch = false;
      datasets.forEach(d => {
        const attrs = getClinicalAttributes(d);
        if (!selections[d.id] || !attrs.includes(selections[d.id])) {
          selections[d.id] = initial[d.id] || "";
          mismatch = true;
        }
      });

      if (mismatch) {
        const serializedConfig = Object.entries(selections)
          .map(([id, col]) => `${id}:${col}`)
          .join(";");
        dispatch({ type: "DP_SET_BATCH", patch: { batchVariable: serializedConfig } });
        setBatchSelections(selections);
      } else {
        setBatchSelections(selections);
      }
    } else {
      setBatchSelections(initial);
      const serializedConfig = Object.entries(initial)
        .map(([id, col]) => `${id}:${col}`)
        .join(";");
      dispatch({ type: "DP_SET_BATCH", patch: { batchVariable: serializedConfig } });
    }
  }, [cfg.batchVariable, datasets, dispatch]);

  const getNextAndPrevSteps = () => {
    const activeSteps = computeDPSteps(datasets);
    const currentIdx = activeSteps.findIndex(s => s.id === "batch");
    const next = activeSteps[currentIdx + 1]?.id || "module-select";
    let prev = activeSteps[currentIdx - 1]?.id || "upload";
    if (prev === "upload" && multipleDs) prev = "all-datasets";
    return { next, prev };
  };

  const submittedCfg = state.dpSubmittedBatchConfig;

  const hasSettingsChanged = done && submittedCfg && (
    submittedCfg.method !== cfg.method ||
    submittedCfg.methodOthers !== cfg.methodOthers ||
    submittedCfg.batchVariable !== cfg.batchVariable
  );

  const completedModules: string[] = [];
  if (state.dpInlineDeDone) completedModules.push("DE Analysis");
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");

  const downstreamDone = completedModules.length > 0;

  const handleBatchSelectChange = (targetDatasetId: string, column: string) => {
    const updated = { ...batchSelections, [targetDatasetId]: column };
    setBatchSelections(updated);

    const serializedConfig = Object.entries(updated)
      .map(([id, col]) => `${id}:${col}`)
      .join(";");

    dispatch({ type: "DP_SET_BATCH", patch: { batchVariable: serializedConfig } });
  };

  const runBatch = async () => {
    setLoading(true);
    try {
      const results = await runBatchCorrectionAPI(datasets, cfg);
      setPcaPoints(results);
      pcaPlotsCache[cacheKey] = results;
      dispatch({ type: "DP_SUBMIT_BATCH" });
      setDone(true);
    } catch (err: any) {
      console.error("Batch correction error:", err);
      toast({
        title: "Batch Correction Error",
        description: err.message || "Failed to execute batch correction.",
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
    const targets = datasetId === "all" ? datasets : [ds];
    targets.forEach(t => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: t.id, patch: { batchDone: true } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const handleSkip = () => {
    datasets.forEach(t => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: t.id, patch: { batchDone: false } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const targetBatchDatasets = [
    ...(transcriptomicsDs.length > 1 ? transcriptomicsDs : []),
    ...(othersDs.length > 1 ? othersDs : [])
  ];
  const isSelectionIncomplete = targetBatchDatasets.some(d => !batchSelections[d.id]);

  const totalSamples = useMemo(() => {
    return datasets.reduce((acc, curr) => acc + (curr.nSamples || 0), 0);
  }, [datasets]);

  const sharedFeatures = useMemo(() => {
    if (datasets.length === 0) return 0;
    let sharedSet = new Set<string>();
    const firstDs = datasets[0];
    if (firstDs.parsedData) {
      firstDs.parsedData.forEach(row => {
        if (row && row[0]) sharedSet.add(row[0]);
      });
    } else {
      return firstDs.nFeatures || 0;
    }

    for (let i = 1; i < datasets.length; i++) {
      const d = datasets[i];
      const currentSet = new Set<string>();
      if (d.parsedData) {
        d.parsedData.forEach(row => {
          if (row && row[0]) currentSet.add(row[0]);
        });
        const nextShared = new Set<string>();
        sharedSet.forEach(f => {
          if (currentSet.has(f)) {
            nextShared.add(f);
          }
        });
        sharedSet = nextShared;
      }
    }
    return sharedSet.size > 0 ? sharedSet.size : Math.min(...datasets.map(d => d.nFeatures || 0));
  }, [datasets]);

  const renderPCACard = (title: string, beforeSrc?: string, afterSrc?: string, singleSrc?: string) => {
    return (
      <div style={{ border: "1px solid var(--border)", borderRadius: 8, padding: 16, background: "white", marginBottom: 20 }}>
        <div style={{ fontSize: 13, fontWeight: 700, marginBottom: 12, color: "var(--foreground)" }}>
          {title}
        </div>
        {done ? (
          <div style={{ display: "flex", gap: 20, flexWrap: "wrap" }}>
            <div style={{ flex: "1 1 280px", border: "1px solid var(--border)", borderRadius: 8, padding: 14, background: "white", display: "flex", flexDirection: "column" }}>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 10 }}>Before Correction</div>
              {beforeSrc ? (
                <img
                  src={`data:image/png;base64,${beforeSrc}`}
                  style={{ width: "100%", height: "auto", borderRadius: 8 }}
                  alt="Before Correction PCA"
                />
              ) : (
                <div style={{ padding: 40, color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>Some errors. Couldn't generating PCA plots</div>
              )}
            </div>
            <div style={{ flex: "1 1 280px", border: "1px solid var(--border)", borderRadius: 8, padding: 14, background: "white", display: "flex", flexDirection: "column" }}>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 10 }}>After Correction</div>
              {afterSrc ? (
                <img
                  src={`data:image/png;base64,${afterSrc}`}
                  style={{ width: "100%", height: "auto", borderRadius: 8 }}
                  alt="After Correction PCA"
                />
              ) : (
                <div style={{ padding: 40, color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>Some errors. Couldn't generating PCA plots</div>
              )}
            </div>
          </div>
        ) : (
          <div style={{ display: "flex", gap: 20, flexWrap: "wrap", justifyContent: "center" }}>
            <div style={{ flex: "1 1 300px", maxWidth: "500px", display: "flex", flexDirection: "column", alignItems: "center" }}>
              {singleSrc ? (
                <img
                  src={`data:image/png;base64,${singleSrc}`}
                  style={{ width: "100%", height: "auto", borderRadius: 8 }}
                  alt="PCA Plot"
                />
              ) : (
                <div style={{ padding: 40, color: "var(--muted-foreground)", fontSize: 13, textAlign: "center" }}>Generating PCA Plot...</div>
              )}
            </div>
          </div>
        )}
      </div>
    );
  };

  // Dataset configurations check for specific methods
  const hasRawCounts = datasets.some(d => d.dataType === "readcounts" && !d.isNormalized);
  const hasOthersOrNormalized = datasets.some(
    d => d.dataType === "microarray" || d.dataType === "others" || (d.dataType === "readcounts" && d.isNormalized)
  );

  const getActiveMethodLabel = () => {
    const parts: string[] = [];
    if (hasRawCounts) {
      parts.push(submittedCfg?.method === "combat_seq" ? "ComBat-seq" : "removeBatchEffect (limma)");
    }
    if (hasOthersOrNormalized) {
      parts.push(submittedCfg?.methodOthers === "combat" ? "ComBat (parametric)" : "removeBatchEffect (limma)");
    }
    return parts.join(" & ") || "Batch Correction";
  };

  return (
    <>
      {showSkipModal && <SkipModal stepName="Batch Effects Removal" stepId="batch" datasetIds={datasets.map(d => d.id)} onConfirm={handleSkip} onCancel={() => setShowSkipModal(false)} />}
      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showRedoWarnModal && (
        <ChangeWarnModal
          onConfirm={() => {
            setShowRedoWarnModal(false);
            delete pcaPlotsCache[cacheKey];
            redoStepAPI('batch', datasets.map(d => d.id)).catch(e => console.error(e));
            if (downstreamDone) {
              setShowDiscardModal(true);
            } else {
              runBatch();
            }
          }}
          onCancel={() => setShowRedoWarnModal(false)}
        />
      )}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="batch"
          datasetIds={datasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            delete pcaPlotsCache[cacheKey];
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: datasetId,
              fromStep: "batch"
            } as any);
            await runBatch();
          }}
        />
      )}
      {loading && (
        <Spinner
          label="Running batch effect removal…"
          sublabel={`Methods: ${hasRawCounts ? (cfg.method === "combat_seq" ? "ComBat-seq" : "removeBatchEffect") : ""} ${hasOthersOrNormalized ? (cfg.methodOthers === "combat" ? "ComBat" : "removeBatchEffect") : ""}`}
        />
      )}
      {loadingPCA && (
        <Spinner
          label="Creating PCA plots..."
          sublabel="Please wait while the server transfers the plots"
        />
      )}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>Batch effects removal</div>
        <div className="card-sub">
            Adjusts the data to reduce differences between batches so they can be compared more accurately.
        </div>
        <hr className="card-divider" />

        <div className="banner info" style={{ marginBottom: 14 }}>
          🔬 Detected <strong>{datasets.length} datasets</strong>:{" "}
          {datasets.map((d) => <span key={d.id} style={{ fontWeight: 600, marginRight: 6 }}>{d.name || d.id}</span>)}
        </div>

        <div className="chips-row" style={{ marginBottom: 14 }}>
          <div className="stat-chip">
            <div className="stat-chip-val">{datasets.length}</div>
            <div className="stat-chip-lbl">Datasets</div>
          </div>
          <div className="stat-chip">
            <div className="stat-chip-val">{totalSamples.toLocaleString()}</div>
            <div className="stat-chip-lbl">Total Samples</div>
          </div>
          <div className="stat-chip">
            <div className="stat-chip-val">{sharedFeatures.toLocaleString()}</div>
            <div className="stat-chip-lbl">Shared Features</div>
          </div>
        </div>

        {/* Pre-Correction PCA Diagnostics Panel */}
        {!done && (
          <div style={{ marginBottom: 24 }}>
            <div style={{ fontSize: 13, fontWeight: 700, color: "var(--foreground)", marginBottom: 14 }}>
              Pre-Correction PCA Diagnostics
            </div>
            {datasetId === "all" ? (
              datasets.map(d => (
                <div key={d.id}>
                  {renderPCACard(`Dataset PCA: ${d.name}`, undefined, undefined, pcaPoints?.datasets?.[d.id]?.plot_batch)}
                </div>
              ))
            ) : (
              ds && (
                <div>
                  {renderPCACard(`Dataset PCA: ${ds.name}`, undefined, undefined, pcaPoints?.datasets?.[ds.id]?.plot_batch)}
                </div>
              )
            )}
            {multipleDs && datasetId === "all" && allSameDataType && renderPCACard("Combined Datasets PCA", undefined, undefined, pcaPoints?.combined?.plot_batch || pcaPoints?.plot_batch)}
          </div>
        )}

        {/* Method selection (split into appropriate divs) */}
        <div style={{ marginBottom: 16 }}>
          <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 8 }}>Correction Method Selection</label>

          {hasRawCounts && (
            <div style={{ marginBottom: 16, padding: 14, border: "1px solid var(--border)", borderRadius: 8, background: "#f8fafc" }}>
              <div style={{ fontSize: 12, fontWeight: 700, marginBottom: 8 }}>
                Batch correction method for unnormalized read counts
              </div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {BATCH_METHODS_RAW.map(m => (
                  <label key={m.value} style={{
                    display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 14px", borderRadius: 8, cursor: "pointer",
                    border: `1px solid ${cfg.method === m.value ? "var(--primary)" : "var(--border)"}`,
                    background: cfg.method === m.value ? "var(--selected-bg)" : "white",
                  }}>
                    <input
                      type="radio"
                      name="batchMethodRaw"
                      value={m.value}
                      checked={cfg.method === m.value}
                      onChange={() => dispatch({ type: "DP_SET_BATCH", patch: { method: m.value } })}
                      style={{ accentColor: "var(--primary)", marginTop: 2 }}
                      data-testid={`radio-batch-raw-${m.value}`}
                    />
                    <div>
                      <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                      <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
                    </div>
                  </label>
                ))}
              </div>
            </div>
          )}

          {hasOthersOrNormalized && (
            <div style={{ padding: 14, border: "1px solid var(--border)", borderRadius: 8, background: "#f8fafc" }}>
              <div style={{ fontSize: 12, fontWeight: 700, marginBottom: 8 }}>
                Batch correction methods for normalized data (eg. normalized counts, microarray, others)
              </div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {BATCH_METHODS_OTHERS.map(m => (
                  <label key={m.value} style={{
                    display: "flex", alignItems: "flex-start", gap: 10, padding: "10px 14px", borderRadius: 8, cursor: "pointer",
                    border: `1px solid ${cfg.methodOthers === m.value ? "var(--primary)" : "var(--border)"}`,
                    background: cfg.methodOthers === m.value ? "var(--selected-bg)" : "white",
                  }}>
                    <input
                      type="radio"
                      name="batchMethodOthers"
                      value={m.value}
                      checked={cfg.methodOthers === m.value}
                      onChange={() => dispatch({ type: "DP_SET_BATCH", patch: { methodOthers: m.value } })}
                      style={{ accentColor: "var(--primary)", marginTop: 2 }}
                      data-testid={`radio-batch-others-${m.value}`}
                    />
                    <div>
                      <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
                      <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
                    </div>
                  </label>
                ))}
              </div>
            </div>
          )}
        </div>
      </div>

      {/* Done state results */}
      {done && (
        <div className="card" style={{ opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Batch Correction Results {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated - settings changed)</span>}
          </div>

          <div className="banner success">
            ✅ Batch correction complete using <strong>{getActiveMethodLabel()}</strong>. Datasets merged: <strong>{datasets.length}</strong> → <strong>1 combined matrix</strong>.
          </div>

          <div className="chips-row" style={{ marginTop: 10 }}>
            <div className="stat-chip"><div className="stat-chip-val">{datasets.length}</div><div className="stat-chip-lbl">Datasets Merged</div></div>
            <div className="stat-chip"><div className="stat-chip-val">{datasets.reduce((a, d) => a + (d.nSamples || 0), 0)}</div><div className="stat-chip-lbl">Total Samples</div></div>
            <div className="stat-chip"><div className="stat-chip-val">{(ds?.nFeatures || 0).toLocaleString()}</div><div className="stat-chip-lbl">Features</div></div>
          </div>

          {/* Before vs After PCA Plots */}
          <div style={{ marginTop: 24, borderTop: "1px solid var(--border)", paddingTop: 16 }}>
            <div style={{ fontSize: 13, fontWeight: 700, color: "var(--foreground)", marginBottom: 14 }}>
              Before vs After Correction PCA Diagnostics
            </div>
            {datasetId === "all" ? (
              datasets.map(d => (
                <div key={d.id}>
                  {renderPCACard(`Dataset PCA: ${d.name}`, pcaPoints?.datasets?.[d.id]?.before_batch, pcaPoints?.datasets?.[d.id]?.after_batch)}
                </div>
              ))
            ) : (
              ds && (
                <div>
                  {renderPCACard(`Dataset PCA: ${ds.name}`, pcaPoints?.datasets?.[ds.id]?.before_batch, pcaPoints?.datasets?.[ds.id]?.after_batch)}
                </div>
              )
            )}
            {multipleDs && datasetId === "all" && allSameDataType && renderPCACard("Combined Datasets PCA", pcaPoints?.combined?.before_batch || pcaPoints?.before_batch, pcaPoints?.combined?.after_batch || pcaPoints?.after_batch)}
          </div>
        </div>
      )}

      {/* Action panel footer */}
      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => {
            if (onBack) {
              onBack();
            } else {
              dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: getNextAndPrevSteps().prev } as never);
            }
          }}>← Back</button>
          <button className="btn btn-default" onClick={() => setShowSkipModal(true)} data-testid="btn-skip-batch" style={{ color: "var(--muted-foreground)" }}>Skip</button>
        </div>

        <div style={{ display: "flex", gap: 8 }}>
          {hasSettingsChanged && (
            <button className="btn btn-default" style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)" }} onClick={() => {
              setShowRedoWarnModal(true);
            }} data-testid="btn-redo-batch">
              🔄 Redo Batch Correction
            </button>
          )}

          {!done ? (
            <button className="btn btn-primary" onClick={runBatch} disabled={isSelectionIncomplete} data-testid="btn-run-batch">
              Apply Batch Correction
            </button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} data-testid="btn-next-batch">
              Continue to Analysis Module →
            </button>
          )}
        </div>
      </div>
    </>
  );
}