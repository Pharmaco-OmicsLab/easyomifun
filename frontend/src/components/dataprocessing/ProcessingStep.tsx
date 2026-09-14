import { useState, useEffect } from "react";
import { useAppStore } from "../../store/appStore";
import { computeDPSteps } from "../../dataObject";
import Spinner from "../Spinner";
import SkipModal from "../shared/SkipModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import type { ProcessingConfig, FilterMethod } from "../../dataObject";
import { processDatasetAPI, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

interface Props {
  datasetId: string;
  mode?: "dp" | "de";
}

// Filter method options per dataType group
const ALL_FILTER_METHODS: { value: FilterMethod; label: string; desc: string }[] = [
  { value: "cpm", label: "CPM Threshold", desc: "Keeps genes with expression above the CPM threshold in at least a number of samples. Reduce noise, but may exclude genes expressed only in a small subset of samples." },
  { value: "min_count", label: "Raw Count Threshold", desc: "Keeps genes with total read count of all samples above threshold. Removes low-abundance genes to reduce technical noise." },
  { value: "variance", label: "Variance Filter", desc: "Keeps genes with expression variance above threshold. Improve signal for discovery, but removes genes with consistent expression." },
];

const RESTRICTED_FILTER_METHODS: { value: FilterMethod; label: string; desc: string }[] = [
  { value: "variance", label: "Variance Filter", desc: "Keeps genes with expression variance above threshold. Improve signal for discovery, but removes genes with consistent expression." },
];

const MISSING_METHODS = [
  { value: "knn", label: "KNN Imputation", hasParams: true, paramLabel: "k neighbours" },
];

/** Sub-card for a single dataType group's filtering settings */
function FilteringCard({ title, cfg, localFilterMethod, onFilterMethodChange, localVarianceThreshold,
  onVarianceThresholdChange, onCpmThresholdChange, restrictedMethods, dataTestIdPrefix,
  maxSamples, onMinSamplesChange, onCountThresholdChange,
}: {
  title: string; cfg: ProcessingConfig; localFilterMethod: FilterMethod | "" | null; onFilterMethodChange: (m: FilterMethod | "" | null) => void; 
  localVarianceThreshold: number | "" | null; onVarianceThresholdChange: (v: number | "" | null) => void; 
  onCpmThresholdChange: (v: number | "" | null) => void; restrictedMethods: boolean; dataTestIdPrefix: string;
  maxSamples: number; onMinSamplesChange: (v: number | "" | null) => void; onCountThresholdChange: (v: number | "" | null) => void;
}) {
  const methods = restrictedMethods ? RESTRICTED_FILTER_METHODS : ALL_FILTER_METHODS;

  const isMethodActive = (methodVal: string) => {
    if (restrictedMethods) {
      return localFilterMethod === methodVal;
    }
    const activeMethods = (localFilterMethod || "").split(",").map(m => m.trim());
    return activeMethods.includes(methodVal);
  };

  const toggleMethod = (methodVal: string) => {
    if (restrictedMethods) {
      onFilterMethodChange(methodVal as FilterMethod);
      return;
    }
    const activeMethods = (localFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
    let newMethods: string[];
    if (activeMethods.includes(methodVal)) {
      newMethods = activeMethods.filter(m => m !== methodVal);
    } else {
      newMethods = [...activeMethods, methodVal];
    }
    if (newMethods.length === 0) {
      onFilterMethodChange("");
    } else {
      onFilterMethodChange(newMethods.join(",") as FilterMethod);
    }
  };

  return (
    <div style={{ border: "1px solid var(--border)", borderRadius: 10, padding: "14px 16px", background: "white", marginBottom: 12 }}>
      <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", marginBottom: 10 }}>{title}</div>
      <div style={{ display: "flex", flexDirection: "column", gap: 8, marginBottom: 12 }}>
        {methods.map(m => (
          <label key={m.value} style={{
            display: "flex", alignItems: "flex-start", gap: 10, padding: "9px 12px", borderRadius: 8, cursor: "pointer",
            border: `1px solid ${isMethodActive(m.value) ? "var(--primary)" : "var(--border)"}`,
            background: isMethodActive(m.value) ? "var(--selected-bg)" : "hsl(220 20% 98%)", transition: "all .12s",
          }}>
            <input
              type={restrictedMethods ? "radio" : "checkbox"}
              name={`filterMethod-${dataTestIdPrefix}`}
              value={m.value}
              checked={isMethodActive(m.value)}
              onChange={() => toggleMethod(m.value)}
              style={{ accentColor: "var(--primary)", marginTop: 2 }}
              data-testid={`${restrictedMethods ? "radio" : "checkbox"}-filter-${dataTestIdPrefix}-${m.value}`}
            />
            <div>
              <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
              <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
            </div>
          </label>
        ))}
      </div>

      {/* CPM params (readcounts only) */}
      {!restrictedMethods && isMethodActive("cpm") && (
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 4 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>CPM Threshold</label>
            <input type="number" value={cfg.cpmThreshold ?? ""} min={0} step={0.5}
              onChange={e => {
                const valStr = e.target.value;
                if (valStr === "") {
                  onCpmThresholdChange(null);
                } else {
                  const val = Number(valStr);
                  onCpmThresholdChange(val < 0 ? 0 : val);
                }
              }}
              style={{ width: 120, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
              data-testid={`input-cpm-threshold-${dataTestIdPrefix}`} />
          </div>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Min. Samples</label>
            <input type="number" value={cfg.minSamples ?? ""} min={1} max={maxSamples}
              onChange={e => {
                const valStr = e.target.value;
                if (valStr === "") {
                  onMinSamplesChange(null);
                } else {
                  const val = Number(valStr);
                  if (val < 1) {
                    onMinSamplesChange(1);
                  } else if (val > maxSamples) {
                    onMinSamplesChange(maxSamples);
                  } else {
                    onMinSamplesChange(val);
                  }
                }
              }}
              style={{ width: 120, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>
        </div>
      )}

      {/* Count params */}
      {!restrictedMethods && isMethodActive("min_count") && (
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 4 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Sum Count Threshold</label>
            <input type="number" value={cfg.countThreshold ?? ""} min={0} step={1}
              onChange={e => {
                const valStr = e.target.value;
                if (valStr === "") {
                  onCountThresholdChange(null);
                } else {
                  const val = Number(valStr);
                  onCountThresholdChange(val < 0 ? 0 : val);
                }
              }}
              style={{ width: 160, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>
        </div>
      )}

      {/* Variance threshold */}
      {isMethodActive("variance") && (
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 4 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Variance Threshold (%)</label>
            <input type="number" value={localVarianceThreshold ?? ""} min={0} max={100} step={1}
              onChange={e => {
                const valStr = e.target.value;
                if (valStr === "") {
                  onVarianceThresholdChange(null);
                } else {
                  const val = Number(valStr);
                  if (val < 0) {
                    onVarianceThresholdChange(0);
                  } else if (val > 100) {
                    onVarianceThresholdChange(100);
                  } else {
                    onVarianceThresholdChange(val);
                  }
                }
              }}
              style={{ width: 160, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
              data-testid={`input-variance-threshold-${dataTestIdPrefix}`} />
          </div>
        </div>
      )}
    </div>
  );
}

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

interface ProcessingResultCardProps {
  targetDs: any; // Replace with your explicit Dataset type if applicable
}

function ProcessingResultCard({ targetDs }: ProcessingResultCardProps) {
  const total = targetDs.processingInputFeatures || targetDs.nFeatures || 0;
  const retained = targetDs.processingRetainedFeatures || 0;
  const removed = targetDs.processingRemovedFeatures || 0;
  const percentRetained = total > 0 ? ((retained / total) * 100).toFixed(1) : "0.0";
  const slices = [
    { label: "Retained", value: retained, color: "#3b82f6" },
    { label: "Removed", value: removed, color: "#94a3b8" }
  ];

  return (
    <div className="result-ds-card" style={{ margin: 0 }}>
      <div className="result-ds-card-header" style={{ marginBottom: 12 }}>
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: targetDs.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>
          {targetDs.name} Filtering Results
        </div>
      </div>
      
      <div className="banner success" style={{ marginBottom: 12 }}>
        ✅ Filtering complete. <strong>{retained.toLocaleString()} features retained</strong> ({percentRetained}%), {removed.toLocaleString()} removed.
      </div>
      
      <div className="chips-row">
        <div className="stat-chip">
          <div className="stat-chip-val">{total.toLocaleString()}</div>
          <div className="stat-chip-lbl">Input Features</div>
        </div>
        <div className="stat-chip">
          <div className="stat-chip-val">{retained.toLocaleString()}</div>
          <div className="stat-chip-lbl">Retained</div>
        </div>
        <div className="stat-chip">
          <div className="stat-chip-val">{removed.toLocaleString()}</div>
          <div className="stat-chip-lbl">Removed</div>
        </div>
      </div>
      <InteractivePieChart slices={slices} />
    </div>
  );
}

export default function ProcessingStep({ datasetId, mode = "dp" }: Props) {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const cfg = state.dpProcessingConfig;

  const allDs = mode === "dp" ? state.dpDatasets : state.deDatasets;
  const ds = allDs.find(d => d.id === datasetId);
  const multipleDs = allDs.length > 1;

  // Target all datasets (or all in context)
  const targetDatasets = datasetId === "all" ? allDs : (ds ? [ds] : []);

  const isRawReadCounts = (d: any) => d.dataType === "readcounts" && !d.isNormalized;

  // Check if all target readcounts datasets are normalized
  const rcDatasets = targetDatasets.filter(d => d.dataType === "readcounts");
  const allRcNormalized = rcDatasets.length > 0 && rcDatasets.every(d => d.isNormalized);

  const [loading, setLoading] = useState(false);
  const [activeTabId, setActiveTabId] = useState<string>("");
  const [showSkipModal, setShowSkipModal] = useState(false);

  useEffect(() => {
    if (targetDatasets.length > 0) {
      if (!activeTabId || !targetDatasets.some(d => d.id === activeTabId)) {
        setActiveTabId(targetDatasets[0].id);
      }
    }
  }, [targetDatasets, activeTabId]);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  // Per-type local filter method state (since we may have multiple cards)
  const [rcFilterMethod, setRcFilterMethod] = useState<FilterMethod | "" | null>(
    cfg.rcFilterMethod || ""
  );
  const [normRcFilterMethod, setNormRcFilterMethod] = useState<FilterMethod | "" | null>(
    cfg.normRcFilterMethod || "variance"
  );
  const [maFilterMethod, setMaFilterMethod] = useState<FilterMethod | "" | null>(cfg.maFilterMethod || "variance");
  const [protFilterMethod, setProtFilterMethod] = useState<FilterMethod | "" | null>(cfg.protFilterMethod || "variance");
  const [othFilterMethod, setOthFilterMethod] = useState<FilterMethod | "" | null>(cfg.othFilterMethod || "variance");

  const maxFeatures = targetDatasets.length > 0 ? Math.min(...targetDatasets.map(d => d.nFeatures || 100)) : 10;
  const maxSamples = targetDatasets.length > 0 ? Math.min(...targetDatasets.map(d => d.nSamples || 1)) : 1;

  const [rcVariance, setRcVariance] = useState<number | "" | null>(cfg.rcVariance ?? 10);
  const [normRcVariance, setNormRcVariance] = useState<number | "" | null>(cfg.normRcVariance ?? 10);
  const [maVariance, setMaVariance] = useState<number | "" | null>(cfg.maVariance ?? 10);
  const [protVariance, setProtVariance] = useState<number | "" | null>(cfg.protVariance ?? 10);
  const [othVariance, setOthVariance] = useState<number | "" | null>(cfg.othVariance ?? 10);

  useEffect(() => {
    const patch: Partial<ProcessingConfig> = {};
    let hasChanges = false;

    if (cfg.naRemovePercent === null || cfg.naRemovePercent === undefined) {
      patch.naRemovePercent = 50;
      hasChanges = true;
    }
    if (cfg.knnK === null || cfg.knnK === undefined) {
      patch.knnK = Math.min(10, maxFeatures);
      hasChanges = true;
    }
    if (cfg.cpmThreshold === null || cfg.cpmThreshold === undefined) {
      patch.cpmThreshold = 1;
      hasChanges = true;
    }
    if (cfg.minSamples === null || cfg.minSamples === undefined) {
      patch.minSamples = Math.min(2, maxSamples);
      hasChanges = true;
    }
    if (cfg.countThreshold === null || cfg.countThreshold === undefined) {
      patch.countThreshold = 20;
      hasChanges = true;
    }
    if (cfg.rcVariance === null || cfg.rcVariance === undefined) {
      patch.rcVariance = 10;
      hasChanges = true;
    }
    if (cfg.normRcVariance === null || cfg.normRcVariance === undefined) {
      patch.normRcVariance = 10;
      hasChanges = true;
    }
    if (cfg.maVariance === null || cfg.maVariance === undefined) {
      patch.maVariance = 10;
      hasChanges = true;
    }
    if (cfg.protVariance === null || cfg.protVariance === undefined) {
      patch.protVariance = 10;
      hasChanges = true;
    }
    if (cfg.othVariance === null || cfg.othVariance === undefined) {
      patch.othVariance = 10;
      hasChanges = true;
    }

    if (hasChanges) {
      dispatch({ type: "DP_SET_PROCESSING", patch });
    }
  }, [
    cfg.naRemovePercent,
    cfg.knnK,
    cfg.cpmThreshold,
    cfg.minSamples,
    cfg.countThreshold,
    cfg.rcVariance,
    cfg.normRcVariance,
    cfg.maVariance,
    cfg.protVariance,
    cfg.othVariance,
    maxFeatures,
    maxSamples,
    dispatch
  ]);

  useEffect(() => {
    if (cfg.rcFilterMethod !== undefined) setRcFilterMethod(cfg.rcFilterMethod || "");
    if (cfg.normRcFilterMethod !== undefined) setNormRcFilterMethod(cfg.normRcFilterMethod || "variance");
    if (cfg.maFilterMethod !== undefined) setMaFilterMethod(cfg.maFilterMethod || "variance");
    if (cfg.protFilterMethod !== undefined) setProtFilterMethod(cfg.protFilterMethod || "variance");
    if (cfg.othFilterMethod !== undefined) setOthFilterMethod(cfg.othFilterMethod || "variance");

    setRcVariance(cfg.rcVariance ?? 10);
    setNormRcVariance(cfg.normRcVariance ?? 10);
    setMaVariance(cfg.maVariance ?? 10);
    setProtVariance(cfg.protVariance ?? 10);
    setOthVariance(cfg.othVariance ?? 10);
  }, [
    cfg.rcFilterMethod,
    cfg.normRcFilterMethod,
    cfg.maFilterMethod,
    cfg.protFilterMethod,
    cfg.othFilterMethod,
    cfg.rcVariance,
    cfg.normRcVariance,
    cfg.maVariance,
    cfg.protVariance,
    cfg.othVariance
  ]);

  // Group by dataType and normalization status
  const rawRcDatasets = targetDatasets.filter(d => isRawReadCounts(d));
  const normRcDatasets = targetDatasets.filter(d => d.dataType === "readcounts" && d.isNormalized);
  const maDatasets = targetDatasets.filter(d => d.dataType === "microarray");
  const protDatasets = targetDatasets.filter(d => d.dataType === "proteomics");
  const othDatasets = targetDatasets.filter(d => d.dataType === "others");

  const hasRawRc = rawRcDatasets.length > 0;
  const hasNormRc = normRcDatasets.length > 0;
  const hasMa = maDatasets.length > 0;
  const hasProt = protDatasets.length > 0;
  const hasOth = othDatasets.length > 0;

  const isMixedTypes = [rawRcDatasets, normRcDatasets, maDatasets, protDatasets, othDatasets].filter(g => g.length > 0).length > 1;

  const getSingleTitle = (type?: string) => {
    switch (type) {
      case "readcounts":
        return `Filtering for Readcounts ${multipleDs ? "Datasets" : "Dataset"}`;
      case "microarray":
        return `Filtering for Microarray ${multipleDs ? "Datasets" : "Dataset"}`;
      case "proteomics":
        return `Filtering for Proteomics ${multipleDs ? "Datasets" : "Dataset"}`;
      default:
        return `Filtering for Others ${multipleDs ? "Datasets" : "Dataset"}`;
    }
  };

  const done = targetDatasets.length > 0 && targetDatasets.every(d => d.processingDone);

  // Missing values
  const singleDs = ds ?? allDs[0];
  const headers = singleDs?.columns || [];
  const geneInfoCols = singleDs?.geneInfoCols || [];
  const geneIdCol = singleDs?.geneIdCol || "";
  const dataColIndices: number[] = [];
  headers.forEach((col, idx) => {
    const isAnnotationCol = [
      "gene_symbol", "entrez_id", "gene_biotype", "gene_id", "probe_id", "id"
    ].includes(col.toLowerCase()) || geneInfoCols.includes(col) || col === geneIdCol;
    if (idx !== 0 && !isAnnotationCol) dataColIndices.push(idx);
  });
  let missingValueCount = singleDs?.processingMissingValuesCount || 0;
  const hasMissingValues = targetDatasets.some(d => d.hasNA) || missingValueCount > 0;

  const appliedCfg = state.dpSubmittedProcessingConfig;
  
  const checkCfgChanged = () => {
    if (!done || !appliedCfg) return false;
    
    const normStr = (val: any) => (val === null || val === undefined) ? "" : String(val).trim();
    const normNum = (val: any) => (val === null || val === undefined || val === "") ? null : Number(val);
    
    if (hasMissingValues) {
      if (normStr(appliedCfg.missingMethod) !== normStr(cfg.missingMethod) ||
          normNum(appliedCfg.knnK) !== normNum(cfg.knnK) ||
          normNum(appliedCfg.naRemovePercent) !== normNum(cfg.naRemovePercent)) {
        return true;
      }
    }
    
    if (hasRawRc) {
      if (normStr(appliedCfg.rcFilterMethod) !== normStr(rcFilterMethod) ||
          normNum(appliedCfg.rcVariance) !== normNum(rcVariance) ||
          normNum(appliedCfg.cpmThreshold) !== normNum(cfg.cpmThreshold) ||
          normNum(appliedCfg.minSamples) !== normNum(cfg.minSamples) ||
          normNum(appliedCfg.countThreshold) !== normNum(cfg.countThreshold)) {
        return true;
      }
    }
    if (hasNormRc) {
      if (normStr(appliedCfg.normRcFilterMethod) !== normStr(normRcFilterMethod) ||
          normNum(appliedCfg.normRcVariance) !== normNum(normRcVariance)) {
        return true;
      }
    }
    if (hasMa) {
      if (normStr(appliedCfg.maFilterMethod) !== normStr(maFilterMethod) ||
          normNum(appliedCfg.maVariance) !== normNum(maVariance)) {
        return true;
      }
    }
    if (hasProt) {
      if (normStr(appliedCfg.protFilterMethod) !== normStr(protFilterMethod) ||
          normNum(appliedCfg.protVariance) !== normNum(protVariance)) {
        return true;
      }
    }
    if (hasOth) {
      if (normStr(appliedCfg.othFilterMethod) !== normStr(othFilterMethod) ||
          normNum(appliedCfg.othVariance) !== normNum(othVariance)) {
        return true;
      }
    }
    
    return false;
  };

  const cfgChanged = checkCfgChanged();

  const getValidationError = (): string | null => {
    // 1. Check if at least one filtering method is selected for each active card
    if (hasRawRc) {
      const activeMethods = (rcFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Raw Readcounts.";
      }
    }
    if (hasNormRc) {
      if (!normRcFilterMethod || normRcFilterMethod.trim() === "") {
        return "Please select a filtering method for Normalized Readcounts.";
      }
    }
    if (hasMa) {
      if (!maFilterMethod || maFilterMethod.trim() === "") {
        return "Please select a filtering method for Microarray.";
      }
    }
    if (hasProt) {
      if (!protFilterMethod || protFilterMethod.trim() === "") {
        return "Please select a filtering method for Proteomics.";
      }
    }
    if (hasOth) {
      if (!othFilterMethod || othFilterMethod.trim() === "") {
        return "Please select a filtering method for Other data types.";
      }
    }

    // 2. Missing values validation (only if there are missing values in target datasets)
    if (hasMissingValues) {
      if (cfg.naRemovePercent === null || cfg.naRemovePercent === undefined || cfg.naRemovePercent === "") {
        return "Please specify the Missing Value Threshold (%).";
      }
      const naPercent = Number(cfg.naRemovePercent);
      if (isNaN(naPercent) || naPercent < 0 || naPercent > 100) {
        return "Missing Value Threshold (%) must be between 0 and 100.";
      }

      if (cfg.missingMethod !== "knn") {
        return "Please select KNN Imputation for handling missing values.";
      }

      if (cfg.knnK === null || cfg.knnK === undefined || cfg.knnK === "") {
        return "Please specify the number of neighbors (k) for KNN Imputation.";
      }
      const k = Number(cfg.knnK);
      if (isNaN(k) || k < 1 || k > maxFeatures) {
        return `Number of neighbors (k) must be between 1 and ${maxFeatures}.`;
      }
    }

    return null;
  };

  const validationError = getValidationError();

  const completedModules: string[] = [];
  targetDatasets.forEach(d => {
    if (d.normalizationDone && !completedModules.includes("Normalization")) completedModules.push("Normalization");
    if (d.batchDone && !completedModules.includes("Batch Correction")) completedModules.push("Batch Correction");
    if ((state.dpInlineDeDone || (state.dpDeResults[d.id] && state.dpDeResults[d.id].length > 0)) && !completedModules.includes("DE Analysis")) completedModules.push("DE Analysis");
  });
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");
  const downstreamDone = completedModules.length > 0;

  useEffect(() => {
    if (done && !state.dpSubmittedProcessingConfig) {
      dispatch({ type: "DP_SUBMIT_PROCESSING" });
    }
  }, [done, state.dpSubmittedProcessingConfig, dispatch]);

  // Navigation
  const getNextAndPrevSteps = () => {
    const activeSteps = computeDPSteps(allDs);
    const currentStepId = "processing";
    const currentIdx = activeSteps.findIndex(s => s.id === currentStepId);
    const next = activeSteps[currentIdx + 1]?.id || "module-select";
    let prev = activeSteps[currentIdx - 1]?.id || "upload";
    if (prev === "upload" && multipleDs) prev = "all-datasets";
    return { next, prev };
  };

  const runProcessing = async () => {
    if (validationError) {
      toast({
        title: "Validation Error",
        description: validationError,
        variant: "destructive"
      });
      return;
    }
    setLoading(true);
    try {
      let repFilterMethod: FilterMethod | "" | null = "";
      let repVarianceThreshold: number | "" | null = null;
      if (hasRawRc) {
        repFilterMethod = rcFilterMethod;
        repVarianceThreshold = rcVariance;
      } else if (hasNormRc) {
        repFilterMethod = normRcFilterMethod;
        repVarianceThreshold = normRcVariance;
      } else if (hasMa) {
        repFilterMethod = maFilterMethod;
        repVarianceThreshold = maVariance;
      } else if (hasProt) {
        repFilterMethod = protFilterMethod;
        repVarianceThreshold = protVariance;
      } else if (hasOth) {
        repFilterMethod = othFilterMethod;
        repVarianceThreshold = othVariance;
      }

      const activeConfig = {
        ...cfg,
        filterMethod: repFilterMethod,
        varianceThreshold: repVarianceThreshold,
        rcFilterMethod,
        normRcFilterMethod,
        maFilterMethod,
        protFilterMethod,
        othFilterMethod,
        rcVariance,
        normRcVariance,
        maVariance,
        protVariance,
        othVariance
      };
      
      const results = await processDatasetAPI(targetDatasets, activeConfig);

      dispatch({
        type: "DP_SET_PROCESSING",
        patch: {
          filterMethod: repFilterMethod,
          varianceThreshold: repVarianceThreshold,
          rcFilterMethod,
          normRcFilterMethod,
          maFilterMethod,
          protFilterMethod,
          othFilterMethod,
          rcVariance,
          normRcVariance,
          maVariance,
          protVariance,
          othVariance
        }
      });
      dispatch({ type: "DP_SUBMIT_PROCESSING" });

      if (Array.isArray(results)) {
        results.forEach(res => {
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: res.datasetId,
            patch: {
              processingDone: true,
              processingInputFeatures: res.inputFeatures,
              processingRemovedFeatures: res.removedFeatures,
              processingRetainedFeatures: res.retainedFeatures,
              processingMissingValuesCount: res.missingValuesCount,
              parsedData: res.parsedData || undefined,
              columns: res.columns || undefined,
              sampleIds: res.sampleIds || undefined,
              nSamples: res.nSamples !== undefined ? res.nSamples : undefined,
              nFeatures: res.retainedFeatures,
            }
          } as never);
        });
      }
    } catch (err: any) {
      console.error("Processing error:", err);
      toast({
        title: "Processing Error",
        description: err.message || "An error occurred during dataset processing.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const handleContinue = () => {
    if (cfgChanged) { setShowWarnModal(true); return; }
    proceedToNextStep();
  };

  const proceedToNextStep = () => {
    setShowWarnModal(false);
    targetDatasets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { processingDone: true } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  const handleSkip = () => {
    targetDatasets.forEach(targetDs => {
      dispatch({ type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET", id: targetDs.id, patch: { processingDone: false } } as never);
    });
    const { next } = getNextAndPrevSteps();
    dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: next } as never);
  };

  return (
    <>
      {showSkipModal && <SkipModal stepName="Filtering" stepId="processing" datasetIds={targetDatasets.map(d => d.id)} onConfirm={handleSkip} onCancel={() => setShowSkipModal(false)} />}
      {showWarnModal && <ChangeWarnModal onConfirm={proceedToNextStep} onCancel={() => setShowWarnModal(false)} />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="processing"
          datasetIds={targetDatasets.map(d => d.id)}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({ type: "RESET_DOWNSTREAM_STEPS", datasetId, fromStep: "processing" } as any);
            await runProcessing();
          }}
        />
      )}
      {loading && <Spinner label="Applying filtering and missing value handling…" />}

      {/* Remove features with too many missing values Card — only show if any dataset has NA */}
      {hasMissingValues && (
        <div className={`card ${(!done || cfgChanged) ? "banner-danger" : ""}`}>
          <div className="card-title" style={{ marginBottom: 4 }}>Remove Features with Too Many Missing Values</div>
          <div className="card-sub" style={{ color: (!done || cfgChanged) ? "#991b1b" : "var(--muted-foreground)" }}>
            {(!done || cfgChanged)
              ? "Filter out features that exceed a threshold of missing values across samples."
              : "Features with excessive missing values have been removed."}
          </div>
          <hr className="card-divider" style={{ borderColor: (!done || cfgChanged) ? "#fca5a5" : "var(--border)" }} />
          <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
            <div style={{ display: "flex", flexDirection: "column", gap: 6 }}>
              <label style={{ fontSize: 13, fontWeight: 500, color: (!done || cfgChanged) ? "#991b1b" : "var(--foreground)" }}>
                Missing Value Threshold (% of samples)
              </label>
              <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                <input
                  type="number"
                  value={cfg.naRemovePercent ?? ""}
                  min={0}
                  max={100}
                  step={1}
                  onChange={e => {
                    const valStr = e.target.value;
                    if (valStr === "") {
                      dispatch({ type: "DP_SET_PROCESSING", patch: { naRemovePercent: "" } });
                    } else {
                      const val = Number(valStr);
                      if (val < 0) {
                        dispatch({ type: "DP_SET_PROCESSING", patch: { naRemovePercent: 0 } });
                      } else if (val > 100) {
                        dispatch({ type: "DP_SET_PROCESSING", patch: { naRemovePercent: 100 } });
                      } else {
                        dispatch({ type: "DP_SET_PROCESSING", patch: { naRemovePercent: val } });
                      }
                    }
                  }}
                  style={{
                    width: 80,
                    padding: "8px 10px",
                    border: `1px solid ${(!done || cfgChanged) ? "#fecaca" : "var(--border)"}`,
                    borderRadius: 7,
                    fontSize: 13
                  }}
                  data-testid="input-na-remove-percent"
                />
                <span style={{ fontSize: 12, color: "var(--muted-foreground)" }}>
                  % (Features with missing values in more than this % of samples will be filtered out. Range: 0-100)
                </span>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Missing Values Card — only show if any dataset has NA */}
      {hasMissingValues && (
        <div className={`card ${(!done || cfgChanged) ? "banner-danger" : ""}`}>
          <div className="card-title" style={{ marginBottom: 4 }}>Missing Values</div>
          <div className="card-sub" style={{ color: (!done || cfgChanged) ? "#991b1b" : "var(--muted-foreground)" }}>
            {(!done || cfgChanged)
              ? "Handle missing values to minimize their impact on normalization."
              : "Missing values have been successfully imputed."}
          </div>
          {missingValueCount > 0 && (
            <div style={{ marginTop: 10, marginBottom: 4, fontWeight: 600, color: "#991b1b" }}>
              ⚠️ Found {missingValueCount} missing or non-numeric values in the expression data.
            </div>
          )}
          {done && !cfgChanged && (
            <div style={{ marginTop: 10, marginBottom: 4, fontWeight: 600, color: "#065f46" }}>
              ✅ All missing values handled and imputed.
            </div>
          )}
          <hr className="card-divider" style={{ borderColor: (!done || cfgChanged) ? "#fca5a5" : "var(--border)" }} />
          <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
            {MISSING_METHODS.map(m => (
              <label key={m.value} style={{
                display: "flex", alignItems: "center", gap: 10, padding: "9px 12px", borderRadius: 8, cursor: "pointer",
                border: `1px solid ${cfg.missingMethod === m.value ? "var(--primary)" : (!done || cfgChanged) ? "#fecaca" : "var(--border)"}`,
                background: cfg.missingMethod === m.value ? "var(--selected-bg)" : "white", transition: "all .12s",
              }}>
                <input type="radio" name="missingMethod" value={m.value} checked={cfg.missingMethod === m.value}
                  onChange={() => dispatch({ type: "DP_SET_PROCESSING", patch: { missingMethod: m.value as typeof cfg.missingMethod } })}
                  style={{ accentColor: "var(--primary)" }}
                  data-testid={`radio-missing-${m.value}`} />
                <span style={{ fontSize: 13, fontWeight: 500, color: (!done || cfgChanged) ? "#991b1b" : "var(--foreground)" }}>{m.label}</span>
                {m.hasParams && cfg.missingMethod === m.value && (
                  <div style={{ marginLeft: "auto", display: "flex", alignItems: "center", gap: 6 }}>
                    <label style={{ fontSize: 11, color: (!done || cfgChanged) ? "#b91c1c" : "var(--muted-foreground)" }}>{m.paramLabel}:</label>
                    <input type="number" value={cfg.knnK ?? ""} min={1} max={maxFeatures}
                      onChange={e => {
                        const valStr = e.target.value;
                        if (valStr === "") {
                          dispatch({ type: "DP_SET_PROCESSING", patch: { knnK: null } });
                        } else {
                          const val = Number(valStr);
                          if (val < 1) {
                            dispatch({ type: "DP_SET_PROCESSING", patch: { knnK: 1 } });
                          } else if (val > maxFeatures) {
                            dispatch({ type: "DP_SET_PROCESSING", patch: { knnK: maxFeatures } });
                          } else {
                            dispatch({ type: "DP_SET_PROCESSING", patch: { knnK: val } });
                          }
                        }
                      }}
                      style={{ width: 60, padding: "4px 8px", border: "1px solid var(--border)", borderRadius: 6, fontSize: 12 }}
                      data-testid="input-knn-k" />
                  </div>
                )}
              </label>
            ))}
          </div>
        </div>
      )}

      {/* Filtering Card(s) */}
      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>
          {isMixedTypes ? "Filtering" : getSingleTitle(targetDatasets[0]?.dataType)}
        </div>
        <div className="card-sub">Remove low-quality or low-abundance features before normalization.</div>
        <hr className="card-divider" />

        {isMixedTypes ? (
          <>
            {/* Raw Read Counts -> Full Filter Methods */}
            {rawRcDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Raw Readcounts ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                localFilterMethod={rcFilterMethod}
                onFilterMethodChange={setRcFilterMethod}
                localVarianceThreshold={rcVariance}
                onVarianceThresholdChange={setRcVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={false}
                dataTestIdPrefix="raw-rc"
                maxSamples={rawRcDatasets.length > 0 ? Math.max(...rawRcDatasets.map(d => d.nSamples || 0)) : 1}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            )}

            {/* Normalized Read Counts -> Restricted (Variance only) */}
            {normRcDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Normalized Readcounts ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                localFilterMethod={normRcFilterMethod}
                onFilterMethodChange={setNormRcFilterMethod}
                localVarianceThreshold={normRcVariance}
                onVarianceThresholdChange={setNormRcVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={true}
                dataTestIdPrefix="norm-rc"
                maxSamples={normRcDatasets.length > 0 ? Math.max(...normRcDatasets.map(d => d.nSamples || 0)) : 1}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            )}

            {/* Microarray -> Restricted */}
            {maDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Microarray ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                localFilterMethod={maFilterMethod}
                onFilterMethodChange={setMaFilterMethod}
                localVarianceThreshold={maVariance}
                onVarianceThresholdChange={setMaVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={true}
                dataTestIdPrefix="ma"
                maxSamples={maDatasets.length > 0 ? Math.max(...maDatasets.map(d => d.nSamples || 0)) : 1}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            )}

            {/* Proteomics -> Restricted */}
            {protDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Proteomics ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                localFilterMethod={protFilterMethod}
                onFilterMethodChange={setProtFilterMethod}
                localVarianceThreshold={protVariance}
                onVarianceThresholdChange={setProtVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={true}
                dataTestIdPrefix="prot"
                maxSamples={protDatasets.length > 0 ? Math.max(...protDatasets.map(d => d.nSamples || 0)) : 1}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            )}

            {/* Others -> Restricted */}
            {othDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Others ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                localFilterMethod={othFilterMethod}
                onFilterMethodChange={setOthFilterMethod}
                localVarianceThreshold={othVariance}
                onVarianceThresholdChange={setOthVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={true}
                dataTestIdPrefix="oth"
                maxSamples={othDatasets.length > 0 ? Math.max(...othDatasets.map(d => d.nSamples || 0)) : 1}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            )}
          </>
          ) : (
          // Single-type: render full filtering options for raw readcounts, or variance-only for other data types
          (() => {
            const targetDs = targetDatasets[0];
            const isRawRc = targetDs && isRawReadCounts(targetDs);
            
            const type = targetDs?.dataType ?? "readcounts";
            const isNormRc = type === "readcounts" && !isRawRc;
            const localMethod = isRawRc ? rcFilterMethod : isNormRc ? normRcFilterMethod : type === "microarray" ? maFilterMethod : type === "proteomics" ? protFilterMethod : othFilterMethod;
            const setLocalMethod = isRawRc ? setRcFilterMethod : isNormRc ? setNormRcFilterMethod : type === "microarray" ? setMaFilterMethod : type === "proteomics" ? setProtFilterMethod : setOthFilterMethod;
            const localVariance = isRawRc ? rcVariance : isNormRc ? normRcVariance : type === "microarray" ? maVariance : type === "proteomics" ? protVariance : othVariance;
            const setLocalVariance = isRawRc ? setRcVariance : isNormRc ? setNormRcVariance : type === "microarray" ? setMaVariance : type === "proteomics" ? setProtVariance : setOthVariance;
            
            const methods = isRawRc ? ALL_FILTER_METHODS : RESTRICTED_FILTER_METHODS;
            const effectiveMethod = isRawRc ? localMethod : (methods.find(m => m.value === localMethod) ? localMethod : methods[0].value);
            const maxSamples = targetDatasets.length > 0 ? Math.max(...targetDatasets.map(d => d.nSamples || 0)) : 1;

            return (
              <FilteringCard
                title={getSingleTitle(type)}
                cfg={cfg}
                localFilterMethod={effectiveMethod}
                onFilterMethodChange={setLocalMethod}
                localVarianceThreshold={localVariance}
                onVarianceThresholdChange={setLocalVariance}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                restrictedMethods={!isRawRc}
                dataTestIdPrefix="single"
                maxSamples={maxSamples}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                onCountThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { countThreshold: v } })}
              />
            );
          })()
        )}
      </div>

      {/* Results */}
      {done && (
        <div className="card" style={{ opacity: cfgChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div className="card-title" style={{ marginBottom: 14 }}>
            Filtering Summary
            {cfgChanged && (
              <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>
                (Outdated - settings changed)
              </span>
            )}
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

          <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
            {(() => {
              const activeDs = targetDatasets.find(d => d.id === (activeTabId || targetDatasets[0]?.id));
              if (!activeDs) return null;
              return <ProcessingResultCard targetDs={activeDs} />;
            })()}
          </div>
        </div>
      )}

      {validationError && (
        <div className="banner danger" style={{ marginBottom: 14 }}>
          ⚠️ <strong>Validation Error:</strong> {validationError}
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: getNextAndPrevSteps().prev } as never)}>← Back</button>
          <button className="btn btn-default" onClick={() => setShowSkipModal(true)} data-testid="btn-skip-processing" style={{ color: "hsl(220 9% 48%)" }}>Skip</button>
        </div>
        <div style={{ display: "flex", gap: 8 }}>
          {cfgChanged && (
            <button
              className="btn btn-default"
              style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)", opacity: validationError ? 0.6 : 1 }}
              disabled={!!validationError}
              onClick={async () => {
                if (downstreamDone) {
                  setShowDiscardModal(true);
                } else {
                  try {
                    await redoStepAPI("processing", targetDatasets.map(d => d.id));
                  } catch (e) {
                    console.error("Failed to redo processing:", e);
                  }
                  runProcessing();
                }
              }}
              data-testid="btn-redo-processing"
            >
              🔄 Redo Filtering
            </button>
          )}
          {!done ? (
            <button className="btn btn-primary" onClick={runProcessing} disabled={!!validationError} data-testid="btn-apply-processing">
              Apply Processing
            </button>
          ) : (
            <button className="btn btn-primary" onClick={handleContinue} disabled={!!validationError} data-testid="btn-continue-normalization">
              Continue →
            </button>
          )}
        </div>
      </div>
    </>
  );
}