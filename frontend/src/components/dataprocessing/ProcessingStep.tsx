import { useState, useEffect, useRef, useMemo } from "react";
import { CheckCircle2, AlertTriangle, RotateCcw } from "lucide-react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import { computeDPSteps } from "../../dataObject";
import Spinner from "../Spinner";
import SkipModal from "../shared/SkipModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import InteractivePCAPlot from "../shared/InteractivePCAPlot";
import type { ProcessingConfig, FilterMethod } from "../../dataObject";
import { processDatasetAPI, fetchPreProcessingQCAPI, redoStepAPI, cancelComputeJobAPI, downloadExportFileAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

interface Props {
  datasetId: string;
  mode?: "dp" | "de";
}

// Filter method options per dataType group
const RAW_RC_FILTER_METHODS: { value: FilterMethod; label: string; desc: string }[] = [
  { value: "cpm", label: "CPM Threshold", desc: "Keeps genes with expression above CPM threshold in at least a minimum number of samples. Reduces noise, but may exclude genes expressed only in a small subset of samples." },
  { value: "abundance", label: "Abundance Filter", desc: "Filters out features with readcounts < threshold in at least % of samples." },
  { value: "variance", label: "Variance Filter", desc: "Keeps genes with expression variance above threshold. Improves signal for discovery, but removes genes with consistent expression." },
];

const OTHER_FILTER_METHODS: { value: FilterMethod; label: string; desc: string }[] = [
  { value: "abundance", label: "Abundance Filter", desc: "Filters out features in the lowest % quantile based on mean or median abundance across samples." },
  { value: "variance", label: "Variance Filter", desc: "Keeps features with expression variance above threshold. Improves signal for discovery, but removes features with consistent expression." },
];

const MISSING_METHODS = [
  { value: "knn", label: "KNN Imputation", hasParams: true, paramLabel: "k neighbours" },
];

/** Sub-card for a single dataType group's filtering settings */
function FilteringCard({
  title,
  cfg,
  isRawRc,
  localFilterMethod,
  onFilterMethodChange,
  localVarianceThreshold,
  onVarianceThresholdChange,
  localAbundanceThreshold,
  onAbundanceThresholdChange,
  localAbundanceMethod,
  onAbundanceMethodChange,
  localSamplePercent,
  onSamplePercentChange,
  onCpmThresholdChange,
  onMinSamplesChange,
  maxSamples,
  dataTestIdPrefix,
}: {
  title: string;
  cfg: ProcessingConfig;
  isRawRc: boolean;
  localFilterMethod: FilterMethod | "" | null;
  onFilterMethodChange: (m: FilterMethod | "" | null) => void;
  localVarianceThreshold: number | "" | null;
  onVarianceThresholdChange: (v: number | "" | null) => void;
  localAbundanceThreshold: number | "" | null;
  onAbundanceThresholdChange: (v: number | "" | null) => void;
  localAbundanceMethod?: "mean" | "median" | null;
  onAbundanceMethodChange?: (m: "mean" | "median") => void;
  localSamplePercent?: number | "" | null;
  onSamplePercentChange?: (v: number | "" | null) => void;
  onCpmThresholdChange?: (v: number | "" | null) => void;
  onMinSamplesChange?: (v: number | "" | null) => void;
  maxSamples: number;
  dataTestIdPrefix: string;
}) {
  const methods = isRawRc ? RAW_RC_FILTER_METHODS : OTHER_FILTER_METHODS;

  const isMethodActive = (methodVal: string) => {
    const activeMethods = (localFilterMethod || "").split(",").map(m => m.trim());
    return activeMethods.includes(methodVal);
  };

  const toggleMethod = (methodVal: string) => {
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
              type="checkbox"
              name={`filterMethod-${dataTestIdPrefix}`}
              value={m.value}
              checked={isMethodActive(m.value)}
              onChange={() => toggleMethod(m.value)}
              style={{ accentColor: "var(--primary)", marginTop: 2 }}
              data-testid={`checkbox-filter-${dataTestIdPrefix}-${m.value}`}
            />
            <div>
              <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)" }}>{m.label}</div>
              <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 2 }}>{m.desc}</div>
            </div>
          </label>
        ))}
      </div>

      {/* CPM params (readcounts only) */}
      {isRawRc && isMethodActive("cpm") && onCpmThresholdChange && onMinSamplesChange && (
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 10 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>CPM Threshold</label>
            <input
              type="number"
              value={cfg.cpmThreshold ?? ""}
              min={0}
              step={0.5}
              placeholder="1"
              onFocus={e => e.target.select()}
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
              data-testid={`input-cpm-threshold-${dataTestIdPrefix}`}
            />
          </div>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Min. Samples</label>
            <input
              type="number"
              value={cfg.minSamples ?? ""}
              min={1}
              max={maxSamples}
              placeholder={String(Math.min(2, maxSamples))}
              onFocus={e => e.target.select()}
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
              data-testid={`input-cpm-min-samples-${dataTestIdPrefix}`}
            />
          </div>
        </div>
      )}

      {/* Abundance params: Readcount & % Samples (Raw RC) OR Mean/Median & Cutoff % (Others) */}
      {isMethodActive("abundance") && (
        isRawRc ? (
          <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 10 }}>
            <div style={{ display: "flex", flexDirection: "column" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Readcount Threshold</label>
              <input
                type="number"
                value={localAbundanceThreshold ?? ""}
                min={0}
                step={1}
                placeholder="10"
                onFocus={e => e.target.select()}
                onChange={e => {
                  const valStr = e.target.value;
                  if (valStr === "") {
                    onAbundanceThresholdChange(null);
                  } else {
                    const val = Number(valStr);
                    onAbundanceThresholdChange(val < 0 ? 0 : val);
                  }
                }}
                style={{ width: 160, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                data-testid={`input-abundance-threshold-${dataTestIdPrefix}`}
              />
            </div>
            <div style={{ display: "flex", flexDirection: "column" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Sample Percentage (%)</label>
              <input
                type="number"
                value={localSamplePercent ?? ""}
                min={0}
                max={100}
                step={1}
                placeholder="50"
                onFocus={e => e.target.select()}
                onChange={e => {
                  const valStr = e.target.value;
                  if (valStr === "") {
                    onSamplePercentChange?.(null);
                  } else {
                    const val = Number(valStr);
                    if (val < 0) onSamplePercentChange?.(0);
                    else if (val > 100) onSamplePercentChange?.(100);
                    else onSamplePercentChange?.(val);
                  }
                }}
                style={{ width: 160, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                data-testid={`input-abundance-sample-percent-${dataTestIdPrefix}`}
              />
            </div>
          </div>
        ) : (
          <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 10 }}>
            <div style={{ display: "flex", flexDirection: "column" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Abundance Metric</label>
              <div style={{ display: "flex", gap: 14, alignItems: "center", height: 38 }}>
                <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, cursor: "pointer", color: "var(--foreground)" }}>
                  <input
                    type="radio"
                    name={`abundanceMethod-${dataTestIdPrefix}`}
                    value="mean"
                    checked={(localAbundanceMethod || "mean") === "mean"}
                    onChange={() => onAbundanceMethodChange?.("mean")}
                    style={{ accentColor: "var(--primary)" }}
                    data-testid={`radio-abundance-mean-${dataTestIdPrefix}`}
                  />
                  Mean
                </label>
                <label style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 13, cursor: "pointer", color: "var(--foreground)" }}>
                  <input
                    type="radio"
                    name={`abundanceMethod-${dataTestIdPrefix}`}
                    value="median"
                    checked={localAbundanceMethod === "median"}
                    onChange={() => onAbundanceMethodChange?.("median")}
                    style={{ accentColor: "var(--primary)" }}
                    data-testid={`radio-abundance-median-${dataTestIdPrefix}`}
                  />
                  Median
                </label>
              </div>
            </div>
            <div style={{ display: "flex", flexDirection: "column" }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Cutoff Threshold (%)</label>
              <input
                type="number"
                value={localAbundanceThreshold ?? ""}
                min={0}
                max={100}
                step={1}
                placeholder="10"
                onFocus={e => e.target.select()}
                onChange={e => {
                  const valStr = e.target.value;
                  if (valStr === "") {
                    onAbundanceThresholdChange(null);
                  } else {
                    const val = Number(valStr);
                    if (val < 0) onAbundanceThresholdChange(0);
                    else if (val > 100) onAbundanceThresholdChange(100);
                    else onAbundanceThresholdChange(val);
                  }
                }}
                style={{ width: 160, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                data-testid={`input-abundance-threshold-${dataTestIdPrefix}`}
              />
            </div>
          </div>
        )
      )}

      {/* Variance threshold */}
      {isMethodActive("variance") && (
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 4 }}>
          <div style={{ display: "flex", flexDirection: "column" }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Variance Threshold (%)</label>
            <input
              type="number"
              value={localVarianceThreshold ?? ""}
              min={0}
              max={100}
              step={1}
              placeholder="10"
              onFocus={e => e.target.select()}
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
              data-testid={`input-variance-threshold-${dataTestIdPrefix}`}
            />
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

function getStandardQcFilename(dsName: string, plotKey: string, ext: string) {
  const d = new Date();
  const yy = String(d.getFullYear()).slice(-2);
  const mm = String(d.getMonth() + 1).padStart(2, "0");
  const dd = String(d.getDate()).padStart(2, "0");
  const datePrefix = `${yy}${mm}${dd}`;
  const cleanName = (dsName || "dataset").replace(/[:/\\?*"<>| ]/g, "_");
  return `${datePrefix}_${cleanName}_${plotKey}.${ext.toLowerCase()}`;
}

const VIRIDIS_STOPS = [
  [68, 1, 84],
  [59, 82, 139],
  [33, 145, 140],
  [94, 201, 98],
  [253, 231, 37]
];

function sampleViridis(t: number): string {
  const clamped = Math.max(0, Math.min(1, t));
  const scaled = clamped * (VIRIDIS_STOPS.length - 1);
  const idx = Math.min(VIRIDIS_STOPS.length - 2, Math.floor(scaled));
  const frac = scaled - idx;
  const c0 = VIRIDIS_STOPS[idx];
  const c1 = VIRIDIS_STOPS[idx + 1];
  const r = Math.round(c0[0] + (c1[0] - c0[0]) * frac);
  const g = Math.round(c0[1] + (c1[1] - c0[1]) * frac);
  const b = Math.round(c0[2] + (c1[2] - c0[2]) * frac);
  return `rgb(${r}, ${g}, ${b})`;
}

interface HexBinCell {
  key: string;
  cx: number;
  cy: number;
  meanVal: number;
  varVal: number;
  count: number;
  sampleNames: string[];
}

function SingleHexbinMeanVariancePlot({
  mvData,
  title,
}: {
  mvData?: { means: number[]; vars: number[]; names: string[] } | null;
  title: string;
}) {
  const [hoveredBin, setHoveredBin] = useState<HexBinCell | null>(null);

  const means: number[] = mvData?.means || [];
  const vars: number[] = mvData?.vars || [];
  const names: string[] = mvData?.names || [];
  const nPoints = means.length;

  const width = 640;
  const height = 320;
  const padL = 62;
  const padR = 78;
  const padT = 32;
  const padB = 44;
  const plotW = width - padL - padR;
  const plotH = height - padT - padB;

  const computed = useMemo(() => {
    if (nPoints === 0) return null;
    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
    for (let i = 0; i < nPoints; i++) {
      const m = means[i], v = vars[i];
      if (!isFinite(m) || !isFinite(v)) continue;
      if (m < minX) minX = m;
      if (m > maxX) maxX = m;
      if (v < minY) minY = v;
      if (v > maxY) maxY = v;
    }
    if (!isFinite(minX)) { minX = 0; maxX = 1; minY = 0; maxY = 1; }
    minY = Math.max(0, minY);
    if (maxX <= minX) maxX = minX + 1;
    if (maxY <= minY) maxY = minY + 1;

    const toX = (val: number) => padL + ((val - minX) / (maxX - minX)) * plotW;
    const toY = (val: number) => padT + (1 - (val - minY) / (maxY - minY)) * plotH;

    // Hexbin grid in pixel space (pointy-top hexagons)
    const hexR = 9.5;
    const dx = Math.sqrt(3) * hexR;
    const dy = 1.5 * hexR;
    const binsMap = new Map<string, HexBinCell>();

    for (let i = 0; i < nPoints; i++) {
      const m = means[i], v = vars[i];
      if (!isFinite(m) || !isFinite(v)) continue;
      const px = toX(m) - padL;
      const py = toY(v) - padT;
      const row = Math.round(py / dy);
      const xOffset = (row % 2 !== 0) ? dx / 2 : 0;
      const col = Math.round((px - xOffset) / dx);
      const key = `${row}:${col}`;
      let cell = binsMap.get(key);
      if (!cell) {
        const cx = padL + col * dx + xOffset;
        const cy = padT + row * dy;
        const meanVal = minX + ((cx - padL) / plotW) * (maxX - minX);
        const varVal = minY + (1 - (cy - padT) / plotH) * (maxY - minY);
        cell = {
          key,
          cx,
          cy,
          meanVal,
          varVal,
          count: 0,
          sampleNames: []
        };
        binsMap.set(key, cell);
      }
      cell.count++;
      if (cell.sampleNames.length < 5) {
        cell.sampleNames.push(names[i] || `Feature_${i + 1}`);
      }
    }

    const bins = Array.from(binsMap.values());
    const maxCount = bins.reduce((acc, b) => Math.max(acc, b.count), 1);

    // Smooth trend curve across 36 horizontal bins
    const nTrendBins = 36;
    const binSize = (maxX - minX) / nTrendBins;
    const trendBuckets = Array.from({ length: nTrendBins }, (_, b) => ({
      sumY: 0,
      count: 0,
      midX: minX + (b + 0.5) * binSize
    }));
    for (let i = 0; i < nPoints; i++) {
      const m = means[i], v = vars[i];
      if (!isFinite(m) || !isFinite(v)) continue;
      const bIdx = Math.min(nTrendBins - 1, Math.max(0, Math.floor((m - minX) / binSize)));
      trendBuckets[bIdx].sumY += v;
      trendBuckets[bIdx].count++;
    }
    const trendPts = trendBuckets
      .filter(b => b.count > 0)
      .map(b => ({ x: toX(b.midX), y: toY(b.sumY / b.count) }));

    return { minX, maxX, minY, maxY, hexR, bins, maxCount, trendPts, toX, toY };
  }, [means, vars, names, nPoints, plotW, plotH]);

  if (!mvData || nPoints === 0 || !computed) {
    return (
      <div style={{ padding: "24px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        No Mean-Variance QC data available for <strong>{title}</strong>.
      </div>
    );
  }

  const { minX, maxX, minY, maxY, hexR, bins, maxCount, trendPts, toX, toY } = computed;
  const xTicks = Array.from({ length: 6 }, (_, i) => minX + (i / 5) * (maxX - minX));
  const yTicks = Array.from({ length: 5 }, (_, i) => minY + (i / 4) * (maxY - minY));
  const logMax = Math.log10(Math.max(maxCount, 2));

  const hexPoints = (cx: number, cy: number, r: number) => {
    const pts: string[] = [];
    for (let k = 0; k < 6; k++) {
      const ang = (Math.PI / 180) * (60 * k - 30);
      pts.push(`${(cx + r * Math.cos(ang)).toFixed(1)},${(cy + r * Math.sin(ang)).toFixed(1)}`);
    }
    return pts.join(" ");
  };

  const trendPath = trendPts.length > 1
    ? trendPts.map((pt, idx) => `${idx === 0 ? "M" : "L"}${pt.x.toFixed(1)},${pt.y.toFixed(1)}`).join(" ")
    : "";

  const gradId = `viridis-grad-${title.replace(/[^a-zA-Z0-9]/g, "_")}`;
  const clipId = `hex-clip-${title.replace(/[^a-zA-Z0-9]/g, "_")}`;

  return (
    <div style={{ position: "relative", width: "100%", maxWidth: 660, margin: "0 auto", background: "#ffffff", borderRadius: 8, border: "1px solid var(--border)", padding: "8px 6px 4px 6px" }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", padding: "2px 10px 4px 10px" }}>
        <div style={{ fontSize: 12.5, fontWeight: 700, color: "#0f172a" }}>{title}</div>
        <div style={{ fontSize: 11, color: "#64748b" }}>
          <strong>{nPoints.toLocaleString()}</strong> features · <strong>{bins.length.toLocaleString()}</strong> hex bins
        </div>
      </div>

      <svg viewBox={`0 0 ${width} ${height}`} style={{ width: "100%", height: "auto", display: "block" }}>
        <defs>
          <linearGradient id={gradId} x1="0%" y1="100%" x2="0%" y2="0%">
            <stop offset="0%" stopColor="rgb(68, 1, 84)" />
            <stop offset="25%" stopColor="rgb(59, 82, 139)" />
            <stop offset="50%" stopColor="rgb(33, 145, 140)" />
            <stop offset="75%" stopColor="rgb(94, 201, 98)" />
            <stop offset="100%" stopColor="rgb(253, 231, 37)" />
          </linearGradient>
          <clipPath id={clipId}>
            <rect x={padL} y={padT} width={plotW} height={plotH} />
          </clipPath>
        </defs>

        {xTicks.map((tVal, idx) => (
          <g key={`x-${idx}`}>
            <line x1={toX(tVal)} y1={padT} x2={toX(tVal)} y2={height - padB} stroke="#f1f5f9" />
            <text x={toX(tVal)} y={height - padB + 15} textAnchor="middle" fontSize="10" fill="#64748b">
              {tVal.toFixed(1)}
            </text>
          </g>
        ))}

        {yTicks.map((tVal, idx) => (
          <g key={`y-${idx}`}>
            <line x1={padL} y1={toY(tVal)} x2={width - padR} y2={toY(tVal)} stroke="#f1f5f9" />
            <text x={padL - 7} y={toY(tVal) + 3.5} textAnchor="end" fontSize="10" fill="#64748b">
              {tVal.toFixed(1)}
            </text>
          </g>
        ))}

        <rect x={padL} y={padT} width={plotW} height={plotH} fill="none" stroke="#cbd5e1" strokeWidth="1.1" />

        <g clipPath={`url(#${clipId})`}>
          {bins.map(b => {
            const t = maxCount > 1 ? Math.log10(b.count) / logMax : 0.5;
            const fill = sampleViridis(t);
            const isHovered = hoveredBin?.key === b.key;
            return (
              <polygon
                key={b.key}
                points={hexPoints(b.cx, b.cy, hexR * 0.96)}
                fill={fill}
                stroke={isHovered ? "#0f172a" : "rgba(255,255,255,0.22)"}
                strokeWidth={isHovered ? 1.8 : 0.4}
                style={{ cursor: "pointer" }}
                onMouseEnter={() => setHoveredBin(b)}
                onMouseLeave={() => setHoveredBin(null)}
              />
            );
          })}

          {trendPath && (
            <path d={trendPath} fill="none" stroke="#ef4444" strokeWidth="2.2" style={{ pointerEvents: "none" }} />
          )}
        </g>

        {/* Colorbar Legend */}
        <g transform={`translate(${width - padR + 16}, ${padT + 10})`}>
          <text x={18} y={-4} textAnchor="middle" fontSize="9.5" fontWeight="600" fill="#475569">Count</text>
          <rect x={10} y={4} width={14} height={plotH - 28} rx={2} fill={`url(#${gradId})`} stroke="#cbd5e1" strokeWidth="0.8" />
          <text x={28} y={12} fontSize="9" fill="#475569">{maxCount.toLocaleString()}</text>
          {maxCount >= 10 && (
            <text x={28} y={4 + (plotH - 28) / 2 + 3} fontSize="9" fill="#475569">
              {Math.round(Math.pow(10, logMax / 2)).toLocaleString()}
            </text>
          )}
          <text x={28} y={plotH - 24} fontSize="9" fill="#475569">1</text>
        </g>

        <text x={padL + plotW / 2} y={height - 8} textAnchor="middle" fontSize="11" fill="#334155" fontWeight="600">
          Mean Expression
        </text>
        <text
          x={15}
          y={padT + plotH / 2}
          textAnchor="middle"
          fontSize="11"
          fill="#334155"
          fontWeight="600"
          transform={`rotate(-90 15 ${padT + plotH / 2})`}
        >
          Variance
        </text>
      </svg>

      {hoveredBin && (
        <div
          style={{
            position: "absolute",
            left: `${Math.min(80, Math.max(20, (hoveredBin.cx / width) * 100))}%`,
            top: `${Math.max(14, (hoveredBin.cy / height) * 100)}%`,
            transform: "translate(-50%, -115%)",
            background: "rgba(15, 23, 42, 0.94)",
            color: "#ffffff",
            padding: "6px 10px",
            borderRadius: 6,
            fontSize: 11,
            pointerEvents: "none",
            whiteSpace: "nowrap",
            zIndex: 20,
            boxShadow: "0 4px 10px rgba(0,0,0,0.22)"
          }}
        >
          <div style={{ fontWeight: 700, marginBottom: 2 }}>
            Hex Bin: {hoveredBin.count.toLocaleString()} {hoveredBin.count === 1 ? "feature" : "features"}
          </div>
          <div>Mean: {hoveredBin.meanVal.toFixed(3)} · Var: {hoveredBin.varVal.toFixed(3)}</div>
          {hoveredBin.sampleNames.length > 0 && (
            <div style={{ color: "#cbd5e1", fontSize: 10.5, marginTop: 2 }}>
              {hoveredBin.sampleNames.slice(0, 4).join(", ")}
              {hoveredBin.count > 4 ? ` +${(hoveredBin.count - 4).toLocaleString()} more` : ""}
            </div>
          )}
        </div>
      )}
    </div>
  );
}

function MeanVarianceQCPlot({ targetDs, showAfter }: { targetDs: any; showAfter: boolean }) {
  const [downloadingFmt, setDownloadingFmt] = useState<string | null>(null);
  const { toast } = useToast();

  const dsDisplayName = targetDs.name || targetDs.id;
  const beforeMv = targetDs.qc?.before?.meanVariance || (!showAfter ? targetDs.qc?.meanVariance : null) || targetDs.qc?.meanVariance;
  const afterMv = showAfter ? (targetDs.qc?.after?.meanVariance || targetDs.qc?.meanVariance) : null;

  const handleDownload = async (fmt: "svg" | "png" | "pdf") => {
    setDownloadingFmt(fmt);
    try {
      const uid = getSessionUserId();
      const { blob, filename } = await downloadExportFileAPI(uid, "qc_mean_variance", targetDs.id, fmt);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename || getStandardQcFilename(dsDisplayName, "qc_mean_variance", fmt);
      a.click();
      URL.revokeObjectURL(url);
    } catch (err: any) {
      toast({
        title: "Download Failed",
        description: err.message || "Failed to download plot file.",
        variant: "destructive"
      });
    } finally {
      setDownloadingFmt(null);
    }
  };

  if (!beforeMv && !afterMv) {
    return (
      <div style={{ padding: "32px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        Loading Hexbin Mean-Variance QC data for <strong>{dsDisplayName}</strong>…
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", flexWrap: "wrap", gap: 8 }}>
        <div style={{ fontSize: 12, color: "var(--muted-foreground)" }}>
          Hexbin Mean-Variance Density · <strong>{dsDisplayName}</strong> {showAfter ? "(Upper: Before Processing · Lower: After Processing)" : "(Before Processing)"}
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
          <span style={{ fontSize: 11, fontWeight: 600, color: "var(--muted-foreground)", textTransform: "uppercase" }}>Download:</span>
          {(["svg", "png", "pdf"] as const).map(fmt => (
            <button
              key={fmt}
              type="button"
              disabled={downloadingFmt !== null}
              onClick={() => handleDownload(fmt)}
              className="btn btn-sm btn-default"
              style={{ padding: "3px 9px", fontSize: 11, fontWeight: 600, textTransform: "uppercase" }}
            >
              {downloadingFmt === fmt ? "…" : fmt}
            </button>
          ))}
        </div>
      </div>

      {beforeMv && (
        <SingleHexbinMeanVariancePlot
          mvData={beforeMv}
          title={`${dsDisplayName} — Before Processing`}
        />
      )}

      {showAfter && afterMv && (
        <SingleHexbinMeanVariancePlot
          mvData={afterMv}
          title={`${dsDisplayName} — After Processing`}
        />
      )}
    </div>
  );
}

function SingleSampleDistributionPlot({
  dist,
  title,
}: {
  dist?: { grid: number[]; samples: Array<{ name: string; y: number[] }> } | null;
  title: string;
}) {
  const [hoveredSample, setHoveredSample] = useState<string | null>(null);

  const grid: number[] = dist?.grid || [];
  const samples: Array<{ name: string; y: number[] }> = dist?.samples || [];

  if (!dist || samples.length === 0) {
    return (
      <div style={{ padding: "24px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        No Distribution QC data available for <strong>{title}</strong>.
      </div>
    );
  }

  const width = 640, height = 310, padL = 58, padR = 24, padT = 28, padB = 44;
  const plotW = width - padL - padR;
  const plotH = height - padT - padB;

  const minX = grid.length > 0 ? grid[0] : 0;
  const maxX = grid.length > 0 ? grid[grid.length - 1] : 1;

  let maxY = 0;
  samples.forEach(s => {
    s.y.forEach(v => { if (v > maxY) maxY = v; });
  });
  maxY = maxY > 0 ? maxY * 1.08 : 1;

  const toX = (v: number) => padL + ((v - minX) / (maxX - minX || 1)) * plotW;
  const toY = (v: number) => padT + (1 - v / maxY) * plotH;

  const xTicks = 5;
  const ticksList = Array.from({ length: xTicks + 1 }, (_, i) => minX + (i / xTicks) * (maxX - minX));

  return (
    <div style={{ width: "100%", maxWidth: 660, margin: "0 auto", background: "#ffffff", borderRadius: 8, border: "1px solid var(--border)", padding: "8px 6px 6px 6px" }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", padding: "2px 10px 4px 10px" }}>
        <div style={{ fontSize: 12.5, fontWeight: 700, color: "#0f172a" }}>{title}</div>
        <div style={{ fontSize: 11, color: "#64748b" }}>
          <strong>{samples.length}</strong> samples
        </div>
      </div>

      <svg viewBox={`0 0 ${width} ${height}`} style={{ width: "100%", height: "auto", display: "block" }}>
        {ticksList.map((gVal, idx) => (
          <g key={idx}>
            <line x1={toX(gVal)} y1={padT} x2={toX(gVal)} y2={height - padB} stroke="#f1f5f9" />
            <text x={toX(gVal)} y={height - padB + 15} textAnchor="middle" fontSize="10" fill="#64748b">
              {gVal.toFixed(1)}
            </text>
          </g>
        ))}

        <line x1={padL} y1={padT} x2={padL} y2={height - padB} stroke="#cbd5e1" strokeWidth="1.2" />
        <line x1={padL} y1={height - padB} x2={width - padR} y2={height - padB} stroke="#cbd5e1" strokeWidth="1.2" />

        {samples.map((s, idx) => {
          const color = `hsl(${(idx * 360) / samples.length}, 75%, 45%)`;
          const isHovered = hoveredSample === s.name;
          const pts = grid.map((gx, gIdx) => `${gIdx ? "L" : "M"}${toX(gx).toFixed(1)},${toY(s.y[gIdx] || 0).toFixed(1)}`).join(" ");

          return (
            <path
              key={s.name}
              d={pts}
              fill="none"
              stroke={color}
              strokeWidth={isHovered ? "2.6" : "1.2"}
              opacity={hoveredSample ? (isHovered ? 1.0 : 0.25) : 0.72}
              style={{ cursor: "pointer", transition: "stroke-width 0.1s ease, opacity 0.1s ease" }}
              onMouseEnter={() => setHoveredSample(s.name)}
              onMouseLeave={() => setHoveredSample(null)}
            >
              <title>{s.name}</title>
            </path>
          );
        })}

        <text x={(padL + width - padR) / 2} y={height - 8} textAnchor="middle" fontSize="11" fill="#334155" fontWeight="600">
          Expression Value
        </text>
        <text x={15} y={(padT + height - padB) / 2} textAnchor="middle" fontSize="11" fill="#334155" fontWeight="600" transform={`rotate(-90 15 ${(padT + height - padB) / 2})`}>
          Density
        </text>
      </svg>

      {samples.length <= 24 && (
        <div style={{ display: "flex", flexWrap: "wrap", gap: 6, padding: "6px 12px 2px 12px", borderTop: "1px solid #f1f5f9", maxHeight: 84, overflowY: "auto" }}>
          {samples.map((s, idx) => {
            const color = `hsl(${(idx * 360) / samples.length}, 75%, 45%)`;
            const isHovered = hoveredSample === s.name;
            return (
              <div
                key={s.name}
                style={{
                  display: "flex",
                  alignItems: "center",
                  gap: 5,
                  fontSize: 10.5,
                  padding: "2px 6px",
                  borderRadius: 4,
                  background: isHovered ? "var(--selected-bg)" : "transparent",
                  cursor: "pointer"
                }}
                onMouseEnter={() => setHoveredSample(s.name)}
                onMouseLeave={() => setHoveredSample(null)}
              >
                <div style={{ width: 8, height: 8, borderRadius: "50%", background: color }} />
                <span style={{ color: isHovered ? "var(--foreground)" : "var(--muted-foreground)", fontWeight: isHovered ? 700 : 400 }}>{s.name}</span>
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}

function SampleDistributionQCPlot({ targetDs, showAfter }: { targetDs: any; showAfter: boolean }) {
  const [downloadingFmt, setDownloadingFmt] = useState<string | null>(null);
  const { toast } = useToast();

  const dsDisplayName = targetDs.name || targetDs.id;
  const beforeDist = targetDs.qc?.before?.distribution || (!showAfter ? targetDs.qc?.distribution : null) || targetDs.qc?.distribution;
  const afterDist = showAfter ? (targetDs.qc?.after?.distribution || targetDs.qc?.distribution) : null;

  const handleDownload = async (fmt: "svg" | "png" | "pdf") => {
    setDownloadingFmt(fmt);
    try {
      const uid = getSessionUserId();
      const { blob, filename } = await downloadExportFileAPI(uid, "qc_distribution", targetDs.id, fmt);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename || getStandardQcFilename(dsDisplayName, "qc_distribution", fmt);
      a.click();
      URL.revokeObjectURL(url);
    } catch (err: any) {
      toast({
        title: "Download Failed",
        description: err.message || "Failed to download plot file.",
        variant: "destructive"
      });
    } finally {
      setDownloadingFmt(null);
    }
  };

  if (!beforeDist && !afterDist) {
    return (
      <div style={{ padding: "32px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        Loading Sample Distribution QC data for <strong>{dsDisplayName}</strong>…
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", flexWrap: "wrap", gap: 8 }}>
        <div style={{ fontSize: 12, color: "var(--muted-foreground)" }}>
          Per-sample Gaussian KDE curves · <strong>{dsDisplayName}</strong> {showAfter ? "(Upper: Before Processing · Lower: After Processing)" : "(Before Processing)"}
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
          <span style={{ fontSize: 11, fontWeight: 600, color: "var(--muted-foreground)", textTransform: "uppercase" }}>Download:</span>
          {(["svg", "png", "pdf"] as const).map(fmt => (
            <button
              key={fmt}
              type="button"
              disabled={downloadingFmt !== null}
              onClick={() => handleDownload(fmt)}
              className="btn btn-sm btn-default"
              style={{ padding: "3px 9px", fontSize: 11, fontWeight: 600, textTransform: "uppercase" }}
            >
              {downloadingFmt === fmt ? "…" : fmt}
            </button>
          ))}
        </div>
      </div>

      {beforeDist && (
        <SingleSampleDistributionPlot
          dist={beforeDist}
          title={`${dsDisplayName} — Before Processing`}
        />
      )}

      {showAfter && afterDist && (
        <SingleSampleDistributionPlot
          dist={afterDist}
          title={`${dsDisplayName} — After Processing`}
        />
      )}
    </div>
  );
}

function NormalPCAQCPlot({ targetDs, showAfter }: { targetDs: any; showAfter: boolean }) {
  const [downloadingFmt, setDownloadingFmt] = useState<string | null>(null);
  const { toast } = useToast();

  const dsDisplayName = targetDs.name || targetDs.id;
  const beforePca = targetDs.qc?.before?.pca || (!showAfter ? (targetDs.qc?.pca || targetDs.normalPca) : null) || targetDs.qc?.pca;
  const afterPca = showAfter ? (targetDs.qc?.after?.pca || targetDs.qc?.pca || targetDs.normalPca) : null;

  const handleDownloadStacked = async (fmt: "svg" | "png" | "pdf") => {
    setDownloadingFmt(fmt);
    try {
      const uid = getSessionUserId();
      const { blob, filename } = await downloadExportFileAPI(uid, "qc_pca", targetDs.id, fmt);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename || getStandardQcFilename(dsDisplayName, "qc_pca", fmt);
      a.click();
      URL.revokeObjectURL(url);
    } catch (err: any) {
      toast({
        title: "Download Failed",
        description: err.message || "Failed to download PCA QC plot file.",
        variant: "destructive"
      });
    } finally {
      setDownloadingFmt(null);
    }
  };

  if (!beforePca && !afterPca) {
    return (
      <div style={{ padding: "32px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        Loading Normal PCA QC data for <strong>{dsDisplayName}</strong>…
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", flexWrap: "wrap", gap: 8 }}>
        <div style={{ fontSize: 12, color: "var(--muted-foreground)" }}>
          Interactive Normal PCA · <strong>{dsDisplayName}</strong> {showAfter ? "(Upper: Before Processing · Lower: After Processing)" : "(Before Processing)"}
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
          <span style={{ fontSize: 11, fontWeight: 600, color: "var(--muted-foreground)", textTransform: "uppercase" }}>Download Stacked QC:</span>
          {(["svg", "png", "pdf"] as const).map(fmt => (
            <button
              key={fmt}
              type="button"
              disabled={downloadingFmt !== null}
              onClick={() => handleDownloadStacked(fmt)}
              className="btn btn-sm btn-default"
              style={{ padding: "3px 9px", fontSize: 11, fontWeight: 600, textTransform: "uppercase" }}
            >
              {downloadingFmt === fmt ? "…" : fmt}
            </button>
          ))}
        </div>
      </div>

      {beforePca && (
        <InteractivePCAPlot
          pcaData={beforePca}
          title={`${dsDisplayName} — Before Processing`}
          colorByField="group"
          exportKey={!showAfter ? "pca_plots" : undefined}
          datasetId={targetDs.id}
          datasetName={dsDisplayName}
          hideDownloadButtons={showAfter}
        />
      )}

      {showAfter && afterPca && (
        <InteractivePCAPlot
          pcaData={afterPca}
          title={`${dsDisplayName} — After Processing`}
          colorByField="group"
          exportKey="pca_plots"
          datasetId={targetDs.id}
          datasetName={dsDisplayName}
        />
      )}
    </div>
  );
}

interface ProcessingResultCardProps {
  targetDs: any;
  isPreProcessingOnly?: boolean;
}

function ProcessingResultCard({ targetDs, isPreProcessingOnly = false }: ProcessingResultCardProps) {
  const [activeTab, setActiveTab] = useState<"summary" | "mean_variance" | "distribution" | "pca">(
    isPreProcessingOnly ? "mean_variance" : "summary"
  );

  useEffect(() => {
    if (isPreProcessingOnly && activeTab === "summary") {
      setActiveTab("mean_variance");
    }
  }, [isPreProcessingOnly, activeTab]);

  const total = targetDs.processingInputFeatures || targetDs.nFeatures || 0;
  const retained = targetDs.processingRetainedFeatures || 0;
  const removed = targetDs.processingRemovedFeatures || 0;
  const percentRetained = total > 0 ? ((retained / total) * 100).toFixed(1) : "0.0";
  const slices = [
    { label: "Retained", value: retained, color: "#3b82f6" },
    { label: "Removed", value: removed, color: "#94a3b8" }
  ];

  const showAfter = !isPreProcessingOnly && !!targetDs.processingDone;

  return (
    <div className="result-ds-card" style={{ margin: 0 }}>
      <div className="result-ds-card-header" style={{ marginBottom: 12 }}>
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: targetDs.color, flexShrink: 0 }} />
        <div style={{ fontSize: 13, fontWeight: 700, color: "hsl(220 25% 12%)", flex: 1 }}>
          {targetDs.name} — {isPreProcessingOnly ? "Pre-Processing QC Diagnostics" : "Processing & QC Results"}
        </div>
      </div>

      <div style={{ display: "flex", gap: 6, marginBottom: 14, borderBottom: "1px solid var(--border)", paddingBottom: 6, flexWrap: "wrap" }}>
        {!isPreProcessingOnly && (
          <button
            type="button"
            onClick={() => setActiveTab("summary")}
            className={`btn btn-sm ${activeTab === "summary" ? "btn-primary" : "btn-default"}`}
            style={{ fontSize: 12, padding: "4px 12px" }}
          >
            Filtering Summary
          </button>
        )}
        <button
          type="button"
          onClick={() => setActiveTab("mean_variance")}
          className={`btn btn-sm ${activeTab === "mean_variance" ? "btn-primary" : "btn-default"}`}
          style={{ fontSize: 12, padding: "4px 12px" }}
        >
          Mean-Variance QC (Hexbin)
        </button>
        <button
          type="button"
          onClick={() => setActiveTab("distribution")}
          className={`btn btn-sm ${activeTab === "distribution" ? "btn-primary" : "btn-default"}`}
          style={{ fontSize: 12, padding: "4px 12px" }}
        >
          Sample Distribution QC
        </button>
        <button
          type="button"
          onClick={() => setActiveTab("pca")}
          className={`btn btn-sm ${activeTab === "pca" ? "btn-primary" : "btn-default"}`}
          style={{ fontSize: 12, padding: "4px 12px" }}
        >
          Normal PCA QC
        </button>
      </div>

      {!isPreProcessingOnly && activeTab === "summary" && (
        <>
          <div className="banner success" style={{ marginBottom: 12, display: "flex", alignItems: "center", gap: 6 }}>
            <CheckCircle2 size={15} style={{ flexShrink: 0 }} />
            <span>Filtering complete. <strong>{retained.toLocaleString()} features retained</strong> ({percentRetained}%), {removed.toLocaleString()} removed.</span>
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
        </>
      )}

      {activeTab === "mean_variance" && (
        <MeanVarianceQCPlot targetDs={targetDs} showAfter={showAfter} />
      )}

      {activeTab === "distribution" && (
        <SampleDistributionQCPlot targetDs={targetDs} showAfter={showAfter} />
      )}

      {activeTab === "pca" && (
        <NormalPCAQCPlot targetDs={targetDs} showAfter={showAfter} />
      )}
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

  const preQcFetchingRef = useRef<Set<string>>(new Set());
  useEffect(() => {
    const missing = targetDatasets.filter(
      d => (!d.qc?.before || !d.qc?.before?.pca) && !preQcFetchingRef.current.has(d.id)
    );
    if (missing.length === 0) return;
    missing.forEach(d => preQcFetchingRef.current.add(d.id));
    let cancelled = false;
    fetchPreProcessingQCAPI(missing)
      .then(resList => {
        if (cancelled || !Array.isArray(resList)) return;
        resList.forEach(item => {
          if (!item || !item.datasetId || !item.qc) return;
          dispatch({
            type: mode === "dp" ? "DP_UPDATE_DATASET" : "DE_UPDATE_DATASET",
            id: item.datasetId,
            patch: {
              qc: item.qc,
              normalPca: item.qc?.pca || item.qc?.before?.pca || undefined,
            }
          } as never);
        });
      })
      .catch(err => {
        console.warn("Pre-processing QC fetch warning:", err);
      });
    return () => {
      cancelled = true;
    };
  }, [targetDatasets, dispatch, mode]);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const maxFeatures = targetDatasets.length > 0 ? Math.min(...targetDatasets.map(d => d.nFeatures || 100)) : 10;
  const maxSamples = targetDatasets.length > 0 ? Math.min(...targetDatasets.map(d => d.nSamples || 1)) : 1;

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
    if (cfg.rcReadcountThreshold === null || cfg.rcReadcountThreshold === undefined) {
      patch.rcReadcountThreshold = 10;
      hasChanges = true;
    }
    if (cfg.rcSamplePercent === null || cfg.rcSamplePercent === undefined) {
      patch.rcSamplePercent = 50;
      hasChanges = true;
    }
    if (cfg.rcVariance === null || cfg.rcVariance === undefined) {
      patch.rcVariance = 10;
      hasChanges = true;
    }
    if (cfg.normRcAbundanceMethod === null || cfg.normRcAbundanceMethod === undefined) {
      patch.normRcAbundanceMethod = "mean";
      hasChanges = true;
    }
    if (cfg.normRcAbundanceThreshold === null || cfg.normRcAbundanceThreshold === undefined) {
      patch.normRcAbundanceThreshold = 10;
      hasChanges = true;
    }
    if (cfg.normRcVariance === null || cfg.normRcVariance === undefined) {
      patch.normRcVariance = 10;
      hasChanges = true;
    }
    if (cfg.maAbundanceMethod === null || cfg.maAbundanceMethod === undefined) {
      patch.maAbundanceMethod = "mean";
      hasChanges = true;
    }
    if (cfg.maAbundanceThreshold === null || cfg.maAbundanceThreshold === undefined) {
      patch.maAbundanceThreshold = 10;
      hasChanges = true;
    }
    if (cfg.maVariance === null || cfg.maVariance === undefined) {
      patch.maVariance = 10;
      hasChanges = true;
    }
    if (cfg.protAbundanceMethod === null || cfg.protAbundanceMethod === undefined) {
      patch.protAbundanceMethod = "mean";
      hasChanges = true;
    }
    if (cfg.protAbundanceThreshold === null || cfg.protAbundanceThreshold === undefined) {
      patch.protAbundanceThreshold = 10;
      hasChanges = true;
    }
    if (cfg.protVariance === null || cfg.protVariance === undefined) {
      patch.protVariance = 10;
      hasChanges = true;
    }
    if (cfg.othAbundanceMethod === null || cfg.othAbundanceMethod === undefined) {
      patch.othAbundanceMethod = "mean";
      hasChanges = true;
    }
    if (cfg.othAbundanceThreshold === null || cfg.othAbundanceThreshold === undefined) {
      patch.othAbundanceThreshold = 10;
      hasChanges = true;
    }
    if (cfg.othVariance === null || cfg.othVariance === undefined) {
      patch.othVariance = 10;
      hasChanges = true;
    }

    if (hasChanges) {
      dispatch({ type: "DP_SET_PROCESSING", patch });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

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
      if (normStr(appliedCfg.rcFilterMethod) !== normStr(cfg.rcFilterMethod) ||
          normNum(appliedCfg.rcVariance) !== normNum(cfg.rcVariance) ||
          normNum(appliedCfg.cpmThreshold) !== normNum(cfg.cpmThreshold) ||
          normNum(appliedCfg.minSamples) !== normNum(cfg.minSamples) ||
          normNum(appliedCfg.rcReadcountThreshold) !== normNum(cfg.rcReadcountThreshold) ||
          normNum(appliedCfg.rcSamplePercent) !== normNum(cfg.rcSamplePercent)) {
        return true;
      }
    }
    if (hasNormRc) {
      if (normStr(appliedCfg.normRcFilterMethod) !== normStr(cfg.normRcFilterMethod) ||
          normNum(appliedCfg.normRcVariance) !== normNum(cfg.normRcVariance) ||
          normStr(appliedCfg.normRcAbundanceMethod) !== normStr(cfg.normRcAbundanceMethod) ||
          normNum(appliedCfg.normRcAbundanceThreshold) !== normNum(cfg.normRcAbundanceThreshold)) {
        return true;
      }
    }
    if (hasMa) {
      if (normStr(appliedCfg.maFilterMethod) !== normStr(cfg.maFilterMethod) ||
          normNum(appliedCfg.maVariance) !== normNum(cfg.maVariance) ||
          normStr(appliedCfg.maAbundanceMethod) !== normStr(cfg.maAbundanceMethod) ||
          normNum(appliedCfg.maAbundanceThreshold) !== normNum(cfg.maAbundanceThreshold)) {
        return true;
      }
    }
    if (hasProt) {
      if (normStr(appliedCfg.protFilterMethod) !== normStr(cfg.protFilterMethod) ||
          normNum(appliedCfg.protVariance) !== normNum(cfg.protVariance) ||
          normStr(appliedCfg.protAbundanceMethod) !== normStr(cfg.protAbundanceMethod) ||
          normNum(appliedCfg.protAbundanceThreshold) !== normNum(cfg.protAbundanceThreshold)) {
        return true;
      }
    }
    if (hasOth) {
      if (normStr(appliedCfg.othFilterMethod) !== normStr(cfg.othFilterMethod) ||
          normNum(appliedCfg.othVariance) !== normNum(cfg.othVariance) ||
          normStr(appliedCfg.othAbundanceMethod) !== normStr(cfg.othAbundanceMethod) ||
          normNum(appliedCfg.othAbundanceThreshold) !== normNum(cfg.othAbundanceThreshold)) {
        return true;
      }
    }
    
    return false;
  };

  const cfgChanged = checkCfgChanged();

  const getValidationError = (): string | null => {
    const isBlank = (val: any) => val === null || val === undefined || val === "";

    // 1. Check if at least one filtering method is selected for each active card
    if (hasRawRc) {
      const activeMethods = (cfg.rcFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Raw Readcounts.";
      }
      if (activeMethods.includes("cpm")) {
        if (isBlank(cfg.cpmThreshold) || Number(cfg.cpmThreshold) < 0) return "Please enter a valid non-negative CPM Threshold for Raw Readcounts.";
        if (isBlank(cfg.minSamples) || Number(cfg.minSamples) < 1) return "Please enter a valid Min. Samples (>= 1) for Raw Readcounts.";
      }
      if (activeMethods.includes("abundance")) {
        if (isBlank(cfg.rcReadcountThreshold) || Number(cfg.rcReadcountThreshold) < 0) return "Please enter a valid non-negative Readcount Threshold for Raw Readcounts.";
        if (isBlank(cfg.rcSamplePercent) || Number(cfg.rcSamplePercent) < 0 || Number(cfg.rcSamplePercent) > 100) return "Sample Percentage (%) for Raw Readcounts must be between 0 and 100.";
      }
      if (activeMethods.includes("variance")) {
        if (isBlank(cfg.rcVariance) || Number(cfg.rcVariance) < 0 || Number(cfg.rcVariance) > 100) return "Variance Threshold (%) for Raw Readcounts must be between 0 and 100.";
      }
    }
    if (hasNormRc) {
      const activeMethods = (cfg.normRcFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Normalized Readcounts.";
      }
      if (activeMethods.includes("abundance")) {
        if (isBlank(cfg.normRcAbundanceThreshold) || Number(cfg.normRcAbundanceThreshold) < 0 || Number(cfg.normRcAbundanceThreshold) > 100) return "Cutoff Threshold (%) for Normalized Readcounts must be between 0 and 100.";
      }
      if (activeMethods.includes("variance")) {
        if (isBlank(cfg.normRcVariance) || Number(cfg.normRcVariance) < 0 || Number(cfg.normRcVariance) > 100) return "Variance Threshold (%) for Normalized Readcounts must be between 0 and 100.";
      }
    }
    if (hasMa) {
      const activeMethods = (cfg.maFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Microarray.";
      }
      if (activeMethods.includes("abundance")) {
        if (isBlank(cfg.maAbundanceThreshold) || Number(cfg.maAbundanceThreshold) < 0 || Number(cfg.maAbundanceThreshold) > 100) return "Cutoff Threshold (%) for Microarray must be between 0 and 100.";
      }
      if (activeMethods.includes("variance")) {
        if (isBlank(cfg.maVariance) || Number(cfg.maVariance) < 0 || Number(cfg.maVariance) > 100) return "Variance Threshold (%) for Microarray must be between 0 and 100.";
      }
    }
    if (hasProt) {
      const activeMethods = (cfg.protFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Proteomics.";
      }
      if (activeMethods.includes("abundance")) {
        if (isBlank(cfg.protAbundanceThreshold) || Number(cfg.protAbundanceThreshold) < 0 || Number(cfg.protAbundanceThreshold) > 100) return "Cutoff Threshold (%) for Proteomics must be between 0 and 100.";
      }
      if (activeMethods.includes("variance")) {
        if (isBlank(cfg.protVariance) || Number(cfg.protVariance) < 0 || Number(cfg.protVariance) > 100) return "Variance Threshold (%) for Proteomics must be between 0 and 100.";
      }
    }
    if (hasOth) {
      const activeMethods = (cfg.othFilterMethod || "").split(",").map(m => m.trim()).filter(Boolean);
      if (activeMethods.length === 0) {
        return "Please select at least one filtering method for Other data types.";
      }
      if (activeMethods.includes("abundance")) {
        if (isBlank(cfg.othAbundanceThreshold) || Number(cfg.othAbundanceThreshold) < 0 || Number(cfg.othAbundanceThreshold) > 100) return "Cutoff Threshold (%) for Other data types must be between 0 and 100.";
      }
      if (activeMethods.includes("variance")) {
        if (isBlank(cfg.othVariance) || Number(cfg.othVariance) < 0 || Number(cfg.othVariance) > 100) return "Variance Threshold (%) for Other data types must be between 0 and 100.";
      }
    }

    // 2. Missing values validation (only if there are missing values in target datasets)
    if (hasMissingValues) {
      if (isBlank(cfg.naRemovePercent)) {
        return "Please specify the Missing Value Threshold (%).";
      }
      const naPercent = Number(cfg.naRemovePercent);
      if (isNaN(naPercent) || naPercent < 0 || naPercent > 100) {
        return "Missing Value Threshold (%) must be between 0 and 100.";
      }

      if (cfg.missingMethod !== "knn") {
        return "Please select KNN Imputation for handling missing values.";
      }

      if (isBlank(cfg.knnK)) {
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

  const abortControllerRef = useRef<AbortController | null>(null);
  const activeJobIdRef = useRef<string | null>(null);

  const handleCancel = async () => {
    abortControllerRef.current?.abort();
    if (activeJobIdRef.current) {
      await cancelComputeJobAPI(activeJobIdRef.current);
    }
    setLoading(false);
    activeJobIdRef.current = null;
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
    abortControllerRef.current = new AbortController();
    setLoading(true);
    try {
      let repFilterMethod: FilterMethod | "" | null = "";
      let repVarianceThreshold: number | "" | null = null;
      if (hasRawRc) {
        repFilterMethod = cfg.rcFilterMethod || "";
        repVarianceThreshold = cfg.rcVariance ?? null;
      } else if (hasNormRc) {
        repFilterMethod = cfg.normRcFilterMethod || "";
        repVarianceThreshold = cfg.normRcVariance ?? null;
      } else if (hasMa) {
        repFilterMethod = cfg.maFilterMethod || "";
        repVarianceThreshold = cfg.maVariance ?? null;
      } else if (hasProt) {
        repFilterMethod = cfg.protFilterMethod || "";
        repVarianceThreshold = cfg.protVariance ?? null;
      } else if (hasOth) {
        repFilterMethod = cfg.othFilterMethod || "";
        repVarianceThreshold = cfg.othVariance ?? null;
      }

      const activeConfig: ProcessingConfig = {
        ...cfg,
        filterMethod: repFilterMethod,
        varianceThreshold: repVarianceThreshold,
      };
      
      const results = await processDatasetAPI(
        targetDatasets,
        activeConfig,
        (info) => { activeJobIdRef.current = info.jobId; },
        abortControllerRef.current.signal
      );

      dispatch({
        type: "DP_SET_PROCESSING",
        patch: {
          filterMethod: repFilterMethod,
          varianceThreshold: repVarianceThreshold,
        }
      });
      dispatch({ type: "DP_SUBMIT_PROCESSING" });

      if (Array.isArray(results)) {
        results.forEach(res => {
          if (!res || typeof res !== "object" || !res.datasetId) return;
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
              hasNA: (res.missingValuesCount || 0) > 0,
              qc: res.qc || undefined,
              normalPca: res.qc?.after?.pca || res.qc?.pca || undefined,
            }
          } as never);
        });
      }
    } catch (err: any) {
      if (err?.name === "AbortError" || err?.message?.includes("Aborted") || err?.message?.includes("Unknown job id")) {
        return;
      }
      console.error("Processing error:", err);
      toast({
        title: "Processing Error",
        description: err.message || "An error occurred during dataset processing.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
      activeJobIdRef.current = null;
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
      {loading && (
        <Spinner
          label="Applying filtering and missing value handling…"
          onCancel={handleCancel}
        />
      )}

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
                  placeholder="50"
                  onFocus={e => e.target.select()}
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
            <div style={{ marginTop: 10, marginBottom: 4, fontWeight: 600, color: "#991b1b", display: "flex", alignItems: "center", gap: 6 }}>
              <AlertTriangle size={14} style={{ flexShrink: 0 }} />
              <span>Found {missingValueCount} missing or non-numeric values in the expression data.</span>
            </div>
          )}
          {done && !cfgChanged && (
            <div style={{ marginTop: 10, marginBottom: 4, fontWeight: 600, color: "#065f46", display: "flex", alignItems: "center", gap: 6 }}>
              <CheckCircle2 size={14} style={{ flexShrink: 0 }} />
              <span>All missing values handled and imputed.</span>
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
                    <input
                      type="number"
                      value={cfg.knnK ?? ""}
                      min={1}
                      max={maxFeatures}
                      placeholder={String(Math.min(10, maxFeatures))}
                      onFocus={e => e.target.select()}
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
            {/* Raw Read Counts -> Full Filter Methods (CPM, Abundance, Variance) */}
            {rawRcDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Raw Readcounts ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                isRawRc={true}
                localFilterMethod={cfg.rcFilterMethod ?? ""}
                onFilterMethodChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { rcFilterMethod: v } })}
                localVarianceThreshold={cfg.rcVariance}
                onVarianceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { rcVariance: v } })}
                localAbundanceThreshold={cfg.rcReadcountThreshold}
                onAbundanceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { rcReadcountThreshold: v } })}
                localSamplePercent={cfg.rcSamplePercent}
                onSamplePercentChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { rcSamplePercent: v } })}
                onCpmThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } })}
                onMinSamplesChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } })}
                maxSamples={rawRcDatasets.length > 0 ? Math.max(...rawRcDatasets.map(d => d.nSamples || 0)) : 1}
                dataTestIdPrefix="raw-rc"
              />
            )}

            {/* Normalized Read Counts -> Abundance, Variance */}
            {normRcDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Normalized Readcounts ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                isRawRc={false}
                localFilterMethod={cfg.normRcFilterMethod ?? ""}
                onFilterMethodChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { normRcFilterMethod: v } })}
                localVarianceThreshold={cfg.normRcVariance}
                onVarianceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { normRcVariance: v } })}
                localAbundanceMethod={cfg.normRcAbundanceMethod}
                onAbundanceMethodChange={m => dispatch({ type: "DP_SET_PROCESSING", patch: { normRcAbundanceMethod: m } })}
                localAbundanceThreshold={cfg.normRcAbundanceThreshold}
                onAbundanceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { normRcAbundanceThreshold: v } })}
                maxSamples={normRcDatasets.length > 0 ? Math.max(...normRcDatasets.map(d => d.nSamples || 0)) : 1}
                dataTestIdPrefix="norm-rc"
              />
            )}

            {/* Microarray -> Abundance, Variance */}
            {maDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Microarray ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                isRawRc={false}
                localFilterMethod={cfg.maFilterMethod ?? ""}
                onFilterMethodChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { maFilterMethod: v } })}
                localVarianceThreshold={cfg.maVariance}
                onVarianceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { maVariance: v } })}
                localAbundanceMethod={cfg.maAbundanceMethod}
                onAbundanceMethodChange={m => dispatch({ type: "DP_SET_PROCESSING", patch: { maAbundanceMethod: m } })}
                localAbundanceThreshold={cfg.maAbundanceThreshold}
                onAbundanceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { maAbundanceThreshold: v } })}
                maxSamples={maDatasets.length > 0 ? Math.max(...maDatasets.map(d => d.nSamples || 0)) : 1}
                dataTestIdPrefix="ma"
              />
            )}

            {/* Proteomics -> Abundance, Variance */}
            {protDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Proteomics ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                isRawRc={false}
                localFilterMethod={cfg.protFilterMethod ?? ""}
                onFilterMethodChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { protFilterMethod: v } })}
                localVarianceThreshold={cfg.protVariance}
                onVarianceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { protVariance: v } })}
                localAbundanceMethod={cfg.protAbundanceMethod}
                onAbundanceMethodChange={m => dispatch({ type: "DP_SET_PROCESSING", patch: { protAbundanceMethod: m } })}
                localAbundanceThreshold={cfg.protAbundanceThreshold}
                onAbundanceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { protAbundanceThreshold: v } })}
                maxSamples={protDatasets.length > 0 ? Math.max(...protDatasets.map(d => d.nSamples || 0)) : 1}
                dataTestIdPrefix="prot"
              />
            )}

            {/* Others -> Abundance, Variance */}
            {othDatasets.length > 0 && (
              <FilteringCard
                title={`Filtering for Others ${multipleDs ? "Datasets" : "Dataset"}`}
                cfg={cfg}
                isRawRc={false}
                localFilterMethod={cfg.othFilterMethod ?? ""}
                onFilterMethodChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { othFilterMethod: v } })}
                localVarianceThreshold={cfg.othVariance}
                onVarianceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { othVariance: v } })}
                localAbundanceMethod={cfg.othAbundanceMethod}
                onAbundanceMethodChange={m => dispatch({ type: "DP_SET_PROCESSING", patch: { othAbundanceMethod: m } })}
                localAbundanceThreshold={cfg.othAbundanceThreshold}
                onAbundanceThresholdChange={v => dispatch({ type: "DP_SET_PROCESSING", patch: { othAbundanceThreshold: v } })}
                maxSamples={othDatasets.length > 0 ? Math.max(...othDatasets.map(d => d.nSamples || 0)) : 1}
                dataTestIdPrefix="oth"
              />
            )}
          </>
        ) : (
          (() => {
            const targetDs = targetDatasets[0];
            const isRawRc = targetDs && isRawReadCounts(targetDs);
            
            const type = targetDs?.dataType ?? "readcounts";
            const isNormRc = type === "readcounts" && !isRawRc;
            const localMethod = isRawRc
              ? (cfg.rcFilterMethod ?? "")
              : isNormRc
              ? (cfg.normRcFilterMethod ?? "")
              : type === "microarray"
              ? (cfg.maFilterMethod ?? "")
              : type === "proteomics"
              ? (cfg.protFilterMethod ?? "")
              : (cfg.othFilterMethod ?? "");

            const setLocalMethod = (m: FilterMethod | "" | null) => {
              if (isRawRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { rcFilterMethod: m } });
              } else if (isNormRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { normRcFilterMethod: m } });
              } else if (type === "microarray") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { maFilterMethod: m } });
              } else if (type === "proteomics") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { protFilterMethod: m } });
              } else {
                dispatch({ type: "DP_SET_PROCESSING", patch: { othFilterMethod: m } });
              }
            };

            const localVariance = isRawRc
              ? cfg.rcVariance
              : isNormRc
              ? cfg.normRcVariance
              : type === "microarray"
              ? cfg.maVariance
              : type === "proteomics"
              ? cfg.protVariance
              : cfg.othVariance;

            const setLocalVariance = (v: number | "" | null) => {
              if (isRawRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { rcVariance: v } });
              } else if (isNormRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { normRcVariance: v } });
              } else if (type === "microarray") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { maVariance: v } });
              } else if (type === "proteomics") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { protVariance: v } });
              } else {
                dispatch({ type: "DP_SET_PROCESSING", patch: { othVariance: v } });
              }
            };

            const localAbundance = isRawRc
              ? cfg.rcReadcountThreshold
              : isNormRc
              ? cfg.normRcAbundanceThreshold
              : type === "microarray"
              ? cfg.maAbundanceThreshold
              : type === "proteomics"
              ? cfg.protAbundanceThreshold
              : cfg.othAbundanceThreshold;

            const setLocalAbundance = (v: number | "" | null) => {
              if (isRawRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { rcReadcountThreshold: v } });
              } else if (isNormRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { normRcAbundanceThreshold: v } });
              } else if (type === "microarray") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { maAbundanceThreshold: v } });
              } else if (type === "proteomics") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { protAbundanceThreshold: v } });
              } else {
                dispatch({ type: "DP_SET_PROCESSING", patch: { othAbundanceThreshold: v } });
              }
            };

            const localAbundMethod = isNormRc
              ? cfg.normRcAbundanceMethod
              : type === "microarray"
              ? cfg.maAbundanceMethod
              : type === "proteomics"
              ? cfg.protAbundanceMethod
              : cfg.othAbundanceMethod;

            const setLocalAbundMethod = (m: "mean" | "median") => {
              if (isNormRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { normRcAbundanceMethod: m } });
              } else if (type === "microarray") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { maAbundanceMethod: m } });
              } else if (type === "proteomics") {
                dispatch({ type: "DP_SET_PROCESSING", patch: { protAbundanceMethod: m } });
              } else {
                dispatch({ type: "DP_SET_PROCESSING", patch: { othAbundanceMethod: m } });
              }
            };

            const localSamplePct = isRawRc ? cfg.rcSamplePercent : null;

            const setLocalSamplePct = (v: number | "" | null) => {
              if (isRawRc) {
                dispatch({ type: "DP_SET_PROCESSING", patch: { rcSamplePercent: v } });
              }
            };

            const maxSamples = targetDatasets.length > 0 ? Math.max(...targetDatasets.map(d => d.nSamples || 0)) : 1;

            return (
              <FilteringCard
                title={getSingleTitle(type)}
                cfg={cfg}
                isRawRc={!!isRawRc}
                localFilterMethod={localMethod}
                onFilterMethodChange={setLocalMethod}
                localVarianceThreshold={localVariance}
                onVarianceThresholdChange={setLocalVariance}
                localAbundanceMethod={!isRawRc ? localAbundMethod : undefined}
                onAbundanceMethodChange={!isRawRc ? setLocalAbundMethod : undefined}
                localAbundanceThreshold={localAbundance}
                onAbundanceThresholdChange={setLocalAbundance}
                localSamplePercent={isRawRc ? localSamplePct : undefined}
                onSamplePercentChange={isRawRc ? setLocalSamplePct : undefined}
                onCpmThresholdChange={isRawRc ? v => dispatch({ type: "DP_SET_PROCESSING", patch: { cpmThreshold: v } }) : undefined}
                onMinSamplesChange={isRawRc ? v => dispatch({ type: "DP_SET_PROCESSING", patch: { minSamples: v } }) : undefined}
                dataTestIdPrefix="single"
                maxSamples={maxSamples}
              />
            );
          })()
        )}
      </div>

      {/* QC Diagnostics & Filtering Results (Pre-Processing shown immediately before compute; Before + After shown after compute) */}
      <div className="card" style={{ opacity: cfgChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
        <div className="card-title" style={{ marginBottom: 14 }}>
          {done ? "Filtering Summary & QC Diagnostics" : "Pre-Processing QC Diagnostics"}
          {cfgChanged && (
            <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>
              (Outdated - settings changed)
            </span>
          )}
        </div>

        {targetDatasets.length > 1 && (
          <div style={{ display: "flex", gap: 8, marginBottom: 16, borderBottom: "1px solid var(--border)", paddingBottom: 10, flexWrap: "wrap" }}>
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
            return <ProcessingResultCard targetDs={activeDs} isPreProcessingOnly={!done} />;
          })()}
        </div>
      </div>

      {validationError && (
        <div className="banner danger" style={{ marginBottom: 14, display: "flex", alignItems: "center", gap: 6 }}>
          <AlertTriangle size={15} style={{ flexShrink: 0 }} />
          <span><strong>Validation Error:</strong> {validationError}</span>
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={() => dispatch({ type: mode === "dp" ? "DP_SET_STEP" : "DE_SET_STEP", step: getNextAndPrevSteps().prev } as never)}>← Back</button>
          <button className="btn btn-default" onClick={() => setShowSkipModal(true)} data-testid="btn-skip-processing">Skip</button>
        </div>
        <div style={{ display: "flex", gap: 8 }}>
          {cfgChanged && (
            <button
              className="btn btn-default"
              style={{ borderColor: "var(--warning-border)", color: "var(--warning-text)", background: "var(--warning-bg)", opacity: validationError ? 0.6 : 1, display: "inline-flex", alignItems: "center", gap: 6 }}
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
              <RotateCcw size={14} /> Redo Filtering
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