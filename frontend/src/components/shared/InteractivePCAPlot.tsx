import { useState, useMemo } from "react";
import type { PCAPlotData, PCAPlotPoint } from "../../dataObject";
import { getSessionUserId } from "../../store/appStore";
import { downloadExportFileAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

const PALETTE = [
  "#2563eb", "#dc2626", "#16a34a", "#d97706", "#9333ea",
  "#0891b2", "#db2777", "#4f46e5", "#65a30d", "#ea580c",
  "#0d9488", "#7c3aed"
];

interface InteractivePCAPlotProps {
  pcaData?: PCAPlotData | null;
  fallbackImgSrc?: string;
  title: string;
  subtitle?: string;
  colorByField?: "group" | "batch" | "dataset";
  shapeByDataset?: boolean;
  exportKey?: string;
  datasetId?: string;
  datasetName?: string;
  hideDownloadButtons?: boolean;
}

function getCleanFilename(dsName: string, plotKey: string, ext: string) {
  const d = new Date();
  const yy = String(d.getFullYear()).slice(-2);
  const mm = String(d.getMonth() + 1).padStart(2, "0");
  const dd = String(d.getDate()).padStart(2, "0");
  const datePrefix = `${yy}${mm}${dd}`;
  const cleanName = (dsName || "dataset").replace(/[:/\\?*"<>| ]/g, "_");
  return `${datePrefix}_${cleanName}_${plotKey}.${ext.toLowerCase()}`;
}

export default function InteractivePCAPlot({
  pcaData,
  fallbackImgSrc,
  title,
  subtitle,
  colorByField = "group",
  shapeByDataset = false,
  exportKey,
  datasetId,
  datasetName,
  hideDownloadButtons = false,
}: InteractivePCAPlotProps) {
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null);
  const [hoveredCategory, setHoveredCategory] = useState<string | null>(null);
  const [downloadingFmt, setDownloadingFmt] = useState<string | null>(null);
  const { toast } = useToast();

  const points: PCAPlotPoint[] = pcaData?.points || [];

  const getCategory = (pt: PCAPlotPoint): string => {
    if (colorByField === "batch") return pt.batch || pt.group || "Batch 1";
    if (colorByField === "dataset") return pt.dataset || pt.group || "Dataset";
    return pt.group || pt.batch || "All Samples";
  };

  const categories = useMemo(() => {
    const set = new Set<string>();
    points.forEach(p => set.add(getCategory(p)));
    return Array.from(set);
  }, [points, colorByField]);

  const datasetsList = useMemo(() => {
    const set = new Set<string>();
    points.forEach(p => {
      if (p.dataset) set.add(p.dataset);
    });
    return Array.from(set);
  }, [points]);

  const colorMap = useMemo(() => {
    const map: Record<string, string> = {};
    categories.forEach((cat, idx) => {
      map[cat] = PALETTE[idx % PALETTE.length];
    });
    return map;
  }, [categories]);

  const handleDownload = async (fmt: "svg" | "png" | "pdf") => {
    if (!exportKey || !datasetId) return;
    setDownloadingFmt(fmt);
    try {
      const uid = getSessionUserId();
      const { blob, filename } = await downloadExportFileAPI(uid, exportKey, datasetId, fmt);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename || getCleanFilename(datasetName || title, exportKey, fmt);
      a.click();
      URL.revokeObjectURL(url);
    } catch (err: any) {
      toast({
        title: "Download Failed",
        description: err.message || "Failed to download PCA plot file.",
        variant: "destructive"
      });
    } finally {
      setDownloadingFmt(null);
    }
  };

  if (points.length === 0) {
    if (fallbackImgSrc) {
      return (
        <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", flexWrap: "wrap", gap: 8 }}>
            <div style={{ fontSize: 13, fontWeight: 700, color: "var(--foreground)" }}>{title}</div>
            {!hideDownloadButtons && exportKey && datasetId && (
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
            )}
          </div>
          <img
            src={`data:image/png;base64,${fallbackImgSrc}`}
            style={{ width: "100%", height: "auto", borderRadius: 8, border: "1px solid var(--border)" }}
            alt={title}
          />
        </div>
      );
    }
    return (
      <div style={{ padding: "32px 16px", textAlign: "center", color: "var(--muted-foreground)", fontSize: 13 }}>
        No PCA coordinates available yet.
      </div>
    );
  }

  const width = 640;
  const height = 350;
  const padL = 62;
  const padR = 24;
  const padT = 26;
  const padB = 48;
  const plotW = width - padL - padR;
  const plotH = height - padT - padB;

  let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
  for (const p of points) {
    if (p.pc1 < minX) minX = p.pc1;
    if (p.pc1 > maxX) maxX = p.pc1;
    if (p.pc2 < minY) minY = p.pc2;
    if (p.pc2 > maxY) maxY = p.pc2;
  }
  if (!isFinite(minX)) { minX = -1; maxX = 1; minY = -1; maxY = 1; }
  const spanX = Math.max(maxX - minX, 1e-3);
  const spanY = Math.max(maxY - minY, 1e-3);
  minX -= spanX * 0.1;
  maxX += spanX * 0.1;
  minY -= spanY * 0.1;
  maxY += spanY * 0.1;

  const toX = (v: number) => padL + ((v - minX) / (maxX - minX)) * plotW;
  const toY = (v: number) => padT + (1 - (v - minY) / (maxY - minY)) * plotH;

  const xTicks = Array.from({ length: 6 }, (_, i) => minX + (i / 5) * (maxX - minX));
  const yTicks = Array.from({ length: 5 }, (_, i) => minY + (i / 4) * (maxY - minY));

  const renderShape = (dsName: string | undefined, cx: number, cy: number, r: number, fill: string, isHovered: boolean, dimmed: boolean) => {
    const dsIdx = shapeByDataset && dsName ? Math.max(0, datasetsList.indexOf(dsName)) : 0;
    const shapeType = dsIdx % 4;
    const opacity = dimmed ? 0.22 : (isHovered ? 1 : 0.86);
    const stroke = isHovered ? "#0f172a" : "#ffffff";
    const strokeWidth = isHovered ? 2.2 : 1.1;

    if (shapeType === 1) {
      const side = r * 1.8;
      return (
        <rect
          x={cx - side / 2}
          y={cy - side / 2}
          width={side}
          height={side}
          rx={1.5}
          fill={fill}
          fillOpacity={opacity}
          stroke={stroke}
          strokeWidth={strokeWidth}
        />
      );
    }
    if (shapeType === 2) {
      const h = r * 2.1;
      const pts = `${cx},${cy - h * 0.6} ${cx - h * 0.58},${cy + h * 0.45} ${cx + h * 0.58},${cy + h * 0.45}`;
      return (
        <polygon
          points={pts}
          fill={fill}
          fillOpacity={opacity}
          stroke={stroke}
          strokeWidth={strokeWidth}
        />
      );
    }
    if (shapeType === 3) {
      const d = r * 1.45;
      const pts = `${cx},${cy - d} ${cx + d},${cy} ${cx},${cy + d} ${cx - d},${cy}`;
      return (
        <polygon
          points={pts}
          fill={fill}
          fillOpacity={opacity}
          stroke={stroke}
          strokeWidth={strokeWidth}
        />
      );
    }
    return (
      <circle
        cx={cx}
        cy={cy}
        r={r}
        fill={fill}
        fillOpacity={opacity}
        stroke={stroke}
        strokeWidth={strokeWidth}
      />
    );
  };

  const hoveredPt = hoveredIdx !== null ? points[hoveredIdx] : null;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", flexWrap: "wrap", gap: 8 }}>
        <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
          <span style={{ fontSize: 13, fontWeight: 700, color: "var(--foreground)" }}>{title}</span>
          {pcaData?.isLog2Transformed && (
            <span
              style={{
                fontSize: 10.5,
                fontWeight: 600,
                padding: "2px 7px",
                borderRadius: 999,
                background: "#eff6ff",
                color: "#1d4ed8",
                border: "1px solid #bfdbfe"
              }}
              title="Log2(x + 1) transformation applied on a temporary copy for PCA visualization only; original count matrix is unchanged."
            >
              log2(x + 1) PCA preview
            </span>
          )}
          {subtitle && (
            <span style={{ fontSize: 11.5, color: "var(--muted-foreground)" }}>· {subtitle}</span>
          )}
        </div>

        <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
          <span style={{ fontSize: 11.5, color: "var(--muted-foreground)" }}>
            <strong>{points.length}</strong> samples
          </span>
          {!hideDownloadButtons && exportKey && datasetId && (
            <div style={{ display: "flex", alignItems: "center", gap: 5 }}>
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
          )}
        </div>
      </div>

      <div style={{ position: "relative", width: "100%", maxWidth: 660, margin: "0 auto", background: "#ffffff", borderRadius: 8, border: "1px solid var(--border)", padding: "8px 6px 6px 6px" }}>
        <svg viewBox={`0 0 ${width} ${height}`} style={{ width: "100%", height: "auto", display: "block" }}>
          {xTicks.map((tVal, idx) => (
            <g key={`x-${idx}`}>
              <line x1={toX(tVal)} y1={padT} x2={toX(tVal)} y2={height - padB} stroke="#f1f5f9" strokeWidth="1" />
              <text x={toX(tVal)} y={height - padB + 16} textAnchor="middle" fontSize="10.5" fill="#64748b">
                {tVal.toFixed(1)}
              </text>
            </g>
          ))}
          {yTicks.map((tVal, idx) => (
            <g key={`y-${idx}`}>
              <line x1={padL} y1={toY(tVal)} x2={width - padR} y2={toY(tVal)} stroke="#f1f5f9" strokeWidth="1" />
              <text x={padL - 8} y={toY(tVal) + 3.5} textAnchor="end" fontSize="10.5" fill="#64748b">
                {tVal.toFixed(1)}
              </text>
            </g>
          ))}

          {/* Zero reference lines */}
          {minX <= 0 && maxX >= 0 && (
            <line x1={toX(0)} y1={padT} x2={toX(0)} y2={height - padB} stroke="#cbd5e1" strokeDasharray="4 4" strokeWidth="1" />
          )}
          {minY <= 0 && maxY >= 0 && (
            <line x1={padL} y1={toY(0)} x2={width - padR} y2={toY(0)} stroke="#cbd5e1" strokeDasharray="4 4" strokeWidth="1" />
          )}

          {/* Plot frame */}
          <rect x={padL} y={padT} width={plotW} height={plotH} fill="none" stroke="#cbd5e1" strokeWidth="1.2" />

          {/* Points */}
          {points.map((pt, idx) => {
            const cat = getCategory(pt);
            const fill = colorMap[cat] || "#2563eb";
            const isHovered = hoveredIdx === idx;
            const dimmed = (hoveredCategory !== null && hoveredCategory !== cat) || (hoveredIdx !== null && !isHovered);
            const cx = toX(pt.pc1);
            const cy = toY(pt.pc2);
            const r = isHovered ? 6.5 : 4.8;
            return (
              <g
                key={`${pt.sample}-${idx}`}
                style={{ cursor: "pointer" }}
                onMouseEnter={() => setHoveredIdx(idx)}
                onMouseLeave={() => setHoveredIdx(null)}
              >
                {renderShape(pt.dataset, cx, cy, r, fill, isHovered, dimmed)}
              </g>
            );
          })}

          {/* Axis labels */}
          <text x={(padL + width - padR) / 2} y={height - 8} textAnchor="middle" fontSize="11.5" fill="#334155" fontWeight="600">
            {pcaData?.pc1Label || "PC1"}
          </text>
          <text
            x={16}
            y={(padT + height - padB) / 2}
            textAnchor="middle"
            fontSize="11.5"
            fill="#334155"
            fontWeight="600"
            transform={`rotate(-90 16 ${(padT + height - padB) / 2})`}
          >
            {pcaData?.pc2Label || "PC2"}
          </text>
        </svg>

        {hoveredPt && (
          <div
            style={{
              position: "absolute",
              left: `${Math.min(82, Math.max(18, (toX(hoveredPt.pc1) / width) * 100))}%`,
              top: `${Math.max(12, (toY(hoveredPt.pc2) / height) * 100)}%`,
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
            <div style={{ fontWeight: 700, marginBottom: 2 }}>Sample: {hoveredPt.sample}</div>
            {hoveredPt.batch && <div>Batch: <strong>{hoveredPt.batch}</strong></div>}
            {hoveredPt.group && hoveredPt.group !== "All Samples" && <div>Group: <strong>{hoveredPt.group}</strong></div>}
            {hoveredPt.dataset && <div>Dataset: <strong>{hoveredPt.dataset}</strong></div>}
            <div style={{ color: "#cbd5e1", marginTop: 2 }}>
              PC1: {hoveredPt.pc1.toFixed(2)} · PC2: {hoveredPt.pc2.toFixed(2)}
            </div>
          </div>
        )}

        {/* Legend */}
        {(categories.length > 1 || ( categories.length === 1 && categories[0] !== "All Samples" )) && (
          <div style={{ display: "flex", flexWrap: "wrap", gap: 8, padding: "6px 12px 2px 12px", borderTop: "1px solid #f1f5f9", justifyContent: "center" }}>
            <span style={{ fontSize: 10.5, fontWeight: 700, color: "#64748b", alignSelf: "center" }}>
              {colorByField === "batch" ? "Batch:" : colorByField === "dataset" ? "Dataset:" : "Group:"}
            </span>
            {categories.map(cat => {
              const isHighlighted = hoveredCategory === cat;
              return (
                <div
                  key={cat}
                  style={{
                    display: "flex",
                    alignItems: "center",
                    gap: 5,
                    fontSize: 11,
                    padding: "2px 7px",
                    borderRadius: 4,
                    background: isHighlighted ? "var(--selected-bg)" : "transparent",
                    cursor: "pointer"
                  }}
                  onMouseEnter={() => setHoveredCategory(cat)}
                  onMouseLeave={() => setHoveredCategory(null)}
                >
                  <div style={{ width: 9, height: 9, borderRadius: "50%", background: colorMap[cat] }} />
                  <span style={{ fontWeight: isHighlighted ? 700 : 500, color: "var(--foreground)" }}>{cat}</span>
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
}
