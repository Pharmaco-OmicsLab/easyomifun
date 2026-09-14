import { useState } from "react";
import { Download, CheckCircle2, ChevronDown, Check, Loader2, FileText, File } from "lucide-react";
import RestartWarnModal from "./RestartWarnModal";
import Spinner from "../Spinner";
import { useToast } from "../../hooks/use-toast";
import { getSessionUserId } from "../../store/appStore";
import { SERVER_URL } from "../../lib/api";

export interface ExportFile {
  id: string;
  label: string;
  ext: string;
  desc: string;
  getTestIds?: (fmt: string) => string[];
}

export interface ExportGroup {
  name: string;
  badge?: string;
  badgeColor?: string;
  files?: ExportFile[];
  customContent?: React.ReactNode;
  initiallyCollapsed?: boolean;
  datasetId?: string;
  datasetLabel?: string;
  modelId?: string;
  modelLabel?: string;
  dbId?: string;
  dbLabel?: string;
}

export interface StatChip {
  label: string;
  value: string;
}

export interface Banner {
  type: "info" | "success" | "warning";
  text: string | React.ReactNode;
}

interface Props {
  title?: string;
  subtitle?: string;
  stats?: StatChip[];
  banners?: Banner[];
  groups: ExportGroup[];
  onBack?: () => void;
  backLabel?: string;
  onReset?: () => void;
  resetLabel?: string;
  resetTestId?: string;
  additionalCard?: React.ReactNode;
  onDownloadReport?: (format: "md" | "pdf") => void;
  loadingReport?: boolean;
  module: "dp" | "de" | "ea" | "fs";
}

export default function SharedExportStep({
  title = "Export Results",
  subtitle,
  stats,
  banners,
  groups,
  onBack,
  backLabel = "← Back",
  onReset,
  resetLabel = "Start New Analysis",
  resetTestId,
  additionalCard,
  onDownloadReport,
  loadingReport = false,
  module,
}: Props) {
  const { toast } = useToast();
  const [showRestartModal, setShowRestartModal] = useState(false);
  const [showReportDropdown, setShowReportDropdown] = useState(false);
  const [downloaded, setDownloaded] = useState<string[]>([]);
  const [loadingFile, setLoadingFile] = useState<string | null>(null);
  const [loadingAllZip, setLoadingAllZip] = useState(false);
  const safeGroups = Array.isArray(groups) ? groups : [];
  const [prevGroups, setPrevGroups] = useState(groups);
  const [collapsed, setCollapsed] = useState<Record<string, boolean>>(() => {
    const initial: Record<string, boolean> = {};
    const autoCollapse = safeGroups.length > 2;
    safeGroups.forEach((g, idx) => {
      if (!g) return;
      if (g.initiallyCollapsed !== undefined) {
        initial[g.name] = g.initiallyCollapsed;
      } else {
        initial[g.name] = autoCollapse && idx > 0;
      }
    });
    return initial;
  });

  if (groups !== prevGroups) {
    setPrevGroups(groups);
    const initial: Record<string, boolean> = {};
    const autoCollapse = safeGroups.length > 2;
    safeGroups.forEach((g, idx) => {
      if (!g) return;
      if (g.initiallyCollapsed !== undefined) {
        initial[g.name] = g.initiallyCollapsed;
      } else {
        initial[g.name] = autoCollapse && idx > 0;
      }
    });
    setCollapsed(initial);
  }

  // Filter states
  const [selectedDataset, setSelectedDataset] = useState<string | null>(null);
  const [selectedModel, setSelectedModel] = useState<string | null>(null);
  const [selectedDb, setSelectedDb] = useState<string | null>(null);

  // Extract unique filters from groups
  const datasets = Array.from(
    new Map(
      safeGroups
        .filter((g) => g && g.datasetId && g.datasetLabel)
        .map((g) => [g.datasetId, g.datasetLabel])
    ).entries()
  ) as [string, string][];

  const models = Array.from(
    new Map(
      safeGroups
        .filter((g) => g && g.modelId && g.modelLabel)
        .map((g) => [g.modelId, g.modelLabel])
    ).entries()
  ) as [string, string][];

  const dbs = Array.from(
    new Map(
      safeGroups
        .filter((g) => g && g.dbId && g.dbLabel)
        .map((g) => [g.dbId, g.dbLabel])
    ).entries()
  ) as [string, string][];

  // Filter groups
  const filteredGroups = safeGroups.filter((g) => {
    if (!g) return false;
    if (selectedDataset && g.datasetId && g.datasetId !== selectedDataset) return false;
    if (selectedModel && g.modelId && g.modelId !== selectedModel) return false;
    if (selectedDb && g.dbId && g.dbId !== selectedDb) return false;
    return true;
  });

  const toggleGroup = (name: string) => {
    setCollapsed((prev) => ({ ...prev, [name]: !prev[name] }));
  };

  const handleExpandAll = () => {
    const next: Record<string, boolean> = {};
    safeGroups.forEach((g) => {
      if (g) next[g.name] = false;
    });
    setCollapsed(next);
  };

  const handleCollapseAll = () => {
    const next: Record<string, boolean> = {};
    safeGroups.forEach((g) => {
      if (g) next[g.name] = true;
    });
    setCollapsed(next);
  };

  /**
   * Download a single file. The fileId uses pipe-delimited format for FS exports:
   *   "<type>|<dsId>"  e.g. "cv_results|usr_abc123_1786020309496_0_fs"
   *   "<type>|<dsId>|<model>" e.g. "performance_train|usr_abc123_1786020309496_0_fs|logistic"
  /**
   * Helper to poll the async /api/export-job endpoint until finished, then return the blob and filename
   */
  const pollExportJob = async (jobId: string): Promise<{ blob: Blob; filename: string }> => {
    const maxAttempts = 120; // 2 minutes max
    for (let i = 0; i < maxAttempts; i++) {
      await new Promise((resolve) => setTimeout(resolve, 1000));
      const res = await fetch(`${SERVER_URL}/api/export-job?id=${encodeURIComponent(jobId)}`);
      const contentType = res.headers.get("Content-Type") || "";

      if (res.ok) {
        if (contentType.includes("application/json")) {
          const json = await res.json();
          if (json.status === "running") {
            continue;
          } else if (json.status === "error" || json.status === "failed") {
            throw new Error(json.message || "Export job failed");
          }
        } else {
          // Binary blob received!
          const blob = await res.blob();
          const disposition = res.headers.get("Content-Disposition") || "";
          const match = disposition.match(/filename="?([^"]+)"?/);
          const filename = match ? match[1] : "";
          return { blob, filename };
        }
      } else {
        let msg = `Export request failed (status ${res.status})`;
        try {
          const errJson = await res.json();
          if (errJson && errJson.message) msg = errJson.message;
        } catch (_) {}
        throw new Error(msg);
      }
    }
    throw new Error("Export job timed out. Please try again.");
  };

  /**
   * Downloads a single file asynchronously via /api/export-job.
   * Structured file IDs use the pipe-delimited format: type|dsId[|model]
   * The backend provides the clean filename via Content-Disposition header.
   */
  const downloadFile = async (fileId: string, ext: string) => {
    const key = `${fileId}_${ext.toLowerCase()}`;
    setLoadingFile(key);

    try {
      const userId = getSessionUserId();
      const payload: any = {
        userId,
        ext: ext.toLowerCase()
      };

      // Structured file IDs use pipe-delimited format: type|dsId[|model]
      if (fileId.includes("|")) {
        const parts = fileId.split("|");
        payload.type = parts[0];
        payload.dsId = parts[1];
        if (parts[2]) payload.model = parts[2];
      } else {
        throw new Error("Invalid non-structured file ID requested: " + fileId);
      }

      const postRes = await fetch(`${SERVER_URL}/api/export-job`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload)
      });

      if (!postRes.ok) {
        let msg = `Failed to start export job (status ${postRes.status})`;
        try {
          const errJson = await postRes.json();
          if (errJson && errJson.message) msg = errJson.message;
        } catch (_) {}
        throw new Error(msg);
      }

      const postData = await postRes.json();
      if (!postData.jobId) {
        throw new Error(postData.message || "No export jobId returned from backend.");
      }

      const { blob, filename } = await pollExportJob(postData.jobId);
      setLoadingFile(null);
      setDownloaded((prev) => [...prev, key, `${fileId}_${ext.toUpperCase()}`]);
      
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      if (filename) {
        a.download = filename;
      } else {
        const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
        const typePart = fileId.includes("|") ? fileId.split("|")[0] : fileId;
        a.download = `${datePrefix}_${typePart}.${ext.toLowerCase()}`;
      }
      a.click();
      URL.revokeObjectURL(url);
    } catch (e: any) {
      setLoadingFile(null);
      console.error("Export download failed:", e);
      toast({
        title: "Export Download Failed",
        description: e.message || "Failed to download the export file. Please check the backend connection.",
        variant: "destructive"
      });
    }
  };

  const handleDownloadAllZip = async () => {
    setLoadingAllZip(true);

    try {
      const userId = getSessionUserId();
      const dsIds = Array.from(new Set(groups.filter((g) => g.datasetId).map((g) => g.datasetId!)));
      const payload: any = {
        userId,
        module
      };
      if (dsIds.length > 0) payload.datasetIds = dsIds.join(",");

      const postRes = await fetch(`${SERVER_URL}/api/export-job`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload)
      });

      if (!postRes.ok) {
        let msg = `Failed to start ZIP export job (status ${postRes.status})`;
        try {
          const errJson = await postRes.json();
          if (errJson && errJson.message) msg = errJson.message;
        } catch (_) {}
        throw new Error(msg);
      }

      const postData = await postRes.json();
      if (!postData.jobId) {
        throw new Error(postData.message || "No export jobId returned from backend.");
      }

      const { blob, filename } = await pollExportJob(postData.jobId);
      setLoadingAllZip(false);

      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
      a.download = filename || `${datePrefix}_all_results.zip`;
      a.click();
      URL.revokeObjectURL(url);
    } catch (e: any) {
      setLoadingAllZip(false);
      console.error("Export ZIP download failed:", e);
      toast({
        title: "ZIP Compile Failed",
        description: e.message || "Failed to compile the zip file. Please check the backend connection.",
        variant: "destructive"
      });
    }
  };

  return (
    <div>
      {loadingAllZip && <Spinner label="Preparing export ZIP archive..." />}
      {loadingFile && <Spinner label="Generating download file..." />}

      {showRestartModal && (
        <RestartWarnModal
          onConfirm={() => {
            setShowRestartModal(false);
            if (onReset) onReset();
          }}
          onCancel={() => setShowRestartModal(false)}
        />
      )}

      {/* Header card with summary & stats */}
      <div
        className="card"
        style={{
          marginBottom: 16,
          background: "linear-gradient(135deg, hsl(150 45% 96%) 0%, hsl(214 50% 97%) 100%)",
          borderColor: "hsl(150 35% 82%)",
        }}
      >
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", marginBottom: (banners && banners.length > 0) ? 16 : 0, flexWrap: "wrap", gap: 12 }}>
          <div>
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 4 }}>
              <CheckCircle2 size={20} color="hsl(150 55% 38%)" />
              <div className="card-title" style={{ fontSize: 17, color: "hsl(150 55% 28%)", margin: 0 }}>
                {title}
              </div>
            </div>
            {subtitle && <div style={{ fontSize: 13, color: "hsl(220 9% 48%)", marginLeft: 28 }}>{subtitle}</div>}
          </div>

          <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
            {onDownloadReport && (
              <div style={{ position: "relative" }}>
                <button
                  onClick={() => setShowReportDropdown(prev => !prev)}
                  className="btn btn-default"
                  style={{
                    display: "flex",
                    alignItems: "center",
                    gap: 6,
                    fontSize: 13,
                    fontWeight: 600,
                  }}
                  disabled={loadingReport}
                  data-testid="btn-download-report-toggle"
                >
                  {loadingReport ? (
                    <Loader2 size={14} className="animate-spin" />
                  ) : (
                    <FileText size={14} />
                  )}
                  Download Report <ChevronDown size={14} />
                </button>
                {showReportDropdown && (
                  <>
                    <div
                      style={{
                        position: "fixed",
                        top: 0,
                        left: 0,
                        right: 0,
                        bottom: 0,
                        zIndex: 99,
                      }}
                      onClick={() => setShowReportDropdown(false)}
                    />
                    <div
                      style={{
                        position: "absolute",
                        top: "100%",
                        right: 0,
                        marginTop: 4,
                        background: "#fff",
                        border: "1px solid hsl(214 30% 82%)",
                        borderRadius: 6,
                        boxShadow: "0 4px 12px rgba(0,0,0,0.08)",
                        zIndex: 100,
                        minWidth: 170,
                        display: "flex",
                        flexDirection: "column",
                        padding: 4,
                      }}
                    >
                      <button
                        onClick={() => {
                          onDownloadReport("md");
                          setShowReportDropdown(false);
                        }}
                        style={{
                          padding: "8px 12px",
                          background: "none",
                          border: "none",
                          textAlign: "left",
                          cursor: "pointer",
                          borderRadius: 4,
                          fontSize: 13,
                          color: "#333",
                          display: "flex",
                          alignItems: "center",
                          gap: 8,
                          width: "100%",
                        }}
                        className="hover:bg-slate-100"
                        data-testid="btn-download-report-md"
                      >
                        <FileText size={14} style={{ color: "hsl(214 60% 45%)" }} /> Markdown (.MD)
                      </button>
                      <button
                        onClick={() => {
                          onDownloadReport("pdf");
                          setShowReportDropdown(false);
                        }}
                        style={{
                          padding: "8px 12px",
                          background: "none",
                          border: "none",
                          textAlign: "left",
                          cursor: "pointer",
                          borderRadius: 4,
                          fontSize: 13,
                          color: "#333",
                          display: "flex",
                          alignItems: "center",
                          gap: 8,
                          width: "100%",
                        }}
                        className="hover:bg-slate-100"
                        data-testid="btn-download-report-pdf"
                      >
                        <File size={14} style={{ color: "hsl(0 75% 45%)" }} /> PDF Document (.PDF)
                      </button>
                    </div>
                  </>
                )}
              </div>
            )}

            <button
              onClick={handleDownloadAllZip}
              className="btn btn-default"
              style={{
                display: "flex",
                alignItems: "center",
                gap: 6,
                fontSize: 13,
                fontWeight: 600,
                borderColor: "hsl(214 30% 70%)",
              }}
              data-testid="btn-download-all-zip"
            >
              <Download size={14} /> Download All (.ZIP)
            </button>
          </div>
        </div>


        {banners && banners.length > 0 && (
          <div style={{ display: "flex", flexDirection: "column", gap: 8, marginTop: 12 }}>
            {banners.map((b, idx) => (
              <div key={idx} className={`banner ${b.type}`} style={{ marginBottom: 0 }}>
                {b.type === "success" && <CheckCircle2 size={14} style={{ marginRight: 6, flexShrink: 0 }} />}
                {b.text}
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Filters Toolbar */}
      {(datasets.length > 1 || models.length > 1 || dbs.length > 1) && (
        <div className="card" style={{ marginBottom: 14, padding: "12px 16px" }}>
          <div
            style={{
              fontSize: 11,
              fontWeight: 700,
              color: "var(--foreground)",
              textTransform: "uppercase",
              letterSpacing: ".04em",
              marginBottom: 10,
            }}
          >
            Filter Export List
          </div>
          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            {datasets.length > 1 && (
              <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                <span style={{ fontSize: 12, color: "var(--muted-foreground)", width: 80, fontWeight: 500 }}>Dataset:</span>
                <button
                  onClick={() => setSelectedDataset(null)}
                  className={`btn btn-sm ${selectedDataset === null ? "btn-primary" : "btn-default"}`}
                  style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                >
                  All
                </button>
                {datasets.map(([id, label]) => (
                  <button
                    key={id}
                    onClick={() => setSelectedDataset(id)}
                    className={`btn btn-sm ${selectedDataset === id ? "btn-primary" : "btn-default"}`}
                    style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                  >
                    {label}
                  </button>
                ))}
              </div>
            )}

            {models.length > 1 && (
              <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                <span style={{ fontSize: 12, color: "var(--muted-foreground)", width: 80, fontWeight: 500 }}>Model:</span>
                <button
                  onClick={() => setSelectedModel(null)}
                  className={`btn btn-sm ${selectedModel === null ? "btn-primary" : "btn-default"}`}
                  style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                >
                  All
                </button>
                {models.map(([id, label]) => (
                  <button
                    key={id}
                    onClick={() => setSelectedModel(id)}
                    className={`btn btn-sm ${selectedModel === id ? "btn-primary" : "btn-default"}`}
                    style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                  >
                    {label}
                  </button>
                ))}
              </div>
            )}

            {dbs.length > 1 && (
              <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                <span style={{ fontSize: 12, color: "var(--muted-foreground)", width: 80, fontWeight: 500 }}>Database:</span>
                <button
                  onClick={() => setSelectedDb(null)}
                  className={`btn btn-sm ${selectedDb === null ? "btn-primary" : "btn-default"}`}
                  style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                >
                  All
                </button>
                {dbs.map(([id, label]) => (
                  <button
                    key={id}
                    onClick={() => setSelectedDb(id)}
                    className={`btn btn-sm ${selectedDb === id ? "btn-primary" : "btn-default"}`}
                    style={{ borderRadius: 6, fontSize: 11, padding: "3px 8px" }}
                  >
                    {label}
                  </button>
                ))}
              </div>
            )}
          </div>
        </div>
      )}

      {/* Expand/Collapse All */}
      {filteredGroups.length > 1 && (
        <div style={{ display: "flex", justifyContent: "flex-end", gap: 8, marginBottom: 10 }}>
          <button
            onClick={handleExpandAll}
            className="btn btn-default btn-sm"
            style={{ fontSize: 11, padding: "4px 8px" }}
          >
            Expand All
          </button>
          <button
            onClick={handleCollapseAll}
            className="btn btn-default btn-sm"
            style={{ fontSize: 11, padding: "4px 8px" }}
          >
            Collapse All
          </button>
        </div>
      )}

      {additionalCard && <div style={{ marginBottom: 14 }}>{additionalCard}</div>}

      {/* File groups */}
      {filteredGroups.map((group) => {
        const isCollapsed = !!collapsed[group.name];
        const fileCount = group.files?.length || 0;

        return (
          <div key={group.name} className="card" style={{ marginBottom: 14, overflow: "hidden", padding: 0 }}>
            {/* Header / Accordion Trigger */}
            <div
              onClick={() => toggleGroup(group.name)}
              style={{
                display: "flex",
                alignItems: "center",
                justifyContent: "space-between",
                padding: "12px 16px",
                cursor: "pointer",
                userSelect: "none",
                background: isCollapsed ? "var(--background)" : "hsl(214 20% 98%)",
                borderBottom: isCollapsed ? "none" : "1px solid var(--border)",
                transition: "background 0.2s ease",
              }}
            >
              <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                <ChevronDown
                  size={16}
                  style={{
                    transform: isCollapsed ? "rotate(-90deg)" : "rotate(0deg)",
                    transition: "transform 0.2s ease",
                    color: "var(--muted-foreground)",
                  }}
                />
                <div className="card-title" style={{ fontSize: 13, margin: 0 }}>
                  {group.name}
                </div>
                <span style={{ fontSize: 11, color: "var(--muted-foreground)", marginLeft: 4 }}>
                  ({fileCount} {fileCount === 1 ? "file" : "files"})
                </span>
              </div>
            </div>

            {/* Accordion Content */}
            {!isCollapsed && (
              <div style={{ padding: "16px" }}>
                {group.customContent && <div style={{ marginBottom: 16 }}>{group.customContent}</div>}

                {group.files && group.files.length > 0 && (
                  <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                    {group.files.map((f) => {
                      const isPlot = ["pdf", "png", "tiff", "tif", "jpeg", "jpg"].includes(f.ext.toLowerCase());
                      const isRData = ["rdata"].includes(f.ext.toLowerCase());
                      const formats = isPlot
                        ? ["pdf", "png", "tiff"]
                        : isRData
                        ? ["rdata"]
                        : ["csv", "tsv", "xlsx"];

                      return (
                        <div
                          key={f.id}
                          style={{ display: "flex", alignItems: "center", gap: 12, padding: "10px 12px",
                            background: "hsl(214 50% 98%)", border: "1px solid hsl(214 20% 89%)", borderRadius: 8, flexWrap: "wrap",
                          }}
                        >
                          {/* File extension badge/icon */}
                          <div
                            style={{ width: 36, height: 36, borderRadius: 6, background: "hsl(214 50% 94%)", border: "1px solid hsl(214 30% 82%)",
                              display: "flex", alignItems: "center", justifyContent: "center", fontSize: 9, fontWeight: 700, color: "hsl(214 55% 38%)",
                              textTransform: "uppercase", flexShrink: 0,
                            }}
                          >
                            {f.ext}
                          </div>

                          {/* Info */}
                          <div style={{ flex: 1, minWidth: 200 }}>
                            <div style={{ fontSize: 13, fontWeight: 600, color: "hsl(220 15% 22%)", marginBottom: 2 }}>
                              {f.label}
                            </div>
                            <div style={{ fontSize: 11, color: "hsl(220 9% 52%)" }}>{f.desc}</div>
                          </div>

                          {/* Download Buttons */}
                          <div style={{ display: "flex", gap: 6, flexWrap: "wrap" }}>
                            {formats.map((fmt) => {
                              const key = `${f.id}_${fmt.toLowerCase()}`;
                              const isDone = downloaded.includes(key);
                              const isLoader = loadingFile === key;

                              // Generate test IDs
                              const testIds = f.getTestIds
                                ? f.getTestIds(fmt)
                                : (() => {
                                    const t = [`btn-download-${f.id}-${fmt}`, `btn-download-${f.id}-${fmt.toUpperCase()}`];
                                    if (fmt === f.ext.toLowerCase()) {
                                      t.push(`btn-download-${f.id}`);
                                    }
                                    return t;
                                  })();

                              return (
                                <button
                                  key={fmt} onClick={() => downloadFile(f.id, fmt)} className="btn btn-sm btn-default"
                                  style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px",}}
                                  data-testid={testIds.join(" ")}
                                  disabled={loadingFile !== null}
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
                      );
                    })}
                  </div>
                )}
              </div>
            )}
          </div>
        );
      })}

      {/* Actions */}
      <div style={{ display: "flex", justifyContent: "flex-end", alignItems: "center", gap: 8, marginTop: 12 }}>
        {onBack && (
          <button className="btn btn-default" onClick={onBack} data-testid="btn-export-back">
            {backLabel}
          </button>
        )}
        {onReset && (
          <button
            className="btn btn-primary"
            onClick={() => setShowRestartModal(true)}
            data-testid={resetTestId ? `${resetTestId} btn-export-reset` : "btn-export-reset"}
          >
            {resetLabel}
          </button>
        )}
      </div>
    </div>
  );
}
