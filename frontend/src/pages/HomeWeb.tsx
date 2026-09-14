import { useLocation } from "wouter";
import { MonitorCog, BarChart2, Search, ChartNoAxesCombined, Download, Package, Terminal, X, ExternalLink } from "lucide-react";
import { useState, useRef, useEffect } from "react";
import PlotFootnote from "../components/shared/PlotFootnote";
import { useAppStore } from "../store/appStore";
import { SERVER_URL } from "../lib/api";
import ReloadButton from "../components/shared/ReloadButton";


const modules = [
  {
    id: "feature-selection",
    href: "/feature-selection",
    icon: Search,
    label: "Feature Selection",
    description: "Apply supervised ML models (Logistic, SVM, RF, GBM) to identify the most informative features.",
    bg: "#eef2ff",
    border: "#fde68a",
    accent: "#f59e0b",
  },
  {
    id: "de-analysis",
    href: "/de-analysis",
    icon: BarChart2,
    label: "Differential Expression Analysis",
    description: "Identify differentially expressed genes between conditions. Supports multi-dataset meta-analysis with volcano and MA plots.",
    bg: "#eef2ff",
    border: "#a7f3d0",
    accent: "#10b981",
  },
  {
    id: "enrichment",
    href: "/enrichment",
    icon: ChartNoAxesCombined,
    label: "Enrichment Analysis",
    description: "Perform ORA and GSEA on differential expression results using GO, KEGG, Reactome, and MSigDB gene sets.",
    bg: "#eef2ff",
    border: "#e9d5ff",
    accent: "#a855f7",
  },
  {
    id: "data-processing",
    href: "/data-processing",
    icon: MonitorCog,
    label: "Data Processing",
    description: "Upload expression data, annotate features, apply quality filtering and normalization across multiple datasets.",
    bg: "#eef2ff",
    border: "#c4d1f5",
    accent: "#7835ff",
  },
];

interface DownloadPlatform {
  name: string;
  url: string;
  filename: string;
}

interface DownloadOption {
  id: string;
  label: string;
  sublabel: string;
  icon: typeof MonitorCog;
  accent: string;
  bg: string;
  border: string;
  tag: string;
  tagColor: string;
  tagBg: string;
  description: string;
  steps: string[];
  zipUrl?: string;
  zipFilename?: string;
  downloads?: DownloadPlatform[];
  //guideUrl: string;
  //guideLabel: string;
}

// ── Download options config ────────────────────────────────────────────────────
const DOWNLOAD_OPTIONS: DownloadOption[] = [
  {
    id: "electron",
    label: "Option A — Desktop App",
    sublabel: "Recommended · Standalone Installer",
    icon: MonitorCog,
    accent: "#7835ff",
    bg: "#f5f3ff",
    border: "#ddd6fe",
    tag: "Recommended",
    tagColor: "#7835ff",
    tagBg: "#ede9fe",
    description: "Standalone desktop application with built-in portable R. Best user experience.",
    steps: [
      "Download the EasyOmiFun installer for your platform (.exe / .dmg / .AppImage)",
      "Launches instantly from your desktop shortcut or applications folder",
      "Runs fully offline with all dependencies self-contained",
    ],
    downloads: [
      { name: "Windows (.zip)", url: `${import.meta.env.BASE_URL}downloads/EasyOmiFun-Setup-1.0.0.zip`, filename: "EasyOmiFun-Setup-1.0.0.zip" },
      //{ name: "macOS (.dmg)", url: `${import.meta.env.BASE_URL}downloads/EasyOmiFun-1.0.0.dmg`, filename: "EasyOmiFun-1.0.0.dmg" },
      //{ name: "Linux (.AppImage)", url: `${import.meta.env.BASE_URL}downloads/EasyOmiFun-1.0.0.AppImage`, filename: "EasyOmiFun-1.0.0.AppImage" },
    ],
    //guideUrl: "/DEVELOPMENT_AND_DISTRIBUTION.md",
    //guideLabel: "View docs",
  },
];

// ── Download Panel ─────────────────────────────────────────────────────────────
function DownloadPanel({ onClose }: { onClose: () => void }) {
  const panelRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleKey = (e: KeyboardEvent) => { if (e.key === "Escape") onClose(); };
    window.addEventListener("keydown", handleKey);
    return () => window.removeEventListener("keydown", handleKey);
  }, [onClose]);

  useEffect(() => {
    const handleClick = (e: MouseEvent) => {
      if (panelRef.current && !panelRef.current.contains(e.target as Node)) onClose();
    };
    // slight delay so the button click that opened it doesn't immediately close it
    const timer = setTimeout(() => document.addEventListener("mousedown", handleClick), 50);
    return () => { clearTimeout(timer); document.removeEventListener("mousedown", handleClick); };
  }, [onClose]);

  return (
    <div style={{
      position: "fixed", inset: 0, zIndex: 1000,
      background: "rgba(10, 16, 40, 0.45)",
      backdropFilter: "blur(4px)",
      display: "flex", alignItems: "center", justifyContent: "center",
      padding: "24px",
      animation: "fadeIn 0.15s ease-out",
    }}>
      <div
        ref={panelRef}
        style={{
          background: "#ffffff",
          borderRadius: 20,
          boxShadow: "0 32px 80px rgba(10, 16, 40, 0.22)",
          width: "100%",
          maxWidth: 680,
          maxHeight: "100vh",
          display: "flex",
          flexDirection: "column",
          overflow: "hidden",
          animation: "slideUp 0.2s ease-out",
        }}
      >
        {/* Header - Stays fixed at top */}
        <div style={{
          padding: "28px 32px 20px",
          borderBottom: "1px solid #e8edf5",
          display: "flex", alignItems: "flex-start", justifyContent: "space-between",
          flexShrink: 0,
        }}>
          <div>
            <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 6 }}>
              <div style={{
                width: 36, height: 36, borderRadius: 10,
                background: "linear-gradient(135deg, #7835ff, #289ddf)",
                display: "flex", alignItems: "center", justifyContent: "center",
              }}>
                <Download size={18} color="#fff" />
              </div>
              <h2 style={{ fontSize: 20, fontWeight: 700, color: "#101a36", margin: 0 }}>
                Run EasyOmiFun Locally
              </h2>
            </div>
          </div>
          <button
            onClick={onClose}
            style={{
              width: 32, height: 32, borderRadius: 8, border: "1px solid #e2e8f0",
              background: "#f8fafc", cursor: "pointer",
              display: "flex", alignItems: "center", justifyContent: "center",
              color: "#64748b", transition: "all .15s",
              flexShrink: 0,
            }}
            onMouseEnter={e => { (e.currentTarget as HTMLButtonElement).style.background = "#f1f5f9"; }}
            onMouseLeave={e => { (e.currentTarget as HTMLButtonElement).style.background = "#f8fafc"; }}
          >
            <X size={16} />
          </button>
        </div>

        {/* Options - Scrollable area */}
        <div style={{
          padding: "24px 32px",
          display: "flex",
          flexDirection: "column",
          gap: 16,
          flex: 1,
          overflowY: "auto",
        }}>
          {DOWNLOAD_OPTIONS.map(opt => {
            const Icon = opt.icon;
            return (
              <div
                key={opt.id}
                style={{
                  border: `1.5px solid ${opt.border}`,
                  borderRadius: 14,
                  background: opt.bg,
                  padding: "20px 22px",
                  transition: "box-shadow .15s, border-color .15s",
                }}
                onMouseEnter={e => {
                  (e.currentTarget as HTMLDivElement).style.boxShadow = `0 6px 20px ${opt.accent}22`;
                  (e.currentTarget as HTMLDivElement).style.borderColor = opt.accent;
                }}
                onMouseLeave={e => {
                  (e.currentTarget as HTMLDivElement).style.boxShadow = "none";
                  (e.currentTarget as HTMLDivElement).style.borderColor = opt.border;
                }}
              >
                {/* Option header row */}
                <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 12 }}>
                  <div style={{
                    width: 38, height: 38, borderRadius: 10,
                    background: opt.accent + "18",
                    display: "flex", alignItems: "center", justifyContent: "center",
                    flexShrink: 0,
                  }}>
                    <Icon size={20} color={opt.accent} />
                  </div>
                  <div style={{ flex: 1 }}>
                    <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                      <span style={{ fontSize: 15, fontWeight: 700, color: "#101a36" }}>{opt.label}</span>
                      <span style={{
                        fontSize: 10, fontWeight: 700, textTransform: "uppercase",
                        letterSpacing: ".06em",
                        color: opt.tagColor, background: opt.tagBg,
                        padding: "2px 7px", borderRadius: 6,
                      }}>{opt.tag}</span>
                    </div>
                    <div style={{ fontSize: 12, color: "#606c80", marginTop: 1 }}>{opt.sublabel}</div>
                  </div>
                </div>

                <p style={{ fontSize: 13, color: "#4a5568", margin: "0 0 12px", lineHeight: 1.5 }}>
                  {opt.description}
                </p>

                {/* Steps */}
                <div style={{ display: "flex", flexDirection: "column", gap: 5, marginBottom: 16 }}>
                  {opt.steps.map((step, i) => (
                    <div key={i} style={{ display: "flex", alignItems: "flex-start", gap: 10, fontSize: 13 }}>
                      <span style={{
                        minWidth: 20, height: 20, borderRadius: 6,
                        background: opt.accent + "20",
                        color: opt.accent,
                        fontSize: 11, fontWeight: 700,
                        display: "flex", alignItems: "center", justifyContent: "center",
                        flexShrink: 0, marginTop: 1,
                      }}>{i + 1}</span>
                      <span style={{ color: "#374151", lineHeight: 1.5 }}>
                        {step.includes(":") ? (
                          <>
                            {step.split(": ")[0]}:{" "}
                            <code style={{
                              background: "#101a3615", padding: "1px 6px",
                              borderRadius: 4, fontSize: 12, fontFamily: "monospace",
                            }}>
                              {step.split(": ")[1]}
                            </code>
                          </>
                        ) : step}
                      </span>
                    </div>
                  ))}
                </div>

                {/* Action row: Download button + guide link */}
                <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
                  {/* Multi platform downloads if configured */}
                  {opt.downloads ? (
                    opt.downloads.map((dl, idx) => (
                      <a
                        key={idx}
                        href={dl.url}
                        download={dl.filename}
                        style={{
                          display: "inline-flex", alignItems: "center", gap: 7,
                          fontSize: 13, fontWeight: 700,
                          color: "#ffffff",
                          textDecoration: "none",
                          padding: "8px 18px",
                          borderRadius: 8,
                          background: opt.accent,
                          boxShadow: `0 3px 10px ${opt.accent}40`,
                          transition: "all .15s",
                        }}
                        onMouseEnter={e => {
                          const el = e.currentTarget as HTMLAnchorElement;
                          el.style.filter = "brightness(1.1)";
                          el.style.boxShadow = `0 5px 16px ${opt.accent}55`;
                        }}
                        onMouseLeave={e => {
                          const el = e.currentTarget as HTMLAnchorElement;
                          el.style.filter = "brightness(1)";
                          el.style.boxShadow = `0 3px 10px ${opt.accent}40`;
                        }}
                      >
                        <Download size={14} />
                        {dl.name}
                      </a>
                    ))
                  ) : (
                    opt.zipUrl && (
                      <a
                        href={opt.zipUrl}
                        download={opt.zipFilename}
                        style={{
                          display: "inline-flex", alignItems: "center", gap: 7,
                          fontSize: 13, fontWeight: 700,
                          color: "#ffffff",
                          textDecoration: "none",
                          padding: "8px 18px",
                          borderRadius: 8,
                          background: opt.accent,
                          boxShadow: `0 3px 10px ${opt.accent}40`,
                          transition: "all .15s",
                        }}
                        onMouseEnter={e => {
                          const el = e.currentTarget as HTMLAnchorElement;
                          el.style.filter = "brightness(1.1)";
                          el.style.boxShadow = `0 5px 16px ${opt.accent}55`;
                        }}
                        onMouseLeave={e => {
                          const el = e.currentTarget as HTMLAnchorElement;
                          el.style.filter = "brightness(1)";
                          el.style.boxShadow = `0 3px 10px ${opt.accent}40`;
                        }}
                      >
                        <Download size={14} />
                        Download ZIP
                      </a>
                    )
                  )}

                  {/* Secondary: View guide in new tab */}
                  {/*
                  <a
                    href={opt.guideUrl}
                    target="_blank"
                    rel="noopener noreferrer"
                    style={{
                      display: "inline-flex", alignItems: "center", gap: 5,
                      fontSize: 12, fontWeight: 500,
                      color: opt.accent,
                      textDecoration: "none",
                      padding: "8px 12px",
                      borderRadius: 8,
                      border: `1px solid ${opt.accent}30`,
                      background: opt.accent + "0c",
                      transition: "background .15s",
                    }}
                    onMouseEnter={e => { (e.currentTarget as HTMLAnchorElement).style.background = opt.accent + "18"; }}
                    onMouseLeave={e => { (e.currentTarget as HTMLAnchorElement).style.background = opt.accent + "0c"; }}
                  >
                    <ExternalLink size={12} />
                    {opt.guideLabel}
                  </a>
                  */}
                </div>

              </div>
            );
          })}
        </div>

        {/* Footer note - Stays fixed at bottom */}
        <div style={{
          padding: "14px 32px 22px",
          borderTop: "1px solid #e8edf5",
          fontSize: 12, color: "#94a3b8",
          textAlign: "center",
          flexShrink: 0,
        }}>
          All three options run fully offline after setup · No data leaves your computer
        </div>
      </div>
    </div>
  );
}

export function HomeWeb() {
  const [, navigate] = useLocation();
  const { dispatch } = useAppStore();
  const [showDownload, setShowDownload] = useState(false);

  return (
    <div style={{ minHeight: "100vh", background: "#eaefff", display: "flex", flexDirection: "column" }}>
      {/* Keyframe animations */}
      <style>{`
        @keyframes fadeIn { from { opacity: 0 } to { opacity: 1 } }
        @keyframes slideUp { from { opacity: 0; transform: translateY(16px) } to { opacity: 1; transform: translateY(0) } }
      `}</style>

      {/* Top ribbon */}
      <div className="ribbon">
        <div className="ribbon-logo" style={{ display: "flex", alignItems: "center", gap: "10px" }}>
          <img
            src={`${import.meta.env.BASE_URL}favicon.svg`}
            alt="EasyOmiFun Logo"
            style={{ width: "32px", height: "32px", objectFit: "contain" }}
          />
          <span
            className="ribbon-app-name"
            style={{
              fontSize: "16px",
              fontWeight: "700",
              letterSpacing: "-0.01em",
              display: "inline-block",
              background: "linear-gradient(90deg, #7835ff 0%, #3b82f6 50%, #289ddf 100%)",
              WebkitBackgroundClip: "text",
              WebkitTextFillColor: "transparent",
              backgroundClip: "text"
            }}
          >
            EasyOmiFun
          </span>
        </div>
        <div style={{ flex: 1 }} />

        {/* Download button in ribbon */}
        <button
          id="btn-download-local"
          onClick={() => setShowDownload(true)}
          style={{
            display: "flex", alignItems: "center", gap: 7,
            fontSize: 13, fontWeight: 600,
            color: "#7835ff",
            background: "#f0eaff",
            border: "1px solid #c4aaff",
            borderRadius: 8,
            padding: "7px 14px",
            cursor: "pointer",
            transition: "all .15s",
          }}
          onMouseEnter={e => {
            const el = e.currentTarget as HTMLButtonElement;
            el.style.background = "#e4d9ff";
            el.style.borderColor = "#a882ff";
          }}
          onMouseLeave={e => {
            const el = e.currentTarget as HTMLButtonElement;
            el.style.background = "#f0eaff";
            el.style.borderColor = "#c4aaff";
          }}
        >
          <Download size={14} />
          Run Locally
        </button>
        <ReloadButton />
      </div>

      {/* Hero */}
      <div style={{
        background: "#ffffff",
        padding: "60px 28px 48px",
        textAlign: "center",
        borderBottom: "1px solid #c4d1f5",
      }}>
        <br /><br />
        <h1 style={{
          fontSize: 48,
          fontWeight: 800,
          display: "inline-block",
          background: "linear-gradient(90deg, #7835ff 0%, #3b82f6 50%, #289ddf 100%)",
          WebkitBackgroundClip: "text",
          WebkitTextFillColor: "transparent",
          backgroundClip: "text",
          marginBottom: 16,
          lineHeight: 1.2,
          letterSpacing: "-0.02em"
        }}>
          EasyOmiFun
        </h1>
        <p style={{ fontSize: 18, color: "#606c80", maxWidth: 1500, margin: "0 auto", lineHeight: 1.6 }}>
          An end-to-end platform for processing transcriptomics data to Differential Expression analysis to Enrichment analysis.
          <br />Supports integration of multiple datasets and ML adaptation.
          <br />
        </p>

        {/* Hero download CTA */}
        <div style={{ marginTop: 28, display: "flex", justifyContent: "center" }}>
          <button
            id="btn-download-hero"
            onClick={() => setShowDownload(true)}
            style={{
              display: "inline-flex", alignItems: "center", gap: 9,
              fontSize: 15, fontWeight: 700,
              color: "#ffffff",
              background: "linear-gradient(135deg, #7835ff, #289ddf)",
              border: "none",
              borderRadius: 12,
              padding: "13px 26px",
              cursor: "pointer",
              boxShadow: "0 6px 20px rgba(120, 53, 255, 0.30)",
              transition: "all .2s",
            }}
            onMouseEnter={e => {
              const el = e.currentTarget as HTMLButtonElement;
              el.style.transform = "translateY(-2px)";
              el.style.boxShadow = "0 10px 28px rgba(120, 53, 255, 0.40)";
            }}
            onMouseLeave={e => {
              const el = e.currentTarget as HTMLButtonElement;
              el.style.transform = "translateY(0)";
              el.style.boxShadow = "0 6px 20px rgba(120, 53, 255, 0.30)";
            }}
          >
            <Download size={17} />
            Download & Run Locally
          </button>
        </div>
      </div>

      {/* Module cards */}
      <div style={{ flex: 1, padding: "40px 28px", background: "#eaefff" }}>
        <div style={{ maxWidth: 1100, margin: "0 auto" }}>
          <p style={{ fontSize: 12, fontWeight: 700, textTransform: "uppercase", letterSpacing: ".08em", color: "#606c80", marginBottom: 20 }}>
            Analysis Modules
          </p>
          <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fill, minmax(260px, 1fr))", gap: 20 }}>
            {modules.map((mod) => {
              const Icon = mod.icon;
              return (
                <div
                  key={mod.id}
                  data-testid={`card-module-${mod.id}`}
                  onClick={() => {
                    dispatch({ type: "RESET_STORE", pipelineMode: mod.id === "data-processing" });
                    navigate(mod.href);
                  }}
                  style={{
                    background: "#ffffff",
                    border: "1px solid #c4d1f5",
                    borderRadius: 12,
                    padding: "24px",
                    cursor: "pointer",
                    transition: "all .2s ease-in-out",
                    height: 220,
                    boxSizing: "border-box",
                    position: "relative",
                    display: "flex",
                    flexDirection: "column",
                    alignItems: "center"
                  }}
                  onMouseEnter={e => {
                    (e.currentTarget as HTMLDivElement).style.borderColor = "#7835ff";
                    (e.currentTarget as HTMLDivElement).style.transform = "translateY(-4px)";
                    (e.currentTarget as HTMLDivElement).style.boxShadow = "0 12px 24px rgba(120, 53, 255, 0.12)";
                  }}
                  onMouseLeave={e => {
                    (e.currentTarget as HTMLDivElement).style.borderColor = "#c4d1f5";
                    (e.currentTarget as HTMLDivElement).style.transform = "translateY(0)";
                    (e.currentTarget as HTMLDivElement).style.boxShadow = "none";
                  }}
                >
                  <div style={{
                    width: 70,
                    height: 70,
                    borderRadius: 12,
                    background: mod.bg,
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                    position: "absolute",
                    top: "35%",
                    left: "50%",
                    transform: "translate(-50%, -50%)"
                  }}>
                    <Icon size={35} color={mod.accent} />
                  </div>

                  <h3 style={{
                    fontSize: 16,
                    fontWeight: 600,
                    color: "#101a36",
                    lineHeight: 1.3,
                    textAlign: "center",
                    width: "calc(100% - 48px)",
                    position: "absolute",
                    top: 130
                  }}>
                    {mod.label}
                  </h3>
                </div>
              );
            })}
          </div>
        </div>
      </div>

      {/* Page Footer Footnote Ribbon */}
      <PlotFootnote style={{ borderRadius: "0px", marginTop: "0px", borderTop: "1px solid rgba(255, 255, 255, 0.1)" }} />

      {/* Download modal */}
      {showDownload && <DownloadPanel onClose={() => setShowDownload(false)} />}
    </div>
  );
}
