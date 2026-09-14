import { useLocation } from "wouter";
import { MonitorCog, BarChart2, Search, ChartNoAxesCombined } from "lucide-react";
import PlotFootnote from "../components/shared/PlotFootnote";
import { useAppStore } from "../store/appStore";
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

export function HomeElectron() {
  const [, navigate] = useLocation();
  const { dispatch } = useAppStore();

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
    </div>
  );
}
