import { useLocation } from "wouter";
import { MonitorCog, BarChart2, Search, ChartNoAxesCombined, BookOpen, X } from "lucide-react";
import { useState, useEffect } from "react";
import PlotFootnote from "../components/shared/PlotFootnote";
import { useAppStore } from "../store/appStore";
import ReloadButton from "../components/shared/ReloadButton";
import QuickStartGuideModal, { type GuideModuleId } from "../components/shared/QuickStartGuideModal";

// Independent analysis modules and suites
const modules = [
  {
    id: "feature-selection",
    path: "/feature-selection",
    href: "/feature-selection",
    testId: "card-feature-selection",
    badge: "Standalone Workbench",
    badgeColor: "#b45309",
    badgeBg: "#fef3c7",
    title: "Feature Selection (FS)",
    label: "Feature Selection",
    description: "Supervised machine learning biomarker panel discovery with STABL, Boruta, RF, GBM, SVM, and Logistic Regression.",
    icon: Search,
    submodules: [
      "6 Supervised ML Classifiers",
      "K-Fold CV & External Validation",
      "ROC & Classification Metrics",
    ],
  },
  {
    id: "de-analysis",
    path: "/de-analysis",
    href: "/de-analysis",
    testId: "card-de-analysis",
    badge: "Independent Analysis Suite",
    badgeColor: "#0369a1",
    badgeBg: "#e0f2fe",
    title: "DE Analysis",
    label: "Differential Expression Analysis",
    description: "Statistical differential expression testing on pre-normalized matrices with interactive volcano plots and multi-cohort meta-analysis.",
    icon: BarChart2,
    submodules: [
      "Differential Expression Testing",
      "Multi-Cohort Meta-Analysis",
      "Inline Enrichment Analysis (EA)",
    ],
  },
  {
    id: "enrichment",
    path: "/enrichment",
    href: "/enrichment",
    testId: "card-enrichment",
    badge: "Standalone Workbench",
    badgeColor: "#7c3aed",
    badgeBg: "#ede9fe",
    title: "Enrichment Analysis (EA)",
    label: "Enrichment Analysis",
    description: "Dedicated pathway enrichment platform to perform ORA and GSEA from custom gene lists or pre-computed ranked statistics.",
    icon: ChartNoAxesCombined,
    submodules: [
      "Over-Representation Analysis (ORA)",
      "Gene Set Enrichment Analysis (GSEA)",
      "GO, KEGG, Reactome, MSigDB",
    ],
  },
  {
    id: "data-processing",
    path: "/data-processing",
    href: "/data-processing",
    testId: "card-data-processing",
    badge: "Integrated Analysis Suite",
    badgeColor: "#1e40af",
    badgeBg: "#dbeafe",
    title: "Data Processing (DP)",
    label: "Data Processing",
    description: "End-to-end data ingestion, annotation, QC filtering, batch effect correction, and cross-cohort normalization.",
    icon: MonitorCog,
    submodules: [
      "Inline DE & Meta-Analysis",
      "Inline Enrichment Analysis (EA)",
      "Inline Feature Selection (FS)",
    ],
  },
];

export function HomeElectron() {
  const [, navigate] = useLocation();
  const { dispatch } = useAppStore();
  const [showGuide, setShowGuide] = useState(false);
  const [guideModule, setGuideModule] = useState<GuideModuleId>("data-processing");

  return (
    <div style={{ minHeight: "100vh", background: "#f8fafc", display: "flex", flexDirection: "column" }}>
      {/* Keyframe animations */}
      <style>{`
        @keyframes fadeIn { from { opacity: 0 } to { opacity: 1 } }
        @keyframes slideUp { from { opacity: 0; transform: translateY(14px) } to { opacity: 1; transform: translateY(0) } }
      `}</style>

      {/* Top Bar matching @easyomicsmarker_ui/ authoritative scientific design */}
      <header
        style={{
          background: "#172554",
          borderBottom: "1px solid rgba(255, 255, 255, 0.12)",
          padding: "0 24px",
          height: 56,
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          position: "sticky",
          top: 0,
          zIndex: 100,
        }}
      >
        {/* Left: Logo, Name, Lab Pill */}
        <div style={{ display: "flex", alignItems: "center", gap: 14 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
            <img
              src={`${import.meta.env.BASE_URL}favicon.svg`}
              alt="EasyOmiFun Logo"
              style={{ width: 32, height: 32, objectFit: "contain" }}
            />
            <span
              style={{
                fontFamily: "var(--font-display)",
                fontSize: 17,
                fontWeight: 700,
                color: "#ffffff",
                letterSpacing: "-0.01em",
              }}
            >
              EasyOmiFun
            </span>
          </div>

          <div
            style={{
              background: "rgba(255, 255, 255, 0.12)",
              color: "#e2e8f0",
              fontSize: 11,
              fontWeight: 600,
              letterSpacing: "0.04em",
              padding: "3px 10px",
              borderRadius: 9999,
              textTransform: "uppercase",
              display: "inline-flex",
              alignItems: "center",
            }}
          >
            CGU · The Pharmaco-Omics Lab
          </div>
        </div>

        {/* Right: Quick-Start Guide, ReloadButton */}
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <button
            type="button"
            onClick={() => {
              setGuideModule("data-processing");
              setShowGuide(true);
            }}
            data-testid="btn-quickstart-guide"
            style={{
              display: "flex",
              alignItems: "center",
              gap: 6,
              background: "rgba(255, 255, 255, 0.1)",
              color: "#f8fafc",
              border: "1px solid rgba(255, 255, 255, 0.2)",
              borderRadius: 8,
              padding: "6px 12px",
              fontSize: 12,
              fontWeight: 600,
              cursor: "pointer",
              transition: "all 0.15s ease",
            }}
            onMouseEnter={e => {
              e.currentTarget.style.background = "rgba(255, 255, 255, 0.18)";
              e.currentTarget.style.borderColor = "rgba(255, 255, 255, 0.35)";
            }}
            onMouseLeave={e => {
              e.currentTarget.style.background = "rgba(255, 255, 255, 0.1)";
              e.currentTarget.style.borderColor = "rgba(255, 255, 255, 0.2)";
            }}
          >
            <BookOpen size={14} />
            <span style={{ display: "inline" }}>Data Format & Method Guide</span>
          </button>

          <ReloadButton />
        </div>
      </header>

      {/* Hero Banner */}
      <section
        style={{
          background: "#ffffff",
          padding: "48px 28px 42px",
          textAlign: "center",
          borderBottom: "1px solid #e2e8f0",
        }}
      >
        <div style={{ maxWidth: 860, margin: "0 auto" }}>
          {/* Logo above EasyOmiFun */}
          <div style={{ display: "flex", justifyContent: "center", marginBottom: 20 }}>
            <img
              src={`${import.meta.env.BASE_URL}favicon.svg`}
              alt="EasyOmiFun Logo"
              style={{
                width: 120,
                height: 120,
                objectFit: "contain",
                filter: "drop-shadow(0 10px 22px rgba(30, 64, 175, 0.16))",
              }}
            />
          </div>

          <h1
            style={{
              fontFamily: "var(--font-display)",
              fontSize: 44,
              fontWeight: 800,
              color: "#0f172a",
              marginBottom: 12,
              lineHeight: 1.15,
              letterSpacing: "-0.025em",
            }}
          >
            EasyOmiFun
          </h1>
          <p
            style={{
              fontSize: 17,
              color: "#475569",
              maxWidth: 740,
              margin: "0 auto",
              lineHeight: 1.6,
            }}
          >
            An interactive platform for the discovery of clinically relevant biomarkers, equipped with comprehensive multi-omics data processing, rigorous statistical modeling, and deep biological interpretation.
          </p>
        </div>
      </section>

      {/* Accessible Module Cards */}
      <main style={{ flex: 1, padding: "36px 28px 48px", background: "#f8fafc" }}>
        <div style={{ maxWidth: 1100, margin: "0 auto" }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 20 }}>
            <div>
              <p
                style={{
                  fontSize: 12,
                  fontWeight: 700,
                  textTransform: "uppercase",
                  letterSpacing: ".08em",
                  color: "#1e40af",
                  margin: 0,
                }}
              >
                Analysis Suites & Workbenches (Independent Modules)
              </p>
              <p style={{ fontSize: 13, color: "#64748b", margin: "4px 0 0 0" }}>
                Select an independent analysis module below, or launch full multi-dataset data processing with integrated downstream submodules.
              </p>
            </div>
          </div>

          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(auto-fit, minmax(250px, 1fr))",
              gap: 20,
            }}
          >
            {modules.map((mod) => {
              const Icon = mod.icon;
              return (
                <div
                  key={mod.id}
                  role="button"
                  tabIndex={0}
                  data-testid={mod.testId}
                  onClick={() => {
                    dispatch({ type: "RESET_STORE", pipelineMode: mod.id === "data-processing" });
                    navigate(mod.path);
                  }}
                  onKeyDown={(e) => {
                    if (e.key === "Enter" || e.key === " ") {
                      e.preventDefault();
                      dispatch({ type: "RESET_STORE", pipelineMode: mod.id === "data-processing" });
                      navigate(mod.path);
                    }
                  }}
                  style={{
                    background: "#ffffff",
                    border: "1px solid #cbd5e1",
                    borderRadius: 14,
                    padding: "20px",
                    cursor: "pointer",
                    transition: "all .15s ease",
                    display: "flex",
                    flexDirection: "column",
                    alignItems: "flex-start",
                    textAlign: "left",
                    minHeight: 250,
                    boxSizing: "border-box",
                    boxShadow: "0 1px 3px rgba(15, 23, 42, 0.05)",
                    position: "relative",
                  }}
                  onMouseEnter={e => {
                    e.currentTarget.style.borderColor = "#1e40af";
                    e.currentTarget.style.transform = "translateY(-3px)";
                    e.currentTarget.style.boxShadow = "0 12px 24px -6px rgba(30, 64, 175, 0.15)";
                  }}
                  onMouseLeave={e => {
                    e.currentTarget.style.borderColor = "#cbd5e1";
                    e.currentTarget.style.transform = "translateY(0)";
                    e.currentTarget.style.boxShadow = "0 1px 3px rgba(15, 23, 42, 0.05)";
                  }}
                >
                  {/* Top row: Icon + Category Badge */}
                  <div
                    style={{
                      display: "flex",
                      alignItems: "center",
                      justifyContent: "space-between",
                      width: "100%",
                      marginBottom: 14,
                    }}
                  >
                    <div
                      style={{
                        width: 40,
                        height: 40,
                        borderRadius: 10,
                        background: mod.badgeBg,
                        display: "flex",
                        alignItems: "center",
                        justifyContent: "center",
                        color: mod.badgeColor,
                      }}
                    >
                      <Icon size={20} />
                    </div>
                    <span
                      style={{
                        fontSize: 11,
                        fontWeight: 700,
                        background: mod.badgeBg,
                        color: mod.badgeColor,
                        padding: "3px 9px",
                        borderRadius: 9999,
                        letterSpacing: "0.02em",
                      }}
                    >
                      {mod.badge}
                    </span>
                  </div>

                  {/* Title */}
                  <h3
                    style={{
                      fontSize: 17,
                      fontWeight: 700,
                      color: "#0f172a",
                      margin: "0 0 6px 0",
                      lineHeight: 1.3,
                    }}
                  >
                    {mod.title}
                  </h3>

                  {/* Description */}
                  <p
                    style={{
                      fontSize: 13,
                      color: "#475569",
                      lineHeight: 1.5,
                      margin: 0,
                    }}
                  >
                    {mod.description}
                  </p>

                  {/* Submodules & Capabilities list */}
                  <div style={{ marginTop: 12, marginBottom: 14, width: "100%", borderTop: "1px solid #f1f5f9", paddingTop: 10, flex: 1 }}>
                    <div style={{ fontSize: 11, fontWeight: 700, textTransform: "uppercase", letterSpacing: ".04em", color: "#64748b", marginBottom: 6 }}>
                      {mod.id === "data-processing" || mod.id === "de-analysis" ? "Integrated Submodules" : "Core Capabilities"}
                    </div>
                    <ul style={{ listStyle: "none", margin: 0, padding: 0, display: "flex", flexDirection: "column", gap: 5 }}>
                      {mod.submodules.map((sub, i) => (
                        <li key={i} style={{ fontSize: 12, color: "#334155", display: "flex", alignItems: "center", gap: 6 }}>
                          <span style={{ color: mod.badgeColor, fontSize: 13, lineHeight: 1 }}>•</span>
                          <span>{sub}</span>
                        </li>
                      ))}
                    </ul>
                  </div>

                  {/* Bottom Action Row: Format & Guide Button + Launch Module */}
                  <div
                    style={{
                      display: "flex",
                      alignItems: "center",
                      justifyContent: "space-between",
                      width: "100%",
                      marginTop: "auto",
                      paddingTop: 8,
                    }}
                  >
                    <button
                      type="button"
                      onClick={(e) => {
                        e.stopPropagation();
                        setGuideModule(mod.id as GuideModuleId);
                        setShowGuide(true);
                      }}
                      style={{
                        display: "inline-flex",
                        alignItems: "center",
                        gap: 5,
                        fontSize: 11,
                        fontWeight: 600,
                        color: "#1e40af",
                        background: "#eff6ff",
                        border: "1px solid #bfdbfe",
                        borderRadius: 6,
                        padding: "4px 8px",
                        cursor: "pointer",
                        transition: "all 0.15s ease",
                      }}
                      onMouseEnter={e => {
                        e.currentTarget.style.background = "#dbeafe";
                        e.currentTarget.style.borderColor = "#93c5fd";
                      }}
                      onMouseLeave={e => {
                        e.currentTarget.style.background = "#eff6ff";
                        e.currentTarget.style.borderColor = "#bfdbfe";
                      }}
                    >
                      <BookOpen size={12} />
                      <span>Format & Guide</span>
                    </button>

                    <div
                      style={{
                        display: "flex",
                        alignItems: "center",
                        gap: 4,
                        color: "#1e40af",
                        fontSize: 13,
                        fontWeight: 700,
                      }}
                    >
                      <span>Launch</span>
                      <span>→</span>
                    </div>
                  </div>
                </div>
              );
            })}
          </div>
        </div>
      </main>

      {/* Page Footer */}
      <PlotFootnote />

      {/* Data Format & Method Guide modal */}
      {showGuide && (
        <QuickStartGuideModal
          onClose={() => setShowGuide(false)}
          initialModuleId={guideModule}
        />
      )}
    </div>
  );
}
