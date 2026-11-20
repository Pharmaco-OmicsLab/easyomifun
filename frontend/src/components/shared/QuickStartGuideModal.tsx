import { useEffect, useState } from "react";
import {
  BookOpen,
  X,
  MonitorCog,
  BarChart2,
  ChartNoAxesCombined,
  Search,
  Table2,
  Cpu,
  FileSpreadsheet,
  CheckCircle2,
  Layers,
} from "lucide-react";

export type GuideModuleId = "data-processing" | "de-analysis" | "enrichment" | "feature-selection";

interface QuickStartGuideModalProps {
  onClose: () => void;
  initialModuleId?: GuideModuleId;
}

interface ModuleGuideData {
  id: GuideModuleId;
  title: string;
  badge: string;
  badgeBg: string;
  badgeColor: string;
  icon: typeof MonitorCog;
  summary: string;
  capabilities: string[];
  underTheHood: {
    title: string;
    description: string;
  }[];
  dataFormats: {
    name: string;
    required: boolean;
    description: string;
    columns: { header: string; type: string; example: string; note: string }[];
    mockRows: Record<string, string>[];
  }[];
}

const MODULE_GUIDES: Record<GuideModuleId, ModuleGuideData> = {
  "data-processing": {
    id: "data-processing",
    title: "Data Processing (DP)",
    badge: "Integrated Analysis Suite",
    badgeBg: "#dbeafe",
    badgeColor: "#1e40af",
    icon: MonitorCog,
    summary:
      "A complete pipeline for multi-cohort transcriptomics & multi-omics data ingestion, clinical annotation, quality control, missing value imputation, cross-cohort batch effect correction, and automated downstream analysis integration.",
    capabilities: [
      "Multi-cohort dataset ingestion (CSV, TSV, TXT, XLSX exports)",
      "Quality Control (QC): Missingness filtering, zero-count removal, low-variance gene exclusion",
      "Normalisation & variance stabilization (log2 transformation, quantile scaling)",
      "ComBat Empirical Bayes cross-batch harmonisation",
      "Principal Component Analysis (PCA) 2D diagnostic projections",
      "Seamless inline handoff to Inline DE, Inline EA, and Inline FS submodules without file re-uploading",
    ],
    underTheHood: [
      {
        title: "Log2 Transformation & Variance Stabilization",
        description:
          "High-throughput count or intensity data is stabilized via y = log2(x + 1), suppressing heteroscedasticity and bringing feature distributions closer to normality for downstream statistical modeling.",
      },
      {
        title: "ComBat Empirical Bayes Harmonisation",
        description:
          "Removes technical batch discrepancies across independent cohorts while strictly preserving biological differences. ComBat estimates additive (location) and multiplicative (scale) batch parameters using empirical Bayes shrinkage, pooling information across all genes to avoid over-fitting even in small sample sizes.",
      },
      {
        title: "QC & Singular Value Decomposition (PCA)",
        description:
          "Computes sample covariance matrices and performs singular value decomposition (SVD). Pre- and post-correction PCA scatter plots quantify the proportion of total variance explained by biological conditions versus batch covariates.",
      },
      {
        title: "Downstream Submodule Orchestration",
        description:
          "Harmonized expression matrices and synchronized clinical metadata are held in an isolated state slice, allowing instant execution of Inline DE, Inline Meta-Analysis, Inline EA, and Inline FS without session collisions.",
      },
    ],
    dataFormats: [
      {
        name: "1. Expression Matrix (Required)",
        required: true,
        description:
          "Tabular file of gene/feature expression values. Rows correspond to genes/probes, and columns represent sample IDs (samples-in-rows is also automatically detected).",
        columns: [
          { header: "Feature_ID", type: "Text (Monospace)", example: "TP53, ENSG00000141510", note: "First column: Unique gene symbol or identifier" },
          { header: "Sample_01 ... Sample_N", type: "Numeric", example: "8.42, 12.15, 4.88", note: "Expression values (raw counts or log2 intensities)" },
        ],
        mockRows: [
          { Feature_ID: "TP53", Sample_01: "8.42", Sample_02: "8.91", Sample_03: "5.12", Sample_04: "5.34", Sample_05: "8.77" },
          { Feature_ID: "EGFR", Sample_01: "12.15", Sample_02: "11.80", Sample_03: "9.04", Sample_04: "9.22", Sample_05: "11.95" },
          { Feature_ID: "BRCA1", Sample_01: "4.88", Sample_02: "5.01", Sample_03: "7.33", Sample_04: "7.15", Sample_05: "4.92" },
          { Feature_ID: "MYC", Sample_01: "9.73", Sample_02: "10.02", Sample_03: "6.45", Sample_04: "6.81", Sample_05: "9.90" },
        ],
      },
      {
        name: "2. Clinical & Phenotype Metadata (Required for Grouping & Batch Correction)",
        required: false,
        description:
          "Tabular file containing phenotype annotations, biological comparison conditions, and technical covariates. Sample identifiers must strictly correspond to column headers in the expression matrix.",
        columns: [
          { header: "SampleID", type: "Text (Monospace)", example: "Sample_01", note: "Must match sample columns in expression matrix exactly" },
          { header: "Group", type: "Categorical", example: "Tumor / Normal", note: "Biological condition or clinical phenotype" },
          { header: "Batch", type: "Categorical (Optional)", example: "Cohort_A / Cohort_B", note: "Required if performing ComBat batch correction" },
          { header: "Age / Sex", type: "Numeric / Text (Optional)", example: "58 / Female", note: "Clinical covariates for subgroup modeling" },
        ],
        mockRows: [
          { SampleID: "Sample_01", Group: "Tumor", Batch: "Cohort_A", Age: "58", Sex: "Female" },
          { SampleID: "Sample_02", Group: "Tumor", Batch: "Cohort_A", Age: "62", Sex: "Male" },
          { SampleID: "Sample_03", Group: "Normal", Batch: "Cohort_B", Age: "55", Sex: "Female" },
          { SampleID: "Sample_04", Group: "Normal", Batch: "Cohort_B", Age: "64", Sex: "Male" },
        ],
      },
    ],
  },
  "de-analysis": {
    id: "de-analysis",
    title: "DE Analysis",
    badge: "Independent Analysis Suite",
    badgeBg: "#e0f2fe",
    badgeColor: "#0369a1",
    icon: BarChart2,
    summary:
      "A rigorous statistical testing suite to discover significantly perturbed genes between biological conditions across single cohorts, perform multi-cohort inverse-variance meta-analysis, and trigger inline pathway enrichment.",
    capabilities: [
      "Two-condition differential expression testing (limma-style linear models & Welch's t-test)",
      "Benjamini-Hochberg (BH) False Discovery Rate (FDR) / Adjusted p-value correction",
      "Interactive Volcano plots and MA plots with user-defined log2FC and p-value cutoffs",
      "Multi-cohort meta-analysis: Fixed-effects and DerSimonian-Laird random-effects pooling",
      "Cross-study heterogeneity testing (Cochran's Q and I² statistics)",
      "Integrated Inline EA submodule to immediately interrogate biological mechanisms of DEGs",
    ],
    underTheHood: [
      {
        title: "Two-Sample Statistical Modeling",
        description:
          "Compares group means using Welch's t-test (robust against unequal variances) or empirical Bayes moderated linear regression. For each feature, computes test statistics, standard errors, and raw two-tailed p-values.",
      },
      {
        title: "Benjamini-Hochberg False Discovery Rate (FDR)",
        description:
          "To control the expected proportion of false discoveries across 20,000+ parallel hypothesis tests, raw p-values are ranked and adjusted via q = min_{k >= i} { (m * p_(k)) / k }. Users can filter strictly by adjusted p-value (q < 0.05).",
      },
      {
        title: "Inverse-Variance Weighted Meta-Analysis",
        description:
          "Integrates effect sizes across multiple independent cohorts. In the DerSimonian-Laird random-effects model, inter-study variance (tau^2) is calculated alongside intra-study variance, producing robust pooled effect sizes and forest plot statistics even when study platforms differ.",
      },
      {
        title: "Inline Enrichment Connectivity",
        description:
          "Filtered significant up-regulated and down-regulated DEGs are automatically dispatched into an isolated Inline EA runner, preventing manual re-entry of gene identifiers.",
      },
    ],
    dataFormats: [
      {
        name: "1. Normalized Expression Matrix (Required)",
        required: true,
        description:
          "Pre-normalized expression values (log2 intensities or normalized counts).",
        columns: [
          { header: "Gene_Symbol", type: "Text (Monospace)", example: "TP53, EGFR", note: "Unique gene or transcript identifier" },
          { header: "Sample Columns", type: "Numeric", example: "8.42, 11.80", note: "Continuous normalized expression values" },
        ],
        mockRows: [
          { Gene_Symbol: "TP53", S01_Case: "8.42", S02_Case: "8.91", S03_Control: "5.12", S04_Control: "5.34" },
          { Gene_Symbol: "EGFR", S01_Case: "12.15", S02_Case: "11.80", S03_Control: "9.04", S04_Control: "9.22" },
          { Gene_Symbol: "BRCA1", S01_Case: "4.88", S02_Case: "5.01", S03_Control: "7.33", S04_Control: "7.15" },
        ],
      },
      {
        name: "2. Sample Condition Annotations (Required)",
        required: true,
        description:
          "Defines sample identifiers and their comparison groups (e.g. Treated vs Control, Tumor vs Normal).",
        columns: [
          { header: "SampleID", type: "Text (Monospace)", example: "S01_Case", note: "Matches sample column names" },
          { header: "Condition / Group", type: "Categorical (2 levels)", example: "Case / Control", note: "Comparison groups" },
          { header: "Cohort / Dataset (Meta-analysis)", type: "Categorical (Optional)", example: "GSE10072", note: "Required for multi-cohort meta-analysis" },
        ],
        mockRows: [
          { SampleID: "S01_Case", Condition: "Case", Cohort: "GSE10072" },
          { SampleID: "S02_Case", Condition: "Case", Cohort: "GSE10072" },
          { SampleID: "S03_Control", Condition: "Control", Cohort: "GSE10072" },
          { SampleID: "S04_Control", Condition: "Control", Cohort: "GSE10072" },
        ],
      },
    ],
  },
  enrichment: {
    id: "enrichment",
    title: "Enrichment Analysis (EA)",
    badge: "Standalone Workbench",
    badgeBg: "#ede9fe",
    badgeColor: "#7c3aed",
    icon: ChartNoAxesCombined,
    summary:
      "A dedicated standalone biological pathway interpretation platform to uncover functional mechanisms, signaling pathways, and gene ontology categories from custom gene lists or pre-computed ranking metrics.",
    capabilities: [
      "Over-Representation Analysis (ORA) with Fisher's Exact / Hypergeometric testing",
      "Gene Set Enrichment Analysis (GSEA) with weighted Kolmogorov-Smirnov running sums",
      "Comprehensive pathway collections: KEGG, Reactome, GO (BP, MF, CC), MSigDB Hallmarks",
      "Support for custom user-uploaded GMT pathway gene sets",
      "Interactive dot plots, bar charts, running-sum mountain plots, and enrichment tables",
      "Direct copy-paste gene list input or CSV/TSV table uploads",
    ],
    underTheHood: [
      {
        title: "Over-Representation Analysis (ORA)",
        description:
          "Tests whether an input list of candidate genes contains significantly more members of a specific pathway than expected by random chance. Evaluated using a one-tailed Fisher's exact / hypergeometric test against the total genomic background size, followed by Benjamini-Hochberg FDR adjustment.",
      },
      {
        title: "Gene Set Enrichment Analysis (GSEA)",
        description:
          "Takes a continuous ranked list of all measured genes (sorted by log2FC or signed test statistic). Calculates an Enrichment Score (ES) by walking down the ranked list, increasing a running-sum statistic when a gene belongs to the gene set and decreasing it when not. Empirical p-values are computed by gene-label permutations.",
      },
      {
        title: "Curated & Custom GMT Gene Sets",
        description:
          "Integrates pre-indexed gene sets from KEGG, Reactome, and Gene Ontology. Custom .gmt files can also be supplied where each row defines a pathway name, description, and list of gene symbols.",
      },
    ],
    dataFormats: [
      {
        name: "Option A: Gene List for ORA (Single-column or pasted text)",
        required: true,
        description:
          "A simple list of gene symbols (official HGNC gene names, e.g. TP53, MYC, EGFR), one per line or in a single CSV column.",
        columns: [
          { header: "Gene_Symbol", type: "Text (Monospace)", example: "TP53, EGFR, MYC, VEGFA", note: "Recognized gene symbols" },
        ],
        mockRows: [
          { Gene_Symbol: "TP53" },
          { Gene_Symbol: "EGFR" },
          { Gene_Symbol: "MYC" },
          { Gene_Symbol: "VEGFA" },
          { Gene_Symbol: "CDKN2A" },
        ],
      },
      {
        name: "Option B: Ranked Statistics Table for GSEA",
        required: true,
        description:
          "A 2-column tabular file containing gene symbols and a continuous ranking statistic (e.g. log2 Fold Change, Wald statistic, or signed -log10 p-value).",
        columns: [
          { header: "Gene", type: "Text (Monospace)", example: "MYC", note: "Gene symbol" },
          { header: "Score / log2FC", type: "Numeric", example: "2.45, -1.82", note: "Continuous ranking metric (positive = up, negative = down)" },
        ],
        mockRows: [
          { Gene: "MYC", "Score / log2FC": "2.45" },
          { Gene: "CDK4", "Score / log2FC": "1.89" },
          { Gene: "VEGFA", "Score / log2FC": "1.42" },
          { Gene: "CDKN2A", "Score / log2FC": "-2.10" },
          { Gene: "PTEN", "Score / log2FC": "-2.65" },
        ],
      },
    ],
  },
  "feature-selection": {
    id: "feature-selection",
    title: "Feature Selection (FS)",
    badge: "Standalone Workbench",
    badgeBg: "#fef3c7",
    badgeColor: "#b45309",
    icon: Search,
    summary:
      "A high-dimensional machine learning biomarker discovery workbench designed to isolate sparse, clinically predictive multi-gene signatures across 6 distinct ML classifiers with cross-validation and ROC evaluation.",
    capabilities: [
      "6 Supervised ML Architectures: STABL, Boruta, Random Forest, GBM, SVM, and Logistic Regression",
      "STABL (Stability Selection): Synthetic multi-sample subsampling to control False Discovery Rates",
      "Boruta: All-relevant feature selection using shuffled 'shadow features' and binomial distributions",
      "Repeated Stratified K-Fold Cross-Validation and External Holdout validation splits",
      "Comprehensive ML metrics: ROC-AUC curves, Precision-Recall curves, Confusion Matrix, F1-score",
      "Biomarker signature frequency rankings and VIP (Variable Importance in Projection) plots",
    ],
    underTheHood: [
      {
        title: "STABL (Stability Selection)",
        description:
          "Combines subsampling with Lasso or Elastic-Net regularization across hundreds of synthetic iterations. Features are retained only if their selection frequency exceeds a rigorous stability threshold, controlling the per-family error rate in ultra-high dimensional omics data.",
      },
      {
        title: "Boruta All-Relevant Selection",
        description:
          "Augments the dataset by generating random permuted copies ('shadow features') for all original genes. An ensemble classifier is trained, and features scoring significantly higher importance than the best shadow feature (via two-sided binomial tests) are confirmed as biologically relevant.",
      },
      {
        title: "Tree & Linear Classifiers (RF, GBM, SVM, LogReg)",
        description:
          "Compares non-linear tree ensembles (Random Forest with Mean Decrease Impurity, Gradient Boosting with learning rate decay) against margin-maximizing classifiers (SVM with RBF kernel) and L1/L2 penalized Logistic Regression.",
      },
      {
        title: "Validation Architecture",
        description:
          "Stratified K-fold cross-validation preserves minority clinical class proportions across folds. Performance is evaluated via out-of-fold predictions to prevent data leakage and provide unbiased generalization estimates.",
      },
    ],
    dataFormats: [
      {
        name: "1. Expression Matrix (Required)",
        required: true,
        description:
          "Normalized feature expression matrix (samples in columns, genes in rows; or samples in rows).",
        columns: [
          { header: "Feature_ID", type: "Text (Monospace)", example: "TP53, EGFR", note: "Gene or biomarker identifier" },
          { header: "Sample Columns", type: "Numeric", example: "8.42, 11.80", note: "Continuous feature values" },
        ],
        mockRows: [
          { Feature_ID: "TP53", S01: "8.42", S02: "8.91", S03: "5.12", S04: "5.34" },
          { Feature_ID: "EGFR", S01: "12.15", S02: "11.80", S03: "9.04", S04: "9.22" },
          { Feature_ID: "BRCA1", S01: "4.88", S02: "5.01", S03: "7.33", S04: "7.15" },
          { Feature_ID: "MYC", S01: "9.73", S02: "10.02", S03: "6.45", S04: "6.81" },
        ],
      },
      {
        name: "2. Clinical Outcome Annotation (Required)",
        required: true,
        description:
          "Clinical response or binary classification outcome column matching sample identifiers.",
        columns: [
          { header: "SampleID", type: "Text (Monospace)", example: "S01, S02", note: "Matches sample names in expression matrix" },
          { header: "Outcome / Target", type: "Binary / Categorical", example: "Responder / Non-Responder", note: "Classification target for ML models" },
        ],
        mockRows: [
          { SampleID: "S01", Outcome: "Responder" },
          { SampleID: "S02", Outcome: "Responder" },
          { SampleID: "S03", Outcome: "Non-Responder" },
          { SampleID: "S04", Outcome: "Non-Responder" },
        ],
      },
    ],
  },
};

export default function QuickStartGuideModal({
  onClose,
  initialModuleId = "data-processing",
}: QuickStartGuideModalProps) {
  const [activeTab, setActiveTab] = useState<GuideModuleId>(initialModuleId);

  useEffect(() => {
    const handleKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    window.addEventListener("keydown", handleKey);
    return () => window.removeEventListener("keydown", handleKey);
  }, [onClose]);

  const activeData = MODULE_GUIDES[activeTab];
  const Icon = activeData.icon;

  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-labelledby="guide-modal-title"
      style={{
        position: "fixed",
        inset: 0,
        zIndex: 1000,
        background: "rgba(15, 23, 42, 0.65)",
        backdropFilter: "blur(5px)",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: "20px",
        animation: "fadeIn 0.15s ease-out",
      }}
      onClick={onClose}
    >
      <div
        onClick={(e) => e.stopPropagation()}
        style={{
          background: "#ffffff",
          borderRadius: 16,
          boxShadow: "0 25px 50px -12px rgba(15, 23, 42, 0.3)",
          width: "100%",
          maxWidth: 920,
          maxHeight: "92vh",
          display: "flex",
          flexDirection: "column",
          overflow: "hidden",
          border: "1px solid #cbd5e1",
          animation: "slideUp 0.2s ease-out",
        }}
      >
        {/* Header */}
        <div
          style={{
            padding: "18px 24px",
            borderBottom: "1px solid #e2e8f0",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            background: "#f8fafc",
          }}
        >
          <div style={{ display: "flex", alignItems: "center", gap: 12 }}>
            <div
              style={{
                width: 38,
                height: 38,
                borderRadius: 10,
                background: "#dbeafe",
                color: "#1e40af",
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
              }}
            >
              <BookOpen size={20} />
            </div>
            <div>
              <h2 id="guide-modal-title" style={{ fontSize: 18, fontWeight: 700, color: "#0f172a", margin: 0 }}>
                Data Format Guide & Methodological Architecture
              </h2>
              <p style={{ fontSize: 12, color: "#64748b", margin: "2px 0 0 0" }}>
                Accepted input schemas, mock tables, and under-the-hood statistical modeling for each module
              </p>
            </div>
          </div>
          <button
            type="button"
            onClick={onClose}
            aria-label="Close Guide"
            style={{
              width: 32,
              height: 32,
              borderRadius: 6,
              border: "1px solid #cbd5e1",
              background: "#ffffff",
              cursor: "pointer",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              color: "#64748b",
              transition: "all 0.15s ease",
            }}
            onMouseEnter={(e) => {
              e.currentTarget.style.color = "#0f172a";
              e.currentTarget.style.borderColor = "#94a3b8";
            }}
            onMouseLeave={(e) => {
              e.currentTarget.style.color = "#64748b";
              e.currentTarget.style.borderColor = "#cbd5e1";
            }}
          >
            <X size={16} />
          </button>
        </div>

        {/* Tab Ribbon for the 4 Independent Modules */}
        <div
          style={{
            display: "flex",
            alignItems: "center",
            gap: 8,
            padding: "10px 24px",
            background: "#ffffff",
            borderBottom: "1px solid #e2e8f0",
            overflowX: "auto",
          }}
        >
          {(Object.keys(MODULE_GUIDES) as GuideModuleId[]).map((modId) => {
            const mod = MODULE_GUIDES[modId];
            const TabIcon = mod.icon;
            const isActive = activeTab === modId;
            return (
              <button
                key={modId}
                type="button"
                onClick={() => setActiveTab(modId)}
                style={{
                  display: "flex",
                  alignItems: "center",
                  gap: 8,
                  padding: "8px 14px",
                  borderRadius: 8,
                  fontSize: 13,
                  fontWeight: isActive ? 700 : 500,
                  color: isActive ? "#1e40af" : "#475569",
                  background: isActive ? "#eff6ff" : "transparent",
                  border: isActive ? "1px solid #bfdbfe" : "1px solid transparent",
                  cursor: "pointer",
                  transition: "all 0.15s ease",
                  whiteSpace: "nowrap",
                }}
              >
                <TabIcon size={16} color={isActive ? "#1e40af" : "#64748b"} />
                <span>{mod.title}</span>
              </button>
            );
          })}
        </div>

        {/* Modal Scrollable Body */}
        <div style={{ padding: "24px", overflowY: "auto", display: "flex", flexDirection: "column", gap: 24 }}>
          {/* Module Banner */}
          <div
            style={{
              border: "1px solid #e2e8f0",
              borderRadius: 12,
              padding: "18px 20px",
              background: "#f8fafc",
              display: "flex",
              flexDirection: "column",
              gap: 12,
            }}
          >
            <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", flexWrap: "wrap", gap: 10 }}>
              <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
                <div
                  style={{
                    width: 36,
                    height: 36,
                    borderRadius: 8,
                    background: activeData.badgeBg,
                    color: activeData.badgeColor,
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                  }}
                >
                  <Icon size={20} />
                </div>
                <div>
                  <h3 style={{ fontSize: 17, fontWeight: 700, color: "#0f172a", margin: 0 }}>
                    {activeData.title}
                  </h3>
                  <span
                    style={{
                      fontSize: 11,
                      fontWeight: 700,
                      background: activeData.badgeBg,
                      color: activeData.badgeColor,
                      padding: "2px 8px",
                      borderRadius: 9999,
                      marginTop: 4,
                      display: "inline-block",
                    }}
                  >
                    {activeData.badge}
                  </span>
                </div>
              </div>
            </div>
            <p style={{ fontSize: 13, color: "#334155", lineHeight: 1.6, margin: 0 }}>
              {activeData.summary}
            </p>

            {/* Core Capabilities */}
            <div style={{ marginTop: 4 }}>
              <div style={{ fontSize: 12, fontWeight: 700, color: "#1e40af", textTransform: "uppercase", letterSpacing: ".04em", marginBottom: 6 }}>
                Key Capabilities
              </div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "#475569", lineHeight: 1.6 }}>
                {activeData.capabilities.map((cap, i) => (
                  <li key={i}>{cap}</li>
                ))}
              </ul>
            </div>
          </div>

          {/* Under The Hood: Statistical & Algorithmic Logic */}
          <div style={{ border: "1px solid #e2e8f0", borderRadius: 12, padding: "18px 20px", background: "#ffffff" }}>
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 12 }}>
              <Cpu size={18} color="#1e40af" />
              <h3 style={{ fontSize: 15, fontWeight: 700, color: "#0f172a", margin: 0 }}>
                Under the Hood: Core Statistical Logic & Methodology
              </h3>
            </div>
            <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(380px, 1fr))", gap: 14 }}>
              {activeData.underTheHood.map((item, i) => (
                <div
                  key={i}
                  style={{
                    border: "1px solid #f1f5f9",
                    background: "#f8fafc",
                    borderRadius: 8,
                    padding: "12px 14px",
                  }}
                >
                  <div style={{ fontSize: 13, fontWeight: 700, color: "#1e40af", marginBottom: 4 }}>
                    {item.title}
                  </div>
                  <p style={{ fontSize: 12, color: "#475569", lineHeight: 1.5, margin: 0 }}>
                    {item.description}
                  </p>
                </div>
              ))}
            </div>
          </div>

          {/* Input Data Format & Mock Tables */}
          <div style={{ border: "1px solid #e2e8f0", borderRadius: 12, padding: "18px 20px", background: "#ffffff" }}>
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 14 }}>
              <Table2 size={18} color="#1e40af" />
              <h3 style={{ fontSize: 15, fontWeight: 700, color: "#0f172a", margin: 0 }}>
                Accepted Data Schemas & Visual Mock Tables
              </h3>
            </div>
            <p style={{ fontSize: 13, color: "#475569", lineHeight: 1.5, margin: "0 0 16px 0" }}>
              Files should be formatted in standard <strong>CSV</strong>, <strong>TSV</strong>, or tab-delimited <strong>TXT</strong> format. Below are the required schema structures and sample rows:
            </p>

            <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>
              {activeData.dataFormats.map((fmt, i) => (
                <div
                  key={i}
                  style={{
                    border: "1px solid #cbd5e1",
                    borderRadius: 10,
                    overflow: "hidden",
                    background: "#f8fafc",
                  }}
                >
                  {/* Table Header Info */}
                  <div
                    style={{
                      padding: "10px 14px",
                      background: "#f1f5f9",
                      borderBottom: "1px solid #cbd5e1",
                      display: "flex",
                      alignItems: "center",
                      justifyContent: "space-between",
                    }}
                  >
                    <div style={{ fontSize: 13, fontWeight: 700, color: "#0f172a" }}>
                      {fmt.name}
                    </div>
                    {fmt.required ? (
                      <span style={{ fontSize: 11, fontWeight: 700, background: "#dbeafe", color: "#1e40af", padding: "2px 8px", borderRadius: 4 }}>
                        Required
                      </span>
                    ) : (
                      <span style={{ fontSize: 11, fontWeight: 600, background: "#e2e8f0", color: "#64748b", padding: "2px 8px", borderRadius: 4 }}>
                        Optional
                      </span>
                    )}
                  </div>
                  <div style={{ padding: "10px 14px", fontSize: 12, color: "#475569", borderBottom: "1px solid #e2e8f0" }}>
                    {fmt.description}
                  </div>

                  {/* Mock Table */}
                  <div style={{ overflowX: "auto" }}>
                    <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12, textAlign: "left" }}>
                      <thead>
                        <tr style={{ background: "#f8fafc", borderBottom: "1px solid #e2e8f0" }}>
                          {Object.keys(fmt.mockRows[0]).map((col) => (
                            <th
                              key={col}
                              style={{
                                padding: "8px 12px",
                                fontWeight: 700,
                                color: "#1e40af",
                                borderRight: "1px solid #e2e8f0",
                                fontFamily: "var(--font-mono)",
                              }}
                            >
                              {col}
                            </th>
                          ))}
                        </tr>
                      </thead>
                      <tbody>
                        {fmt.mockRows.map((row, rIdx) => (
                          <tr
                            key={rIdx}
                            style={{
                              borderBottom: rIdx === fmt.mockRows.length - 1 ? "none" : "1px solid #e2e8f0",
                              background: rIdx % 2 === 0 ? "#ffffff" : "#f8fafc",
                            }}
                          >
                            {Object.entries(row).map(([k, val], cIdx) => (
                              <td
                                key={cIdx}
                                style={{
                                  padding: "7px 12px",
                                  color: cIdx === 0 ? "#0f172a" : "#334155",
                                  fontWeight: cIdx === 0 ? 600 : 400,
                                  borderRight: "1px solid #e2e8f0",
                                  fontFamily: "var(--font-mono)",
                                }}
                              >
                                {val}
                              </td>
                            ))}
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>

                  {/* Schema Notes */}
                  <div style={{ padding: "8px 14px", background: "#f8fafc", borderTop: "1px solid #e2e8f0" }}>
                    <div style={{ fontSize: 11, fontWeight: 700, color: "#64748b", textTransform: "uppercase", marginBottom: 4 }}>
                      Column Details & Constraints:
                    </div>
                    <ul style={{ margin: 0, paddingLeft: 16, fontSize: 11, color: "#475569", lineHeight: 1.5 }}>
                      {fmt.columns.map((col, cIdx) => (
                        <li key={cIdx}>
                          <code>{col.header}</code>: {col.note} (<em>{col.type}</em>)
                        </li>
                      ))}
                    </ul>
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Footer */}
        <div
          style={{
            padding: "14px 24px",
            borderTop: "1px solid #e2e8f0",
            background: "#f8fafc",
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
          }}
        >
          <div style={{ fontSize: 12, color: "#64748b" }}>
            All modules operate independently with isolated data state slices.
          </div>
          <button
            type="button"
            onClick={onClose}
            style={{
              padding: "8px 20px",
              background: "#1e40af",
              color: "#ffffff",
              fontSize: 13,
              fontWeight: 600,
              borderRadius: 8,
              border: "none",
              cursor: "pointer",
              transition: "background 0.15s ease",
            }}
            onMouseEnter={(e) => (e.currentTarget.style.background = "#1d4ed8")}
            onMouseLeave={(e) => (e.currentTarget.style.background = "#1e40af")}
          >
            Got It
          </button>
        </div>
      </div>
    </div>
  );
}
