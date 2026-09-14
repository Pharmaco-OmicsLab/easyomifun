import { useState, useEffect } from "react";
import { useLocation } from "wouter";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import type { EnrichmentDb, GSEADb, EnrichmentMethod } from "../../dataObject";
import { runEnrichmentAnalysisAPI, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

const ORGS = [
  "Human (Homo sapiens)",
  "Mouse (Mus musculus)",
  "Rat (Rattus norvegicus)",
  "Pig (Sus scrofa)",
  "Chicken (Gallus gallus)",
  "Shrimp (Penaeus monodon)"
];

const getValidRankColumns = (columns: string[], geneIdCol: string) => {
  return (columns || []).filter(c => {
    if (c === geneIdCol) return false;
    const lower = c.toLowerCase();
    const hasPVal = lower.includes("p") && (lower.includes("val") || lower.includes("value"));
    const hasQVal = lower.includes("q") && (lower.includes("val") || lower.includes("value"));
    const hasFDR = lower.includes("fdr");
    const hasDirection = lower.includes("direction");
    return !(hasPVal || hasQVal || hasFDR || hasDirection);
  });
};

interface OraRow {
  Description: string;
  Hits_count: number;
  Total_input_gene: number;
  GeneRatio: string;
  Pvalue: number;
  "P.adjust": number;
  FeaturesID: string;
  direction?: string;
  database?: string;
}

interface GseaRow {
  Description: string;
  setSize: number;
  Hits: number;
  enrichmentScore: number;
  NES: number;
  pvalue: number;
  "p.adjust": number;
  core_enrichment: string;
  database?: string;
}

export default function EnrichmentAnalysisStep() {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const { toast } = useToast();
  const enConfig = state.enConfig; 
  const [loading, setLoading] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const done = state.standaloneEnDone;
  const submittedCfg = state.enSubmittedConfig;

  const selectedMethods: EnrichmentMethod[] = Array.isArray(enConfig.methods)
    ? enConfig.methods
    : [];

  // Guarantee precise EnrichmentDb[] typing from state
  const currentOraDbSelection: EnrichmentDb[] = Array.isArray(enConfig.oraDatabase)
    ? (enConfig.oraDatabase as EnrichmentDb[])
    : typeof enConfig.oraDatabase === "string" && enConfig.oraDatabase
    ? [enConfig.oraDatabase as EnrichmentDb]
    : [];

  // Guarantee precise GSEADb[] typing from state
  const currentGseaDbSelection: GSEADb[] = Array.isArray(enConfig.gseaDatabase)
    ? (enConfig.gseaDatabase as GSEADb[])
    : typeof enConfig.gseaDatabase === "string" && enConfig.gseaDatabase
    ? [enConfig.gseaDatabase as GSEADb]
    : [];

  // Check method and database selections
  const isOraSelected = selectedMethods.includes("ora");
  const isGseaSelected = selectedMethods.includes("gsea");
  const isNoMethodSelected = selectedMethods.length === 0;
  const isNoDbSelected = (isOraSelected && currentOraDbSelection.length === 0) ||
                         (isGseaSelected && currentGseaDbSelection.length === 0);

  // Check missing rank column for GSEA
  const isGseaMissingRankCol = isGseaSelected && !enConfig.rankByCol;

  // Aggregate invalid condition flag
  const isInvalidConfig = isNoMethodSelected || isNoDbSelected || isGseaMissingRankCol;

  // ─── Frozen result snapshots ─────────────────────────────────────────────
  const cacheKey = state.enDatasets[0]?.id || "default";
  const cachedEnResults = state.enResultsCache[cacheKey];
  const [snapshotOraRows, setSnapshotOraRows] = useState<OraRow[]>(cachedEnResults?.oraResults || []);
  const [snapshotGseaRows, setSnapshotGseaRows] = useState<GseaRow[]>(cachedEnResults?.gseaResults || []);
  const [snapshotMethods, setSnapshotMethods] = useState<EnrichmentMethod[]>(selectedMethods);

  // Track configuration adjustments made after a successful analysis calculation
  const hasSettingsChanged = done && submittedCfg && (
    JSON.stringify(submittedCfg.methods) !== JSON.stringify(selectedMethods) ||
    JSON.stringify(submittedCfg.oraDatabase) !== JSON.stringify(currentOraDbSelection) ||
    JSON.stringify(submittedCfg.gseaDatabase) !== JSON.stringify(currentGseaDbSelection) ||
    submittedCfg.pValueCutoff !== enConfig.pValueCutoff ||
    submittedCfg.qValueCutoff !== enConfig.qValueCutoff ||
    submittedCfg.minGeneSetSize !== enConfig.minGeneSetSize ||
    submittedCfg.maxGeneSetSize !== enConfig.maxGeneSetSize ||
    submittedCfg.organism !== enConfig.organism ||
    submittedCfg.rankByCol !== enConfig.rankByCol
  );

  // Rank column options — derived from uploaded datasets
  const firstDs = state.enDatasets[0];
  const rankColumnOptions = firstDs
    ? getValidRankColumns(firstDs.columns || [], firstDs.geneIdCol || "")
    : [];

  const [showWarnModal, setShowWarnModal] = useState(false);

  const updateConfig = (patch: Partial<typeof enConfig>) => {
    dispatch({ type: "EN_SET_CONFIG", patch });
  };

  const handleToggleMethod = (m: EnrichmentMethod) => {
    const next: EnrichmentMethod[] = selectedMethods.includes(m)
      ? selectedMethods.filter(x => x !== m)
      : [...selectedMethods, m];
    updateConfig({ methods: next });
  };

  const handleToggleOraDatabase = (dbVal: EnrichmentDb) => {
    const next: EnrichmentDb[] = currentOraDbSelection.includes(dbVal)
      ? currentOraDbSelection.filter(v => v !== dbVal)
      : [...currentOraDbSelection, dbVal];
    updateConfig({ oraDatabase: next });
  };

  const handleToggleGseaDatabase = (dbVal: GSEADb | "MSigDB_C5") => {
    const typedVal = dbVal as GSEADb;
    const next: GSEADb[] = currentGseaDbSelection.includes(typedVal)
      ? currentGseaDbSelection.filter(v => v !== typedVal)
      : [...currentGseaDbSelection, typedVal];
    updateConfig({ gseaDatabase: next });
  };

  const handleRun = async () => {
    if (isInvalidConfig) return; 
    setLoading(true);
    setLoadingMsg(`Running ${selectedMethods.map(m => m.toUpperCase()).join(" + ")} analysis…`);
    if (done) {
      try {
        await redoStepAPI("ea", state.enDatasets.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    try {
      const datasetId = state.enDatasets[0]?.id;
      const results = await runEnrichmentAnalysisAPI(enConfig, datasetId);
      setSnapshotOraRows(results.oraResults || []);
      setSnapshotGseaRows(results.gseaResults || []);
      setSnapshotMethods([...selectedMethods]);
      dispatch({ type: "EN_SET_RESULTS", id: cacheKey, results: { oraResults: results.oraResults || [], gseaResults: results.gseaResults || [] } });
      dispatch({ type: "EN_SUBMIT_CONFIG" });
      dispatch({ type: "SET_STANDALONE_EN_DONE", done: true });
      setShowWarnModal(false);
    } catch (err: any) {
      console.error("Enrichment analysis error:", err);
      toast({
        title: "Enrichment Analysis Error",
        description: err.message || "Failed to execute enrichment analysis.",
        variant: "destructive"
      });
    } finally {
      setLoading(false);
    }
  };

  const handleContinue = () => {
    if (isInvalidConfig) return;
    if (hasSettingsChanged) {
      setShowWarnModal(true);
    } else {
      proceedToNext();
    }
  };

  const handleConfirmBypass = () => {
    setShowWarnModal(false);
    proceedToNext();
  };

  const proceedToNext = () => {
    dispatch({ type: "EN_SET_STEP", step: "export" });
  };

  const ORGS_CONST = ORGS; 

  const ORA_DATABASES: { val: EnrichmentDb; label: string }[] = [
    { val: "KEGG" as EnrichmentDb, label: "KEGG Pathways" },
    { val: "GO:BP" as EnrichmentDb, label: "Gene Ontology — Biological Process (GO:BP)" },
    { val: "GO:MF" as EnrichmentDb, label: "Gene Ontology — Molecular Function (GO:MF)" },
    { val: "GO:CC" as EnrichmentDb, label: "Gene Ontology — Cellular Component (GO:CC)" },
    { val: "REACTOME" as EnrichmentDb, label: "Reactome Pathways" },
  ];

  const GSEA_DATABASES: { val: GSEADb; label: string }[] = [
    { val: "KEGG" as GSEADb, label: "KEGG Pathways" },
    { val: "GO:BP" as GSEADb, label: "Gene Ontology — Biological Process (GO:BP)" },
    { val: "GO:MF" as GSEADb, label: "Gene Ontology — Molecular Function (GO:MF)" },
    { val: "GO:CC" as GSEADb, label: "Gene Ontology — Cellular Component (GO:CC)" },
    { val: "REACTOME" as GSEADb, label: "Reactome Pathways" },
    { val: "MSigDB_H" as GSEADb, label: "MSigDB Hallmark" },
  ];

  const activeOraRows = Array.isArray(snapshotOraRows) ? snapshotOraRows : [];
  const activeGseaRows = Array.isArray(snapshotGseaRows) ? snapshotGseaRows : [];

  const currentOrg = enConfig.organism || "Human (Homo sapiens)";
  const getOrganismKey = (orgString: string) => {
    const lower = orgString.toLowerCase();
    if (lower.includes("human")) return "human";
    if (lower.includes("mouse")) return "mouse";
    if (lower.includes("shrimp")) return "shrimp";
    return "other";
  };
  const orgKey = getOrganismKey(currentOrg);

  const filteredOraDatabases = ORA_DATABASES.filter(db => {
    if (db.val === "MSigDB_H") {
      return orgKey === "human" || orgKey === "mouse";
    }
    if (db.val === "REACTOME") {
      return orgKey !== "shrimp";
    }
    return true;
  });

  const filteredGseaDatabases = GSEA_DATABASES.filter(db => {
    if (db.val === "MSigDB_H") {
      return orgKey === "human" || orgKey === "mouse";
    }
    if (db.val === "REACTOME") {
      return orgKey !== "shrimp";
    }
    return true;
  });

  return (
    <>
      {loading && <Spinner label={loadingMsg} sublabel="Please wait…" />}

      {showWarnModal && (
        <ChangeWarnModal 
          onConfirm={handleConfirmBypass} 
          onCancel={() => setShowWarnModal(false)} 
        />
      )}

      {/* Method Selection card */}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 12 }}>Enrichment Methods (Select multiple)</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 10, marginBottom: 14 }}>
          {[
            { id: "gsea" as const, icon: "📈", label: "GSEA", desc: "Gene Set Enrichment Analysis — ranked-based enrichment using all genes" },
            { id: "ora" as const, icon: "🔬", label: "ORA", desc: "Over-Representation Analysis — tests if gene sets are enriched in a list of significant genes" },
          ].map(m => {
            const isChosen = selectedMethods.includes(m.id);
            return (
              <div
                key={m.id}
                onClick={() => handleToggleMethod(m.id)}
                style={{
                  padding: "12px 14px", borderRadius: 8, cursor: "pointer",
                  border: `2px solid ${isChosen ? "var(--primary)" : "var(--border)"}`,
                  background: isChosen ? "var(--selected-bg)" : "white",
                  transition: "all .15s",
                  position: "relative"
                }}
                data-testid={`card-enmethod-${m.id}`}
              >
                {isChosen && (
                  <span style={{ position: "absolute", top: 8, right: 8, fontSize: 12, color: "var(--primary)", fontWeight: 700 }}>✓</span>
                )}
                <div style={{ fontSize: 18, marginBottom: 6 }}>{m.icon}</div>
                <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 4 }}>{m.label}</div>
                <div style={{ fontSize: 11, color: "var(--muted-foreground)", lineHeight: 1.5 }}>{m.desc}</div>
              </div>
            );
          })}
        </div>

        {/* Organism + Rank Col */}
        <div style={{ display: "flex", gap: 14, flexWrap: "wrap", alignItems: "flex-start" }}>
          <div style={{ display: "flex", flexDirection: "column", minWidth: 240 }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Select Organism</label>
            <details style={{ width: 250, position: "relative" }} data-testid="details-organism">
              <summary style={{
                display: "flex", justifyContent: "space-between", alignItems: "center",
                padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
              }}>
                <span>{enConfig.organism || "Human (Homo sapiens)"}</span>
                <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
              </summary>

              <div style={{
                position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
              }}>
                {ORGS_CONST.map(g => (
                  <div
                    key={g}
                    onClick={(e) => {
                      updateConfig({ organism: g });
                      const details = (e.target as HTMLElement).closest("details");
                      if (details) details.removeAttribute("open");
                    }}
                    style={{
                      padding: "8px 10px",
                      fontSize: 12,
                      cursor: "pointer",
                      background: (enConfig.organism || "Human (Homo sapiens)") === g ? "var(--selected-bg)" : "transparent",
                      fontWeight: (enConfig.organism || "Human (Homo sapiens)") === g ? 600 : 400
                    }}
                    onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                    onMouseLeave={e => e.currentTarget.style.background = (enConfig.organism || "Human (Homo sapiens)") === g ? "var(--selected-bg)" : "transparent"}
                  >
                    {g}
                  </div>
                ))}
              </div>
            </details>
          </div>

          {isGseaSelected && (
            <div style={{ display: "flex", flexDirection: "column", minWidth: 200 }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Rank genes by column</label>
              <details style={{ width: 200, position: "relative" }} data-testid="details-rank-col">
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", 
                  border: `1px solid ${isGseaMissingRankCol ? "hsl(0 84% 60%)" : "var(--border)"}`, 
                  borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{enConfig.rankByCol || "Select column..."}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>
                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {rankColumnOptions.length > 0 ? rankColumnOptions.map(c => (
                    <div
                      key={c}
                      onClick={(e) => {
                        updateConfig({ rankByCol: c });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px", fontSize: 12, cursor: "pointer",
                        background: enConfig.rankByCol === c ? "var(--selected-bg)" : "transparent",
                        fontWeight: enConfig.rankByCol === c ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = enConfig.rankByCol === c ? "var(--selected-bg)" : "transparent"}
                    >
                      {c}
                    </div>
                  )) : (
                    <div style={{ padding: "8px 10px", fontSize: 12, color: "var(--muted-foreground)" }}>Upload a dataset first</div>
                  )}
                </div>
              </details>
              {isGseaMissingRankCol && (
                <div style={{ fontSize: 11, color: "hsl(0 84% 60%)", marginTop: 4, fontWeight: 500 }}>
                  ⚠️ Rank column is required for GSEA
                </div>
              )}
            </div>
          )}
        </div>

        {isNoMethodSelected && (
          <div className="banner warning" style={{ background: "hsl(38 90% 95%)", color: "hsl(38 80% 25%)", border: "1px solid hsl(38 70% 80%)", padding: 10, borderRadius: 6, marginTop: 12, fontSize: 12 }}>
            ⚠️ Please select at least one analysis method.
          </div>
        )}
      </div>

      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 14 }}>
          Analysis Configuration
        </div>

        <div style={{ display: "grid", gridTemplateColumns: (isOraSelected && isGseaSelected) ? "1fr 1fr" : "1fr", gap: 20 }}>
          {/* GSEA Databases */}
          {isGseaSelected && (
            <div style={{ marginBottom: 14 }}>
              <label style={{ fontSize: 13, fontWeight: 500, color: "hsl(220 15% 25%)", display: "block", marginBottom: 8 }}>
                GSEA Gene Set Databases (Select multiple)
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {filteredGseaDatabases.map(db => {
                  const isChecked = currentGseaDbSelection.includes(db.val as GSEADb);
                  return (
                    <label key={db.val} className={`radio-option ${isChecked ? "selected" : ""}`}>
                      <input
                        type="checkbox"
                        checked={isChecked}
                        onChange={() => handleToggleGseaDatabase(db.val)}
                      />
                      <div style={{ fontWeight: 500 }}>{db.label}</div>
                    </label>
                  );
                })}
              </div>
              {isGseaSelected && currentGseaDbSelection.length === 0 && (
                <div className="banner warning" style={{ background: "hsl(38 90% 95%)", color: "hsl(38 80% 25%)", border: "1px solid hsl(38 70% 80%)", padding: 10, borderRadius: 6, marginTop: 8, fontSize: 12 }}>
                  ⚠️ Please select at least one GSEA database.
                </div>
              )}
            </div>
          )}

          {/* ORA Databases */}
          {isOraSelected && (
            <div style={{ marginBottom: 14 }}>
              <label style={{ fontSize: 13, fontWeight: 500, color: "hsl(220 15% 25%)", display: "block", marginBottom: 8 }}>
                ORA Gene Set Databases (Select multiple)
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {filteredOraDatabases.map(db => {
                  const isChecked = currentOraDbSelection.includes(db.val);
                  return (
                    <label key={db.val} className={`radio-option ${isChecked ? "selected" : ""}`}>
                      <input
                        type="checkbox"
                        checked={isChecked}
                        onChange={() => handleToggleOraDatabase(db.val)}
                      />
                      <div style={{ fontWeight: 500 }}>{db.label}</div>
                    </label>
                  );
                })}
              </div>
              {isOraSelected && currentOraDbSelection.length === 0 && (
                <div className="banner warning" style={{ background: "hsl(38 90% 95%)", color: "hsl(38 80% 25%)", border: "1px solid hsl(38 70% 80%)", padding: 10, borderRadius: 6, marginTop: 8, fontSize: 12 }}>
                  ⚠️ Please select at least one ORA database.
                </div>
              )}
            </div>
          )}
        </div>

        {/* Settings */}
        <hr className="card-divider" />
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
          {/* p-value cutoff */}
          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>p-value cutoff</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Filter out insignificant enriched results having p-value larger than user-input
            </div>
            <input
              type="number" step="0.01" min="0" max="1"
              value={enConfig.pValueCutoff}
              onChange={e => updateConfig({ pValueCutoff: parseFloat(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          {/* q-value cutoff */}
          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>q-value cutoff</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Apply Multiple Testing to remove False Positives where q-value larger than user-input
            </div>
            <input
              type="number" step="0.05" min="0" max="1"
              value={enConfig.qValueCutoff}
              onChange={e => updateConfig({ qValueCutoff: parseFloat(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          {/* Min gene set size */}
          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>Min gene set size</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Excludes small gene set size pathways. Keeps only clear biological results
            </div>
            <input
              type="number" min="1"
              value={enConfig.minGeneSetSize}
              onChange={e => updateConfig({ minGeneSetSize: parseInt(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          {/* Max gene set size */}
          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>Max gene set size</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Excludes enriched results with large gene set size that are too broad to be interpreted
            </div>
            <input
              type="number" min="10"
              value={enConfig.maxGeneSetSize}
              onChange={e => updateConfig({ maxGeneSetSize: parseInt(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>
        </div>
      </div>

      {/* ORA Results Card */}
      {done && snapshotMethods.includes("ora") && (
        <div className="card" style={{ marginBottom: 14, opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 14 }}>
            <div className="card-title">
              ORA Results
              {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated — settings changed)</span>}
            </div>
            <span style={{ fontSize: 11, fontWeight: 600, padding: "3px 10px", borderRadius: 4, background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--primary)" }}>
              {activeOraRows.length} terms
            </span>
          </div>
          {activeOraRows.length > 0 ? (
            <div style={{ overflowX: "auto" }}>
              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                <thead>
                  <tr style={{ background: "var(--neutral-bg)" }}>
                    <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Description</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Hits Count</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Total Input Gene</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Gene Ratio</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Pvalue</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>P.adjust</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Features ID</th>
                  </tr>
                </thead>
                <tbody>
                  {activeOraRows.slice(0, 10).map((r, i) => (
                    <tr key={i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                      <td style={{ padding: "8px 10px", fontSize: 12 }}>
                        {r.Description}
                        {r.direction && (
                          <span style={{
                            fontSize: 10,
                            fontWeight: 600,
                            padding: "1px 5px",
                            borderRadius: 4,
                            marginLeft: 8,
                            background: r.direction.toLowerCase() === "up" ? "hsl(0 60% 95%)" : r.direction.toLowerCase() === "down" ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                            color: r.direction.toLowerCase() === "up" ? "hsl(0 65% 38%)" : r.direction.toLowerCase() === "down" ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                            display: "inline-block",
                            verticalAlign: "middle"
                          }}>
                            {r.direction.toLowerCase() === "up" ? "↑ Up" : r.direction.toLowerCase() === "down" ? "↓ Down" : "Not Significant"}
                          </span>
                        )}
                      </td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Hits_count}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Total_input_gene}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600 }}>{r.GeneRatio}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}><span style={{ fontWeight: 600 }}>{typeof r.Pvalue === "number" ? r.Pvalue.toFixed(4) : r.Pvalue}</span></td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}><span style={{ fontWeight: 600 }}>{typeof r["P.adjust"] === "number" ? r["P.adjust"].toFixed(4) : r["P.adjust"]}</span></td>
                      <td style={{ padding: "8px 10px", textAlign: "center", maxWidth: 200, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={r.FeaturesID}>{r.FeaturesID}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <div style={{ fontSize: 13, color: "var(--muted-foreground)", textAlign: "center", padding: "20px 0" }}>No significant ORA terms found.</div>
          )}
        </div>
      )}

      {/* GSEA Results Card */}
      {done && snapshotMethods.includes("gsea") && (
        <div className="card" style={{ marginBottom: 14, opacity: hasSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 14 }}>
            <div className="card-title">
              GSEA Results
              {hasSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated — settings changed)</span>}
            </div>
            <span style={{ fontSize: 11, fontWeight: 600, padding: "3px 10px", borderRadius: 4, background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--primary)" }}>
              {activeGseaRows.length} gene sets
            </span>
          </div>
          {activeGseaRows.length > 0 ? (
            <div style={{ overflowX: "auto" }}>
              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                <thead>
                  <tr style={{ background: "var(--neutral-bg)" }}>
                    <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Description</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>setSize</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Hits</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>enrichmentScore</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>NES</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>pvalue</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>p.adjust</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>core_enrichment</th>
                  </tr>
                </thead>
                <tbody>
                  {activeGseaRows.slice(0, 10).map((r, i) => (
                    <tr key={i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                      <td style={{ padding: "8px 10px", fontSize: 12, fontWeight: 500 }}>{r.Description}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.setSize}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Hits}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r.enrichmentScore === "number" ? r.enrichmentScore.toFixed(3) : r.enrichmentScore}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>
                        <span style={{ fontWeight: 700, fontSize: 13, color: r.NES > 0 ? "hsl(150 55% 30%)" : "hsl(0 65% 40%)" }}>
                          {r.NES > 0 ? "+" : ""}{typeof r.NES === "number" ? r.NES.toFixed(2) : r.NES}
                        </span>
                      </td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r.pvalue === "number" ? r.pvalue.toFixed(3) : r.pvalue}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r["p.adjust"] === "number" ? r["p.adjust"].toFixed(3) : r["p.adjust"]}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center", maxWidth: 200, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={r.core_enrichment}>{r.core_enrichment}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <div style={{ fontSize: 13, color: "var(--muted-foreground)", textAlign: "center", padding: "20px 0" }}>No significant GSEA gene sets found.</div>
          )}
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button
            className="btn btn-default"
            onClick={() => {
              dispatch({ type: "EN_SET_STEP", step: "upload" });
            }}
            data-testid="btn-en-back"
          >
            ← Back
          </button>
        </div>

        {hasSettingsChanged ? (
          <div style={{ display: "flex", gap: 8 }}>
            <button
              className="btn btn-default"
              onClick={handleRun}
              disabled={isInvalidConfig}
              style={{
                borderColor: "hsl(38 70% 72%)",
                color: "hsl(32 90% 35%)",
                background: isInvalidConfig ? "hsl(39, 100%, 95%)" : "hsl(38 90% 95%)",
                fontWeight: 600,
                cursor: isInvalidConfig ? "not-allowed" : "pointer",
                opacity: isInvalidConfig ? 0.6 : 1,
              }}
              data-testid="btn-en-rerun"
            >
              🔄 Re-run Analysis
            </button>
            <button
              className="btn btn-primary"
              onClick={handleContinue}
              disabled={isInvalidConfig}
              style={{
                cursor: isInvalidConfig ? "not-allowed" : "pointer",
                opacity: isInvalidConfig ? 0.6 : 1,
              }}
              data-testid="btn-en-continue-export"
            >
              Continue to Export →
            </button>
          </div>
        ) : !done ? (
          <button
            className="btn btn-primary"
            onClick={handleRun}
            disabled={isInvalidConfig}
            style={{
              cursor: isInvalidConfig ? "not-allowed" : "pointer",
              opacity: isInvalidConfig ? 0.6 : 1,
            }}
            data-testid="btn-en-run"
          >
            Run {selectedMethods.map(m => m.toUpperCase()).join(" + ")} Analysis
          </button>
        ) : (
          <button
            className="btn btn-primary"
            onClick={handleContinue}
            disabled={isInvalidConfig}
            style={{
              cursor: isInvalidConfig ? "not-allowed" : "pointer",
              opacity: isInvalidConfig ? 0.6 : 1,
            }}
            data-testid="btn-en-continue-export"
          >
            Continue to Export →
          </button>
        )}
      </div>
    </>
  );
}