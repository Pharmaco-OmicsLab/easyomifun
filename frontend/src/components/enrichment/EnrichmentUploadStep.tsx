import { useState } from "react";
import { FileText, AlertTriangle, CheckCircle } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import { type EnrichmentConfig } from "../../dataObject";
import { parseExpressionFile, detectGeneIdType, getDesktopFilePath } from "../../lib/dataParser";
import { uploadFileInChunks } from "../../lib/chunkUploader";
import { fetchSampleDataAPI, uploadEADatasetDataAPI, clearDatasetAPI } from "../../lib/api";
import DiscardWarnModal from "../shared/DiscardWarnModal";

const GENE_ID_LABELS: Record<string, string> = {
  ensembl: "Ensembl ID",
  entrez: "Entrez ID",
  genename: "Gene Name",
};

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

export default function EnrichmentUploadStep({ datasetId }: { datasetId: string }) {
  const { state, dispatch } = useAppStore();
  const ds = state.enDatasets.find(d => d.id === datasetId);
  const enConfig = state.enConfig;
  const [loading, setLoading] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const [drag, setDrag] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  if (!ds) return null;

  const updateDs = (patch: Partial<typeof ds>) =>
    dispatch({ type: "EN_UPDATE_DATASET", id: datasetId, patch });

  const updateConfig = (patch: Partial<EnrichmentConfig>) =>
    dispatch({ type: "EN_SET_CONFIG", patch });

  const handleClear = async () => {
    try {
      await clearDatasetAPI(datasetId, "expression");
    } catch (e) {
      console.warn("clearDatasetAPI failed (non-fatal):", e);
    }
    updateDs({
      expressionFile: null,
      expressionFileName: "",
      expressionFilePath: undefined,
      expressionUploadId: undefined,
      submittedExpressionFileName: "",
      columns: [],
      geneInfoCols: [],
      geneIdCol: "",
      geneIdType: "",
      detectedGeneIdType: "",
      nFeatures: 0,
      uploadDone: false,
      parsedData: undefined,
      hasNA: false,
      integrityOk: false,
      integrityIssues: [],
    });
  };

  const handleLoadSample = async () => {
    setLoading(true);
    setLoadingMsg("Loading sample gene list…");
    try {
      const result = await fetchSampleDataAPI("gene_list_logfc");
      let rows = (result.parsedData || []) as any[];
      if (rows.length > 0 && !Array.isArray(rows[0]) && typeof rows[0] === "object" && rows[0] !== null) {
        rows = rows.map(r => result.columns.map(col => r[col] !== undefined ? String(r[col]) : ""));
      } else if (rows.length > 0 && !Array.isArray(rows[0])) {
        rows = rows.map(r => [r]);
      }
      const sampleVal = rows[0]?.[0] || "";
      const detectedId = detectGeneIdType(sampleVal);
      updateDs({
        expressionFileName: "gene_list_logfc.csv",
        columns: result.columns,
        geneInfoCols: [],
        geneIdCol: result.columns[0] || "GeneName",
        geneIdType: detectedId,
        detectedGeneIdType: detectedId,
        nFeatures: rows.length,
        integrityOk: true,
        integrityIssues: [],
        parsedData: rows,
        hasNA: false,
      });
      // Auto-detect rankByCol for GSEA (stored globally in enConfig)
      const geneIdCol = result.columns[0] || "GeneName";
      const validCols = getValidRankColumns(result.columns, geneIdCol);
      const logfcCol = validCols.find(c => c.toLowerCase() === "logfc");
      if (logfcCol) updateConfig({ rankByCol: logfcCol });
      else if (validCols.length > 0) updateConfig({ rankByCol: validCols[0] });
      else updateConfig({ rankByCol: result.columns[1] || result.columns[0] || "" });
    } catch (err: any) {
      updateDs({
        integrityOk: false,
        integrityIssues: [`Failed to load example data: ${err.message}`],
      });
    } finally {
      setLoading(false);
    }
  };

  const handleFile = async (file: File) => {
    setLoading(true);
    setLoadingMsg(`Parsing ${file.name}…`);
    try {
      const desktopPath = getDesktopFilePath(file);
      const isDesktop = !!desktopPath;

      const parsed = await parseExpressionFile(file, { previewOnly: true });
      const detectedId = parsed.detectedGeneIdType;

      let uploadId: string | undefined = undefined;
      if (!isDesktop) {
        setLoadingMsg(`Uploading ${file.name} in chunks…`);
        uploadId = await uploadFileInChunks(file, ds.id, "expression", (pct) => {
          setLoadingMsg(`Uploading ${file.name} (${pct}%)…`);
        });
      }

      updateDs({
        expressionFile: file,
        expressionFileName: file.name,
        expressionFilePath: desktopPath || undefined,
        expressionUploadId: uploadId,
        columns: parsed.columns,
        geneInfoCols: parsed.geneInfoCols,
        geneIdCol: parsed.geneIdCol || (parsed.columns[0] || ""),
        detectedGeneIdType: detectedId,
        geneIdType: detectedId,
        nFeatures: parsed.nFeatures,
        parsedData: parsed.parsedData,
        hasNA: parsed.hasNA,
        integrityOk: true,
        integrityIssues: [],
      });
      const geneIdCol = parsed.geneIdCol || (parsed.columns[0] || "");
      const validCols = getValidRankColumns(parsed.columns, geneIdCol);
      const logfcCol = validCols.find(c => c.toLowerCase() === "logfc");
      if (logfcCol) {
        updateConfig({ rankByCol: logfcCol });
      } else if (validCols.length > 0) {
        updateConfig({ rankByCol: validCols[0] });
      } else if (parsed.columns.length > 1) {
        updateConfig({ rankByCol: parsed.columns[1] });
      }
    } catch (error: any) {
      updateDs({
        integrityOk: false,
        integrityIssues: [`Error parsing file: ${error.message}`],
      });
    } finally {
      setLoading(false);
    }
  };

  const geneInfoColOptions = ds.columns;
  const idTypeMismatch = ds.geneIdType && ds.detectedGeneIdType && ds.geneIdType !== ds.detectedGeneIdType;

  const canProceed = !!ds.expressionFileName && !!ds.geneIdCol && !!ds.geneIdType;

  const done = ds.uploadDone;
  const hasSettingsChanged = done && (
    ds.expressionFileName !== ds.submittedExpressionFileName ||
    ds.geneIdCol !== ds.submittedGeneIdCol ||
    ds.geneIdType !== ds.submittedGeneIdType
  );

  const completedModules: string[] = [];
  if (state.standaloneEnDone || state.enStep === "export") {
    completedModules.push("Enrichment Analysis");
  }
  const downstreamDone = completedModules.length > 0;

  const handleContinue = async () => {
    let success = true;
    if (!ds.uploadDone || hasSettingsChanged) {
      setLoading(true);
      setLoadingMsg("Uploading temporary data to server…");
      try {
        if (ds.expressionFileName && ds.parsedData) {
          await uploadEADatasetDataAPI([{
            ...ds,
            isNormalized: "" as any,
            dataType: "" as any,
            platform: "" as any,
          }]);
          updateDs({
            integrityOk: true,
            integrityIssues: [],
          });
        }
      } catch (e: any) {
        console.error("Failed to upload temporary data to server:", e);
        updateDs({
          integrityOk: false,
          integrityIssues: [`Failed to upload temporary data to server: ${e.message || e}`],
        });
        success = false;
      }
      setLoading(false);
    }

    if (!success) return;

    updateDs({
      uploadDone: true,
      submittedExpressionFileName: ds.expressionFileName,
      submittedGeneIdCol: ds.geneIdCol,
      submittedGeneIdType: ds.geneIdType,
    });
    dispatch({ type: "EN_SUBMIT_CONFIG" });
    
    const sorted = [...state.enDatasets].sort((a, b) => a.id.localeCompare(b.id));
    const idx = sorted.findIndex(d => d.id === datasetId);

    if (state.enDatasets.length > 1) {
      if (idx < sorted.length - 1) {
        const nextDs = sorted[idx + 1];
        dispatch({ type: "EN_SELECT_DATASET", id: nextDs.id });
        dispatch({ type: "EN_SET_CONTEXT", id: nextDs.id });
      } else {
        dispatch({ type: "EN_SET_CONTEXT", id: "all" });
      }
    } else if (state.enSelectedContext !== "all") {
      dispatch({ type: "EN_SET_STEP", step: "analysis" });
    }
  };

  const onContinueClick = () => {
    if (hasSettingsChanged && downstreamDone) {
      setShowDiscardModal(true);
    } else {
      handleContinue();
    }
  };

  return (
    <>
      {loading && <Spinner label={loadingMsg} sublabel="Please wait…" />}

      {/* Gene List Upload OR Config + Preview Table */}
      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-header">
          <div>
            <div className="card-title">Gene List</div>
            <div className="card-sub">
              {ds.expressionFileName 
                ? "Configure options and review dataset mapping" 
                : "Upload your gene list (CSV / TSV). Rows = genes. Case Sensitive"}
            </div>
          </div>
          <div style={{ display: "flex", gap: 8 }}>
            {ds.expressionFileName && (
              <button 
                className="link-button danger" 
                onClick={handleClear}
              >
                Clear
              </button>
            )}
            {!ds.expressionFileName && (
              <button className="link-button primary" onClick={handleLoadSample}>Example Data</button>
            )}
          </div>
        </div>

        {!ds.expressionFileName ? (
          <>
            <div
              className={`drop-zone ${drag ? "dragover" : ""}`}
              onClick={() => {
                const i = document.createElement("input"); i.type = "file"; i.accept = ".csv,.tsv,.txt";
                i.onchange = e => { const f = (e.target as HTMLInputElement).files?.[0]; if (f) handleFile(f); };
                i.click();
              }}
              onDragOver={e => { e.preventDefault(); setDrag(true); }}
              onDragLeave={() => setDrag(false)}
              onDrop={e => { e.preventDefault(); setDrag(false); const f = e.dataTransfer.files?.[0]; if (f) handleFile(f); }}
            >
              <div className="drop-zone-icon">📊</div>
              <div className="drop-zone-title">Drop gene list file here</div>
              <div className="drop-zone-hint">CSV · TSV · TXT</div>
            </div>
            {ds.integrityIssues && ds.integrityIssues.length > 0 && (
              <div className="banner warn" style={{ marginTop: 14 }}>
                <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
                  {ds.integrityIssues.map((issue, idx) => (
                    <div key={idx}>{issue}</div>
                  ))}
                </div>
              </div>
            )}
          </>
        ) : (
          <>
            {/* File Info Badge */}
            <div className="file-chip" style={{ marginBottom: 14 }}>
              <FileText size={15} />
              <span className="file-chip-name">{ds.expressionFileName}</span>
              <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>{ds.nFeatures.toLocaleString()} features</span>
            </div>

            {ds.integrityIssues && ds.integrityIssues.length > 0 && (
              <div className="banner warn" style={{ marginBottom: 14 }}>
                <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
                  {ds.integrityIssues.map((issue, idx) => (
                    <div key={idx}>{issue}</div>
                  ))}
                </div>
              </div>
            )}

            {/* Data Summary */}
            <div style={{ fontSize: 12, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 10, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Data Summary
            </div>
            <div className="chips-row" style={{ marginBottom: 16 }}>
              <div className="stat-chip"><div className="stat-chip-val">{ds.nFeatures.toLocaleString()}</div><div className="stat-chip-lbl">Features</div></div>
            </div>

            {/* Column Configuration */}
            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 12, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Column Configuration
            </div>
            <div style={{ display: "flex", flexDirection: "column", width: 178, marginBottom: 14 }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Dataset name</label>
              <input 
                type="text" 
                value={ds.name} 
                onChange={e => updateDs({ name: e.target.value })}
                style={{ width: "100%", padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                data-testid="input-dataset-name"
              />
            </div>

            {/* Row 1: Gene ID Column and Gene ID Type */}
            <div style={{ display: "flex", gap: 14, flexWrap: "wrap", marginBottom: 14 }}>
              <div style={{ display: "flex", flexDirection: "column", minWidth: 180 }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Gene ID Column</label>
                
                <details style={{ width: 180, position: "relative" }} data-testid="details-gene-id-col">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{ds.geneIdCol || "Select column..."}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {geneInfoColOptions.map(col => (
                      <div
                        key={col}
                        onClick={(e) => {
                          updateDs({ geneIdCol: col });
                          const details = (e.target as HTMLElement).closest("details");
                          if (details) details.removeAttribute("open");
                        }}
                        style={{
                          padding: "8px 10px",
                          fontSize: 12,
                          cursor: "pointer",
                          background: ds.geneIdCol === col ? "var(--selected-bg)" : "transparent",
                          fontWeight: ds.geneIdCol === col ? 600 : 400
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                        onMouseLeave={e => e.currentTarget.style.background = ds.geneIdCol === col ? "var(--selected-bg)" : "transparent"}
                      >
                        {col}
                      </div>
                    ))}
                  </div>
                </details>
              </div>

              {ds.geneIdCol && (
                <div style={{ display: "flex", flexDirection: "column", minWidth: 240 }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>
                    Gene ID Type
                    {ds.detectedGeneIdType && (
                      <span style={{
                        marginLeft: 10, fontSize: 11, fontWeight: 500, padding: "2px 8px", borderRadius: 4,
                        background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--border)",
                      }}>
                        Auto-detected: {GENE_ID_LABELS[ds.detectedGeneIdType] ?? ds.detectedGeneIdType}
                      </span>
                    )}
                  </label>
                  <details style={{ width: 250, position: "relative" }} data-testid="details-gene-id-type">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: `1px solid ${idTypeMismatch ? "var(--destructive)" : "var(--border)"}`, borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>{GENE_ID_LABELS[ds.geneIdType] || ds.geneIdType || "Select type..."}</span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {Object.entries(GENE_ID_LABELS).map(([val, label]) => (
                        <div
                          key={val}
                          onClick={(e) => {
                            updateDs({ geneIdType: val as any });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.geneIdType === val ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.geneIdType === val ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.geneIdType === val ? "var(--selected-bg)" : "transparent"}
                        >
                          {label}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>
              )}
            </div>

            {/* Verification Banners */}
            {ds.geneIdCol && (
              <div style={{ marginBottom: 14 }}>
                {idTypeMismatch && (
                  <div className="banner warn" style={{ marginTop: 10 }}>
                    <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                    <span>
                      <strong>ID type mismatch:</strong> Auto-detector suggests <strong>{GENE_ID_LABELS[ds.detectedGeneIdType]}</strong>{" "}
                      (e.g. "{ds.parsedData?.[0]?.[0] || ""}"), but you selected <strong>{GENE_ID_LABELS[ds.geneIdType]}</strong>. Please verify.
                    </span>
                  </div>
                )}
                {!idTypeMismatch && ds.geneIdType && ds.detectedGeneIdType && (
                  <div className="banner success" style={{ marginTop: 10 }}>
                    <CheckCircle size={15} style={{ flexShrink: 0 }} />
                    ID type matches auto-detection ({GENE_ID_LABELS[ds.detectedGeneIdType]}).
                  </div>
                )}
              </div>
            )}

            <hr className="card-divider" />
            <div style={{ fontSize: 11, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Preview (first 10 rows)
            </div>
            <div className="preview-wrap">
              <table>
                <thead>
                  <tr>{(ds.columns && ds.columns.length > 0 ? ds.columns : []).map(col => <th key={col}>{col}</th>)}</tr>
                </thead>
                <tbody>
                  {(ds.parsedData && ds.parsedData.length > 0 ? ds.parsedData : []).slice(0, 10).map((row, i) => (
                    <tr key={i}>
                      {row.map((cell: string, j: number) => <td key={j}>{cell}</td>)}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </>
        )}
      </div>

      {showDiscardModal && (
        <DiscardWarnModal
          stepId="upload"
          datasetIds={[datasetId]}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: datasetId,
              fromStep: "enrichment-upload"
            } as any);
            await handleContinue();
          }}
        />
      )}

      <div className="action-row" style={{ display: "flex", gap: 8, justifyContent: "flex-end" }}>
        {state.enDatasets.length > 1 && (() => {
          const sorted = [...state.enDatasets].sort((a, b) => a.id.localeCompare(b.id));
          const idx = sorted.findIndex(d => d.id === datasetId);
          if (idx > 0) {
            return (
              <button 
                type="button"
                className="btn btn-default" 
                onClick={() => {
                  const prevDs = sorted[idx - 1];
                  dispatch({ type: "EN_SELECT_DATASET", id: prevDs.id });
                  dispatch({ type: "EN_SET_CONTEXT", id: prevDs.id });
                }}
                data-testid="btn-upload-back"
              >
                ← Back
              </button>
            );
          }
          return null;
        })()}
        <button
          className="btn btn-primary"
          disabled={!canProceed}
          onClick={onContinueClick}
          data-testid="btn-en-upload-continue"
        >
          {state.enDatasets.length > 1 && (() => {
            const sorted = [...state.enDatasets].sort((a, b) => a.id.localeCompare(b.id));
            const idx = sorted.findIndex(d => d.id === datasetId);
            return idx < sorted.length - 1 ? "Next Dataset →" : "Continue to Upload Summary →";
          })()}
          {state.enDatasets.length <= 1 && "Continue to Analysis →"}
        </button>
      </div>
    </>
  );
}