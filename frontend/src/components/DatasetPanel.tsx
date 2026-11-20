import { useState, useEffect } from "react";
import { Plus, Trash2, Edit2, CheckCircle2, AlertTriangle, Check, X } from "lucide-react";
import { useAppStore } from "../store/appStore";
import { isMissingClinical } from "../dataObject";
import { clearDatasetAPI } from "../lib/api";

type Mode = "dp" | "de" | "fs" | "en";

interface Props { mode: Mode; }

export default function DatasetPanel({ mode }: Props) {
  const { state, dispatch } = useAppStore();
  const [editingId, setEditingId] = useState<string | null>(null);
  const [tempName, setTempName] = useState("");
  const [confirmDeleteId, setConfirmDeleteId] = useState<string | null>(null);

  useEffect(() => {
    if (!confirmDeleteId) return;
    const timer = setTimeout(() => setConfirmDeleteId(null), 3000);
    return () => clearTimeout(timer);
  }, [confirmDeleteId]);

  const datasets =
    mode === "dp" ? state.dpDatasets :
    mode === "de" ? state.deDatasets :
    mode === "fs" ? state.fsDatasets :
    state.enDatasets;

  const currentId =
    mode === "dp" ? state.dpCurrentDatasetId :
    mode === "de" ? state.deCurrentDatasetId :
    mode === "fs" ? state.fsCurrentDatasetId :
    state.enCurrentDatasetId;

  const dpStep = mode === "dp" ? state.dpStep : mode === "de" ? state.deStep : mode === "fs" ? state.fsStep : state.enStep;
  const isViewingAll =
    mode === "dp" ? (state.dpSelectedContext === "all" && state.dpDatasets.length > 1) :
    mode === "de" ? (state.deSelectedContext === "all" && state.deDatasets.length > 1) :
    mode === "fs" ? (state.fsSelectedContext === "all" && state.fsDatasets.length > 1) :
    (state.enSelectedContext === "all" && state.enDatasets.length > 1);

  let uploadDsId: string | null = null;
  if (isViewingAll && dpStep === "upload") {
    const pending = datasets.find(d => !d.uploadDone);
    if (pending) {
      uploadDsId = pending.id;
    } else {
      uploadDsId = currentId;
    }
  }

  let clinicalPendingDsId: string | null = null;
  if (isViewingAll && (dpStep === "batch" || dpStep === "inline-de" || dpStep === "inline-fs")) {
    const pending = datasets.find(isMissingClinical);
    if (pending) {
      clinicalPendingDsId = pending.id;
    }
  }

  function addDataset() {
    dispatch({ type: mode === "dp" ? "DP_ADD_DATASET" : mode === "de" ? "DE_ADD_DATASET" : mode === "fs" ? "FS_ADD_DATASET" : "EN_ADD_DATASET" });
    if (mode === "dp") {
      dispatch({ type: "DP_SET_STEP", step: "upload" });
    }
  }

  function removeDataset(id: string) {
    clearDatasetAPI(id, "all" as any).catch(err => console.warn("Failed to delete dataset from server:", err));
    dispatch({ type: mode === "dp" ? "DP_REMOVE_DATASET" : mode === "de" ? "DE_REMOVE_DATASET" : mode === "fs" ? "FS_REMOVE_DATASET" : "EN_REMOVE_DATASET", id });
  }

  function selectDataset(id: string) {
    if (datasets.length > 1) {
      if (mode === "dp") { dispatch({ type: "DP_SELECT_DATASET", id }); dispatch({ type: "DP_SET_CONTEXT", id: "all" }); }
      else if (mode === "de") { dispatch({ type: "DE_SELECT_DATASET", id }); dispatch({ type: "DE_SET_CONTEXT", id: "all" }); }
      else if (mode === "fs") { dispatch({ type: "FS_SELECT_DATASET", id }); dispatch({ type: "FS_SET_CONTEXT", id: "all" }); }
      else { dispatch({ type: "EN_SELECT_DATASET", id }); dispatch({ type: "EN_SET_CONTEXT", id: "all" }); }
    } else {
      if (mode === "dp") { dispatch({ type: "DP_SELECT_DATASET", id }); dispatch({ type: "DP_SET_CONTEXT", id }); }
      else if (mode === "de") { dispatch({ type: "DE_SELECT_DATASET", id }); dispatch({ type: "DE_SET_CONTEXT", id }); }
      else if (mode === "fs") { dispatch({ type: "FS_SELECT_DATASET", id }); dispatch({ type: "FS_SET_CONTEXT", id }); }
      else { dispatch({ type: "EN_SELECT_DATASET", id }); dispatch({ type: "EN_SET_CONTEXT", id }); }
    }
  }

  function handleStartRename(id: string, name: string) {
    setEditingId(id);
    setTempName(name);
  }

  function handleCommitRename(id: string) {
    if (tempName.trim()) {
      dispatch({
        type: mode === "dp" ? "DP_UPDATE_DATASET" : mode === "de" ? "DE_UPDATE_DATASET" : mode === "fs" ? "FS_UPDATE_DATASET" : "EN_UPDATE_DATASET",
        id,
        patch: { name: tempName.trim() }
      } as any);
    }
    setEditingId(null);
  }

  return (
    <div className="dataset-panel" data-testid="panel-datasets">
      <div className="dataset-panel-header">
        <span className="dataset-panel-title">Datasets</span>
        <span style={{ fontSize: 11, color: "hsl(220 9% 55%)", fontVariantNumeric: "tabular-nums" }}>{datasets.length}</span>
      </div>
      <div className="dataset-list">
        {datasets.length > 1 && (
          <div
            className={`dataset-card ${isViewingAll ? "selected" : ""}`}
            onClick={() => {
              if (mode === "dp") dispatch({ type: "DP_SET_CONTEXT", id: "all" });
              else if (mode === "de") dispatch({ type: "DE_SET_CONTEXT", id: "all" });
              else if (mode === "fs") dispatch({ type: "FS_SET_CONTEXT", id: "all" });
              else dispatch({ type: "EN_SET_CONTEXT", id: "all" });
            }}
            data-testid="card-all-datasets"
          >
            <div className="dataset-dot" style={{ background: "#1e40af" }} />
            <div style={{ flex: 1, minWidth: 0 }}>
              <div className="dataset-card-name" style={{ fontWeight: 600 }}>All Datasets</div>
              <div className="dataset-card-status" style={{ fontVariantNumeric: "tabular-nums" }}>
                {datasets.length} datasets combined
              </div>
            </div>
          </div>
        )}

        {datasets.map(ds => {
          let isSelected = uploadDsId ? (ds.id === uploadDsId) : (ds.id === currentId);
          if (clinicalPendingDsId) {
            isSelected = ds.id === clinicalPendingDsId;
          }
          return (
            <div
              key={ds.id}
              className={`dataset-card ${isSelected ? "selected" : ""}`}
              onClick={() => selectDataset(ds.id)}
              data-testid={`card-dataset-${ds.id}`}
            >
              <div className="dataset-dot" style={{ background: ds.color }} />
              <div style={{ flex: 1, minWidth: 0 }}>
                {editingId === ds.id ? (
                  <input
                    type="text"
                    value={tempName}
                    onChange={e => setTempName(e.target.value)}
                    onBlur={() => handleCommitRename(ds.id)}
                    onKeyDown={e => {
                      if (e.key === "Enter") handleCommitRename(ds.id);
                      else if (e.key === "Escape") setEditingId(null);
                    }}
                    autoFocus
                    style={{
                      width: "90%",
                      padding: "2px 6px",
                      border: "1px solid var(--primary)",
                      borderRadius: 4,
                      fontSize: 12,
                      background: "white",
                      color: "black",
                    }}
                    onClick={e => e.stopPropagation()}
                  />
                ) : (
                  <div 
                    className="dataset-card-name" 
                    style={{ display: "flex", alignItems: "center", gap: 6, cursor: "pointer", justifyContent: "space-between" }}
                    onDoubleClick={(e) => { e.stopPropagation(); handleStartRename(ds.id, ds.name); }}
                  >
                    <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", flex: 1 }}>{ds.name}</span>
                    <button
                      type="button"
                      onClick={(e) => { e.stopPropagation(); handleStartRename(ds.id, ds.name); }}
                      style={{
                        background: "none", border: "none", padding: 2, cursor: "pointer",
                        color: "hsl(220 9% 65%)", display: "flex", alignItems: "center"
                      }}
                      title="Rename Dataset"
                      className="rename-btn"
                    >
                      <Edit2 size={11} />
                    </button>
                  </div>
                )}
                <div className="dataset-card-status">
                  {ds.uploadDone ? (
                    <div style={{ display: "flex", flexDirection: "column", gap: 2 }}>
                      <span style={{ color: "#15803d", fontWeight: 500, fontVariantNumeric: "tabular-nums" }}>
                        {ds.nSamples > 0 ? `${ds.nSamples} samples · ${ds.nFeatures.toLocaleString()} genes` : "Loaded"}
                      </span>
                      {ds.submittedClinicalFileName ? (
                        <span style={{ color: "#0369a1", fontSize: 10, fontWeight: 500, display: "inline-flex", alignItems: "center", gap: 3 }}>
                          <Check size={11} /> Clinical: {ds.submittedClinicalFileName}
                        </span>
                      ) : (
                        <span style={{ color: "#b91c1c", fontSize: 10, fontWeight: 500, display: "inline-flex", alignItems: "center", gap: 3 }}>
                          <X size={11} /> Missing Clinical Data
                        </span>
                      )}
                    </div>
                  ) : <span>No data</span>}
                </div>
              </div>

              {/* Ready / Pending Badges replacing emojis */}
              {ds.uploadDone && ds.integrityOk && (
                <span
                  style={{
                    display: "inline-flex",
                    alignItems: "center",
                    gap: 3,
                    background: "#dcfce7",
                    color: "#15803d",
                    padding: "2px 7px",
                    borderRadius: 9999,
                    fontSize: 10,
                    fontWeight: 600,
                    flexShrink: 0,
                  }}
                  title="Integrity check passed"
                >
                  <CheckCircle2 size={12} />
                  <span>Ready</span>
                </span>
              )}

              {ds.uploadDone && !ds.integrityOk && (
                <span
                  style={{
                    display: "inline-flex",
                    alignItems: "center",
                    gap: 3,
                    background: "#fef3c7",
                    color: "#b45309",
                    padding: "2px 7px",
                    borderRadius: 9999,
                    fontSize: 10,
                    fontWeight: 600,
                    flexShrink: 0,
                  }}
                  title={ds.integrityIssues.join("; ")}
                >
                  <AlertTriangle size={12} />
                  <span>Pending</span>
                </span>
              )}

              {datasets.length > 1 && (
                <button
                  type="button"
                  className="dataset-remove-btn"
                  onClick={e => {
                    e.stopPropagation();
                    if (confirmDeleteId === ds.id) {
                      setConfirmDeleteId(null);
                      removeDataset(ds.id);
                    } else {
                      setConfirmDeleteId(ds.id);
                    }
                  }}
                  onBlur={() => {
                    setTimeout(() => setConfirmDeleteId(cur => (cur === ds.id ? null : cur)), 250);
                  }}
                  data-testid={`button-remove-${ds.id}`}
                  id={`btn-remove-dataset-${ds.id}`}
                  title={confirmDeleteId === ds.id ? "Click again to confirm delete" : "Remove dataset"}
                  style={
                    confirmDeleteId === ds.id
                      ? {
                          background: "#fee2e2",
                          color: "#b91c1c",
                          border: "1px solid #f87171",
                          padding: "2px 6px",
                          width: "auto",
                          height: "22px",
                          borderRadius: "4px",
                        }
                      : undefined
                  }
                >
                  {confirmDeleteId === ds.id ? (
                    <span style={{ fontSize: 10, fontWeight: 700 }}>Delete?</span>
                  ) : (
                    <Trash2 size={13} />
                  )}
                </button>
              )}
            </div>
          );
        })}

        {mode !== "en" && (
          <button
            type="button"
            className="add-dataset-btn"
            onClick={addDataset}
            data-testid="button-add-dataset"
            id="btn-add-dataset"
          >
            <Plus size={14} /> Add Dataset
          </button>
        )}
      </div>

      {datasets.length > 0 && datasets.some(d => d.uploadDone) && (
        <div style={{ padding: "10px 12px", borderTop: "1px solid var(--border)", flexShrink: 0 }}>
          <div style={{ fontSize: 10, fontWeight: 700, textTransform: "uppercase", letterSpacing: ".05em", color: "var(--muted-foreground)", marginBottom: 6 }}>
            Integrity Check
          </div>
          <div className="integrity-list">
            {datasets.filter(d => d.uploadDone).map(d => (
              <div key={d.id} className={`integrity-item ${d.integrityOk ? "ok" : "err"}`}>
                <div style={{ width: 6, height: 6, borderRadius: "50%", background: d.integrityOk ? "hsl(150 55% 40%)" : "hsl(0 60% 50%)", flexShrink: 0 }} />
                <span style={{ flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{d.name}</span>
                <span style={{ fontVariantNumeric: "tabular-nums", display: "inline-flex", alignItems: "center" }}>
                  {d.integrityOk ? "OK" : <AlertTriangle size={12} color="hsl(38 80% 40%)" />}
                </span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}
