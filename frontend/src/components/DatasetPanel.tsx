import { useState } from "react";
import { Plus, Trash2, Edit2 } from "lucide-react";
import { useAppStore } from "../store/appStore";
import { isMissingClinical } from "../dataObject";
import { clearDatasetAPI } from "../lib/api";

type Mode = "dp" | "de" | "fs" | "en";

interface Props { mode: Mode; }

export default function DatasetPanel({ mode }: Props) {
  const { state, dispatch } = useAppStore();
  const [editingId, setEditingId] = useState<string | null>(null);
  const [tempName, setTempName] = useState("");

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
        <span style={{ fontSize: 11, color: "hsl(220 9% 55%)" }}>{datasets.length}</span>
      </div>
      <div className="dataset-list">
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
                    <span style={{ color: "hsl(150 55% 35%)", fontWeight: 500 }}>
                      {ds.nSamples > 0 ? `${ds.nSamples} samples · ${ds.nFeatures.toLocaleString()} genes` : "Loaded"}
                    </span>
                    {ds.submittedClinicalFileName ? (
                      <span style={{ color: "hsl(210 60% 45%)", fontSize: 10, fontWeight: 500 }}>
                        ✓ Clinical: {ds.submittedClinicalFileName}
                      </span>
                    ) : (
                      <span style={{ color: "hsl(0 65% 45%)", fontSize: 10, fontWeight: 500 }}>
                        ✗ Missing Clinical Data
                      </span>
                    )}
                  </div>
                ) : <span>No data</span>}
              </div>
            </div>
            {!ds.integrityOk && ds.uploadDone && <span style={{ fontSize: 14 }} title={ds.integrityIssues.join("; ")}>⚠️</span>}
            {ds.integrityOk && ds.uploadDone && <span style={{ fontSize: 14 }}>✅</span>}
            {datasets.length > 1 && (
              <button
                className="dataset-remove-btn"
                onClick={e => { e.stopPropagation(); removeDataset(ds.id); }}
                data-testid={`btn-remove-dataset-${ds.id}`}
                title="Remove dataset"
              >
                <Trash2 size={13} />
              </button>
            )}
          </div>
        );
      })}

        {mode !== "en" && (
          <button className="add-dataset-btn" onClick={addDataset} data-testid="btn-add-dataset">
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
                <span>{d.integrityOk ? "OK" : "⚠"}</span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}
