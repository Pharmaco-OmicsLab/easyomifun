import { useState } from "react";
import { redoStepAPI, clearDownstreamAPI } from "../../lib/api";
import Spinner from "../Spinner";

interface Props {
  stepId?: string;
  datasetIds?: string[];
  onConfirm: () => void;
  onCancel: () => void;
  modulesList?: string[];
}

export default function DiscardWarnModal({ stepId, datasetIds, onConfirm, onCancel }: Props) {
  const [loading, setLoading] = useState(false);

  const handleConfirm = async () => {
    if (stepId && datasetIds && datasetIds.length > 0) {
      setLoading(true);
      try {
        await Promise.all([
          redoStepAPI(stepId, datasetIds),
          clearDownstreamAPI(stepId, datasetIds)
        ]);
      } catch (err) {
        console.error("Failed to redo step or clear downstream results:", err);
      } finally {
        setLoading(false);
        onConfirm();
      }
    } else {
      onConfirm();
    }
  };

  return (
    <div className="modal-backdrop" onClick={onCancel} style={{ zIndex: 1100 }}>
      {loading && <Spinner label="Discarding downstream results on server..." />}
      <div className="modal-box" onClick={e => e.stopPropagation()} style={{ maxWidth: 400 }}>
        <div className="modal-title" style={{ textAlign: "center", marginBottom: 8, fontSize: 16 }}>
          <strong>Discard Downstream Results?</strong>
        </div>
        <p style={{ fontSize: 13, color: "hsl(220 9% 46%)", textAlign: "center", lineHeight: 1.6, marginBottom: 16 }}>
          Redoing this step will overwrite your current settings and discard the results of all subsequent steps. You will need to run the downstream steps again.
        </p>
        <div style={{ display: "flex", gap: 10, justifyContent: "center" }}>
          <button
            className="btn btn-default"
            onClick={onCancel}
            data-testid="btn-discard-cancel"
            disabled={loading}
          >
            Go Back
          </button>
          <button
            className="btn"
            style={{
              background: "hsl(0 75% 48%)", color: "white",
              border: "1px solid hsl(0 75% 40%)", borderRadius: 8,
              padding: "7px 18px", fontSize: 13, fontWeight: 600,
              cursor: "pointer",
            }}
            onClick={handleConfirm}
            data-testid="btn-discard-confirm"
            disabled={loading}
          >
            Discard & Redo
          </button>
        </div>
      </div>
    </div>
  );
}
