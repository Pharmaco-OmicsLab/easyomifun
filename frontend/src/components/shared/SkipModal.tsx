import { useState, useRef } from "react";
import { skipStepAPI, cancelComputeJobAPI } from "../../lib/api";
import Spinner from "../Spinner";

interface Props {
  stepName: string;
  stepId: string;
  datasetIds: string[];
  onConfirm: () => void;
  onCancel: () => void;
}

export default function SkipModal({ stepName, stepId, datasetIds, onConfirm, onCancel }: Props) {
  const [loading, setLoading] = useState(false);
  const abortControllerRef = useRef<AbortController | null>(null);
  const activeJobIdRef = useRef<string | null>(null);

  const handleModalCancel = () => {
    if (activeJobIdRef.current) {
      cancelComputeJobAPI(activeJobIdRef.current);
      activeJobIdRef.current = null;
    }
    if (abortControllerRef.current) {
      abortControllerRef.current.abort();
      abortControllerRef.current = null;
    }
    setLoading(false);
  };

  const handleConfirm = async () => {
    setLoading(true);
    const controller = new AbortController();
    abortControllerRef.current = controller;
    try {
      await skipStepAPI(
        stepId,
        datasetIds,
        ({ jobId }) => { activeJobIdRef.current = jobId; },
        controller.signal
      );
    } catch (err: any) {
      if (err?.name === "AbortError" || controller.signal.aborted) {
        return;
      }
      console.error("Failed to call skip API:", err);
    } finally {
      setLoading(false);
      onConfirm();
    }
  };

  return (
    <div className="modal-backdrop" onClick={onCancel}>
      {loading && <Spinner label="Skipping step on server..." onCancel={handleModalCancel} />}
      <div className="modal-box" onClick={e => e.stopPropagation()} style={{ maxWidth: 380 }}>
        <div className="modal-title" style={{ textAlign: "center", marginBottom: 8, fontSize: 15 }}>
          <strong>Skip {stepName}?</strong>
        </div>
        <p style={{ fontSize: 13, color: "hsl(220 9% 46%)", textAlign: "center", lineHeight: 1.6, marginBottom: 20 }}>
          The <strong>{stepName}</strong> step will be skipped. You can return to it later
          by clicking its label in the step ribbon.
        </p>
        <div style={{ display: "flex", gap: 10, justifyContent: "center" }}>
          <button
            className="btn btn-default"
            onClick={onCancel}
            data-testid="btn-skip-cancel"
            disabled={loading}
          >
            Cancel
          </button>
          <button
            className="btn"
            style={{
              background: "hsl(0 65% 45%)", color: "white",
              border: "1px solid hsl(0 65% 38%)", borderRadius: 8,
              padding: "7px 18px", fontSize: 13, fontWeight: 600,
              cursor: "pointer",
            }}
            onClick={handleConfirm}
            data-testid="btn-skip-confirm"
            disabled={loading}
          >
            Yes, Skip
          </button>
        </div>
      </div>
    </div>
  );
}