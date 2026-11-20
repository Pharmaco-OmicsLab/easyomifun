import { AlertTriangle } from "lucide-react";
import { clearAllResultsAPI } from "../../lib/api";

interface Props {
  onConfirm: () => void;
  onCancel: () => void;
}

export default function RestartWarnModal({ onConfirm, onCancel }: Props) {
  const handleConfirm = () => {
    clearAllResultsAPI().catch((err) =>
      console.warn("clearAllResultsAPI background cleanup error:", err)
    );
    onConfirm();
  };

  return (
    <div className="modal-overlay" style={{
      position: "fixed", top: 0, left: 0, right: 0, bottom: 0,
      background: "rgba(16, 26, 54, 0.4)", display: "flex",
      alignItems: "center", justifyContent: "center", zIndex: 1000
    }} data-testid="modal-restart-warn">
      <div className="card" style={{ width: 440, padding: 24, boxShadow: "0 12px 24px rgba(0,0,0,0.15)", borderRadius: 12 }}>
        <div style={{ display: "flex", gap: 12, alignItems: "flex-start", marginBottom: 16 }}>
          <div style={{
            background: "hsl(38 90% 96%)", color: "hsl(38 85% 40%)",
            padding: 8, borderRadius: "50%", display: "flex", flexShrink: 0
          }}>
            <AlertTriangle size={24} />
          </div>
          <div>
            <div className="card-title" style={{ fontSize: 16, fontWeight: 700, color: "var(--foreground)", marginBottom: 6 }}>
              Have you saved all your results?
            </div>
            <p style={{ fontSize: 13, color: "hsl(220 9% 45%)", lineHeight: 1.5, margin: 0 }}>
              Starting a new analysis will clear all current settings, uploaded files, and generated plots. Please make sure to download all required exports before proceeding.
            </p>
          </div>
        </div>
        <div style={{ display: "flex", justifyContent: "flex-end", gap: 10 }}>
          <button className="btn btn-default" onClick={onCancel} data-testid="btn-restart-cancel">
            Cancel
          </button>
          <button
            className="btn btn-primary"
            style={{ background: "hsl(38 85% 45%)", borderColor: "hsl(38 85% 45%)" }}
            onClick={handleConfirm}
            data-testid="btn-restart-confirm"
          >
            Proceed, Start New
          </button>
        </div>
      </div>
    </div>
  );
}
