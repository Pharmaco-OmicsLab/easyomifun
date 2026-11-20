import { useEffect } from "react";
import { AlertTriangle } from "lucide-react";

interface Props {
  onConfirm: () => void;
  onCancel: () => void;
}

export default function ChangeWarnModal({ onConfirm, onCancel }: Props) {
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") {
        onCancel();
      }
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [onCancel]);

  return (
    <div className="modal-backdrop" onClick={onCancel} style={{ zIndex: 1100 }}>
      <div
        className="modal-box"
        role="dialog"
        aria-modal="true"
        aria-labelledby="change-warn-title"
        onClick={(e) => e.stopPropagation()}
        style={{
          maxWidth: 440,
          background: "#ffffff",
          border: "1px solid #e2e8f0",
          borderRadius: 16,
          padding: 24,
          boxShadow: "0 20px 25px -5px rgba(15, 23, 42, 0.1), 0 8px 10px -6px rgba(15, 23, 42, 0.05)",
        }}
      >
        <div style={{ display: "flex", alignItems: "flex-start", gap: 14 }}>
          <div
            style={{
              width: 38,
              height: 38,
              borderRadius: "50%",
              background: "#fef3c7",
              color: "#b45309",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              flexShrink: 0,
              marginTop: 2,
            }}
          >
            <AlertTriangle size={20} />
          </div>
          <div style={{ flex: 1, minWidth: 0 }}>
            <h3
              id="change-warn-title"
              style={{
                fontSize: 16,
                fontWeight: 700,
                color: "#0f172a",
                margin: 0,
                lineHeight: 1.3,
              }}
            >
              Unsaved Configuration Changes
            </h3>
            <p
              style={{
                fontSize: 13,
                color: "#334155",
                lineHeight: 1.6,
                marginTop: 8,
                marginBottom: 0,
              }}
            >
              You have modified your settings, but you haven't run the tool with them yet. If you continue, your previous results will be saved instead.
            </p>
          </div>
        </div>
        <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", marginTop: 20 }}>
          <button
            type="button"
            className="btn btn-default"
            onClick={onCancel}
            data-testid="btn-warn-cancel"
            style={{
              borderRadius: 8,
              padding: "7px 16px",
              fontSize: 13,
              fontWeight: 500,
              color: "#334155",
              borderColor: "#cbd5e1",
            }}
          >
            Go Back
          </button>
          <button
            type="button"
            className="btn btn-primary"
            onClick={onConfirm}
            data-testid="btn-warn-confirm"
            style={{
              borderRadius: 8,
              padding: "7px 16px",
              fontSize: 13,
              fontWeight: 600,
            }}
          >
            Continue Anyway
          </button>
        </div>
      </div>
    </div>
  );
}
