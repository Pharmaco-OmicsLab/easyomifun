interface Props {
  onConfirm: () => void;
  onCancel: () => void;
}

export default function ChangeWarnModal({ onConfirm, onCancel }: Props) {
  return (
    <div className="modal-backdrop" onClick={onCancel}>
      <div className="modal-box" onClick={e => e.stopPropagation()} style={{ maxWidth: 390 }}>
        <div className="modal-title" style={{ textAlign: "center", marginBottom: 8, fontSize: 15 }}>
          <strong>Unsaved Configuration Changes</strong>
        </div>
        <p style={{ fontSize: 13, color: "hsl(220 9% 46%)", textAlign: "center", lineHeight: 1.6, marginBottom: 20 }}>
          You have modified your settings, but you haven't run the tool with them yet. 
          If you continue, your previous results will be saved instead.
        </p>
        <div style={{ display: "flex", gap: 10, justifyContent: "center" }}>
          <button
            className="btn btn-default"
            onClick={onCancel}
            data-testid="btn-warn-cancel"
          >
            Go Back
          </button>
          <button
            className="btn"
            style={{
              background: "hsl(0 65% 45%)", color: "white",
              border: "1px solid hsl(0 65% 38%)", borderRadius: 8,
              padding: "7px 18px", fontSize: 13, fontWeight: 600,
              cursor: "pointer",
            }}
            onClick={onConfirm}
            data-testid="btn-warn-confirm"
          >
            Continue Anyway
          </button>
        </div>
      </div>
    </div>
  );
}