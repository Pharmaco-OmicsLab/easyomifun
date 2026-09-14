interface Props {
  onConfirm: () => void;
  onCancel: () => void;
  type: "no_matching" | "partial_overlap";
}

export default function WarnSampleModal({ onConfirm, onCancel, type }: Props) {
  const isNoMatching = type === "no_matching";

  return (
    <div className="modal-backdrop" onClick={onCancel} style={{ zIndex: 1000 }}>
      <div className="modal-box" onClick={e => e.stopPropagation()} style={{ maxWidth: 400 }}>
        <div className="modal-title" style={{ textAlign: "center", marginBottom: 8, fontSize: 15 }}>
          <strong>{isNoMatching ? "No Matching Samples" : "Partially Matched Samples"}</strong>
        </div>
        <p style={{ fontSize: 13, color: "hsl(220 9% 46%)", textAlign: "center", lineHeight: 1.6, marginBottom: 20 }}>
          {isNoMatching
            ? "There are no overlapping samples found between your expression and clinical data. Please check your sample IDs. Continue would not store the uploaded clinical data"
            : "Your expression and clinical data samples are only partially matched. The system will only keep samples that have both expression and clinical data."}
        </p>
        <div style={{ display: "flex", gap: 10, justifyContent: "center" }}>
          <button
            className="btn btn-default"
            onClick={onCancel}
            data-testid="btn-warn-cancel"
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
            onClick={onConfirm}
            data-testid="btn-warn-confirm"
          >
            {isNoMatching ? "Are you sure?" : "Continue Anyway"}
          </button>
        </div>
      </div>
    </div>
  );
}
