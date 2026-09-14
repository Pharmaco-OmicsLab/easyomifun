interface Props {
  label?: string;
  sublabel?: string;
  onCancel?: () => void;
}

export default function Spinner({ label = "Processing...", sublabel, onCancel }: Props) {
  return (
    <div className="spinner-overlay">
      <div className="spinner" />
      <div className="spinner-label">{label}</div>
      {sublabel && <div className="spinner-sublabel">{sublabel}</div>}
      {onCancel && (
        <button
          type="button"
          className="btn btn-default"
          style={{ marginTop: 14 }}
          onClick={onCancel}
        >
          Cancel
        </button>
      )}
    </div>
  );
}
