import { useState, useEffect } from "react";

export interface SpinnerProps {
  message?: string;
  label?: string;
  sublabel?: string;
  onCancel?: () => void;
}

export type Props = SpinnerProps;

export default function Spinner({ message, label, sublabel, onCancel }: SpinnerProps) {
  const [elapsed, setElapsed] = useState(0);

  useEffect(() => {
    const timer = setInterval(() => {
      setElapsed((prev) => prev + 1);
    }, 1000);
    return () => clearInterval(timer);
  }, []);

  const displayMessage = message || label || "Processing...";

  return (
    <div className="spinner-overlay">
      <div className="spinner-box" role="status" aria-live="polite">
        <div className="spinner" />
        <div className="spinner-label">{displayMessage}</div>
        {sublabel && <div className="spinner-sublabel">{sublabel}</div>}
        <div className="spinner-elapsed tnum">{elapsed}s elapsed</div>
        {onCancel && (
          <button
            type="button"
            className="btn btn-default spinner-cancel"
            style={{
              marginTop: 6,
              color: "var(--ink-soft)",
              borderColor: "var(--line-strong)",
              borderRadius: 8,
              fontSize: 13,
              fontWeight: 500,
            }}
            onClick={onCancel}
          >
            Cancel
          </button>
        )}
      </div>
    </div>
  );
}
