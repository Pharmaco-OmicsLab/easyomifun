import { useState, useEffect } from "react";
import { createPortal } from "react-dom";
import { RotateCcw, AlertTriangle, X } from "lucide-react";
import { useLocation } from "wouter";
import { SERVER_URL } from "../../lib/api";

export default function ReloadButton() {
  const [showConfirm, setShowConfirm] = useState(false);
  const [location] = useLocation();

  const isHome = location === "/" || location === "" || location === "/index.html";

  const handleReload = () => {
    const oldUserId = sessionStorage.getItem("easyomifun_user_id");
    if (oldUserId) {
      try {
        navigator.sendBeacon(`${SERVER_URL}/api/cleanup?userId=${oldUserId}`);
      } catch (_) {}
    }

    sessionStorage.clear();
    localStorage.clear();

    window.location.replace(import.meta.env.BASE_URL);
  };

  useEffect(() => {
    if (!showConfirm) return;
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") setShowConfirm(false);
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [showConfirm]);

  return (
    <>
      <button
        type="button"
        onClick={() => setShowConfirm(true)}
        title="Reset Session & Clear All Data"
        data-testid="button-reload-app"
        id="btn-reload-app"
        style={{
          display: "flex",
          alignItems: "center",
          gap: "6px",
          background: isHome ? "rgba(255, 255, 255, 0.1)" : "rgba(15, 23, 42, 0.05)",
          color: isHome ? "#f8fafc" : "#334155",
          border: isHome ? "1px solid rgba(255, 255, 255, 0.25)" : "1px solid rgba(15, 23, 42, 0.18)",
          borderRadius: "8px",
          padding: "6px 12px",
          fontSize: "12px",
          fontWeight: 600,
          cursor: "pointer",
          transition: "all 0.15s ease",
          flexShrink: 0,
          height: "32px",
          boxSizing: "border-box",
        }}
        onMouseEnter={e => {
          e.currentTarget.style.background = isHome ? "rgba(255, 255, 255, 0.18)" : "rgba(15, 23, 42, 0.1)";
          if (!isHome) {
            e.currentTarget.style.color = "#0f172a";
            e.currentTarget.style.borderColor = "rgba(15, 23, 42, 0.3)";
          }
        }}
        onMouseLeave={e => {
          e.currentTarget.style.background = isHome ? "rgba(255, 255, 255, 0.1)" : "rgba(15, 23, 42, 0.05)";
          if (!isHome) {
            e.currentTarget.style.color = "#334155";
            e.currentTarget.style.borderColor = "rgba(15, 23, 42, 0.18)";
          }
        }}
      >
        <RotateCcw size={13} />
        <span>Reset Session</span>
      </button>

      {/* Confirmation Modal */}
      {showConfirm && typeof document !== "undefined" && createPortal(
        <div
          role="dialog"
          aria-modal="true"
          aria-labelledby="reset-modal-title"
          style={{
            position: "fixed",
            inset: 0,
            zIndex: 9999,
            background: "rgba(15, 23, 42, 0.55)",
            backdropFilter: "blur(4px)",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            padding: "20px",
            animation: "fadeIn 0.15s ease-out",
          }}
          onClick={() => setShowConfirm(false)}
        >
          <div
            onClick={e => e.stopPropagation()}
            style={{
              width: "100%",
              maxWidth: "460px",
              background: "#ffffff",
              borderRadius: "16px",
              boxShadow: "0 25px 50px -12px rgba(15, 23, 42, 0.25)",
              border: "1px solid #e2e8f0",
              padding: "24px",
              boxSizing: "border-box",
            }}
          >
            <div style={{ display: "flex", alignItems: "flex-start", gap: 14 }}>
              <div
                style={{
                  width: 40,
                  height: 40,
                  borderRadius: "50%",
                  background: "#fef2f2",
                  color: "#dc2626",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                  flexShrink: 0,
                }}
              >
                <AlertTriangle size={20} />
              </div>

              <div style={{ flex: 1, minWidth: 0 }}>
                <h3
                  id="reset-modal-title"
                  style={{
                    fontSize: "16px",
                    fontWeight: 700,
                    color: "#0f172a",
                    margin: "0 0 6px 0",
                    lineHeight: "1.3",
                  }}
                >
                  Reset Session & Clear All Data?
                </h3>
                <p
                  style={{
                    fontSize: "13px",
                    color: "#475569",
                    lineHeight: "1.5",
                    margin: 0,
                  }}
                >
                  This will permanently clear all uploaded datasets, parameters, and computed results from the current session.
                </p>
              </div>

              <button
                type="button"
                onClick={() => setShowConfirm(false)}
                aria-label="Close"
                style={{
                  background: "none",
                  border: "none",
                  color: "#94a3b8",
                  cursor: "pointer",
                  padding: "4px",
                  borderRadius: "6px",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                }}
              >
                <X size={18} />
              </button>
            </div>

            <div style={{ display: "flex", justifyContent: "flex-end", gap: 10, marginTop: 24 }}>
              <button
                type="button"
                onClick={() => setShowConfirm(false)}
                style={{
                  padding: "8px 16px",
                  fontSize: "13px",
                  fontWeight: 600,
                  borderRadius: "8px",
                  border: "1px solid #cbd5e1",
                  background: "#f8fafc",
                  color: "#334155",
                  cursor: "pointer",
                  transition: "all 0.15s ease",
                }}
                onMouseEnter={e => { e.currentTarget.style.background = "#f1f5f9"; }}
                onMouseLeave={e => { e.currentTarget.style.background = "#f8fafc"; }}
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={handleReload}
                data-testid="button-confirm-reload-app"
                style={{
                  padding: "8px 18px",
                  fontSize: "13px",
                  fontWeight: 600,
                  borderRadius: "8px",
                  border: "none",
                  background: "#b91c1c",
                  color: "#ffffff",
                  cursor: "pointer",
                  boxShadow: "0 2px 8px rgba(185, 28, 28, 0.25)",
                  transition: "all 0.15s ease",
                }}
                onMouseEnter={e => { e.currentTarget.style.background = "#991b1b"; }}
                onMouseLeave={e => { e.currentTarget.style.background = "#b91c1c"; }}
              >
                Reset Session
              </button>
            </div>
          </div>
        </div>,
        document.body
      )}
    </>
  );
}
