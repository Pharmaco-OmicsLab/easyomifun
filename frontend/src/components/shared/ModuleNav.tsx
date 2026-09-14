import { useState } from "react";
import { useLocation } from "wouter";
import { useAppStore } from "../../store/appStore";

const MODULES = [
  { id: "fs", full: "Feature Selection", path: "/feature-selection" },
  { id: "de", full: "Differential Expression", path: "/de-analysis" },
  { id: "ea", full: "Enrichment Analysis", path: "/enrichment" },
  { id: "dp", full: "Data Processing", path: "/data-processing" },
];

interface Props {
  active: "dp" | "de" | "ea" | "fs";
}

export default function ModuleNav({ active }: Props) {
  const [, navigate] = useLocation();
  const [isOpen, setIsOpen] = useState(false);
  const { dispatch } = useAppStore();
  const activeModule = MODULES.find(m => m.id === active) || MODULES[0];

  return (
    /* ADJUST HEIGHT/WIDTH HERE: Set your desired container constraints here */
    <div style={{ position: "relative", width: "100%", maxWidth: 280 }}>
      {/* Main Clickable Trigger Button */}
      <button
        onClick={() => setIsOpen(!isOpen)}
        style={{
          width: "100%",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          padding: "8px 12px",
          background: "#eaefff",
          border: "1px solid #c4d1f5",
          borderRadius: 8,
          cursor: "pointer",
          fontSize: 13,
          fontWeight: 600,
          color: "#101a36",
          textAlign: "left",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
          {activeModule.full}
        </div>
        <span style={{ fontSize: 10, color: "#7835ff", transform: isOpen ? "rotate(180deg)" : "none" }}>▼</span>
      </button>

      {/* Absolute Positioned Menu List */}
      {isOpen && (
        <div style={{
          position: "absolute",
          top: "100%",
          left: 0,
          right: 0,
          marginTop: 4,
          background: "#eaefff",
          border: "1px solid #c4d1f5",
          borderRadius: 8,
          boxShadow: "0 4px 12px rgba(16, 26, 54, 0.12)",
          zIndex: 50,
          overflow: "hidden",
        }}>
          {MODULES.map(m => {
            const isSelected = m.id === active;
            return (
              <div
                key={m.id}
                onClick={() => {
                  if (!isSelected) {
                    navigate(m.path);
                    dispatch({ type: "SET_PIPELINE_MODE", mode: m.id === "dp" });
                  }
                  setIsOpen(false);
                }}
                style={{
                  display: "flex",
                  alignItems: "center",
                  gap: 8,
                  padding: "10px 14px",
                  fontSize: 13,
                  cursor: isSelected ? "default" : "pointer",
                  background: isSelected ? "linear-gradient(135deg, #7835ff 0%, #289ddf 100%)" : "transparent",
                  color: isSelected ? "white" : "#101a36",
                  fontWeight: isSelected ? 600 : 400,
                  transition: "all 0.15s ease",
                }}
                onMouseEnter={e => {
                  if (!isSelected) {
                    e.currentTarget.style.background = "#dbe4ff";
                    e.currentTarget.style.color = "#7835ff";
                  }
                }}
                onMouseLeave={e => {
                  if (!isSelected) {
                    e.currentTarget.style.background = "transparent";
                    e.currentTarget.style.color = "#101a36";
                  }
                }}
              >
                {m.full}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}