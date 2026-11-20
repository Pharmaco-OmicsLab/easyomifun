import { useState, useRef, useEffect } from "react";
import { useLocation } from "wouter";
import { ChevronDown, Check } from "lucide-react";
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
  const containerRef = useRef<HTMLDivElement>(null);
  const { dispatch } = useAppStore();

  const activeModule = MODULES.find(m => m.id === active) || MODULES[0];

  useEffect(() => {
    if (!isOpen) return;

    const handleMouseDown = (e: MouseEvent) => {
      if (containerRef.current && !containerRef.current.contains(e.target as Node)) {
        setIsOpen(false);
      }
    };

    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") {
        setIsOpen(false);
      }
    };

    document.addEventListener("mousedown", handleMouseDown);
    document.addEventListener("keydown", handleKeyDown);
    return () => {
      document.removeEventListener("mousedown", handleMouseDown);
      document.removeEventListener("keydown", handleKeyDown);
    };
  }, [isOpen]);

  return (
    <div ref={containerRef} style={{ position: "relative", width: "100%", maxWidth: 280 }}>
      {/* Main Clickable Trigger Button */}
      <button
        type="button"
        onClick={() => setIsOpen(!isOpen)}
        aria-expanded={isOpen}
        aria-haspopup="menu"
        style={{
          width: "100%",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          padding: "7px 12px",
          background: "#ffffff",
          border: "1px solid #cbd5e1",
          borderRadius: 8,
          cursor: "pointer",
          fontSize: 13,
          fontWeight: 600,
          color: "#0f172a",
          textAlign: "left",
          transition: "all 0.15s ease",
          boxShadow: "0 1px 2px rgba(15, 23, 42, 0.05)",
        }}
        onMouseEnter={e => {
          e.currentTarget.style.borderColor = "#94a3b8";
          e.currentTarget.style.background = "#f8fafc";
        }}
        onMouseLeave={e => {
          e.currentTarget.style.borderColor = "#cbd5e1";
          e.currentTarget.style.background = "#ffffff";
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 8, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          <span style={{
            width: 8,
            height: 8,
            borderRadius: "50%",
            background: "#1e40af",
            flexShrink: 0,
          }} />
          <span style={{ overflow: "hidden", textOverflow: "ellipsis" }}>{activeModule.full}</span>
        </div>
        <ChevronDown
          size={14}
          style={{
            color: "#1e40af",
            transform: isOpen ? "rotate(180deg)" : "none",
            transition: "transform 0.15s ease",
            flexShrink: 0,
            marginLeft: 8,
          }}
        />
      </button>

      {/* Absolute Positioned Menu List */}
      {isOpen && (
        <div
          role="menu"
          style={{
            position: "absolute",
            top: "100%",
            left: 0,
            right: 0,
            marginTop: 4,
            background: "#ffffff",
            border: "1px solid #cbd5e1",
            borderRadius: 8,
            boxShadow: "0 10px 25px -5px rgba(15, 23, 42, 0.12), 0 8px 10px -6px rgba(15, 23, 42, 0.08)",
            zIndex: 50,
            overflow: "hidden",
            padding: "4px 0",
            animation: "fadeIn 0.1s ease-out",
          }}
        >
          {MODULES.map(m => {
            const isSelected = m.id === active;
            return (
              <div
                key={m.id}
                role="menuitem"
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
                  justifyContent: "space-between",
                  padding: "9px 12px",
                  fontSize: 13,
                  cursor: isSelected ? "default" : "pointer",
                  background: isSelected ? "#eff6ff" : "transparent",
                  color: isSelected ? "#1e40af" : "#0f172a",
                  fontWeight: isSelected ? 600 : 400,
                  borderLeft: isSelected ? "3px solid #1e40af" : "3px solid transparent",
                  transition: "background 0.12s ease",
                }}
                onMouseEnter={e => {
                  if (!isSelected) {
                    e.currentTarget.style.background = "#f8fafc";
                  }
                }}
                onMouseLeave={e => {
                  if (!isSelected) {
                    e.currentTarget.style.background = "transparent";
                  }
                }}
              >
                <span>{m.full}</span>
                {isSelected && <Check size={14} color="#1e40af" />}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
