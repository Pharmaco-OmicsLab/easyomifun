import React, { useState, useRef, useEffect } from "react";
import { useLocation } from "wouter";
import { Quote, Check, Copy } from "lucide-react";

interface PlotFootnoteProps {
  className?: string;
  style?: React.CSSProperties;
  compact?: boolean;
}

const CITATION_TEXT =
  "Kien, D. T., Dat, H. B., Phat, N. K., Tu, B. T. T., Thu, N. Q., Tan, L. N., & Long, N. P. (2026). EasyOmiFun: An Open-source Analytical Platform for Rapid Omics Data Analysis and Clinically Actionable Biomarker Discovery. CGU Pharmaco-Omics Lab. https://easyverse.app/easyomifun/";

export default function PlotFootnote({ className, style, compact = false }: PlotFootnoteProps) {
  const [location] = useLocation();
  const [copied, setCopied] = useState(false);
  const [isOpen, setIsOpen] = useState(false);
  const [isPinned, setIsPinned] = useState(false);

  const containerRef = useRef<HTMLDivElement>(null);

  const isHome = location === "/" || location === "" || location === "/index.html";

  // Close when clicking anywhere outside
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (containerRef.current && !containerRef.current.contains(event.target as Node)) {
        setIsOpen(false);
        setIsPinned(false);
      }
    }

    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, []);

  const handleCopyCitation = () => {
    navigator.clipboard.writeText(CITATION_TEXT);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  const handleClickToggle = () => {
    if (isPinned) {
      setIsPinned(false);
      setIsOpen(false);
    } else {
      setIsPinned(true);
      setIsOpen(true);
    }
  };

  const handleMouseEnter = () => {
    setIsOpen(true);
  };

  const handleMouseLeave = () => {
    if (!isPinned) {
      setIsOpen(false);
    }
  };

  // 1. Analytical workspaces: Sleek, compact single-line 36px footer bar
  if (!isHome && !compact) {
    return (
      <footer
        className={className}
        style={{
          height: "36px",
          minHeight: "36px",
          maxHeight: "36px",
          padding: "6px 20px",
          background: "#0f1f35",
          borderTop: "1px solid #1e3a8a",
          color: "#cbd5e1",
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          fontSize: "12px",
          boxSizing: "border-box",
          position: "relative",
          zIndex: 40,
          ...style,
        }}
      >
        {/* Left: Logos & Attribution */}
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <img
            src={`${import.meta.env.BASE_URL}logo.svg`}
            alt="EasyOmiFun Logo"
            style={{ height: "20px", width: "auto", objectFit: "contain" }}
          />
          <span style={{ fontWeight: 600, color: "#ffffff" }}>EasyOmiFun</span>
          <span style={{ color: "#64748b" }}>·</span>
          <span style={{ color: "#94a3b8" }}>CGU · The Pharmaco-Omics Lab</span>
        </div>

        {/* Right: Citation popover trigger */}
        <div
          ref={containerRef}
          onMouseEnter={handleMouseEnter}
          onMouseLeave={handleMouseLeave}
          style={{ position: "relative" }}
        >
          <button
            type="button"
            onClick={handleClickToggle}
            style={{
              display: "inline-flex",
              alignItems: "center",
              gap: 5,
              background: isPinned || isOpen ? "rgba(255, 255, 255, 0.16)" : "rgba(255, 255, 255, 0.08)",
              border: isPinned ? "1px solid #67e8f9" : "1px solid rgba(255, 255, 255, 0.15)",
              borderRadius: "4px",
              padding: "2px 8px",
              color: "#cbd5e1",
              fontSize: "11px",
              cursor: "pointer",
              transition: "all 0.15s ease",
            }}
          >
            <Quote size={11} color="#67e8f9" />
            <span>Cite</span>
          </button>

          {/* Citation Popover */}
          {isOpen && (
            <div
              style={{
                position: "absolute",
                bottom: "calc(100% + 8px)",
                right: 0,
                width: "360px",
                background: "#0f172a",
                border: "1px solid #334155",
                borderRadius: "8px",
                padding: "12px 14px",
                boxShadow: "0 10px 25px -5px rgba(0, 0, 0, 0.5)",
                zIndex: 100,
                color: "#e2e8f0",
                fontSize: "12px",
                lineHeight: "1.5",
              }}
            >
              {/* Invisible hit bridge covering the 8px gap */}
              <div
                style={{
                  position: "absolute",
                  bottom: "-8px",
                  left: 0,
                  right: 0,
                  height: "8px",
                }}
              />

              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 6 }}>
                <span style={{ fontWeight: 700, color: "#ffffff", fontSize: "12px" }}>
                  Citation
                </span>
                <button
                  type="button"
                  onClick={handleCopyCitation}
                  style={{
                    display: "inline-flex",
                    alignItems: "center",
                    gap: 4,
                    background: copied ? "#15803d" : "#1e293b",
                    color: "#ffffff",
                    border: "1px solid #475569",
                    borderRadius: "4px",
                    padding: "2px 7px",
                    fontSize: "11px",
                    cursor: "pointer",
                  }}
                >
                  {copied ? <Check size={11} /> : <Copy size={11} />}
                  <span>{copied ? "Copied!" : "Copy"}</span>
                </button>
              </div>
              <p style={{ margin: 0, color: "#cbd5e1", fontStyle: "italic" }}>
                {CITATION_TEXT}
              </p>
            </div>
          )}
        </div>
      </footer>
    );
  }

  // 2. Home Landing pages: Full dark institutional footer
  return (
    <footer
      className={className}
      style={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
        justifyContent: "center",
        gap: "32px",
        borderTop: "2px solid #5eead4",
        backgroundColor: "#0f1f35",
        color: "#cbd5e1",
        padding: "36px 24px",
        fontFamily: "var(--font-sans), sans-serif",
        lineHeight: "1.5",
        width: "100%",
        boxSizing: "border-box",
        marginTop: "auto",
        ...style,
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          alignItems: "center",
          justifyContent: "center",
          gap: "36px",
          flexWrap: "wrap",
          maxWidth: "850px",
          width: "100%",
        }}
      >
        <img
          src={`${import.meta.env.BASE_URL}logo.svg`}
          alt="Pharmaco-Omics Lab"
          style={{ height: "6.5rem", width: "auto", objectFit: "contain" }}
        />
        <div style={{ textAlign: "left", flex: 1, minWidth: "260px" }}>
          <h4 style={{ margin: "0 0 6px 0", fontSize: "17px", fontWeight: 700, color: "#ffffff" }}>
            <a
              href="https://pharmacoomics.com/"
              target="_blank"
              rel="noopener noreferrer"
              style={{ color: "#ffffff", textDecoration: "none" }}
              onMouseEnter={e => { (e.currentTarget as HTMLAnchorElement).style.textDecoration = "underline"; }}
              onMouseLeave={e => { (e.currentTarget as HTMLAnchorElement).style.textDecoration = "none"; }}
            >
              CGU - The Pharmaco-Omics Lab
            </a>
          </h4>
          <p style={{ margin: "3px 0", fontSize: "13px", color: "#cbd5e1" }}>
            Graduate Institute of Biomedical Sciences, College of Medicine, Chang Gung University, Taoyuan 333, Taiwan
          </p>
          <p style={{ margin: "3px 0", fontSize: "13px", color: "#cbd5e1" }}>
            Molecular Medicine Research Center, Chang Gung University, Taoyuan 333, Taiwan
          </p>
          <p style={{ margin: "3px 0", fontSize: "13px", color: "#cbd5e1" }}>
            Contact:{" "}
            <a
              href="mailto:pharmacoomicslab@gmail.com"
              style={{ color: "#67e8f9", textDecoration: "underline" }}
            >
              Doan Trung Kien
            </a>{" "}
            (Main developer) or{" "}
            <a
              href="mailto:pharmacoomicslab@gmail.com"
              style={{ color: "#67e8f9", textDecoration: "underline" }}
            >
              Nguyen Phuoc Long
            </a>{" "}
            (PI)
          </p>
          <p style={{ margin: "4px 0 0 0", fontSize: "13px", color: "#cbd5e1" }}>
            Explore more of our apps at{" "}
            <a
              href="https://easyverse.app"
              target="_blank"
              rel="noopener noreferrer"
              style={{ color: "#67e8f9", fontWeight: 600, textDecoration: "underline" }}
            >
              easyverse.app →
            </a>
          </p>
        </div>
      </div>
    </footer>
  );
}