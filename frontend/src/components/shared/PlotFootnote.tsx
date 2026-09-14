import React from "react";

interface PlotFootnoteProps {
  className?: string;
  style?: React.CSSProperties;
  compact?: boolean;
}

export default function PlotFootnote({ className, style, compact = false }: PlotFootnoteProps) {
  // Updated logo size specifically to 90px for standard, and 60px for compact
  const logoSize = compact ? "60px" : "90px";
  const titleSize = compact ? "13px" : "15px";
  const textSize = compact ? "10px" : "12px";
  const gapSize = compact ? "12px" : "24px";
  const paddingSize = compact ? "12px" : "20px";

  return (
    <div
      className={className}
      style={{
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        backgroundColor: "#101a36",
        color: "#ffffff",
        padding: paddingSize,
        borderRadius: "8px",
        fontFamily: "var(--font-sans), sans-serif",
        lineHeight: "1.4",
        width: "100%",
        boxSizing: "border-box",
        marginTop: "16px",
        ...style,
      }}
    >
      {/* Centered wrapper div grouping logo and text together */}
      <div
        style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          gap: gapSize,
          flexWrap: "wrap",
          maxWidth: "750px",
          width: "100%",
          boxSizing: "border-box",
        }}
      >
        {/* Left side: Logo in a big square div */}
        <div
          style={{
            width: logoSize,
            height: logoSize,
            minWidth: logoSize,
            maxHeight: "100px", // Increased from 94px to 100px so the 90px logo fits perfectly
            borderRadius: compact ? "8px" : "12px",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            padding: compact ? "4px" : "8px",
            boxShadow: "0 4px 12px rgba(0, 0, 0, 0.2)",
          }}
        >
          <img
            src={`${import.meta.env.BASE_URL}logo.svg`}
            alt="EasyOmiFun Logo"
            style={{
              width: "100%",
              height: "100%",
              objectFit: "contain",
            }}
          />
        </div>

        {/* Right side: Detailed Lab Information (Now Left-Aligned) */}
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            gap: "2px",
            minWidth: "180px",
            maxWidth: "650px",
            flex: 1,                 // Allows the block to fill space cleanly
            textAlign: "left",       // Left-aligns standard text lines
            alignItems: "flex-start" // Left-aligns flex children (like the anchors/links)
          }}
        >
          <div
            style={{
              fontWeight: "700",
              fontSize: titleSize,
              color: "#ffffff",
            }}
          >
            CGU - The Pharmaco-Omics Lab
          </div>
          <div style={{ fontSize: textSize, color: "#d0d7e5" }}>
            Graduate Institute of Biomedical Sciences, College of Medicine, Chang Gung University, Taoyuan 333, Taiwan
          </div>
          <div style={{ fontSize: textSize, color: "#d0d7e5" }}>
            Molecular Medicine Research Center, Chang Gung University, Taoyuan 333, Taiwan
          </div>
          <div style={{ fontSize: textSize, color: "#d0d7e5" }}>
            Contact:{" "}
            <a
              href="mailto:pharmacoomicslab@gmail.com"
              style={{
                color: "#1bb1c8",
                textDecoration: "underline",
                fontWeight: "500",
              }}
            >
              Doan Trung Kien
            </a>{" "}
            (Main developer) or{" "}
            <a
              href="mailto:pharmacoomicslab@gmail.com"
              style={{
                color: "#1bb1c8",
                textDecoration: "underline",
                fontWeight: "500",
              }}
            >
              Nguyen Phuoc Long
            </a>{" "}
            (PI)
          </div>
        </div>
      </div>
    </div>
  );
}
