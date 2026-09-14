import { RotateCw } from "lucide-react";
import { useLocation } from "wouter";
import { useQueryClient } from "@tanstack/react-query";
import { useAppStore } from "../../store/appStore";
import { SERVER_URL } from "../../lib/api";

export default function ReloadButton() {
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

  return (
    <button
      onClick={handleReload}
      title="Reload App"
      data-testid="btn-reload-app"
      style={{
        display: "flex",
        alignItems: "center",
        gap: "6px",
        background: "linear-gradient(135deg, #7835ff 0%, #289ddf 100%)",
        color: "white",
        border: "1px solid #7835ff",
        borderRadius: "7px",
        padding: "6px 12px",
        fontSize: "12px",
        fontWeight: 600,
        cursor: "pointer",
        transition: "opacity 0.15s",
        flexShrink: 0,
        height: "32px",
        boxSizing: "border-box"
      }}
      onMouseEnter={e => {
        e.currentTarget.style.opacity = "0.9";
      }}
      onMouseLeave={e => {
        e.currentTarget.style.opacity = "1.0";
      }}
    >
      <RotateCw size={13} />
      <span>Reload App</span>
    </button>
  );
}
