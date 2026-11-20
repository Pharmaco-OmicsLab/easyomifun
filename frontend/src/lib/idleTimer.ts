import { SERVER_URL, isAnyJobExecuting } from "./api";
import { isDesktopMode } from "./dataParser";

const IDLE_TIMEOUT_MS = 15 * 60 * 1000; // 15 minutes

type DisconnectHandler = () => void;

class IdleSessionManager {
  private lastActionTime = Date.now();
  private timer: number | null = null;
  private isDisconnected = false;
  private listeners: DisconnectHandler[] = [];
  private boundHandler: () => void;

  constructor() {
    let lastThrottled = 0;
    this.boundHandler = () => {
      const now = Date.now();
      if (now - lastThrottled > 1000) {
        lastThrottled = now;
        this.resetTimer();
      }
    };
  }

  public async init() {
    if (typeof window === "undefined") return;

    // 15-minute hanging idle timeout only applies in server mode (the default mode)
    if (isDesktopMode() || import.meta.env.VITE_ELECTRON === "true") {
      return;
    }

    try {
      const res = await fetch(`${SERVER_URL}/api/app-mode`);
      if (res.ok) {
        const data = await res.json();
        if (data.mode === "desktop") return;
      }
    } catch (_) {}

    const events = ["mousedown", "keydown", "scroll", "touchstart", "mousemove"];
    events.forEach((evt) => window.addEventListener(evt, this.boundHandler, { passive: true }));

    this.timer = window.setInterval(() => this.checkIdle(), 10000);
  }

  public onDisconnect(handler: DisconnectHandler) {
    this.listeners.push(handler);
    return () => {
      this.listeners = this.listeners.filter((h) => h !== handler);
    };
  }

  public resetTimer() {
    if (this.isDisconnected) return;
    this.lastActionTime = Date.now();
  }

  private checkIdle() {
    if (this.isDisconnected) return;

    // Do NOT disconnect while background/compute job is active
    if (isAnyJobExecuting()) {
      this.lastActionTime = Date.now();
      return;
    }

    if (Date.now() - this.lastActionTime >= IDLE_TIMEOUT_MS) {
      this.triggerDisconnect();
    }
  }

  public triggerDisconnect() {
    if (this.isDisconnected) return;
    this.isDisconnected = true;

    if (this.timer) {
      window.clearInterval(this.timer);
      this.timer = null;
    }

    const userId = sessionStorage.getItem("easyomifun_user_id") || "";
    if (userId) {
      try {
        navigator.sendBeacon(`${SERVER_URL}/api/cleanup?userId=${encodeURIComponent(userId)}`);
      } catch (_) {
        fetch(`${SERVER_URL}/api/cleanup?userId=${encodeURIComponent(userId)}`, { method: "POST" }).catch(() => {});
      }
    }

    sessionStorage.clear();
    localStorage.removeItem("easyomifun_user_id");

    this.listeners.forEach((fn) => {
      try {
        fn();
      } catch (err) {
        console.error("Error in idle disconnect handler:", err);
      }
    });
  }

  public destroy() {
    if (typeof window === "undefined") return;
    const events = ["mousedown", "keydown", "scroll", "touchstart", "mousemove"];
    events.forEach((evt) => window.removeEventListener(evt, this.boundHandler));
    if (this.timer) {
      window.clearInterval(this.timer);
      this.timer = null;
    }
  }
}

export const idleManager = new IdleSessionManager();
