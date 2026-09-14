import { clsx, type ClassValue } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

/**
 * Safely format a numerical metric (AUC, Balanced Accuracy, PPV, NPV, importance, etc.)
 * Handles numbers, numeric strings, arrays (from unboxed R data), undefined, null, and NaN.
 */
export function formatMetric(val: any, fallback: number | string = 0.5, digits = 3): string {
  if (val === undefined || val === null || val === "") {
    return typeof fallback === "number" ? fallback.toFixed(digits) : String(fallback);
  }
  const raw = Array.isArray(val) ? val[0] : val;
  if (raw === undefined || raw === null || raw === "") {
    return typeof fallback === "number" ? fallback.toFixed(digits) : String(fallback);
  }
  const num = Number(raw);
  if (isNaN(num)) {
    return typeof raw === "string" ? raw : (typeof fallback === "number" ? fallback.toFixed(digits) : String(fallback));
  }
  return num.toFixed(digits);
}

/**
 * Safely format a confidence interval string like "[0.812, 0.945]".
 * Handles pre-formatted strings, lower/upper bounds as numbers/strings/arrays, or missing data.
 */
export function formatCi(ci: any, ciLower?: any, ciUpper?: any, digits = 3): string {
  if (ci && typeof ci === "string" && ci.trim() !== "" && ci !== "N/A" && ci !== "NA" && ci !== "NA - NA") {
    return ci;
  }
  const rawLower = Array.isArray(ciLower) ? ciLower[0] : ciLower;
  const rawUpper = Array.isArray(ciUpper) ? ciUpper[0] : ciUpper;
  const l = rawLower !== undefined && rawLower !== null && rawLower !== "" ? Number(rawLower) : NaN;
  const u = rawUpper !== undefined && rawUpper !== null && rawUpper !== "" ? Number(rawUpper) : NaN;
  if (!isNaN(l) && !isNaN(u)) {
    return `[${l.toFixed(digits)}, ${u.toFixed(digits)}]`;
  }
  return "—";
}

