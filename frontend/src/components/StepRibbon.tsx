import { Check, ChevronRight } from "lucide-react";

export interface StepConfig {
  id: string;
  label: string;
  done?: boolean;
}

export type Step = StepConfig;

export interface Props {
  steps: StepConfig[];
  currentStep: string;
  completedSteps?: string[];
  skippedSteps?: string[];
  onStepClick?: (id: string) => void;
  onSelect?: (id: string) => void;
  visitedSteps?: string[];
}

export default function StepRibbon({
  steps,
  currentStep,
  completedSteps = [],
  skippedSteps = [],
  onStepClick,
  onSelect,
  visitedSteps = [],
}: Props) {
  const activeIdx = steps.findIndex(s => s.id === currentStep);
  const handleSelect = onSelect || onStepClick;

  return (
    <nav
      className="step-ribbon"
      aria-label="Analysis pipeline steps"
      style={{
        display: "flex",
        alignItems: "center",
        gap: 6,
        overflowX: "auto",
        scrollbarWidth: "none",
        flex: 1,
      }}
    >
      {steps.map((step, idx) => {
        const isDone = Boolean(step.done || completedSteps.includes(step.id));
        const isActive = step.id === currentStep;
        const isSkipped =
          skippedSteps.includes(step.id) ||
          (!isDone && !isActive && activeIdx !== -1 && idx < activeIdx && visitedSteps.includes(step.id));
        const canClick =
          isDone || isActive || isSkipped || visitedSteps.includes(step.id) || completedSteps.includes("upload");

        const cls = `step-pill ${isActive ? "active" : isDone ? "done" : isSkipped ? "skipped" : "future"}`;

        return (
          <div key={step.id} style={{ display: "flex", alignItems: "center", gap: 6, flexShrink: 0 }}>
            {idx > 0 && (
              <ChevronRight
                size={13}
                className="step-arrow"
                style={{ color: "#94a3b8", flexShrink: 0 }}
              />
            )}
            <button
              type="button"
              className={cls}
              onClick={() => canClick && handleSelect?.(step.id)}
              data-testid={`step-${step.id}`}
              aria-current={isActive ? "step" : undefined}
              title={isSkipped ? `${step.label} (Skipped)` : step.label}
              disabled={!canClick}
              style={{
                display: "inline-flex",
                alignItems: "center",
                gap: 7,
                padding: "6px 12px",
                borderRadius: 8,
                fontSize: 12,
                fontWeight: isActive ? 600 : 500,
                cursor: canClick ? "pointer" : "not-allowed",
                transition: "all 0.15s ease",
                border: isActive
                  ? "1px solid #1e40af"
                  : isDone
                  ? "1px solid #bfdbfe"
                  : "1px solid transparent",
                background: isActive
                  ? "#1e40af"
                  : isDone
                  ? "#eff6ff"
                  : isSkipped
                  ? "#fef3c7"
                  : "transparent",
                color: isActive
                  ? "#ffffff"
                  : isDone
                  ? "#1e40af"
                  : isSkipped
                  ? "#b45309"
                  : "#64748b",
                whiteSpace: "nowrap",
                boxShadow: isActive ? "0 2px 6px rgba(30, 64, 175, 0.25)" : "none",
              }}
            >
              {/* Circular 20px badge showing checkmark when done and not active, or 1-based index */}
              <span
                className="step-num"
                style={{
                  width: 20,
                  height: 20,
                  borderRadius: "50%",
                  display: "inline-flex",
                  alignItems: "center",
                  justifyContent: "center",
                  fontSize: 11,
                  fontWeight: 700,
                  fontVariantNumeric: "tabular-nums",
                  flexShrink: 0,
                  background: isActive
                    ? "rgba(255, 255, 255, 0.25)"
                    : isDone
                    ? "#1e40af"
                    : isSkipped
                    ? "#d97706"
                    : "#e2e8f0",
                  color: isActive || isDone || isSkipped ? "#ffffff" : "#64748b",
                }}
              >
                {isDone && !isActive ? <Check size={11} strokeWidth={3} /> : idx + 1}
              </span>
              <span>{step.label}{isSkipped ? " (Skipped)" : ""}</span>
            </button>
          </div>
        );
      })}
    </nav>
  );
}
