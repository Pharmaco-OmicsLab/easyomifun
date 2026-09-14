import { Check, ChevronRight } from "lucide-react";

interface Step {
  id: string;
  label: string;
}

interface Props {
  steps: Step[];
  currentStep: string;
  completedSteps: string[];
  skippedSteps?: string[];
  onStepClick: (id: string) => void;
  visitedSteps?: string[];
}

export default function StepRibbon({ steps, currentStep, completedSteps, skippedSteps = [], onStepClick, visitedSteps = [] }: Props) {
  const activeIdx = steps.findIndex(s => s.id === currentStep);

  return (
    <div className="step-ribbon">
      {steps.map((step, idx) => {
        const isDone = completedSteps.includes(step.id);
        const isActive = step.id === currentStep;
        
        const isSkipped = skippedSteps.includes(step.id) || (!isDone && !isActive && activeIdx !== -1 && idx < activeIdx && visitedSteps.includes(step.id));
        
        const cls = `step-pill ${isActive ? "active" : isDone ? "done" : isSkipped ? "skipped" : "future"}`;

        return (
          <div key={step.id} style={{ display: "flex", alignItems: "center", gap: 4 }}>
            {idx > 0 && <ChevronRight size={12} className="step-arrow" />}
            <button
              className={cls}
              onClick={() => (isDone || isActive || isSkipped || visitedSteps.includes(step.id) || completedSteps.includes("upload")) && onStepClick(step.id)}
              data-testid={`step-${step.id}`}
              title={isSkipped ? `${step.label} (Skipped)` : step.label}
            >
              <span className="step-num">
                {isDone ? <Check size={10} /> : isSkipped ? "✕" : idx + 1}
              </span>
              {step.label}{isSkipped ? " (Skipped)" : ""}
            </button>
          </div>
        );
      })}
    </div>
  );
}