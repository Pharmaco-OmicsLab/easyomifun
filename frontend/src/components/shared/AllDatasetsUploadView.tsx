import { useState } from "react";
import { CheckCircle, AlertCircle, Upload } from "lucide-react";
import DiscardWarnModal from "./DiscardWarnModal";
import { getExpressionSampleColumns } from "../../lib/dataParser";

export interface UploadSummaryDataset {
  id: string;
  name: string;
  color: string;
  expressionFileName: string;
  nFeatures: number;
  nSamples: number;
  sampleIds?: string[];
  matchingSamples?: string[];
  geneIdType?: string;
  isNormalized?: boolean;
  clinicalFileName?: string;
  dataType?: string;
  clinicalSampleIdCol?: string;
  clinicalGroupCol?: string;
  clinicalBatchCol?: string;
  clinicalOtherCovariates?: string[];
  clinicalParsedData?: string[][];
  columns?: string[];
  clinicalColumns?: string[];
  submittedClinicalSampleIdCol?: string;
  submittedClinicalGroupCol?: string;
  submittedClinicalBatchCol?: string;
  submittedClinicalOtherCovariates?: string[];
  geneIdCol?: string;
  geneInfoCols?: string[];
}

interface Props {
  title?: string;
  subtitle?: string;
  datasets: UploadSummaryDataset[];
  onDatasetClick: (id: string) => void;
  onContinue: () => void;
  continueLabel?: string;
  onBack?: () => void;
  backLabel?: string;
  hasStepMismatch?: boolean;
  onConfirmDiscard?: () => void;
  showClinicalDetails?: boolean;
  stepId?: string;
  datasetIds?: string[];
  onSkip?: () => void;
  isContinueDisabled?: boolean;
}

export default function AllDatasetsUploadView({
  title = "All Datasets — Upload Summary",
  subtitle,
  datasets,
  onDatasetClick,
  onContinue,
  continueLabel = "Continue →",
  onBack,
  backLabel = "← Back",
  hasStepMismatch = false,
  onConfirmDiscard,
  showClinicalDetails = false,
  stepId,
  datasetIds,
  onSkip,
  isContinueDisabled = false,
}: Props) {
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const allDone = datasets.every(d => d.expressionFileName !== "");

  const handleContinue = () => {
    if (hasStepMismatch) {
      setShowDiscardModal(true);
    } else {
      onContinue();
    }
  };

  const handleConfirmDiscard = () => {
    setShowDiscardModal(false);
    if (onConfirmDiscard) {
      onConfirmDiscard();
    } else {
      onContinue();
    }
  };

  const getOverlappingSamplesCount = (ds: any) => {
    if (ds.matchingSamples && ds.matchingSamples.length > 0) return ds.matchingSamples.length;
    if (!ds.clinicalParsedData || !ds.columns) return 0;
    const sampleIdCol = ds.clinicalSampleIdCol || ds.submittedClinicalSampleIdCol || "";
    if (!sampleIdCol) return 0;
    const cols = ds.clinicalColumns || [];
    const sampleIdIdx = cols.findIndex((c: string) => c.toLowerCase() === sampleIdCol.toLowerCase());
    if (sampleIdIdx === -1) return 0;
    
    const geneIdCol = ds.geneIdCol || "";
    const geneInfoCols = ds.geneInfoCols || [];
    const exprSamples = (ds.sampleIds && ds.sampleIds.length > 0)
      ? ds.sampleIds
      : getExpressionSampleColumns(ds.columns || [], geneIdCol, geneInfoCols);
    
    const clinSamples = ds.clinicalParsedData
      .map((row: any) => row && row[sampleIdIdx] ? String(row[sampleIdIdx]) : "")
      .filter(Boolean);
    const matchingSamples = clinSamples.filter((s: string) => exprSamples.includes(s));
    return matchingSamples.length;
  };

  return (
    <>
      {showDiscardModal && (
        <DiscardWarnModal
          stepId={stepId}
          datasetIds={datasetIds}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={handleConfirmDiscard}
        />
      )}

      <div className="card">
        <div className="card-title" style={{ marginBottom: 4 }}>{title}</div>
        {subtitle && <div className="card-sub">{subtitle}</div>}
        <hr className="card-divider" />

        <div className="all-ds-grid">
          {datasets.map(ds => {
            const hasData = ds.expressionFileName !== "";
            return (
              <div
                key={ds.id}
                className={`all-ds-card ${!hasData ? "no-data" : ""}`}
                onClick={() => onDatasetClick(ds.id)}
                title={hasData ? "Click to view dataset" : "Click to upload data for this dataset"}
                data-testid={`all-ds-card-${ds.id}`}
              >
                <div className="all-ds-card-header">
                  <div className="all-ds-dot" style={{ background: ds.color }} />
                  <div className="all-ds-name">{ds.name}</div>
                  {hasData
                    ? <CheckCircle size={14} style={{ color: "hsl(150 60% 35%)", flexShrink: 0 }} />
                    : <AlertCircle size={14} style={{ color: "hsl(220 9% 62%)", flexShrink: 0 }} />
                  }
                </div>

                {hasData ? (
                  showClinicalDetails ? (
                    <div style={{ display: "flex", flexDirection: "column", gap: 6, fontSize: 12, marginTop: 8 }}>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Samples (Overlap):</span>
                        <strong style={{ color: "var(--foreground)" }}>{getOverlappingSamplesCount(ds)}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Features:</span>
                        <strong style={{ color: "var(--foreground)" }}>{ds.nFeatures.toLocaleString()}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Type of Data:</span>
                        <strong style={{ color: "var(--foreground)", textTransform: "capitalize" }}>{ds.dataType || "—"}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Normalized:</span>
                        <strong style={{ color: "var(--foreground)" }}>{ds.isNormalized ? "Yes" : "Raw"}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Group Column:</span>
                        <strong style={{ color: "var(--foreground)" }}>{ds.clinicalGroupCol || ds.submittedClinicalGroupCol || "—"}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between", borderBottom: "1px dashed #e1e8ff", paddingBottom: 4 }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Batch Column:</span>
                        <strong style={{ color: "var(--foreground)" }}>{ds.clinicalBatchCol || ds.submittedClinicalBatchCol || "—"}</strong>
                      </div>
                      <div style={{ display: "flex", justifyContent: "space-between" }}>
                        <span style={{ color: "hsl(220 9% 50%)" }}>Other Covariates:</span>
                        <strong style={{ color: "var(--foreground)", textAlign: "right" }}>
                          {(() => {
                            const covs = ds.clinicalOtherCovariates || ds.submittedClinicalOtherCovariates || [];
                            return covs.length > 0 ? covs.join(", ") : "None";
                          })()}
                        </strong>
                      </div>
                    </div>
                  ) : (
                    <>
                      <div className="all-ds-stat-grid">
                        <div className="all-ds-stat">
                          <div className="all-ds-stat-val">{ds.nFeatures.toLocaleString()}</div>
                          <div className="all-ds-stat-lbl">Features</div>
                        </div>
                        <div className="all-ds-stat">
                          <div className="all-ds-stat-val">{ds.nSamples}</div>
                          <div className="all-ds-stat-lbl">Samples</div>
                        </div>
                        <div className="all-ds-stat">
                          <div className="all-ds-stat-val" style={{ fontSize: 12, textTransform: "capitalize" }}>
                            {ds.dataType || "—"}
                          </div>
                          <div className="all-ds-stat-lbl">Data Type</div>
                        </div>
                        <div className="all-ds-stat">
                          <div className="all-ds-stat-val" style={{ fontSize: 12 }}>
                            {ds.isNormalized ? "Yes" : "Raw"}
                          </div>
                          <div className="all-ds-stat-lbl">Normalized</div>
                        </div>
                      </div>
                      <div style={{ marginTop: 10, fontSize: 11, color: "hsl(220 9% 52%)" }}>
                        📄 {ds.expressionFileName}
                        {ds.clinicalFileName && (
                           <span style={{ marginLeft: 8 }}>· 🧬 {ds.clinicalFileName}</span>
                        )}
                      </div>
                    </>
                  )
                ) : (
                  <div style={{
                    display: "flex", flexDirection: "column", alignItems: "center", justifyContent: "center",
                    height: 80, color: "hsl(220 9% 60%)", gap: 8,
                  }}>
                    <Upload size={22} style={{ opacity: 0.5 }} />
                    <span style={{ fontSize: 12 }}>Click to upload data</span>
                  </div>
                )}
              </div>
            );
          })}
        </div>
      </div>

      {/* Status summary */}
      {!showClinicalDetails && (
        <div className="card">
          <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
            <div className="stat-chip">
              <div className="stat-chip-val">{datasets.length}</div>
              <div className="stat-chip-lbl">Total Datasets</div>
            </div>
          </div>

          {!allDone && (
            <div className="banner warn" style={{ marginTop: 12, marginBottom: 0 }}>
              ⚠️ <strong>{datasets.filter(d => !d.expressionFileName).length} dataset(s)</strong> still need expression data uploaded before continuing.
            </div>
          )}
          {allDone && (
            <div className="banner success" style={{ marginTop: 12, marginBottom: 0 }}>
              ✅ All datasets have expression data. You can continue to the next step.
            </div>
          )}
        </div>
      )}

        <div className="action-row" style={{ display: "flex", gap: 8, justifyContent: "flex-end", alignItems: "center" }}>
          {onBack && (
            <button 
              className="btn btn-default" 
              onClick={onBack} 
              data-testid="btn-all-ds-back"
            >
              {backLabel}
            </button>
          )}

          {onSkip && (
            <button 
              className="btn btn-default" 
              onClick={onSkip} 
              data-testid="btn-all-ds-skip" 
              style={{ color: "var(--muted-foreground)" }}
            >
              {stepId === "batch" ? "Skip Batch Correction" : "Skip"}
            </button>
          )}

          <button 
            className="btn btn-primary" 
            disabled={!allDone || isContinueDisabled} 
            onClick={handleContinue} 
            data-testid="btn-all-ds-continue"
          >
            {continueLabel}
          </button>
        </div>
    </>
  );
}