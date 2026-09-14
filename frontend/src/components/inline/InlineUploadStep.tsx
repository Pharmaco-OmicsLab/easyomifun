import { useState, useEffect } from "react";
import { FileText, X, AlertTriangle, CheckCircle, Info, Download } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import { computeDPSteps, isMissingClinical } from "../../dataObject";
import { parseClinicalFile, alignSamplesLocal, unparseCSV, getDesktopFilePath, getExpressionSampleColumns, isAnnotationOrInfoColumn } from "../../lib/dataParser";
import { uploadFileInChunks } from "../../lib/chunkUploader";
import { fetchSampleDataAPI, supplementClinicalDataAPI } from "../../lib/api";
import WarnSampleModal from "../shared/WarnSampleModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import SkipModal from "../shared/SkipModal";

interface Props {
  datasetId: string;
  mode: "de" | "fs" | "batch";
  editingFromSummary?: boolean;
  onBack?: () => void;
  onContinue?: () => void;
}

const filterDatasetForUpload = (dataset: any, choice: "all" | "overlap", mode: "de" | "fs" | "batch") => {
  if (!dataset.clinicalFileName || !dataset.clinicalSampleIdCol) {
    return dataset;
  }

  if (choice === "all") {
    return {
      ...dataset,
      clinicalFile: null,
      clinicalFileName: "",
      clinicalSampleIdCol: "",
      clinicalGroupCol: "",
      clinicalBatchCol: "",
      clinicalColumns: [],
      clinicalParsedData: [],
    };
  }

  const clinicalCols = dataset.clinicalColumns || [];
  const clinicalRows = dataset.clinicalParsedData || [];
  const clinSampleIdCol = dataset.clinicalSampleIdCol;
  const clinGroupCol = mode !== "batch" ? dataset.clinicalGroupCol : "";
  const clinBatchCol = mode === "batch" ? dataset.clinicalBatchCol : "";

  const sampleIdx = clinicalCols.indexOf(clinSampleIdCol);
  const groupIdx = clinGroupCol ? clinicalCols.indexOf(clinGroupCol) : -1;
  const batchIdx = clinBatchCol ? clinicalCols.indexOf(clinBatchCol) : -1;

  const checkIdxs = [sampleIdx, groupIdx, batchIdx].filter(idx => idx !== -1);
  const isValMissing = (val: any) => {
    return val === undefined || val === null || String(val).trim() === "" || String(val) === "NA" || String(val) === "NaN";
  };

  const filteredClinicalRows = clinicalRows.filter((row: any[]) => {
    for (const idx of checkIdxs) {
      if (idx >= row.length || isValMissing(row[idx])) {
        return false;
      }
    }
    return true;
  });

  const validClinicalSamples = filteredClinicalRows.map((row: any[]) => row[sampleIdx]).filter(Boolean);

  let keepCols = dataset.columns || [];
  let filteredExpressionRows = dataset.parsedData || [];
  let nSamples = dataset.nSamples || 0;

  const isFsHeaders = dataset.fs_featuresOrientation === "headers";

  if (isFsHeaders) {
    const sampleIdColIdx = (dataset.fs_featureIndexValue || 1) - 1;
    filteredExpressionRows = (dataset.parsedData || []).filter((row: any[]) => {
      const rowSampleId = row[sampleIdColIdx];
      return rowSampleId && validClinicalSamples.includes(rowSampleId);
    });
    const commonSamples = filteredExpressionRows.map((row: any[]) => row[sampleIdColIdx]);
    nSamples = commonSamples.length;
  } else {
    const geneIdCol = dataset.geneIdCol || (dataset.columns && dataset.columns[0]) || "";
    const geneInfoCols = dataset.geneInfoCols || [];
    
    const fsFeatureIdx = dataset.fs_featureIndexValue !== undefined ? (dataset.fs_featureIndexValue - 1) : -1;
    
    const exprSampleCols = (dataset.sampleIds && dataset.sampleIds.length > 0)
      ? dataset.sampleIds
      : getExpressionSampleColumns(dataset.columns || [], geneIdCol, geneInfoCols);
    
    const commonSamples = exprSampleCols.filter((s: string) => validClinicalSamples.includes(s));

    keepCols = (dataset.columns || []).filter((c: string, idx: number) => {
      if (idx === fsFeatureIdx) return true;
      if (isAnnotationOrInfoColumn(c, geneIdCol, geneInfoCols)) return true;
      return commonSamples.includes(c);
    });

    const keepIndices = keepCols.map((c: string) => (dataset.columns || []).indexOf(c));
    
    if (dataset.uploadDone) {
      filteredExpressionRows = (dataset.parsedData || []).map((row: any[], rIdx: number) => {
        if (!row) return row;
        if (rIdx < 10) {
          return keepIndices.map(idx => row[idx]);
        }
        // Keep it minimized
        const newRow: any[] = [];
        const geneIdIdx = dataset.columns ? dataset.columns.indexOf(geneIdCol) : -1;
        const targetIdx = geneIdIdx !== -1 ? geneIdIdx : 0;
        const keepGeneIdIdx = keepCols.indexOf(geneIdCol);
        const newTargetIdx = keepGeneIdIdx !== -1 ? keepGeneIdIdx : 0;
        newRow[newTargetIdx] = row[targetIdx] !== undefined ? row[targetIdx] : row[0];
        return newRow;
      });
    } else {
      filteredExpressionRows = (dataset.parsedData || []).map((row: any[]) => {
        return keepIndices.map(idx => row[idx]);
      });
    }
    
    nSamples = commonSamples.length;
  }

  const finalClinicalRows = filteredClinicalRows.filter((row: any[]) => {
    return validClinicalSamples.includes(row[sampleIdx]);
  });

  return {
    ...dataset,
    columns: keepCols,
    sampleIds: isFsHeaders ? dataset.sampleIds : ((dataset.sampleIds && dataset.sampleIds.length > 0) ? dataset.sampleIds.filter((s: string) => validClinicalSamples.includes(s)) : keepCols.filter((c: string) => !isAnnotationOrInfoColumn(c, dataset.geneIdCol, dataset.geneInfoCols))),
    parsedData: filteredExpressionRows,
    clinicalParsedData: finalClinicalRows,
    nSamples: nSamples,
  };
};

export default function InlineUploadStep({ datasetId, mode, editingFromSummary = false, onBack, onContinue, }: Props) {
  const { state, dispatch } = useAppStore();
  const ds = state.dpDatasets.find(d => d.id === datasetId);
  const [loading, setLoading] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const [clinicalDrag, setClinicalDrag] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);
  const [showChangeWarnModal, setShowChangeWarnModal] = useState(false);
  const [showSkipModal, setShowSkipModal] = useState(false);

  const [matchingSamples, setMatchingSamples] = useState<string[]>([]);
  const [missingClinSamples, setMissingClinSamples] = useState<string[]>([]);
  const [hasMissingClinicalValues, setHasMissingClinicalValues] = useState<boolean>(false);
  const [removedSamples, setRemovedSamples] = useState<string[]>([]);
  const [minGroupSampleSize, setMinGroupSampleSize] = useState<number | undefined>(undefined);

  useEffect(() => {
    if (!ds) return;
    const currentDs = ds;
    if (ds.uploadDone && ds.matchingSamples && (!currentDs.parsedData || currentDs.parsedData.length === 0)) {
      setMatchingSamples(ds.matchingSamples);
      setMissingClinSamples(ds.missingClinSamples || []);
      setHasMissingClinicalValues(ds.hasMissingClinicalValues || false);
      setRemovedSamples(ds.removedSamples || []);
      setMinGroupSampleSize(ds.minGroupSampleSize);
      return;
    }
    function performAlignment() {
      if (!currentDs.expressionFileName) {
        setMatchingSamples([]);
        setMissingClinSamples([]);
        setHasMissingClinicalValues(false);
        setRemovedSamples([]);
        return;
      }
      try {
        const exprSamples = (currentDs.sampleIds && currentDs.sampleIds.length > 0)
          ? currentDs.sampleIds
          : getExpressionSampleColumns(currentDs.columns || [], currentDs.geneIdCol, currentDs.geneInfoCols);
        const res = alignSamplesLocal({
          expressionColumns: exprSamples,
          geneIdCol: "",
          geneInfoCols: [],
          clinicalParsedData: currentDs.clinicalParsedData || [],
          clinicalColumns: currentDs.clinicalColumns || [],
          clinicalSampleIdCol: currentDs.clinicalSampleIdCol || "",
          geneIdType: currentDs.geneIdType || "",
          clinicalGroupCol: mode !== "batch" ? (currentDs.clinicalGroupCol || "") : "",
          clinicalBatchCol: mode === "batch" ? (currentDs.clinicalBatchCol || "") : "",
          clinicalOtherCovariates: mode === "batch" ? (currentDs.clinicalOtherCovariates || []) : []
        });
        setMatchingSamples(res.commonSamples || []);
        setMissingClinSamples(res.missingClinicalSamples || []);
        setHasMissingClinicalValues(res.hasMissingClinicalValues || false);
        setRemovedSamples(res.removedSamplesDueToMissing || []);
        setMinGroupSampleSize(res.minGroupSampleSize);
      } catch (err) {
        console.error("Error performing local sample alignment:", err);
      }
    }
    performAlignment();
  }, [
    ds?.expressionFileName,
    ds?.clinicalFileName,
    ds?.geneIdCol,
    JSON.stringify(ds?.geneInfoCols || []),
    JSON.stringify(ds?.sampleIds || []),
    ds?.clinicalSampleIdCol,
    ds?.clinicalGroupCol,
    ds?.clinicalBatchCol,
    JSON.stringify(ds?.clinicalColumns || []),
    JSON.stringify(ds?.clinicalParsedData || []),
    JSON.stringify(ds?.columns || [])
  ]);

  if (!ds) return null;

  const updateDs = (patch: Partial<typeof ds>) => {
    dispatch({ type: "DP_UPDATE_DATASET", id: datasetId, patch } as never);
  };

  const handleClear = async () => {
    try {
      await clearDatasetAPI(ds.id, "clinical");
    } catch (e) {
      console.warn("clearDatasetAPI failed (non-fatal):", e);
    }
    dispatch({
      type: "RESET_DOWNSTREAM_STEPS",
      datasetId: ds.id,
      fromStep: "upload"
    } as any);
    updateDs({
      clinicalFile: null,
      clinicalFileName: "",
      clinicalFilePath: undefined,
      clinicalUploadId: undefined,
      clinicalRawText: "",
      submittedClinicalFileName: "",
      clinicalSampleIdCol: "",
      clinicalGroupCol: "",
      clinicalBatchCol: "",
      fs_positiveClass: "",
      fs_negativeClass: "",
      submitted_fs_positiveClass: "",
      submitted_fs_negativeClass: "",
      minGroupSampleSize: undefined,
      clinicalColumns: undefined,
      clinicalParsedData: undefined,
      deUploadBypassed: false,
      fsUploadBypassed: false,
      integrityOk: true,
      integrityIssues: []
    });
  };

  const handleLoadSample = async () => {
    setLoading(true);
    setLoadingMsg("Loading sample clinical data…");
    try {
      const result = await fetchSampleDataAPI("clinical");
      const rows = result.parsedData as string[][];
      const rawText = unparseCSV(result.columns, rows);
      const groupCol = result.columns[1] || "Group";
      const gIdx = result.columns.indexOf(groupCol);
      let autoPos = "";
      let autoNeg = "";
      if (gIdx !== -1 && rows) {
        const vals = [...new Set(
          rows.map((row: any[]) => String(row[gIdx] ?? "").trim())
            .filter(v => v !== "" && v !== "NA" && v !== "NaN")
        )];
        if (vals.length === 2) {
          autoPos = vals[1];
          autoNeg = vals[0];
        }
      }
      updateDs({
        clinicalFileName: "sample_clinical.csv",
        clinicalRawText: rawText,
        clinicalSampleIdCol: result.columns[0] || "SampleID",
        clinicalGroupCol: groupCol,
        clinicalBatchCol: mode === "batch" ? (result.columns[4] || "Batch") : "",
        clinicalColumns: result.columns,
        clinicalParsedData: rows,
        fs_positiveClass: mode === "fs" ? autoPos : ds.fs_positiveClass,
        fs_negativeClass: mode === "fs" ? autoNeg : ds.fs_negativeClass,
        deUploadBypassed: false,
        fsUploadBypassed: false,
      });
    } catch (err: any) {
      updateDs({ integrityIssues: [`Failed to load example data: ${err.message}`] });
    } finally {
      setLoading(false);
    }
  };

  const handleClinicalFile = async (file: File) => {
    setLoadingMsg(`Parsing ${file.name}…`);
    setLoading(true);
    try {
      const desktopPath = getDesktopFilePath(file);
      const isDesktop = !!desktopPath;

      const parsed = await parseClinicalFile(file);

      let uploadId: string | undefined = undefined;
      if (!isDesktop) {
        setLoadingMsg(`Uploading ${file.name} in chunks…`);
        uploadId = await uploadFileInChunks(file, ds.id, "clinical", (pct) => {
          setLoadingMsg(`Uploading ${file.name} (${pct}%)…`);
        });
      }

      let autoPos = "";
      let autoNeg = "";
      if (parsed.clinicalGroupCol && parsed.clinicalColumns && parsed.clinicalParsedData) {
        const gIdx = parsed.clinicalColumns.indexOf(parsed.clinicalGroupCol);
        if (gIdx !== -1) {
          const vals = [...new Set(
            parsed.clinicalParsedData
              .map((row: any[]) => String(row[gIdx] ?? "").trim())
              .filter(v => v !== "" && v !== "NA" && v !== "NaN")
          )];
          if (vals.length === 2) {
            autoPos = vals[1];
            autoNeg = vals[0];
          }
        }
      }

      updateDs({
        clinicalFile: file,
        clinicalFileName: file.name,
        clinicalFilePath: desktopPath || undefined,
        clinicalUploadId: uploadId,
        clinicalRawText: "",
        clinicalSampleIdCol: parsed.clinicalSampleIdCol,
        clinicalGroupCol: parsed.clinicalGroupCol,
        clinicalColumns: parsed.clinicalColumns,
        clinicalParsedData: parsed.clinicalParsedData,
        fs_positiveClass: mode === "fs" ? autoPos : ds.fs_positiveClass,
        fs_negativeClass: mode === "fs" ? autoNeg : ds.fs_negativeClass,
        deUploadBypassed: false,
        fsUploadBypassed: false,
        uploadDone: false,
      });
    } catch (error: any) {
      updateDs({
        integrityIssues: [`Error parsing clinical file: ${error.message}`],
      });
    } finally {
      setLoading(false);
    }
  };

  // Samples matching comparison logic
  const exprSamples = (ds.sampleIds && ds.sampleIds.length > 0)
    ? ds.sampleIds
    : getExpressionSampleColumns(ds.columns || [], ds.geneIdCol, ds.geneInfoCols);
  const sampleIdIdx = ds.clinicalFileName && ds.clinicalSampleIdCol && ds.clinicalColumns
    ? ds.clinicalColumns.indexOf(ds.clinicalSampleIdCol)
    : -1;
  const clinSamples = sampleIdIdx !== -1 && ds.clinicalParsedData
    ? ds.clinicalParsedData.map(row => row && row[sampleIdIdx]).filter(Boolean)
    : [];

  const hasClinical = !!ds.clinicalFileName;

  const isNoOverlap = hasClinical && exprSamples.length > 0 && matchingSamples.length === 0;
  const isPartialMatch = hasClinical && exprSamples.length > 0 && matchingSamples.length > 0 && (matchingSamples.length !== exprSamples.length || clinSamples.length !== exprSamples.length);

  // Derive unique class count from the selected group column (FS only)
  const uniqueGroupValues: string[] = (() => {
    if (mode !== "fs" || !ds.clinicalGroupCol || !ds.clinicalColumns || !ds.clinicalParsedData) return [];
    const gIdx = ds.clinicalColumns.indexOf(ds.clinicalGroupCol);
    if (gIdx === -1) return [];
    const vals = ds.clinicalParsedData
      .map((row: any[]) => String(row[gIdx] ?? "").trim())
      .filter(v => v !== "" && v !== "NA" && v !== "NaN");
    return [...new Set(vals)];
  })();
  const uniqueGroupCount = uniqueGroupValues.length;
  const hasMultipleClasses = mode === "fs" && ds.clinicalGroupCol !== "" && uniqueGroupCount !== 2;
  const hasUnsetClasses = mode === "fs" && ds.clinicalGroupCol !== "" && (
    !ds.fs_positiveClass ||
    !ds.fs_negativeClass ||
    ds.fs_positiveClass === ds.fs_negativeClass ||
    !uniqueGroupValues.includes(ds.fs_positiveClass) ||
    !uniqueGroupValues.includes(ds.fs_negativeClass)
  );

  // Auto-sync positive and negative classes when 2 classes are available
  useEffect(() => {
    if (mode !== "fs" || !ds || !ds.clinicalGroupCol || uniqueGroupValues.length !== 2) return;
    const pos = ds.fs_positiveClass;
    const neg = ds.fs_negativeClass;
    const isPosValid = !!pos && uniqueGroupValues.includes(pos);
    const isNegValid = !!neg && uniqueGroupValues.includes(neg);
    if (!isPosValid || !isNegValid || pos === neg) {
      let nextPos = isPosValid ? pos! : uniqueGroupValues[1];
      let nextNeg = uniqueGroupValues.find(v => v !== nextPos) || uniqueGroupValues[0];
      if (isNegValid && !isPosValid) {
        nextNeg = neg!;
        nextPos = uniqueGroupValues.find(v => v !== nextNeg) || uniqueGroupValues[1];
      }
      updateDs({
        fs_positiveClass: nextPos,
        fs_negativeClass: nextNeg
      });
    }
  }, [mode, ds?.clinicalGroupCol, JSON.stringify(uniqueGroupValues), ds?.fs_positiveClass, ds?.fs_negativeClass]);

  // Derive unique batch count from the selected batch column (Batch mode only)
  const uniqueBatchValues: string[] = (() => {
    if (mode !== "batch" || !ds.clinicalBatchCol || !ds.clinicalColumns || !ds.clinicalParsedData) return [];
    const bIdx = ds.clinicalColumns.indexOf(ds.clinicalBatchCol);
    if (bIdx === -1) return [];
    const sIdx = ds.clinicalSampleIdCol ? ds.clinicalColumns.indexOf(ds.clinicalSampleIdCol) : -1;
    const matchedSet = new Set(matchingSamples.length > 0 ? matchingSamples : exprSamples);
    const sourceRows = (sIdx !== -1 && matchedSet.size > 0)
      ? ds.clinicalParsedData.filter((row: any[]) => row && row[sIdx] && matchedSet.has(String(row[sIdx]).trim()))
      : ds.clinicalParsedData;
    const rowsToUse = sourceRows.length > 0 ? sourceRows : ds.clinicalParsedData;
    const vals = rowsToUse
      .map((row: any[]) => String(row[bIdx] ?? "").trim())
      .filter(v => v !== "" && v !== "NA" && v !== "NaN");
    return [...new Set(vals)];
  })();
  const uniqueBatchCount = uniqueBatchValues.length;
  const hasSingleBatch = mode === "batch" && ds.clinicalBatchCol !== "" && uniqueBatchCount <= 1;

  const hasDanger =
    isNoOverlap ||
    hasMissingClinicalValues ||
    (mode === "fs" && matchingSamples.length < 30) ||
    (mode === "fs" && hasMultipleClasses) ||
    (mode === "fs" && hasUnsetClasses) ||
    !ds.integrityOk ||
    ((ds.integrityIssues?.length ?? 0) > 0);

  const canProceed =
    !hasDanger &&
    ds.clinicalFileName !== "" &&
    ds.clinicalSampleIdCol !== "" &&
    (mode === "batch" ? ds.clinicalBatchCol !== "" : ds.clinicalGroupCol !== "") &&
    (mode !== "fs" || (!hasUnsetClasses && !hasMultipleClasses)) &&
    matchingSamples.length >= (mode === "fs" ? 30 : 1);

  const done = mode === "batch" ? (ds.batchUploadBypassed || ds.batchDone) : (mode === "de" ? (ds.deUploadBypassed || ds.uploadDone) : (ds.fsUploadBypassed || ds.uploadDone));

  const hasSettingsChanged = (
    ds.clinicalFileName !== ds.submittedClinicalFileName ||
    ds.clinicalSampleIdCol !== ds.submittedClinicalSampleIdCol ||
    ds.clinicalGroupCol !== ds.submittedClinicalGroupCol ||
    ds.clinicalBatchCol !== ds.submittedClinicalBatchCol ||
    (mode === "fs" && (
      ds.fs_positiveClass !== ds.submitted_fs_positiveClass ||
      ds.fs_negativeClass !== ds.submitted_fs_negativeClass
    )) ||
    JSON.stringify(ds.clinicalOtherCovariates || []) !== JSON.stringify(ds.submittedClinicalOtherCovariates || [])
  );

  const completedModules: string[] = [];
  if (mode === "de") {
    if (state.dpInlineDeDone || Object.keys(state.dpDeResults).length > 0) completedModules.push("DE Analysis");
    if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");
  } else if (mode === "fs") {
    if (state.dpInlineFsDone) completedModules.push("Feature Selection / Model Selection");
  }
  const downstreamDone = completedModules.length > 0;

  const sorted = [...state.dpDatasets].sort((a, b) => a.id.localeCompare(b.id));
  const isIncomplete = (d: typeof sorted[0]) => !d.clinicalFileName || (mode === "batch" ? !d.clinicalBatchCol : !d.clinicalGroupCol);
  const pendingDatasets = sorted.filter(isIncomplete);
  const pendingIdx = pendingDatasets.findIndex(d => d.id === datasetId);
  const idx = sorted.findIndex(d => d.id === datasetId);
  const multipleDs = state.dpDatasets.length > 1;

  const proceedBack = () => {
    if (onBack) {
      onBack();
      return;
    }
    if (multipleDs && idx > 0) {
      const prevDs = sorted[idx - 1];
      dispatch({ type: "DP_SELECT_DATASET", id: prevDs.id });
      dispatch({ type: "DP_SET_CONTEXT", id: prevDs.id });
    } else {
      if (mode === "batch") {
        const activeSteps = computeDPSteps(state.dpDatasets);
        const currentIdx = activeSteps.findIndex(s => s.id === "batch");
        let prev = activeSteps[currentIdx - 1]?.id || "upload";
        if (prev === "upload" && state.dpDatasets.length > 1) prev = "all-datasets";
        dispatch({ type: "DP_SET_STEP", step: prev as any });
      } else {
        dispatch({ type: "DP_SET_STEP", step: "module-select" });
      }
    }
  };

  const handleBack = () => {
    if (hasSettingsChanged) {
      setShowChangeWarnModal(true);
    } else {
      proceedBack();
    }
  };

  const getContinueLabel = () => {
    if (multipleDs) {
      if (editingFromSummary) return "Continue to Upload Summary →";
      return pendingIdx < pendingDatasets.length - 1 ? "Next Dataset →" : "Continue to Upload Summary →";
    } else {
      return mode === "batch" ? "Proceed to Batch Correction →" : (
        mode === "de" ? "Proceed to DE Analysis →" : "Proceed to Feature Selection →"
      );
    }
  };

  const handleNext = async () => {
    let datasetToUpload = { ...ds };
    if (ds.clinicalFileName) {
      datasetToUpload = filterDatasetForUpload(ds, "overlap", mode);
    }

    if (!done || hasSettingsChanged) {
      setLoadingMsg("Updating clinical metadata on server…");
      setLoading(true);
      try {
        if (datasetToUpload.clinicalFileName && (datasetToUpload.clinicalParsedData || datasetToUpload.clinicalFilePath || datasetToUpload.clinicalUploadId || (datasetToUpload.clinicalColumns && datasetToUpload.clinicalColumns.length > 0))) {
          // Use supplementClinicalDataAPI to upload only the clinical data
          const isClinUnchanged = ds.clinicalFileName && ds.clinicalFileName === ds.submittedClinicalFileName;
          const res = await supplementClinicalDataAPI(
            datasetToUpload.id,
            datasetToUpload.clinicalColumns || [],
            (datasetToUpload.clinicalParsedData && datasetToUpload.clinicalParsedData.length > 0) ? datasetToUpload.clinicalParsedData : [],
            datasetToUpload.clinicalSampleIdCol,
            mode !== "batch" ? (datasetToUpload.clinicalGroupCol || "") : "",
            mode === "batch" ? (datasetToUpload.clinicalBatchCol || "") : "",
            mode === "batch" ? (datasetToUpload.clinicalOtherCovariates || []) : [],
            isClinUnchanged ? "" : (datasetToUpload.clinicalRawText || ""),
            isClinUnchanged ? undefined : datasetToUpload.clinicalFilePath,
            isClinUnchanged ? undefined : datasetToUpload.clinicalUploadId,
            mode === "fs" ? datasetToUpload.fs_positiveClass : undefined,
            mode === "fs" ? datasetToUpload.fs_negativeClass : undefined
          );
          if (res && res.status === "success") {
            if (res.samples !== undefined) datasetToUpload.nSamples = res.samples;
            if (res.total !== undefined) datasetToUpload.nFeatures = res.total;
            if (res.groups && Array.isArray(res.groups)) datasetToUpload.groups = res.groups;
            if (res.sampleIds && Array.isArray(res.sampleIds)) datasetToUpload.sampleIds = res.sampleIds;
          }
        }
      } catch (e) {
        console.error("Failed to upload clinical data to server:", e);
      }
      setLoading(false);
    }

    updateDs({
      columns: datasetToUpload.columns,
      parsedData: datasetToUpload.parsedData,
      sampleIds: datasetToUpload.sampleIds || (matchingSamples.length > 0 ? matchingSamples : datasetToUpload.sampleIds),
      nSamples: datasetToUpload.nSamples,
      nFeatures: datasetToUpload.nFeatures !== undefined ? datasetToUpload.nFeatures : ds.nFeatures,
      groups: datasetToUpload.groups || ds.groups || [],
      uploadDone: true,
      minGroupSampleSize: minGroupSampleSize,
      submittedClinicalFileName: datasetToUpload.clinicalFileName,
      submittedClinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      submittedClinicalGroupCol: mode !== "batch" ? (datasetToUpload.clinicalGroupCol || "") : "",
      submittedClinicalBatchCol: mode === "batch" ? (datasetToUpload.clinicalBatchCol || "") : "",
      submittedClinicalOtherCovariates: mode === "batch" ? (datasetToUpload.clinicalOtherCovariates || []) : [],
      submitted_fs_positiveClass: mode === "fs" ? (datasetToUpload.fs_positiveClass || "") : (ds.submitted_fs_positiveClass || ""),
      submitted_fs_negativeClass: mode === "fs" ? (datasetToUpload.fs_negativeClass || "") : (ds.submitted_fs_negativeClass || ""),
      fs_positiveClass: mode === "fs" ? (datasetToUpload.fs_positiveClass || "") : (ds.fs_positiveClass || ""),
      fs_negativeClass: mode === "fs" ? (datasetToUpload.fs_negativeClass || "") : (ds.fs_negativeClass || ""),
      clinicalFile: datasetToUpload.clinicalFile,
      clinicalFileName: datasetToUpload.clinicalFileName,
      clinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      clinicalGroupCol: mode !== "batch" ? (datasetToUpload.clinicalGroupCol || "") : "",
      clinicalBatchCol: mode === "batch" ? (datasetToUpload.clinicalBatchCol || "") : "",
      clinicalParsedData: datasetToUpload.clinicalParsedData,
      clinicalColumns: datasetToUpload.clinicalColumns,
      matchingSamples: matchingSamples,
      missingClinSamples: missingClinSamples,
      clinicalNoExprSamples: [],
      hasMissingClinicalValues: hasMissingClinicalValues,
      removedSamples: removedSamples,
    });

    setLoading(false);

    if (onContinue) {
      onContinue();
      return;
    }

    const nextPending = sorted.find(d => d.id !== datasetId && isMissingClinical(d));

    if (nextPending) {
      dispatch({ type: "DP_SELECT_DATASET", id: nextPending.id });
      dispatch({ type: "DP_SET_CONTEXT", id: nextPending.id });
    }
  };

  const onContinueClick = () => {
    if (isPartialMatch) {
      setShowWarnModal(true);
    } else if (hasSettingsChanged && downstreamDone) {
      setShowDiscardModal(true);
    } else {
      handleNext();
    }
  };

  const handleConfirmWarn = () => {
    setShowWarnModal(false);
    if (hasSettingsChanged && downstreamDone) {
      setShowDiscardModal(true);
    } else {
      handleNext();
    }
  };

  const exprSet = new Set(exprSamples);
  const matchedRows = ds.clinicalParsedData && sampleIdIdx !== -1
    ? ds.clinicalParsedData.filter(row => row && row[sampleIdIdx] && exprSet.has(String(row[sampleIdIdx])))
    : [];
  const rowsToPreview = matchedRows.length > 0 ? matchedRows : (ds.clinicalParsedData || []);
  const displayRows = rowsToPreview.slice(0, 10);

  return (
    <>
      {loading && <Spinner label={loadingMsg} sublabel="Please wait…" />}

      {/* Clinical Data Upload */}
      <div className="card">
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">
              {/* Dynamic dataset name template string */}
              Clinical / Phenotype Data for {ds.name}
              {(!ds.clinicalFileName || matchingSamples.length === 0) && (
                <span style={{
                  marginLeft: 8, fontSize: 11, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                  border: "1px solid hsl(0 50% 85%)", padding: "2px 7px", borderRadius: 4, fontWeight: 500,
                }}>
                  Required
                </span>
              )}
            </div>
            <div className="card-sub">Upload sample metadata with condition/group assignments for analysis.</div>
          </div>
          <div style={{ display: "flex", gap: 10 }}>
            <button className="link-button danger" onClick={handleClear}>Clear</button>
            <button className="link-button primary" onClick={handleLoadSample}>Example Data</button>
          </div>
        </div>

        {!ds.clinicalFileName ? (
          <div className={`drop-zone ${clinicalDrag ? "dragover" : ""}`} style={{ marginTop: 16 }}
            onClick={() => {
              const input = document.createElement("input");
              input.type = "file"; input.accept = ".csv,.tsv,.txt";
              input.onchange = e => { const f = (e.target as HTMLInputElement).files?.[0]; if (f) handleClinicalFile(f); };
              input.click();
            }}
            onDragOver={e => { e.preventDefault(); setClinicalDrag(true); }}
            onDragLeave={() => setClinicalDrag(false)}
            onDrop={e => { e.preventDefault(); setClinicalDrag(false); const f = e.dataTransfer.files?.[0]; if (f) handleClinicalFile(f); }}
            data-testid="dropzone-clinical">
            <div className="drop-zone-icon">🧬</div>
            <div className="drop-zone-title">Drop clinical metadata file here</div>
            <div className="drop-zone-hint">CSV · TSV &nbsp;·&nbsp; SampleID + Group columns required</div>
          </div>
        ) : (
          <>
            <div className="file-chip" style={{ marginTop: 14 }}>
              <FileText size={15} />
              {isPartialMatch || isNoOverlap ? (
                <>
                  <span className="file-chip-name">
                    {ds.clinicalFileName}
                  </span>
                  <span style={{ fontSize: 12, color: "hsl(38 85% 35%)" }}>
                    ({matchingSamples.length} samples matched with the {exprSamples.length} expression samples)
                  </span>
                </>
              ) : (
                <>
                  <span className="file-chip-name">{ds.clinicalFileName}</span>
                  <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>
                    ({matchingSamples.length} samples matched)
                  </span>
                </>
              )}
              <button className="file-chip-clear" onClick={handleClear} data-testid="btn-clear-clinical"><X size={16} /></button>
            </div>

            <div style={{ marginTop: 14, display: "flex", gap: 16, flexWrap: "wrap" }}>
              {/* Sample ID Column — only shown if clinical data hasn't been provided yet */}
              {!done && (
                <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Sample ID Column:</label>
                  <details style={{ width: "100%" }} data-testid="details-clinical-sample-id-col">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>{ds.clinicalSampleIdCol || "Select column..."}</span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {(ds.clinicalColumns || []).filter(c => c !== ds.clinicalSampleIdCol).map(c => (
                        <div
                          key={c}
                          onClick={(e) => {
                            updateDs({ clinicalSampleIdCol: c, deUploadBypassed: false, fsUploadBypassed: false });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.clinicalSampleIdCol === c ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.clinicalSampleIdCol === c ? "var(--selected-bg)" : "transparent"}
                        >
                          {c}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>
              )}

              {/* Group Column — shown when not in batch mode */}
              {mode !== "batch" && (
                <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5, display: "flex", alignItems: "center", gap: 6 }}>
                    Group Column
                    <span style={{
                      fontSize: 10, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                      border: "1px solid hsl(0 50% 85%)", padding: "1px 5px", borderRadius: 3, fontWeight: 500,
                    }}>Required</span>
                  </label>
                  <details style={{ width: "100%" }} data-testid="details-clinical-group-col">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", borderRadius: 7, fontSize: 13,
                      border: !ds.clinicalGroupCol ? "1px solid hsl(0 60% 70%)" : "1px solid var(--border)",
                      background: !ds.clinicalGroupCol ? "hsl(0 70% 99%)" : "#fff",
                      cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span style={{ color: !ds.clinicalGroupCol ? "hsl(0 50% 55%)" : "inherit" }}>
                        {ds.clinicalGroupCol || "Select column…"}
                      </span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {(ds.clinicalColumns || []).filter(c => c !== ds.clinicalSampleIdCol).map(c => (
                        <div
                          key={c}
                          onClick={(e) => {
                            let autoPos = "";
                            let autoNeg = "";
                            if (mode === "fs" && ds.clinicalColumns && ds.clinicalParsedData) {
                              const gIdx = ds.clinicalColumns.indexOf(c);
                              if (gIdx !== -1) {
                                const vals = Array.from(new Set(
                                  ds.clinicalParsedData.map((r: any) => r && r[gIdx]).filter((v: any) => v !== undefined && v !== null && String(v).trim() !== "")
                                )).map(String);
                                if (vals.length === 2) {
                                  autoPos = vals[0];
                                  autoNeg = vals[1];
                                }
                              }
                            }
                            updateDs({
                              clinicalGroupCol: c,
                              de_referenceGroup: "",
                              de_comparisonGroup: "",
                              fs_positiveClass: autoPos,
                              fs_negativeClass: autoNeg,
                              deUploadBypassed: false,
                              fsUploadBypassed: false
                            });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.clinicalGroupCol === c ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.clinicalGroupCol === c ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.clinicalGroupCol === c ? "var(--selected-bg)" : "transparent"}
                        >
                          {c}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>
              )}

              {/* Positive Class and Negative Class for Feature Selection (REQUIRED) */}
              {mode === "fs" && ds.clinicalGroupCol && (
                <>
                  <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                    <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5, display: "flex", alignItems: "center", gap: 6 }}>
                      Positive Class
                      <span style={{
                        fontSize: 10, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                        border: "1px solid hsl(0 50% 85%)", padding: "1px 5px", borderRadius: 3, fontWeight: 500,
                      }}>Required</span>
                    </label>
                    <details style={{ width: "100%" }} data-testid="details-fs-positive-class">
                      <summary style={{
                        display: "flex", justifyContent: "space-between", alignItems: "center",
                        padding: "8px 10px", borderRadius: 7, fontSize: 13,
                        border: !ds.fs_positiveClass ? "1px solid hsl(0 60% 70%)" : "1px solid var(--border)",
                        background: !ds.fs_positiveClass ? "hsl(0 70% 99%)" : "#fff",
                        cursor: "pointer", listStyle: "none", userSelect: "none"
                      }}>
                        <span style={{ color: !ds.fs_positiveClass ? "hsl(0 50% 55%)" : "inherit" }}>
                          {ds.fs_positiveClass || "Select positive class…"}
                        </span>
                        <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                      </summary>

                      <div style={{
                        position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                        border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                        overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                      }}>
                        {uniqueGroupValues.map(v => (
                          <div
                            key={v}
                            onClick={(e) => {
                              const autoNeg = uniqueGroupValues.find(x => x !== v) || "";
                              updateDs({
                                fs_positiveClass: v,
                                fs_negativeClass: autoNeg,
                                fsUploadBypassed: false
                              });
                              const details = (e.target as HTMLElement).closest("details");
                              if (details) details.removeAttribute("open");
                            }}
                            style={{
                              padding: "8px 10px",
                              fontSize: 12,
                              cursor: "pointer",
                              background: ds.fs_positiveClass === v ? "var(--selected-bg)" : "transparent",
                              fontWeight: ds.fs_positiveClass === v ? 600 : 400
                            }}
                            onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                            onMouseLeave={e => e.currentTarget.style.background = ds.fs_positiveClass === v ? "var(--selected-bg)" : "transparent"}
                          >
                            {v}
                          </div>
                        ))}
                      </div>
                    </details>
                  </div>

                  <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                    <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5, display: "flex", alignItems: "center", gap: 6 }}>
                      Negative Class
                      <span style={{
                        fontSize: 10, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                        border: "1px solid hsl(0 50% 85%)", padding: "1px 5px", borderRadius: 3, fontWeight: 500,
                      }}>Required</span>
                    </label>
                    <details style={{ width: "100%" }} data-testid="details-fs-negative-class">
                      <summary style={{
                        display: "flex", justifyContent: "space-between", alignItems: "center",
                        padding: "8px 10px", borderRadius: 7, fontSize: 13,
                        border: !ds.fs_negativeClass ? "1px solid hsl(0 60% 70%)" : "1px solid var(--border)",
                        background: !ds.fs_negativeClass ? "hsl(0 70% 99%)" : "#fff",
                        cursor: "pointer", listStyle: "none", userSelect: "none"
                      }}>
                        <span style={{ color: !ds.fs_negativeClass ? "hsl(0 50% 55%)" : "inherit" }}>
                          {ds.fs_negativeClass || "Select negative class…"}
                        </span>
                        <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                      </summary>

                      <div style={{
                        position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                        border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                        overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                      }}>
                        {uniqueGroupValues.map(v => (
                          <div
                            key={v}
                            onClick={(e) => {
                              const autoPos = uniqueGroupValues.find(x => x !== v) || "";
                              updateDs({
                                fs_negativeClass: v,
                                fs_positiveClass: autoPos,
                                fsUploadBypassed: false
                              });
                              const details = (e.target as HTMLElement).closest("details");
                              if (details) details.removeAttribute("open");
                            }}
                            style={{
                              padding: "8px 10px",
                              fontSize: 12,
                              cursor: "pointer",
                              background: ds.fs_negativeClass === v ? "var(--selected-bg)" : "transparent",
                              fontWeight: ds.fs_negativeClass === v ? 600 : 400
                            }}
                            onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                            onMouseLeave={e => e.currentTarget.style.background = ds.fs_negativeClass === v ? "var(--selected-bg)" : "transparent"}
                          >
                            {v}
                          </div>
                        ))}
                      </div>
                    </details>
                  </div>
                </>
              )}

              {/* Batch Column — shown for batch effects only (REQUIRED) */}
              {mode === "batch" && (
                <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                  <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5, display: "flex", alignItems: "center", gap: 6 }}>
                    Batch Column
                    <span style={{
                      fontSize: 10, background: "hsl(0 70% 96%)", color: "hsl(0 65% 40%)",
                      border: "1px solid hsl(0 50% 85%)", padding: "1px 5px", borderRadius: 3, fontWeight: 500,
                    }}>Required</span>
                  </label>
                  <details style={{ width: "100%" }} data-testid="details-clinical-batch-col">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", borderRadius: 7, fontSize: 13,
                      border: !ds.clinicalBatchCol ? "1px solid hsl(0 60% 70%)" : "1px solid var(--border)",
                      background: !ds.clinicalBatchCol ? "hsl(0 70% 99%)" : "#fff",
                      cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span style={{ color: !ds.clinicalBatchCol ? "hsl(0 50% 55%)" : "inherit" }}>
                        {ds.clinicalBatchCol || "Select column…"}
                      </span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {(ds.clinicalColumns || [])
                        .filter(c => c !== ds.clinicalSampleIdCol && c !== ds.clinicalGroupCol)
                        .map(c => (
                          <div
                            key={c}
                            onClick={(e) => {
                              updateDs({ clinicalBatchCol: c, deUploadBypassed: false, fsUploadBypassed: false });
                              const details = (e.target as HTMLElement).closest("details");
                              if (details) details.removeAttribute("open");
                            }}
                            style={{
                              padding: "8px 10px",
                              fontSize: 12,
                              cursor: "pointer",
                              background: ds.clinicalBatchCol === c ? "var(--selected-bg)" : "transparent",
                              fontWeight: ds.clinicalBatchCol === c ? 600 : 400
                            }}
                            onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                            onMouseLeave={e => e.currentTarget.style.background = ds.clinicalBatchCol === c ? "var(--selected-bg)" : "transparent"}
                          >
                            {c}
                          </div>
                        ))}
                    </div>
                  </details>
                </div>
              )}
            </div>

            {mode === "fs" && (
              <div style={{ marginTop: 16, display: "flex", flexDirection: "column", gap: 10 }}>
                <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 13, cursor: "pointer", fontWeight: 500 }}>
                  <input
                    type="checkbox"
                    checked={ds.fs_enabledTargetPrevalence || false}
                    onChange={e => {
                      updateDs({
                        fs_enabledTargetPrevalence: e.target.checked,
                        fs_targetPrevalence: e.target.checked ? (ds.fs_targetPrevalence ?? 0.5) : undefined
                      });
                    }}
                    style={{ accentColor: "var(--primary)" }}
                  />
                  Enabled Target Prevalence
                </label>

                {ds.fs_enabledTargetPrevalence && (
                  <div style={{ display: "flex", flexDirection: "column", width: 220 }}>
                    <label style={{ fontSize: 13, fontWeight: 500, marginBottom: 6 }}>Target Prevalence (0 to 1)</label>
                    <input
                      type="number"
                      min={0}
                      max={1}
                      step={0.01}
                      value={ds.fs_targetPrevalence !== undefined ? ds.fs_targetPrevalence : ""}
                      onChange={e => {
                        let val = parseFloat(e.target.value);
                        if (!isNaN(val)) {
                          if (val < 0) val = 0;
                          if (val > 1) val = 1;
                        }
                        updateDs({ fs_targetPrevalence: isNaN(val) ? undefined : val });
                      }}
                      style={{ width: "100%", padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                    />
                  </div>
                )}
              </div>
            )}

            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Clinical Data Preview (first 10 rows)
            </div>
            <div className="preview-wrap">
              {displayRows.length > 0 ? (
                <table>
                  <thead>
                    <tr>
                      {(ds.clinicalColumns || []).map(col => (
                        <th key={col}>{col}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {displayRows.map((row, i) => (
                      row && Array.isArray(row) && (
                        <tr key={i}>
                          {row.map((cell, j) => (
                            <td key={j}>{cell !== null && cell !== undefined ? String(cell) : ""}</td>
                          ))}
                        </tr>
                      )
                    ))}
                  </tbody>
                </table>
              ) : (
                <div style={{ padding: 12, textAlign: "center", fontSize: 13, color: "var(--muted-foreground)" }}>
                  No overlapping samples found (select correct Sample ID column to preview).
                </div>
              )}
            </div>
          </>
        )}
      </div>

      {/* Integrity check */}
      {ds.expressionFileName && (
        <div className="card">
          <div className="card-title" style={{ marginBottom: 12 }}>Data Integrity Check</div>
          {!ds.clinicalFileName ? (
            <div className="banner danger">
              <Info size={15} />
              Clinical data upload is required before proceeding to analysis.
            </div>
          ) : (
            <>
              {hasClinical && (
                <>
                  {/* Output stats chips */}
                  <div style={{ marginBottom: 14 }}>
                    <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
                      Sample Matching Statistics
                    </div>
                    <div className="chips-row" style={{ marginBottom: 12 }}>
                      <div className="stat-chip">
                        <div className="stat-chip-val">{matchingSamples.length}</div>
                        <div className="stat-chip-lbl">Matching Samples</div>
                      </div>
                      <div className="stat-chip">
                        <div className="stat-chip-val" style={{ color: missingClinSamples.length > 0 ? "hsl(0, 0%, 0%)" : "var(--muted-foreground)" }}>
                          {missingClinSamples.length}
                        </div>
                        <div className="stat-chip-lbl">Missing Clinical Info</div>
                      </div>
                    </div>

                    {missingClinSamples.length > 0 && (
                      <div style={{ marginBottom: 12, display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
                        <span style={{ fontSize: 12, color: "var(--muted-foreground)" }}>Download missing samples:</span>
                        <button type="button" className="btn btn-sm btn-default" onClick={() => {
                          const content = "SampleID\n" + missingClinSamples.join("\n");
                          const blob = new Blob([content], { type: "text/csv;charset=utf-8;" });
                          const url = URL.createObjectURL(blob);
                          const link = document.createElement("a");
                          link.href = url;
                          link.setAttribute("download", `${ds.name || "dataset"}_missing_clinical_samples.csv`);
                          document.body.appendChild(link);
                          link.click();
                          document.body.removeChild(link);
                        }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                          <Download size={11} /> CSV
                        </button>
                        <button type="button" className="btn btn-sm btn-default" onClick={() => {
                          const content = "SampleID\n" + missingClinSamples.join("\n");
                          const blob = new Blob([content], { type: "text/tab-separated-values;charset=utf-8;" });
                          const url = URL.createObjectURL(blob);
                          const link = document.createElement("a");
                          link.href = url;
                          link.setAttribute("download", `${ds.name || "dataset"}_missing_clinical_samples.tsv`);
                          document.body.appendChild(link);
                          link.click();
                          document.body.removeChild(link);
                        }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                          <Download size={11} /> TSV
                        </button>
                        <button type="button" className="btn btn-sm btn-default" onClick={() => {
                          const content = `
                            <html xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns="http://www.w3.org/TR/REC-html40">
                            <head><!--[if gte mso 9]><xml><x:ExcelWorkbook><x:ExcelWorksheets><x:ExcelWorksheet><x:Name>Missing Samples</x:Name><x:WorksheetOptions><x:DisplayGridlines/></x:WorksheetOptions></x:ExcelWorksheet></x:ExcelWorksheets></x:ExcelWorkbook></xml><![endif]--></head>
                            <body>
                              <table>
                                <thead><tr><th>SampleID</th></tr></thead>
                                <tbody>
                                  ${missingClinSamples.map(s => `<tr><td>${s}</td></tr>`).join("")}
                                </tbody>
                              </table>
                            </body>
                            </html>
                          `;
                          const blob = new Blob([content], { type: "application/vnd.ms-excel;charset=utf-8;" });
                          const url = URL.createObjectURL(blob);
                          const link = document.createElement("a");
                          link.href = url;
                          link.setAttribute("download", `${ds.name || "dataset"}_missing_clinical_samples.xlsx`);
                          document.body.appendChild(link);
                          link.click();
                          document.body.removeChild(link);
                        }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                          <Download size={11} /> XLSX
                        </button>
                      </div>
                    )}
                  </div>
                </>
              )}
              {isNoOverlap && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>Danger: No overlapping samples found between expression and clinical data.</span>
                </div>
              )}
              {hasMissingClinicalValues && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>
                    There are missing values in the selected {mode === "batch" ? "batch" : "group"} column. Those samples will be removed.
                  </span>
                </div>
              )}
              {mode === "fs" && minGroupSampleSize !== undefined && minGroupSampleSize < 10 && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>
                    Danger: The sample size of the group with the lowest sample size is less than 10 ({minGroupSampleSize}). Cannot proceed with feature selection.
                  </span>
                </div>
              )}
              {mode === "fs" && hasMultipleClasses && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>
                    <strong>Incorrect class count detected ({uniqueGroupCount} classes: {uniqueGroupValues.join(", ")}).</strong>{" "}
                    Feature selection only supports binary classification (exactly 2 classes). Please select a different group column or recode your groups to have exactly 2 distinct classes.
                  </span>
                </div>
              )}
              {mode === "fs" && !hasMultipleClasses && hasUnsetClasses && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>
                    Please select both a Positive Class and a Negative Class for binary classification.
                  </span>
                </div>
              )}
              {mode === "batch" && hasSingleBatch && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>
                    <strong>Cannot perform batch-correction when there is only {uniqueBatchCount} batch{uniqueBatchCount === 1 ? ` (${uniqueBatchValues[0] ? `"${uniqueBatchValues[0]}"` : "1 value"})` : ""}.</strong>{" "}
                    Batch effect correction requires at least 2 distinct batches. Please select a batch column with multiple batches or skip batch correction.
                  </span>
                </div>
              )}
              {(!ds.integrityOk || (ds.integrityIssues && ds.integrityIssues.length > 0)) && (
                <div className="banner danger" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <div>
                    <strong>Issues found:</strong>
                    <ul style={{ marginTop: 4, paddingLeft: 18 }}>
                      {ds.integrityIssues.map((issue, i) => <li key={i}>{issue}</li>)}
                    </ul>
                  </div>
                </div>
              )}
              {isPartialMatch && (
                <div className="banner warn" style={{ marginBottom: 10 }}>
                  <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                  <span>Warning: the system will only keep samples that have both expression and clinical data.</span>
                </div>
              )}
              {!isNoOverlap && !isPartialMatch && hasClinical && !hasDanger && (
                <div className="banner success" style={{ marginBottom: 10 }}>
                  <CheckCircle size={15} style={{ flexShrink: 0 }} />
                  <span>Success: All samples match perfectly between expression and clinical data.</span>
                </div>
              )}
              {ds.integrityOk && canProceed && !hasDanger && (
                <div className="banner success">
                  <CheckCircle size={15} />
                  All checks passed. Dataset is ready to proceed.
                </div>
              )}
            </>
          )}
        </div>
      )}

      {/* Action Row */}
      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button className="btn btn-default" onClick={handleBack} data-testid="btn-inline-upload-back">
            ← Back
          </button>
          {mode === "batch" && (
            <button className="btn btn-default" onClick={() => {
              setShowSkipModal(true);
            }} data-testid="btn-skip-batch-clinical-upload" style={{ color: "var(--muted-foreground)" }}>
              Skip Batch Correction
            </button>
          )}
        </div>
        <button
          className="btn btn-primary"
          disabled={!canProceed}
          onClick={onContinueClick}
          data-testid={`btn-inline-${mode}-upload-continue`}
        >
          {getContinueLabel()}
        </button>
      </div>

      {showSkipModal && (
        <SkipModal
          stepName="Batch Correction Removal"
          stepId="batch"
          datasetIds={sorted.map(d => d.id)}
          onConfirm={() => {
            setShowSkipModal(false);
            dispatch({ type: "DP_SET_STEP", step: "module-select" });
          }}
          onCancel={() => setShowSkipModal(false)}
        />
      )}

      {showWarnModal && (
        <WarnSampleModal
          type="partial_overlap"
          onConfirm={handleConfirmWarn}
          onCancel={() => setShowWarnModal(false)}
        />
      )}

      {showDiscardModal && (
        <DiscardWarnModal
          stepId="upload"
          datasetIds={[ds.id]}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: ds.id,
              fromStep: mode === "de" ? "de-analysis" : "model-selection"
            } as any);
            if (mode === "de") {
              dispatch({ type: "SET_DP_INLINE_DE_DONE", done: false });
            } else {
              dispatch({ type: "SET_DP_INLINE_FS_DONE", done: false });
            }
            await handleNext();
          }}
        />
      )}
      {showChangeWarnModal && (
        <ChangeWarnModal
          onConfirm={() => {
            setShowChangeWarnModal(false);
            updateDs({
              clinicalFileName: ds.submittedClinicalFileName || "",
              clinicalSampleIdCol: ds.submittedClinicalSampleIdCol || "",
              clinicalGroupCol: ds.submittedClinicalGroupCol || "",
              clinicalBatchCol: ds.submittedClinicalBatchCol || "",
              clinicalOtherCovariates: ds.submittedClinicalOtherCovariates || []
            });
            proceedBack();
          }}
          onCancel={() => setShowChangeWarnModal(false)}
        />
      )}
    </>
  );
}
