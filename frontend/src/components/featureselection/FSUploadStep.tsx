import { useState, useEffect } from "react";
import { useLocation } from "wouter";
import { FileText, X, AlertTriangle, CheckCircle, Download, Layers } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import Spinner from "../Spinner";
import { parseExpressionFile, parseClinicalFile, alignSamplesLocal, unparseCSV, validateFSSamplesLocal, getDesktopFilePath } from "../../lib/dataParser";
import { uploadFileInChunks } from "../../lib/chunkUploader";
import { fetchSampleDataAPI, uploadFSDatasetsAPI, clearDatasetAPI, redoStepAPI, clearDownstreamAPI } from "../../lib/api";
import { getHierarchyGroup, type DataType, type FSMultiDatasetMode, type FSValidationStrategy } from "../../dataObject";
import WarnSampleModal from "../shared/WarnSampleModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";

interface Props {
  datasetId: string;
}

const filterDatasetForUpload = (dataset: any, choice: "all" | "overlap") => {
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
      fs_positiveClass: "",
      fs_negativeClass: "",
      clinicalColumns: [],
      clinicalParsedData: [],
    };
  }

  const clinicalCols = dataset.clinicalColumns || [];
  const clinicalRows = dataset.clinicalParsedData || [];
  const clinSampleIdCol = dataset.clinicalSampleIdCol;
  const clinGroupCol = dataset.clinicalGroupCol;
  const clinBatchCol = dataset.clinicalBatchCol;

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
    
    const exprSampleCols = (dataset.columns || []).filter((c: string, idx: number) => {
      if (idx === fsFeatureIdx) return false;
      if (c === geneIdCol || geneInfoCols.includes(c)) return false;
      return true;
    });
    
    const commonSamples = exprSampleCols.filter((s: string) => validClinicalSamples.includes(s));

    keepCols = (dataset.columns || []).filter((c: string, idx: number) => {
      if (idx === fsFeatureIdx) return true;
      if (c === geneIdCol || geneInfoCols.includes(c)) return true;
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
    parsedData: filteredExpressionRows,
    clinicalParsedData: finalClinicalRows,
    nSamples: nSamples,
  };
};

export default function FSUploadStep({ datasetId }: Props) {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const ds = state.fsDatasets.find(d => d.id === datasetId);
  const [loading, setLoading] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const [expressionDrag, setExpressionDrag] = useState(false);
  const [clinicalDrag, setClinicalDrag] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  const [matchingSamples, setMatchingSamples] = useState<string[]>([]);
  const [missingClinSamples, setMissingClinSamples] = useState<string[]>([]);
  const [clinicalNoExprSamples, setClinicalNoExprSamples] = useState<string[]>([]);
  const [hasMissingClinicalValues, setHasMissingClinicalValues] = useState<boolean>(false);
  const [removedSamples, setRemovedSamples] = useState<string[]>([]);
  const [minGroupSampleSize, setMinGroupSampleSize] = useState<number | undefined>(undefined);

  // Samples matching comparison logic
  const exprSamples = ds
    ? (ds.fs_featuresOrientation === "headers" 
      ? (ds.parsedData ? ds.parsedData.map(r => r[(ds.fs_featureIndexValue || 1) - 1]).filter(Boolean) : [])
      : ds.columns.filter((c, idx) => idx !== ((ds.fs_featureIndexValue || 1) - 1) && !(ds.geneInfoCols || []).includes(c)))
    : [];

  const sampleIdIdx = ds && ds.clinicalFileName && ds.clinicalSampleIdCol && ds.clinicalColumns
    ? ds.clinicalColumns.indexOf(ds.clinicalSampleIdCol)
    : -1;
  const clinSamples = ds && sampleIdIdx !== -1 && ds.clinicalParsedData
    ? ds.clinicalParsedData.map(row => row[sampleIdIdx]).filter(Boolean)
    : [];

  useEffect(() => {
    if (!ds) return;
    const currentDs = ds;
    if (ds.uploadDone && ds.matchingSamples && (!currentDs.parsedData || currentDs.parsedData.length === 0)) {
      setMatchingSamples(ds.matchingSamples);
      setMissingClinSamples(ds.missingClinSamples || []);
      setClinicalNoExprSamples(ds.clinicalNoExprSamples || []);
      setHasMissingClinicalValues(ds.hasMissingClinicalValues || false);
      setRemovedSamples(ds.removedSamples || []);
      return;
    }
    function performAlignment() {
      if (!currentDs.expressionFileName) {
        setMatchingSamples([]);
        setMissingClinSamples([]);
        setClinicalNoExprSamples([]);
        setHasMissingClinicalValues(false);
        setRemovedSamples([]);
        return;
      }
      try {
        const exprSamples = currentDs.fs_featuresOrientation === "headers" 
          ? (currentDs.parsedData ? currentDs.parsedData.map(r => r[(currentDs.fs_featureIndexValue || 1) - 1]).filter(Boolean) : [])
          : currentDs.columns.filter((c, idx) => idx !== ((currentDs.fs_featureIndexValue || 1) - 1) && !(currentDs.geneInfoCols || []).includes(c));

        const res = alignSamplesLocal({
          expressionColumns: exprSamples,
          geneIdCol: "",
          geneInfoCols: [],
          clinicalParsedData: currentDs.clinicalParsedData || [],
          clinicalColumns: currentDs.clinicalColumns || [],
          clinicalSampleIdCol: currentDs.clinicalSampleIdCol || "",
          geneIdType: currentDs.geneIdType || "",
          clinicalGroupCol: currentDs.clinicalGroupCol || "",
          clinicalBatchCol: currentDs.clinicalBatchCol || "",
          clinicalOtherCovariates: currentDs.clinicalOtherCovariates || [],
          nFeatures: currentDs.nFeatures
        });
        setMatchingSamples(res.commonSamples || []);
        setMissingClinSamples(res.missingClinicalSamples || []);
        setClinicalNoExprSamples(res.missingExpressionSamples || []);
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
    ds?.fs_featuresOrientation,
    ds?.fs_featureIndexValue,
    ds?.clinicalSampleIdCol,
    ds?.clinicalGroupCol,
    ds?.clinicalBatchCol,
    JSON.stringify(exprSamples),
    JSON.stringify(clinSamples)
  ]);

  const requireClinical = true;

  if (!ds) return null;

  const multipleDs = state.fsDatasets.length > 1;

  const updateDs = (patch: Partial<typeof ds>) =>
    dispatch({ type: "FS_UPDATE_DATASET", id: datasetId, patch });

  const handleClear = async (scope: "expression" | "clinical") => {
    try {
      await clearDatasetAPI(ds.id, scope);
    } catch (e) {
      console.warn("clearDatasetAPI failed (non-fatal):", e);
    }
    dispatch({
      type: "RESET_DOWNSTREAM_STEPS",
      datasetId: ds.id,
      fromStep: "upload"
    } as any);
    if (scope === "expression") {
      updateDs({
        expressionFile: null, expressionFileName: "", expressionFilePath: undefined, expressionUploadId: undefined, expressionRawText: "", columns: [],
        geneInfoCols: [], geneIdCol: "", geneIdType: "", detectedGeneIdType: "",
        nSamples: 0, nFeatures: 0, uploadDone: false,
        integrityOk: false, integrityIssues: requireClinical ? ["Clinical data not yet uploaded"] : [],
        parsedData: undefined,
        hasNA: false,
        submittedExpressionFileName: "",
      });
    } else {
      const issues: string[] = [];
      if (requireClinical && !!ds.expressionFileName) {
        issues.push("Clinical data not yet uploaded");
      }
      updateDs({
        clinicalFile: null, clinicalFileName: "", clinicalFilePath: undefined, clinicalUploadId: undefined, clinicalRawText: "", clinicalSampleIdCol: "", clinicalGroupCol: "",
        fs_positiveClass: "", fs_negativeClass: "",
        submittedClinicalFileName: "",
        submitted_fs_positiveClass: "", submitted_fs_negativeClass: "",
        integrityOk: issues.length === 0, integrityIssues: issues,
        clinicalColumns: undefined,
        clinicalParsedData: undefined,
      });
    }
  };

  const handleLoadSample = async (scope: "expression" | "clinical") => {
    setLoading(true);
    try {
      if (scope === "expression") {
        setLoadingMsg("Loading sample expression dataset…");
        const result = await fetchSampleDataAPI("expression");
        const issues: string[] = [];
        if (!ds.clinicalFileName && requireClinical) issues.push("Clinical data not yet uploaded");

        const rawText = unparseCSV(result.columns, result.parsedData);
        updateDs({
          expressionFileName: "sample_expression.csv",
          expressionRawText: rawText,
          columns: result.columns,
          geneInfoCols: result.columns.length > 0 ? [result.columns[0]] : [],
          geneIdCol: result.columns[0] || "GeneID",
          geneIdType: "ensembl",
          detectedGeneIdType: "ensembl",
          fs_featuresOrientation: "column",
          fs_featureIndexValue: 1,
          fs_datasetPurpose: state.fsDatasets.length === 1 ? "train-and-test" : "train",
          fs_isInternalValidation: state.fsDatasets.length > 1,
          nSamples: result.columns.length - 1,
          nFeatures: result.parsedData.length,
          parsedData: result.parsedData,
          integrityOk: issues.length === 0,
          integrityIssues: issues,
          hasNA: false,
        });
      } else {
        setLoadingMsg("Loading sample clinical data…");
        const result = await fetchSampleDataAPI("clinical");
        const rows = result.parsedData as string[][];
        const rawText = unparseCSV(result.columns, rows);
        const groupCol = result.columns[1] || "Group";
        let autoPos = "";
        let autoNeg = "";
        const gIdx = result.columns.indexOf(groupCol);
        if (gIdx !== -1) {
          const vals = Array.from(new Set(rows.map((r: any) => r[gIdx]).filter((v: any) => v !== undefined && v !== null && String(v).trim() !== ""))).map(String);
          if (vals.length === 2) {
            autoPos = vals[0];
            autoNeg = vals[1];
          }
        }
        updateDs({
          clinicalFileName: "sample_clinical.csv",
          clinicalRawText: rawText,
          clinicalSampleIdCol: result.columns[0] || "SampleID",
          clinicalGroupCol: groupCol,
          fs_positiveClass: autoPos,
          fs_negativeClass: autoNeg,
          clinicalColumns: result.columns,
          clinicalParsedData: rows,
          integrityOk: !requireClinical || !!ds.expressionFileName,
          integrityIssues: (requireClinical && !ds.expressionFileName) ? ["Expression data not yet uploaded"] : [],
        });
      }
    } catch (err: any) {
      updateDs({ integrityIssues: [`Failed to load example data: ${err.message}`] });
    } finally {
      setLoading(false);
    }
  };

  const handleExpressionFile = async (file: File) => {
    setLoadingMsg(`Parsing ${file.name}…`);
    setLoading(true);
    try {
      const desktopPath = getDesktopFilePath(file);
      const isDesktop = !!desktopPath;

      const parsed = await parseExpressionFile(file, { previewOnly: true });
      const geneIdCol = parsed.geneIdCol;
      const geneInfoColsToExclude = (parsed.geneInfoCols || []).filter(c => c !== geneIdCol);
      const keepIndices = parsed.columns
        .map((c, idx) => ({ name: c, idx }))
        .filter(item => !geneInfoColsToExclude.includes(item.name));
      const cleanColumns = keepIndices.map(item => item.name);
      const cleanParsedData = parsed.parsedData.map(row => keepIndices.map(item => row[item.idx]));

      let uploadId: string | undefined = undefined;

      if (!isDesktop) {
        setLoadingMsg(`Uploading ${file.name} in chunks…`);
        uploadId = await uploadFileInChunks(file, ds.id, "expression", (pct) => {
          setLoadingMsg(`Uploading ${file.name} (${pct}%)…`);
        });
      }

      updateDs({
        expressionFile: file,
        expressionFileName: file.name,
        expressionFilePath: desktopPath || undefined,
        expressionUploadId: uploadId,
        expressionRawText: "",
        columns: cleanColumns,
        geneInfoCols: cleanColumns.length > 0 ? [cleanColumns[0]] : [],
        geneIdCol: geneIdCol,
        detectedGeneIdType: parsed.detectedGeneIdType,
        geneIdType: parsed.detectedGeneIdType,
        fs_featuresOrientation: ds.fs_featuresOrientation || "column",
        fs_featureIndexValue: ds.fs_featureIndexValue || 1,
        fs_datasetPurpose: state.fsDatasets.length === 1 ? "train-and-test" : "train",
        fs_isInternalValidation: state.fsDatasets.length > 1 ? true : false,
        nSamples: parsed.nSamples,
        nFeatures: parsed.nFeatures,
        parsedData: cleanParsedData,
        hasNA: parsed.hasNA,
        integrityOk: true,
        integrityIssues: [],
        uploadDone: false,
      });
    } catch (error: any) {
      updateDs({
        integrityOk: false,
        integrityIssues: [`Error parsing expression file: ${error.message}`],
      });
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

      const groupCol = parsed.clinicalColumns.find(c => c !== parsed.clinicalSampleIdCol) || "";
      let autoPos = "";
      let autoNeg = "";
      if (groupCol && parsed.clinicalColumns && parsed.clinicalParsedData) {
        const gIdx = parsed.clinicalColumns.indexOf(groupCol);
        if (gIdx !== -1) {
          const vals = Array.from(new Set(parsed.clinicalParsedData.map((r: any) => r[gIdx]).filter((v: any) => v !== undefined && v !== null && String(v).trim() !== ""))).map(String);
          if (vals.length === 2) {
            autoPos = vals[0];
            autoNeg = vals[1];
          }
        }
      }

      const issues = (ds.integrityIssues || []).filter(i => !i.includes("Clinical"));
      updateDs({
        clinicalFile: file,
        clinicalFileName: file.name,
        clinicalFilePath: desktopPath || undefined,
        clinicalUploadId: uploadId,
        clinicalRawText: "",
        clinicalSampleIdCol: parsed.clinicalSampleIdCol,
        clinicalGroupCol: groupCol,
        fs_positiveClass: autoPos,
        fs_negativeClass: autoNeg,
        clinicalColumns: parsed.clinicalColumns,
        clinicalParsedData: parsed.clinicalParsedData,
        integrityOk: issues.length === 0 && (!!ds.expressionFileName || !requireClinical),
        integrityIssues: issues,
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

  // ----------------------------------------
  // Helper calculations for FS UI validation
  // ----------------------------------------
  const totalCols = (ds.columns || []).length;
  const totalRows = ds.nFeatures || (ds.parsedData || []).length;
  const nonSampleCount = (ds.geneInfoCols || []).length;
  
  let computedSamples = ds.nSamples || 0;
  let computedFeatures = ds.nFeatures || 0;

  if (ds.fs_featuresOrientation === "headers") {
    // Columns are Features, Rows are Samples
    // Target identifier column index (1-based index mapped to 0-based array position)
    const targetIdx = ds.fs_featureIndexValue ? ds.fs_featureIndexValue - 1 : 0;
    
    // Total sample count matches number of data rows minus non-sample configuration offsets if any apply
    computedSamples = ds.nFeatures || totalRows;
    
    // Total features equals total columns minus index identifiers and explicitly defined non-sample columns
    let excludedColsCount = nonSampleCount;
    if (ds.geneInfoCols && ds.columns && !ds.geneInfoCols.includes(ds.columns[targetIdx])) {
      excludedColsCount += 1;
    } else if (!ds.geneInfoCols) {
      excludedColsCount = 1;
    }
    computedFeatures = Math.max(0, totalCols - excludedColsCount);
  } else {
    // Features are Rows, Columns are Samples
    const targetIdx = ds.fs_featureIndexValue ? ds.fs_featureIndexValue - 1 : 0;
    
    // Total features equals total data rows in the uploaded file
    computedFeatures = ds.nFeatures || totalRows;
    
    // Total samples equals total columns minus index identifiers and explicitly defined non-sample columns
    let excludedColsCount = nonSampleCount;
    if (ds.geneInfoCols && ds.columns && !ds.geneInfoCols.includes(ds.columns[targetIdx])) {
      excludedColsCount += 1;
    } else if (!ds.geneInfoCols) {
      excludedColsCount = 1;
    }
    computedSamples = Math.max(0, totalCols - excludedColsCount);
  }
  // ----------------------------------------


  const hasClinical = !!ds.clinicalFileName;
  const liveSamplesCount = hasClinical ? matchingSamples.length : computedSamples;
  const hasMismatch = hasClinical && exprSamples.length > 0 && (matchingSamples.length !== exprSamples.length || clinSamples.length !== exprSamples.length);
  const isNoOverlap = hasClinical && exprSamples.length > 0 && matchingSamples.length === 0;
  const isPartialMatch = hasClinical && exprSamples.length > 0 && matchingSamples.length > 0 && (matchingSamples.length !== exprSamples.length || clinSamples.length !== exprSamples.length);

  // Derive unique class count from the selected group column
  const uniqueGroupValues: string[] = (() => {
    if (!ds.clinicalGroupCol || !ds.clinicalColumns || !ds.clinicalParsedData) return [];
    const gIdx = ds.clinicalColumns.indexOf(ds.clinicalGroupCol);
    if (gIdx === -1) return [];
    const vals = (ds.clinicalParsedData as any[][])
      .map(row => String(row[gIdx] ?? "").trim())
      .filter(v => v !== "" && v !== "NA" && v !== "NaN");
    return [...new Set(vals)];
  })();
  const uniqueGroupCount = uniqueGroupValues.length;
  const hasMultipleClasses = ds.clinicalGroupCol !== "" && uniqueGroupCount !== 2;
  const hasUnsetClasses = ds.clinicalGroupCol !== "" && (
    !ds.fs_positiveClass ||
    !ds.fs_negativeClass ||
    ds.fs_positiveClass === ds.fs_negativeClass ||
    !uniqueGroupValues.includes(ds.fs_positiveClass) ||
    !uniqueGroupValues.includes(ds.fs_negativeClass)
  );

  // Auto-sync positive and negative classes when group column changes or when one changes
  useEffect(() => {
    if (!ds || !ds.clinicalGroupCol || uniqueGroupValues.length !== 2) return;
    const [valA, valB] = uniqueGroupValues;
    const currentPos = ds.fs_positiveClass;
    const currentNeg = ds.fs_negativeClass;
    if (!currentPos && !currentNeg) {
      updateDs({ fs_positiveClass: valA, fs_negativeClass: valB });
    } else if (currentPos && (!currentNeg || currentNeg === currentPos || !uniqueGroupValues.includes(currentNeg))) {
      const nextNeg = uniqueGroupValues.find(v => v !== currentPos) || "";
      if (nextNeg !== currentNeg) {
        updateDs({ fs_negativeClass: nextNeg });
      }
    } else if (currentNeg && (!currentPos || currentPos === currentNeg || !uniqueGroupValues.includes(currentPos))) {
      const nextPos = uniqueGroupValues.find(v => v !== currentNeg) || "";
      if (nextPos !== currentPos) {
        updateDs({ fs_positiveClass: nextPos });
      }
    }
  }, [ds?.clinicalGroupCol, JSON.stringify(uniqueGroupValues), ds?.fs_positiveClass, ds?.fs_negativeClass]);

  const hasDanger =
    isNoOverlap ||
    hasMissingClinicalValues ||
    liveSamplesCount < 30 ||
    (minGroupSampleSize !== undefined && minGroupSampleSize < 2) ||
    hasMultipleClasses ||
    hasUnsetClasses ||
    !ds.integrityOk ||
    ((ds.integrityIssues?.length ?? 0) > 0);

  const canProceed = ds.expressionFileName !== "" && 
    (!requireClinical || ds.clinicalFileName !== "") &&
    (!hasClinical || (ds.clinicalSampleIdCol !== "" && matchingSamples.length >= 30)) &&
    liveSamplesCount >= 30 &&
    (minGroupSampleSize === undefined || minGroupSampleSize >= 2) &&
    !hasMultipleClasses &&
    !hasUnsetClasses;

  const submittedCfg = state.fsSubmittedConfig;
  const hasDatasetCompositionChanged = Boolean(
    submittedCfg && (
      state.fsDatasets.length !== Object.keys(submittedCfg.fs_trainRatios || {}).length ||
      state.fsDatasets.some(d => !submittedCfg.fs_trainRatios?.[d.id])
    )
  );

  const hasSettingsChanged = (ds.uploadDone && (
    ds.expressionFileName !== ds.submittedExpressionFileName ||
    ds.clinicalFileName !== ds.submittedClinicalFileName ||
    ds.clinicalSampleIdCol !== ds.submittedClinicalSampleIdCol ||
    ds.clinicalGroupCol !== ds.submittedClinicalGroupCol ||
    ds.fs_positiveClass !== ds.submitted_fs_positiveClass ||
    ds.fs_negativeClass !== ds.submitted_fs_negativeClass ||
    ds.isNormalized !== ds.submittedIsNormalized ||
    ds.dataType !== ds.submittedDataType ||
    ds.platform !== ds.submittedPlatform ||
    ds.geneIdCol !== ds.submittedGeneIdCol ||
    ds.geneIdType !== ds.submittedGeneIdType ||
    ds.fs_featuresOrientation !== ds.submittedFsFeaturesOrientation ||
    ds.fs_featureIndexValue !== ds.submittedFsFeatureIndexValue ||
    ds.fs_datasetPurpose !== ds.submittedFsDatasetPurpose ||
    ds.fs_isInternalValidation !== ds.submittedFsIsInternalValidation
  )) || hasDatasetCompositionChanged;
  
  const trainCount = state.fsDatasets.filter(d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || !d.fs_datasetPurpose).length;
  const hasInternalVal = state.fsDatasets.some(d => (d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || !d.fs_datasetPurpose) && d.fs_isInternalValidation);
  const testCount = state.fsDatasets.filter(d => d.fs_datasetPurpose === "test" || d.fs_datasetPurpose === "train-and-test").length;

  const allTrainHaveInternalVal = state.fsDatasets
    .filter(d => d.fs_datasetPurpose === "train" || d.fs_datasetPurpose === "train-and-test" || !d.fs_datasetPurpose)
    .every(d => d.fs_isInternalValidation);

  const groupSizeErrors: string[] = [];
  state.fsDatasets.forEach(d => {
    const isUploaded = d.uploadDone || d.id === state.fsCurrentDatasetId;
    if (isUploaded) {
      // Find the group column name and sample groups
      const cols = d.clinicalColumns || [];
      const rows = d.clinicalParsedData || [];
      const groupCol = d.clinicalGroupCol || "Group";
      const groupIdx = cols.indexOf(groupCol);
      if (groupIdx !== -1 && rows.length > 0) {
        // Calculate splits if internal validation or testing
        const isTesting = d.fs_datasetPurpose === "test";
        const isTrainAndTest = d.fs_datasetPurpose === "train-and-test";
        const isInternalVal = (d.fs_datasetPurpose === "train" || !d.fs_datasetPurpose) && d.fs_isInternalValidation;

        if (isTesting || isTrainAndTest || isInternalVal) {
          
          // Map samples in clinical to their group value
          const sampleGroupsMap: Record<string, string> = {};
          rows.forEach(row => {
            const sampleId = row[cols.indexOf(d.clinicalSampleIdCol || "SampleID")];
            const grp = row[groupIdx];
            if (sampleId && grp) {
              sampleGroupsMap[sampleId] = grp;
            }
          });

          // Intersect with expression samples to get actual matched samples
          const exprSamples = d.fs_featuresOrientation === "headers" 
            ? (d.parsedData ? d.parsedData.map(r => r[(d.fs_featureIndexValue || 1) - 1]).filter(Boolean) : [])
            : d.columns.filter((c, idx) => idx !== ((d.fs_featureIndexValue || 1) - 1) && !(d.geneInfoCols || []).includes(c));
          const activeMatched = exprSamples.filter(s => !!sampleGroupsMap[s]);
          const groupCounts: Record<string, number> = {};
          activeMatched.forEach(s => {
            const grp = sampleGroupsMap[s];
            groupCounts[grp] = (groupCounts[grp] || 0) + 1;
          });

          const minGroupCount = Math.min(...Object.values(groupCounts));

          if (minGroupCount <= 10) {
            groupSizeErrors.push(
              `${d.name}: The group with the minimum sample size (${minGroupCount} samples) is not larger than 10.`
            );
          }
        }
      }
    }
  });

  const sorted = [...state.fsDatasets].sort((a, b) => a.id.localeCompare(b.id));
  const idx = sorted.findIndex(d => d.id === datasetId);
  const isLastDataset = state.fsDatasets.length <= 1 || idx === sorted.length - 1;

  // Compute shared feature count across all datasets (only meaningful on the last dataset step)
  // Aware of feature orientation: when features are column names (headers) vs in a single column (rows).
  const getFsDatasetFeatureIds = (d: typeof sorted[0]): Set<string> => {
    if (d.fs_featuresOrientation === "headers") {
      const sampleIdCol = d.clinicalSampleIdCol || d.geneIdCol || "SampleID";
      const infoCols = d.geneInfoCols || [];
      const featureCols = (d.columns || []).filter(c => c !== sampleIdCol && !infoCols.includes(c));
      return new Set(featureCols);
    }
    if (!d.parsedData || d.parsedData.length === 0) return new Set();
    const geneIdIdx = d.columns ? d.columns.indexOf(d.geneIdCol) : -1;
    if (geneIdIdx === -1) return new Set(d.parsedData.map(row => String(row[0])));
    return new Set(d.parsedData.map(row => String(row[geneIdIdx])));
  };

  const sharedFsFeatureCount: number = (() => {
    if (state.fsDatasets.length <= 1) return 0;
    if (sorted.some(d => !d.parsedData || d.parsedData.length === 0)) return 0;
    let intersection = getFsDatasetFeatureIds(sorted[0]);
    for (let i = 1; i < sorted.length; i++) {
      const next = getFsDatasetFeatureIds(sorted[i]);
      intersection = new Set([...intersection].filter(x => next.has(x)));
    }
    return intersection.size;
  })();

  const sharedFsFeatureError: string | null =
    state.fsDatasets.length > 1 &&
    isLastDataset &&
    sorted.every(d => d.parsedData && d.parsedData.length > 0) &&
    sharedFsFeatureCount <= 10
      ? `Only ${sharedFsFeatureCount} shared feature(s) found across datasets. At least 11 shared features are required to proceed.`
      : null;

  // Cross-dataset purpose allocation is deferred to ModelSelectionStep.
  const validationError = isLastDataset ? (sharedFsFeatureError ?? null) : null;
  const isBlockedByValidation = state.fsDatasets.length > 1 && isLastDataset && !!validationError;

  
  const completedModules: string[] = [];
  if (state.standaloneFsDone) completedModules.push("Feature Selection / Model Selection");

  const downstreamDone = completedModules.length > 0;

  const handleNext = async () => {
    setLoading(true);
    setLoadingMsg("Uploading dataset to server…");
    let datasetToUpload = { ...ds };
    if (ds.clinicalFileName) {
      datasetToUpload = filterDatasetForUpload(ds, "overlap");
    }

    let missingCount = ds.processingMissingValuesCount || 0;
    let matchedGroups: string[] | undefined = ds.groups;
    let finalMatchingSamples = matchingSamples;
    let finalMissingClinSamples = missingClinSamples;
    let finalClinicalNoExprSamples = clinicalNoExprSamples;
    let finalClinicalColumns = datasetToUpload.clinicalColumns;

    try {
      if (datasetToUpload.expressionFileName && (datasetToUpload.parsedData || datasetToUpload.expressionFilePath || datasetToUpload.expressionUploadId)) {
        const res = await uploadFSDatasetsAPI([datasetToUpload]);
        if (res && res.status === "success" && Array.isArray(res.datasets)) {
          const matched = res.datasets.find((d: any) => d.datasetId === ds.id);
          if (matched) {
            if (matched.missingValuesCount !== undefined) {
              missingCount = matched.missingValuesCount;
            }
            if (matched.total !== undefined && matched.total > 0) {
              datasetToUpload.nFeatures = matched.total;
            }
            if (matched.samples !== undefined && matched.samples > 0) {
              datasetToUpload.nSamples = matched.samples;
            }
            if (matched.groups && Array.isArray(matched.groups)) {
              matchedGroups = matched.groups;
            }
            if (matched.matchingSamples && Array.isArray(matched.matchingSamples)) {
              finalMatchingSamples = matched.matchingSamples;
            }
            if (matched.clinicalColumns && Array.isArray(matched.clinicalColumns)) {
              finalClinicalColumns = matched.clinicalColumns;
            }
            if (matched.missingClinSamples && Array.isArray(matched.missingClinSamples)) {
              finalMissingClinSamples = matched.missingClinSamples;
            }
            if (matched.clinicalNoExprSamples && Array.isArray(matched.clinicalNoExprSamples)) {
              finalClinicalNoExprSamples = matched.clinicalNoExprSamples;
            }
          }
        }
      }
    } catch (e) {
      console.error("Failed to upload dataset to server:", e);
    }

    // Only run cross-dataset server-side validation on the last dataset upload.
    // For intermediate datasets, we avoid sending incomplete datasets to the backend.
    if (isLastDataset) {
      setLoadingMsg("Checking sample size sufficiency on server…");
      try {
        const updatedDatasets = state.fsDatasets.map(d => {
          if (d.id === datasetId) {
            return {
              ...d,
              uploadDone: true,
              fs_datasetPurpose: state.fsDatasets.length === 1 ? "train-and-test" : (ds.fs_datasetPurpose || "train"),
              nSamples: datasetToUpload.nSamples,
              nFeatures: datasetToUpload.nFeatures || ds.nFeatures || 0,
              columns: datasetToUpload.columns,
              parsedData: datasetToUpload.parsedData,
              processingMissingValuesCount: missingCount,
              hasNA: missingCount > 0,
              clinicalFile: datasetToUpload.clinicalFile,
              clinicalFileName: datasetToUpload.clinicalFileName,
              clinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
              clinicalGroupCol: datasetToUpload.clinicalGroupCol,
              clinicalBatchCol: datasetToUpload.clinicalBatchCol,
              fs_positiveClass: datasetToUpload.fs_positiveClass,
              fs_negativeClass: datasetToUpload.fs_negativeClass,
              minGroupSampleSize: minGroupSampleSize,
            };
          }
          return d;
        });

        const valRes = validateFSSamplesLocal(updatedDatasets, state.fsConfig.trainRatio || 0.7);
        if (!valRes.sufficient) {
          setLoading(false);
          alert(valRes.message || "Insufficient sample size to continue to Model Selection.");
          return;
        }
      } catch (e: any) {
        console.error("Failed to validate FS sample size:", e);
      }
    }
    setLoading(false);
    updateDs({
      uploadDone: true,
      columns: datasetToUpload.columns,
      parsedData: datasetToUpload.parsedData,
      nSamples: datasetToUpload.nSamples,
      nFeatures: datasetToUpload.nFeatures || ds.nFeatures || 0,
      groups: matchedGroups,
      processingMissingValuesCount: missingCount,
      hasNA: missingCount > 0,
      clinicalFile: datasetToUpload.clinicalFile,
      clinicalFileName: datasetToUpload.clinicalFileName,
      clinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      clinicalGroupCol: datasetToUpload.clinicalGroupCol,
      fs_positiveClass: datasetToUpload.fs_positiveClass,
      fs_negativeClass: datasetToUpload.fs_negativeClass,
      clinicalParsedData: datasetToUpload.clinicalParsedData,
      clinicalColumns: finalClinicalColumns,
      submittedExpressionFileName: ds.expressionFileName,
      submittedClinicalFileName: datasetToUpload.clinicalFileName,
      submittedClinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      submittedClinicalGroupCol: datasetToUpload.clinicalGroupCol,
      submitted_fs_positiveClass: datasetToUpload.fs_positiveClass,
      submitted_fs_negativeClass: datasetToUpload.fs_negativeClass,
      submittedFsFeaturesOrientation: ds.fs_featuresOrientation,
      submittedFsFeatureIndexValue: ds.fs_featureIndexValue,
      submittedFsDatasetPurpose: ds.fs_datasetPurpose,
      submittedFsIsInternalValidation: ds.fs_isInternalValidation,
      matchingSamples: finalMatchingSamples,
      missingClinSamples: finalMissingClinSamples,
      clinicalNoExprSamples: finalClinicalNoExprSamples,
      hasMissingClinicalValues: hasMissingClinicalValues,
      removedSamples: removedSamples,
    });
    
    const sorted = [...state.fsDatasets].sort((a, b) => a.id.localeCompare(b.id));
    const idx = sorted.findIndex(d => d.id === datasetId);

    if (state.fsDatasets.length > 1) {
      if (idx < sorted.length - 1) {
        const nextDs = sorted[idx + 1];
        dispatch({ type: "FS_SELECT_DATASET", id: nextDs.id });
        dispatch({ type: "FS_SET_CONTEXT", id: nextDs.id });
        dispatch({ type: "FS_SET_STEP", step: "upload" });
      } else {
        dispatch({ type: "FS_SET_STEP", step: "all-datasets" });
        dispatch({ type: "FS_SET_CONTEXT", id: "all" });
      }
    } else if (state.fsSelectedContext !== "all") {
      dispatch({ type: "FS_SET_STEP", step: "model-selection" });
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

  const targetSelectorLabel = ds.fs_featuresOrientation === "column" 
    ? "Feature Column:" 
    : "Sample ID Column:";

  return (
    <>
      {loading && <Spinner label={loadingMsg} sublabel="Please wait…" />}
      {showDiscardModal && (
        <DiscardWarnModal
          stepId="upload"
          datasetIds={[datasetId]}
          modulesList={completedModules}
          onCancel={() => setShowDiscardModal(false)}
          onConfirm={async () => {
            setShowDiscardModal(false);
            dispatch({
              type: "RESET_DOWNSTREAM_STEPS",
              datasetId: datasetId,
              fromStep: "upload"
            } as any);
            await handleNext();
          }}
        />
      )}

      {/* Expression Data Card */}
      <div className="card">
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">Expression Data{multipleDs ? ` for ${ds.name}` : ""}</div>
            <div className="card-sub">Upload your matrix (CSV, TSV, or TXT).</div>
          </div>
          <div style={{ display: "flex", gap: 10, alignItems: "center" }}>
            <button className="link-button danger" onClick={() => handleClear("expression")}>Clear</button>
            <button className="link-button primary" onClick={() => handleLoadSample("expression")}>Example Data</button>
          </div>
        </div>

        {!ds.expressionFileName ? (
          <div
            className={`drop-zone ${expressionDrag ? "dragover" : ""}`}
            style={{ marginTop: 16 }}
            onClick={() => {
              const input = document.createElement("input");
              input.type = "file"; input.accept = ".csv,.tsv,.txt";
              input.onchange = e => { const f = (e.target as HTMLInputElement).files?.[0]; if (f) handleExpressionFile(f); };
              input.click();
            }}
            onDragOver={e => { e.preventDefault(); setExpressionDrag(true); }}
            onDragLeave={() => setExpressionDrag(false)}
            onDrop={e => { e.preventDefault(); setExpressionDrag(false); const f = e.dataTransfer.files?.[0]; if (f) handleExpressionFile(f); }}
          >
            <div className="drop-zone-icon">📊</div>
            <div className="drop-zone-title">Drop expression file here</div>
            <div className="drop-zone-hint">CSV · TSV · TXT</div>
          </div>
        ) : (
          <>
            <div className="file-chip" style={{ marginTop: 14 }}>
              <FileText size={15} />
              <span className="file-chip-name">{ds.expressionFileName}</span>
              <button className="file-chip-clear" onClick={() => handleClear("expression")}>
                <X size={16} />
              </button>
            </div>

            <hr className="card-divider" />

            {/* Dataset Metadata & Data Type */}
            <div style={{ marginBottom: 18, display: "flex", flexDirection: "row", gap: 14, flexWrap: "wrap", alignItems: "flex-start" }}>
              {/* Dataset Name Input */}
              <div style={{ display: "flex", flexDirection: "column", width: 220 }}>
                <label style={{ fontSize: 13, fontWeight: 500, marginBottom: 6 }}>Dataset name</label>
                <input 
                  type="text" 
                  value={ds.name} 
                  onChange={e => updateDs({ name: e.target.value })}
                  style={{ width: "100%", padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                  data-testid="input-dataset-name"
                />
              </div>

              {/* Data Type & Hierarchy Dropdown */}
              <div style={{ display: "flex", flexDirection: "column", width: 220, position: "relative" }}>
                <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 6 }}>
                  <label style={{ fontSize: 13, fontWeight: 500 }}>Data Type</label>
                  <span style={{
                    fontSize: 10, fontWeight: 700, padding: "1px 6px", borderRadius: 4,
                    background: getHierarchyGroup(ds.dataType || "readcounts") === "transcriptomics" ? "hsl(214 50% 93%)" : (getHierarchyGroup(ds.dataType || "readcounts") === "proteomics" ? "hsl(140 40% 93%)" : "hsl(30 50% 93%)"),
                    color: getHierarchyGroup(ds.dataType || "readcounts") === "transcriptomics" ? "hsl(214 58% 30%)" : (getHierarchyGroup(ds.dataType || "readcounts") === "proteomics" ? "hsl(140 50% 25%)" : "hsl(30 60% 30%)"),
                    border: "1px solid var(--border)"
                  }}>
                    {getHierarchyGroup(ds.dataType || "readcounts").toUpperCase()}
                  </span>
                </div>

                <details style={{ width: "100%" }}>
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>
                      {ds.dataType === "microarray" && "Microarray"}
                      {ds.dataType === "proteomics" && "Proteomics"}
                      {ds.dataType === "others" && "Others (Untyped)"}
                      {(!ds.dataType || ds.dataType === "readcounts") && "RNA-seq (Readcounts)"}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {[
                      { value: "readcounts", label: "RNA-seq (Readcounts)" },
                      { value: "microarray", label: "Microarray probe intensities" },
                      { value: "proteomics", label: "Proteomics" },
                      { value: "others", label: "Others (eg. metabolomics, lipidomics)" }
                    ].map((option) => {
                      const isSelected = (ds.dataType || "readcounts") === option.value;
                      return (
                        <div
                          key={option.value}
                          onClick={(e) => {
                            const newType = option.value as DataType;
                            const patch: Partial<typeof ds> = { dataType: newType };
                            if (newType !== "microarray") {
                              patch.platform = "";
                            }
                            updateDs(patch);
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px", fontSize: 12, cursor: "pointer",
                            background: isSelected ? "var(--selected-bg)" : "transparent",
                            fontWeight: isSelected ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = isSelected ? "var(--selected-bg)" : "transparent"}
                        >
                          {option.label}
                        </div>
                      );
                    })}
                  </div>
                </details>
              </div>
            </div>

            {/* Configuration */}
            <div style={{ marginBottom: 18, display: "flex", flexDirection: "row", gap: 14 }}> 
              {/* Orientation Selector Dropdown */}
              <div style={{ display: "flex", flexDirection: "column", width: 200, position: "relative" }}>
                <label style={{ fontSize: 13, fontWeight: 500, marginBottom: 5 }}>Features Orientation</label>
                <details style={{ width: "100%" }}>
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>
                      {ds.fs_featuresOrientation === "column" ? "Features are Rows" : "Features are Headers"}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>
                  
                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4, 
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,  
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    <div
                      onClick={(e) => {
                        updateDs({ fs_featuresOrientation: "headers" });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px", fontSize: 12, cursor: "pointer",
                        background: ds.fs_featuresOrientation !== "column" ? "var(--selected-bg)" : "transparent",
                        fontWeight: ds.fs_featuresOrientation !== "column" ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = ds.fs_featuresOrientation !== "column" ? "var(--selected-bg)" : "transparent"}
                    >
                      Features are Headers
                    </div>

                    <div
                      onClick={(e) => {
                        updateDs({ fs_featuresOrientation: "column" });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px", fontSize: 12, cursor: "pointer",
                        background: ds.fs_featuresOrientation === "column" ? "var(--selected-bg)" : "transparent",
                        fontWeight: ds.fs_featuresOrientation === "column" ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = ds.fs_featuresOrientation === "column" ? "var(--selected-bg)" : "transparent"}
                    >
                      Features are Rows
                    </div>
                  </div>
                </details>
              </div>

              {/* Target Selector Column */}
              <div style={{ display: "flex", flexDirection: "column", width: 200, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 4 }}>{targetSelectorLabel}</label>
                <details style={{ width: "100%" }} data-testid="details-fs-feature-index">
                  <summary style={{display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"}}>
                    <span>
                      {ds.fs_featureIndexValue 
                        ? (ds.columns[ds.fs_featureIndexValue - 1] || "Select Column") 
                        : (ds.columns && ds.columns.length > 0 ? ds.columns[0] : "— Select Column —")}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>
                  
                  <div style={{position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4, 
                  border: "1px solid var(--border)",  borderRadius: 7,  padding: "4px 0",  maxHeight: 160,  
                  overflowY: "auto",  background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"}}>
                    {(ds.columns || []).map((col, idx) => {
                      const isSelected = ds.fs_featureIndexValue 
                        ? ds.fs_featureIndexValue === idx + 1 
                        : idx === 0;

                      return (
                        <div
                          key={`${col}-${idx}`}
                          onClick={(e) => {
                            updateDs({ fs_featureIndexValue: idx + 1 });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: isSelected ? "var(--selected-bg)" : "transparent",
                            fontWeight: isSelected ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = isSelected ? "var(--selected-bg)" : "transparent"}
                        >
                          {col}
                        </div>
                      );
                    })}
                  </div>
                </details>
              </div>

              {/* Non-sample columns Selector */}
              <div style={{ display: "flex", flexDirection: "column", width: 200, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Non-sample Columns</label>
                <details style={{ width: "100%" }}>
                  <summary style={{display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"}}>
                    <span>
                      {(!ds.geneInfoCols || ds.geneInfoCols.length === 0)
                        ? "" 
                        : `${ds.geneInfoCols.length} selected`}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>
                  
                  <div style={{position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4, 
                  border: "1px solid var(--border)",  borderRadius: 7,  padding: "8px 10px",  maxHeight: 160,  
                  overflowY: "auto",  background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"}}>
                    {(ds.columns || []).map(col => (
                      <label key={col} style={{ display: "flex", alignItems: "center", gap: 7, fontSize: 12, cursor: "pointer", padding: "4px 0" }}>
                        <input type="checkbox" checked={(ds.geneInfoCols || []).includes(col)}
                          onChange={e => {
                            const currentCols = ds.geneInfoCols || [];
                            const next = e.target.checked ? [...currentCols, col] : currentCols.filter(c => c !== col);
                            updateDs({ geneInfoCols: next });
                          }}
                          style={{ accentColor: "var(--primary)" }}
                          data-testid={`check-nonsample-${col}`} />
                        {col}
                      </label>
                    ))}
                  </div>
                </details>
              </div>
            </div>

            {/* Data Summary Block */}
            {!!ds.expressionFileName && (
              <>
                <hr className="card-divider" />
                <div style={{ fontSize: 12, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 10, textTransform: "uppercase", letterSpacing: ".04em" }}>
                  Data Summary
                </div>
                <div className="chips-row" style={{ marginBottom: 16 }}>
                  <div className="stat-chip">
                    <div className="stat-chip-val">{computedSamples}</div>
                    <div className="stat-chip-lbl">Samples</div>
                  </div>
                  <div className="stat-chip">
                    <div className="stat-chip-val">{computedFeatures.toLocaleString()}</div>
                    <div className="stat-chip-lbl">Features</div>
                  </div>
                </div>
              </>
            )}

            {/* Preview table */}
            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Preview (first 10 rows)
            </div>
            <div className="preview-wrap">
              <table>
                <thead><tr>{(ds.columns && ds.columns.length > 0 ? ds.columns : []).map(col => <th key={col}>{col}</th>)}</tr></thead>
                <tbody>{(ds.parsedData && ds.parsedData.length > 0 ? ds.parsedData : []).slice(0, 10).map((row, i) => <tr key={i}>{row.map((cell, j) => <td key={j}>{cell}</td>)}</tr>)}</tbody>
              </table>
            </div>
          </>
        )}
      </div>

      {/* Clinical Data Upload Card */}
      <div className="card">
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">
              Clinical / Phenotype Data{multipleDs ? ` for ${ds.name}` : ""}
              <span style={{
                marginLeft: 8, fontSize: 11, 
                background: requireClinical ? "hsl(0 70% 96%)" : "var(--muted)", 
                color: requireClinical ? "hsl(0 65% 40%)" : "var(--muted-foreground)",
                border: requireClinical ? "1px solid hsl(0 50% 85%)" : "1px solid var(--border)", 
                padding: "2px 7px", borderRadius: 4, fontWeight: 500,
              }}>
                {requireClinical ? "Required" : "Optional"}
              </span>
            </div>
            <div className="card-sub">Upload sample metadata with condition/group assignments.</div>
          </div>
          <div style={{ display: "flex", gap: 10 }}>
            <button className="link-button danger" onClick={() => handleClear("clinical")}>Clear</button>
            <button className="link-button primary" onClick={() => handleLoadSample("clinical")}>Example Data</button>
          </div>
        </div>

        {!ds.clinicalFileName ? (
          <div className={`drop-zone ${clinicalDrag ? "dragover" : ""}`} style={{ marginTop: 16, marginBottom: 16 }}
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
            <div className="file-chip" style={{ marginTop: 14, marginBottom: 16 }}>
              <FileText size={15} />
              {isPartialMatch || isNoOverlap ? (
                <>
                  <span className="file-chip-name">
                    {ds.clinicalFileName}
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
              <button className="file-chip-clear" onClick={() => handleClear("clinical")}><X size={16} /></button>
            </div>
            <hr className="card-divider" />
            <div style={{ marginTop: 14, display: "flex", gap: 16, flexWrap: "wrap" }}>
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
                    {(ds.clinicalColumns || []).map(c => (
                      <div
                        key={c}
                        onClick={(e) => {
                          updateDs({ clinicalSampleIdCol: c });
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

              <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Group Column</label>
                <details style={{ width: "100%" }} data-testid="details-clinical-group-col">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{ds.clinicalGroupCol || "Select column..."}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {(ds.clinicalColumns || []).map(c => (
                      <div
                        key={c}
                        onClick={(e) => {
                          let autoPos = "";
                          let autoNeg = "";
                          if (ds.clinicalColumns && ds.clinicalParsedData) {
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
                            fs_positiveClass: autoPos,
                            fs_negativeClass: autoNeg
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

              {/* Positive Class and Negative Class for Feature Selection (REQUIRED) */}
              {ds.clinicalGroupCol && (
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
                                fs_negativeClass: autoNeg
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
                                fs_positiveClass: autoPos
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
            </div>

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

            {/* Clinical preview */}
            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Clinical Data Preview
            </div>
            <div className="preview-wrap">
              <table>
                <thead><tr>{(ds.clinicalColumns && ds.clinicalColumns.length > 0 ? ds.clinicalColumns : []).map(col => <th key={col}>{col}</th>)}</tr></thead>
                <tbody>{(ds.clinicalParsedData && ds.clinicalParsedData.length > 0 ? ds.clinicalParsedData : []).slice(0, 8).map((row, i) => <tr key={i}>{row.map((cell, j) => <td key={j}>{cell}</td>)}</tr>)}</tbody>
              </table>
            </div>
          </>
        )}
      </div>
      

      {/* Integrity check */}
      {ds.expressionFileName && (
        <div className="card">
          <div className="card-title" style={{ marginBottom: 12 }}>Data Integrity Check</div>
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
                    <div className="stat-chip-val">{missingClinSamples.length}</div>
                    <div className="stat-chip-lbl">Missing Clinical Info</div>
                  </div>
                  {clinicalNoExprSamples.length > 0 && (
                    <div className="stat-chip">
                      <div className="stat-chip-val">
                        {clinicalNoExprSamples.length}
                      </div>
                      <div className="stat-chip-lbl">Missing Expression</div>
                    </div>
                  )}
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
                        <head></head>
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
                There are missing values in the selected group column. Those samples will be removed.
              </span>
            </div>
          )}
          {liveSamplesCount < 30 && (
            <div className="banner danger" style={{ marginBottom: 10 }}>
              <AlertTriangle size={15} style={{ flexShrink: 0 }} />
              <span>Danger: Dataset must have at least 30 matching samples (currently has {liveSamplesCount}).</span>
            </div>
          )}
          {minGroupSampleSize !== undefined && minGroupSampleSize < 2 && (
            <div className="banner danger" style={{ marginBottom: 10 }}>
              <AlertTriangle size={15} style={{ flexShrink: 0 }} />
              <span>
                Danger: The sample size of the group with the lowest sample size is less than 2 ({minGroupSampleSize}). Cannot proceed with feature selection.
              </span>
            </div>
          )}
          {hasMultipleClasses && (
            <div className="banner danger" style={{ marginBottom: 10 }}>
              <AlertTriangle size={15} style={{ flexShrink: 0 }} />
              <span>
                <strong>Incorrect class count detected ({uniqueGroupCount} classes: {uniqueGroupValues.join(", ")}).</strong>{" "}
                Feature selection only supports binary classification (exactly 2 classes). Please select a different group column or recode your groups to have exactly 2 distinct classes.
              </span>
            </div>
          )}
          {!hasMultipleClasses && hasUnsetClasses && (
            <div className="banner danger" style={{ marginBottom: 10 }}>
              <AlertTriangle size={15} style={{ flexShrink: 0 }} />
              <span>
                Please select both a Positive Class and a Negative Class for binary classification.
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
          {!hasMismatch && hasClinical && !hasDanger && (
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
        </div>
      )}



      <div className="action-row" style={{ display: "flex", gap: 8, justifyContent: "flex-end", marginTop: 16 }}>
        {(() => {
          const sorted = [...state.fsDatasets].sort((a, b) => a.id.localeCompare(b.id));
          const idx = sorted.findIndex(d => d.id === datasetId);
          if (state.fsDatasets.length > 1 && idx > 0) {
            return (
              <button 
                type="button"
                className="btn btn-default" 
                onClick={() => {
                  const prevDs = sorted[idx - 1];
                  dispatch({ type: "FS_SELECT_DATASET", id: prevDs.id });
                  dispatch({ type: "FS_SET_CONTEXT", id: prevDs.id });
                  dispatch({ type: "FS_SET_STEP", step: "upload" });
                }}
                data-testid="btn-upload-back"
              >
                ← Back
              </button>
            );
          } else {
            return (
              <button 
                type="button"
                className="btn btn-default" 
                onClick={() => navigate("/")}
                data-testid="btn-upload-back"
              >
                ← Back
              </button>
            );
          }
        })()}
        
        {/* The button disabled state checks for validationError to block continuous progression */}
        <button 
          className="btn btn-primary" 
          disabled={!canProceed || isBlockedByValidation} 
          onClick={onContinueClick} 
          data-testid="btn-next-upload"
        >
          {state.fsDatasets.length > 1 ? (
            idx < sorted.length - 1 ? "Next Dataset →" : "Continue to Upload Summary →"
          ) : (
            "Continue to Model Selection →"
          )}
        </button>
      </div>

      {showWarnModal && (
        <WarnSampleModal
          type="partial_overlap"
          onConfirm={handleConfirmWarn}
          onCancel={() => setShowWarnModal(false)}
        />
      )}
    </>
  );
}