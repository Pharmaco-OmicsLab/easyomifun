import { useState, useEffect, useMemo } from "react";
import { useLocation } from "wouter";
import { FileText, X, AlertTriangle, CheckCircle, Download } from "lucide-react";
import { useAppStore } from "../../store/appStore";
import { useMicroarrayPlatforms } from "../../hooks/useMicroarrayPlatforms";
import Spinner from "../Spinner";
import type { DataType, Platform } from "../../dataObject";
import { parseExpressionFile, parseClinicalFile, detectGeneIdType, alignSamplesLocal, unparseCSV, getDesktopFilePath, getExpressionSampleColumns, isAnnotationOrInfoColumn } from "../../lib/dataParser";
import { uploadFileInChunks } from "../../lib/chunkUploader";
import { fetchSampleDataAPI, uploadRawDatasetDataAPI, clearDatasetAPI } from "../../lib/api";
import WarnSampleModal from "../shared/WarnSampleModal";
import DiscardWarnModal from "../shared/DiscardWarnModal";

function detectIsNormalized(rows: string[][]): boolean {
  if (!rows || rows.length === 0) return false;
  return rows.some(row => row.slice(4).some(cell => cell.trim().includes(".")));
}

function detectHasNA(rows: (string | number)[][]): boolean {
  if (!rows || rows.length === 0) return false;
  for (const row of rows) {
    for (let i = 1; i < row.length; i++) {
      const val = row[i];
      if (val === undefined || val === null) return true;
      const strVal = String(val).trim();
      if (strVal === "" || strVal.toLowerCase() === "na" || strVal.toLowerCase() === "nan" || isNaN(Number(strVal))) {
        return true;
      }
    }
  }
  return false;
}

const GENE_ID_LABELS: Record<string, string> = {
  ensembl: "Ensembl ID",
  entrez: "Entrez ID",
  genename: "Gene Name",
};

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

interface Props {
  datasetId: string;
  showGeneIdType?: boolean;
}

export default function DataUploadStep({ datasetId, showGeneIdType = true }: Props) {
  const { state, dispatch } = useAppStore();
  const [, navigate] = useLocation();
  const ds = state.dpDatasets.find(d => d.id === datasetId);
  const [loading, setLoading] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const [expressionDrag, setExpressionDrag] = useState(false);
  const [clinicalDrag, setClinicalDrag] = useState(false);
  const [showWarnModal, setShowWarnModal] = useState(false);
  const [showDiscardModal, setShowDiscardModal] = useState(false);

  if (!ds) return null;

  const { platforms } = useMicroarrayPlatforms();

  const getGeneIdTypeLabel = (type: string) => {
    if (GENE_ID_LABELS[type]) return GENE_ID_LABELS[type];
    const allEntries = [...platforms.affymetrix, ...platforms.illumina];
    const entry = allEntries.find(e => e.platform === type);
    return entry ? `${entry.platform} (Probe ID)` : type;
  };

  const microarrayGeneIdOptions = useMemo(() => {
    if (ds.dataType !== "microarray" || !ds.platform) return [];
    const entries = ds.platform === "affymetrix"
      ? platforms.affymetrix
      : platforms.illumina;
    return entries.map(e => ({ value: e.platform, label: `${e.platform} (Probe ID)` }));
  }, [ds.dataType, ds.platform, platforms]);

  const geneIdTypeOptions = useMemo(() => {
    if (ds.dataType === "microarray") {
      return [
        { value: "ensembl", label: "Ensembl ID" },
        { value: "entrez", label: "Entrez ID" },
        { value: "genename", label: "Gene Name" },
        ...microarrayGeneIdOptions
      ];
    } else if (ds.dataType === "readcounts") {
      return [
        { value: "ensembl", label: "Ensembl ID" },
        { value: "entrez", label: "Entrez ID" },
        { value: "genename", label: "Gene Name" },
      ];
    } else if (ds.dataType === "proteomics") {
      return [
        { value: "genename", label: "Gene Name" },
        { value: "ensembl", label: "Ensembl ID" },
        { value: "entrez", label: "Entrez ID" },
      ];
    } else {
      return [
        { value: "ensembl", label: "Ensembl ID" },
        { value: "entrez", label: "Entrez ID" },
        { value: "genename", label: "Gene Name" },
      ];
    }
  }, [ds.dataType, microarrayGeneIdOptions]);

  const multipleDs = state.dpDatasets.length > 1;

  const updateDs = (patch: Partial<typeof ds>) => {
    dispatch({ type: "DP_UPDATE_DATASET", id: datasetId, patch } as never);
  };

  const [matchingSamples, setMatchingSamples] = useState<string[]>([]);
  const [missingClinSamples, setMissingClinSamples] = useState<string[]>([]);
  const [clinicalNoExprSamples, setClinicalNoExprSamples] = useState<string[]>([]);
  const [hasMissingClinicalValues, setHasMissingClinicalValues] = useState<boolean>(false);
  const [removedSamples, setRemovedSamples] = useState<string[]>([]);

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
        const res = alignSamplesLocal({
          expressionColumns: currentDs.columns || [],
          geneIdCol: currentDs.geneIdCol || "",
          geneInfoCols: currentDs.geneInfoCols || [],
          clinicalParsedData: currentDs.clinicalParsedData || [],
          clinicalColumns: currentDs.clinicalColumns || [],
          clinicalSampleIdCol: currentDs.clinicalSampleIdCol || "",
          expressionParsedData: currentDs.parsedData || [],
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

        let patch: any = {};
        if (res.nFeatures !== undefined && res.nFeatures !== currentDs.nFeatures && res.nFeatures > 0) {
          patch.nFeatures = res.nFeatures;
        }
        if (res.detectedGeneIdType !== undefined && res.detectedGeneIdType !== currentDs.detectedGeneIdType && res.detectedGeneIdType !== "") {
          patch.detectedGeneIdType = res.detectedGeneIdType;
        }
        if (res.detectedIsNormalized !== undefined && res.detectedIsNormalized !== currentDs.detectedIsNormalized) {
          patch.detectedIsNormalized = res.detectedIsNormalized;
        }
        if (Object.keys(patch).length > 0) {
          updateDs(patch);
        }
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
    ds?.clinicalSampleIdCol,
    ds?.clinicalGroupCol,
    ds?.clinicalBatchCol,
    JSON.stringify(ds?.clinicalColumns || []),
    JSON.stringify(ds?.clinicalParsedData || []),
    JSON.stringify(ds?.columns || []),
    JSON.stringify(ds?.parsedData || [])
  ]);

  useEffect(() => {
    if (ds.dataType === "microarray" && ds.platform === "affymetrix" && ds.isNormalized !== true) {
      updateDs({ isNormalized: true });
    }
  }, [ds.dataType, ds.platform, ds.isNormalized]);

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
        isNormalized: false, detectedIsNormalized: false,
        nSamples: 0, nFeatures: 0, uploadDone: false,
        integrityOk: false, integrityIssues: [],
        parsedData: undefined,
        hasNA: false,
        submittedExpressionFileName: "",
      });
    } else {
      updateDs({
        clinicalFile: null,
        clinicalFileName: "",
        clinicalFilePath: undefined,
        clinicalUploadId: undefined,
        clinicalRawText: "",
        clinicalSampleIdCol: "",
        clinicalGroupCol: "",
        clinicalBatchCol: "",
        integrityOk: true,
        integrityIssues: [],
        clinicalColumns: undefined,
        clinicalParsedData: undefined,
        submittedClinicalFileName: "",
      });
    }
  };

  const handleLoadSample = async (scope: "expression" | "clinical") => {
    setLoading(true);
    try {
      if (scope === "expression") {
        setLoadingMsg("Loading sample expression dataset…");
        const result = await fetchSampleDataAPI("expression");
        const rows = result.parsedData as string[][];
        const autoNorm = detectIsNormalized(rows);
        const autoGeneType = detectGeneIdType(rows[0]?.[0] || "");
        const keepIndices = result.columns.map((c, idx) => ({ name: c, idx }));
        const cleanColumns = keepIndices.map(item => item.name);
        const cleanParsedData = rows.map(row => keepIndices.map(item => row[item.idx]));

        const sampleHasNA = detectHasNA(cleanParsedData);
        const rawText = unparseCSV(cleanColumns, cleanParsedData);
        updateDs({
          expressionFileName: "sample_expression.csv",
          expressionRawText: rawText,
          columns: cleanColumns,
          geneInfoCols: cleanColumns.length > 0 ? [cleanColumns[0]] : [],
          geneIdCol: cleanColumns[0],
          geneIdType: autoGeneType,
          detectedGeneIdType: autoGeneType,
          isNormalized: autoNorm,
          detectedIsNormalized: autoNorm,
          nSamples: cleanColumns.length - 1,
          nFeatures: cleanParsedData.length,
          parsedData: cleanParsedData,
          integrityOk: true,
          integrityIssues: [],
          hasNA: sampleHasNA,
        });
      } else {
        setLoadingMsg("Loading sample clinical data…");
        const result = await fetchSampleDataAPI("clinical");
        const rows = result.parsedData as string[][];
        const rawText = unparseCSV(result.columns, rows);
        updateDs({
          clinicalFileName: "sample_clinical.csv",
          clinicalRawText: rawText,
          clinicalSampleIdCol: result.columns[0] || "SampleID",
          clinicalGroupCol: "",
          clinicalBatchCol: "",
          clinicalColumns: result.columns,
          clinicalParsedData: rows,
          integrityOk: true,
          integrityIssues: [],
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
      const keepIndices = parsed.columns.map((c, idx) => ({ name: c, idx }));
      const cleanColumns = keepIndices.map(item => item.name);
      const cleanParsedData = parsed.parsedData.map(row => keepIndices.map(item => row[item.idx]));
      const fileHasNA = detectHasNA(cleanParsedData);

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
        isNormalized: parsed.isNormalized,
        detectedIsNormalized: parsed.isNormalized,
        nSamples: parsed.nSamples,
        nFeatures: parsed.nFeatures,
        parsedData: cleanParsedData,
        hasNA: fileHasNA,
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

      updateDs({
        clinicalFile: file,
        clinicalFileName: file.name,
        clinicalFilePath: desktopPath || undefined,
        clinicalUploadId: uploadId,
        clinicalRawText: "",
        clinicalSampleIdCol: parsed.clinicalSampleIdCol,
        clinicalGroupCol: "",
        clinicalBatchCol: "",
        clinicalColumns: parsed.clinicalColumns,
        clinicalParsedData: parsed.clinicalParsedData,
        integrityOk: !!ds.expressionFileName,
        integrityIssues: !ds.expressionFileName ? ["Expression data not yet uploaded"] : [],
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
  const hasClinical = !!ds.clinicalFileName;

  const isFullMatch = hasClinical && exprSamples.length > 0 && missingClinSamples.length === 0;
  const isNoOverlap = hasClinical && exprSamples.length > 0 && matchingSamples.length === 0;
  const isPartialMatch = hasClinical && exprSamples.length > 0 && matchingSamples.length > 0 && missingClinSamples.length > 0;
  const hasDanger = isNoOverlap || hasMissingClinicalValues || !ds.integrityOk || ((ds.integrityIssues?.length ?? 0) > 0);

  const showOverlapOptions = isPartialMatch;
  const needsChoice = hasClinical && isPartialMatch;
  const choiceMade = !needsChoice || !!ds.sampleOverlapChoice;

  const canProceed = ds.expressionFileName !== "" && (!ds.clinicalFileName || (ds.clinicalSampleIdCol !== "" && choiceMade));

  const hasSettingsChanged = ds.uploadDone && (
    ds.expressionFileName !== ds.submittedExpressionFileName ||
    ds.clinicalFileName !== ds.submittedClinicalFileName ||
    ds.clinicalSampleIdCol !== ds.submittedClinicalSampleIdCol ||
    ds.clinicalGroupCol !== ds.submittedClinicalGroupCol ||
    ds.clinicalBatchCol !== ds.submittedClinicalBatchCol ||
    ds.isNormalized !== ds.submittedIsNormalized ||
    ds.dataType !== ds.submittedDataType ||
    ds.platform !== ds.submittedPlatform ||
    ds.geneIdCol !== ds.submittedGeneIdCol ||
    ds.geneIdType !== ds.submittedGeneIdType
  );

  const completedModules: string[] = [];
  if (ds.annotationDone) completedModules.push("Annotation");
  if (ds.processingDone) completedModules.push("Processing");
  if (ds.normalizationDone) completedModules.push("Normalization");
  if (ds.batchDone) completedModules.push("Batch Correction");
  if (state.dpInlineDeDone) completedModules.push("DE Analysis");
  if (state.dpInlineFsDone) completedModules.push("Feature Selection");
  if (state.dpInlineEaDone) completedModules.push("Enrichment Analysis");

  const downstreamDone = completedModules.length > 0;

  const handleNext = async () => {
    const clearedClinical = ds.sampleOverlapChoice === "all";
    let datasetToUpload = { ...ds };
    if (ds.clinicalFileName && ds.sampleOverlapChoice === "overlap") {
      datasetToUpload = filterDatasetForUpload(ds, "overlap");
    } else if (clearedClinical) {
      datasetToUpload = filterDatasetForUpload(ds, "all");
    }

    let missingCount = ds.processingMissingValuesCount || 0;
    let matchedGroups: string[] | undefined = ds.groups;
    let finalMatchingSamples = matchingSamples;
    let finalMissingClinSamples = missingClinSamples;
    let finalClinicalNoExprSamples = clinicalNoExprSamples;
    let finalClinicalColumns = datasetToUpload.clinicalColumns;

    if (!ds.uploadDone || hasSettingsChanged || clearedClinical) {
      setLoadingMsg("Uploading data to server…");
      setLoading(true);
      try {
        const res = await uploadRawDatasetDataAPI([datasetToUpload]);
        if (res && res.status === "success" && Array.isArray(res.datasets)) {
          const matched = res.datasets.find((d: any) => d.datasetId === ds.id);
          if (matched) {
            if (matched.missingValuesCount !== undefined) missingCount = matched.missingValuesCount;
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
      } catch (e) {
        console.error("Failed to upload data to server:", e);
      }
      setLoading(false);
    }

    updateDs({
      uploadDone: true,
      columns: datasetToUpload.columns,
      parsedData: datasetToUpload.parsedData,
      processingMissingValuesCount: missingCount,
      hasNA: missingCount > 0,
      nSamples: datasetToUpload.nSamples,
      nFeatures: datasetToUpload.nFeatures || ds.nFeatures || 0,
      groups: matchedGroups,
      submittedExpressionFileName: ds.expressionFileName,
      submittedClinicalFileName: datasetToUpload.clinicalFileName,
      submittedClinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      submittedClinicalGroupCol: datasetToUpload.clinicalGroupCol,
      submittedClinicalBatchCol: datasetToUpload.clinicalBatchCol,
      submittedIsNormalized: ds.isNormalized,
      submittedDataType: ds.dataType,
      submittedPlatform: ds.platform,
      submittedGeneIdCol: ds.geneIdCol,
      submittedGeneIdType: ds.geneIdType,
      clinicalFile: datasetToUpload.clinicalFile,
      clinicalFileName: datasetToUpload.clinicalFileName,
      clinicalSampleIdCol: datasetToUpload.clinicalSampleIdCol,
      clinicalGroupCol: datasetToUpload.clinicalGroupCol,
      clinicalBatchCol: datasetToUpload.clinicalBatchCol,
      clinicalParsedData: datasetToUpload.clinicalParsedData,
      clinicalColumns: finalClinicalColumns,
      sampleOverlapChoice: clearedClinical ? null : ds.sampleOverlapChoice,
      matchingSamples: finalMatchingSamples,
      missingClinSamples: finalMissingClinSamples,
      clinicalNoExprSamples: finalClinicalNoExprSamples,
      hasMissingClinicalValues: hasMissingClinicalValues,
      removedSamples: removedSamples,
    });

    const sorted = [...state.dpDatasets].sort((a, b) => a.id.localeCompare(b.id));
    const idx = sorted.findIndex(d => d.id === datasetId);

    if (state.dpDatasets.length > 1) {
      if (idx < sorted.length - 1) {
        const nextDs = sorted[idx + 1];
        dispatch({ type: "DP_SELECT_DATASET", id: nextDs.id });
        dispatch({ type: "DP_SET_CONTEXT", id: nextDs.id });
        dispatch({ type: "DP_SET_STEP", step: "upload" });
      } else {
        dispatch({ type: "DP_SET_STEP", step: "all-datasets" });
        dispatch({ type: "DP_SET_CONTEXT", id: "all" });
      }
    } else {
      const skipNorm = ds.isNormalized === true;
      const isMicroarray = ds.dataType === "microarray";
      const isOthers = ds.dataType === "others";
      const isProteomics = ds.dataType === "proteomics";

      if (isMicroarray) {
        if (!skipNorm) {
          dispatch({ type: "DP_SET_STEP", step: "normalization" });
        } else {
          dispatch({ type: "DP_SET_STEP", step: "annotation" });
        }
      } else if (isOthers || isProteomics) {
        // Others and Proteomics: Upload > Processing > Normalization > Batch
        dispatch({ type: "DP_SET_STEP", step: "processing" });
      } else {
        // Readcounts: Upload > Annotation > Processing > Normalization > Batch
        dispatch({ type: "DP_SET_STEP", step: "annotation" });
      }
    }
  };

  const onContinueClick = () => {
    if (isNoOverlap) {
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

  // Define your matching/mismatch flags near your other mismatch variables
  const isMicroarray = ds.dataType === "microarray";
  const isOthers = ds.dataType === "others";
  const isProteomics = ds.dataType === "proteomics";
  const isOthersOrProteomics = isOthers || isProteomics;
  const normMismatch = ds.expressionFileName && (
    isMicroarray
      ? !ds.detectedIsNormalized // Warns if microarray data looks like integers (not normalized/floats)
      : ds.isNormalized !== ds.detectedIsNormalized // Original read counts mismatch logic
  );
  const geneInfoColOptions = ds.columns;
  const isPlatformProbeId = ds.geneIdType && !["ensembl", "entrez", "genename"].includes(ds.geneIdType);
  const idTypeMismatch = ds.geneIdType && ds.detectedGeneIdType && ds.geneIdType !== ds.detectedGeneIdType && !isPlatformProbeId;


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

      {/* Expression Data */}
      <div className="card">
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">Expression Data{multipleDs ? ` for ${ds.name}` : ""}</div>
            <div className="card-sub">Upload your expression matrix (CSV, TSV, or TXT). Rows = features, Columns = samples.</div>
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
            data-testid="dropzone-expression"
          >
            <div className="drop-zone-icon">📊</div>
            <div className="drop-zone-title">Drop expression file here</div>
            <div className="drop-zone-hint">CSV · TSV · TXT &nbsp;·&nbsp; Max 500 MB</div>
          </div>
        ) : (
          <>
            <div className="file-chip" style={{ marginTop: 14 }}>
              <FileText size={15} />
              <span className="file-chip-name">{ds.expressionFileName}</span>
              <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>
                {ds.nFeatures.toLocaleString()} features · {ds.nSamples} samples
              </span>
              <button className="file-chip-clear" onClick={() => handleClear("expression")} data-testid="btn-clear-expression">
                <X size={16} />
              </button>
            </div>

            <hr className="card-divider" />

            {/* Normalization / Data Type Handling */}
            <div style={{ marginBottom: 14 }}>
              {!isMicroarray ? (
                <>
                  {/* --- READ COUNTS / OTHERS / PROTEOMICS VIEW --- */}
                  <label style={{ fontSize: 13, fontWeight: 500, color: "var(--foreground)", display: "block", marginBottom: 8 }}>
                    Is the data already normalized?
                    <span style={{
                      marginLeft: 10, fontSize: 11, padding: "2px 8px", borderRadius: 4,
                      background: "var(--selected-bg)", color: "var(--foreground)",
                      border: "1px solid var(--border)", fontWeight: 500,
                    }}>
                      Auto-detected: {ds.detectedIsNormalized ? (isOthersOrProteomics ? "Normalized Data" : "Already normalized") : (isOthersOrProteomics ? "Raw Data" : "Raw counts / unnormalized")}
                    </span>
                  </label>
                  <div className="radio-group">
                    {[
                      { val: false, label: isOthersOrProteomics ? "Raw Data" : "Raw counts / unnormalized", hint: isOthersOrProteomics ? "Data will be normalized using VSN or Quantile normalization in a later step" : "Normalization will be applied in a later step" },
                      { val: true, label: isOthersOrProteomics ? "Normalized Data" : "Already normalized", hint: isOthersOrProteomics ? "Normalization step will be skipped" : "Normalization step will be skipped" },
                    ].map(opt => (
                      <label key={String(opt.val)} className={`radio-option ${ds.isNormalized === opt.val ? "selected" : ""}`}>
                        <input type="radio" checked={ds.isNormalized === opt.val}
                          onChange={() => updateDs({ isNormalized: opt.val })}
                          data-testid={`radio-normalized-${opt.val}`} />
                        <div>
                          <div style={{ fontWeight: 500 }}>{opt.label}</div>
                          <div style={{ fontSize: 11, color: "hsl(220 9% 52%)" }}>{opt.hint}</div>
                        </div>
                      </label>
                    ))}
                  </div>

                  {normMismatch && (
                    <div className="banner warn" style={{ marginTop: 10 }}>
                      <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                      <span>
                        <strong>Normalization mismatch:</strong> You selected <strong>"{ds.isNormalized ? (isOthersOrProteomics ? "Normalized Data" : "Already normalized") : (isOthersOrProteomics ? "Raw Data" : "Raw counts")}"</strong>, but the file pattern suggests <strong>"{ds.detectedIsNormalized ? (isOthersOrProteomics ? "Normalized Data" : "Already normalized") : (isOthersOrProteomics ? "Raw Data" : "Raw counts")}"</strong>. Please verify before continuing.
                      </span>
                    </div>
                  )}
                </>
              ) : (
                <>
                  {/* --- MICROARRAY PROBE INTENSITIES VIEW --- */}
                  <label style={{ fontSize: 13, fontWeight: 500, color: "hsl(220 15% 25%)", display: "block", marginBottom: 8 }}>
                    Microarray Normalization Status
                    <span style={{
                      marginLeft: 10, fontSize: 11, padding: "2px 8px", borderRadius: 4,
                      background: ds.detectedIsNormalized ? "hsl(140 40% 96%)" : "hsl(32 90% 96%)",
                      color: ds.detectedIsNormalized ? "hsl(140 60% 25%)" : "hsl(32 90% 25%)",
                      border: ds.detectedIsNormalized ? "1px solid hsl(140 30% 82%)" : "1px solid hsl(32 60% 80%)",
                      fontWeight: 500,
                    }}>
                      File Pattern: {ds.detectedIsNormalized ? "Continuous/Float Data" : "Integer/Counts Data"}
                    </span>
                  </label>

                  <div className="radio-group">
                    {[
                      { val: false, label: "Raw intensities", hint: "Data will be treated as raw continuous values" },
                      { val: true, label: "Normalized intensities", hint: "Standard downstream analysis will be applied" },
                    ].filter(opt => {
                      if (ds.platform === "affymetrix") {
                        return opt.val === true;
                      }
                      return true;
                    }).map(opt => (
                      <label key={String(opt.val)} className={`radio-option ${ds.isNormalized === opt.val ? "selected" : ""}`}>
                        <input type="radio" checked={ds.isNormalized === opt.val}
                          onChange={() => updateDs({ isNormalized: opt.val })}
                          data-testid={`radio-microarray-${opt.val}`} />
                        <div>
                          <div style={{ fontWeight: 500 }}>{opt.label}</div>
                          <div style={{ fontSize: 11, color: "hsl(220 9% 52%)" }}>{opt.hint}</div>
                        </div>
                      </label>
                    ))}
                  </div>

                  {normMismatch && (
                    <div className="banner warn" style={{ marginTop: 10 }}>
                      <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                      <span>
                        <strong>Warning:</strong> You selected <strong>"Microarray Probe intensities"</strong>, but the file pattern suggests <strong>"Read counts"</strong> (integer structure detected). Please verify before continuing.
                      </span>
                    </div>
                  )}
                </>
              )}
            </div>

            {/* Dataset Name Input */}
            <div style={{ display: "flex", flexDirection: "column", width: 280, marginBottom: 14 }}>
              <label style={{ fontSize: 13, fontWeight: 500, marginBottom: 6 }}>Dataset Name</label>
              <input 
                type="text" 
                value={ds.name} 
                onChange={e => updateDs({ name: e.target.value })}
                style={{ width: "100%", padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
                data-testid="input-dataset-name"
              />
            </div>

            {/* Data Type and Platform side-by-side arrangement */}
            <div style={{ display: "flex", gap: 14, flexWrap: "wrap", marginBottom: 14 }}>
              
              {/* Data Type Dropdown */}
              <div style={{ display: "flex", flexDirection: "column", width: 280, position: "relative" }}>
                <label style={{ fontSize: 13, fontWeight: 500, color: "var(--foreground)", marginBottom: 8 }}>Data Type</label>
                <details style={{ width: "100%" }} data-testid="details-data-type">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>
                      {ds.dataType === "readcounts" && "Read counts"}
                      {ds.dataType === "microarray" && "Microarray probe intensities"}
                      {ds.dataType === "proteomics" && "Proteomics"}
                      {ds.dataType === "others" && "Others (eg. metabolomics, lipidomics)"}
                      {!ds.dataType && "Select data type..."}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {[
                      { value: "readcounts", label: "Read counts" },
                      { value: "microarray", label: "Microarray probe intensities" },
                      { value: "proteomics", label: "Proteomics" },
                      { value: "others", label: "Others (eg. metabolomics, lipidomics)" }
                    ].map(opt => (
                      <div
                        key={opt.value}
                        onClick={(e) => {
                          const newType = opt.value as DataType;
                          const patch: Partial<typeof ds> = { dataType: newType };
                          if (newType === "readcounts") {
                            patch.geneIdType = "ensembl";
                            patch.platform = "";
                          } else if (newType === "microarray") {
                            const platformFamily = ds.platform || "affymetrix";
                            const entries = platformFamily === "affymetrix" ? platforms.affymetrix : platforms.illumina;
                            const firstEntry = entries && entries.length > 0 ? entries[0] : null;
                            patch.platform = platformFamily;
                            patch.geneIdType = firstEntry ? firstEntry.platform : "";
                            patch.microarrayPlatformId = firstEntry ? firstEntry.platform : "";
                            patch.microarrayOrganism = firstEntry ? firstEntry.organism_display : "";
                            if (platformFamily === "affymetrix") {
                              patch.isNormalized = true;
                            }
                          } else {
                            patch.geneIdType = "";
                            patch.platform = "";
                          }
                          updateDs(patch);
                          
                          const details = (e.target as HTMLElement).closest("details");
                          if (details) details.removeAttribute("open");
                        }}
                        style={{
                          padding: "8px 10px",
                          fontSize: 12,
                          cursor: "pointer",
                          background: ds.dataType === opt.value ? "var(--selected-bg)" : "transparent",
                          fontWeight: ds.dataType === opt.value ? 600 : 400
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                        onMouseLeave={e => e.currentTarget.style.background = ds.dataType === opt.value ? "var(--selected-bg)" : "transparent"}
                      >
                        {opt.label}
                      </div>
                    ))}
                  </div>
                </details>
              </div>

              {/* Platform Dropdown */}
              {isMicroarray && (
                <div style={{ display: "flex", flexDirection: "column", width: 240, position: "relative" }}>
                  <label style={{ fontSize: 13, fontWeight: 500, color: "var(--foreground)", marginBottom: 8 }}>Platform</label>
                  <details style={{ width: "100%" }} data-testid="details-platform">
                    <summary style={{
                      display: "flex", justifyContent: "space-between", alignItems: "center",
                      padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                      background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                    }}>
                      <span>
                        {ds.platform === "affymetrix" && "Affymetrix"}
                        {ds.platform === "illumina" && "Illumina"}
                        {!ds.platform && "Select platform..."}
                      </span>
                      <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                    </summary>

                    <div style={{
                      position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                      border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                      overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                    }}>
                      {[
                        { value: "affymetrix", label: "Affymetrix" },
                        { value: "illumina", label: "Illumina" }
                      ].map(opt => (
                        <div
                          key={opt.value}
                          onClick={(e) => {
                            const newPlatform = opt.value as Platform;
                            const entries = newPlatform === "affymetrix" ? platforms.affymetrix : platforms.illumina;
                            const firstEntry = entries && entries.length > 0 ? entries[0] : null;
                            const patch: Partial<typeof ds> = {
                              platform: newPlatform,
                              geneIdType: firstEntry ? firstEntry.platform : "",
                              microarrayPlatformId: firstEntry ? firstEntry.platform : "",
                              microarrayOrganism: firstEntry ? firstEntry.organism_display : ""
                            };
                            if (ds.dataType === "microarray" && newPlatform === "affymetrix") {
                              patch.isNormalized = true;
                            }
                            updateDs(patch);
                            
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.platform === opt.value ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.platform === opt.value ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.platform === opt.value ? "var(--selected-bg)" : "transparent"}
                        >
                          {opt.label}
                        </div>
                      ))}
                    </div>
                  </details>
                </div>
              )}
            </div>

            <hr className="card-divider" />

            {/* Data Summary */}
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 10, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Data Summary
            </div>
            <div className="chips-row" style={{ marginBottom: 16 }}>
              <div className="stat-chip"><div className="stat-chip-val">{ds.nSamples}</div><div className="stat-chip-lbl">Samples</div></div>
              <div className="stat-chip"><div className="stat-chip-val">{ds.nFeatures.toLocaleString()}</div><div className="stat-chip-lbl">Features</div></div>
            </div>

            {/* Column Configuration */}
            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 12, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Column Configuration
            </div>
            <div style={{ display: "flex", gap: 14, flexWrap: "wrap", marginBottom: 14 }}>

              {/* Gene Info Columns Dropdown */}
              <div style={{ display: "flex", flexDirection: "column", minWidth: 200, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Gene Info Columns</label>
                <details style={{ width: "100%" }}>
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>
                      {(() => {
                        if (ds.geneInfoCols.length === 0) return "";
                        if (ds.geneInfoCols.length === 1) return ds.geneInfoCols[0];
                        return `${ds.geneInfoCols.length} selected`;
                      })()}
                    </span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "8px 10px", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {geneInfoColOptions.map(col => (
                      <label key={col} style={{ display: "flex", alignItems: "center", gap: 7, fontSize: 12, cursor: "pointer", padding: "4px 0" }}>
                        <input type="checkbox" checked={ds.geneInfoCols.includes(col)}
                          onChange={e => {
                            const next = e.target.checked ? [...ds.geneInfoCols, col] : ds.geneInfoCols.filter(c => c !== col);
                            const nextIdCol = next.includes(ds.geneIdCol) ? ds.geneIdCol : "";
                            const nextExprSamples = ds.columns.filter(c => c !== nextIdCol && !next.includes(c));
                            const patch: any = { geneInfoCols: next, geneIdCol: nextIdCol, nSamples: nextExprSamples.length };
                            if (!nextIdCol) {
                              patch.geneIdType = "";
                              patch.detectedGeneIdType = "";
                            }
                            updateDs(patch);
                          }}
                          style={{ accentColor: "var(--primary)" }}
                          data-testid={`check-geneinfo-${col}`} />
                        {col}
                      </label>
                    ))}
                  </div>
                </details>
               </div>

              <div style={{ display: "flex", flexDirection: "column", minWidth: 180, position: "relative" }}>
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Gene ID Column</label>
                <details style={{ width: "100%" }} data-testid="details-gene-id-col">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{ds.geneIdCol || "Select column..."}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {ds.geneInfoCols.length === 0 ? (
                      <div style={{ padding: "8px 10px", fontSize: 12, color: "var(--muted-foreground)" }}>
                        Please select Gene Info Columns first
                      </div>
                    ) : (
                      ds.geneInfoCols.map(col => (
                        <div
                          key={col}
                          onClick={(e) => {
                            const nextCol = col;
                            const colIdx = ds.columns.indexOf(nextCol);
                            const sampleVal = ds.parsedData?.[0]?.[colIdx] || "";
                            const detectedId = detectGeneIdType(sampleVal);
                            const isDetectedProbe = detectedId && !["ensembl", "entrez", "genename"].includes(detectedId);
                            const allEntries = [...platforms.affymetrix, ...platforms.illumina];
                            const entry = isDetectedProbe ? allEntries.find(e => e.platform === detectedId) : null;
                            updateDs({
                              geneIdCol: nextCol,
                              detectedGeneIdType: detectedId,
                              geneIdType: detectedId,
                              microarrayPlatformId: entry ? entry.platform : "",
                              microarrayOrganism: entry ? entry.organism_display : ""
                            });
                            const details = (e.target as HTMLElement).closest("details");
                            if (details) details.removeAttribute("open");
                          }}
                          style={{
                            padding: "8px 10px",
                            fontSize: 12,
                            cursor: "pointer",
                            background: ds.geneIdCol === col ? "var(--selected-bg)" : "transparent",
                            fontWeight: ds.geneIdCol === col ? 600 : 400
                          }}
                          onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                          onMouseLeave={e => e.currentTarget.style.background = ds.geneIdCol === col ? "var(--selected-bg)" : "transparent"}
                        >
                          {col}
                        </div>
                      ))
                    )}
                  </div>
                </details>
              </div>
            </div>

            {/* Gene ID Type */}
            {showGeneIdType && ds.geneIdCol && ds.dataType !== "others" && ds.dataType !== "proteomics" && (
              <div style={{ marginBottom: 14, width: 400 }}>
                <label style={{ fontSize: 12, fontWeight: 500, display: "block", marginBottom: 6 }}>
                  Gene ID Type
                  {ds.detectedGeneIdType && (
                    <span style={{
                      marginLeft: 10, fontSize: 11, fontWeight: 500, padding: "2px 8px", borderRadius: 4,
                      background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--border)",
                    }}>
                      Auto-detected: {GENE_ID_LABELS[ds.detectedGeneIdType] ?? ds.detectedGeneIdType}
                    </span>
                  )}
                </label>
                <details style={{ width: 250, position: "relative" }} data-testid="details-gene-id-type">
                  <summary style={{
                    display: "flex", justifyContent: "space-between", alignItems: "center",
                    padding: "8px 10px", border: `1px solid ${idTypeMismatch ? "var(--destructive)" : "var(--border)"}`, borderRadius: 7, fontSize: 13,
                    background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                  }}>
                    <span>{getGeneIdTypeLabel(ds.geneIdType) || "Select type..."}</span>
                    <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                  </summary>

                  <div style={{
                    position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                    border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                    overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                  }}>
                    {geneIdTypeOptions.map(opt => (
                      <div
                        key={opt.value}
                        onClick={(e) => {
                          const val = opt.value;
                          const allEntries = [...platforms.affymetrix, ...platforms.illumina];
                          const entry = allEntries.find(e => e.platform === val);
                          if (entry) {
                            updateDs({
                              geneIdType: val as any,
                              microarrayPlatformId: val,
                              microarrayOrganism: entry.organism_display
                            });
                          } else {
                            updateDs({
                              geneIdType: val as any,
                              microarrayPlatformId: "",
                              microarrayOrganism: ""
                            });
                          }
                          const details = (e.target as HTMLElement).closest("details");
                          if (details) details.removeAttribute("open");
                        }}
                        style={{
                          padding: "8px 10px",
                          fontSize: 12,
                          cursor: "pointer",
                          background: ds.geneIdType === opt.value ? "var(--selected-bg)" : "transparent",
                          fontWeight: ds.geneIdType === opt.value ? 600 : 400
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                        onMouseLeave={e => e.currentTarget.style.background = ds.geneIdType === opt.value ? "var(--selected-bg)" : "transparent"}
                      >
                        {opt.label}
                      </div>
                    ))}
                  </div>
                </details>
                {idTypeMismatch && (
                  <div className="banner warn" style={{ marginTop: 10, width: 700 }}>
                    <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                    <span>
                      <strong>ID type mismatch:</strong> Auto-detector suggests <strong>{GENE_ID_LABELS[ds.detectedGeneIdType] || ds.detectedGeneIdType}</strong>{" "}
                      (e.g. "{ds.parsedData?.[0]?.[0] || ""}"), but you selected <strong>{getGeneIdTypeLabel(ds.geneIdType)}</strong>. Please verify.
                    </span>
                  </div>
                )}
                {!idTypeMismatch && ds.geneIdType && ds.detectedGeneIdType && (
                  <div className="banner success" style={{ marginTop: 10 }}>
                    <CheckCircle size={15} style={{ flexShrink: 0 }} />
                    ID type matches auto-detection ({GENE_ID_LABELS[ds.detectedGeneIdType] || ds.detectedGeneIdType}).
                  </div>
                )}
                {ds.dataType === "microarray" && ds.microarrayOrganism && isPlatformProbeId && (
                  <div className="banner success" style={{ marginTop: 10, width: 700, display: "flex", gap: 8, alignItems: "center" }}>
                    <CheckCircle size={15} style={{ flexShrink: 0 }} />
                    <span>
                      Platform organism detected: <strong>{ds.microarrayOrganism}</strong>. This organism will be used for downstream annotation.
                    </span>
                  </div>
                )}
              </div>
            )}

            {/* Preview table */}
            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Preview (first 10 rows)
            </div>
            <div className="preview-wrap">
              <table>
                <thead><tr>{(ds.columns || []).map(col => <th key={col}>{col}</th>)}</tr></thead>
                <tbody>{(ds.parsedData || []).slice(0, 10).map((row, i) => <tr key={i}>{row.map((cell, j) => <td key={j}>{cell}</td>)}</tr>)}</tbody>
              </table>
            </div>
          </>
        )}
      </div>

      {/* Clinical Data Upload */}
      <div className="card">
        <div className="card-header" style={{ marginBottom: 0 }}>
          <div>
            <div className="card-title">
              Clinical / Phenotype Data{multipleDs ? ` for ${ds.name}` : ""}
              <span style={{
                marginLeft: 8,
                fontSize: 11,
                background: "var(--muted)",
                color: "var(--muted-foreground)",
                border: "1px solid var(--border)",
                padding: "2px 7px",
                borderRadius: 4,
                fontWeight: 500,
              }}>
                Optional
              </span>
            </div>
            <div className="card-sub">Upload sample metadata with condition/group assignments for analysis.</div>
          </div>
          <div style={{ display: "flex", gap: 10 }}>
            <button className="link-button danger" onClick={() => handleClear("clinical")}>Clear</button>
            <button className="link-button primary" onClick={() => handleLoadSample("clinical")}>Example Data</button>
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
                </>
              ) : (
                <>
                  <span className="file-chip-name">{ds.clinicalFileName}</span>
                  <span style={{ fontSize: 12, color: "hsl(220 9% 55%)" }}>
                    ({matchingSamples.length} samples matched)
                  </span>
                </>
              )}
              <button className="file-chip-clear" onClick={() => handleClear("clinical")} data-testid="btn-clear-clinical"><X size={16} /></button>
            </div>

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
                <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Group Column (Optional):</label>
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
                          updateDs({ clinicalGroupCol: c });
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
            </div>

            <hr className="card-divider" />
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              Clinical Data Preview (first 10 rows)
            </div>
            <div className="preview-wrap">
              <table>
                <thead>
                  <tr>
                    {(ds.clinicalColumns || []).map(col => (
                      <th key={col}>{col}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {(ds.clinicalParsedData || []).slice(0, 10).map((row, i) => (
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
            </div>
          </>
        )}
      </div>

      {/* Integrity check */}
      {ds.expressionFileName && (
        <div className="card">
          <div className="card-title" style={{ marginBottom: 12 }}>Data Integrity Check</div>
          {hasClinical ? (
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

                {clinicalNoExprSamples.length > 0 && (
                  <div style={{ marginBottom: 12, display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
                    <span style={{ fontSize: 12, color: "var(--muted-foreground)" }}>Download clinical samples without expression:</span>
                    <button type="button" className="btn btn-sm btn-default" onClick={() => {
                      const content = "SampleID\n" + clinicalNoExprSamples.join("\n");
                      const blob = new Blob([content], { type: "text/csv;charset=utf-8;" });
                      const url = URL.createObjectURL(blob);
                      const link = document.createElement("a");
                      link.href = url;
                      link.setAttribute("download", `${ds.name || "dataset"}_clinical_samples_without_expression.csv`);
                      document.body.appendChild(link);
                      link.click();
                      document.body.removeChild(link);
                    }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                      <Download size={11} /> CSV
                    </button>
                    <button type="button" className="btn btn-sm btn-default" onClick={() => {
                      const content = "SampleID\n" + clinicalNoExprSamples.join("\n");
                      const blob = new Blob([content], { type: "text/tab-separated-values;charset=utf-8;" });
                      const url = URL.createObjectURL(blob);
                      const link = document.createElement("a");
                      link.href = url;
                      link.setAttribute("download", `${ds.name || "dataset"}_clinical_samples_without_expression.tsv`);
                      document.body.appendChild(link);
                      link.click();
                      document.body.removeChild(link);
                    }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                      <Download size={11} /> TSV
                    </button>
                    <button type="button" className="btn btn-sm btn-default" onClick={() => {
                      const content = `
                        <html xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns="http://www.w3.org/TR/REC-html40">
                        <head><!--[if gte mso 9]><xml><x:ExcelWorkbook><x:ExcelWorksheets><x:ExcelWorksheet><x:Name>Clinical w-o Expression</x:Name><x:WorksheetOptions><x:DisplayGridlines/></x:WorksheetOptions></x:ExcelWorksheet></x:ExcelWorksheets></x:ExcelWorkbook></xml><![endif]--></head>
                        <body>
                          <table>
                            <thead><tr><th>SampleID</th></tr></thead>
                            <tbody>
                              ${clinicalNoExprSamples.map(s => `<tr><td>${s}</td></tr>`).join("")}
                            </tbody>
                          </table>
                        </body>
                        </html>
                      `;
                      const blob = new Blob([content], { type: "application/vnd.ms-excel;charset=utf-8;" });
                      const url = URL.createObjectURL(blob);
                      const link = document.createElement("a");
                      link.href = url;
                      link.setAttribute("download", `${ds.name || "dataset"}_clinical_samples_without_expression.xlsx`);
                      document.body.appendChild(link);
                      link.click();
                      document.body.removeChild(link);
                    }} style={{ gap: 4, display: "flex", alignItems: "center", fontSize: 11, padding: "4px 8px" }}>
                      <Download size={11} /> XLSX
                    </button>
                  </div>
                )}

                {/* Overlap Choice Options */}
                {showOverlapOptions && (
                  <div style={{ marginTop: 16, marginBottom: 16, padding: 12, border: "1px solid var(--border)", borderRadius: 8, background: "var(--background)" }}>
                    <label style={{ fontSize: 13, fontWeight: 600, display: "block", marginBottom: 8 }}>
                      How would you like to handle mismatched samples? <span style={{ color: "var(--destructive)" }}>*</span>
                    </label>
                    <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                      <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 13, cursor: "pointer" }}>
                        <input
                          type="radio"
                          name="sampleOverlapChoice"
                          value="all"
                          checked={ds.sampleOverlapChoice === "all"}
                          onChange={() => {
                            updateDs({
                              clinicalFile: null,
                              clinicalFileName: "",
                              clinicalSampleIdCol: "",
                              clinicalGroupCol: "",
                              clinicalBatchCol: "",
                              clinicalColumns: [],
                              clinicalParsedData: [],
                              sampleOverlapChoice: null,
                            });
                          }}
                        />
                        Keep all samples in expression data
                      </label>
                      <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 13, cursor: "pointer" }}>
                        <input
                          type="radio"
                          name="sampleOverlapChoice"
                          value="overlap"
                          checked={ds.sampleOverlapChoice === "overlap"}
                          onChange={() => updateDs({ sampleOverlapChoice: "overlap" })}
                        />
                        Keep only samples with both expression and clinical data
                      </label>
                    </div>
                  </div>
                )}
              </div>

              {/* Banners */}
              <div style={{ marginTop: 12 }}>
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
                      Danger: Missing values identified in the Sample ID Column, Batch Column, or Group Column.
                      Those samples with missing values will be removed: {removedSamples.join(", ")}
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
                {isPartialMatch && ds.sampleOverlapChoice !== "all" && (
                  <div className="banner warn" style={{ marginBottom: 10 }}>
                    <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                    <span>Warning: Expression and clinical data samples are partially matched.</span>
                  </div>
                )}
                {isPartialMatch && ds.sampleOverlapChoice === "all" && (
                  <div className="banner warn" style={{ marginBottom: 10 }}>
                    <AlertTriangle size={15} style={{ flexShrink: 0 }} />
                    <span>Warning: Clinical data won't be stored and used because some samples are missing clinical info.</span>
                  </div>
                )}
                {isFullMatch && !hasDanger && (
                  <div className="banner success" style={{ marginBottom: 10 }}>
                    <CheckCircle size={15} style={{ flexShrink: 0 }} />
                    <span>Success: All samples match perfectly between expression and clinical data.</span>
                  </div>
                )}
              </div>
            </>
          ) : !hasDanger && ds.integrityOk ? (
            <div className="banner success">
              <CheckCircle size={15} />
              All checks passed. Dataset is ready to proceed.
            </div>
          ) : (
            <div className="banner danger">
              <AlertTriangle size={15} style={{ flexShrink: 0 }} />
              <div>
                <strong>Issues found:</strong>
                <ul style={{ marginTop: 4, paddingLeft: 18 }}>
                  {ds.integrityIssues.map((issue, i) => <li key={i}>{issue}</li>)}
                </ul>
              </div>
            </div>
          )}
        </div>
      )}

      <div className="action-row" style={{ display: "flex", gap: 8, justifyContent: "flex-end", marginTop: 16 }}>
        {(() => {
          const sorted = [...state.dpDatasets].sort((a, b) => a.id.localeCompare(b.id));
          const idx = sorted.findIndex(d => d.id === datasetId);
          if (state.dpDatasets.length > 1 && idx > 0) {
            return (
              <button
                type="button"
                className="btn btn-default"
                onClick={() => {
                  const prevDs = sorted[idx - 1];
                  dispatch({ type: "DP_SELECT_DATASET", id: prevDs.id });
                  dispatch({ type: "DP_SET_CONTEXT", id: prevDs.id });
                  dispatch({ type: "DP_SET_STEP", step: "upload" });
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

        <button
          className="btn btn-primary"
          disabled={!canProceed}
          onClick={onContinueClick}
          data-testid="btn-next-upload"
        >
          {state.dpDatasets.length > 1 && (() => {
            const sorted = [...state.dpDatasets].sort((a, b) => a.id.localeCompare(b.id));
            const idx = sorted.findIndex(d => d.id === datasetId);
            return idx < sorted.length - 1 ? "Next Dataset →" : "Continue to Upload Summary →";
          })()}
          {state.dpDatasets.length <= 1 && (() => {
            const dt = ds.dataType;
            if (dt === "microarray") {
              return ds.isNormalized ? "Continue to Annotation →" : "Continue to Normalization →";
            } else if (dt === "readcounts") {
              return "Continue to Annotation →";
            } else {
              // proteomics and others: Upload > Processing > Normalization...
              return "Continue to Processing →";
            }
          })()}
        </button>
      </div>

      {showWarnModal && (
        <WarnSampleModal
          type="no_matching"
          onConfirm={handleConfirmWarn}
          onCancel={() => setShowWarnModal(false)}
        />
      )}
    </>
  );
}