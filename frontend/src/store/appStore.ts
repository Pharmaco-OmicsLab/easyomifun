import { createContext, useContext } from "react";
import type {
  DatasetConfig, ProcessingConfig, NormalizationConfig, BatchConfig,
  AnnotationConfig, DEConfig, DEResult, DPStep, DEStep, FSStep, EnrichmentStep,
  SelectedModule, MetaMethod, FSConfig, EnrichmentConfig, 
} from "../dataObject";
import { DATASET_COLORS, computeDPSteps } from "../dataObject";
import { getExpressionSampleColumns } from "../lib/dataParser";

export interface AppState {
  // Legacy / Pipeline Mode (compatibility)
  pipelineMode: boolean;

  // Data Processing
  dpDatasets: DatasetConfig[];
  dpCurrentDatasetId: string;
  dpStep: DPStep;
  dpProcessingConfig: ProcessingConfig;
  dpNormConfig: NormalizationConfig;
  dpBatchConfig: BatchConfig;
  dpAnnotationConfig: AnnotationConfig;
  dpSelectedContext: string;
  dpSelectedModule: SelectedModule;
  dpPanelOpen: boolean;
  inlineDeUploadReviewed: boolean;
  inlineFsUploadReviewed: boolean;
  inlineDpUploadReviewed: boolean;
  dpVisitedSteps: string[];
  deVisitedSteps: string[];
  fsVisitedSteps: string[];
  enVisitedSteps: string[];
  deMetaSkipped: boolean;
  dpDeMetaSkipped: boolean;
  dpInlineDeDone: boolean;
  dpInlineMetaDone: boolean;
  dpInlineFsDone: boolean;
  dpInlineEaDone: boolean;
  dpDeResults: Record<string, any>;
  dpDeMetaMethod: MetaMethod | null;
  dpDeMetaVotes: number;

  // DE Analysis
  deDatasets: DatasetConfig[];
  deCurrentDatasetId: string;
  deStep: DEStep;
  deConfig: DEConfig;
  deResults: Record<string, any>;
  deMetaMethod: MetaMethod | null;
  deMetaVotes: number;
  deSelectedContext: string;
  deSelectedModule: SelectedModule;
  deInlineEaDone: boolean;
  dePanelOpen: boolean;

  // Feature Selection
  fsDatasets: DatasetConfig[];
  fsCurrentDatasetId: string;
  fsStep: FSStep;
  fsConfig: FSConfig;
  fsSelectedContext: string;
  fsPanelOpen: boolean;
  standaloneFsDone: boolean;

  // Enrichment Analysis
  enDatasets: DatasetConfig[];
  enCurrentDatasetId: string;
  enStep: EnrichmentStep;
  enConfig: EnrichmentConfig;
  enSelectedContext: string;
  enPanelOpen: boolean;
  standaloneEnDone: boolean;

  // Submitted Configs
  dpSubmittedAnnotationConfig: AnnotationConfig | null;
  dpSubmittedProcessingConfig: ProcessingConfig | null;
  dpSubmittedNormConfig: NormalizationConfig | null;
  dpSubmittedBatchConfig: BatchConfig | null;
  deSubmittedConfig: DEConfig | null;
  fsSubmittedConfig: FSConfig | null;
  enSubmittedConfig: EnrichmentConfig | null;

  // ML Results Caches
  fsTrainingResultsCache: any | null;
  fsCvResultsCache: any | null;
  fsTestingResultsCache: any | null;
  dpTrainingResultsCache: any | null;
  dpCvResultsCache: any | null;
  dpTestingResultsCache: any | null;
  enResultsCache: Record<string, { oraResults: any[]; gseaResults: any[] }>;
  dpInlineEnResultsCache: Record<string, { oraResults: any[]; gseaResults: any[] }>;
  deInlineEnResultsCache: Record<string, { oraResults: any[]; gseaResults: any[] }>;
}

export type AppAction =
  // Pipeline / Downstream
  | { type: "RESET_DOWNSTREAM_STEPS"; datasetId: string; fromStep: "upload" | "annotation" | "processing" | "normalization" | "normalization-counts" | "normalization-others"
     | "batch" | "de-analysis" | "meta-analysis" | "model-selection" | "cross-validation" | "enrichment-upload" | "inline-de" | "inline-de-meta" }
  | { type: "DP_SUBMIT_ANNOTATION" }
  | { type: "DP_SUBMIT_PROCESSING" }
  | { type: "DP_SUBMIT_NORM" }
  | { type: "DP_SUBMIT_BATCH" }
  | { type: "DE_SUBMIT_CONFIG" }
  | { type: "FS_SUBMIT_CONFIG" }
  | { type: "EN_SUBMIT_CONFIG" }
  // Store
  | { type: "RESET_STORE"; pipelineMode?: boolean }
  | { type: "SET_PIPELINE_MODE"; mode: boolean }
  | { type: "SET_DP_INLINE_DE_DONE"; done: boolean }
  | { type: "SET_DP_INLINE_FS_DONE"; done: boolean }
  | { type: "SET_DP_INLINE_EA_DONE"; done: boolean }
  | { type: "SET_DP_INLINE_META_DONE"; done: boolean }
  | { type: "DP_INLINE_EN_SET_RESULTS"; id: string; results: { oraResults: any[]; gseaResults: any[] } }
  | { type: "SET_DE_INLINE_EA_DONE"; done: boolean }
  | { type: "DE_INLINE_EN_SET_RESULTS"; id: string; results: { oraResults: any[]; gseaResults: any[] } }
  | { type: "DE_SET_MODULE"; module: SelectedModule }
  | { type: "SET_STANDALONE_FS_DONE"; done: boolean }
  | { type: "SET_STANDALONE_EN_DONE"; done: boolean }
  | { type: "EN_SET_RESULTS"; id: string; results: { oraResults: any[]; gseaResults: any[] } }
  | { type: "PREPARE_DE_FROM_DP" }
  | { type: "PREPARE_FS_FROM_DP" }
  | { type: "PREPARE_EN_FROM_DE" }
  | { type: "REDO_NORMALIZATION" }
  | { type: "SET_INLINE_DE_UPLOAD_REVIEWED"; reviewed: boolean }
  | { type: "SET_INLINE_FS_UPLOAD_REVIEWED"; reviewed: boolean }
  | { type: "SET_INLINE_DP_UPLOAD_REVIEWED"; reviewed: boolean }
  | { type: "SET_DE_META_SKIPPED"; skipped: boolean }
  | { type: "DE_SET_META_SKIPPED"; skipped: boolean }
  // DP
  | { type: "DP_ADD_DATASET" }
  | { type: "DP_REMOVE_DATASET"; id: string }
  | { type: "DP_SELECT_DATASET"; id: string }
  | { type: "DP_SET_STEP"; step: DPStep }
  | { type: "DP_SET_CONTEXT"; id: string }
  | { type: "DP_UPDATE_DATASET"; id: string; patch: Partial<DatasetConfig> }
  | { type: "DP_UPDATE_ALL_DATASETS"; patch: Partial<DatasetConfig> }
  | { type: "DP_SET_PROCESSING"; patch: Partial<ProcessingConfig> }
  | { type: "DP_SET_NORM"; patch: Partial<NormalizationConfig> }
  | { type: "DP_SET_BATCH"; patch: Partial<BatchConfig> }
  | { type: "DP_SET_ANNOTATION"; patch: Partial<AnnotationConfig> }
  | { type: "DP_SET_MODULE"; module: SelectedModule }
  | { type: "DP_TOGGLE_PANEL" }
  | { type: "DP_SET_DE_RESULTS"; id: string; results: any }
  | { type: "DP_SET_DE_META"; method: MetaMethod }
  | { type: "DP_SET_DE_META_VOTES"; n: number }
  | { type: "DP_SET_DE_META_SKIPPED"; skipped: boolean }
  // DE
  | { type: "DE_ADD_DATASET" }
  | { type: "DE_REMOVE_DATASET"; id: string }
  | { type: "DE_SELECT_DATASET"; id: string }
  | { type: "DE_SET_STEP"; step: DEStep }
  | { type: "DE_SET_CONTEXT"; id: string }
  | { type: "DE_UPDATE_DATASET"; id: string; patch: Partial<DatasetConfig> }
  | { type: "DE_SET_CONFIG"; patch: Partial<DEConfig> }
  | { type: "DE_SET_RESULTS"; id: string; results: DEResult[] }
  | { type: "DE_SET_META"; method: MetaMethod }
  | { type: "DE_SET_META_VOTES"; n: number }
  | { type: "DE_TOGGLE_PANEL" }
  // FS
  | { type: "FS_ADD_DATASET" }
  | { type: "FS_REMOVE_DATASET"; id: string }
  | { type: "FS_SELECT_DATASET"; id: string }
  | { type: "FS_SET_STEP"; step: FSStep }
  | { type: "FS_SET_CONTEXT"; id: string }
  | { type: "FS_UPDATE_DATASET"; id: string; patch: Partial<DatasetConfig> }
  | { type: "FS_SET_CONFIG"; patch: Partial<FSConfig> }
  | { type: "FS_TOGGLE_PANEL" }
  | { type: "FS_SET_TRAINING_CACHE"; results: any }
  | { type: "FS_SET_CV_CACHE"; results: any }
  | { type: "FS_SET_TESTING_CACHE"; results: any }
  | { type: "DP_SET_TRAINING_CACHE"; results: any }
  | { type: "DP_SET_CV_CACHE"; results: any }
  | { type: "DP_SET_TESTING_CACHE"; results: any }
  // EN
  | { type: "EN_ADD_DATASET" }
  | { type: "EN_REMOVE_DATASET"; id: string }
  | { type: "EN_SELECT_DATASET"; id: string }
  | { type: "EN_SET_STEP"; step: EnrichmentStep }
  | { type: "EN_SET_CONTEXT"; id: string }
  | { type: "EN_UPDATE_DATASET"; id: string; patch: Partial<DatasetConfig> }
  | { type: "EN_SET_CONFIG"; patch: Partial<EnrichmentConfig> }
  | { type: "EN_TOGGLE_PANEL" };

export function getSessionUserId(): string {
  if (typeof window !== "undefined" && window.sessionStorage) {
    let uid = window.sessionStorage.getItem("easyomifun_user_id");
    if (!uid) {
      uid = `usr_${Math.random().toString(36).substring(2, 9)}`;
      window.sessionStorage.setItem("easyomifun_user_id", uid);
    }
    return uid;
  }
  return "usr_default";
}

export function minimizeParsedData(
  parsedData: any[][] | undefined | null,
  geneIdCol?: string,
  columns?: string[]
): any[][] | undefined | null {
  if (!parsedData) return parsedData;
  const geneIdIdx = (columns && geneIdCol) ? columns.indexOf(geneIdCol) : -1;
  const targetIdx = geneIdIdx !== -1 ? geneIdIdx : 0;

  return parsedData.map((row, idx) => {
    if (!row) return row;
    if (idx < 10) return row;
    const newRow: any[] = [];
    newRow[targetIdx] = row[targetIdx] !== undefined ? row[targetIdx] : row[0];
    return newRow;
  });
}

function newDataset(idx: number, prefix = "Dataset", moduleName = "dp", _stepName?: string): DatasetConfig {
  const userId = getSessionUserId();
  const randToken = Math.random().toString(36).substring(2, 7);
  const datasetIdNum = `${Date.now()}_${randToken}_${idx}`;
  const normMod = moduleName === "enrichment" || moduleName === "en" ? "ea" : moduleName;
  return {
    id: `${userId}_${datasetIdNum}_${normMod}`, name: `${prefix} ${idx + 1}`,
    color: DATASET_COLORS[idx % DATASET_COLORS.length],
    module: normMod,
    parentModule: normMod,
    isInline: false,
    parentDatasetId: undefined,
    expressionFile: null, expressionFileName: "",
    clinicalFile: null, clinicalFileName: "", clinicalSampleIdCol: "", clinicalGroupCol: "", clinicalBatchCol: "",
    clinicalOtherCovariates: [],
    submittedClinicalOtherCovariates: [],
    isNormalized: false, detectedIsNormalized: false,
    dataType: "readcounts", platform: "",
    microarrayPlatformId: "",
    microarrayOrganism: "",
    columns: [], geneInfoCols: [], geneIdCol: "", geneIdType: "", detectedGeneIdType: "",
    nSamples: 0, nFeatures: 0,
    fs_featuresOrientation: "column", fs_featureIndexValue: 0, fs_datasetPurpose: "train-and-test", fs_validationStrategy: "train-test-split", fs_isInternalValidation: true, fs_trainRatio: 0.7,
    fs_isCV: false,
    integrityOk: false, integrityIssues: [],
    annotationMapped: 0, annotationTotal: 0, annotationUnique: 0, annotationMulti: 0, annotationRetained: 0,
    uploadDone: false, annotationDone: false, processingDone: false, normalizationDone: false, batchDone: false,
    hasNA: false,
    sampleOverlapChoice: null,
    de_referenceGroup: "",
    de_comparisonGroup: "",
    submitted_de_referenceGroup: "",
    submitted_de_comparisonGroup: "",
    fs_positiveClass: "",
    fs_negativeClass: "",
    submitted_fs_positiveClass: "",
    submitted_fs_negativeClass: "",
    deStep: "upload",
  };
}

const defaultDP = newDataset(0, "Dataset", "dp", "upload");
const defaultDE = newDataset(0, "Dataset", "de", "upload");
const defaultFS = newDataset(0, "Dataset", "fs", "upload");
const defaultEN = newDataset(0, "Dataset", "ea", "upload");

export const initialState: AppState = {
  pipelineMode: false,

  dpDatasets: [defaultDP], dpCurrentDatasetId: defaultDP.id,
  dpStep: "upload", dpSelectedContext: defaultDP.id, dpSelectedModule: null,
  dpProcessingConfig: { filterMethod: "", cpmThreshold: null, minSamples: null, countThreshold: null, varianceThreshold: null, missingMethod: "", knnK: null, naRemovePercent: 20, applyFilter: null, applyMissing: null, rcFilterMethod: "", normRcFilterMethod: "", maFilterMethod: "", protFilterMethod: "", othFilterMethod: "", rcVariance: null, normRcVariance: null, maVariance: null, protVariance: null, othVariance: null },
  dpNormConfig: { method: "tmm", logTransform: true, transformationType: "log2", priorCount: 0.5 },
  dpBatchConfig: { method: "combat_seq", methodOthers: "combat", batchVariable: "" },
  dpAnnotationConfig: { selectOrganism: "Human (Homo sapiens)", annotationSource: "ensembl", geneBiotype: "Protein-coding", multiMappedStrategy: "keep-first" },
  dpPanelOpen: true,
  inlineDeUploadReviewed: false,
  inlineFsUploadReviewed: false,
  inlineDpUploadReviewed: false,
  dpVisitedSteps: ["upload"],
  deVisitedSteps: ["upload"],
  fsVisitedSteps: ["upload"],
  enVisitedSteps: ["upload"],
  deMetaSkipped: false,
  dpDeMetaSkipped: false,
  dpInlineDeDone: false,
  dpInlineMetaDone: false,
  dpInlineFsDone: false,
  dpInlineEaDone: false,
  dpDeResults: {},
  dpDeMetaMethod: null,
  dpDeMetaVotes: 2,

  deDatasets: [defaultDE], deCurrentDatasetId: defaultDE.id,
  deStep: "upload", deSelectedContext: defaultDE.id, deSelectedModule: null,
  deInlineEaDone: false,
  deConfig: { method: "deseq2", referenceGroup: "", comparisonGroup: "", pValueThreshold: 0.05, logFcThreshold: 1.0, adjustMethod: "BH" },
  deResults: {}, deMetaMethod: null, deMetaVotes: 2,
  dePanelOpen: true,

  fsDatasets: [defaultFS], fsCurrentDatasetId: defaultFS.id,
  fsStep: "upload", fsSelectedContext: defaultFS.id,
  fsConfig: { trainRatio: 0.7, cvEnabled: true, cvFolds: 5, cvMethod: "k_fold", selectedModels: [], maxFeaturesSelect: 10, multiDatasetMode: "combine", validationStrategy: "train-test-split" },
  fsPanelOpen: true,
  standaloneFsDone: false,
  fsTrainingResultsCache: null,
  fsCvResultsCache: null,
  fsTestingResultsCache: null,
  dpTrainingResultsCache: null,
  dpCvResultsCache: null,
  dpTestingResultsCache: null,
  enResultsCache: {},
  dpInlineEnResultsCache: {},
  deInlineEnResultsCache: {},

  enDatasets: [defaultEN], enCurrentDatasetId: defaultEN.id,
  enStep: "upload", enSelectedContext: defaultEN.id,
  standaloneEnDone: false,
  enConfig: { methods: ["gsea"], oraDatabase: ["KEGG", "GO:BP"], gseaDatabase: ["KEGG", "GO:BP"], rankByCol: "", pValueCutoff: 0.05, qValueCutoff: 0.2, minGeneSetSize: 10, maxGeneSetSize: 500, organism: "Human (Homo sapiens)" },
  enPanelOpen: false,

  dpSubmittedAnnotationConfig: null,
  dpSubmittedProcessingConfig: null,
  dpSubmittedNormConfig: null,
  dpSubmittedBatchConfig: null,
  deSubmittedConfig: null,
  fsSubmittedConfig: null,
  enSubmittedConfig: null,
};

export function appReducer(state: AppState, action: AppAction): AppState {
  switch (action.type) {
    // Pipeline
    case "RESET_STORE": {
      const dp = newDataset(0, "Dataset", "dp", "upload");
      const de = newDataset(0, "Dataset", "de", "upload");
      const fs = newDataset(0, "Dataset", "fs", "upload");
      const en = newDataset(0, "Dataset", "enrichment", "upload");
      return {
        pipelineMode: action.pipelineMode ?? false,

        dpDatasets: [dp], dpCurrentDatasetId: dp.id,
        dpStep: "upload", dpSelectedContext: dp.id, dpSelectedModule: null,
        dpProcessingConfig: { filterMethod: "", cpmThreshold: null, minSamples: null, countThreshold: null, varianceThreshold: null, missingMethod: "", knnK: null, naRemovePercent: 20, applyFilter: null, applyMissing: null, rcFilterMethod: "", normRcFilterMethod: "", maFilterMethod: "", protFilterMethod: "", othFilterMethod: "", rcVariance: null, normRcVariance: null, maVariance: null, protVariance: null, othVariance: null },
        dpNormConfig: { method: "tmm", logTransform: true, transformationType: "log2", priorCount: 0.5 },
        dpBatchConfig: { method: "combat_seq", methodOthers: "combat", batchVariable: "" },
        dpAnnotationConfig: { selectOrganism: "Human (Homo sapiens)", annotationSource: "ensembl", geneBiotype: "Protein-coding", multiMappedStrategy: "keep-first" },
        dpPanelOpen: true,
        inlineDeUploadReviewed: false,
        inlineFsUploadReviewed: false,
        inlineDpUploadReviewed: false,
        dpVisitedSteps: ["upload"],
        deVisitedSteps: ["upload"],
        fsVisitedSteps: ["upload"],
        enVisitedSteps: ["upload"],
        deMetaSkipped: false,
        dpDeMetaSkipped: false,
        dpInlineDeDone: false,
        dpInlineMetaDone: false,
        dpInlineFsDone: false,
        dpInlineEaDone: false,
        dpDeResults: {},
        dpDeMetaMethod: null,
        dpDeMetaVotes: 2,

        dpSubmittedAnnotationConfig: null,
        dpSubmittedProcessingConfig: null,
        dpSubmittedNormConfig: null,
        dpSubmittedBatchConfig: null,
        deSubmittedConfig: null,
        fsSubmittedConfig: null,
        enSubmittedConfig: null,

        deDatasets: [de], deCurrentDatasetId: de.id,
        deStep: "upload", deSelectedContext: de.id, deSelectedModule: null,
        deInlineEaDone: false,
        deConfig: { method: "deseq2", referenceGroup: "", comparisonGroup: "", pValueThreshold: 0.05, logFcThreshold: 1.0, adjustMethod: "BH" },
        deResults: {}, deMetaMethod: null, deMetaVotes: 2,
        dePanelOpen: true,

        fsDatasets: [fs], fsCurrentDatasetId: fs.id,
        fsStep: "upload", fsSelectedContext: fs.id,
        fsConfig: { trainRatio: 0.7, cvEnabled: true, cvFolds: 5, cvMethod: "k_fold", selectedModels: [], maxFeaturesSelect: 10 },
        fsPanelOpen: true,
        standaloneFsDone: false,
        fsTrainingResultsCache: null,
        fsCvResultsCache: null,
        fsTestingResultsCache: null,
        dpTrainingResultsCache: null,
        dpCvResultsCache: null,
        dpTestingResultsCache: null,
        enResultsCache: {},
        dpInlineEnResultsCache: {},
        deInlineEnResultsCache: {},

        enDatasets: [en], enCurrentDatasetId: en.id,
        enStep: "upload", enSelectedContext: en.id,
        standaloneEnDone: false,
        enConfig: { methods: ["gsea"], oraDatabase: ["KEGG", "GO:BP"], gseaDatabase: ["KEGG", "GO:BP"], rankByCol: "", pValueCutoff: 0.05, qValueCutoff: 0.2, minGeneSetSize: 10, maxGeneSetSize: 500, organism: "Human (Homo sapiens)" },
        enPanelOpen: false,
      };
    }
    case "SET_INLINE_DE_UPLOAD_REVIEWED": return { ...state, inlineDeUploadReviewed: action.reviewed };
    case "SET_INLINE_FS_UPLOAD_REVIEWED": return { ...state, inlineFsUploadReviewed: action.reviewed };
    case "SET_INLINE_DP_UPLOAD_REVIEWED": return { ...state, inlineDpUploadReviewed: action.reviewed };
    case "SET_DE_META_SKIPPED":
    case "DE_SET_META_SKIPPED": return { ...state, deMetaSkipped: action.skipped };
    case "SET_PIPELINE_MODE": return { ...state, pipelineMode: action.mode };
    case "SET_DP_INLINE_DE_DONE": return { ...state, dpInlineDeDone: action.done };
    case "SET_DP_INLINE_FS_DONE": return { ...state, dpInlineFsDone: action.done };
    case "SET_DP_INLINE_EA_DONE": return { ...state, dpInlineEaDone: action.done };
    case "SET_DP_INLINE_META_DONE": return { ...state, dpInlineMetaDone: action.done };
    case "DP_INLINE_EN_SET_RESULTS": return { ...state, dpInlineEnResultsCache: { ...state.dpInlineEnResultsCache, [action.id]: action.results } };
    case "SET_DE_INLINE_EA_DONE": return { ...state, deInlineEaDone: action.done };
    case "DE_INLINE_EN_SET_RESULTS": return { ...state, deInlineEnResultsCache: { ...state.deInlineEnResultsCache, [action.id]: action.results } };
    case "DE_SET_MODULE": return { ...state, deSelectedModule: action.module };
    case "SET_STANDALONE_FS_DONE": return { ...state, standaloneFsDone: action.done };
    case "SET_STANDALONE_EN_DONE": return { ...state, standaloneEnDone: action.done };
    case "PREPARE_DE_FROM_DP": {
      const syncDatasets = state.dpDatasets.map(d => {
        const newId = d.id.replace("_dp_", "_de_").replace(/_dp_[a-z0-9-]+$/, "_de_analysis");
        return {
          ...d,
          id: newId,
          parentDatasetId: d.id,
          parentModule: "dp",
          module: "de",
          isInline: true,
          uploadDone: true,
          annotationDone: true,
          processingDone: true,
          normalizationDone: true,
          batchDone: true,
          nSamples: d.nSamples,
          nFeatures: d.nFeatures,
          sampleIds: d.sampleIds,
          matchingSamples: d.matchingSamples || (d.sampleIds && d.sampleIds.length > 0 ? d.sampleIds : (d.columns ? getExpressionSampleColumns(d.columns, d.geneIdCol, d.geneInfoCols) : [])),
          groups: d.groups || [],
          clinicalColumns: d.clinicalColumns || [],
          columns: d.columns || []
        };
      });
      const newCurrentId = state.dpCurrentDatasetId.replace("_dp_", "_de_").replace(/_dp_[a-z0-9-]+$/, "_de_analysis");
      const newContext = state.dpSelectedContext === "all" ? "all" : state.dpSelectedContext.replace("_dp_", "_de_").replace(/_dp_[a-z0-9-]+$/, "_de_analysis");
      return {
        ...state,
        deDatasets: syncDatasets,
        deCurrentDatasetId: newCurrentId,
        deSelectedContext: newContext,
        deStep: "analysis",
        deVisitedSteps: ["upload", "analysis"],
      };
    }
    case "PREPARE_FS_FROM_DP": {
      const syncDatasets = state.dpDatasets.map(d => {
        const newId = d.id.replace("_dp_", "_fs_").replace(/_dp_[a-z0-9-]+$/, "_fs_model-selection");
        return {
          ...d,
          id: newId,
          parentDatasetId: d.id,
          parentModule: "dp",
          module: "fs",
          isInline: true,
          uploadDone: true,
          annotationDone: true,
          processingDone: true,
          normalizationDone: true,
          batchDone: true,
          nSamples: d.nSamples,
          nFeatures: d.nFeatures,
          sampleIds: d.sampleIds,
          matchingSamples: d.matchingSamples || (d.sampleIds && d.sampleIds.length > 0 ? d.sampleIds : (d.columns ? getExpressionSampleColumns(d.columns, d.geneIdCol, d.geneInfoCols) : [])),
          groups: d.groups || [],
          clinicalColumns: d.clinicalColumns || [],
          columns: d.columns || [],
          fs_positiveClass: d.fs_positiveClass || "",
          fs_negativeClass: d.fs_negativeClass || "",
          submitted_fs_positiveClass: d.submitted_fs_positiveClass || "",
          submitted_fs_negativeClass: d.submitted_fs_negativeClass || ""
        };
      });
      const newCurrentId = state.dpCurrentDatasetId.replace("_dp_", "_fs_").replace(/_dp_[a-z0-9-]+$/, "_fs_model-selection");
      const newContext = state.dpSelectedContext === "all" ? "all" : state.dpSelectedContext.replace("_dp_", "_fs_").replace(/_dp_[a-z0-9-]+$/, "_fs_model-selection");
      return {
        ...state,
        fsDatasets: syncDatasets,
        fsCurrentDatasetId: newCurrentId,
        fsSelectedContext: newContext,
        fsStep: "model-selection",
        fsVisitedSteps: ["upload", "model-selection"],
      };
    }
    case "PREPARE_EN_FROM_DE": {
      const activeDeId = state.deCurrentDatasetId || state.deDatasets[0]?.id;
      const rawRes = state.deResults[activeDeId] || Object.values(state.deResults)[0] || [];
      const results: any[] = Array.isArray(rawRes)
        ? rawRes
        : (rawRes?.full_results || rawRes?.results || rawRes?.top10 || rawRes?.comb_pval || []);
      const resultsRows = results.map(r => [r?.gene || r?.Feature || "", String(r?.logFC ?? r?.Combined_LogFC ?? ""), String(r?.pValue ?? r?.Combined_PValue ?? ""), String(r?.adjPValue ?? r?.FDR ?? "")]);
      const newEn: DatasetConfig = {
        id: "ds_enrichment_pipeline",
        name: "DE Results Dataset",
        color: "#7835ff",
        expressionFile: null,
        expressionFileName: "de_results_for_enrichment.csv",
        clinicalFile: null,
        clinicalFileName: "",
        clinicalSampleIdCol: "",
        clinicalGroupCol: "",
        clinicalBatchCol: "",
        clinicalOtherCovariates: [],
        submittedClinicalOtherCovariates: [],
        isNormalized: true,
        detectedIsNormalized: true,
        dataType: "readcounts",
        platform: "affymetrix",
        microarrayPlatformId: "",
        microarrayOrganism: "",
        columns: ["GeneName", "LogFC", "PValue", "AdjPValue"],
        geneInfoCols: [],
        geneIdCol: "GeneName",
        geneIdType: "genename",
        detectedGeneIdType: "genename",
        nSamples: 0,
        nFeatures: results.length || 867,
        fs_featuresOrientation: "column",
        fs_featureIndexValue: 0,
        fs_datasetPurpose: "train",
        fs_isInternalValidation: true,
        fs_trainRatio: 0.7,
        integrityOk: true,
        integrityIssues: [],
        annotationMapped: 0,
        annotationTotal: 0,
        annotationUnique: 0,
        annotationMulti: 0,
        annotationRetained: 0,
        uploadDone: true,
        annotationDone: true,
        processingDone: true,
        normalizationDone: true,
        batchDone: true,
        hasNA: false,
        parentModule: "de",
        module: "ea",
        isInline: true,
        parentDatasetId: activeDeId,
        parsedData: resultsRows.length > 0 ? (minimizeParsedData(resultsRows, "GeneName", ["GeneName", "LogFC", "PValue", "AdjPValue"]) || []) : [],
      };
      return {
        ...state,
        enDatasets: [newEn],
        enCurrentDatasetId: newEn.id,
        enSelectedContext: newEn.id,
        enStep: "analysis",
        enVisitedSteps: ["upload", "analysis"],
        enConfig: {
          ...state.enConfig,
          methods: state.enConfig.methods && state.enConfig.methods.length > 0 ? state.enConfig.methods : ["gsea"],
          rankByCol: "LogFC"
        }
      };
    }
    case "REDO_NORMALIZATION": {
      const activeSteps = computeDPSteps(state.dpDatasets);
      const normIdx = activeSteps.findIndex(s => s.id === "normalization" || s.id === "normalization-counts");
      const allowed = normIdx !== -1 ? activeSteps.slice(0, normIdx).map(s => s.id) : ["upload"];
      const nextDpVisited = state.dpVisitedSteps.filter(sId => allowed.includes(sId) || sId === "upload");

      return {
        ...state,
        dpInlineDeDone: false,
        dpInlineFsDone: false,
        dpInlineEaDone: false,
        dpInlineMetaDone: false,
        dpDeResults: {},
        dpDeMetaMethod: null,
        dpDeMetaVotes: 2,
        dpDeMetaSkipped: false,
        inlineDpUploadReviewed: false,
        dpVisitedSteps: nextDpVisited,
        dpSubmittedNormConfig: null,
        dpSubmittedBatchConfig: null,
        dpTrainingResultsCache: null,
        dpCvResultsCache: null,
        dpTestingResultsCache: null,
        dpInlineEnResultsCache: {},
      };
    }

    case "DP_SUBMIT_ANNOTATION": return { ...state, dpSubmittedAnnotationConfig: { ...state.dpAnnotationConfig } };
    case "DP_SUBMIT_PROCESSING": return { ...state, dpSubmittedProcessingConfig: { ...state.dpProcessingConfig } };
    case "DP_SUBMIT_NORM": return { ...state, dpSubmittedNormConfig: { ...state.dpNormConfig } };
    case "DP_SUBMIT_BATCH": return { ...state, dpSubmittedBatchConfig: { ...state.dpBatchConfig } };
    case "DE_SUBMIT_CONFIG": return { ...state, deSubmittedConfig: { ...state.deConfig } };
    case "FS_SUBMIT_CONFIG": {
      const ratios: Record<string, number> = {};
      const purposes: Record<string, string> = {};
      const strategies: Record<string, string> = {};
      const internals: Record<string, boolean> = {};
      state.fsDatasets.forEach(d => {
        ratios[d.id] = d.fs_trainRatio ?? 0.7;
        purposes[d.id] = d.fs_datasetPurpose || "train-and-test";
        strategies[d.id] = d.fs_validationStrategy || "train-test-split";
        internals[d.id] = d.fs_isInternalValidation ?? false;
      });
      return {
        ...state,
        fsSubmittedConfig: {
          ...state.fsConfig,
          fs_trainRatios: ratios,
          fs_datasetPurposes: purposes,
          fs_validationStrategies: strategies,
          fs_isInternalValidations: internals
        }
      };
    }
    case "EN_SUBMIT_CONFIG": return { ...state, enSubmittedConfig: { ...state.enConfig } };

    case "RESET_DOWNSTREAM_STEPS": {
      const { datasetId, fromStep } = action;
      const isViewingAll = datasetId === "all";

      let targetModule: "dp" | "de" | "fs" | "en" = "dp";
      if (
        datasetId.includes("_de") ||
        state.deDatasets.some(d => d.id === datasetId) ||
        ["de-analysis", "meta-analysis", "analysis", "meta"].includes(fromStep)
      ) {
        targetModule = "de";
      } else if (
        datasetId.includes("_fs") ||
        state.fsDatasets.some(d => d.id === datasetId) ||
        ["model-selection", "cross-validation", "testing"].includes(fromStep)
      ) {
        targetModule = "fs";
      } else if (
        datasetId.includes("_en") ||
        state.enDatasets.some(d => d.id === datasetId) ||
        ["enrichment-upload", "enrichment-analysis"].includes(fromStep)
      ) {
        targetModule = "en";
      } else {
        targetModule = "dp";
      }

      const nextState = {
        ...state,
      } as AppState;

      let nextDpVisitedSteps = state.dpVisitedSteps;
      let nextDeVisitedSteps = state.deVisitedSteps;
      let nextFsVisitedSteps = state.fsVisitedSteps;
      let nextEnVisitedSteps = state.enVisitedSteps;

      if (targetModule === "dp") {
        const resetDatasetFlags = (d: DatasetConfig) => {
          const isMicroarray = d.dataType === "microarray";
          const isOthers = d.dataType === "others";

          const patch: Partial<DatasetConfig> = {};

          if (isMicroarray) {
            if (fromStep === "upload") {
              patch.normalizationDone = false;
              patch.annotationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "normalization") {
              patch.annotationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "annotation") {
              patch.batchDone = false;
            }
          } else if (isOthers) {
            if (fromStep === "upload") {
              patch.processingDone = false;
              patch.normalizationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "processing") {
              patch.normalizationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "normalization" || fromStep === "normalization-counts") {
              patch.batchDone = false;
            }
          } else {
            if (fromStep === "upload") {
              patch.annotationDone = false;
              patch.processingDone = false;
              patch.normalizationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "annotation") {
              patch.processingDone = false;
              patch.normalizationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "processing") {
              patch.normalizationDone = false;
              patch.batchDone = false;
            } else if (fromStep === "normalization" || fromStep === "normalization-counts") {
              patch.batchDone = false;
            }
          }
          return { ...d, ...patch };
        };

        const updatedDpDatasets = state.dpDatasets.map(d => {
          if (isViewingAll || d.id === datasetId) {
            return resetDatasetFlags(d);
          }
          return d;
        });

        nextState.dpDatasets = updatedDpDatasets;

        if (["upload", "annotation", "processing", "normalization", "normalization-counts", "normalization-others", "batch"].includes(fromStep)) {
          nextState.dpInlineDeDone = false;
          nextState.dpInlineFsDone = false;
          nextState.dpInlineEaDone = false;
          nextState.dpInlineMetaDone = false;
          nextState.dpSelectedModule = nextState.dpSelectedModule === "enrichment" ? null : nextState.dpSelectedModule;
          nextState.dpDeResults = {};
          nextState.dpDeMetaMethod = null;
          nextState.dpDeMetaVotes = 2;
          nextState.dpDeMetaSkipped = false;
          nextState.inlineDpUploadReviewed = false;
          nextState.inlineDeUploadReviewed = false;
          nextState.inlineFsUploadReviewed = false;

          nextState.dpTrainingResultsCache = null;
          nextState.dpCvResultsCache = null;
          nextState.dpTestingResultsCache = null;
          nextState.dpInlineEnResultsCache = {};

          const activeSteps = computeDPSteps(updatedDpDatasets);
          const fromIdx = activeSteps.findIndex(
            s => s.id === fromStep || 
                 (fromStep === "normalization" && s.id === "normalization-counts") || 
                 (fromStep === "normalization-counts" && s.id === "normalization")
          );
          if (fromIdx !== -1) {
            const allowedStepIds = activeSteps.slice(0, fromIdx).map(s => s.id);
            nextDpVisitedSteps = state.dpVisitedSteps.filter(sId => allowedStepIds.includes(sId) || sId === "upload");
          }

          if (fromStep === "upload") {
            nextState.dpSubmittedAnnotationConfig = null;
            nextState.dpSubmittedProcessingConfig = null;
            nextState.dpSubmittedNormConfig = null;
            nextState.dpSubmittedBatchConfig = null;
          } else if (fromStep === "annotation") {
            nextState.dpSubmittedProcessingConfig = null;
            nextState.dpSubmittedNormConfig = null;
            nextState.dpSubmittedBatchConfig = null;
          } else if (fromStep === "processing") {
            nextState.dpSubmittedNormConfig = null;
            nextState.dpSubmittedBatchConfig = null;
          } else if (fromStep === "normalization" || fromStep === "normalization-counts" || fromStep === "normalization-others") {
            nextState.dpSubmittedBatchConfig = null;
          } else if (fromStep === "batch") {
            nextState.dpSubmittedBatchConfig = null;
          }
        }

        if (fromStep === "inline-de" || fromStep === "inline-de-meta") {
          nextState.dpInlineMetaDone = false;
          nextState.dpInlineEaDone = false;
          nextState.dpInlineEnResultsCache = {};
          nextState.dpInlineFsDone = false;
          nextState.dpTrainingResultsCache = null;
          nextState.dpCvResultsCache = null;
          nextState.dpTestingResultsCache = null;
        }
      }

      if (targetModule === "de") {
        if (fromStep === "upload") {
          nextState.deDatasets = state.deDatasets.map(d => (isViewingAll || d.id === datasetId) ? { ...d, uploadDone: false } : d);
          nextState.deResults = {};
          nextState.deMetaMethod = null;
          nextState.deMetaVotes = 2;
          nextState.deMetaSkipped = false;
          nextState.deInlineEaDone = false;
          nextState.deInlineEnResultsCache = {};
          nextState.deSubmittedConfig = null;
          nextDeVisitedSteps = ["upload"];
        } else if (fromStep === "de-analysis" || fromStep === "meta-analysis" || fromStep === "analysis" || fromStep === "meta") {
          nextState.deInlineEaDone = false;
          nextState.deInlineEnResultsCache = {};
          nextState.deSubmittedConfig = null;
          const allowed = ["upload", "analysis"];
          nextDeVisitedSteps = state.deVisitedSteps.filter(sId => allowed.includes(sId));
          if (fromStep === "de-analysis" || fromStep === "analysis") {
            const deId = datasetId === "all" ? state.deCurrentDatasetId : datasetId;
            const nextDeResults = { ...state.deResults };
            delete nextDeResults[deId];
            nextState.deResults = nextDeResults;
          }
          if (fromStep === "meta-analysis" || fromStep === "meta") {
            const nextDeResults = { ...state.deResults };
            delete nextDeResults["meta"];
            delete nextDeResults["meta_transcriptomics"];
            delete nextDeResults["meta_proteomics"];
            nextState.deResults = nextDeResults;
            nextState.deMetaMethod = null;
          }
        }
      }

      if (targetModule === "fs") {
        if (fromStep === "upload") {
          nextState.fsDatasets = state.fsDatasets.map(d => (isViewingAll || d.id === datasetId) ? { ...d, uploadDone: false } : d);
          nextState.standaloneFsDone = false;
          nextState.fsTrainingResultsCache = null;
          nextState.fsCvResultsCache = null;
          nextState.fsTestingResultsCache = null;
          nextState.fsSubmittedConfig = null;
          nextFsVisitedSteps = ["upload"];
        } else if (fromStep === "model-selection") {
          nextState.standaloneFsDone = false;
          nextState.inlineFsUploadReviewed = false;
          const allowed = ["upload", "model-selection"];
          nextFsVisitedSteps = state.fsVisitedSteps.filter(sId => allowed.includes(sId));
          
          nextState.fsTrainingResultsCache = null;
          nextState.fsCvResultsCache = null;
          nextState.fsTestingResultsCache = null;
          nextState.fsSubmittedConfig = null;
        } else if (fromStep === "cross-validation") {
          nextState.standaloneFsDone = false;
          const allowed = ["upload", "model-selection", "cross-validation"];
          nextFsVisitedSteps = state.fsVisitedSteps.filter(sId => allowed.includes(sId));

          nextState.fsCvResultsCache = null;
          nextState.fsTestingResultsCache = null;
        } else if (fromStep === "testing") {
          nextState.fsTestingResultsCache = null;
        }
      }

      if (targetModule === "en") {
        if (fromStep === "upload" || fromStep === "enrichment-upload") {
          nextState.enDatasets = state.enDatasets.map(d => (isViewingAll || d.id === datasetId) ? { ...d, uploadDone: false } : d);
          nextState.standaloneEnDone = false;
          nextState.enResultsCache = {};
          nextState.enSubmittedConfig = null;
          nextEnVisitedSteps = ["upload"];
        } else if (fromStep === "analysis" || fromStep === "enrichment-analysis") {
          nextState.standaloneEnDone = false;
          nextState.enResultsCache = {};
          nextState.enSubmittedConfig = null;
          const allowed = ["upload", "analysis"];
          nextEnVisitedSteps = state.enVisitedSteps.filter(sId => allowed.includes(sId));
        }
      }

      nextState.dpVisitedSteps = nextDpVisitedSteps;
      nextState.deVisitedSteps = nextDeVisitedSteps;
      nextState.fsVisitedSteps = nextFsVisitedSteps;
      nextState.enVisitedSteps = nextEnVisitedSteps;

      return nextState;
    }

    // DP
    case "DP_ADD_DATASET": {
      const idx = state.dpDatasets.length;
      const ds = newDataset(idx, "Dataset", "dp", state.dpStep);
      return { ...state, dpDatasets: [...state.dpDatasets, ds], dpCurrentDatasetId: ds.id, dpSelectedContext: ds.id };
    }
    case "DP_REMOVE_DATASET": {
      const rem = state.dpDatasets.filter(d => d.id !== action.id);
      if (!rem.length) return state;
      const nc = rem[rem.length - 1].id;
      const context = rem.length > 1 ? state.dpSelectedContext : nc;
      return { ...state, dpDatasets: rem, dpCurrentDatasetId: nc, dpSelectedContext: context };
    }
    case "DP_SELECT_DATASET": return { ...state, dpCurrentDatasetId: action.id };
    case "DP_SET_STEP": {
      const step = action.step;
      const visited = state.dpVisitedSteps.includes(step)
        ? state.dpVisitedSteps
        : [...state.dpVisitedSteps, step];
      return {
        ...state,
        dpStep: step,
        dpVisitedSteps: visited
      };
    }
    case "DP_SET_CONTEXT": return { ...state, dpSelectedContext: action.id };
    case "DP_UPDATE_DATASET": {
      const updatedDatasets = state.dpDatasets.map(d => {
        if (d.id === action.id) {
          const merged = { ...d, ...action.patch };
          if (merged.uploadDone && merged.parsedData) {
            merged.parsedData = minimizeParsedData(merged.parsedData, merged.geneIdCol, merged.columns);
          }
          return merged;
        }
        return d;
      });
      return { ...state, dpDatasets: updatedDatasets };
    }
    case "DP_UPDATE_ALL_DATASETS": {
      const updatedDatasets = state.dpDatasets.map(d => {
        const merged = { ...d, ...action.patch };
        if (merged.uploadDone && merged.parsedData) {
          merged.parsedData = minimizeParsedData(merged.parsedData, merged.geneIdCol, merged.columns);
        }
        return merged;
      });
      return { ...state, dpDatasets: updatedDatasets };
    }
    case "DP_SET_PROCESSING": return { ...state, dpProcessingConfig: { ...state.dpProcessingConfig, ...action.patch } };
    case "DP_SET_NORM": return { ...state, dpNormConfig: { ...state.dpNormConfig, ...action.patch } };
    case "DP_SET_BATCH": return { ...state, dpBatchConfig: { ...state.dpBatchConfig, ...action.patch } };
    case "DP_SET_ANNOTATION": return { ...state, dpAnnotationConfig: { ...state.dpAnnotationConfig, ...action.patch } };
    case "DP_SET_MODULE": return { ...state, dpSelectedModule: action.module };
    case "DP_TOGGLE_PANEL": return { ...state, dpPanelOpen: !state.dpPanelOpen };

    // DE
    case "DE_ADD_DATASET": {
      const idx = state.deDatasets.length;
      const ds = newDataset(idx, "Dataset", "de", state.deStep);
      return { ...state, deDatasets: [...state.deDatasets, ds], deCurrentDatasetId: ds.id, deSelectedContext: "all", deStep: "upload" };
    }
    case "DE_REMOVE_DATASET": {
      const rem = state.deDatasets.filter(d => d.id !== action.id);
      if (!rem.length) return state;
      const nc = rem[rem.length - 1].id;
      const context = rem.length > 1 ? state.deSelectedContext : nc;
      return { ...state, deDatasets: rem, deCurrentDatasetId: nc, deSelectedContext: context };
    }
    case "DE_SELECT_DATASET": return { ...state, deCurrentDatasetId: action.id };
    case "DE_SET_STEP": {
      const step = (action.step === "module-select" && state.deMetaSkipped) ? "export" : action.step;
      const currentContext = state.deSelectedContext;
      let nextDatasets = state.deDatasets;
      if (currentContext !== "all") {
        nextDatasets = state.deDatasets.map(d => d.id === currentContext ? { ...d, deStep: step } : d);
      }
      const visited = state.deVisitedSteps.includes(step)
        ? state.deVisitedSteps
        : [...state.deVisitedSteps, step];
      return {
        ...state,
        deStep: step,
        deDatasets: nextDatasets,
        deVisitedSteps: visited
      };
    }
    case "DE_SET_CONTEXT": {
      const targetId = action.id;
      let nextStep = state.deStep;
      if (targetId === "all") {
        const deSteps = state.deDatasets.map(d => d.deStep || "upload");
        const STEP_TO_MULTI_INDEX: Record<string, number> = {
          "upload": 0,
          "all-datasets": 1,
          "analysis": 2,
          "meta": 3,
          "module-select": 4,
          "inline-enrichment": 4,
          "export": 5
        };
        const MULTI_INDEX_TO_STEP: DEStep[] = ["upload", "all-datasets", "analysis", "meta", "module-select", "export"];
        const indices = deSteps.map(step => STEP_TO_MULTI_INDEX[step] ?? 0);
        const minIndex = Math.min(...indices);
        nextStep = MULTI_INDEX_TO_STEP[minIndex];
      } else {
        const ds = state.deDatasets.find(d => d.id === targetId);
        if (ds) {
          if (ds.deStep) {
            nextStep = ds.deStep;
          } else {
            if (!ds.uploadDone) {
              nextStep = "upload";
            } else if (!state.deResults[ds.id]) {
              nextStep = "analysis";
            } else {
              nextStep = state.deMetaSkipped ? "export" : "module-select";
            }
          }
        }
      }
      const visited = state.deVisitedSteps.includes(nextStep)
        ? state.deVisitedSteps
        : [...state.deVisitedSteps, nextStep];
      return {
        ...state,
        deSelectedContext: targetId,
        deStep: nextStep,
        deCurrentDatasetId: targetId === "all" ? state.deCurrentDatasetId : targetId,
        deVisitedSteps: visited
      };
    }
    case "DE_UPDATE_DATASET": {
      const updatedDatasets = state.deDatasets.map(d => {
        if (d.id === action.id) {
          const merged = { ...d, ...action.patch };
          if (merged.uploadDone && merged.parsedData) {
            merged.parsedData = minimizeParsedData(merged.parsedData, merged.geneIdCol, merged.columns);
          }
          return merged;
        }
        return d;
      });
      return { ...state, deDatasets: updatedDatasets };
    }
     case "DE_SET_CONFIG": return { ...state, deConfig: { ...state.deConfig, ...action.patch } };
     case "DE_SET_RESULTS": return { ...state, deResults: { ...state.deResults, [action.id]: action.results } };
     case "DP_SET_DE_RESULTS": return { ...state, dpDeResults: { ...state.dpDeResults, [action.id]: action.results } };
     case "DP_SET_DE_META": return { ...state, dpDeMetaMethod: action.method };
     case "DP_SET_DE_META_VOTES": return { ...state, dpDeMetaVotes: action.n };
     case "DP_SET_DE_META_SKIPPED": return { ...state, dpDeMetaSkipped: action.skipped };
     case "EN_SET_RESULTS": return { ...state, enResultsCache: { ...state.enResultsCache, [action.id]: action.results } };
     case "DE_SET_META": return { ...state, deMetaMethod: action.method };
     case "DE_SET_META_VOTES": return { ...state, deMetaVotes: action.n };
    case "DE_TOGGLE_PANEL": return { ...state, dePanelOpen: !state.dePanelOpen };

    // FS
    case "FS_ADD_DATASET": { const idx = state.fsDatasets.length; const ds = newDataset(idx, "Dataset", "fs", state.fsStep); return { ...state, fsDatasets: [...state.fsDatasets, ds], fsCurrentDatasetId: ds.id, fsSelectedContext: "all", fsStep: "upload" }; }
    case "FS_REMOVE_DATASET": {
      const rem = state.fsDatasets.filter(d => d.id !== action.id);
      if (!rem.length) return state;
      const nc = rem[rem.length - 1].id;
      const context = rem.length > 1 ? state.fsSelectedContext : nc;
      return { ...state, fsDatasets: rem, fsCurrentDatasetId: nc, fsSelectedContext: context };
    }
    case "FS_SELECT_DATASET": return { ...state, fsCurrentDatasetId: action.id };
    case "FS_SET_STEP": {
      const visited = state.fsVisitedSteps.includes(action.step)
        ? state.fsVisitedSteps
        : [...state.fsVisitedSteps, action.step];
      return { ...state, fsStep: action.step, fsVisitedSteps: visited };
    }
    case "FS_SET_CONTEXT": return { ...state, fsSelectedContext: action.id };
    case "FS_UPDATE_DATASET": {
      const updatedDatasets = state.fsDatasets.map(d => {
        if (d.id === action.id) {
          const merged = { ...d, ...action.patch };
          if (merged.uploadDone && merged.parsedData) {
            merged.parsedData = minimizeParsedData(merged.parsedData, merged.geneIdCol, merged.columns);
          }
          return merged;
        }
        return d;
      });
      return { ...state, fsDatasets: updatedDatasets };
    }
    case "FS_SET_CONFIG": return { ...state, fsConfig: { ...state.fsConfig, ...action.patch } };
    case "FS_TOGGLE_PANEL": return { ...state, fsPanelOpen: !state.fsPanelOpen };
    case "FS_SET_TRAINING_CACHE": return { ...state, fsTrainingResultsCache: action.results };
    case "FS_SET_CV_CACHE": return { ...state, fsCvResultsCache: action.results };
    case "FS_SET_TESTING_CACHE": return { ...state, fsTestingResultsCache: action.results };
    case "DP_SET_TRAINING_CACHE": return { ...state, dpTrainingResultsCache: action.results };
    case "DP_SET_CV_CACHE": return { ...state, dpCvResultsCache: action.results };
    case "DP_SET_TESTING_CACHE": return { ...state, dpTestingResultsCache: action.results };

    // EN
    case "EN_ADD_DATASET": { const idx = state.enDatasets.length; const ds = newDataset(idx, "Dataset", "enrichment", state.enStep); return { ...state, enDatasets: [...state.enDatasets, ds], enCurrentDatasetId: ds.id, enSelectedContext: "all", enStep: "upload" }; }
    case "EN_REMOVE_DATASET": {
      const rem = state.enDatasets.filter(d => d.id !== action.id);
      if (!rem.length) return state;
      const nc = rem[rem.length - 1].id;
      const context = rem.length > 1 ? state.enSelectedContext : nc;
      return { ...state, enDatasets: rem, enCurrentDatasetId: nc, enSelectedContext: context };
    }
    case "EN_SELECT_DATASET": return { ...state, enCurrentDatasetId: action.id };
    case "EN_SET_STEP": {
      const visited = state.enVisitedSteps.includes(action.step)
        ? state.enVisitedSteps
        : [...state.enVisitedSteps, action.step];
      return { ...state, enStep: action.step, enVisitedSteps: visited };
    }
    case "EN_SET_CONTEXT": return { ...state, enSelectedContext: action.id };
    case "EN_UPDATE_DATASET": {
      const updatedDatasets = state.enDatasets.map(d => {
        if (d.id === action.id) {
          const merged = { ...d, ...action.patch };
          if (merged.uploadDone && merged.parsedData) {
            merged.parsedData = minimizeParsedData(merged.parsedData, merged.geneIdCol, merged.columns);
          }
          return merged;
        }
        return d;
      });
      return { ...state, enDatasets: updatedDatasets };
    }
    case "EN_SET_CONFIG": return { ...state, enConfig: { ...state.enConfig, ...action.patch } };
    case "EN_TOGGLE_PANEL": return { ...state, enPanelOpen: !state.enPanelOpen };

    default: return state;
  }
}

export const AppContext = createContext<{ state: AppState; dispatch: React.Dispatch<AppAction> } | null>(null);

export function useAppStore() {
  const ctx = useContext(AppContext);
  if (!ctx) throw new Error("useAppStore must be used within AppProvider");
  return ctx;
}
