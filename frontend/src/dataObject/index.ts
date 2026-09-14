export type GeneIdType = "ensembl" | "entrez" | "genename" | "others" | "" | string;
export type DataType = "readcounts" | "microarray" | "proteomics" | "others";
export type Platform = "" | "affymetrix" | "illumina"
export type MissingValueMethod = "mean" | "median" | "knn" | "remove" | "zero";
export type NormMethod = "tmm" | "deseq2" | "cpm" | "quantile" | "vsn" | "none" | "vst" | "rlog" | "uq";
export type FilterMethod = "cpm" | "min_count" | "variance" | "none";
export type PValueAdjMethod = "BH" | "BY" | "bonferroni" | "holm" | "none";
export type BatchMethod = "combat" | "combat_seq" | "limma_removebatch" | "none";
export type MetaMethod = "combine_pvalue" | "effect_size" | "vote_counting" | "shared_genes";
export type SelectedModule = "de" | "feature-selection" | "enrichment" | null;
export type FSStep = "upload" | "all-datasets" | "model-selection" | "cross-validation" | "testing" | "export";
export type EnrichmentStep = "upload" | "analysis" | "export";
export type EnrichmentMethod = "ora" | "gsea";
export type EnrichmentDb = "GO:BP" | "GO:MF" | "GO:CC" | "KEGG" | "REACTOME" | "MSigDB_H" | "Reactome" | "";
export type GSEADb = "MSigDB_H" | "MSigDB_C5" | "MSigDB_C2" | "KEGG" | "GO_BP" | "Reactome" | "GO:BP" | "GO:MF" | "GO:CC" | "REACTOME" | "";
export type MLModel = "logistic" | "svm" | "randomforest" | "gbm" | "stabl" | "boruta";
export type CVMethod = "k_fold" | "loocv";

export type HierarchyGroup = "transcriptomics" | "proteomics" | "others";
export type FSMultiDatasetMode = "individual" | "combine";
export type FSValidationStrategy = "train-test-split" | "cv-only";

export function getHierarchyGroup(dataType: DataType | string): HierarchyGroup {
  if (dataType === "readcounts" || dataType === "microarray") return "transcriptomics";
  if (dataType === "proteomics") return "proteomics";
  return "others";
}

export function canPoolDatasets(dt1: DataType | string, dt2: DataType | string): boolean {
  if (dt1 === "others" || dt2 === "others") return false;
  return dt1 === dt2;
}

export function canCrossTestDatasets(dt1: DataType | string, dt2: DataType | string): boolean {
  return getHierarchyGroup(dt1) === getHierarchyGroup(dt2);
}

export function isMetaEligible(datasets: { dataType?: DataType | string }[]): boolean {
  let txCount = 0;
  let prCount = 0;
  for (const d of datasets) {
    const group = getHierarchyGroup(d.dataType || "");
    if (group === "transcriptomics") txCount++;
    else if (group === "proteomics") prCount++;
  }
  return txCount > 1 || prCount > 1;
}

/**
 * Returns true if Enrichment Analysis is eligible as a downstream step.
 *
 * Rules:
 *  - No transcriptomics or proteomics datasets → false (e.g., all 'others').
 *  - For each EA-capable hierarchy (transcriptomics, proteomics):
 *      - count = 1 → eligible using per-dataset DE results (metaDone not required).
 *      - count > 1 → eligible ONLY if metaDone = true for that hierarchy.
 */
export function isEAEligible(
  datasets: { dataType?: DataType | string }[],
  metaDone: boolean
): boolean {
  const txCount = datasets.filter(d => getHierarchyGroup(d.dataType || "") === "transcriptomics").length;
  const prCount = datasets.filter(d => getHierarchyGroup(d.dataType || "") === "proteomics").length;

  if (txCount === 0 && prCount === 0) return false; // no EA-capable data types (e.g., all 'others')

  // If any eligible hierarchy has >1 dataset, meta-analysis must be complete
  const needsMeta = txCount > 1 || prCount > 1;
  if (needsMeta && !metaDone) return false;

  return true;
}

export interface GeneColumn { name: string; isGeneId: boolean; }

export interface DatasetConfig {
  id: string;
  name: string;
  color: string;
  expressionFile: File | null;
  expressionFileName: string;
  clinicalFile: File | null;
  clinicalFileName: string;
  clinicalSampleIdCol: string;
  clinicalGroupCol: string;
  clinicalBatchCol: string;
  clinicalOtherCovariates?: string[];
  isNormalized: boolean;
  detectedIsNormalized: boolean;
  dataType: DataType;
  platform: Platform;
  microarrayPlatformId?: string;
  microarrayOrganism?: string;
  columns: string[];
  sampleIds?: string[];
  geneInfoCols: string[];
  geneIdCol: string;
  geneIdType: GeneIdType;
  detectedGeneIdType: GeneIdType;
  nSamples: number;
  nFeatures: number;
  fs_featuresOrientation: "column" | "headers";
  fs_featureIndexValue: number;
  fs_datasetPurpose: "train" | "test" | "train-and-test";
  fs_validationStrategy?: FSValidationStrategy;
  fs_isInternalValidation: boolean;
  fs_trainRatio: number;
  fs_isCV?: boolean;
  integrityOk: boolean;
  integrityIssues: string[];
  expressionRawText?: string;
  clinicalRawText?: string;
  expressionFilePath?: string;
  clinicalFilePath?: string;
  expressionUploadId?: string;
  clinicalUploadId?: string;
  annotationMapped: number;
  annotationTotal: number;
  annotationUnique: number;
  annotationMulti: number;
  annotationRetained: number;
  processingInputFeatures: number;
  processingRemovedFeatures: number;
  processingRetainedFeatures: number;
  processingMissingValuesCount: number;
  uploadDone: boolean;
  annotationDone: boolean;
  processingDone: boolean;
  normalizationDone: boolean;
  batchDone: boolean;
  hasNA: boolean;
  parsedData: (string | number)[][];
  clinicalColumns: string[];
  clinicalParsedData: string[][];
  sampleOverlapChoice: "all" | "overlap" | null;
  deUploadBypassed: boolean;
  fsUploadBypassed: boolean;
  submittedExpressionFileName: string;
  submittedClinicalFileName: string;
  submittedClinicalSampleIdCol: string;
  submittedClinicalGroupCol: string;
  submittedClinicalBatchCol: string;
  submittedClinicalOtherCovariates: string[];
  submittedIsNormalized: boolean;
  submittedDataType: string;
  submittedPlatform: string;
  submittedGeneIdCol: string;
  submittedGeneIdType: string;
  de_referenceGroup: string;
  de_comparisonGroup: string;
  submitted_de_referenceGroup: string;
  submitted_de_comparisonGroup: string;
  fs_positiveClass?: string;
  fs_negativeClass?: string;
  submitted_fs_positiveClass?: string;
  submitted_fs_negativeClass?: string;
  positiveClass?: string;
  negativeClass?: string;
  deStep: DEStep;
  minGroupSampleSize?: number;
  normalizationMethod?: NormMethod;
  normalizationLogTransform?: boolean;
  normalizationTransformationType?: "log2" | "log10" | "none";
  boxplotBefore?: string;
  boxplotAfter?: string;
  matchingSamples?: string[];
  missingClinSamples?: string[];
  clinicalNoExprSamples?: string[];
  hasMissingClinicalValues?: boolean;
  removedSamples?: string[];
  module?: string;
  parentModule?: string;
  isInline?: boolean;
  parentDatasetId?: string;
  groups?: string[];
  sharedFeaturesCount?: number;
}

export interface ProcessingConfig {
  filterMethod: FilterMethod | "" | null;
  cpmThreshold: number | "" | null;
  countThreshold: number | "" | null;
  varianceThreshold: number | "" | null;
  minSamples: number | "" | null;
  missingMethod: MissingValueMethod | "" | null;
  knnK: number | "" | null;
  naRemovePercent?: number | "" | null;
  applyFilter: boolean | null;
  applyMissing: boolean | null;
  rcFilterMethod?: FilterMethod | "" | null;
  normRcFilterMethod?: FilterMethod | "" | null;
  maFilterMethod?: FilterMethod | "" | null;
  protFilterMethod?: FilterMethod | "" | null;
  othFilterMethod?: FilterMethod | "" | null;
  rcVariance?: number | "" | null;
  normRcVariance?: number | "" | null;
  maVariance?: number | "" | null;
  protVariance?: number | "" | null;
  othVariance?: number | "" | null;
}

export interface NormalizationConfig {
  method: NormMethod;
  logTransform: boolean;
  transformationType?: "log2" | "log10" | "none";
  priorCount: number;
}

export interface BatchConfig {
  method: BatchMethod;
  methodOthers: BatchMethod;
  batchVariable: string;
}

export interface AnnotationConfig {
  selectOrganism: string;
  annotationSource: string;
  geneBiotype: string;
  multiMappedStrategy?: string;
}

export interface DEConfig {
  method?: string;
  referenceGroup: string;
  comparisonGroup: string;
  pValueThreshold: number;
  logFcThreshold: number;
  adjustMethod: PValueAdjMethod;
}

export interface DEResult {
  gene: string;
  logFC: number;
  pValue: number;
  adjPValue: number;
  baseMean: number;
  significant: boolean;
  direction: "up" | "down" | "ns";
}

export interface FSConfig {
  trainRatio: number;
  cvEnabled: boolean;
  cvFolds: number;
  cvMethod: CVMethod;
  selectedModels: MLModel[];
  fs_trainRatios?: Record<string, number>;
  fs_datasetPurposes?: Record<string, string>;
  fs_validationStrategies?: Record<string, string>;
  fs_isInternalValidations?: Record<string, boolean>;
  maxFeaturesSelect?: number;
  multiDatasetMode?: FSMultiDatasetMode;
  validationStrategy?: FSValidationStrategy;
}

export interface EnrichmentConfig {
  methods: EnrichmentMethod[];
  oraDatabase: EnrichmentDb[];
  gseaDatabase: GSEADb[];
  rankByCol: string;
  pValueCutoff: number;
  qValueCutoff: number;
  minGeneSetSize: number;
  maxGeneSetSize: number;
  organism?: string;
  geneIdType?: string;
}

export type DPStep = "upload" | "all-datasets" | "annotation" | "processing" | "normalization" | "normalization-counts" | "normalization-others" | "batch" | "module-select" | "inline-de" | "inline-de-meta" | "inline-fs" | "inline-enrichment" | "export";
export type DEStep = "upload" | "all-datasets" | "analysis" | "meta" | "module-select" | "inline-enrichment" | "export";

export const DP_STEPS: { id: DPStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "annotation", label: "Annotation" },
  { id: "processing", label: "Processing" },
  { id: "normalization", label: "Normalization" },
  { id: "module-select", label: "Analysis Module" },
  { id: "export", label: "Export" },
];

export const DP_STEPS_WITH_BATCH: { id: DPStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "annotation", label: "Annotation" },
  { id: "processing", label: "Processing" },
  { id: "normalization", label: "Normalization" },
  { id: "batch", label: "Batch Correction" },
  { id: "module-select", label: "Analysis Module" },
  { id: "export", label: "Export" },
];

export const DP_STEPS_NO_BATCH = DP_STEPS;

export function isMissingClinical(d: DatasetConfig | undefined): boolean {
  if (!d) return true;
  if (!d.submittedClinicalFileName) return true;
  const exprSamples = (d.sampleIds && d.sampleIds.length > 0)
    ? d.sampleIds
    : (d.columns || []).filter(c => {
        if (c === d.geneIdCol || (d.geneInfoCols || []).includes(c)) return false;
        const lower = c.toLowerCase().trim();
        if (["entrez_id", "gene_symbol", "gene_biotype"].includes(lower)) return false;
        return true;
      });
  const sampleIdIdx = d.submittedClinicalSampleIdCol && d.clinicalColumns
    ? d.clinicalColumns.indexOf(d.submittedClinicalSampleIdCol)
    : -1;
  const clinSamples = sampleIdIdx !== -1 && d.clinicalParsedData
    ? d.clinicalParsedData.map(row => row && row[sampleIdIdx]).filter(Boolean)
    : [];
  const clinSet = new Set(clinSamples);
  const matchingSamples = exprSamples.filter(s => clinSet.has(s));
  return matchingSamples.length === 0;
}

export function computeDPSteps(datasets: DatasetConfig[]): { id: string; label: string }[] {
  const multipleDs = datasets.length > 1;
  const dataTypes = new Set(datasets.map(d => d.dataType));

  // Default ribbon when no datasets configured yet (readcounts flow)
  if (datasets.length === 0) {
    return [
      { id: "upload", label: "Upload" },
      { id: "annotation", label: "Annotation" },
      { id: "processing", label: "Processing" },
      { id: "normalization-counts", label: "Normalization" },
      { id: "module-select", label: "Analysis Module" },
      { id: "export", label: "Export" },
    ];
  }

  const base: { id: string; label: string }[] = [{ id: "upload", label: "Upload" }];

  if (multipleDs) {
    base.push({ id: "all-datasets", label: "Upload Summary" });
    
    // Upload > Microarray Normalization (only apply if microarray present and needs normalization)
    const hasUnnormalizedMicroarray = datasets.some(d => d.dataType === "microarray" && !d.isNormalized && (d.platform?.toLowerCase() === "illumina" || !d.platform));
    if (hasUnnormalizedMicroarray) {
      base.push({ id: "normalization", label: "Microarray Normalization" });
    }
    
    // Annotation (only apply for microarray, readcounts)
    const hasMicroarrayOrReadcounts = datasets.some(d => d.dataType === "microarray" || d.dataType === "readcounts");
    if (hasMicroarrayOrReadcounts) {
      base.push({ id: "annotation", label: "Annotation" });
    }
    
    // Processing (all data type)
    base.push({ id: "processing", label: "Processing" });
    
    // Counts and Proteomics and Others Normalization (dynamically rendering for readcounts, proteomics, others)
    const unnormalizedTypes = new Set(
      datasets
        .filter(d => (d.dataType === "readcounts" || d.dataType === "proteomics" || d.dataType === "others") && !d.isNormalized)
        .map(d => d.dataType)
    );
    if (unnormalizedTypes.size > 0) {
      const typeLabels: string[] = [];
      if (unnormalizedTypes.has("readcounts")) typeLabels.push("Readcounts");
      if (unnormalizedTypes.has("proteomics")) typeLabels.push("Proteomics");
      if (unnormalizedTypes.has("others")) typeLabels.push("Others");
      const normLabel = typeLabels.length === 1
        ? `${typeLabels[0]} Normalization`
        : typeLabels.length === 2
          ? `${typeLabels[0]}/${typeLabels[1]} Normalization`
          : "Counts Normalization";
      base.push({ id: "normalization-counts", label: normLabel });
    }
    
    base.push({ id: "batch", label: "Batch Correction" });
  } else {
    const ds = datasets[0];
    const type = ds?.dataType ?? "readcounts";
    const isUnnormalized = !ds?.isNormalized;
    const isIllumina = ds?.platform?.toLowerCase() === "illumina";

    if (type === "microarray") {
      // single Microarray: Upload > Normalization (only for illumina platform) > Annotation > Processing > Batch Correction > Module Selection > Export
      if (isIllumina && isUnnormalized) {
        base.push({ id: "normalization", label: "Normalization" });
      }
      base.push({ id: "annotation", label: "Annotation" });
      base.push({ id: "processing", label: "Processing" });
      base.push({ id: "batch", label: "Batch Correction" });
    } else if (type === "readcounts") {
      // single Readcounts: Upload > Annotation > Processing > Normalization > Batch Correction > Module Selection > Export
      base.push({ id: "annotation", label: "Annotation" });
      base.push({ id: "processing", label: "Processing" });
      if (isUnnormalized) {
        base.push({ id: "normalization-counts", label: "Normalization" });
      }
      base.push({ id: "batch", label: "Batch Correction" });
    } else if (type === "proteomics") {
      // single Proteomics: Upload > Processing > Normalization > Batch Correction > Module Selection > Export
      base.push({ id: "processing", label: "Processing" });
      if (isUnnormalized) {
        base.push({ id: "normalization-counts", label: "Normalization" });
      }
      base.push({ id: "batch", label: "Batch Correction" });
    } else {
      // single Others: Upload > Processing > Normalization > Batch Correction > Module Selection > Export
      base.push({ id: "processing", label: "Processing" });
      if (isUnnormalized) {
        base.push({ id: "normalization-counts", label: "Normalization" });
      }
      base.push({ id: "batch", label: "Batch Correction" });
    }
  }

  base.push(
    { id: "module-select", label: "Analysis Module" },
    { id: "export", label: "Export" }
  );
  return base;
}

export const DE_STEPS: { id: DEStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "analysis", label: "Analysis" },
  { id: "module-select", label: "Analysis Module" },
  { id: "export", label: "Export" },
];

export const DE_STEPS_MULTI: { id: DEStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "all-datasets", label: "Upload Summary" },
  { id: "analysis", label: "Analysis" },
  { id: "meta", label: "Meta-analysis" },
  { id: "module-select", label: "Analysis Module" },
  { id: "export", label: "Export" },
];

export const FS_STEPS: { id: FSStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "model-selection", label: "Model Selection" },
  { id: "export", label: "Export" },
];

export const FS_STEPS_WITH_CV: { id: FSStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "model-selection", label: "Model Selection" },
  { id: "cross-validation", label: "Cross-Validation" },
  { id: "export", label: "Export" },
];

export const EN_STEPS: { id: EnrichmentStep; label: string }[] = [
  { id: "upload", label: "Data Upload" },
  { id: "analysis", label: "Analysis" },
  { id: "export", label: "Export" },
];

export const DATASET_COLORS = [
  "#3062B8", "#16a34a", "#d97706", "#dc2626", "#7c3aed",
  "#0891b2", "#db2777", "#65a30d", "#ea580c", "#0284c7",
];
