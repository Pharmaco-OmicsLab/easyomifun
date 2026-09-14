import type {
  DatasetConfig,
  ProcessingConfig,
  NormalizationConfig,
  BatchConfig,
  AnnotationConfig,
  DEConfig,
  DEResult,
  MLModel,
  CVMethod,
  EnrichmentConfig,
  MetaMethod
} from "../dataObject";

// Base URL for backend API requests. Defaults to empty string for same-origin or proxied requests;
// an alternate backend URL can be configured via VITE_SERVER_URL.
export const SERVER_URL = (import.meta.env.VITE_SERVER_URL ?? "").replace(/\/$/, "");

export interface AnnotationAPIResponse {
  datasetId: string;
  total: number;
  mapped: number;
  unmapped: number;
  unique: number;
  multi: number;
  retained?: number;
  nSamples?: number;
  columns?: string[];
  parsedData?: any[];
  sampleIds?: string[];
}

export interface ProcessingAPIResponse {
  datasetId: string;
  inputFeatures: number;
  removedFeatures: number;
  retainedFeatures: number;
  missingValuesCount: number;
  nSamples?: number;
  parsedData?: any[];
  columns?: string[];
  sampleIds?: string[];
}

export interface PCAPoint {
  xBefore: number;
  yBefore: number;
  xAfter: number;
  yAfter: number;
  batch: string;
  group: string;
  covariate: string;
}

export interface CVResult {
  model: string;
  bestScore: string;
  best_auc?: number;
  ci?: string;
  ci_lower?: number;
  ci_upper?: number;
  std?: string;
  std_val?: number;
  accuracy: string;
  ppv?: string;
  npv?: string;
  optimization_history?: number[];
}

async function safeFetch<T>(url: string, options: RequestInit): Promise<T> {
  if (options.headers) {
    (options.headers as any)["Content-Type"] = "text/plain";
  } else {
    options.headers = { "Content-Type": "text/plain" };
  }
  try {
    const res = await fetch(url, options);
    if (!res.ok) {
      let errorMsg = `API Error (${res.status})`;
      try {
        const errJson = await res.json();
        const detail = errJson && (errJson.detail ?? errJson.message);
        if (detail) {
          errorMsg = typeof detail === 'string' ? detail : JSON.stringify(detail);
        }
      } catch (_) { }
      throw new Error(errorMsg);
    }
    return await res.json() as T;
  } catch (err: any) {
    console.error(`Fetch error at ${url}:`, err);
    throw new Error(err.message || String(err));
  }
}

// === Data Processing Module ===================================================
export async function fetchMicroarrayPlatformsAPI(): Promise<any> {
  const url = `${SERVER_URL}/api/microarray-platforms`;
  return safeFetch<any>(url, {
    method: "GET"
  });
}

export async function annotateDatasetAPI(
  datasets: DatasetConfig[], 
  config: AnnotationConfig
): Promise<AnnotationAPIResponse[]> {
  const url = `${SERVER_URL}/api/annotation`;
  
  const payload = datasets.map(d => {
    const isOthers = d.dataType === "others";
    const isMicroarray = d.dataType === "microarray";
    
    // Organism: for microarray, use platform-derived organism; for others, use global config
    const organism = isOthers ? "skip"
                   : isMicroarray ? (d.microarrayOrganism || config.selectOrganism)
                   : config.selectOrganism;

    return {
      datasetId: d.id,
      name: d.name,
      dataType: d.dataType || "readcounts",
      strategy: isOthers ? "skip" : (config.multiMappedStrategy || "keep-first"),
      organism,
      biotype: isOthers ? "skip" : config.geneBiotype,
      // New: microarray platform fields
      microarrayPlatformId: isMicroarray ? (d.microarrayPlatformId || "") : "",
      platformFamily: isMicroarray ? (d.platform || "") : "",
      geneIdType: d.submittedGeneIdType || d.geneIdType || "",
    };
  });

  console.log("Outgoing Request Payload to /api/annotation:", JSON.stringify(payload, null, 2));
  
  return submitAndPollJob<AnnotationAPIResponse[]>(url, payload);
}

export async function processDatasetAPI(datasets: DatasetConfig[], config: ProcessingConfig): Promise<any[]> {
  const url = `${SERVER_URL}/api/processing`;

  const payload = datasets.map(d => {
    let useNA = d.hasNA;
    if (!useNA && d.parsedData && d.columns) {
      const headers = d.columns;
      const geneInfoCols = d.geneInfoCols || [];
      const geneIdCol = d.geneIdCol || "";
      const dataColIndices: number[] = [];
      headers.forEach((col, idx) => {
        if (idx !== 0 && col !== geneIdCol && !geneInfoCols.includes(col)) dataColIndices.push(idx);
      });
      for (const row of d.parsedData) {
        for (const idx of dataColIndices) {
          const val = row[idx];
          if (val === undefined || val === null) {
            useNA = true;
            break;
          }
          const trimmed = String(val).trim();
          if (trimmed === "" || trimmed.toLowerCase() === "na" || trimmed.toLowerCase() === "nan" || isNaN(Number(trimmed))) {
            useNA = true;
            break;
          }
        }
        if (useNA) break;
      }
    }
    let filterMethod: any = config.filterMethod;
    let varianceThreshold: any = config.varianceThreshold;

    if (d.dataType === "readcounts") {
      if (d.isNormalized) {
        filterMethod = config.normRcFilterMethod;
        varianceThreshold = config.normRcVariance;
      } else {
        filterMethod = config.rcFilterMethod;
        varianceThreshold = config.rcVariance;
      }
    } else if (d.dataType === "microarray") {
      filterMethod = config.maFilterMethod;
      varianceThreshold = config.maVariance;
    } else if (d.dataType === "proteomics") {
      filterMethod = config.protFilterMethod;
      varianceThreshold = config.protVariance;
    } else if (d.dataType === "others") {
      filterMethod = config.othFilterMethod;
      varianceThreshold = config.othVariance;
    }

    const methodsList = (filterMethod || "").split(",").map((m: any) => m.trim());

    return {
      datasetId: d.id,
      name: d.name,
      filterMethod: filterMethod,
      filterParams: {
        cpmThreshold: methodsList.includes("cpm") ? config.cpmThreshold : null,
        minSamples: methodsList.includes("cpm") ? config.minSamples : null,
        countThreshold: methodsList.includes("min_count") ? config.countThreshold : null,
        varianceThreshold: methodsList.includes("variance") ? varianceThreshold : null,
      },
      missingMethod: useNA ? config.missingMethod : null,
      missingParams: useNA ? { knnK: config.knnK } : null,
      naRemovePercent: useNA ? config.naRemovePercent : null,
      isNA: useNA,
      dataType: d.dataType
    };
  });
  return submitAndPollJob<any[]>(url, payload);
}

export async function normalizeDatasetAPI(datasets: DatasetConfig[], config: NormalizationConfig): Promise<any[]> {
  const url = `${SERVER_URL}/api/normalization`;
  const payload = datasets.map(d => {
    const isAffyMicroarray = d.dataType === "microarray" && d.platform === "affymetrix";
    const isSkip = d.isNormalized === true || isAffyMicroarray;
    return {
      datasetId: d.id,
      name: d.name,
      dataType: d.dataType,
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      platform: d.platform || null,
      method: isSkip ? "none" : config.method,
      logTransform: config.logTransform,
      transformationType: config.transformationType || (config.logTransform ? "log2" : "none"),
      priorCount: config.priorCount
    };
  });
  return submitAndPollJob<any[]>(url, payload);
}

export async function fetchPCAResultsAPI(datasets: DatasetConfig[]): Promise<PCAPoint[]> {
  const url = `${SERVER_URL}/api/pca`;
  const payload = {
    datasets: datasets.map(d => ({
      id: d.id,
      name: d.name,
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || "",
      geneIdCol: d.geneIdCol || "",
      geneInfoCols: d.geneInfoCols || [],
      clinicalOtherCovariates: d.clinicalOtherCovariates || []
    }))
  };

  return submitAndPollJob<PCAPoint[]>(url, payload);
}

export async function runBatchCorrectionAPI(datasets: DatasetConfig[], config: BatchConfig): Promise<PCAPoint[]> {
  const url = `${SERVER_URL}/api/batch-correction`;
  const payload = datasets.map(d => {
    const methodToUse = d.dataType === "readcounts" ? config.method : config.methodOthers;
    const finalMethod = (methodToUse && d.clinicalBatchCol) ? methodToUse : "skip";
    return {
      datasetId: d.id,
      name: d.name,
      method: finalMethod,
      dataType: d.dataType,
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || "",
      clinicalOtherCovariates: d.clinicalOtherCovariates || [],
      positiveClass: d.fs_positiveClass || d.positiveClass || "",
      negativeClass: d.fs_negativeClass || d.negativeClass || ""
    };
  });
  return submitAndPollJob<PCAPoint[]>(url, payload);
}

export interface TrainingResponse {
  features: { gene: string, importance: number, rank: number }[];
  accuracies: Record<string, number>;
  loss_histories: Record<string, number[]>;
}

export interface FSProgress {
  jobId: string;
  elapsed: number;
  current?: number;
  total?: number;
  model?: string;
}

/** Cancel a running feature-selection job (standalone or inline). Best-effort. */
export async function cancelFSJobAPI(jobId: string, inline = false): Promise<void> {
  const path = inline ? "inline-fs-job" : "feature-selection-job";
  const url = `${SERVER_URL}/api/${path}?id=${encodeURIComponent(jobId)}`;
  try {
    await safeFetch(url, { method: "DELETE", headers: {} });
  } catch (_) { /* best-effort cancel */ }
}

/**
 * Poll an EXISTING feature-selection job (no submit) until it finishes. Used to resume
 * a job after a page reload — the background job keeps running server-side, so we just
 * re-attach and poll by its jobId.
 */
export async function pollFSJobAPI(
  jobId: string,
  inline: boolean,
  onProgress?: (info: FSProgress) => void
): Promise<TrainingResponse> {
  const path = inline ? "inline-fs-job" : "feature-selection-job";
  const pollUrl = `${SERVER_URL}/api/${path}?id=${encodeURIComponent(jobId)}`;
  const startedAt = Date.now();
  const MAX_MS = 60 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const res = await safeFetch<{ status: string; result?: TrainingResponse; message?: string; elapsed?: number; progress?: { current?: number; total?: number; model?: string } }>(
      pollUrl,
      { method: "GET", headers: {} }
    );
    if (res.status === "done") {
      if (!res.result) throw new Error("Feature selection finished but returned no result.");
      return res.result;
    }
    if (res.status === "error") throw new Error(res.message || "Feature selection job failed.");
    if (onProgress) {
      onProgress({
        jobId,
        elapsed: res.elapsed ?? Math.floor((Date.now() - startedAt) / 1000),
        current: res.progress?.current,
        total: res.progress?.total,
        model: res.progress?.model
      });
    }
    if (Date.now() - startedAt > MAX_MS) throw new Error("Feature selection timed out.");
    attempt++;
    const pollMs = attempt <= 1 ? 200 : (attempt === 2 ? 400 : (attempt === 3 ? 800 : (attempt === 4 ? 1200 : 1500)));
    await new Promise(r => setTimeout(r, pollMs));
  }
}

export async function runFeatureSelectionAPI(
  datasets: DatasetConfig[],
  models: MLModel[],
  splitRatio: number,
  parameters?: Record<string, Record<string, string | number | null>>,
  maxFeaturesSelect?: number,
  onProgress?: (info: FSProgress) => void,
  multiDatasetMode: "individual" | "combine" = "combine"
): Promise<TrainingResponse> {
  const modelsPayload: Record<string, { model: string; parameters: Record<string, string | number> }> = {};
  models.forEach(m => {
    modelsPayload[m] = {
      model: m,
      parameters: parameters && parameters[m] ? parameters[m] : {}
    };
  });

  const payload = {
    datasets: datasets.map(d => ({
      id: d.id,
      name: d.name,
      dataType: d.dataType,
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      fs_isCV: d.fs_isCV ?? false,
      fs_isInternalValidation: d.fs_isInternalValidation ?? true,
      maxFeaturesSelect: maxFeaturesSelect ?? "",
      datasetPurpose: d.fs_datasetPurpose || "train-and-test",
      validationStrategy: d.fs_validationStrategy || "train-test-split",
      trainRatio: d.fs_trainRatio ?? splitRatio,
      enabledTargetPrevalence: d.fs_enabledTargetPrevalence ?? false,
      targetPrevalence: d.fs_enabledTargetPrevalence && d.fs_targetPrevalence !== undefined ? d.fs_targetPrevalence : null,
      positiveClass: d.fs_positiveClass || d.positiveClass || "",
      negativeClass: d.fs_negativeClass || d.negativeClass || "",
      fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
      fs_negativeClass: d.fs_negativeClass || d.negativeClass || ""
    })),
    models: modelsPayload,
    splitRatio,
    maxFeaturesSelect: maxFeaturesSelect ?? "",
    multiDatasetMode
  };

  console.log("Outgoing Request Payload to /api/feature-selection-job:", JSON.stringify(payload, null, 2));

  // 1. Submit the job
  const submit = await safeFetch<{ jobId: string; status: string }>(
    `${SERVER_URL}/api/feature-selection-job`,
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    }
  );

  // 2. Poll status until complete or failed (1-hour timeout limit)
  const pollUrl = `${SERVER_URL}/api/feature-selection-job?id=${encodeURIComponent(submit.jobId)}`;
  const startedAt = Date.now();
  const MAX_MS = 60 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const res = await safeFetch<{ status: string; result?: TrainingResponse; message?: string; elapsed?: number; progress?: { current?: number; total?: number; model?: string } }>(
      pollUrl,
      { method: "GET", headers: {} }
    );

    if (res.status === "done") {
      if (!res.result) throw new Error("Feature selection finished but returned no result.");
      const finalResult = res.result as any;
      finalResult.jobId = submit.jobId;
      return finalResult;
    }

    if (res.status === "error") {
      throw new Error(res.message || "Feature selection job failed.");
    }

    if (onProgress) {
      onProgress({
        jobId: submit.jobId,
        elapsed: res.elapsed ?? Math.floor((Date.now() - startedAt) / 1000),
        current: res.progress?.current,
        total: res.progress?.total,
        model: res.progress?.model
      });
    }

    if (Date.now() - startedAt > MAX_MS) {
      throw new Error("Feature selection timed out.");
    }

    attempt++;
    const pollMs = attempt <= 1 ? 200 : (attempt === 2 ? 400 : (attempt === 3 ? 800 : (attempt === 4 ? 1200 : 1500)));
    await new Promise(r => setTimeout(r, pollMs));
  }
}

export async function runInlineFSAPI(
  datasets: DatasetConfig[],
  models: MLModel[],
  splitRatio: number,
  parameters?: Record<string, Record<string, string | number | null>>,
  maxFeaturesSelect?: number,
  onProgress?: (info: FSProgress) => void,
  multiDatasetMode: "individual" | "combine" = "combine"
): Promise<TrainingResponse> {
  const modelsPayload: Record<string, { model: string; parameters: Record<string, string | number> }> = {};
  models.forEach(m => {
    modelsPayload[m] = {
      model: m,
      parameters: parameters && parameters[m] ? parameters[m] : {}
    };
  });

  // Build a minimal payload — the backend reads expression + clinical data
  // from the inline FS stack (output of the DP pipeline), so no raw data is sent.
  const payload = {
    datasets: datasets.map(d => ({
      datasetId: d.id,
      name: d.name,
      dataType: d.dataType,
      trainRatio: d.fs_trainRatio ?? splitRatio,
      datasetPurpose: d.fs_datasetPurpose || "train-and-test",
      validationStrategy: d.fs_validationStrategy || "train-test-split",
      isInternalValidation: d.fs_isInternalValidation ?? true,
      maxFeaturesSelect: maxFeaturesSelect ?? "",
      enabledTargetPrevalence: d.fs_enabledTargetPrevalence ?? false,
      targetPrevalence: d.fs_enabledTargetPrevalence && d.fs_targetPrevalence !== undefined ? d.fs_targetPrevalence : null,
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || "",
      positiveClass: d.fs_positiveClass || d.positiveClass || "",
      negativeClass: d.fs_negativeClass || d.negativeClass || "",
      fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
      fs_negativeClass: d.fs_negativeClass || d.negativeClass || ""
    })),
    models: modelsPayload,
    splitRatio,
    maxFeaturesSelect: maxFeaturesSelect ?? "",
    multiDatasetMode
  };

  console.log("Outgoing Request Payload to /api/inline-fs-job:", JSON.stringify(payload, null, 2));

  // 1. Submit the job
  const submit = await safeFetch<{ jobId: string; status: string }>(
    `${SERVER_URL}/api/inline-fs-job`,
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    }
  );

  // 2. Poll until done or error (1-hour timeout)
  const pollUrl = `${SERVER_URL}/api/inline-fs-job?id=${encodeURIComponent(submit.jobId)}`;
  const startedAt = Date.now();
  const MAX_MS = 60 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const res = await safeFetch<{ status: string; result?: TrainingResponse; message?: string; elapsed?: number; progress?: { current?: number; total?: number; model?: string } }>(
      pollUrl,
      { method: "GET", headers: {} }
    );

    if (res.status === "done") {
      if (!res.result) throw new Error("Inline feature selection finished but returned no result.");
      const finalResult = res.result as any;
      finalResult.jobId = submit.jobId;
      return finalResult;
    }

    if (res.status === "error") {
      throw new Error(res.message || "Inline feature selection job failed.");
    }

    if (onProgress) {
      onProgress({
        jobId: submit.jobId,
        elapsed: res.elapsed ?? Math.floor((Date.now() - startedAt) / 1000),
        current: res.progress?.current,
        total: res.progress?.total,
        model: res.progress?.model
      });
    }

    if (Date.now() - startedAt > MAX_MS) {
      throw new Error("Inline feature selection timed out.");
    }

    attempt++;
    const pollMs = attempt <= 1 ? 200 : (attempt === 2 ? 400 : (attempt === 3 ? 800 : (attempt === 4 ? 1200 : 1500)));
    await new Promise(r => setTimeout(r, pollMs));
  }
}

// ─── Inline Analysis APIs ─────────────────────────────────────────────────────
export async function runInlineDEAPI(
  datasets: DatasetConfig[],
  config: DEConfig
): Promise<Record<string, DEResult[]>> {
  const url = `${SERVER_URL}/api/inline-de`;
  const payload = datasets.map(d => {
    const isNormalizedOrLimma = d.dataType === "microarray" || d.dataType === "proteomics" || d.dataType === "others" || d.isNormalized;
    const datasetMethod = isNormalizedOrLimma ? "limma" : (config.method || "deseq2");

    return {
      datasetId: d.id,
      name: d.name,
      dataType: d.dataType,
      isNormalized: d.isNormalized,
      isInline: true,
      parentModule: d.parentModule || "dp",
      parentDatasetId: d.parentDatasetId || undefined,
      method: datasetMethod,
      pValueThreshold: config.pValueThreshold,
      logFcThreshold: config.logFcThreshold,
      adjustMethod: config.adjustMethod,
      referenceGroup: d.de_referenceGroup || d.referenceGroup || "",
      comparisonGroup: d.de_comparisonGroup || d.comparisonGroup || "",
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || ""
    };
  });
  console.log("Outgoing Request Payload to /api/inline-de:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<Record<string, DEResult[]>>(url, payload);
}

export async function runInlineDEMetaAPI(
  method: MetaMethod | "" | null,
  params: {
    pvalueMethod?: "fisher" | "stouffer" | "" | null;
    effectSizeModel?: "fixed" | "random" | "" | null;
    votes?: number | string | null;
    pValueThreshold?: number | string | null;
    logFcThreshold?: number | string | null;
  } = {},
  datasets: DatasetConfig[] = []
): Promise<any> {
  const url = `${SERVER_URL}/api/inline-de-meta`;
  const payload = datasets.map(d => ({
    datasetId: d.id,
    name: d.name,
    method: method || "",
    isInline: true,
    parentModule: d.parentModule || "dp",
    parentDatasetId: d.parentDatasetId || undefined,
    pvalueMethod: params.pvalueMethod ?? "",
    effectSizeModel: params.effectSizeModel ?? "",
    votes: params.votes ?? "",
    pValueThreshold: params.pValueThreshold ?? "",
    logFcThreshold: params.logFcThreshold ?? "",
    clinicalSampleIdCol: d.clinicalSampleIdCol || "",
    clinicalGroupCol: d.clinicalGroupCol || "",
    clinicalBatchCol: d.clinicalBatchCol || "",
    de_referenceGroup: d.de_referenceGroup || d.referenceGroup || "",
    de_comparisonGroup: d.de_comparisonGroup || d.comparisonGroup || ""
  }));
  console.log("Outgoing Request Payload to /api/inline-de-meta:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<any>(url, payload);
}



export async function refitFeaturesAPI(
  jobId: string,
  selectionMethod: "breakoff" | "percentage" | "max_features" | "overlap",
  percentageValue?: number | null | string,
  maxFeaturesValue?: number | null | string
): Promise<TrainingResponse> {
  const url = `${SERVER_URL}/api/refit-features`;
  const isPct = selectionMethod === "percentage";
  const isMax = selectionMethod === "max_features";

  const pctToSend = isPct ? (percentageValue !== "" && percentageValue != null ? Number(percentageValue) : 80) : null;
  const maxToSend = isMax ? (maxFeaturesValue !== "" && maxFeaturesValue != null ? Number(maxFeaturesValue) : 10) : null;

  const payload = {
    jobId,
    selectionMethod,
    percentageValue: pctToSend,
    maxFeaturesValue: maxToSend
  };
  return submitAndPollJob<TrainingResponse>(url, payload);
}

export async function runInlineEAAPI(
  config: EnrichmentConfig,
  source: "inline_de" | "inline_de_meta" | "de" | "meta" | "auto",
  geneIdCol: string,
  datasetId?: string,
  geneIdType?: string,
  direction?: string,
  datasets?: { id: string; dataType?: string; name?: string; parentModule?: string; module?: string; isInline?: boolean }[]
): Promise<{
  oraResults?: any[];
  gseaResults?: any[];
  unmappedGenes?: string[];
  totalInputGenes?: number;
  mappedGenesCount?: number;
}> {
  const url = `${SERVER_URL}/api/inline-ea`;
  const methods = config.methods;
  const inferredParentModule = (config as any).parentModule ||
    ((config as any).module === "dp" || (datasetId && datasetId.includes("_dp")) || source === "inline_de" || source === "inline_de_meta" ? "dp" : "de");
  const payload = [{
    methods: methods,
    oraDatabase: methods.includes("ora") ? (config.oraDatabase || []) : [],
    gseaDatabase: methods.includes("gsea") ? (config.gseaDatabase || []) : [],
    rankByCol: config.rankByCol || "LogFC",
    pValueCutoff: config.pValueCutoff,
    qValueCutoff: config.qValueCutoff,
    minGeneSetSize: config.minGeneSetSize,
    maxGeneSetSize: config.maxGeneSetSize,
    organism: config.organism,
    source,
    geneIdCol,
    datasetId: datasetId || "",
    geneIdType: geneIdType || "",
    direction: direction,
    dataType: (config as any).dataType || (config as any).dataClass || "",
    dataClass: (config as any).dataClass || "",
    module: "ea",
    parentModule: inferredParentModule,
    isInline: true,
    datasets: (datasets || []).map(d => ({
      ...d,
      parentModule: d.parentModule || inferredParentModule,
      module: "ea",
      isInline: true
    }))
  }];
  console.log("Outgoing Request Payload to /api/inline-ea:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<{
    oraResults?: any[];
    gseaResults?: any[];
    unmappedGenes?: string[];
    totalInputGenes?: number;
    mappedGenesCount?: number;
  }>(url, payload);
}

// ─── General purposes APIs ─────────────────────────────────────────────────────
export async function redoStepAPI(step: string, datasetIds: string[]): Promise<any> {
  const url = `${SERVER_URL}/api/redo`;
  const payload = { step, datasetIds };
  return submitAndPollJob<any>(url, payload);
}

export async function clearDownstreamAPI(step: string, datasetIds: string[]): Promise<any> {
  const url = `${SERVER_URL}/api/clear-downstream`;
  return safeFetch<any>(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ step, datasetIds })
  });
}

export async function skipStepAPI(step: string, datasetIds: string[]): Promise<any> {
  const url = `${SERVER_URL}/api/skip`;
  const payload = { step, datasetIds };
  return safeFetch<any>(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload)
  });
}

export async function clearAllResultsAPI(): Promise<any> {
  const userId = sessionStorage.getItem("easyomifun_user_id") || "";
  const url = `${SERVER_URL}/api/cleanup?userId=${userId}`;
  return safeFetch<any>(url, {
    method: "POST"
  });
}

export async function uploadRawDatasetDataAPI(datasets: DatasetConfig[]): Promise<any> {
  const url = `${SERVER_URL}/api/upload-datasets`;
  const payload = {
    datasets: datasets.map(d => {
      const exprUnchanged = d.expressionFileName && d.expressionFileName === d.submittedExpressionFileName;
      const clinUnchanged = d.clinicalFileName && d.clinicalFileName === d.submittedClinicalFileName;
      return {
        datasetId: d.id || "",
        datasetName: d.name || "",
        parentModule: d.parentModule || "dp",
        isInline: d.isInline ?? false,
        parentDatasetId: d.parentDatasetId || undefined,
        // Direct local file path (Desktop Electron mode only)
        expressionFilePath: exprUnchanged ? undefined : d.expressionFilePath,
        clinicalFilePath: clinUnchanged ? undefined : d.clinicalFilePath,
        // Chunk upload ID (Web mode chunked upload)
        expressionUploadId: exprUnchanged ? undefined : d.expressionUploadId,
        clinicalUploadId: clinUnchanged ? undefined : d.clinicalUploadId,
        // expression data
        expressionColumns: d.columns || [],
        expressionParsedData: exprUnchanged ? [] : (d.expressionFilePath || d.expressionUploadId ? [] : (d.expressionRawText ? [] : (d.parsedData || []))),
        expressionRawText: exprUnchanged ? "" : (d.expressionFilePath || d.expressionUploadId ? "" : (d.expressionRawText || "")),
        isNormalized: d.isNormalized !== undefined ? d.isNormalized : false,
        dataType: d.dataType || "",
        platform: d.platform || "",
        geneInfoCols: d.geneInfoCols || [],
        geneIdCol: d.geneIdCol || "",
        geneIdType: d.geneIdType || "",
        // clinical data
        clinicalColumns: d.clinicalColumns || [],
        clinicalParsedData: clinUnchanged ? [] : (d.clinicalFilePath || d.clinicalUploadId ? [] : (d.clinicalRawText ? [] : (d.clinicalParsedData || []))),
        clinicalRawText: clinUnchanged ? "" : (d.clinicalFilePath || d.clinicalUploadId ? "" : (d.clinicalRawText || "")),
        sampleIdCol: d.clinicalSampleIdCol || "",
        groupCol: d.clinicalGroupCol || "",
        batchCol: d.clinicalBatchCol || "",
        otherCovariates: d.clinicalOtherCovariates || [],
        referenceGroup: d.de_referenceGroup || "",
        comparisonGroup: d.de_comparisonGroup || "",
        positiveClass: d.fs_positiveClass || d.positiveClass || "",
        negativeClass: d.fs_negativeClass || d.negativeClass || "",
        fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
        fs_negativeClass: d.fs_negativeClass || d.negativeClass || ""
      };
    })
  };
  return submitAndPollJob<any>(url, payload);
}

export async function uploadDEDatasetDataAPI(datasets: DatasetConfig[]): Promise<any> {
  const url = `${SERVER_URL}/api/upload-datasets`;
  const payload = {
    datasets: datasets.map(d => {
      const exprUnchanged = d.expressionFileName && d.expressionFileName === d.submittedExpressionFileName;
      const clinUnchanged = d.clinicalFileName && d.clinicalFileName === d.submittedClinicalFileName;
      return {
        datasetId: d.id || "",
        datasetName: d.name || "",
        parentModule: d.parentModule || "de",
        isInline: d.isInline ?? false,
        parentDatasetId: d.parentDatasetId || undefined,
        // Direct local file path (Desktop Electron mode only)
        expressionFilePath: exprUnchanged ? undefined : d.expressionFilePath,
        clinicalFilePath: clinUnchanged ? undefined : d.clinicalFilePath,
        // Chunk upload ID (Web mode chunked upload)
        expressionUploadId: exprUnchanged ? undefined : d.expressionUploadId,
        clinicalUploadId: clinUnchanged ? undefined : d.clinicalUploadId,
        // expression data
        expressionColumns: d.columns || [],
        expressionParsedData: exprUnchanged ? [] : (d.expressionFilePath || d.expressionUploadId ? [] : (d.expressionRawText ? [] : (d.parsedData || []))),
        expressionRawText: exprUnchanged ? "" : (d.expressionFilePath || d.expressionUploadId ? "" : (d.expressionRawText || "")),
        isNormalized: d.isNormalized !== undefined ? d.isNormalized : false,
        dataType: d.dataType || "",
        platform: d.platform || "",
        geneInfoCols: d.geneInfoCols || [],
        geneIdCol: d.geneIdCol || "",
        geneIdType: d.geneIdType || "",
        // clinical data
        clinicalColumns: d.clinicalColumns || [],
        clinicalParsedData: clinUnchanged ? [] : (d.clinicalFilePath || d.clinicalUploadId ? [] : (d.clinicalRawText ? [] : (d.clinicalParsedData || []))),
        clinicalRawText: clinUnchanged ? "" : (d.clinicalFilePath || d.clinicalUploadId ? "" : (d.clinicalRawText || "")),
        sampleIdCol: d.clinicalSampleIdCol || "",
        groupCol: d.clinicalGroupCol || "",
        batchCol: d.clinicalBatchCol || "",
        otherCovariates: d.clinicalOtherCovariates || [],
        referenceGroup: d.de_referenceGroup || "",
        comparisonGroup: d.de_comparisonGroup || ""
      };
    })
  };
  return submitAndPollJob<any>(url, payload);
}

export async function clearDatasetAPI(datasetId: string, scope: "expression" | "clinical"): Promise<any> {
  const url = `${SERVER_URL}/api/dataset/${datasetId}/${scope}`;
  return safeFetch<any>(url, {
    method: "DELETE"
  });
}

// Used by InlineUploadStep
export async function supplementClinicalDataAPI(
  datasetId: string,
  clinicalColumns: string[],
  clinicalParsedData: any[][],
  clinicalSampleIdCol: string,
  clinicalGroupCol: string,
  clinicalBatchCol: string,
  clinicalOtherCovariates: string[],
  clinicalRawText?: string,
  clinicalFilePath?: string,
  clinicalUploadId?: string,
  positiveClass?: string,
  negativeClass?: string
): Promise<any> {
  const url = `${SERVER_URL}/api/supplement-clinical`;
  const payload = {
    datasetId: datasetId || "",
    clinicalFilePath: clinicalFilePath || undefined,
    clinicalUploadId: clinicalUploadId || undefined,
    clinicalColumns: clinicalColumns || [],
    clinicalParsedData: (clinicalFilePath || clinicalUploadId || clinicalRawText) ? [] : (clinicalParsedData || []),
    clinicalRawText: (clinicalFilePath || clinicalUploadId) ? "" : (clinicalRawText || ""),
    clinicalSampleIdCol: clinicalSampleIdCol || "",
    clinicalGroupCol: clinicalGroupCol || "",
    clinicalBatchCol: clinicalBatchCol || "",
    clinicalOtherCovariates: clinicalOtherCovariates || [],
    positiveClass: positiveClass || "",
    negativeClass: negativeClass || "",
    fs_positiveClass: positiveClass || "",
    fs_negativeClass: negativeClass || ""
  };
  console.log("Outgoing Request Payload to /api/supplement-clinical:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<any>(url, payload);
}

export async function uploadFSDatasetsAPI(datasets: DatasetConfig[]): Promise<any> {
  const url = `${SERVER_URL}/api/upload-datasets`;
  const payload = {
    datasets: datasets.map(d => {
      const exprUnchanged = d.expressionFileName && d.expressionFileName === d.submittedExpressionFileName;
      const clinUnchanged = d.clinicalFileName && d.clinicalFileName === d.submittedClinicalFileName;
      return {
        datasetId: d.id || "",
        datasetName: d.name || "",
        parentModule: d.parentModule || "fs",
        isInline: d.isInline ?? false,
        parentDatasetId: d.parentDatasetId || undefined,
        // Direct local file path (Desktop Electron mode only)
        expressionFilePath: exprUnchanged ? undefined : d.expressionFilePath,
        clinicalFilePath: clinUnchanged ? undefined : d.clinicalFilePath,
        // Chunk upload ID (Web mode chunked upload)
        expressionUploadId: exprUnchanged ? undefined : d.expressionUploadId,
        clinicalUploadId: clinUnchanged ? undefined : d.clinicalUploadId,
        // expression data
        expressionColumns: d.columns || [],
        expressionParsedData: exprUnchanged ? [] : (d.expressionFilePath || d.expressionUploadId ? [] : (d.expressionRawText ? [] : (d.parsedData || []))),
        expressionRawText: exprUnchanged ? "" : (d.expressionFilePath || d.expressionUploadId ? "" : (d.expressionRawText || "")),
        isNormalized: d.isNormalized !== undefined ? d.isNormalized : false,
        dataType: d.dataType || "",
        platform: d.platform || "",
        geneInfoCols: d.geneInfoCols || [],
        geneIdCol: d.geneIdCol || "",
        geneIdType: d.geneIdType || "",
        // clinical data
        clinicalColumns: d.clinicalColumns || [],
        clinicalParsedData: clinUnchanged ? [] : (d.clinicalFilePath || d.clinicalUploadId ? [] : (d.clinicalRawText ? [] : (d.clinicalParsedData || []))),
        clinicalRawText: clinUnchanged ? "" : (d.clinicalFilePath || d.clinicalUploadId ? "" : (d.clinicalRawText || "")),
        sampleIdCol: d.clinicalSampleIdCol || "",
        groupCol: d.clinicalGroupCol || "",
        batchCol: d.clinicalBatchCol || "",
        otherCovariates: d.clinicalOtherCovariates || [],
        referenceGroup: d.de_referenceGroup || "",
        comparisonGroup: d.de_comparisonGroup || "",
        positiveClass: d.fs_positiveClass || d.positiveClass || "",
        negativeClass: d.fs_negativeClass || d.negativeClass || "",
        fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
        fs_negativeClass: d.fs_negativeClass || d.negativeClass || "",
        // FS-specific parameters
        featureOrientation: d.fs_featuresOrientation || "",
        featureIndexValue: d.fs_featureIndexValue !== undefined ? d.fs_featureIndexValue : 0,
        datasetPurpose: d.fs_datasetPurpose || "",
        isInternalValidation: d.fs_isInternalValidation !== undefined ? d.fs_isInternalValidation : false,
        enabledTargetPrevalence: d.fs_enabledTargetPrevalence ?? false,
        targetPrevalence: d.fs_enabledTargetPrevalence && d.fs_targetPrevalence !== undefined ? d.fs_targetPrevalence : null
      };
    })
  };
  return submitAndPollJob<any>(url, payload);
}

export async function uploadEADatasetDataAPI(datasets: DatasetConfig[]): Promise<any> {
  const url = `${SERVER_URL}/api/upload-datasets`;
  const payload = {
    datasets: datasets.map(d => {
      const exprUnchanged = d.expressionFileName && d.expressionFileName === d.submittedExpressionFileName;
      return {
        datasetId: d.id || "",
        datasetName: d.name || "",
        parentModule: d.parentModule || "ea",
        isInline: d.isInline ?? false,
        parentDatasetId: d.parentDatasetId || undefined,
        // Direct local file path (Desktop Electron mode only)
        expressionFilePath: exprUnchanged ? undefined : d.expressionFilePath,
        // Chunk upload ID (Web mode chunked upload)
        expressionUploadId: exprUnchanged ? undefined : d.expressionUploadId,
        // expression / gene list data
        expressionColumns: d.columns || [],
        expressionParsedData: exprUnchanged ? [] : (d.expressionFilePath || d.expressionUploadId ? [] : (d.expressionRawText ? [] : (d.parsedData || []))),
        expressionRawText: exprUnchanged ? "" : (d.expressionFilePath || d.expressionUploadId ? "" : (d.expressionRawText || "")),
        isNormalized: false,
        dataType: d.dataType || "",
        platform: d.platform || "",
        geneInfoCols: d.geneInfoCols || [],
        geneIdCol: d.geneIdCol || "",
        geneIdType: d.geneIdType || "",
        clinicalColumns: [],
        clinicalParsedData: [],
        clinicalRawText: "",
        sampleIdCol: "",
        groupCol: "",
        batchCol: "",
        otherCovariates: [],
        referenceGroup: "",
        comparisonGroup: ""
      };
    })
  };
  return submitAndPollJob<any>(url, payload);
}

export interface DatasetInfoItem {
  datasetId: string;
  baseId: string;
  module: string;
  parentModule: string;
  isInline: boolean;
  nSamples: number;
  nFeatures: number;
  columns: string[];
  sampleIds: string[];
  matchingSamples: string[];
  missingClinSamples: string[];
  clinicalNoExprSamples: string[];
  groups: string[];
  clinicalColumns: string[];
  dataType: string;
  isNormalized: boolean;
}

export interface DatasetInfoResponse {
  status: string;
  datasets: Record<string, DatasetInfoItem>;
  sharedFeaturesCount: number;
  sharedFeatures?: string[] | null;
}

export async function fetchDatasetInfoAPI(
  datasetIds: string[],
  module = "dp"
): Promise<DatasetInfoResponse> {
  const url = `${SERVER_URL}/api/dataset-info`;
  return safeFetch<DatasetInfoResponse>(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ datasetIds, module })
  });
}

export interface SampleDataResponse {
  columns: string[];
  parsedData: string[][];
}
export async function fetchSampleDataAPI(
  type: "expression" | "clinical" | "enrichment" | "gene_list" | "gene_list_logfc"
): Promise<SampleDataResponse> {
  const url = `${SERVER_URL}/api/sample-data`;
  return safeFetch<SampleDataResponse>(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ type })
  });
}

export interface AlignSamplesResponse {
  commonSamples: string[];
  missingClinicalSamples: string[];
  missingExpressionSamples: string[];
  nFeatures?: number;
  detectedGeneIdType?: string;
  detectedIsNormalized?: boolean;
  hasMissingClinicalValues?: boolean;
  removedSamplesDueToMissing?: string[];
  minGroupSampleSize?: number;
}
/**
 * Downloads an analysis report (PDF or Markdown).
 * For PDF, triggers async generation via POST /api/export-report and polls until complete.
 * For MD, directly fetches the markdown text.
 */
export async function downloadReportAPI(
  userId: string,
  module: string,
  format: "md" | "pdf",
  onProgress?: (info: { elapsed: number }) => void
): Promise<{ blob: Blob; filename: string }> {
  if (format === "md") {
    const url = `${SERVER_URL}/api/export-report?userId=${encodeURIComponent(userId)}&module=${encodeURIComponent(module)}&format=md`;
    const res = await fetch(url);
    if (!res.ok) {
      let msg = `Failed to download report (status ${res.status})`;
      try {
        const errJson = await res.json();
        if (errJson && errJson.message) msg = errJson.message;
      } catch (_) {}
      throw new Error(msg);
    }
    const blob = await res.blob();
    const disposition = res.headers.get("Content-Disposition") || "";
    const match = disposition.match(/filename="?([^"]+)"?/);
    const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
    const filename = match ? match[1] : `${datePrefix}_analysis_report_${module}.md`;
    return { blob, filename };
  }

  // PDF async job submit + poll
  const submitRes = await safeFetch<{ jobId: string; status: string; downloadUrl?: string }>(
    `${SERVER_URL}/api/export-report`,
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ userId, module, format: "pdf" }),
    }
  );

  if (submitRes.status === "done" && submitRes.downloadUrl) {
    const res = await fetch(`${SERVER_URL}${submitRes.downloadUrl}`);
    const blob = await res.blob();
    const disposition = res.headers.get("Content-Disposition") || "";
    const match = disposition.match(/filename="?([^"]+)"?/);
    const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
    return { blob, filename: match ? match[1] : `${datePrefix}_analysis_report_${module}.pdf` };
  }

  const jobId = submitRes.jobId;
  const startedAt = Date.now();
  const MAX_MS = 10 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const pollUrl = `${SERVER_URL}/api/export-report?id=${encodeURIComponent(jobId)}`;
    const res = await fetch(pollUrl);

    const contentType = res.headers.get("Content-Type") || "";
    if (contentType.includes("application/pdf")) {
      const blob = await res.blob();
      const disposition = res.headers.get("Content-Disposition") || "";
      const match = disposition.match(/filename="?([^"]+)"?/);
      const datePrefix = new Date().toISOString().slice(2, 10).replace(/-/g, "");
      const filename = match ? match[1] : `${datePrefix}_analysis_report_${module}.pdf`;
      return { blob, filename };
    }

    let json: any = null;
    try {
      json = await res.json();
    } catch (_) {}

    if (json && json.status === "error") {
      throw new Error(json.message || "PDF report compilation failed.");
    }

    if (Date.now() - startedAt > MAX_MS) {
      throw new Error("PDF report compilation timed out.");
    }

    attempt++;
    if (onProgress) {
      onProgress({ elapsed: Math.floor((Date.now() - startedAt) / 1000) });
    }

    const delayMs = attempt <= 2 ? 500 : (attempt <= 5 ? 1000 : 1500);
    await new Promise((r) => setTimeout(r, delayMs));
  }
}

// === Background compute-job (meta / de) ==========================
// Generic submit+poll against the additive /api/compute-job endpoint. Mirrors the
// feature-selection job loop; the backend runs the heavy compute in a worker process
// so the single-threaded server stays responsive to other requests while it runs.
// The result shape is identical to what the old synchronous /api/de and
// /api/meta-analysis routes returned, so callers consume it unchanged.

/** Cancel a running compute-job (meta / de). Best-effort. */
export async function cancelComputeJobAPI(jobId: string): Promise<void> {
  const url = `${SERVER_URL}/api/compute-job?id=${encodeURIComponent(jobId)}`;
  try {
    await safeFetch(url, { method: "DELETE", headers: {} });
  } catch (_) { /* best-effort cancel */ }
}

async function submitAndPollJob<T>(
  url: string,
  payload: unknown,
  onProgress?: (info: { jobId: string; elapsed: number }) => void
): Promise<T> {
  // 1. Submit the job
  const submit = await safeFetch<{ jobId: string; status: string }>(
    url,
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    }
  );

  // 2. Poll status until complete or failed (1-hour timeout limit)
  const pollUrl = `${SERVER_URL}/api/compute-job?id=${encodeURIComponent(submit.jobId)}`;
  const startedAt = Date.now();
  const MAX_MS = 60 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const res = await safeFetch<{ status: string; result?: T; message?: string; elapsed?: number }>(
      pollUrl,
      { method: "GET", headers: {} }
    );
    if (res.status === "done") {
      if (res.result === undefined || res.result === null) {
        throw new Error(`Job finished but returned no result.`);
      }
      return res.result;
    }
    if (res.status === "error") {
      throw new Error(res.message || `Job failed.`);
    }
    if (onProgress) {
      onProgress({
        jobId: submit.jobId,
        elapsed: res.elapsed ?? Math.floor((Date.now() - startedAt) / 1000)
      });
    }
    if (Date.now() - startedAt > MAX_MS) {
      throw new Error(`Job timed out.`);
    }
    attempt++;
    const pollMs = attempt <= 1 ? 200 : (attempt === 2 ? 400 : (attempt === 3 ? 800 : (attempt === 4 ? 1200 : 1500)));
    await new Promise(r => setTimeout(r, pollMs));
  }
}

async function runComputeJob<T>(
  kind: "meta" | "de",
  payload: unknown,
  onProgress?: (info: { jobId: string; elapsed: number }) => void
): Promise<T> {
  // 1. Submit the job
  const submit = await safeFetch<{ jobId: string; status: string }>(
    `${SERVER_URL}/api/compute-job`,
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ kind, payload })
    }
  );

  // 2. Poll status until complete or failed (1-hour timeout limit)
  const pollUrl = `${SERVER_URL}/api/compute-job?id=${encodeURIComponent(submit.jobId)}`;
  const startedAt = Date.now();
  const MAX_MS = 60 * 60 * 1000;
  let attempt = 0;

  // eslint-disable-next-line no-constant-condition
  while (true) {
    const res = await safeFetch<{ status: string; result?: T; message?: string; elapsed?: number }>(
      pollUrl,
      { method: "GET", headers: {} }
    );
    if (res.status === "done") {
      if (res.result === undefined || res.result === null) {
        throw new Error(`${kind} job finished but returned no result.`);
      }
      return res.result;
    }
    if (res.status === "error") {
      throw new Error(res.message || `${kind} job failed.`);
    }
    if (onProgress) {
      onProgress({
        jobId: submit.jobId,
        elapsed: res.elapsed ?? Math.floor((Date.now() - startedAt) / 1000)
      });
    }
    if (Date.now() - startedAt > MAX_MS) {
      throw new Error(`${kind} job timed out.`);
    }
    attempt++;
    const pollMs = attempt <= 1 ? 200 : (attempt === 2 ? 400 : (attempt === 3 ? 800 : (attempt === 4 ? 1200 : 1500)));
    await new Promise(r => setTimeout(r, pollMs));
  }
}

// === DE Module ===================================================
export async function runDifferentialExpressionAPI(
  datasets: DatasetConfig[],
  config: DEConfig,
  onProgress?: (info: { jobId: string; elapsed: number }) => void
): Promise<Record<string, DEResult[]>> {
  const payload = datasets.map(d => {
    // Determine the method per dataset based on normalization/data type
    const isNormalizedOrLimma = d.dataType === "microarray" || d.dataType === "proteomics" || d.dataType === "others" || d.isNormalized;
    const datasetMethod = isNormalizedOrLimma ? "limma" : (config.method || "deseq2");

    return {
      datasetId: d.id || "",
      name: d.name || "",
      method: datasetMethod || "",
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      pValueThreshold: config.pValueThreshold !== undefined && config.pValueThreshold !== null ? config.pValueThreshold : 0.05,
      logFcThreshold: config.logFcThreshold !== undefined && config.logFcThreshold !== null ? config.logFcThreshold : 1.0,
      adjustMethod: config.adjustMethod || "BH",
      referenceGroup: d.de_referenceGroup || d.referenceGroup || "",
      comparisonGroup: d.de_comparisonGroup || d.comparisonGroup || ""
    };
  });

  console.log("Outgoing Request Payload to /api/compute-job (kind=de):", JSON.stringify(payload, null, 2));

  return runComputeJob<Record<string, DEResult[]>>("de", payload, onProgress);
}


export async function runMetaAnalysisAPI(
  method: MetaMethod | "" | null,
  params: {
    pvalueMethod?: "fisher" | "stouffer" | "" | null;
    effectSizeModel?: "fixed" | "random" | "" | null;
    votes?: number | string | null;
    pValueThreshold?: number | string | null;
    logFcThreshold?: number | string | null;
  } = {},
  datasets: DatasetConfig[] = [],
  onProgress?: (info: { jobId: string; elapsed: number }) => void
): Promise<any> {
  const payload = datasets.map(d => ({
    datasetId: d.id || "",
    name: d.name || "",
    method: method || "",
    isInline: d.isInline ?? false,
    parentModule: d.parentModule || undefined,
    parentDatasetId: d.parentDatasetId || undefined,
    pvalueMethod: params.pvalueMethod ?? "",
    effectSizeModel: params.effectSizeModel ?? "",
    votes: params.votes ?? "",
    pValueThreshold: params.pValueThreshold ?? "",
    logFcThreshold: params.logFcThreshold ?? "",
    de_referenceGroup: d.de_referenceGroup || d.referenceGroup || "",
    de_comparisonGroup: d.de_comparisonGroup || d.comparisonGroup || ""
  }));
  console.log("Outgoing Request Payload to /api/compute-job (kind=meta):", JSON.stringify(payload, null, 2));
  return runComputeJob<any>("meta", payload, onProgress);
}

// ─── FS Module APIs ─────────────────────────────────────────────────────
export interface ModelMetrics {
  auc: number;
  acc: number;
  ppv?: number;
  npv?: number;
  ci?: string;
  ci_lower?: number | string;
  ci_upper?: number | string;
  fpr?: number[];
  tpr?: number[];
  tp?: number;
  fp?: number;
  fn?: number;
  tn?: number;
}

export interface TrainingResponse {
  features: { gene: string, importance: number, rank: number }[];
  accuracies: Record<string, number>;
  loss_histories: Record<string, number[]>;
  performance_metrics?: Record<string, ModelMetrics>;
  roc_plot?: string;
  venn_plot?: string;
  overlap_count?: number;
}

export async function runCrossValidationAPI(
  models: MLModel[],
  method: CVMethod,
  folds: number,
  datasets: DatasetConfig[],
  multiDatasetMode: "individual" | "combine" = "combine"
): Promise<CVResult[]> {
  const url = `${SERVER_URL}/api/cross-validation`;
  const payload = {
    models: models || [],
    cvMethod: method || "",
    folds: folds || 5,
    multiDatasetMode: multiDatasetMode || "combine",
    datasets: datasets.map(d => ({
      datasetId: d.id || "",
      name: d.name || "",
      dataType: d.dataType || "",
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      fs_trainRatio: d.fs_trainRatio ?? 0.7,
      fs_datasetPurpose: d.fs_datasetPurpose || "train-and-test",
      fs_validationStrategy: d.fs_validationStrategy || "train-test-split",
      fs_isCV: d.fs_isCV ?? false,
      fs_isInternalValidation: d.fs_isInternalValidation ?? true,
      enabledTargetPrevalence: d.fs_enabledTargetPrevalence ?? false,
      targetPrevalence: d.fs_enabledTargetPrevalence && d.fs_targetPrevalence !== undefined ? d.fs_targetPrevalence : null,
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || "",
      positiveClass: d.fs_positiveClass || d.positiveClass || "",
      negativeClass: d.fs_negativeClass || d.negativeClass || "",
      fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
      fs_negativeClass: d.fs_negativeClass || d.negativeClass || ""
    }))
  };
  
  console.log("Outgoing Request Payload to /api/cross-validation:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<CVResult[]>(url, payload);
}

export interface TestingResult {
  auc: number;
  acc: number;
  ppv?: number;
  npv?: number;
  ci?: string;
  ci_lower?: number | string;
  ci_upper?: number | string;
  fpr?: number[];
  tpr?: number[];
  tp?: number;
  fp?: number;
  fn?: number;
  tn?: number;
}

export async function runTestingAPI(
  models: MLModel[],
  datasets: DatasetConfig[],
  multiDatasetMode: "individual" | "combine" = "combine"
): Promise<Record<string, Record<string, TestingResult>>> {
  const url = `${SERVER_URL}/api/testing`;
  const payload = {
    models: models || [],
    multiDatasetMode: multiDatasetMode || "combine",
    datasets: datasets.map(d => ({
      datasetId: d.id || "",
      name: d.name || "",
      dataType: d.dataType || "",
      isInline: d.isInline ?? false,
      parentModule: d.parentModule || undefined,
      parentDatasetId: d.parentDatasetId || undefined,
      fs_trainRatio: d.fs_trainRatio ?? 0.7,
      fs_datasetPurpose: d.fs_datasetPurpose || "train-and-test",
      fs_validationStrategy: d.fs_validationStrategy || "train-test-split",
      fs_isCV: d.fs_isCV ?? false,
      fs_isInternalValidation: d.fs_isInternalValidation ?? true,
      enabledTargetPrevalence: d.fs_enabledTargetPrevalence ?? false,
      targetPrevalence: d.fs_enabledTargetPrevalence && d.fs_targetPrevalence !== undefined ? d.fs_targetPrevalence : null,
      clinicalSampleIdCol: d.clinicalSampleIdCol || "",
      clinicalGroupCol: d.clinicalGroupCol || "",
      clinicalBatchCol: d.clinicalBatchCol || "",
      positiveClass: d.fs_positiveClass || d.positiveClass || "",
      negativeClass: d.fs_negativeClass || d.negativeClass || "",
      fs_positiveClass: d.fs_positiveClass || d.positiveClass || "",
      fs_negativeClass: d.fs_negativeClass || d.negativeClass || ""
    }))
  };
  console.log("Outgoing Request Payload to /api/testing:", JSON.stringify(payload, null, 2));
  return submitAndPollJob<Record<string, Record<string, TestingResult>>>(url, payload);
}

// ─── EA Module APIs ─────────────────────────────────────────────────────
export async function runEnrichmentAnalysisAPI(
  config: EnrichmentConfig,
  datasetId?: string
): Promise<{ oraResults?: any[]; gseaResults?: any[] }> {
  const url = `${SERVER_URL}/api/ea`;
  const methods = config.methods || ["ora"];
  const payload = {
    datasetId: datasetId || "",
    methods: methods,
    oraDatabase: methods.includes("ora") ? (config.oraDatabase || []) : [],
    gseaDatabase: methods.includes("gsea") ? (config.gseaDatabase || []) : [],
    rankByCol: config.rankByCol || "",
    pValueCutoff: config.pValueCutoff !== undefined && config.pValueCutoff !== null ? config.pValueCutoff : 0.05,
    qValueCutoff: config.qValueCutoff !== undefined && config.qValueCutoff !== null ? config.qValueCutoff : 0.05,
    minGeneSetSize: config.minGeneSetSize !== undefined && config.minGeneSetSize !== null ? config.minGeneSetSize : 15,
    maxGeneSetSize: config.maxGeneSetSize !== undefined && config.maxGeneSetSize !== null ? config.maxGeneSetSize : 500,
    organism: config.organism || ""
  };
  return submitAndPollJob<{ oraResults?: any[]; gseaResults?: any[] }>(url, payload);
}
