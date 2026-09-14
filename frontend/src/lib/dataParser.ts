import Papa from "papaparse";
import type { GeneIdType } from "../dataObject";
import type { AlignSamplesResponse } from "./api";

export interface ParsedExpressionData {
  columns: string[];
  parsedData: (string | number)[][];
  geneInfoCols: string[];
  geneIdCol: string;
  detectedGeneIdType: GeneIdType;
  nSamples: number;
  nFeatures: number;
  isNormalized: boolean;
  hasNA: boolean;
}

export interface ParsedClinicalData {
  clinicalColumns: string[];
  clinicalParsedData: string[][];
  clinicalSampleIdCol: string;
  clinicalGroupCol: string;
}

export function detectGeneIdType(value: any): GeneIdType {
  if (value === undefined || value === null) return "";
  const v = String(value).trim();
  if (/^ENS[A-Z]*G\d+/.test(v)) return "ensembl";
  if (/^ILMN_\d+/.test(v)) return "illumina";
  if (/^AFFX-/.test(v)) return "affymetrix";
  if (/^\d+$/.test(v)) return "entrez";
  return "genename";
}

export function isDesktopMode(): boolean {
  if (typeof window === "undefined") return false;
  if (Boolean((window as any).electronAPI?.isDesktop)) return true;
  if (typeof navigator !== "undefined" && navigator.userAgent && navigator.userAgent.toLowerCase().includes("electron")) return true;
  return false;
}

export function getDesktopFilePath(file: File): string | null {
  try {
    // 1. Electron preload webUtils API if available
    const electronAPI = typeof window !== "undefined" ? (window as any).electronAPI : undefined;
    if (electronAPI && typeof electronAPI.getPathForFile === "function") {
      const p = electronAPI.getPathForFile(file);
      if (p && typeof p === "string" && p.trim() !== "") {
        return p;
      }
    }
    // 2. Native DOM File.path property (always populated by Chromium/Electron on desktop)
    const nativePath = (file as any)?.path;
    if (nativePath && typeof nativePath === "string" && nativePath.trim() !== "") {
      return nativePath;
    }
  } catch (err) {
    console.warn("Could not get native file path:", err);
  }
  return null;
}

export function countFileRows(file: File): Promise<number> {
  return new Promise((resolve) => {
    let nonEmptyLineCount = 0;
    let lineHasContent = false;

    const CHUNK_SIZE = 4 * 1024 * 1024;
    let offset = 0;

    const reader = new FileReader();
    reader.onload = (e) => {
      const buffer = new Uint8Array(e.target?.result as ArrayBuffer);
      for (let i = 0; i < buffer.length; i++) {
        const byte = buffer[i];
        if (byte === 10) {
          if (lineHasContent) {
            nonEmptyLineCount++;
            lineHasContent = false;
          }
        } else if (byte !== 13 && byte !== 32 && byte !== 9) {
          lineHasContent = true;
        }
      }
      offset += CHUNK_SIZE;
      if (offset < file.size) {
        readNextChunk();
      } else {
        if (lineHasContent) {
          nonEmptyLineCount++;
        }
        const dataRows = Math.max(0, nonEmptyLineCount - 1);
        resolve(dataRows);
      }
    };
    reader.onerror = () => {
      resolve(0);
    };

    function readNextChunk() {
      const slice = file.slice(offset, offset + CHUNK_SIZE);
      reader.readAsArrayBuffer(slice);
    }

    readNextChunk();
  });
}

export function parseExpressionFile(
  file: File,
  options?: { previewOnly?: boolean; maxRows?: number }
): Promise<ParsedExpressionData> {
  return new Promise((resolve, reject) => {
    const parseConfig: Papa.ParseConfig = {
      skipEmptyLines: true,
      ...(options?.previewOnly ? { preview: (options.maxRows || 100) + 1 } : {}),
      complete: async (results: Papa.ParseResult<string[]>) => {
        const data = results.data;
        if (data.length === 0) {
          reject(new Error("Expression file is empty."));
          return;
        }
        const headers = data[0];
        const rows = data.slice(1);
        const geneIdCol = headers[0] || "GeneID";

        const geneInfoCols: string[] = [];
        headers.forEach((col, idx) => {
          if (idx === 0) return;
          let nonNumericCount = 0;
          let totalCount = 0;
          for (let rIdx = 0; rIdx < Math.min(20, rows.length); rIdx++) {
            const val = rows[rIdx][idx];
            if (val !== undefined && val !== null) {
              const strVal = val.trim();
              if (strVal !== "") {
                totalCount++;
                if (isNaN(Number(strVal)) && strVal.toLowerCase() !== "na" && strVal.toLowerCase() !== "nan") {
                  nonNumericCount++;
                }
              }
            }
          }
          if (totalCount > 0 && nonNumericCount / totalCount > 0.5) {
            geneInfoCols.push(col);
          }
        });

        const sampleColNames = headers.filter(c => c !== geneIdCol && !geneInfoCols.includes(c));
        const nSamples = sampleColNames.length;
        let nFeatures = rows.length;
        if (options?.previewOnly && typeof file !== "undefined" && file && file.size > 0) {
          try {
            const counted = await countFileRows(file);
            if (counted > 0) {
              nFeatures = counted;
            }
          } catch (err) {
            console.warn("Could not count full file rows:", err);
          }
        }

        const testGeneId = rows[0]?.[0] || "";
        const detectedGeneIdType = detectGeneIdType(testGeneId);

        // Auto-detect normalization
        let isNormalized = false;
        const testColsIdx = headers.map((col, idx) => ({ col, idx })).filter(x => x.col !== geneIdCol && !geneInfoCols.includes(x.col)).map(x => x.idx);
        for (let rIdx = 0; rIdx < Math.min(100, rows.length); rIdx++) {
          const row = rows[rIdx];
          for (const cIdx of testColsIdx) {
            const val = row[cIdx];
            if (val && val.includes(".")) {
              isNormalized = true;
              break;
            }
          }
          if (isNormalized) break;
        }

        // Calculate hasNA: check if there are any non-numeric/missing values in the expression data columns
        let hasNA = false;
        for (const row of rows) {
          for (const cIdx of testColsIdx) {
            const val = row[cIdx];
            if (val === undefined || val === null) {
              hasNA = true;
              break;
            }
            const trimmed = String(val).trim();
            if (trimmed === "" || trimmed.toLowerCase() === "na" || trimmed.toLowerCase() === "nan" || isNaN(Number(trimmed))) {
              hasNA = true;
              break;
            }
          }
          if (hasNA) break;
        }

        const convertedRows = rows.map(row => {
          return row.map((val, idx) => {
            if (idx === 0) {
              if (val === undefined || val === null) return "";
              // Treat first column (Gene ID / Feature) as characters/strings exactly as they are (do NOT trim or convert to number)
              return String(val);
            }
            const colName = headers[idx];
            if (geneInfoCols.includes(colName)) {
              return val === undefined || val === null ? "" : String(val).trim();
            }
            if (val === undefined || val === null) return null as any;
            const strVal = String(val).trim();
            if (strVal === "" || strVal.toLowerCase() === "na" || strVal.toLowerCase() === "nan") {
              return null as any;
            }
            const num = Number(strVal);
            return isNaN(num) ? 0 : num;
          });
        });

        resolve({
          columns: headers,
          parsedData: convertedRows,
          geneInfoCols,
          geneIdCol,
          detectedGeneIdType,
          nSamples,
          nFeatures,
          isNormalized,
          hasNA,
        });
      },
      error: (error: Error) => {
        reject(error);
      }
    };

    Papa.parse<string[]>(file, parseConfig);
  });
}

export function parseClinicalFile(
  file: File,
  options?: { previewOnly?: boolean; maxRows?: number }
): Promise<ParsedClinicalData> {
  return new Promise((resolve, reject) => {
    const parseConfig: Papa.ParseConfig = {
      skipEmptyLines: true,
      ...(options?.previewOnly ? { preview: (options.maxRows || 100) + 1 } : {}),
      complete: (results: Papa.ParseResult<string[]>) => {
        const data = results.data;
        if (data.length === 0) {
          reject(new Error("Clinical file is empty."));
          return;
        }
        const headers = data[0];
        const rows = data.slice(1);

        let clinicalSampleIdCol = headers[0] || "";
        let clinicalGroupCol = "";
        for (const h of headers) {
          const lh = h.toLowerCase();
          if (lh.includes("sampleid") || lh.includes("sample_id") || lh.includes("sample")) {
            clinicalSampleIdCol = h;
          }
          if (lh.includes("group") || lh.includes("condition") || lh.includes("class") || lh.includes("treatment")) {
            clinicalGroupCol = h;
          }
        }
        if (!clinicalGroupCol && headers.length > 1) {
          clinicalGroupCol = headers[1];
        }

        const convertedClinRows = rows.map(row => {
          return row.map(val => val === undefined || val === null ? "" : String(val).trim());
        });

        resolve({
          clinicalColumns: headers,
          clinicalParsedData: convertedClinRows,
          clinicalSampleIdCol,
          clinicalGroupCol,
        });
      },
      error: (error: Error) => {
        reject(error);
      }
    };

    Papa.parse<string[]>(file, parseConfig);
  });
}

export interface AlignSamplesParams {
  expressionColumns: string[];
  geneIdCol: string;
  geneInfoCols: string[];
  clinicalParsedData: string[][];
  clinicalColumns: string[];
  clinicalSampleIdCol: string;
  expressionParsedData?: (string | number)[][];
  geneIdType?: string;
  clinicalGroupCol?: string;
  clinicalBatchCol?: string;
  clinicalOtherCovariates?: string[];
  nFeatures?: number;
}

function isValMissing(val: any): boolean {
  if (val === undefined || val === null) return true;
  if (Array.isArray(val)) {
    if (val.length === 0) return true;
    val = val[0];
  }
  if (val === undefined || val === null) return true;
  const valClean = String(val).trim();
  if (valClean === "NA" || valClean === "NaN" || valClean === "") return true;
  return false;
}

export function isAnnotationOrInfoColumn(
  col: string,
  geneIdCol?: string,
  geneInfoCols?: string[],
  idx?: number,
  fsFeatureIdx?: number
): boolean {
  if (!col) return false;
  if (fsFeatureIdx !== undefined && idx === fsFeatureIdx) return true;
  if (geneIdCol && col === geneIdCol) return true;
  if (geneInfoCols && geneInfoCols.includes(col)) return true;
  const lower = col.toLowerCase().trim();
  return ["entrez_id", "gene_symbol", "gene_biotype"].includes(lower);
}

export function getExpressionSampleColumns(
  columns: string[],
  geneIdCol?: string,
  geneInfoCols?: string[],
  fsFeatureIdx?: number
): string[] {
  if (!columns || !Array.isArray(columns)) return [];
  return columns.filter((col, idx) => !isAnnotationOrInfoColumn(col, geneIdCol, geneInfoCols, idx, fsFeatureIdx));
}

export function alignSamplesLocal(params: AlignSamplesParams): AlignSamplesResponse {
  const {
    expressionColumns,
    geneIdCol,
    geneInfoCols,
    clinicalParsedData,
    clinicalColumns,
    clinicalSampleIdCol,
    expressionParsedData,
    clinicalGroupCol,
    clinicalBatchCol,
  } = params;

  let exprSamples: string[] = getExpressionSampleColumns(expressionColumns || [], geneIdCol, geneInfoCols);

  if (exprSamples.length === 0) {
    exprSamples = Array.from({ length: 10 }, (_, i) => `Sample_${i + 1}`);
  }

  const clin_sample_idx = clinicalSampleIdCol && clinicalColumns ? clinicalColumns.indexOf(clinicalSampleIdCol) : -1;
  const clin_group_idx = clinicalGroupCol && clinicalColumns ? clinicalColumns.indexOf(clinicalGroupCol) : -1;
  const clin_batch_idx = clinicalBatchCol && clinicalColumns ? clinicalColumns.indexOf(clinicalBatchCol) : -1;

  const indices_to_check: number[] = [];
  if (clin_sample_idx !== -1) indices_to_check.push(clin_sample_idx);
  if (clin_group_idx !== -1) indices_to_check.push(clin_group_idx);
  if (clin_batch_idx !== -1) indices_to_check.push(clin_batch_idx);

  let hasMissingClinicalValues = false;
  const removedSamplesDueToMissing: string[] = [];
  let clinSamples: string[] = [];

  if (clinicalParsedData && clinicalParsedData.length > 0 && clinicalColumns && clinicalColumns.length > 0) {
    for (const row of clinicalParsedData) {
      let is_missing_row = false;
      for (const idx of indices_to_check) {
        if (row.length > idx) {
          if (isValMissing(row[idx])) {
            is_missing_row = true;
            break;
          }
        } else {
          is_missing_row = true;
          break;
        }
      }

      let sampId: string | null = null;
      if (clin_sample_idx !== -1 && row.length > clin_sample_idx) {
        const val = row[clin_sample_idx];
        if (val !== undefined && val !== null) {
          sampId = String(val).trim();
        }
      }

      if (is_missing_row) {
        hasMissingClinicalValues = true;
        if (sampId) {
          removedSamplesDueToMissing.push(sampId);
        }
      } else {
        if (sampId) {
          clinSamples.push(sampId);
        }
      }
    }
  }

  if (clinSamples.length === 0) {
    clinSamples = Array.from({ length: 10 }, (_, i) => `Sample_${i + 1}`);
  }

  const commonSamples = exprSamples.filter(x => clinSamples.includes(x));
  const missingClinicalSamples = exprSamples.filter(x => !clinSamples.includes(x));
  const missingExpressionSamples = clinSamples.filter(x => !exprSamples.includes(x));

  const nFeatures = params.nFeatures !== undefined ? params.nFeatures : undefined;

  let minGroupSampleSize = 0;
  if (clin_group_idx !== -1 && clin_sample_idx !== -1 && clinicalParsedData && clinicalParsedData.length > 0 && commonSamples.length > 0) {
    const groupVals: string[] = [];
    for (const row of clinicalParsedData) {
      if (row.length > clin_sample_idx && row.length > clin_group_idx) {
        const sampId = String(row[clin_sample_idx]).trim();
        if (commonSamples.includes(sampId)) {
          const grpVal = String(row[clin_group_idx]).trim();
          if (grpVal !== "" && grpVal.toLowerCase() !== "na" && grpVal.toLowerCase() !== "nan") {
            groupVals.push(grpVal);
          }
        }
      }
    }
    if (groupVals.length > 0) {
      const counts: Record<string, number> = {};
      for (const val of groupVals) {
        counts[val] = (counts[val] || 0) + 1;
      }
      minGroupSampleSize = Math.min(...Object.values(counts));
    }
  }

  let detectedGeneIdType = "ensembl";
  if (expressionParsedData && expressionParsedData.length > 0 && expressionColumns && geneIdCol) {
    const geneIdIdx = expressionColumns.indexOf(geneIdCol);
    if (geneIdIdx !== -1) {
      const firstRow = expressionParsedData[0];
      if (firstRow && firstRow.length > geneIdIdx) {
        const testVal = firstRow[geneIdIdx];
        if (testVal !== undefined && testVal !== null) {
          detectedGeneIdType = detectGeneIdType(testVal);
        }
      }
    }
  }

  return {
    commonSamples,
    missingClinicalSamples,
    missingExpressionSamples,
    nFeatures,
    detectedGeneIdType,
    detectedIsNormalized: false,
    hasMissingClinicalValues,
    removedSamplesDueToMissing,
    minGroupSampleSize,
  };
}

export function unparseCSV(columns: string[], data: any[][]): string {
  return Papa.unparse({
    fields: columns,
    data: data
  });
}

export interface ValidateFSResponse {
  sufficient: boolean;
  message: string;
}

export function validateFSSamplesLocal(datasets: any[], _trainRatio: number = 0.7): ValidateFSResponse {
  let sufficient = true;
  let message = "";

  for (const d of datasets) {
    const geneIdCol = d.geneIdCol || (d.columns && d.columns.length > 0 ? d.columns[0] : "GeneID");
    const geneInfoCols = d.geneInfoCols || [];
    
    // Find expression sample columns
    const exprSamples = (d.sampleIds && d.sampleIds.length > 0)
      ? d.sampleIds
      : getExpressionSampleColumns(d.columns || [], geneIdCol, geneInfoCols);

    // Parse clinical samples locally
    const clinRows = d.clinicalParsedData || [];
    const clinCols = d.clinicalColumns || [];
    const sampleIdCol = d.clinicalSampleIdCol || (clinCols.length > 0 ? clinCols[0] : "");
    const groupCol = d.clinicalGroupCol || "";

    const sampleIdx = sampleIdCol && clinCols ? clinCols.indexOf(sampleIdCol) : -1;
    const groupIdx = groupCol && clinCols ? clinCols.indexOf(groupCol) : -1;

    if (sampleIdx === -1 || clinRows.length === 0) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' has no clinical data or clinical parsing failed.`;
      break;
    }

    if (groupIdx === -1 || !groupCol) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' does not have group column assigned.`;
      break;
    }

    // Match samples
    const clinSamples = clinRows.map((r: string[]) => r[sampleIdx]).filter(Boolean);
    const commonSamples = exprSamples.filter((x: string) => clinSamples.includes(x));
    const nSamples = commonSamples.length;

    if (nSamples < 30) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' must have at least 30 matching samples (currently has ${nSamples}).`;
      break;
    }

    // Group sizes check
    const validRows = clinRows.filter((r: string[]) => {
      const sId = r[sampleIdx];
      const val = r[groupIdx];
      return commonSamples.includes(sId) && val !== undefined && val !== null && String(val).trim() !== "" && String(val).trim().toLowerCase() !== "na" && String(val).trim().toLowerCase() !== "nan";
    });

    const groups = validRows.map((r: string[]) => String(r[groupIdx]).trim());
    if (groups.length === 0) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' must have more than 1 group.`;
      break;
    }

    const counts: Record<string, number> = {};
    for (const g of groups) {
      counts[g] = (counts[g] || 0) + 1;
    }

    const groupKeys = Object.keys(counts);
    if (groupKeys.length <= 1) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' must have more than 1 group.`;
      break;
    }

    const minGrp = Math.min(...Object.values(counts));
    if (minGrp <= 10) {
      sufficient = false;
      message = `Dataset '${d.name || d.datasetName || d.id}' has a group with ${minGrp} samples. Lowest group size must be larger than 10.`;
      break;
    }
  }

  return { sufficient, message };
}

