import { useState } from "react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import SharedExportStep, { type ExportGroup, type StatChip} from "../shared/SharedExportStep";
import { useToast } from "../../hooks/use-toast";
import { isMetaEligible } from "../../dataObject";
import { downloadReportAPI } from "../../lib/api";

export default function DEExportStep() {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const [loadingReport, setLoadingReport] = useState(false);

  const handleDownloadReport = async (format: "md" | "pdf") => {
    setLoadingReport(true);
    try {
      const userId = getSessionUserId();
      const { blob, filename } = await downloadReportAPI(userId, "de", format);
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename;
      a.click();
      URL.revokeObjectURL(url);
    } catch (e: any) {
      console.error("Report download failed:", e);
      toast({
        title: "Report Download Failed",
        description: e.message || "Failed to download the analysis report.",
        variant: "destructive",
      });
    } finally {
      setLoadingReport(false);
    }
  };
  const multipleDs = state.deDatasets.length > 1;

  const getDatasetGeneIds = (d: typeof state.deDatasets[0]): Set<string> => {
    if (!d || !d.parsedData || !Array.isArray(d.parsedData)) return new Set();
    const geneIdIdx = d.columns ? d.columns.indexOf(d.geneIdCol) : -1;
    if (geneIdIdx === -1) {
      return new Set(d.parsedData.map(row => (row ? String(row[0]) : "")));
    }
    return new Set(d.parsedData.map(row => (row ? String(row[geneIdIdx]) : "")));
  };

  const getOverlappingGeneCount = (datasets: typeof state.deDatasets): number => {
    if (!Array.isArray(datasets) || datasets.length === 0) return 0;
    let intersection = getDatasetGeneIds(datasets[0]);
    for (let i = 1; i < datasets.length; i++) {
      const nextSet = getDatasetGeneIds(datasets[i]);
      intersection = new Set([...intersection].filter(x => nextSet.has(x)));
    }
    return intersection.size;
  };

  const overlappingCount = getOverlappingGeneCount(state.deDatasets);

  const getDeMethodLabel = () => {
    const deMethod = state.deConfig.method || "deseq2";
    switch (deMethod) {
      case "deseq2":
        return "with DESeq2 R package";
      case "edger":
        return "with edgeR R package";
      case "limma_voom":
        return "with limma-voom R package";
      case "limma":
        return "with limma R package";
      default:
        return deMethod;
    }
  };
  const deMethodLabel = getDeMethodLabel();

  const groups: ExportGroup[] = [];

  // Add individual dataset groups with interactive Volcano and MA plots
  (state.deDatasets || []).forEach((ds, idx) => {
    const rawRes = state.deResults ? state.deResults[ds.id] : null;
    let results: any[] = [];
    if (Array.isArray(rawRes)) {
      results = rawRes;
    } else if (rawRes && typeof rawRes === "object") {
      results = (rawRes as any).full_results || (rawRes as any).results || (rawRes as any).top10 || (rawRes as any).comb_pval || [];
      if (!Array.isArray(results)) {
        for (const val of Object.values(rawRes)) {
          if (Array.isArray(val)) {
            results = val;
            break;
          }
        }
        if (!Array.isArray(results)) results = [];
      }
    }
    const sigResults = results.filter(r => r && (r.significant || r.sig || (r.adjPValue !== undefined && r.adjPValue < state.deConfig.pValueThreshold)));
    const cfg = state.deConfig;
    const dsSuffix = multipleDs ? ` of ${ds.name}` : "";

    groups.push({
      name: `Dataset ${idx + 1}: ${ds.name}`,
      badge: "Dataset Results",
      badgeColor: "hsl(214 60% 35%)",
      datasetId: ds.id,
      datasetLabel: ds.name,
      files: [
        {
          id: `de_all_results|${ds.id}`,
          label: "All Features Table",
          ext: "CSV",
          desc: `Complete DE results matrix including all genes (${results.length}) performed by using ${deMethodLabel} method (adjusted p-value cutoff: ${state.deConfig.pValueThreshold}, adjustment method: ${state.deConfig.adjustMethod}, log₂FC threshold: |${state.deConfig.logFcThreshold}|)`,
          getTestIds: (fmt) => {
            const t = [`btn-download-all-${ds.id}-${fmt}`, `btn-download-all-${ds.id}-${fmt.toUpperCase()}`];
            if (fmt.toLowerCase() === "csv") {
              t.push(`btn-download-all-${ds.id}`);
            }
            return t;
          }
        },
        {
          id: `de_sig_results|${ds.id}`,
          label: "Significant Features Table",
          ext: "CSV",
          desc: `Significant gene sets filtered by thresholds (${sigResults.length}) using ${deMethodLabel} method (adjusted p-value cutoff: ${state.deConfig.pValueThreshold}, adjustment method: ${state.deConfig.adjustMethod}, log₂FC threshold: |${state.deConfig.logFcThreshold}|)`,
          getTestIds: (fmt) => {
            const t = [`btn-download-sig-${ds.id}-${fmt}`, `btn-download-sig-${ds.id}-${fmt.toUpperCase()}`];
            if (fmt.toLowerCase() === "csv") {
              t.push(`btn-download-sig-${ds.id}`);
            }
            return t;
          }
        },
        {
          id: `de_volcano|${ds.id}`,
          label: `Volcano Plot${dsSuffix}`,
          ext: "PDF",
          desc: `Volcano plot visualization highlighting log₂ Fold Change vs Statistical Significance (-log₁₀ p-value) for ${ds.name}`,
        },
        {
          id: `de_ma|${ds.id}`,
          label: `MA Plot${dsSuffix}`,
          ext: "PDF",
          desc: `MA plot mapping Average Log Expression vs Log Fold Change for ${ds.name}`,
        },
        {
          id: `de_heatmap_top10|${ds.id}`,
          label: `Top 10 Significant Genes Heatmap${dsSuffix}`,
          ext: "PDF",
          desc: `Clustered heatmap of the top 10 most significant differentially expressed genes${dsSuffix}, showing expression patterns across all samples`,
        }
      ]
    });
  });

  // Add Meta-analysis Results group if applicable
  if (multipleDs && state.deMetaMethod) {
    const userId = getSessionUserId();
    const metaDsId = `${userId}_de_meta`;
    groups.push({
      name: "Meta-analysis Results",
      badge: "Meta-analysis",
      badgeColor: "hsl(38 70% 35%)",
      files: [
        {
          id: `meta_results|${metaDsId}`,
          label: "Meta-analysis Results Table",
          ext: "CSV",
          desc: "Aggregated differential expression statistics across all cohorts",
          getTestIds: (fmt) => {
            const t = [`btn-download-meta-meta_results-${fmt}`, `btn-download-meta-meta_results-${fmt.toUpperCase()}`];
            if (fmt.toLowerCase() === "csv") {
              t.push("btn-download-meta-Meta-analysis Results (CSV)");
            }
            return t;
          }
        },
        ...(state.deMetaMethod === "effect_size" ? [
          {
            id: `forest_plots|${metaDsId}`,
            label: "Forest Plots",
            ext: "PDF",
            desc: "Forest plots showing effect sizes and confidence intervals across cohorts",
            getTestIds: (fmt: string) => {
              const t = [`btn-download-meta-forest_plots-${fmt}`, `btn-download-meta-forest_plots-${fmt.toUpperCase()}`];
              if (fmt.toLowerCase() === "pdf") {
                t.push("btn-download-meta-Forest Plots (PDF)");
              }
              return t;
            }
          },
          {
            id: `de_volcano|${metaDsId}`,
            label: "Meta-analysis Volcano Plot",
            ext: "PDF",
            desc: "Volcano plot mapping combined effect size (Cohen's d) against statistical significance (-log₁₀ FDR)",
            getTestIds: (fmt: string) => [`btn-download-meta-volcano-${fmt}`, `btn-download-meta-volcano-${fmt.toUpperCase()}`]
          },
          {
            id: `heterogeneity_report|${metaDsId}`,
            label: "Heterogeneity Report",
            ext: "CSV",
            desc: "Cochran's Q test statistics and I^2 heterogeneity metrics results (Cochran's Q statistic evaluating null hypothesis of homogeneity, I^2 statistic quantifying percentage of total variation across studies due to heterogeneity rather than chance)",
            getTestIds: (fmt: string) => {
              const t = [`btn-download-meta-heterogeneity_report-${fmt}`, `btn-download-meta-heterogeneity_report-${fmt.toUpperCase()}`];
              if (fmt.toLowerCase() === "csv") {
                t.push("btn-download-meta-Heterogeneity Report (CSV)");
              }
              return t;
            }
          }
        ] : [])
      ]
    });
  }

  // Add Enrichment Analysis Results group if applicable
  if (state.deInlineEaDone) {
    const isMetaActive = state.deDatasets.length > 1 && !state.deMetaSkipped && !!state.deMetaMethod;
    const userId = getSessionUserId();
    const metaDsId = `${userId}_de_meta`;

    const processCache = (cacheKey: string, dsId: string, dsSuffix: string, dsName: string) => {
      const inlineCache = state.deInlineEnResultsCache[cacheKey];
      if (!inlineCache) return;

      const submittedCfg = inlineCache.submittedCfg;
      const activeMethods = Array.isArray(submittedCfg?.methods) ? submittedCfg.methods : (Array.isArray(state.enConfig.methods) ? state.enConfig.methods : []);
      const isGsea = activeMethods.includes("gsea");
      const isOra = activeMethods.includes("ora");

      if (isGsea) {
        const dbs = Array.isArray(submittedCfg?.gseaDatabase) ? submittedCfg.gseaDatabase : (Array.isArray(state.enConfig.gseaDatabase) ? state.enConfig.gseaDatabase : []);
        dbs.forEach(db => {
          const dbLabel = db.toUpperCase();
          const hasSigGsea = Array.isArray(inlineCache?.gseaResults) && inlineCache.gseaResults.filter((r: any) => r && r.database === db).length > 0;
          const dbFiles: any[] = [
            {
              id: `gsea_results|${dsId}|${db}`,
              label: `${dbLabel} GSEA Results Table${dsSuffix}`,
              ext: "CSV",
              desc: `Significant gene sets with NES, p-value, and FDR q-value computed using ${dbLabel} database${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-gsea_results-${db}-${fmt}`, `btn-download-gsea_results-${db}-${fmt.toUpperCase()}`]
            },
          ];

          if (hasSigGsea) {
            dbFiles.push(
              {
                id: `gsea_dotplot|${dsId}|${db}`,
                label: `${dbLabel} GSEA Dot Plot${dsSuffix}`,
                ext: "PDF",
                desc: `Bubble visualization highlighting top enriched GSEA terms by NES using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_dotplot-${db}-${fmt}`, `btn-download-gsea_dotplot-${db}-${fmt.toUpperCase()}`]
              },
              {
                id: `gsea_ridgeplot|${dsId}|${db}`,
                label: `${dbLabel} GSEA Ridge Plot${dsSuffix}`,
                ext: "PDF",
                desc: `Ridge plot showing expression distributions of core enrichment genes for top terms using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_ridgeplot-${db}-${fmt}`, `btn-download-gsea_ridgeplot-${db}-${fmt.toUpperCase()}`]
              },
              {
                id: `gsea_esplot_up|${dsId}|${db}`,
                label: `${dbLabel} GSEA Enrichment Score Plot (Top Up-regulated)${dsSuffix}`,
                ext: "PDF",
                desc: `Running enrichment score profile for the top up-regulated pathway using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_esplot_up-${db}-${fmt}`, `btn-download-gsea_esplot_up-${db}-${fmt.toUpperCase()}`]
              },
              {
                id: `gsea_esplot_down|${dsId}|${db}`,
                label: `${dbLabel} GSEA Enrichment Score Plot (Top Down-regulated)${dsSuffix}`,
                ext: "PDF",
                desc: `Running enrichment score profile for the top down-regulated pathway using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_esplot_down-${db}-${fmt}`, `btn-download-gsea_esplot_down-${db}-${fmt.toUpperCase()}`]
              },
              {
                id: `gsea_ranked_list|${dsId}|${db}`,
                label: `${dbLabel} GSEA Ranked Gene List${dsSuffix}`,
                ext: "CSV",
                desc: `Full ranked gene list with calculated cross-study meta-metrics using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_ranked_list-${db}-${fmt}`, `btn-download-gsea_ranked_list-${db}-${fmt.toUpperCase()}`]
              },
              {
                id: `gsea_leading_edge|${dsId}|${db}`,
                label: `${dbLabel} GSEA Leading Edge Genes${dsSuffix}`,
                ext: "CSV",
                desc: `Core-enrichment leading edge genes extracted for significant terms using ${dbLabel}${dsSuffix}.`,
                getTestIds: (fmt: string) => [`btn-download-gsea_leading_edge-${db}-${fmt}`, `btn-download-gsea_leading_edge-${db}-${fmt.toUpperCase()}`]
              }
            );
          }

          groups.push({
            name: `${dbLabel} (GSEA)${dsSuffix}`,
            badge: "GSEA",
            badgeColor: "hsl(270 55% 40%)",
            datasetId: dsId,
            datasetLabel: dsName,
            files: dbFiles,
          });
        });
      }

      if (isOra) {
        const dbs = Array.isArray(submittedCfg?.oraDatabase) ? submittedCfg.oraDatabase : (Array.isArray(state.enConfig.oraDatabase) ? state.enConfig.oraDatabase : []);
        dbs.forEach(db => {
          const dbLabel = db.toUpperCase();
          const oraList = Array.isArray(inlineCache?.oraResults) ? inlineCache.oraResults.filter((r: any) => r && r.database === db) : [];
          const hasSigOraUp = oraList.some((r: any) => r && r.direction && r.direction.toLowerCase().includes("up"));
          const hasSigOraDown = oraList.some((r: any) => r && r.direction && r.direction.toLowerCase().includes("down"));
          const hasSigOraGeneric = oraList.length > 0 && !hasSigOraUp && !hasSigOraDown;

          const dbFiles: any[] = [
            {
              id: `ora_results|${dsId}|${db}`,
              label: `${dbLabel} ORA Results Table${dsSuffix}`,
              ext: "CSV",
              desc: `Gene sets with fold enrichment and hyper-geometric p-values using ${dbLabel} database${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_results-${db}-${fmt}`, `btn-download-ora_results-${db}-${fmt.toUpperCase()}`]
            },
          ];

          if (hasSigOraUp) {
            dbFiles.push({
              id: `ora_dotplot_up|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot - Up${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms for up-regulated genes using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot_up-${db}-${fmt}`, `btn-download-ora_dotplot_up-${db}-${fmt.toUpperCase()}`]
            });
          }

          if (hasSigOraDown) {
            dbFiles.push({
              id: `ora_dotplot_down|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot - Down${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms for down-regulated genes using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot_down-${db}-${fmt}`, `btn-download-ora_dotplot_down-${db}-${fmt.toUpperCase()}`]
            });
          }

          if (hasSigOraGeneric) {
            dbFiles.push({
              id: `ora_dotplot|${dsId}|${db}`,
              label: `${dbLabel} ORA Dot Plot${dsSuffix}`,
              ext: "PDF",
              desc: `Bubble visualization highlighting top enriched terms by ratio using ${dbLabel}${dsSuffix}.`,
              getTestIds: (fmt: string) => [`btn-download-ora_dotplot-${db}-${fmt}`, `btn-download-ora_dotplot-${db}-${fmt.toUpperCase()}`]
            });
          }

          groups.push({
            name: `${dbLabel} (ORA)${dsSuffix}`,
            badge: "ORA",
            badgeColor: "hsl(270 55% 40%)",
            datasetId: dsId,
            datasetLabel: dsName,
            files: dbFiles,
          });
        });
      }
    };

    if (isMetaActive) {
      const key = state.deInlineEnResultsCache["proteomics"] ? "proteomics" : "transcriptomics";
      processCache(key, metaDsId, " (Meta-analysis)", "Meta-analysis");
    } else {
      (state.deDatasets || []).forEach(d => {
        const cacheKey = (d.dataType === "readcounts" || d.dataType === "microarray") ? "transcriptomics" : "proteomics";
        processCache(cacheKey, d.id, ` (${d.name})`, d.name);
      });
    }
  }

  // Build stats chips
  const totalSigGenes = Object.values(state.deResults || {}).reduce((a, r) => {
    if (!r) return a;
    if (Array.isArray(r)) return a + r.filter(g => g && (g.significant || (g as any).sig)).length;
    if (typeof r === "object") {
      const rows = (r as any).full_results || (r as any).results || (r as any).comb_pval || [];
      if (Array.isArray(rows)) return a + rows.filter((g: any) => g && (g.significant || g.sig || g.qval < 0.05 || g.adjPValue < 0.05)).length;
    }
    return a;
  }, 0);
  const stats: StatChip[] = [
    { label: "Datasets", value: String(state.deDatasets.length) },
  ];
  if (multipleDs) {
    stats.push({ label: "Overlapping Genes", value: String(overlappingCount) });
  }
  stats.push(
    { label: "Total Sig. Genes", value: String(totalSigGenes) },
    { label: "p threshold", value: String(state.deConfig.pValueThreshold) },
    { label: "|log₂FC|", value: `|${state.deConfig.logFcThreshold}|` }
  );


  return (
    <SharedExportStep
      title="DE Analysis Complete"
      subtitle="Differential expression analysis complete. Download your results below."
      stats={stats}
      groups={groups}
      onBack={() => {
        const hasMeta = isMetaEligible(state.deDatasets);
        dispatch({ type: "DE_SET_STEP", step: (hasMeta && state.deMetaSkipped) ? "meta" : "module-select" });
      }}
      onReset={() => dispatch({ type: "DE_SET_STEP", step: "upload" })}
      resetLabel="Start New DE Analysis"
      resetTestId="btn-new-de"
      onDownloadReport={handleDownloadReport}
      loadingReport={loadingReport}
      module="de"
    />
  );
}
