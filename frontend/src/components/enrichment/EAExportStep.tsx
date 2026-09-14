import { useState } from "react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import SharedExportStep, { type ExportGroup, type StatChip } from "../shared/SharedExportStep";
import { useToast } from "../../hooks/use-toast";
import { downloadReportAPI } from "../../lib/api";

export default function EAExportStep() {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const [loadingReport, setLoadingReport] = useState(false);

  const handleDownloadReport = async (format: "md" | "pdf") => {
    setLoadingReport(true);
    try {
      const userId = getSessionUserId();
      const { blob, filename } = await downloadReportAPI(userId, "ea", format);
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

  const submittedCfg = state.enSubmittedConfig || state.enConfig;
  const activeMethods = Array.isArray(submittedCfg?.methods) ? submittedCfg.methods : [];
  const isGsea = activeMethods.includes("gsea");
  const isOra = activeMethods.includes("ora");
  const gseaDbs = isGsea && Array.isArray(submittedCfg?.gseaDatabase) ? submittedCfg.gseaDatabase : [];
  const oraDbs = isOra && Array.isArray(submittedCfg?.oraDatabase) ? submittedCfg.oraDatabase : [];
  const activeDbs = isGsea && isOra ? [...gseaDbs, ...oraDbs] : isGsea ? gseaDbs : oraDbs;

  const activeDsId = state.enCurrentDatasetId || (Array.isArray(state.enDatasets) && state.enDatasets[0]?.id) || "en";
  const activeDsLabel = (Array.isArray(state.enDatasets) ? state.enDatasets.find(d => d.id === activeDsId)?.name : null) || "Dataset";

  const groups: ExportGroup[] = [];
  const resultsCache = state.enResultsCache ? state.enResultsCache[activeDsId] : undefined;

  if (isGsea) {
    gseaDbs.forEach(db => {
      const dbLabel = db.toUpperCase();
      const hasSigGsea = Array.isArray(resultsCache?.gseaResults) && resultsCache.gseaResults.filter((r: any) => r && r.database === db).length > 0;
      const dbFiles: any[] = [
        {
          id: `gsea_results|${activeDsId}|${db}`,
          label: `${dbLabel} GSEA Results Table`,
          ext: "csv",
          desc: `Significant gene sets with NES, p-value, and FDR q-value computed using ${dbLabel} database.`,
        },
      ];

      if (hasSigGsea) {
        dbFiles.push(
          {
            id: `gsea_dotplot|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Dot Plot`,
            ext: "pdf",
            desc: `Bubble visualization highlighting top enriched GSEA terms by NES using ${dbLabel}.`,
          },
          {
            id: `gsea_ridgeplot|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Ridge Plot`,
            ext: "pdf",
            desc: `Ridge plot showing expression distributions of core enrichment genes for top terms using ${dbLabel}.`,
          },
          {
            id: `gsea_esplot_up|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Enrichment Score Plot (Top Up-regulated)`,
            ext: "pdf",
            desc: `Running enrichment score profile for the top up-regulated pathway using ${dbLabel}.`,
          },
          {
            id: `gsea_esplot_down|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Enrichment Score Plot (Top Down-regulated)`,
            ext: "pdf",
            desc: `Running enrichment score profile for the top down-regulated pathway using ${dbLabel}.`,
          },
          {
            id: `gsea_ranked_list|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Ranked Gene List`,
            ext: "csv",
            desc: `Full ranked gene list with ranking metric values used for ${dbLabel}.`,
          },
          {
            id: `gsea_leading_edge|${activeDsId}|${db}`,
            label: `${dbLabel} GSEA Leading Edge Genes`,
            ext: "csv",
            desc: `Leading edge genes for all significant ${dbLabel} gene sets.`,
          }
        );
      }

      groups.push({
        name: `${dbLabel} (GSEA)`,
        badge: "GSEA",
        badgeColor: "hsl(270 55% 40%)",
        datasetId: activeDsId,
        datasetLabel: activeDsLabel,
        files: dbFiles,
      });
    });
  }

  if (isOra) {
    oraDbs.forEach(db => {
      const dbLabel = db.toUpperCase();
      const hasSigOra = Array.isArray(resultsCache?.oraResults) && resultsCache.oraResults.filter((r: any) => r && r.database === db).length > 0;
      const dbFiles: any[] = [
        {
          id: `ora_results|${activeDsId}|${db}`,
          label: `${dbLabel} ORA Results Table`,
          ext: "csv",
          desc: `Significant gene sets with fold enrichment and hyper-geometric p-values using ${dbLabel} database.`,
        },
      ];

      if (hasSigOra) {
        dbFiles.push({
          id: `ora_dotplot|${activeDsId}|${db}`,
          label: `${dbLabel} ORA Dot Plot`,
          ext: "pdf",
          desc: `Bubble chart showing top enriched ${dbLabel} terms by GeneRatio.`,
        });
      }

      groups.push({
        name: `${dbLabel} (ORA)`,
        badge: "ORA",
        badgeColor: "hsl(270 55% 40%)",
        datasetId: activeDsId,
        datasetLabel: activeDsLabel,
        files: dbFiles,
      });
    });
  }

  const formatDatabaseValue = (): string => {
    if (activeDbs.length === 0) return "None Selected";
    return activeDbs
      .map((db) => {
        switch (db) {
          case "MSigDB_H":
            return "MSigDB Hallmarks";
          case "MSigDB_C5":
            return "GO Gene Sets (C5)";
          case "MSigDB_C2":
            return "Canonical Pathways (C2)";
          case "GO_BP":
            return "GO:BP";
          case "KEGG":
            return "KEGG";
          case "Reactome":
            return "Reactome";
          default:
            return String(db);
        }
      })
      .join(", ");
  };

  const exportStats: StatChip[] = [
    { label: "Method", value: activeMethods.map(m => m.toUpperCase()).join(" + ") },
    { label: "Gene sets", value: isGsea ? "523" : "312" },
    { label: "Significant", value: isGsea ? "47" : "28" },
    { label: "Database", value: formatDatabaseValue() },
  ];

  return (
    <SharedExportStep
      title="Enrichment Analysis Complete"
      subtitle={`${activeMethods.map(m => m.toUpperCase()).join(" + ")} · ${formatDatabaseValue()}`}
      stats={exportStats}
      groups={groups}
      onBack={() => dispatch({ type: "EN_SET_STEP", step: "analysis" })}
      onReset={() => dispatch({ type: "EN_SET_STEP", step: "upload" })}
      resetLabel="Start New Analysis"
      onDownloadReport={handleDownloadReport}
      loadingReport={loadingReport}
      module="ea"
    />
  );
}
