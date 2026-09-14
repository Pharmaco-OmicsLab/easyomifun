import { useState, useEffect, useMemo } from "react";
import { useAppStore, getSessionUserId } from "../../store/appStore";
import Spinner from "../Spinner";
import ChangeWarnModal from "../shared/ChangeWarnModal";
import type { EnrichmentDb, GSEADb, EnrichmentMethod } from "../../dataObject";
import { runInlineEAAPI, redoStepAPI } from "../../lib/api";
import { useToast } from "../../hooks/use-toast";

interface OraRow {
  Description: string;
  Hits_count: number;
  Total_input_gene: number;
  GeneRatio: string;
  Pvalue: number;
  "P.adjust": number;
  FeaturesID: string;
  direction?: string;
  database?: string;
}

interface GseaRow {
  Description: string;
  setSize: number;
  Hits: number;
  enrichmentScore: number;
  NES: number;
  pvalue: number;
  "p.adjust": number;
  core_enrichment: string;
  database?: string;
}

const ORGS = [
  "Human (Homo sapiens)",
  "Mouse (Mus musculus)",
  "Rat (Rattus norvegicus)",
  "Pig (Sus scrofa)",
  "Chicken (Gallus gallus)",
  "Shrimp (Penaeus monodon)"
];

const GENE_ID_LABELS: Record<string, string> = {
  ensembl: "Ensembl ID",
  entrez: "Entrez ID",
  genename: "Gene Name",
};

const ORA_OPTIONS = [
  { id: "sig-up", label: "Significant upregulated genes" },
  { id: "sig-down", label: "Significant downregulated genes" },
];

const ORA_DATABASES: { val: EnrichmentDb; label: string }[] = [
  { val: "KEGG" as EnrichmentDb, label: "KEGG Pathways" },
  { val: "GO:BP" as EnrichmentDb, label: "Gene Ontology — Biological Process (GO:BP)" },
  { val: "GO:MF" as EnrichmentDb, label: "Gene Ontology — Molecular Function (GO:MF)" },
  { val: "GO:CC" as EnrichmentDb, label: "Gene Ontology — Cellular Component (GO:CC)" },
  { val: "REACTOME" as EnrichmentDb, label: "Reactome Pathways" },
];

const GSEA_DATABASES: { val: GSEADb; label: string }[] = [
  { val: "KEGG" as GSEADb, label: "KEGG Pathways" },
  { val: "GO:BP" as GSEADb, label: "Gene Ontology — Biological Process (GO:BP)" },
  { val: "GO:MF" as GSEADb, label: "Gene Ontology — Molecular Function (GO:MF)" },
  { val: "GO:CC" as GSEADb, label: "Gene Ontology — Cellular Component (GO:CC)" },
  { val: "REACTOME" as GSEADb, label: "Reactome Pathways" },
  { val: "MSigDB_H" as GSEADb, label: "MSigDB Hallmark" },
];

function getBaseDatasetId(id: string): string {
  if (!id) return "";
  return id.replace(/_(dp|de|fs|enrichment|en|ea)(.*)$/, "");
}

function parseGeneName(row: any): string {
  if (row === null || row === undefined) return "";
  if (typeof row === "string" || typeof row === "number") {
    const s = String(row).trim();
    if (s && s !== "[object Object]") return s;
  }
  if (Array.isArray(row) && row.length > 0) {
    return parseGeneName(row[0]);
  }
  if (typeof row === "object") {
    const fields = [
      "gene", "Gene", "gene_id", "Gene_ID", "gene_name", "GeneName", "geneName",
      "Gene_Symbol", "symbol", "Symbol", "ID", "id", "probe_id", "ProbeID", "Feature", "feature"
    ];
    for (const f of fields) {
      if (row[f] !== undefined && row[f] !== null) {
        const parsed = parseGeneName(row[f]);
        if (parsed) return parsed;
      }
    }
  }
  return "";
}

export default function DEInlineEAStep() {
  const { state, dispatch } = useAppStore();
  const { toast } = useToast();
  const enConfig = state.enConfig;

  const userId = state.userId || "user";
  const deMetaKey = `${userId}_de_meta_results`;

  // Config state
  const [selectedOraOptions, setSelectedOraOptions] = useState<string[]>(["sig-up", "sig-down"]);

  const enrichmentDatasets = state.deDatasets;

  const transcriptomicsDS = enrichmentDatasets.filter(
    d => d.dataType === "readcounts" || d.dataType === "microarray"
  );
  const proteomicsDS = enrichmentDatasets.filter(
    d => d.dataType === "proteomics"
  );

  const hasTranscriptomics = transcriptomicsDS.length > 0;
  const hasProteomics = proteomicsDS.length > 0;

  const hasTxSingleDe = transcriptomicsDS.length === 1 && transcriptomicsDS[0]
    ? !!(state.deResults[transcriptomicsDS[0].id] || Object.entries(state.deResults).find(([k]) => getBaseDatasetId(k) === getBaseDatasetId(transcriptomicsDS[0].id))?.[1])
    : false;
  const hasTxMetaResults = !!(
    state.deResults["meta_transcriptomics"] ||
    state.deResults[deMetaKey] ||
    state.deResults["meta"]
  );
  const canRunTxEA = transcriptomicsDS.length === 1
    ? hasTxSingleDe
    : (transcriptomicsDS.length > 1 && !state.deMetaSkipped && (hasTxMetaResults || !!state.deMetaMethod));

  const hasPrSingleDe = proteomicsDS.length === 1 && proteomicsDS[0]
    ? !!(state.deResults[proteomicsDS[0].id] || Object.entries(state.deResults).find(([k]) => getBaseDatasetId(k) === getBaseDatasetId(proteomicsDS[0].id))?.[1])
    : false;
  const hasPrMetaResults = !!(
    state.deResults["meta_proteomics"] ||
    state.deResults[deMetaKey] ||
    state.deResults["meta"]
  );
  const canRunPrEA = proteomicsDS.length === 1
    ? hasPrSingleDe
    : (proteomicsDS.length > 1 && !state.deMetaSkipped && (hasPrMetaResults || !!state.deMetaMethod));

  // Active type of data to run/view enrichment
  const [activeType, setActiveType] = useState<"transcriptomics" | "proteomics">(
    (hasTranscriptomics && canRunTxEA) ? "transcriptomics" : "proteomics"
  );

  useEffect(() => {
    if (!(hasTranscriptomics && canRunTxEA) && (hasProteomics && canRunPrEA) && activeType === "transcriptomics") {
      setActiveType("proteomics");
    }
  }, [hasTranscriptomics, canRunTxEA, hasProteomics, canRunPrEA]);

  const cachedTx = state.deInlineEnResultsCache["transcriptomics"];
  const cachedPr = state.deInlineEnResultsCache["proteomics"];

  // Tx states
  const [txOraRows, setTxOraRows] = useState<OraRow[]>(cachedTx?.oraResults || []);
  const [txGseaRows, setTxGseaRows] = useState<GseaRow[]>(cachedTx?.gseaResults || []);
  const [txMethods, setTxMethods] = useState<EnrichmentMethod[]>(cachedTx?.methods || []);
  const [txUnmappedGenes, setTxUnmappedGenes] = useState<string[]>(cachedTx?.unmappedGenes || []);
  const [txTotalInputGenes, setTxTotalInputGenes] = useState<number>(cachedTx?.totalInputGenes || 0);
  const [txMappedGenesCount, setTxMappedGenesCount] = useState<number>(cachedTx?.mappedGenesCount || 0);
  const [txDone, setTxDone] = useState(state.deInlineEaDone || !!cachedTx);
  const [txSubmittedCfg, setTxSubmittedCfg] = useState<any>(
    cachedTx?.submittedCfg || ((state.deInlineEaDone || !!cachedTx) ? {
      oraDatabase: enConfig.oraDatabase || [],
      gseaDatabase: enConfig.gseaDatabase || [],
      pValueCutoff: enConfig.pValueCutoff,
      qValueCutoff: enConfig.qValueCutoff,
      minGeneSetSize: enConfig.minGeneSetSize,
      maxGeneSetSize: enConfig.maxGeneSetSize,
      rankByCol: enConfig.rankByCol,
      methods: enConfig.methods || [],
      oraOptions: ["sig-up", "sig-down"]
    } : null)
  );

  // Pr states
  const [prOraRows, setPrOraRows] = useState<OraRow[]>(cachedPr?.oraResults || []);
  const [prGseaRows, setPrGseaRows] = useState<GseaRow[]>(cachedPr?.gseaResults || []);
  const [prMethods, setPrMethods] = useState<EnrichmentMethod[]>(cachedPr?.methods || []);
  const [prUnmappedGenes, setPrUnmappedGenes] = useState<string[]>(cachedPr?.unmappedGenes || []);
  const [prTotalInputGenes, setPrTotalInputGenes] = useState<number>(cachedPr?.totalInputGenes || 0);
  const [prMappedGenesCount, setPrMappedGenesCount] = useState<number>(cachedPr?.mappedGenesCount || 0);
  const [prDone, setPrDone] = useState(state.deInlineEaDone || !!cachedPr);
  const [prSubmittedCfg, setPrSubmittedCfg] = useState<any>(
    cachedPr?.submittedCfg || ((state.deInlineEaDone || !!cachedPr) ? {
      oraDatabase: enConfig.oraDatabase || [],
      gseaDatabase: enConfig.gseaDatabase || [],
      pValueCutoff: enConfig.pValueCutoff,
      qValueCutoff: enConfig.qValueCutoff,
      minGeneSetSize: enConfig.minGeneSetSize,
      maxGeneSetSize: enConfig.maxGeneSetSize,
      rankByCol: enConfig.rankByCol,
      methods: enConfig.methods || [],
      oraOptions: ["sig-up", "sig-down"]
    } : null)
  );

  const activeDsList = activeType === "transcriptomics" ? transcriptomicsDS : proteomicsDS;
  const isMetaActiveType = activeDsList.length > 1 && !state.deMetaSkipped && !!state.deMetaMethod;

  let activeRawDeData: any = [];
  if (isMetaActiveType) {
    const classMetaKey = `meta_${activeType}`;
    activeRawDeData = state.deResults[classMetaKey] || state.deResults["meta"] || state.deResults[deMetaKey] || [];
  } else {
    const typeDatasets = activeType === "transcriptomics" ? transcriptomicsDS : proteomicsDS;
    let found: any = null;
    for (const ds of typeDatasets) {
      if (!ds.id) continue;
      const dsBaseId = getBaseDatasetId(ds.id);
      const entry = Object.entries(state.deResults).find(([k]) => {
        if (k === ds.id) return true;
        return getBaseDatasetId(k) === dsBaseId;
      });
      if (entry && entry[1]) {
        found = entry[1];
        break;
      }
    }
    activeRawDeData = found || [];
  }

  const extractDEResultsList = (data: any): any[] => {
    if (!data) return [];
    if (Array.isArray(data)) {
      return data.filter(item => item && typeof item === "object" && !item.totalFeatures);
    }
    if (Array.isArray(data.full_results) && data.full_results.length > 0) return data.full_results;
    if (Array.isArray(data.top10) && data.top10.length > 0) return data.top10;
    if (Array.isArray(data.results) && data.results.length > 0) return data.results;
    if (Array.isArray(data.comb_pval) && data.comb_pval.length > 0) return data.comb_pval;
    if (typeof data === "object") {
      const acc: any[] = [];
      for (const [key, val] of Object.entries(data)) {
        if (key === "stats" || key === "hadDuplicates" || key === "actualMethod" || key === "volcanoPlot" || key === "maPlot") continue;
        if (Array.isArray(val)) {
          acc.push(...val.filter(item => item && typeof item === "object" && !item.totalFeatures));
        } else if (val && typeof val === "object") {
          if (Array.isArray((val as any).full_results)) acc.push(...(val as any).full_results);
          else if (Array.isArray((val as any).top10)) acc.push(...(val as any).top10);
          else if (Array.isArray((val as any).results)) acc.push(...(val as any).results);
        }
      }
      if (acc.length > 0) return acc;
    }
    return [];
  };

  const deResultsList: any[] = extractDEResultsList(activeRawDeData);

  const sigStatus = useMemo(() => {
    const stats = activeRawDeData?.stats;
    let sigUp = stats?.sigUp ? Number(stats.sigUp) : 0;
    let sigDown = stats?.sigDown ? Number(stats.sigDown) : 0;
    let totalSig = stats?.numSignificant ? Number(stats.numSignificant) : (sigUp + sigDown);

    if (totalSig === 0 && deResultsList.length > 0) {
      const sigRows = deResultsList.filter((r: any) => {
        if (r.significant !== undefined) return !!r.significant;
        const padj = r["adj.P.Val."] !== undefined ? r["adj.P.Val."] : r.adjPValue;
        return typeof padj === "number" && padj < (enConfig.pValueCutoff || 0.05);
      });
      if (sigRows.length > 0) {
        totalSig = sigRows.length;
        sigUp = sigRows.filter((r: any) => {
          if (r.direction === "up" || r.dir === "Up") return true;
          const val = r.logFC ?? r.log2FoldChange ?? r["Combined LogFC"] ?? r["Combined Effects Size"] ?? r.combined ?? 0;
          return typeof val === "number" && val > 0;
        }).length;
        sigDown = sigRows.filter((r: any) => {
          if (r.direction === "down" || r.dir === "Down") return true;
          const val = r.logFC ?? r.log2FoldChange ?? r["Combined LogFC"] ?? r["Combined Effects Size"] ?? r.combined ?? 0;
          return typeof val === "number" && val < 0;
        }).length;
      }
    }

    const hasSigUp = sigUp > 0;
    const hasSigDown = sigDown > 0;
    const hasAnySig = totalSig > 0 || hasSigUp || hasSigDown;
    return {
      hasAnySig,
      hasSigUp,
      hasSigDown,
      loading: false
    };
  }, [activeRawDeData, deResultsList, enConfig.pValueCutoff]);

  useEffect(() => {
    const currentMethods: EnrichmentMethod[] = Array.isArray(enConfig.methods) ? enConfig.methods : [];
    if (sigStatus.hasAnySig) {
      if (currentMethods.length === 0) {
        dispatch({ type: "EN_SET_CONFIG", patch: { methods: ["ora", "gsea"] } });
      }
    } else {
      if (!currentMethods.includes("gsea") || currentMethods.includes("ora")) {
        dispatch({ type: "EN_SET_CONFIG", patch: { methods: ["gsea"] } });
      }
    }

    setSelectedOraOptions(prev => {
      const next = prev.filter(opt => {
        if (opt === "sig-up") return sigStatus.hasSigUp;
        if (opt === "sig-down") return sigStatus.hasSigDown;
        return true;
      });
      return next;
    });
  }, [sigStatus.hasAnySig, sigStatus.hasSigUp, sigStatus.hasSigDown, enConfig.methods, dispatch]);

  const [subStep, setSubStep] = useState<"config" | "analysis">((state.deInlineEaDone || !!cachedTx || !!cachedPr) ? "analysis" : "config");
  const [loadingConfig, setLoadingConfig] = useState(false);

  const [loadingAnalysis, setLoadingAnalysis] = useState(false);
  const [loadingMsg, setLoadingMsg] = useState("");
  const [showWarnModal, setShowWarnModal] = useState(false);
  
  // Check if any active dataset has a platform-specific probe ID for microarray, which hardcodes the organism
  const isPlatformProbeId = (d: any) => d?.dataType === "microarray" && d?.geneIdType && !["ensembl", "entrez", "genename"].includes(d.geneIdType) && !!d?.microarrayOrganism;
  const activeSrc = activeType === "transcriptomics" ? transcriptomicsDS : proteomicsDS;
  const lockedMa = activeSrc.find(d => isPlatformProbeId(d));
  const isOrganismLocked = !!lockedMa;
  const lockedOrganism = lockedMa?.microarrayOrganism || "";

  useEffect(() => {
    if (isOrganismLocked && lockedOrganism) {
      if (enConfig.organism !== lockedOrganism) {
        dispatch({
          type: "EN_SET_CONFIG",
          patch: { organism: lockedOrganism }
        });
      }
    } else {
      if (!enConfig.organism) {
        dispatch({
          type: "EN_SET_CONFIG",
          patch: { organism: "Human (Homo sapiens)" }
        });
      }
    }
  }, [isOrganismLocked, lockedOrganism, enConfig.organism, dispatch]);

  useEffect(() => {
    const firstDs = activeSrc[0];
    if (!enConfig.geneIdType && firstDs?.geneIdType) {
      dispatch({
        type: "EN_SET_CONFIG",
        patch: { geneIdType: firstDs.geneIdType }
      });
    }
  }, [activeSrc, enConfig.geneIdType, dispatch]);

  const selectedMethods: EnrichmentMethod[] = Array.isArray(enConfig.methods)
    ? enConfig.methods
    : [];
  const isOraSelected = selectedMethods.includes("ora");
  const isGseaSelected = selectedMethods.includes("gsea");

  const isMetaActive = activeDsList.length > 1 && !state.deMetaSkipped && !!state.deMetaMethod;
  const isOnlyOraMeta = isMetaActive && (state.deMetaMethod === "vote_counting" || state.deMetaMethod === "shared_genes" || state.deMetaMethod === "combine_pvalue");

  useEffect(() => {
    if (isOnlyOraMeta) {
      if (!selectedMethods.includes("ora") || selectedMethods.includes("gsea")) {
        dispatch({ type: "EN_SET_CONFIG", patch: { methods: ["ora"] } });
      }
    }
  }, [isOnlyOraMeta, selectedMethods, dispatch]);

  const currentOraDbSelection: EnrichmentDb[] = Array.isArray(enConfig.oraDatabase)
    ? (enConfig.oraDatabase as EnrichmentDb[])
    : typeof enConfig.oraDatabase === "string" && enConfig.oraDatabase
    ? [enConfig.oraDatabase as EnrichmentDb]
    : [];

  const currentGseaDbSelection: GSEADb[] = Array.isArray(enConfig.gseaDatabase)
    ? (enConfig.gseaDatabase as GSEADb[])
    : typeof enConfig.gseaDatabase === "string" && enConfig.gseaDatabase
    ? [enConfig.gseaDatabase as GSEADb]
    : [];

  const isNoMethodSelected = selectedMethods.length === 0;
  const isNoDbSelected = (isOraSelected && currentOraDbSelection.length === 0) ||
                         (isGseaSelected && currentGseaDbSelection.length === 0);

  const activeDone = activeType === "transcriptomics" ? txDone : prDone;
  const activeOraRows = Array.isArray(activeType === "transcriptomics" ? txOraRows : prOraRows) ? (activeType === "transcriptomics" ? txOraRows : prOraRows) : [];
  const activeGseaRows = Array.isArray(activeType === "transcriptomics" ? txGseaRows : prGseaRows) ? (activeType === "transcriptomics" ? txGseaRows : prGseaRows) : [];
  const activeMethods = activeType === "transcriptomics" ? txMethods : prMethods;
  const activeUnmappedGenes = activeType === "transcriptomics" ? txUnmappedGenes : prUnmappedGenes;
  const activeTotalInputGenes = activeType === "transcriptomics" ? txTotalInputGenes : prTotalInputGenes;
  const activeMappedGenesCount = activeType === "transcriptomics" ? txMappedGenesCount : prMappedGenesCount;
  const activeSubmittedCfg = activeType === "transcriptomics" ? txSubmittedCfg : prSubmittedCfg;

  const handleDownloadUnmappedGenes = () => {
    if (!activeUnmappedGenes || activeUnmappedGenes.length === 0) return;
    const content = "Gene_Name\n" + activeUnmappedGenes.join("\n");
    const blob = new Blob([content], { type: "text/csv;charset=utf-8;" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.setAttribute("href", url);
    link.setAttribute("download", "unmapped_genes.csv");
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  };

  const updateConfig = (patch: Partial<typeof enConfig>) => {
    dispatch({ type: "EN_SET_CONFIG", patch });
  };

  const handleToggleMethod = (m: EnrichmentMethod) => {
    const next: EnrichmentMethod[] = selectedMethods.includes(m)
      ? selectedMethods.filter(x => x !== m)
      : [...selectedMethods, m];
    updateConfig({ methods: next });
  };

  const handleToggleOraDatabase = (dbVal: EnrichmentDb) => {
    const next: EnrichmentDb[] = currentOraDbSelection.includes(dbVal)
      ? currentOraDbSelection.filter(v => v !== dbVal)
      : [...currentOraDbSelection, dbVal];
    updateConfig({ oraDatabase: next });
  };

  const handleToggleGseaDatabase = (dbVal: GSEADb | "MSigDB_C5") => {
    const typedVal = dbVal as GSEADb;
    const next: GSEADb[] = currentGseaDbSelection.includes(typedVal)
      ? currentGseaDbSelection.filter(v => v !== typedVal)
      : [...currentGseaDbSelection, typedVal];
    updateConfig({ gseaDatabase: next });
  };

  const handleToggleOraOption = (id: string) => {
    setSelectedOraOptions(prev => {
      if (prev.includes(id)) {
        return prev.filter(x => x !== id);
      } else {
        return [...prev, id];
      }
    });
  };

  const canProceed = !isNoMethodSelected && (isGseaSelected || selectedOraOptions.length > 0);

  const handleRunAnalysis = async () => {
    if (activeDsList.length === 0) return;
    if (activeDone) {
      try {
        await redoStepAPI("ea", activeDsList.map(d => d.id));
      } catch (e) {
        console.error(e);
      }
    }
    setLoadingAnalysis(true);
    setLoadingMsg(`Running ${selectedMethods.map(m => m.toUpperCase()).join(" + ")} analysis for ${activeType === "transcriptomics" ? "Transcriptomics" : "Proteomics"}…`);
    try {
      let direction: "up" | "down" | "all";
      if (selectedOraOptions.includes("sig-up") && !selectedOraOptions.includes("sig-down")) {
        direction = "up";
      } else if (selectedOraOptions.includes("sig-down") && !selectedOraOptions.includes("sig-up")) {
        direction = "down";
      } else {
        direction = "all";
      }

      const eaSource = (isMetaActive && activeDsList.length > 1) ? "meta" : "de";
      const firstDs = activeDsList[0];
      const geneIdCol = firstDs?.geneIdCol || "gene";
      const currentUserId = getSessionUserId();
      const datasetId = (isMetaActive && activeDsList.length > 1) ? `${currentUserId}_de_meta` : (firstDs?.id || "");
      const results = await runInlineEAAPI(
        { ...enConfig, dataType: firstDs?.dataType, dataClass: activeType, parentModule: "de", module: "ea", isInline: true } as any,
        eaSource,
        geneIdCol,
        datasetId,
        enConfig.geneIdType || firstDs?.geneIdType || "genename",
        direction,
        activeDsList.map(d => ({ id: d.id, dataType: d.dataType, name: d.name, parentModule: "de", module: "ea", isInline: true }))
      );

      const submittedCfgPatch = {
        oraDatabase: [...currentOraDbSelection],
        gseaDatabase: [...currentGseaDbSelection],
        pValueCutoff: enConfig.pValueCutoff,
        qValueCutoff: enConfig.qValueCutoff,
        minGeneSetSize: enConfig.minGeneSetSize,
        maxGeneSetSize: enConfig.maxGeneSetSize,
        rankByCol: enConfig.rankByCol,
        organism: enConfig.organism,
        geneIdType: enConfig.geneIdType,
        methods: [...selectedMethods],
        oraOptions: [...selectedOraOptions]
      };

      if (activeType === "transcriptomics") {
        setTxOraRows(results.oraResults || []);
        setTxGseaRows(results.gseaResults || []);
        setTxMethods([...selectedMethods]);
        setTxUnmappedGenes(results.unmappedGenes || []);
        setTxTotalInputGenes(results.totalInputGenes || 0);
        setTxMappedGenesCount(results.mappedGenesCount || 0);
        setTxDone(true);
        setTxSubmittedCfg(submittedCfgPatch);
        dispatch({
          type: "DE_INLINE_EN_SET_RESULTS",
          id: "transcriptomics",
          results: {
            oraResults: results.oraResults || [],
            gseaResults: results.gseaResults || [],
            methods: [...selectedMethods],
            unmappedGenes: results.unmappedGenes || [],
            totalInputGenes: results.totalInputGenes || 0,
            mappedGenesCount: results.mappedGenesCount || 0,
            submittedCfg: submittedCfgPatch
          } as any
        });
      } else {
        setPrOraRows(results.oraResults || []);
        setPrGseaRows(results.gseaResults || []);
        setPrMethods([...selectedMethods]);
        setPrUnmappedGenes(results.unmappedGenes || []);
        setPrTotalInputGenes(results.totalInputGenes || 0);
        setPrMappedGenesCount(results.mappedGenesCount || 0);
        setPrDone(true);
        setPrSubmittedCfg(submittedCfgPatch);
        dispatch({
          type: "DE_INLINE_EN_SET_RESULTS",
          id: "proteomics",
          results: {
            oraResults: results.oraResults || [],
            gseaResults: results.gseaResults || [],
            methods: [...selectedMethods],
            unmappedGenes: results.unmappedGenes || [],
            totalInputGenes: results.totalInputGenes || 0,
            mappedGenesCount: results.mappedGenesCount || 0,
            submittedCfg: submittedCfgPatch
          } as any
        });
      }

      setShowWarnModal(false);
      dispatch({ type: "SET_DE_INLINE_EA_DONE", done: true });
    } catch (err: any) {
      console.error("Inline EA error:", err);
      toast({
        title: "Enrichment Analysis Error",
        description: err.message || "Failed to execute enrichment analysis.",
        variant: "destructive"
      });
    } finally {
      setLoadingAnalysis(false);
    }
  };

  const hasAnalysisSettingsChanged = activeDone && activeSubmittedCfg && (
    JSON.stringify(activeSubmittedCfg.oraDatabase) !== JSON.stringify(currentOraDbSelection) ||
    JSON.stringify(activeSubmittedCfg.gseaDatabase) !== JSON.stringify(currentGseaDbSelection) ||
    activeSubmittedCfg.pValueCutoff !== enConfig.pValueCutoff ||
    activeSubmittedCfg.qValueCutoff !== enConfig.qValueCutoff ||
    activeSubmittedCfg.minGeneSetSize !== enConfig.minGeneSetSize ||
    activeSubmittedCfg.maxGeneSetSize !== enConfig.maxGeneSetSize ||
    activeSubmittedCfg.rankByCol !== enConfig.rankByCol ||
    activeSubmittedCfg.organism !== enConfig.organism ||
    activeSubmittedCfg.geneIdType !== enConfig.geneIdType ||
    JSON.stringify(activeSubmittedCfg.methods || []) !== JSON.stringify(selectedMethods) ||
    JSON.stringify(activeSubmittedCfg.oraOptions || []) !== JSON.stringify(selectedOraOptions)
  );

  const handleContinueAnalysis = () => {
    if (hasAnalysisSettingsChanged) {
      setShowWarnModal(true);
      return;
    }
    if (activeType === "transcriptomics" && hasProteomics && canRunPrEA) {
      setActiveType("proteomics");
      setSubStep("config");
      return;
    }
    proceedToNext();
  };

  const handleConfirmBypass = () => {
    setShowWarnModal(false);
    if (activeType === "transcriptomics" && hasProteomics && canRunPrEA) {
      setActiveType("proteomics");
      setSubStep("config");
      return;
    }
    proceedToNext();
  };

  const proceedToNext = () => {
    dispatch({ type: "DE_SET_STEP", step: "module-select" });
  };

  const isMetaSource = activeDsList.length > 1 && !state.deMetaSkipped && !!state.deMetaMethod;
  const rankByOptions = isMetaSource
    ? (state.deMetaMethod === "effect_size"
      ? ["Combined Effects Size"]
      : ["Combined LogFC"])
    : ["LogFC"];
  const currentRankBy = enConfig.rankByCol || (isMetaSource
    ? (state.deMetaMethod === "effect_size" ? "Combined Effects Size" : "Combined LogFC")
    : "LogFC");

  useEffect(() => {
    const defaultRankBy = isMetaSource
      ? (state.deMetaMethod === "effect_size" ? "Combined Effects Size" : "Combined LogFC")
      : "LogFC";
    if (!enConfig.rankByCol || 
      (isMetaSource && enConfig.rankByCol === "LogFC") || 
      (!isMetaSource && (enConfig.rankByCol === "Combined Effects Size" || enConfig.rankByCol === "Combined LogFC")) ||
      (isMetaSource && state.deMetaMethod !== "effect_size" && enConfig.rankByCol === "Combined Effects Size") ||
      (isMetaSource && state.deMetaMethod === "effect_size" && enConfig.rankByCol === "Combined LogFC")) {
      dispatch({
        type: "EN_SET_CONFIG",
        patch: { rankByCol: defaultRankBy }
      });
    }
  }, [isMetaSource, state.deMetaMethod, enConfig.rankByCol, dispatch]);

  if (!canRunTxEA && !canRunPrEA) {
    const isMultiCohort = transcriptomicsDS.length > 1 || proteomicsDS.length > 1;
    return (
      <div className="card" style={{ padding: 24, textAlign: "center" }}>
        <div style={{ fontSize: 15, fontWeight: 700, color: "hsl(220 25% 12%)", marginBottom: 8 }}>
          {isMultiCohort
            ? "Meta-Analysis Required for Multi-Dataset Enrichment Analysis"
            : "Differential Expression Required for Enrichment Analysis"}
        </div>
        <div style={{ fontSize: 13, color: "var(--muted-foreground)", maxWidth: 600, margin: "0 auto 16px" }}>
          {isMultiCohort
            ? "Multiple datasets of the same hierarchy were detected, but meta-analysis results are not available. Enrichment analysis on multi-dataset cohorts requires combined meta-analysis results. Please return to Differential Expression and complete Meta-Analysis."
            : "Differential expression results were not found for the uploaded dataset. Enrichment analysis requires prior differential expression analysis. Please return to Differential Expression and run DE analysis."}
        </div>
        <button
          className="btn btn-primary"
          onClick={() => {
            dispatch({ type: "DE_SET_STEP", step: "analysis" });
          }}
        >
          Return to Differential Expression
        </button>
      </div>
    );
  }

  if (subStep === "config") {
    const handleContinueConfig = async () => {
      setLoadingConfig(true);
      await new Promise(r => setTimeout(r, 600));
      setLoadingConfig(false);
      setSubStep("analysis");
    };

    return (
      <>
        {loadingConfig && <Spinner label="Validating configuration…" sublabel="Please wait…" />}

        {/* Single Organism Selector */}
        <div className="card" style={{ marginBottom: 14 }}>
          <div style={{ fontSize: 13, fontWeight: 700, marginBottom: 12, textTransform: "uppercase", letterSpacing: ".04em" }}>
            {activeType === "transcriptomics" ? "Enrichment Analysis for Transcriptomics data" : "Enrichment Analysis for Proteomics data"}
          </div>
          <div style={{ display: "flex", flexDirection: "column", minWidth: 180, maxWidth: 200, marginBottom: 12 }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Select Organism</label>

            {isOrganismLocked ? (
              <div
                style={{
                  width: 200, padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "hsl(220 15% 96%)", color: "var(--foreground)", cursor: "not-allowed",
                  display: "flex", justifyContent: "space-between", alignItems: "center"
                }}
                title={`Organism locked by microarray platform probe ID (${lockedMa?.geneIdType})`}
                data-testid="details-organism-locked"
              >
                <span>{enConfig.organism || lockedOrganism}</span>
                <span style={{ fontSize: 11, color: "var(--muted-foreground)" }}>🔒</span>
              </div>
            ) : (
              <details style={{ width: 200, position: "relative" }} data-testid="details-organism">
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{enConfig.organism || "Human (Homo sapiens)"}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>

                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {ORGS.map(g => (
                    <div
                      key={g}
                      onClick={(e) => {
                        updateConfig({ organism: g });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px",
                        fontSize: 12,
                        cursor: "pointer",
                        background: (enConfig.organism || "Human (Homo sapiens)") === g ? "var(--selected-bg)" : "transparent",
                        fontWeight: (enConfig.organism || "Human (Homo sapiens)") === g ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = (enConfig.organism || "Human (Homo sapiens)") === g ? "var(--selected-bg)" : "transparent"}
                    >
                      {g}
                    </div>
                  ))}
                </div>
              </details>
            )}
            {isOrganismLocked && (
              <span style={{ fontSize: 11, color: "var(--muted-foreground)", marginTop: 4 }}>
                Locked by platform probe ID ({lockedOrganism})
              </span>
            )}
          </div>

          {/* Gene ID Type Dropdown (DE Mode) */}
          <div style={{ display: "flex", flexDirection: "column", minWidth: 180, maxWidth: 200, marginBottom: isGseaSelected ? 12 : 0 }}>
            <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Gene ID Type</label>

            <details style={{ width: 200, position: "relative" }} data-testid="details-gene-id-type">
              <summary style={{
                display: "flex", justifyContent: "space-between", alignItems: "center",
                padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
              }}>
                <span>{GENE_ID_LABELS[enConfig.geneIdType || "genename"] || "Gene Symbol"}</span>
                <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
              </summary>

              <div style={{
                position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
              }}>
                {Object.entries(GENE_ID_LABELS).map(([val, label]) => (
                  <div
                    key={val}
                    onClick={(e) => {
                      updateConfig({ geneIdType: val });
                      const details = (e.target as HTMLElement).closest("details");
                      if (details) details.removeAttribute("open");
                    }}
                    style={{
                      padding: "8px 10px",
                      fontSize: 12,
                      cursor: "pointer",
                      background: (enConfig.geneIdType || "genename") === val ? "var(--selected-bg)" : "transparent",
                      fontWeight: (enConfig.geneIdType || "genename") === val ? 600 : 400
                    }}
                    onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                    onMouseLeave={e => e.currentTarget.style.background = (enConfig.geneIdType || "genename") === val ? "var(--selected-bg)" : "transparent"}
                  >
                    {label}
                  </div>
                ))}
              </div>
            </details>
          </div>

          {/* Rank genes by column*/}
          {isGseaSelected && (
            <div style={{ display: "flex", flexDirection: "column", minWidth: 180, maxWidth: 200 }}>
              <label style={{ fontSize: 12, fontWeight: 500, marginBottom: 5 }}>Rank genes by column</label>
              
              <details style={{ width: 200, position: "relative" }} data-testid="details-rank-col">
                <summary style={{
                  display: "flex", justifyContent: "space-between", alignItems: "center",
                  padding: "8px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13,
                  background: "#fff", cursor: "pointer", listStyle: "none", userSelect: "none"
                }}>
                  <span>{currentRankBy}</span>
                  <span style={{ fontSize: 10, color: "var(--muted-foreground)" }}>▼</span>
                </summary>

                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 50, marginTop: 4,
                  border: "1px solid var(--border)", borderRadius: 7, padding: "4px 0", maxHeight: 160,
                  overflowY: "auto", background: "#fff", boxShadow: "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                }}>
                  {rankByOptions.map(c => (
                    <div
                      key={c}
                      onClick={(e) => {
                        updateConfig({ rankByCol: c });
                        const details = (e.target as HTMLElement).closest("details");
                        if (details) details.removeAttribute("open");
                      }}
                      style={{
                        padding: "8px 10px",
                        fontSize: 12,
                        cursor: "pointer",
                        background: currentRankBy === c ? "var(--selected-bg)" : "transparent",
                        fontWeight: currentRankBy === c ? 600 : 400
                      }}
                      onMouseEnter={e => e.currentTarget.style.background = "var(--selected-bg)"}
                      onMouseLeave={e => e.currentTarget.style.background = currentRankBy === c ? "var(--selected-bg)" : "transparent"}
                    >
                      {c}
                    </div>
                  ))}
                </div>
              </details>
            </div>
          )}
        </div>

        {/* Method Selection */}
        <div className="card" style={{ marginBottom: 14 }}>
          <div className="card-title" style={{ marginBottom: 12 }}>Enrichment Methods (Select multiple)</div>
          <div style={{ display: "grid", gridTemplateColumns: (isOnlyOraMeta || !sigStatus.hasAnySig) ? "1fr" : "1fr 1fr", gap: 10 }}>
            {(isOnlyOraMeta ? [
              { id: "ora" as const, icon: "🔬", label: "ORA", desc: "Over-Representation Analysis — tests if gene sets are enriched in a list of significant genes" },
            ] : !sigStatus.hasAnySig ? [
              { id: "gsea" as const, icon: "📈", label: "GSEA", desc: "Gene Set Enrichment Analysis — ranked-based enrichment using all genes" },
            ] : [
              { id: "gsea" as const, icon: "📈", label: "GSEA", desc: "Gene Set Enrichment Analysis — ranked-based enrichment using all genes" },
              { id: "ora" as const, icon: "🔬", label: "ORA", desc: "Over-Representation Analysis — tests if gene sets are enriched in a list of significant genes" },
            ]).map(m => {
              const isChosen = selectedMethods.includes(m.id);
              return (
                <div
                  key={m.id}
                  onClick={() => handleToggleMethod(m.id)}
                  style={{
                    padding: "12px 14px", borderRadius: 8, cursor: "pointer",
                    border: `2px solid ${isChosen ? "var(--primary)" : "var(--border)"}`,
                    background: isChosen ? "var(--selected-bg)" : "white",
                    transition: "all .15s",
                    position: "relative"
                  }}
                  data-testid={`card-enmethod-${m.id}`}
                >
                  {isChosen && (
                    <span style={{ position: "absolute", top: 8, right: 8, fontSize: 12, color: "var(--primary)", fontWeight: 700 }}>✓</span>
                  )}
                  <div style={{ fontSize: 18, marginBottom: 6 }}>{m.icon}</div>
                  <div style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", marginBottom: 4 }}>{m.label}</div>
                  <div style={{ fontSize: 11, color: "var(--muted-foreground)", lineHeight: 1.5 }}>{m.desc}</div>
                </div>
              );
            })}
          </div>
          {!sigStatus.hasAnySig && (
            <div className="banner info" style={{ marginTop: 12, marginBottom: 0 }}>
              ℹ️ <strong>ORA disabled:</strong> No significant genes were identified in the Differential Expression analysis. Going straight to GSEA.
            </div>
          )}
        </div>

        {/* ORA Option Selectors & DE Results Preview */}
        <div className="card" style={{ marginBottom: 14 }}>
          <div className="card-header">
            <div>
              <div className="card-title">DE Analysis Results Preview</div>
              <div className="card-sub">
                {isMetaActiveType
                  ? `Showing meta-analyzed ${activeType === "transcriptomics" ? "Transcriptomics" : "Proteomics"} results (combined across ${activeDsList.length} datasets)`
                  : `Showing ${activeType === "transcriptomics" ? "Transcriptomics" : "Proteomics"} DE results${activeDsList[0] ? ` — ${activeDsList[0].name}` : ""}`
                }
              </div>
            </div>
          </div>
          <div style={{ marginTop: 14 }}>
            {isOraSelected && (
              <>
                <div style={{ fontSize: 12, fontWeight: 600, color: "var(--muted-foreground)", marginBottom: 12, textTransform: "uppercase", letterSpacing: ".04em" }}>
                  ORA Gene Selection (Select multiple)
                </div>
                <div style={{ display: "flex", flexDirection: "column", gap: 8, marginBottom: 14 }}>
                  {ORA_OPTIONS.filter(opt => {
                    if (opt.id === "sig-up") return sigStatus.hasSigUp;
                    if (opt.id === "sig-down") return sigStatus.hasSigDown;
                    return true;
                  }).map(opt => {
                    const isSelected = selectedOraOptions.includes(opt.id);
                    return (
                      <label key={opt.id} className={`radio-option ${isSelected ? "selected" : ""}`}>
                        <input
                          type="checkbox"
                          checked={isSelected}
                          onChange={() => handleToggleOraOption(opt.id)}
                          style={{ accentColor: "var(--primary)" }}
                        />
                        <div style={{ fontWeight: 500 }}>{opt.label}</div>
                      </label>
                    );
                  })}
                </div>
              </>
            )}

            {/* Preview table */}
            <hr className="card-divider" />
            <div style={{ fontSize: 11, fontWeight: 600, color: "hsl(220 9% 45%)", marginBottom: 8, textTransform: "uppercase", letterSpacing: ".04em" }}>
              DE Results Preview (top 10 rows)
            </div>
            <div className="preview-wrap">
              <table>
                <thead>
                  {(!isMetaSource) ? (
                    <tr>
                      <th>Feature</th>
                      <th>Fold Change</th>
                      <th>Log₂FC</th>
                      <th>P-value</th>
                      <th>adj.P.Val.</th>
                    </tr>
                  ) : (
                    (() => {
                      const firstRow = deResultsList[0] || {};
                      if (state.deMetaMethod === "effect_size" || "Combined Effects Size" in firstRow || "hedges_g" in firstRow) {
                        return (
                          <tr>
                            <th>Feature</th>
                            <th>Fold Change</th>
                            <th>Combined Effects Size</th>
                            <th>P-value</th>
                            <th>adj.P.Val.</th>
                          </tr>
                        );
                      } else if (state.deMetaMethod === "combine_pvalue" || "Combined LogFC" in firstRow || "combined" in firstRow) {
                        return (
                          <tr>
                            <th>Feature</th>
                            <th>Fold Change</th>
                            <th>Combined Log₂FC</th>
                            <th>P-value</th>
                            <th>adj.P.Val.</th>
                          </tr>
                        );
                      } else if (state.deMetaMethod === "vote_counting" || "votes" in firstRow) {
                        return (
                          <tr>
                            <th>Feature</th>
                            <th>Significant Votes</th>
                            <th>Total Studies</th>
                            <th>Vote Ratio</th>
                            <th>Direction</th>
                          </tr>
                        );
                      } else {
                        return (
                          <tr>
                            <th>Feature</th>
                            <th>Presence in Studies</th>
                            <th>Consensus Direction</th>
                            <th>Log₂FC Range</th>
                          </tr>
                        );
                      }
                    })()
                  )}
                </thead>
                <tbody>
                  {deResultsList.length === 0 ? (
                    <tr>
                      <td colSpan={5} style={{ textAlign: "center", color: "var(--muted-foreground)", padding: 12 }}>
                        {isMetaActiveType
                          ? "No meta-analyzed results found. Please run Meta-Analysis first."
                          : `No DE results found for ${activeType === "transcriptomics" ? "Transcriptomics" : "Proteomics"} dataset.`}
                      </td>
                    </tr>
                  ) : (!isMetaSource) ? (
                    deResultsList.slice(0, 10).map((row, i) => {
                      const geneName = parseGeneName(row) || row?.Feature || row?.Features || `Gene_${i + 1}`;
                      const logfc = typeof row?.logFC === "number" ? row.logFC : (typeof row?.log2FoldChange === "number" ? row.log2FoldChange : (typeof row?.fc === "number" ? row.fc : (Array.isArray(row?.logFC) ? row.logFC[0] : 0)));
                      const foldChange = typeof row?.FoldChange === "number" ? row.FoldChange : (Math.pow(2, logfc));
                      const pval = typeof row?.["P-value"] === "number" ? row["P-value"] : (typeof row?.pValue === "number" ? row.pValue : (typeof row?.pval === "number" ? row.pval : 1));
                      const padj = typeof row?.["adj.P.Val."] === "number" ? row["adj.P.Val."] : (typeof row?.adjPValue === "number" ? row.adjPValue : (typeof row?.qval === "number" ? row.qval : 1));
                      const isSig = row?.significant ?? (padj < (enConfig.pValueCutoff || 0.05) && Math.abs(logfc) >= 1.0);
                      const rawDir = typeof row?.direction === "string" ? row.direction.toLowerCase() : "";
                      const dir = (rawDir === "up" || rawDir === "down" || rawDir === "ns")
                        ? rawDir
                        : (isSig ? (logfc > 0 ? "up" : "down") : "ns");
                      return (
                        <tr key={i}>
                          <td style={{ fontWeight: 600 }}>{geneName}</td>
                          <td>{typeof foldChange === "number" ? foldChange.toFixed(3) : foldChange}</td>
                          <td style={{ color: dir === "up" ? "hsl(0 65% 38%)" : dir === "down" ? "hsl(214 65% 30%)" : "inherit" }}>
                            {logfc > 0 ? "+" : ""}{typeof logfc === "number" ? logfc.toFixed(3) : logfc}
                          </td>
                          <td>{typeof pval === "number" ? (pval < 0.001 ? pval.toExponential(2) : pval.toFixed(4)) : pval}</td>
                          <td>{typeof padj === "number" ? (padj < 0.001 ? padj.toExponential(2) : padj.toFixed(4)) : padj}</td>
                        </tr>
                      );
                    })
                  ) : (
                    deResultsList.slice(0, 10).map((row, i) => {
                      const firstRow = deResultsList[0] || {};
                      const geneName = parseGeneName(row) || row?.Feature || row?.Features || `Gene_${i + 1}`;
                      if ("votes" in firstRow) {
                        const votes = row.votes;
                        const total = row.total;
                        const ratio = total > 0 ? (votes / total).toFixed(2) : "0.00";
                        const rawDir = typeof row.dir === "string" ? row.dir.toLowerCase() : "";
                        const dir = (rawDir === "up" || rawDir === "down" || rawDir === "ns") ? rawDir : (rawDir.includes("up") ? "up" : rawDir.includes("down") ? "down" : "ns");
                        return (
                          <tr key={i}>
                            <td style={{ fontWeight: 600 }}>{geneName}</td>
                            <td>{votes}</td>
                            <td>{total}</td>
                            <td>{ratio}</td>
                            <td>
                              <span style={{
                                fontSize: 11, fontWeight: 600, padding: "2px 7px", borderRadius: 4,
                                background: dir === "up" ? "hsl(0 60% 95%)" : dir === "down" ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                                color: dir === "up" ? "hsl(0 65% 38%)" : dir === "down" ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                              }}>
                                {dir === "up" ? "↑ Up" : dir === "down" ? "↓ Down" : "Not Significant"}
                              </span>
                            </td>
                          </tr>
                        );
                      } else if ("presence" in firstRow) {
                        const rawDir = typeof row.dir === "string" ? row.dir.toLowerCase() : "";
                        const dir = (rawDir === "up" || rawDir === "down" || rawDir === "ns") ? rawDir : (rawDir.includes("up") ? "up" : rawDir.includes("down") ? "down" : "ns");
                        return (
                          <tr key={i}>
                            <td style={{ fontWeight: 600 }}>{geneName}</td>
                            <td>{Array.isArray(row.presence) ? row.presence.join(", ") : row.presence}</td>
                            <td>
                              <span style={{
                                fontSize: 11, fontWeight: 600, padding: "2px 7px", borderRadius: 4,
                                background: dir === "up" ? "hsl(0 60% 95%)" : dir === "down" ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                                color: dir === "up" ? "hsl(0 65% 38%)" : dir === "down" ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                              }}>
                                {dir === "up" ? "↑ Up" : dir === "down" ? "↓ Down" : "Not Significant"}
                              </span>
                            </td>
                            <td>{row.logfc_range || "—"}</td>
                          </tr>
                        );
                      } else {
                        const combinedVal = typeof row?.["Combined Effects Size"] === "number" ? row["Combined Effects Size"]
                          : typeof row?.["Combined Effect Size"] === "number" ? row["Combined Effect Size"]
                          : typeof row?.["Combined LogFC"] === "number" ? row["Combined LogFC"]
                          : typeof row?.combined === "number" ? row.combined
                          : typeof row?.hedges_g === "number" ? row.hedges_g
                          : typeof row?.logFC === "number" ? row.logFC
                          : typeof row?.fc === "number" ? row.fc : 0;
                        const foldChange = typeof row?.FoldChange === "number" ? row.FoldChange : (Math.pow(2, combinedVal));
                        const pval = typeof row?.["P-value"] === "number" ? row["P-value"] : (typeof row?.pValue === "number" ? row.pValue : (typeof row?.pval === "number" ? row.pval : 1));
                        const padj = typeof row?.["adj.P.Val."] === "number" ? row["adj.P.Val."] : (typeof row?.adjPValue === "number" ? row.adjPValue : (typeof row?.qval === "number" ? row.qval : 1));
                        return (
                          <tr key={i}>
                            <td style={{ fontWeight: 600 }}>{geneName}</td>
                            <td>{typeof foldChange === "number" ? foldChange.toFixed(3) : foldChange}</td>
                            <td style={{ color: combinedVal > 0 ? "hsl(0 65% 38%)" : combinedVal < 0 ? "hsl(214 65% 30%)" : "inherit" }}>
                              {combinedVal > 0 ? "+" : ""}{typeof combinedVal === "number" ? combinedVal.toFixed(3) : combinedVal}
                            </td>
                            <td>{typeof pval === "number" ? (pval < 0.001 ? pval.toExponential(2) : pval.toFixed(4)) : pval}</td>
                            <td>{typeof padj === "number" ? (padj < 0.001 ? padj.toExponential(2) : padj.toFixed(4)) : padj}</td>
                          </tr>
                        );
                      }
                    })
                  )}
                </tbody>
              </table>
            </div>
          </div>
        </div>

        <div className="action-row">
          <button
            className="btn btn-default"
            onClick={() => {
              if (activeType === "proteomics" && hasTranscriptomics && canRunTxEA) {
                setActiveType("transcriptomics");
                setSubStep("analysis");
              } else {
                dispatch({ type: "DE_SET_STEP", step: "module-select" });
              }
            }}
          >
            ← Back
          </button>
          <button
            className="btn btn-primary"
            disabled={!canProceed}
            onClick={handleContinueConfig}
            data-testid="btn-en-upload-continue"
          >
            Continue to Analysis →
          </button>
        </div>
      </>
    );
  }

  const currentOrg = enConfig.organism || "Human (Homo sapiens)";
  const getOrganismKey = (orgString: string) => {
    const lower = orgString.toLowerCase();
    if (lower.includes("human")) return "human";
    if (lower.includes("mouse")) return "mouse";
    if (lower.includes("shrimp")) return "shrimp";
    return "other";
  };
  const orgKey = getOrganismKey(currentOrg);

  const filteredOraDatabases = ORA_DATABASES.filter(db => {
    if (db.val === "MSigDB_H") {
      return orgKey === "human" || orgKey === "mouse";
    }
    if (db.val === "REACTOME") {
      return orgKey !== "shrimp";
    }
    return true;
  });

  const filteredGseaDatabases = GSEA_DATABASES.filter(db => {
    if (db.val === "MSigDB_H") {
      return orgKey === "human" || orgKey === "mouse";
    }
    if (db.val === "REACTOME") {
      return orgKey !== "shrimp";
    }
    return true;
  });

  // Analysis view
  return (
    <>
      {loadingAnalysis && <Spinner label={loadingMsg} sublabel="Please wait…" />}

      {showWarnModal && (
        <ChangeWarnModal 
          onConfirm={handleConfirmBypass} 
          onCancel={() => setShowWarnModal(false)} 
        />
      )}

      <div className="card" style={{ marginBottom: 14 }}>
        <div className="card-title" style={{ marginBottom: 14 }}>
          {selectedMethods.map(m => (m === "ora" ? "ORA" : "GSEA")).join(" + ")} Configuration
        </div>

        <div style={{ display: "grid", gridTemplateColumns: (isOraSelected && isGseaSelected) ? "1fr 1fr" : "1fr", gap: 20 }}>
          {isGseaSelected && (
            <div style={{ marginBottom: 14 }}>
              <label style={{ fontSize: 13, fontWeight: 500, color: "hsl(220 15% 25%)", display: "block", marginBottom: 8 }}>
                GSEA Gene Set Databases (Select multiple)
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {filteredGseaDatabases.map(db => {
                  const isChecked = currentGseaDbSelection.includes(db.val as GSEADb);
                  return (
                    <label key={db.val} className={`radio-option ${isChecked ? "selected" : ""}`}>
                      <input
                        type="checkbox"
                        checked={isChecked}
                        onChange={() => handleToggleGseaDatabase(db.val)}
                      />
                      <div style={{ fontWeight: 500 }}>{db.label}</div>
                    </label>
                  );
                })}
              </div>
            </div>
          )}

          {isOraSelected && (
            <div style={{ marginBottom: 14 }}>
              <label style={{ fontSize: 13, fontWeight: 500, color: "hsl(220 15% 25%)", display: "block", marginBottom: 8 }}>
                ORA Gene Set Databases (Select multiple)
              </label>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {filteredOraDatabases.map(db => {
                  const isChecked = currentOraDbSelection.includes(db.val);
                  return (
                    <label key={db.val} className={`radio-option ${isChecked ? "selected" : ""}`}>
                      <input
                        type="checkbox"
                        checked={isChecked}
                        onChange={() => handleToggleOraDatabase(db.val)}
                      />
                      <div style={{ fontWeight: 500 }}>{db.label}</div>
                    </label>
                  );
                })}
              </div>
            </div>
          )}
        </div>
                    
        {isNoDbSelected && (
          <div className="banner warning" style={{ background: "hsl(38 90% 95%)", color: "hsl(38 80% 25%)", border: "1px solid hsl(38 70% 80%)", padding: 10, borderRadius: 6, marginTop: 14, fontSize: 12 }}>
            ⚠️ Please select at least one database to run the analysis.
          </div>
        )}

        <hr className="card-divider" />
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>p-value cutoff</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Filter out insignificant enriched results having p-value larger than user-input
            </div>
            <input
              type="number" step="0.01" min="0" max="1"
              value={enConfig.pValueCutoff}
              onChange={e => updateConfig({ pValueCutoff: parseFloat(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>q-value cutoff</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Apply Multiple Testing to remove False Positives where q-value larger than user-input
            </div>
            <input
              type="number" step="0.05" min="0" max="1"
              value={enConfig.qValueCutoff}
              onChange={e => updateConfig({ qValueCutoff: parseFloat(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>Min gene set size</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Excludes small gene set size pathways. Keeps only clear biological results
            </div>
            <input
              type="number" min="1"
              value={enConfig.minGeneSetSize}
              onChange={e => updateConfig({ minGeneSetSize: parseInt(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>

          <div>
            <label style={{ fontSize: 13, fontWeight: 600, color: "var(--foreground)", display: "block" }}>Max gene set size</label>
            <div style={{ fontSize: 11, color: "var(--muted-foreground)", marginBottom: 6, lineHeight: "1.3" }}>
              Excludes enriched results with large gene set size that are too broad to be interpreted
            </div>
            <input
              type="number" min="10"
              value={enConfig.maxGeneSetSize}
              onChange={e => updateConfig({ maxGeneSetSize: parseInt(e.target.value) })}
              style={{ width: "100%", padding: "7px 10px", border: "1px solid var(--border)", borderRadius: 7, fontSize: 13 }}
            />
          </div>
        </div>
      </div>

      {activeDone && (activeUnmappedGenes.length > 0 || activeTotalInputGenes > 0) && (
        <div
          className="banner warning"
          style={{
            marginBottom: 14,
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            flexWrap: "wrap",
            gap: 10,
            background: "hsl(38 90% 95%)",
            color: "hsl(38 80% 25%)",
            border: "1px solid hsl(38 70% 80%)",
            padding: "12px 16px",
            borderRadius: 8
          }}
          data-testid="unmapped-genes-banner"
        >
          <div>
            ⚠️ <strong>Gene ID Mapping Notice:</strong>{" "}
            {activeUnmappedGenes.length > 0 ? (
              <>
                <strong>{activeUnmappedGenes.length}</strong> out of <strong>{activeTotalInputGenes}</strong> gene names ({((activeUnmappedGenes.length / (activeTotalInputGenes || 1)) * 100).toFixed(1)}%) could not be mapped to Entrez ID and were excluded from enrichment analysis.
              </>
            ) : (
              <>All {activeTotalInputGenes} input gene names were successfully mapped to Entrez ID.</>
            )}
          </div>
          {activeUnmappedGenes.length > 0 && (
            <button
              className="btn btn-default"
              style={{
                fontSize: 12,
                padding: "5px 12px",
                background: "#fff",
                borderColor: "hsl(38 70% 70%)",
                color: "hsl(38 80% 25%)",
                fontWeight: 600,
                cursor: "pointer"
              }}
              onClick={handleDownloadUnmappedGenes}
              data-testid="btn-download-unmapped-genes"
            >
              Download Unmapped Genes ({activeUnmappedGenes.length})
            </button>
          )}
        </div>
      )}

      {/* ORA Results Card */}
      {activeDone && activeMethods.includes("ora") && (
        <div className="card" style={{ marginBottom: 14, opacity: hasAnalysisSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 14 }}>
            <div className="card-title">
              ORA Results
              {hasAnalysisSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated — settings changed)</span>}
            </div>
            <span style={{ fontSize: 11, fontWeight: 600, padding: "3px 10px", borderRadius: 4, background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--primary)" }}>
              {activeOraRows.length} terms
            </span>
          </div>
          {activeOraRows.length > 0 ? (
            <div style={{ overflowX: "auto" }}>
              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                <thead>
                  <tr style={{ background: "var(--neutral-bg)" }}>
                    <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Description</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Hits_count</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Total_input_gene</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Gene Ratio</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Pvalue</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>P.adjust</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>FeaturesID</th>
                  </tr>
                </thead>
                <tbody>
                  {activeOraRows.slice(0, 10).map((r, i) => (
                    <tr key={i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                      <td style={{ padding: "8px 10px", fontSize: 12 }}>
                        {r.Description}
                        {r.direction && (
                          <span style={{
                            fontSize: 10,
                            fontWeight: 600,
                            padding: "1px 5px",
                            borderRadius: 4,
                            marginLeft: 8,
                            background: r.direction.toLowerCase().includes("up") ? "hsl(0 60% 95%)" : r.direction.toLowerCase().includes("down") ? "hsl(214 60% 95%)" : "hsl(220 14% 94%)",
                            color: r.direction.toLowerCase().includes("up") ? "hsl(0 65% 38%)" : r.direction.toLowerCase().includes("down") ? "hsl(214 65% 30%)" : "hsl(220 9% 46%)",
                            display: "inline-block",
                            verticalAlign: "middle"
                          }}>
                            {r.direction.toLowerCase().includes("up") ? "↑ Up" : r.direction.toLowerCase().includes("down") ? "↓ Down" : "Not Significant"}
                          </span>
                        )}
                      </td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Hits_count}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Total_input_gene}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600 }}>{r.GeneRatio}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}><span style={{ fontWeight: 600 }}>{typeof r.Pvalue === "number" ? r.Pvalue.toFixed(4) : r.Pvalue}</span></td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}><span style={{ fontWeight: 600 }}>{typeof r["P.adjust"] === "number" ? r["P.adjust"].toFixed(4) : r["P.adjust"]}</span></td>
                      <td style={{ padding: "8px 10px", textAlign: "center", maxWidth: 200, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={r.FeaturesID}>{r.FeaturesID}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <div style={{ fontSize: 13, color: "var(--muted-foreground)", textAlign: "center", padding: "20px 0" }}>No significant ORA terms found.</div>
          )}
        </div>
      )}

      {/* GSEA Results Card */}
      {activeDone && activeMethods.includes("gsea") && (
        <div className="card" style={{ marginBottom: 14, opacity: hasAnalysisSettingsChanged ? 0.65 : 1, transition: "opacity 0.2s ease" }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 14 }}>
            <div className="card-title">
              GSEA Results
              {hasAnalysisSettingsChanged && <span style={{ fontSize: 12, color: "hsl(38 85% 35%)", fontWeight: 400, marginLeft: 8 }}>(Outdated — settings changed)</span>}
            </div>
            <span style={{ fontSize: 11, fontWeight: 600, padding: "3px 10px", borderRadius: 4, background: "var(--selected-bg)", color: "var(--foreground)", border: "1px solid var(--primary)" }}>
              {activeGseaRows.length} gene sets
            </span>
          </div>
          {activeGseaRows.length > 0 ? (
            <div style={{ overflowX: "auto" }}>
              <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                <thead>
                  <tr style={{ background: "var(--neutral-bg)" }}>
                    <th style={{ padding: "8px 10px", textAlign: "left", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Description</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>setSize</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>Hits</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>enrichmentScore</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>NES</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>pvalue</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>p.adjust</th>
                    <th style={{ padding: "8px 10px", textAlign: "center", fontWeight: 600, color: "var(--foreground)", borderBottom: "2px solid var(--border)" }}>core_enrichment</th>
                  </tr>
                </thead>
                <tbody>
                  {activeGseaRows.slice(0, 10).map((r, i) => (
                    <tr key={i} style={{ borderBottom: "1px solid var(--border)", background: i % 2 === 0 ? "white" : "var(--muted)" }}>
                      <td style={{ padding: "8px 10px", fontSize: 12, fontWeight: 500 }}>{r.Description}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.setSize}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{r.Hits}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r.enrichmentScore === "number" ? r.enrichmentScore.toFixed(3) : r.enrichmentScore}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>
                        <span style={{ fontWeight: 700, fontSize: 13, color: r.NES > 0 ? "hsl(150 55% 30%)" : "hsl(0 65% 40%)" }}>
                          {r.NES > 0 ? "+" : ""}{typeof r.NES === "number" ? r.NES.toFixed(2) : r.NES}
                        </span>
                      </td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r.pvalue === "number" ? r.pvalue.toFixed(3) : r.pvalue}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center" }}>{typeof r["p.adjust"] === "number" ? r["p.adjust"].toFixed(3) : r["p.adjust"]}</td>
                      <td style={{ padding: "8px 10px", textAlign: "center", maxWidth: 200, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={r.core_enrichment}>{r.core_enrichment}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <div style={{ fontSize: 13, color: "var(--muted-foreground)", textAlign: "center", padding: "20px 0" }}>No significant GSEA gene sets found.</div>
          )}
        </div>
      )}

      <div className="action-row">
        <div style={{ display: "flex", gap: 8 }}>
          <button
            className="btn btn-default"
            onClick={() => setSubStep("config")}
            data-testid="btn-en-back"
          >
            ← Back
          </button>
        </div>

        <div style={{ display: "flex", gap: 8 }}>
          {hasAnalysisSettingsChanged && (
            <button
              className="btn btn-default"
              style={{
                borderColor: "hsl(38 70% 72%)",
                color: "hsl(32 90% 35%)",
                background: (isNoMethodSelected || isNoDbSelected) ? "hsl(39, 100%, 95%)" : "hsl(38 90% 95%)",
                fontWeight: 600,
                cursor: (isNoMethodSelected || isNoDbSelected) ? "not-allowed" : "pointer"
              }}
              disabled={isNoMethodSelected || isNoDbSelected}
              onClick={handleRunAnalysis}
              data-testid="btn-en-rerun"
            >
              🔄 Redo Enrichment Analysis
            </button>
          )}

          {!activeDone ? (
            <button
              className="btn btn-primary"
              style={{
                cursor: (isNoMethodSelected || isNoDbSelected) ? "not-allowed" : "pointer",
                opacity: (isNoMethodSelected || isNoDbSelected) ? 0.6 : 1,
              }}
              disabled={isNoMethodSelected || isNoDbSelected}
              onClick={handleRunAnalysis}
              data-testid="btn-en-run"
            >
              Run {selectedMethods.map(m => m.toUpperCase()).join(" + ")} Analysis
            </button>
          ) : (
            <button
              className="btn btn-primary"
              onClick={handleContinueAnalysis}
              data-testid="btn-en-continue-export"
            >
              Continue →
            </button>
          )}
        </div>
      </div>
    </>
  );
}
