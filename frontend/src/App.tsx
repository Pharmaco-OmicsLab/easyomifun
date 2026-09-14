import { useReducer, useEffect } from "react";
import { Switch, Route, Router as WouterRouter } from "wouter";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { Toaster } from "./components/ui/toaster";
import { TooltipProvider } from "./components/ui/tooltip";

import { AppContext, appReducer, initialState } from "./store/appStore";
import { SERVER_URL } from "./lib/api";

import Home from "./pages/Home";
import DataProcessing from "./pages/DataProcessing";
import DEAnalysis from "./pages/DEAnalysis";
import Enrichment from "./pages/Enrichment";
import FeatureSelection from "./pages/FeatureSelection";
import NotFound from "./pages/not-found";

const queryClient = new QueryClient();

function Router() {
  return (
    <Switch>
      <Route path="/" component={Home} />
      <Route path="/data-processing" component={DataProcessing} />
      <Route path="/de-analysis" component={DEAnalysis} />
      <Route path="/enrichment" component={Enrichment} />
      <Route path="/feature-selection" component={FeatureSelection} />
      <Route component={NotFound} />
    </Switch>
  );
}

import { useState } from "react";
import DiscardWarnModal from "./components/shared/DiscardWarnModal";

function AppShell() {
  const [state, dispatch] = useReducer(appReducer, initialState);
  const [pendingAction, setPendingAction] = useState<any>(null);

  useEffect(() => {
    const handleCleanup = () => {
      const userId = sessionStorage.getItem("easyomifun_user_id") || "";
      navigator.sendBeacon(`${SERVER_URL}/api/cleanup?userId=${userId}`);
    };
    window.addEventListener("beforeunload", handleCleanup);
    window.addEventListener("pagehide", handleCleanup);
    return () => {
      window.removeEventListener("beforeunload", handleCleanup);
      window.removeEventListener("pagehide", handleCleanup);
    };
  }, []);

  const customDispatch = (action: any) => {
    const modulesRan = state.dpInlineDeDone || state.dpInlineFsDone || state.dpInlineEaDone;
    const isConfigChange =
      action.type === "DP_SET_ANNOTATION" ||
      action.type === "DP_SET_PROCESSING" ||
      action.type === "DP_SET_NORM" ||
      action.type === "DP_SET_BATCH";

    if (modulesRan && isConfigChange) {
      setPendingAction(action);
    } else {
      dispatch(action);
    }
  };

  const completedModulesList: string[] = [];
  if (state.dpInlineDeDone) completedModulesList.push("DE Analysis");
  if (state.dpInlineFsDone) completedModulesList.push("Feature Selection");
  if (state.dpInlineEaDone) completedModulesList.push("Enrichment Analysis");

  const getStepFromAction = (action: any): string => {
    switch (action?.type) {
      case "DP_SET_ANNOTATION": return "annotation";
      case "DP_SET_PROCESSING": return "processing";
      case "DP_SET_NORM": return "normalization";
      case "DP_SET_BATCH": return "batch";
      default: return "upload";
    }
  };

  const handleConfirm = () => {
    if (pendingAction) {
      const step = getStepFromAction(pendingAction);
      dispatch(pendingAction);
      if (step === "normalization" || step === "annotation" || step === "processing") {
        dispatch({ type: "REDO_NORMALIZATION" });
      }
      setPendingAction(null);
    }
  };

  const handleCancel = () => {
    setPendingAction(null);
  };

  return (
    <AppContext.Provider value={{ state, dispatch: customDispatch }}>
      <QueryClientProvider client={queryClient}>
        <TooltipProvider>
          <WouterRouter base={import.meta.env.BASE_URL.replace(/\/$/, "")}>
            <Router />
          </WouterRouter>
          <Toaster />
          {pendingAction && (
            <DiscardWarnModal
              stepId={getStepFromAction(pendingAction)}
              datasetIds={state.dpDatasets.map(d => d.id)}
              modulesList={completedModulesList}
              onConfirm={handleConfirm}
              onCancel={handleCancel}
            />
          )}
        </TooltipProvider>
      </QueryClientProvider>
    </AppContext.Provider>
  );
}

export default AppShell;
