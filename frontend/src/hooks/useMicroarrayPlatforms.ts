import { useState, useEffect } from "react";
import { fetchMicroarrayPlatformsAPI } from "../lib/api";

export interface MicroarrayPlatformEntry {
  platform: string;
  organism: string;
  organism_display: string;
  annotation_package: string;
  cdf?: string;
}

export interface MicroarrayPlatforms {
  affymetrix: MicroarrayPlatformEntry[];
  illumina: MicroarrayPlatformEntry[];
}

let cachedPlatforms: MicroarrayPlatforms | null = null;

export function useMicroarrayPlatforms() {
  const [platforms, setPlatforms] = useState<MicroarrayPlatforms>(
    cachedPlatforms || { affymetrix: [], illumina: [] }
  );
  const [loading, setLoading] = useState(!cachedPlatforms);

  useEffect(() => {
    if (cachedPlatforms) return;
    fetchMicroarrayPlatformsAPI()
      .then(data => {
        cachedPlatforms = data;
        setPlatforms(data);
        setLoading(false);
      })
      .catch((err) => {
        console.error("Failed to fetch microarray platforms:", err);
        setLoading(false);
      });
  }, []);

  return { platforms, loading };
}

export function getPlatformEntry(
  platforms: MicroarrayPlatforms,
  family: "affymetrix" | "illumina",
  platformId: string
): MicroarrayPlatformEntry | undefined {
  return platforms[family]?.find(p => p.platform === platformId);
}
