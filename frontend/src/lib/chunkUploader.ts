import { SERVER_URL } from "./api";

const CHUNK_SIZE = 4 * 1024 * 1024; // 4MB chunks
const MAX_RETRIES = 3;

export interface ChunkUploadResponse {
  status: "success" | "error";
  uploadId?: string;
  completed?: boolean;
  receivedChunks?: number;
  totalChunks?: number;
  filePath?: string;
  message?: string;
}

/**
 * Uploads a file to the backend server in 4MB chunks.
 * Automatically slices the file Blob, retries transient failures, and reports progress.
 * Returns the final uploadId upon completion.
 */
export async function uploadFileInChunks(
  file: File,
  datasetId: string,
  fileType: "expression" | "clinical" = "expression",
  onProgress?: (pct: number) => void
): Promise<string> {
  const uploadId = `upload_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
  const totalSize = file.size;
  const totalChunks = Math.max(1, Math.ceil(totalSize / CHUNK_SIZE));
  const userId = sessionStorage.getItem("easyomifun_user_id") || "";

  let uploadedBytes = 0;

  for (let chunkIndex = 0; chunkIndex < totalChunks; chunkIndex++) {
    const start = chunkIndex * CHUNK_SIZE;
    const end = Math.min(start + CHUNK_SIZE, totalSize);
    const chunkBlob = file.slice(start, end);
    const chunkText = await chunkBlob.text();

    const payload = {
      uploadId,
      chunkIndex,
      totalChunks,
      chunkData: chunkText,
      userId,
      datasetId,
      fileType,
    };

    let attempt = 0;
    let success = false;
    let lastError: any = null;

    while (attempt < MAX_RETRIES && !success) {
      attempt++;
      try {
        const res = await fetch(`${SERVER_URL}/api/upload-chunk`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(payload),
        });

        if (!res.ok) {
          let errorText = `HTTP error ${res.status}`;
          try {
            const errJson = await res.json();
            if (errJson && errJson.message) errorText = errJson.message;
          } catch (_) {}
          throw new Error(errorText);
        }

        const json = (await res.json()) as ChunkUploadResponse;
        if (json.status !== "success") {
          throw new Error(json.message || "Chunk upload failed on server");
        }

        success = true;
      } catch (err: any) {
        lastError = err;
        console.warn(
          `[CHUNK-UPLOAD] Chunk ${chunkIndex + 1}/${totalChunks} attempt ${attempt} failed:`,
          err.message
        );
        if (attempt < MAX_RETRIES) {
          await new Promise((resolve) => setTimeout(resolve, attempt * 1000));
        }
      }
    }

    if (!success) {
      throw new Error(
        `Failed to upload chunk ${chunkIndex + 1}/${totalChunks} after ${MAX_RETRIES} attempts: ${lastError?.message || "Unknown error"}`
      );
    }

    uploadedBytes += end - start;
    if (onProgress) {
      const pct = Math.min(100, Math.round((uploadedBytes / totalSize) * 100));
      onProgress(pct);
    }
  }

  return uploadId;
}
