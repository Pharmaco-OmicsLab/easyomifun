import { defineConfig, loadEnv } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), "");
  const base = env.VITE_APP_BASE ?? "/";
  const normalizedBase = base.endsWith("/") ? base : `${base}/`;
  const backendTarget = env.VITE_API_PROXY_TARGET || "http://localhost:8080";

  return {
    base: normalizedBase,
    plugins: [
      react(),
      tailwindcss(),
    ],
    server: {
      proxy: {
        '/api': {
          target: backendTarget,
          changeOrigin: true,
          secure: false,
        }
      }
    }
  };
})
