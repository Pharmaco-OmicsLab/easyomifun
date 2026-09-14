import { HomeWeb } from "./HomeWeb";
import { HomeElectron } from "./HomeElectron";
import { isDesktopMode } from "../lib/dataParser";

export default function Home() {
  const isDesktop = import.meta.env.VITE_ELECTRON === "true" || isDesktopMode();
  return isDesktop ? <HomeElectron /> : <HomeWeb />;
}

