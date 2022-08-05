import { ExtendedFs } from "../filesystems/indexedDB";

declare global {
  interface Window {
    ligoIdeFileSystem: ExtendedFs;
  }
}
