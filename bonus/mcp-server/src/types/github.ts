export interface GithubRelease {
  tag_name: string;
  name: string;
  assets: GithubAsset[];
  published_at: string;
}

export interface GithubAsset {
  name: string;
  browser_download_url: string;
  size: number;
}

export interface BinaryInfo {
  version: string;
  path: string;
  lastUpdate: Date;
}

export interface BinariesState {
  compiler?: BinaryInfo;
  vm?: BinaryInfo;
}

export interface GithubContent {
  name: string;
  path: string;
  sha: string;
  size: number;
  url: string;
  html_url: string;
  git_url: string;
  download_url: string | null;
  type: "file" | "dir";
}

export interface DocInfo {
  sha: string;
  path: string;
  lastUpdate: Date;
}

export interface DocsState {
  [docName: string]: DocInfo;
}
