export type LangConfiguration = {
  comments: {
    lineComment: string;
    blockComment: [string, string];
  };
  brackets: [string, string][];
  autoClosingPairs: {
    open: string;
    close: string;
  }[];
  surroundingPairs: {
    open: string;
    close: string;
  }[];
};
