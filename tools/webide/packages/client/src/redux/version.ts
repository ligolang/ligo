export interface VersionState {
  revision: string;
  branch: string;
}

const DEFAULT_STATE: VersionState = {
  revision: 'dev',
  branch: 'dev'
};

export default (state = DEFAULT_STATE): VersionState => {
  return state;
};
