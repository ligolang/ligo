import { validateAddress } from "@taquito/utils";

const validate = (v: string, reg: RegExp, message: string) => !reg.test(v) && message;

const invalidFileFilderChars = /[/∕]+/;

const validNameRegexp = /^[a-zA-Z0-9.\-_~!$&'()*+,;=:@ ]+$/;

const validUrlRegexp = /^(http(s)?:\/\/)\w+[^\s]+(\.[^\s]+){1,}$/;

const validGitRegexp = /^(http(s)?:\/\/)\w+[^\s]+(\.[^\s]+){1,}.git$/;

const validGistIdRegexp = /^[0-9A-Fa-f]{32}$/;

export const validName = (v: string) =>
  validate(
    v,
    validNameRegexp,
    "Names can only contain letters, digits, spaces and special characrets: .-_~!$&'()*+,;=:@"
  );

export const validUrl = (v: string) => validate(v, validUrlRegexp, "URL has to be a network url");

export const validGit = (v: string) =>
  validate(v, validGitRegexp, "Git url has to be an url with .git on the end");

export const validGistId = (v: string) =>
  validate(
    v,
    validGistIdRegexp,
    "Gist ID should be 32 chars string with digits and letters from A to F"
  );

export const validAddress = (v: string) => {
  const valid = validateAddress(v);
  if (valid === 1) {
    return "Invalid address checksum";
  }
  if (valid === 2) {
    return "Invalid address length";
  }

  if (valid === 0) {
    return "Invalid address prefix";
  }

  return false;
};

export const validInt = (value: string) => {
  try {
    BigInt(value);
    return false;
  } catch {
    return true;
  }
};

export const validFileFolderName = (v: string) =>
  invalidFileFilderChars.test(v) && "File and folder named should not include '/' and U+2215 '∕'";
