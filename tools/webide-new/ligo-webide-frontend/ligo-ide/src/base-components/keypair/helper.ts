export const keypairNameValidator = (v: string) =>
  !/^[0-9a-zA-Z\-_]*$/.test(v) &&
  "Keypair name can only contain letters, digits, dash or underscore.";
