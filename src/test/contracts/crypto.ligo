function hasherman512 (const s: bytes) : bytes is sha_512 (s)

function hasherman_blake (const s: bytes) : bytes is blake2b (s)
