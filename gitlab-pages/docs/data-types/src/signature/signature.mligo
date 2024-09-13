let my_sig : signature =
   ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
   signature)
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg