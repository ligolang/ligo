const getChar = (s: string, idx: nat): string => String.sub(idx, 1 as nat, s)

const isPalindrome = (s: string): bool => {
  let p = "";
  let length = String.length(s);
  for (let i = length - 1 ; i >= 0 ; i--) {
    p += getChar(s, abs(i));
  }
  return p == s;
}

const isPalindrome_ = (s: string): bool => {
  let length = String.length(s);
  let isP = true;
  for (let i = 0, j = length - 1 ;  i <= j ; i++, j--) {
    isP = isP && getChar(s, abs(i)) == getChar(s, abs(j))
  }
  return isP;
}

const testPalindrome = (() => {
  const abba = "abba";
  Test.assert(isPalindrome(abba));
  Test.assert(isPalindrome_(abba));
  const ababa = "ababa";
  Test.assert(isPalindrome(ababa));
  Test.assert(isPalindrome_(ababa));
  const abcd = "abcd";
  Test.assert(!isPalindrome(abcd));
  Test.assert(!isPalindrome_(abcd));
  const abcde = "abcde";
  Test.assert(!isPalindrome(abcde));
  Test.assert(!isPalindrome_(abcde));
})();