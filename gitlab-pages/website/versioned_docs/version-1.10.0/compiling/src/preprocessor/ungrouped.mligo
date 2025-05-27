type storage =
  [@layout:comb]
  { tokenPool : nat ;
    xtzPool : tez ;
    lqtTotal : nat ;
    selfIsUpdatingTokenPool : bool ;
    freezeBaker : bool ;
    manager : address ;
    tokenAddress : address ;
#if FA2
    tokenId : nat ;
#endif
    lqtAddress : address ;
  }