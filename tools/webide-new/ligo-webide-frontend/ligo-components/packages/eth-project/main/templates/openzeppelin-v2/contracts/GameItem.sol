// contracts/GameItem.sol
// SPDX-License-Identifier: MIT
pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC721/ERC721Full.sol";
import "openzeppelin-solidity/contracts/drafts/Counters.sol";

contract GameItem is ERC721Full {
  using Counters for Counters.Counter;
  Counters.Counter private _tokenIds;

  constructor() ERC721Full("GameItem", "ITM") public {
  }

  function awardItem(address player, string memory tokenURI) public returns (uint256) {
    _tokenIds.increment();

    uint256 newItemId = _tokenIds.current();
    _mint(player, newItemId);
    _setTokenURI(newItemId, tokenURI);

    return newItemId;
  }
}
