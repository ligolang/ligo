// contracts/GLDToken.sol
// SPDX-License-Identifier: MIT
pragma solidity ^0.5.0;

import "openzeppelin-solidity/contracts/token/ERC20/ERC20.sol";
import "openzeppelin-solidity/contracts/token/ERC20/ERC20Detailed.sol";

contract GLDToken is ERC20, ERC20Detailed {
  constructor(uint256 initialSupply) ERC20Detailed("Gold", "GLD", 18) public {
    _mint(msg.sender, initialSupply);
  }
}