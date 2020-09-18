type tokens = big_map (address, nat)
type allowances = big_map ((address, address), nat) /* (sender,account) -> value */

type storage = {
  tokens      : tokens,
  allowances  : allowances,
  total_amount : nat,
}

type transfer = {
	address_from : address,
	address_to   : address,
	value        : nat,
}

type approve = {
	spender : address,
	value   : nat,
}

type getAllowance = {
	owner    : address,
	spender  : address,
	callback : contract (nat),
}

type getBalance = {
	owner    : address,
	callback : contract (nat),
}

type getTotalSupply = {
	callback : contract (nat),
}

type action =
|	Transfer       ( transfer )
|	Approve        ( approve )
|	GetAllowance   ( getAllowance )
|	GetBalance     ( getBalance )
|	GetTotalSupply ( getTotalSupply )

let transfer = ((p,s) : (transfer, storage)) : (list (operation), storage) => {
   let new_allowances =   
		if (Tezos.sender == p.address_from) { s.allowances; }
		else {
			let authorized_value = switch (Big_map.find_opt ((Tezos.sender,p.address_from), s.allowances)) {
			|	Some value => value
			|	None       => 0n
			};
			if (authorized_value < p.value) { (failwith ("Not Enough Allowance") : allowances); }
			else { Big_map.update ((Tezos.sender,p.address_from), (Some (abs(authorized_value - p.value))), s.allowances); };
		};
	let sender_balance = switch (Big_map.find_opt (p.address_from, s.tokens)) {
	|	Some value => value
	|	None       => 0n
	};
	if (sender_balance < p.value) { (failwith ("Not Enough Balance") : (list (operation), storage)); }
	else {
		let new_tokens = Big_map.update (p.address_from, (Some (abs(sender_balance - p.value))), s.tokens);
		let receiver_balance = switch (Big_map.find_opt (p.address_to, s.tokens)) {
		|	Some value => value
		|	None       => 0n
		};
		let new_tokens = Big_map.update (p.address_to, (Some (receiver_balance + p.value)), new_tokens);
		(([]: list (operation)), { ...s,tokens:new_tokens, allowances:new_allowances});
	};
};

let approve = ((p,s) : (approve, storage)) : (list (operation), storage) => {
	let previous_value = switch (Big_map.find_opt ((p.spender, Tezos.sender), s.allowances)){
	|	Some value => value
	|	None => 0n
	};
	if (previous_value > 0n && p.value > 0n)
	{ (failwith ("Unsafe Allowance Change") : (list (operation), storage)); }
	else {
		let new_allowances = Big_map.update ((p.spender, Tezos.sender), (Some (p.value)), s.allowances);
		(([] : list (operation)), { ...s, allowances : new_allowances});
	};
};

let getAllowance = ((p,s) : (getAllowance, storage)) : (list (operation), storage) => {
	let value = switch (Big_map.find_opt ((p.owner, p.spender), s.allowances)) {
	|	Some value => value
	|	None => 0n
	};
	let op = Tezos.transaction (value, 0mutez, p.callback);
	([op],s)
};

let getBalance = ((p,s) : (getBalance, storage)) : (list (operation), storage) => {
	let value = switch (Big_map.find_opt (p.owner, s.tokens)) {
	|	Some value => value
	|	None => 0n
	};
	let op = Tezos.transaction (value, 0mutez, p.callback);
	([op],s)
};

let getTotalSupply = ((p,s) : (getTotalSupply, storage)) : (list (operation), storage) => {
  let total = s.total_amount;
  let op    = Tezos.transaction (total, 0mutez, p.callback);
  ([op],s)
};


let main = ((a,s): (action, storage)) =>  
 	switch a {
   |	Transfer p => transfer ((p,s))
	|	Approve  p => approve ((p,s))
	|	GetAllowance p => getAllowance ((p,s))
	|  GetBalance p => getBalance ((p,s))
	|	GetTotalSupply p => getTotalSupply ((p,s))
	};
