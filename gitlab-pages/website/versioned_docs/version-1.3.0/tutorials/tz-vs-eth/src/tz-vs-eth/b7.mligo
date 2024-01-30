type committee = {members : address list; quorum : nat}

type leader = {name : string; address : address}

type authority = Dictatorship of leader | Democracy of committee