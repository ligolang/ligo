type sum_aggregator is 
  record [
    counter: int;
    sum: int;
  ]
  
function counter (const n: int) : int is
  {
    var initial : sum_aggregator = record [counter = 0; sum = 0];
    function aggregate (const _: sum_aggregator) : int is
      function (const prev : sum_aggregator) is prev.sum
  } with aggregate (initial)
    
type sum_aggregator is 
  record [
    counter: int;
    sum: int;
  ]

function counter (const n: int) : int is
  begin
    const initial : sum_aggregator = record [counter = 0; sum = 0];
    
    recursive function aggregate (const _: unit) : sum_aggregator -> int is
      function (const prev : sum_aggregator) is
        if prev.counter <= n then
          aggregate (record [counter = prev.counter + 1; 
                             sum     = prev.counter + prev.sum])
        else
          prev.sum
  end with (aggregate (Unit)) (initial)
