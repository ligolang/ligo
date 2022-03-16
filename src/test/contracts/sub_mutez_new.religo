let sub = ((store, delta) : (tez, tez)) : option(tez) => store - delta;

let main = ((_, store) : (unit, tez)) : (list(operation), tez) => {
 ( ([] : list (operation)), Option.unopt(sub((store, 1tez))))
};