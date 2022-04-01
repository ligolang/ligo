let sub = ((store, delta) : (tez, tez)) : tez => store - delta;

let main = ((_, store) : (unit, tez)) : (list(operation), tez) => {
 ( ([] : list (operation)), sub((store, 1tez)))
};