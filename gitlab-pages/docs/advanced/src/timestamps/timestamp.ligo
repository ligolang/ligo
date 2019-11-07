const today: timestamp = now;
const one_day: int = 86400;
const in_24_hrs: timestamp = today + one_day;

const today: timestamp = now;
const one_day: int = 86400;
const a_24_hrs_ago: timestamp = today - one_day;

const not_tommorow: bool = (now = in_24_hrs)