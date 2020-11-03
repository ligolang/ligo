let error1 =
  let fake_addr = ("tz1PpDGHRXFQq3sYDuH8EpLWzPm5PFpe1sLE": address) in
  let a = Test.external_call fake_addr 123 1tz in
  true