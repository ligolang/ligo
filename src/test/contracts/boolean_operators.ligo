// Test PascaLIGO boolean operators

function or_true (const b : bool) : bool is
  begin skip end with b or True

function or_false (const b : bool) : bool is
  begin skip end with b or False

function and_true (const b : bool) : bool is
  begin skip end with b and True

function and_false (const b : bool) : bool is
  begin skip end with b and False

function not_bool (const b: bool) : bool is
  begin skip end with not b
