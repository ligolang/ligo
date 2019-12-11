adts = [
  # typename, variant?, fields_or_ctors
  ("root", True, [
      # ctor, builtin, type
      ("A", False, "a"),
      ("B", True, "int"),
      ("C", True, "string"),
  ]),
  ("a", False, [
    ("a1", False, "ta1"),
    ("a2", False, "ta2"),
  ]),
  ("ta1", True, [
      ("X", False, "root"),
      ("Y", False, "ta2"),
  ]),
  ("ta2", True, [
      ("Z", False, "ta2"),
  ]),
]

print "type 'state fold_config = {"
for (t, is_variant, ctors) in adts:
  tt = ("%s'" % (t,)) # output type t'
  print ("    %s   : %s   -> 'state -> ('state continue_fold) -> (%s  * 'state) ;" % (t, t, tt))
  for (c, builtin, ct,) in ctors:
    if builtin:
      ctt = ct # TODO: use a wrapper instead of a' for the intermediate steps, and target a different type a' just to change what the output type is
    else:
      ctt = ("%s'" % (ct,))
    print ("    %s_%s : %s      -> 'state -> ('state continue_fold) -> (%s     * 'state) ;" % (t, c, ct, ctt))
print "  }"
print ""

print "let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->"
print "  {"
for (t, is_variant, ctors) in adts:
  print ("    %s    = fold_%s    visitor ;" % (t, t))
print "  }"
print ""

for (t, is_variant, ctors) in adts:
  v = t # visitor field
  tt = ("%s'" % (t,)) # output type t'
  print ("and fold_%s : type state . state fold_config -> %s -> state -> (%s * state) = fun visitor x state ->" % (t, t, tt,))
  print "  let continue_fold : state continue_fold = mk_continue_fold visitor in"
  print "  match x with"
  if is_variant:
    for (c, builtin, ct,) in ctors:
      cc = ("%s'" % (c,))
      print ("  | %s v ->" % (c,))
      print ("    let (v, state) = visitor.%s_%s v state continue_fold in" % (t, c,))
      if not builtin:
        print ("    let (v, state) = visitor.%s v state continue_fold in" % (ct,))
      print ("    (%s v, state)" % (cc,))
  else:
    print "  | {"
    for (f, builtin, ft,) in ctors:
      print ("      %s;" % (f,))
    print "  } ->"
    for (f, builtin, ft,) in ctors:
      ff = ("%s'" % (f,))
      print ("    let (%s, state) = visitor.%s_%s %s state continue_fold in" % (f, t, f, f,))
      if not builtin:
        print ("    let (%s, state) = visitor.%s %s state continue_fold in" % (ff, ft, f,))
    print "    ({"
    for (f, builtin, ft,) in ctors:
      ff = ("%s'" % (f,))
      print ("       %s;" % (ff,))
    print "     }, state)"
  print ""
