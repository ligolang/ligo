moduleName = "A"
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
      ("W", True, "unit"),
  ]),
]

from collections import namedtuple
adt = namedtuple('adt', ['name', 'newName', 'isVariant', 'ctorsOrFields'])
ctorOrField = namedtuple('ctorOrField', ['name', 'newName', 'isBuiltin', 'type_', 'newType'])
adts = [
  adt(
    name = name,
    newName = f"{name}'",
    isVariant = isVariant,
    ctorsOrFields = [
      ctorOrField(
        name = cf,
        newName = f"{cf}'",
        isBuiltin = isBuiltin,
        type_ = type_,
        newType = type_ if isBuiltin else f"{type_}'",
      )
      for (cf, isBuiltin, type_) in ctors
    ],
  )
  for (name, isVariant, ctors) in adts
]

print("open %s" % moduleName)

print("")
for (index, t) in enumerate(adts):
  typeOrAnd = "type" if index == 0 else "and"
  print(f"{typeOrAnd} {t.newName} =")
  if t.isVariant:
    for c in t.ctorsOrFields:
      print(f"  | {c.newName} of {c.newType}")
  else:
    print("  {")
    for f in t.ctorsOrFields:
      print(f"    {f.newName} : {f.newType} ;")
    print("  }")


# print("")
# print("type 'state continue_fold =")
# print("  {")
# for t in adts:
#   print(f"    {t.name} : {t.name} -> 'state -> ({t.newName} * 'state) ;")
# print("  }")

def folder(name, extraArgs):
  print("")
  print(f"type 'state {name} =")
  print("  {")
  for t in adts:
    print(f"    {t.name} : {t.name} -> 'state{extraArgs} -> ({t.newName} * 'state) ;")
    for c in t.ctorsOrFields:
      print(f"    {t.name}_{c.name} : {c.type_} -> 'state{extraArgs} -> ({c.newType} * 'state) ;")
  print("  }")

folder("continue_fold", "")
folder("fold_config", " -> ('state continue_fold)")

print("")
print('(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)')
print("let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->")
print("  {")
for t in adts:
  print(f"    {t.name} = fold_{t.name} visitor ;")
  for c in t.ctorsOrFields:
    print(f"    {t.name}_{c.name} = fold_{t.name}_{c.name} visitor ;")
print("}")
print("")

for t in adts:
  print(f"and fold_{t.name} : type state . state fold_config -> {t.name} -> state -> ({t.newName} * state) = fun visitor x state ->")
  print("  let continue_fold : state continue_fold = mk_continue_fold visitor in")
  print(f"  visitor.{t.name} x state continue_fold")
  print("")
  for c in t.ctorsOrFields:
    print(f"and fold_{t.name}_{c.name} : type state . state fold_config -> {c.type_} -> state -> ({c.newType} * state) = fun visitor x state ->")
    print("  let continue_fold : state continue_fold = mk_continue_fold visitor in")
    print(f"  visitor.{t.name}_{c.name} x state continue_fold")
    print("")

  # print("  match x with")
  # if t.isVariant:
  #   for c in t.ctorsOrFields:
  #     print(f"  | {c.name} v ->")
  #     print(f"    let (v', state) = visitor.{t.name}_{c.name} v state continue_fold in")
  #     print(f"    ({c.newName} v', state)")
  # else:
  #   print("  | {", end=' ')
  #   for f in t.ctorsOrFields:
  #     print(f"{f.name};", end=' ')
  #   print("} ->")
  #   for f in t.ctorsOrFields:
  #     print(f"    let ({f.newName}, state) = visitor.{t.name}_{f.name} {f.name} state continue_fold in")
  #   print("    ({", end=' ')
  #   for f in t.ctorsOrFields:
  #     print(f"{f.newName};", end=' ')
  #   print("}, state)")
  # print("")
  # for c in t.ctorsOrFields:
  #   print(f"and fold_{t.name}_{c.name} : type state . state fold_config -> {c.type_} -> state -> ({c.newType} * state) = fun visitor x state ->")
  #   if c.isBuiltin:
  #     print("  ignore visitor; (x, state)")
  #   else:
  #     print("  let continue_fold : state continue_fold = mk_continue_fold visitor in")
  #     print(f"  visitor.{c.type_} x state continue_fold")
  # print("")

# print """let no_op : ('a -> unit) -> 'a fold_config = fun phantom -> failwith "todo" """

print("let no_op : 'a fold_config = {")
for t in adts:
  print(f"  {t.name} = (fun v state continue ->")
  print("    match v with")
  if t.isVariant:
    for c in t.ctorsOrFields:
      print(f"    | {c.name} v -> let (v, state) = continue.{t.name}_{c.name} v state in ({c.newName} v, state)")
  else:
    print("      {", end=' ')
    for f in t.ctorsOrFields:
      print(f"{f.name};", end=' ')
    print("} ->")
    for f in t.ctorsOrFields:
      print(f"      let ({f.newName}, state) = continue.{t.name}_{f.name} {f.name} state in")
    print("      ({", end=' ')
    for f in t.ctorsOrFields:
      print(f"{f.newName};", end=' ')
    print("}, state)")
  print("  );")
  for c in t.ctorsOrFields:
    print(f"  {t.name}_{c.name} = (fun v state continue ->", end=' ')
    if c.isBuiltin:
      print("ignore continue; (v, state)", end=' ')
    else:
      print(f"continue.{c.type_} v state", end=' ')
    print(") ;")
print("}")


 # (fun v state continue ->
 #    let (new_v, new_state) = match v with
 #        | A v -> let (v, state) = continue.a v state in (A' v, state)
 #        | B v -> let (v, state) = (fun x s -> (x,s)) v state in (B' v, state)
 #        | C v -> let (v, state) = (fun x s -> (x,s)) v state in (C' v, state)
 #    in
 #    (new_v, new_state)
 #  );






      # if not builtin:
      #   print ("    let (v', state) = match v' with None -> visitor.%s v state continue_fold | Some v' -> (v', state) in" % (ct,))
      # else:
      #   print "    let Some v' = v' in"

      # if not builtin:
      #   print ("    let (%s, state) = match %s with None -> visitor.%s %s state continue_fold | Some v' -> (v', state) in" % (ff, ff, ft, f))
      # else:
      #   print "    let Some v' = v' in"
