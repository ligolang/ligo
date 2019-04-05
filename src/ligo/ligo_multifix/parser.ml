
module MenhirBasics = struct
  
  exception Error
  
  type token = Lex.Token.token
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState30
  | MenhirState27
  | MenhirState25
  | MenhirState22
  | MenhirState20
  | MenhirState17
  | MenhirState15
  | MenhirState11
  | MenhirState5
  | MenhirState0

# 1 "parser.mly"
  
    open Ast

# 39 "parser.ml"

let rec _menhir_goto_arith_0 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_arith_0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv129 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | Lex.Token.LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Lex.Token.LIST ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Lex.Token.NAME _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv131 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv137 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0)) * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv135 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0)) * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_a_, _, (a : 'tv_variable), _startpos_a_), _endpos_b_, _, (b : 'tv_arith_0)), _endpos_c_, _, (c : 'tv_arith_0)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos_c_ in
        let _v : 'tv_arith_0 = let _endpos = _endpos_c_ in
        let _startpos = _startpos__1_ in
        
# 95 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Let_in (a , b , c)
    )
# 92 "parser.ml"
         in
        _menhir_goto_arith_0 _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv136)) : 'freshtv138)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_arith_0)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 111 "parser.ml"
            ) = let _1 = 
# 47 "parser.mly"
                        ( _1 )
# 115 "parser.ml"
             in
            
# 33 "parser.mly"
                        ( _1 )
# 120 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 128 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 136 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 144 "parser.ml"
            )) : (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 148 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv140)) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * Lexing.position * _menhir_state * 'tv_arith_0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
    | _ ->
        _menhir_fail ()

and _menhir_goto_arith_1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_arith_1 -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.MINUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.LIST ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Lex.Token.NAME _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv120)
    | Lex.Token.PLUS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.LIST ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Lex.Token.NAME _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv122)
    | Lex.Token.EOF | Lex.Token.IN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_arith_1), _startpos__1_) = _menhir_stack in
        let _endpos = _endpos__1_ in
        let _v : 'tv_arith_0 = 
# 99 "parser.mly"
            ( _1 )
# 205 "parser.ml"
         in
        _menhir_goto_arith_0 _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv124)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)

and _menhir_run20 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.LIST ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Lex.Token.NAME _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.LIST ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Lex.Token.NAME _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState22 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_arith_2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_arith_2 -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 | MenhirState30 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.TIMES ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.EOF | Lex.Token.IN | Lex.Token.MINUS | Lex.Token.PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv101 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_arith_2), _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__1_ in
            let _v : 'tv_arith_1 = 
# 91 "parser.mly"
            ( _1 )
# 272 "parser.ml"
             in
            _menhir_goto_arith_1 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv103 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.TIMES ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.EOF | Lex.Token.IN | Lex.Token.MINUS | Lex.Token.PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv107 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_arith_1), _startpos_a_), _endpos_b_, _, (b : 'tv_arith_2), _startpos_b_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_arith_1 = let _endpos = _endpos_b_ in
            let _startpos = _startpos_a_ in
            
# 81 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Addition (a , b)
    )
# 307 "parser.ml"
             in
            _menhir_goto_arith_1 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv109 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)) : 'freshtv112)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv117 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.TIMES ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | Lex.Token.EOF | Lex.Token.IN | Lex.Token.MINUS | Lex.Token.PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv113 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_arith_1), _startpos_a_), _endpos_b_, _, (b : 'tv_arith_2), _startpos_b_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_arith_1 = let _endpos = _endpos_b_ in
            let _startpos = _startpos_a_ in
            
# 87 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Substraction (a , b)
    )
# 342 "parser.ml"
             in
            _menhir_goto_arith_1 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv115 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)) : 'freshtv118)
    | _ ->
        _menhir_fail ()

and _menhir_goto_lead_list_content_SEMICOLON_arith_3_ : _menhir_env -> 'ttv_tail -> 'tv_lead_list_content_SEMICOLON_arith_3_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99 * 'tv_lead_list_content_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * 'tv_lead_list_content_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.LIST ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Lex.Token.NAME _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv94)
    | Lex.Token.RSQUARE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * 'tv_lead_list_content_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, (_1 : 'tv_lead_list_content_SEMICOLON_arith_3_)) = _menhir_stack in
        let _v : 'tv_lead_list_SEMICOLON_arith_3_ = 
# 22 "parser.mly"
                                    ( _1 )
# 384 "parser.ml"
         in
        _menhir_goto_lead_list_SEMICOLON_arith_3_ _menhir_env _menhir_stack _v) : 'freshtv96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * 'tv_lead_list_content_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv98)) : 'freshtv100)

and _menhir_goto_arith_3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_arith_3 -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * 'tv_option_SEMICOLON_) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_3) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * 'tv_option_SEMICOLON_) = Obj.magic _menhir_stack in
        let (_endpos_x_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((x : 'tv_arith_3) : 'tv_arith_3) = _v in
        let (_startpos_x_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, (_1 : 'tv_option_SEMICOLON_)) = _menhir_stack in
        let _v : 'tv_lead_list_first_SEMICOLON_arith_3_ = 
# 29 "parser.mly"
                            ( [ x ] )
# 414 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71) = _menhir_stack in
        let (_v : 'tv_lead_list_first_SEMICOLON_arith_3_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        let (_v : 'tv_lead_list_first_SEMICOLON_arith_3_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        let ((x : 'tv_lead_list_first_SEMICOLON_arith_3_) : 'tv_lead_list_first_SEMICOLON_arith_3_) = _v in
        ((let _v : 'tv_lead_list_content_SEMICOLON_arith_3_ = 
# 25 "parser.mly"
                                      ( x )
# 428 "parser.ml"
         in
        _menhir_goto_lead_list_content_SEMICOLON_arith_3_ _menhir_env _menhir_stack _v) : 'freshtv68)) : 'freshtv70)) : 'freshtv72)) : 'freshtv74)) : 'freshtv76)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * 'tv_lead_list_content_SEMICOLON_arith_3_)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_3) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * 'tv_lead_list_content_SEMICOLON_arith_3_)) = Obj.magic _menhir_stack in
        let (_endpos_x_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((x : 'tv_arith_3) : 'tv_arith_3) = _v in
        let (_startpos_x_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, (xs : 'tv_lead_list_content_SEMICOLON_arith_3_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_lead_list_content_SEMICOLON_arith_3_ = 
# 26 "parser.mly"
                                                         ( xs @ [ x ] )
# 449 "parser.ml"
         in
        _menhir_goto_lead_list_content_SEMICOLON_arith_3_ _menhir_env _menhir_stack _v) : 'freshtv78)) : 'freshtv80)
    | MenhirState0 | MenhirState30 | MenhirState27 | MenhirState25 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_3) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_arith_3) : 'tv_arith_3) = _v in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_arith_2 = 
# 77 "parser.mly"
            ( _1 )
# 470 "parser.ml"
         in
        _menhir_goto_arith_2 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv82)) : 'freshtv84)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_3) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos_b_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((b : 'tv_arith_3) : 'tv_arith_3) = _v in
        let (_startpos_b_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_arith_2), _startpos_a_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_arith_2 = let _endpos = _endpos_b_ in
        let _startpos = _startpos_a_ in
        
# 67 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Multiplication (a , b)
    )
# 498 "parser.ml"
         in
        _menhir_goto_arith_2 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv86)) : 'freshtv88)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_3) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos_b_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((b : 'tv_arith_3) : 'tv_arith_3) = _v in
        let (_startpos_b_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_arith_2), _startpos_a_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_b_ in
        let _v : 'tv_arith_2 = let _endpos = _endpos_b_ in
        let _startpos = _startpos_a_ in
        
# 73 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Division (a , b)
    )
# 526 "parser.ml"
         in
        _menhir_goto_arith_2 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv90)) : 'freshtv92)
    | _ ->
        _menhir_fail ()

and _menhir_goto_lead_list_SEMICOLON_arith_3_ : _menhir_env -> 'ttv_tail -> 'tv_lead_list_SEMICOLON_arith_3_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv65 * _menhir_state * Lexing.position)) * 'tv_lead_list_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.RSQUARE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * _menhir_state * Lexing.position)) * 'tv_lead_list_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state * Lexing.position)) * 'tv_lead_list_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        let (_endpos__4_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), (a : 'tv_lead_list_SEMICOLON_arith_3_)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_arith_3 = let _endpos = _endpos__4_ in
        let _startpos = _startpos__1_ in
        
# 59 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ List (a)
    )
# 562 "parser.ml"
         in
        _menhir_goto_arith_3 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv60)) : 'freshtv62)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63 * _menhir_state * Lexing.position)) * 'tv_lead_list_SEMICOLON_arith_3_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)) : 'freshtv66)

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv57 * 'tv_option_SEMICOLON_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.LIST ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Lex.Token.NAME _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv58)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv37 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) * Lexing.position * _menhir_state * 'tv_arith_0)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * Lexing.position * _menhir_state * 'tv_arith_1 * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * Lexing.position * _menhir_state * 'tv_arith_2 * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * 'tv_lead_list_content_SEMICOLON_arith_3_)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv52)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * 'tv_option_SEMICOLON_) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv54)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv56)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 4 "lex/token.mly"
       (string)
# 644 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 4 "lex/token.mly"
       (string)
# 655 "parser.ml"
    )) : (
# 4 "lex/token.mly"
       (string)
# 659 "parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_variable = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    
# 38 "parser.mly"
  (
    let loc = Location.make _startpos _endpos in
    Location.wrap ~loc _1
  )
# 672 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv33) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 | MenhirState30 | MenhirState27 | MenhirState25 | MenhirState22 | MenhirState20 | MenhirState17 | MenhirState11 | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * 'tv_variable * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * 'tv_variable * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_variable), _startpos_a_) = _menhir_stack in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_a_ in
        let _v : 'tv_arith_4 = let _endpos = _endpos_a_ in
        let _startpos = _startpos_a_ in
        
# 52 "parser.mly"
    (
      let loc = Location.make _startpos _endpos in
      Location.wrap ~loc @@ Arith_variable (a)
    )
# 698 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_4) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_arith_4) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_arith_4) : 'tv_arith_4) = _v in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_arith_3 = 
# 63 "parser.mly"
            ( _1 )
# 723 "parser.ml"
         in
        _menhir_goto_arith_3 _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv18)) : 'freshtv20)) : 'freshtv22)) : 'freshtv24)) : 'freshtv26)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv27 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | Lex.Token.LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Lex.Token.LIST ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Lex.Token.NAME _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_variable * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)) : 'freshtv34)) : 'freshtv36)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.LSQUARE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lex.Token.SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
            ((let x = () in
            let _v : 'tv_option_SEMICOLON_ = 
# 116 "/home/user/.opam/tezos/lib/menhir/standard.mly"
    ( Some x )
# 778 "parser.ml"
             in
            _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _v) : 'freshtv4)) : 'freshtv6)
        | Lex.Token.LIST | Lex.Token.NAME _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
            ((let _v : 'tv_option_SEMICOLON_ = 
# 114 "/home/user/.opam/tezos/lib/menhir/standard.mly"
    ( None )
# 787 "parser.ml"
             in
            _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _v) : 'freshtv8)
        | Lex.Token.RSQUARE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
            ((let _v : 'tv_lead_list_SEMICOLON_arith_3_ = 
# 21 "parser.mly"
    ( [] )
# 796 "parser.ml"
             in
            _menhir_goto_lead_list_SEMICOLON_arith_3_ _menhir_env _menhir_stack _v) : 'freshtv10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv11 * _menhir_state * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)) : 'freshtv14)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.NAME _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and entry_point : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 5 "parser.mly"
       (Ast.entry_point Location.wrap)
# 842 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lex.Token.LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Lex.Token.LIST ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Lex.Token.NAME _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "/home/user/.opam/tezos/lib/menhir/standard.mly"
  

# 875 "parser.ml"
