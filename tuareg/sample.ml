(* sample file indented as we want it to be -*- tuareg -*- *)

let server_comments request t =
  let module M = N in
  let class M = N in
  let m M = N in
  let module M = N in
  let open Grep.Server in
  let x = 5 in
  let modue x y = 5 in
  let open M in

  t >>= Grep.server_comments
    lazy
    parser
    every

let x =
  let open M in
  let x = 5 in
  x + x
;;

;; (* http://caml.inria.fr/mantis/view.php?id=4247 *)
let x = {
  Foo.
  a = b;
  c = d;
  e = {Bar.
       f = 1;
       g = 2;
      };
  h = {  Quux.
         i = 3;
         j = 4;
      };
}

;; (* http://caml.inria.fr/mantis/view.php?id=4249 *)
let x = { a = b;
          c = d;
        }

;; (* http://caml.inria.fr/mantis/view.php?id=4255 *)
{ foo: [ `Foo of int
       | `Bar of string ];
}

let s = { a with
  b = 1;
}
;;

let a = {
  M.
  foo = foo;
  bar = bar;
}

let a = { t with M.
  foo = foo;
  bar = bar;
}

let a = { t with
  M.
  foo = foo;
  bar = bar;
}

type t = [ `Foo of int
         | `Bar of string ]

type t =
| A
| B
| C
with sexp

type t = | A
         | B
         | C

type t = [
| `A
| `B
| `C
]

type t = [                              (* comment *)
| `A
| `B
| `C
]

type t = a
and x = b

module M = struct
  type t =
  | A
  | B
  | C
  with sexp

  type s = [
  | `A
  | `B
  | `C
  ]

  type u =
  | D
  | E
  with sexp
end

module N =
struct
  type u =
  | D
  | E
  with sexp
end

type m =
| T
with sexp

;; (* http://caml.inria.fr/mantis/view.php?id=4334 *)
type foo =
  a
  -> b
  -> c
  -> d

val f :
  a:a
  -> b:b
  -> c:c

type bar = a -> b
  -> c -> d
  -> e -> f

type baz = a -> b ->
  c -> d ->
  e -> f

val quux : a -> b ->
  c -> d ->
  e -> f

type t : a:b -> c:d
  -> e:f -> g

val f : a:b -> c:d
  -> e:f -> g

type t = {
  foo : (a
         -> b
         -> c
         -> d);
}

type t = {
  foo : (    a ->
             b ->
             c ->
             d);
}

type t = {
  foo : a
        -> b
        -> c
        -> d;
  bar :
    a
    -> b
    -> c;
}

type t = {
  foo : a ->
        b ->
        c ->
        d;
  bar :
    a ->
    b ->
    c;
}

type t = {
  a : B.t;
  c : D.t;

  e : F.t;

  g : H.t I.t;
  j :
    K.t L.t;
  m : N.t O.t;
  p :
    ((q:R.t
      -> s:T.U.t
      -> v:(W.t -> X.t option)
      -> y:(Z.t -> A.t -> B.t C.D.t E.t)
      -> f:(G.t -> H.t I.t option)
      -> j:(K.t -> L.t M.t option)
      -> n:(O.t -> p option)
      -> q:R.t
      -> s:(string -> unit) -> T.t
     )
     -> U.t
     -> V.W.t
     -> X.t);
  y : Z.t A.t;
  b : C.t D.t E.t;
  f : (G.t -> H.t -> I.t J.t);
} with sexp_of

type 'a v = id:O.t ->
  ssss:Ssss.t ->
  dddd:ddd.t ->
  t:S_m.t ->
  mmm:Safe_float.t ->
  qqq:int ->
  c:C.t ->
  uuuu:string option ->
  aaaaaa:Aaaaaa.t ->
  a:A.t ->
  rrrrr:Rrrrr.t ->
  time:Time.t ->
  typ:[ `L_p of Safe_float.t ] ->
  bazonk:present option ->
  o_p_e:O_m.t option ->
  only_hjkl:present option ->
  show_junk:int option ->
  d_p_o: Safe_float.t option ->
  asdf:present option ->
  generic:Sexp.t list ->
  'a

type 'a v =
  id:O.t
  -> ssss:Ssss.t
  -> dddd:ddd.t
  -> t:S_m.t
  -> mmm:Safe_float.t
  -> qqq:int
  -> c:C.t
  -> uuuu:string option
  -> aaaaaa:Aaaaaa.t
  -> a:A.t
  -> rrrrr:Rrrrr.t
  -> time:Time.t
  -> typ:[ `L_p of Safe_float.t ]
  -> bazonk:present option
  -> o_p_e:O_m.t option
  -> only_hjkl:present option
  -> show_junk:int option
  -> d_p_o: Safe_float.t option
  -> asdf:present option
  -> generic:Sexp.t list
  -> 'a

;; (* not in mantis *)
let bar x =
  if y
  then x
  else z

let zot x =
  quux ~f:(if x
    then y
    else z)

let zot x = quux ~f:(if x
  then y
  else z)

let () =
  if foo
  then bar
  else if foo1
  then zot
  else bazonk

let () =
  if foo
  then bar
  else
    if foo1
    then zot
    else bazonk

let _ =
  if until
  then _

let () =
  if a then (
    b
  ) else (
    c
  )

let rec count_append l1 l2 count =
  (* http://caml.inria.fr/resources/doc/guides/guidelines.en.html *)
  match l1 with
  | []               ->                         l2
  | [x1]             -> x1                   :: l2
  | [x1; x2]         -> x1 :: x2             :: l2
  | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    x1 :: x2 :: x3 :: x4 :: x5 ::
      (if count > 1000
       then slow_append tl l2
       else count_append tl l2 (count + 1))

let foo =
  (
    if a
    then b
    else c
  )

let quux list = List.map list ~f:(fun item ->
  print_item item
)

let foo x = function
  | Some _ -> true
  | None -> false

let bar x = fun u ->
  match u with
  | Some _ -> true
  | None -> false

let zot u = match u with
  | Some _ -> true
  | None -> false

let () = match x with
    Foo -> 1
  | Bar -> 2

let () =
  match x with
    Foo -> 1
  | Bar -> 2

let r x =
  try  f x;
       g x;
       y x;
  with e -> raise e

let g x =
  try let a = b in
      f x;
      g x;
      y x;
  with e -> raise e

let h x =
  try  ff a b
         c d;
       gg 1 2
         3 4;
  with e -> raise e

let () =
  try
    _
  with
    Bar -> ()

let () =
  (* http://caml.inria.fr/resources/doc/guides/guidelines.en.html *)
  try () with
  | e ->
    let x = z in

    yyyyy
      (a b)

let d x = function
  (* FIXME: should we leave it like this or align "|" with "match"?
     I chose with "match" because it looks otherwise odd and is more
     consistent with the "try" alignments above.  *)
  | A -> (match x with
        | X ->
          false
        | Y -> true
        |  Z ->
          false)
  | B -> false

let a f = function
  | A ->
    1
  |   B ->
    2
  |      C ->
    (function
    |  X  ->
      a
    | Y ->
      b) 12
  | D ->
    (match z with
    | 4 -> 3
    |  5 -> 7)

let x = foo ~f:(fun _ -> 0              (* comment *)
)

let f x =
  (let y = x in
   f x;
   g y;
   h z)

let f x =
  (let y = x in
   f x);
  g y;
  h z

let g y =
  a b;
  c d;
  e f;
  (* comment *)
  g h;
  i j

let () =
  (let a = 1 in
   let b = 2 in
   ( a,
     b))

let () =
  ((a b
      c d e,
    f g h),
   ( i j
       k l,
     m n
       o p))

let () =
  if a
  then
    let b = P.s ~b ~a ~m in
    a +. e *. b,
    b -. e *. b
  else
    q.a -. s *. z,
    q.b +. s *. z

let () =
  (* comment *)
  (let x =
     3
   in
   x + 5)

let foo = 1 and bar = 2 and zot = 3 in
let quux = 4 in
foo
+ bar
+ zot
+ quux

(* indent comment to following code *)
let () =
  try                                   (* foo!
                                           bar *)
    let a = f g c d in
    a b
  with _ -> ()

let () = try
           f x;
  with _ -> ()

let () = (try
            f x;
  with _ -> ())

let () =
  foo (sprintf ("a: %s"
                ^ " b: %s")
         a
         b)

let () =
  try f a
  with A () ->
    ()
  | B () ->
    ()
  |     C () ->
    ()

let f errors input =
  let ( @@ ) string bool = if not bool then errors := string :: !errors in
  input @@ false

let x =
  if mode = foo then bar;
  conn
  >>| fun x -> x + 1
  >>| fun x -> x + 1
  >>| fun x -> x + 1

let () =
  match _ with
  | foo ->
    bar
    >>| function _ ->
      _

let () =
  a
  >>= fun () ->
  b
  >>= fun () ->
  Deferred.all

let x =
  v
  >>= fun x -> y
  >>= fun z -> w
  >>= fun q -> r

let x =
  v 1 2
    3 4
    5 6 >>= fun x ->
  y+1 >>= (* foo! *) fun z ->
  f 1 2 3
    4 5 6 >>= fun y ->
  w*3 >>= fun q -> r

(* this does not work, see comment in tuareg-compute-arrow-indent
 * workaround: wrap code in parens *)
(* let () =
 *   match
 *     a 1 2 3
 *       4 5 6 >>= fun a ->
 *     b >>= fun b ->
 *     c
 *   with
 *   | A -> _ *)

let () =
  match
    let a = a in
    let b = b in
    c
  with
  | A -> _

let () =
  match
    (a >>= fun a ->
     b >>= fun b ->
     c)
  with
    A -> _

let f t =
  let (a, b) = to_open in
  let c = g t a b in
  ()

let () =
  begin
    foo bar
  end
  >>= fun () ->
  begin
    foo
      bar
  end
  >>= fun () ->
  ()

let () =
  (
    foo bar
  )
  >>= fun () ->
  (
    foo
      bar
  )
  >>= fun () ->
  ()

let () =
  match e with
  | `T d ->
    notify `O `T d;
    cancel t u ~now

let () =
  let a = 1
  and b = 2
  and c = 3 in
  a + b + c

let _ =
  foo bar
  || snoo blue

let _ =
  (
    foo bar
    || snoo blue
  )

let _ =
  (foo bar
   || snoo blue)

let () =
  Config.load ()
  >>> fun config ->
  let quux = config.Config.bazonk.Config.Bazonk.quux in
  load_quux ~input quux config
  >>> fun quux ->
  let da = Poo.Snapshot.merge quux in
  load_foobar config ~input
  >>> fun foobar ->
  whatever foobar

let () =
  a
  >>> fun () ->
  b

let () =
  a
  >>= function
  | b -> c
  | d ->
    e
    >>= f

let () =
  foo >>> fun bar ->
  baz >>> fun zot ->
  quux

let () =
  Config.load ()
  >>> fun config ->
  let quux = x in
  x
  >>= fun quux ->
  x

let () =
  Config.load ()
  >>= fun config ->
  let quux = x in
  x
  >>= fun quux ->
  x

let () =
  Hashtbl.iter times ~f:(fun ~key:time ~data:azot ->
    Clock.at time
    >>> fun () ->
    Db.iter t.db ~f:(fun dbo ->
      if S.mem azot (Dbo.azo dbo) then
        Dbo.dont dbo))

let () =
  f 1
  |! (fun x ->
    g x x)
  |! (fun y ->
    h y y)

let () =
  (let a,b = match c with
    | D -> e,f
    | G -> h,i in
   let j = a + b in
   j * j),
  12

module type M = M2
  with type t1 = int
   and type t2 = int
   and module S = M3
  with type t2 = int
  with type t3 = int

let () =
  try
    match () with
    | () -> ()
  with _ -> ()

let () =
  try
    ()
  with _ -> ()

let () =
  (  try ()
     with _ -> ())

let x =
  foo ~bar
  @ snoo

let x =
  foo ~bar:snoo
  @ snoo

let () =
  IO.println out (tagL "ol" (List.map ~f:(tag ~a:[] "li") (
    (List.map results ~f:(fun (what,_) ->
      tag "a" ~a:[("href","#" ^ what)] (what_title what)))
    @ [tag "a" ~a:[("href","#" ^ message_id)] message_title;
       tag "a" ~a:[("href","#" ^ legend_id)] legend_title])))

let x =
  let y =
    (a
     ^ b
     ^ c) in
  f ~a:b ?c:d
    ?e:f ~g:(h i j)
    ~k:(l m)
    (n o p)

let () =
  foobar (fun () ->
    step1
    >>= fun () -> step2)

let w f =
  List.map f ~f:(fun (a, b) ->
    L.r a
    >>= function
    | Ok s -> `Fst (b, s)
    | Error e -> `Snd (b, a, e))

class c (a : b) =
object
  inherit d
  method m = 1
end

let f = {
  a = 1;
}

let f a = {
  a = a;
}

let f a
    b = {
  a = a;
  b = b;
}

let () =
  for i = 10 to 17 do
    printf "%d" i;
  done

let a =
  B.c d ~e:f [
    "g";
    "h";
  ]

let a = match b with
  | Some c -> Some {
    d = c;
    e = e
  }
  | None -> {
    d = c;
    e = e
  }

let () =
  f a ~b:c ~d ~e:g
    u ~q:[
      "a";
      "b";
    ]

let a = {
  b = (
    let z = f u in
    z + z;
  );
  c = (let a = b in {
    z = z;
    y = h;
  });
}

let () =
  { A.
    b =
      C.d e ~f:(fun g -> (h.I.j.K.l, m))
      |! begin fun n ->
        match O.p n with
        | `Q r -> r
        | `S _k -> assert false
      end;
    t =
      u ~v:w
        ~x:(Y.z a);
    b =
      c ~d:e
        ~f:(G.h i);
    j =
      K.l (fun m -> (N.o p m).R.S.t);
    u =
      V.w (fun x -> (Y.x a x).R.S.t);
    v =
      V.w (fun d ->
        (D.g i d).R.S.z);
  }

let x =
  [(W.background `Blue (W.hbox [
    x
   ]));
  ]

let c f =
  if S.is_file f then
    S.load f C.t
    |! fun x -> c := Some x
  else
    C.s C.default |! S.save f
    |! fun () -> c := None

let c f =
  if S.is_file f then (
    S.load f C.t
    |! fun x -> c := Some x
  ) else (
    C.s C.default |! S.save f
    |! fun () -> c := None)

let foo x =
  f1 x >= f2 x
  && f3
    (f4 x)

let foo x =
  (>=)
    (f1 x) (f2 x)
  && f3
    (f4 x)

let a =
  foo
    (fun () ->
      a)

let a =
  foo
    ~f:(fun () ->
      a)

let a =
  foo
    (fun () -> a
    )

let a =
  foo
    ~f:(fun () -> a
    )

let () =
  (* comment *)
  bar a b
    c d;
  foo ~size
    (* comment *)
    ~min:foo
    ?reduce
    ?override
    ()

let foo =
  (* comment *)
  List.map z
    ~f:(fun m ->
      M.q m
      |! T.u ~pr ~verbose:false
          ~p:H.P.US ~is_bar:false)
  |! List.sort ~cmp:(fun a b ->
    compare
      (I.r a.T.s)
      (I.r b.T.s))

let check =
  a lsr 30 >= 3
  && b lsr 20 >= 1
  && c * 10 > f

let () =
  snoo ~f:(fun foo ->
    foo = bar
    && snoo)

let () =
  snoo ~f:(fun foo ->
    foo + bar
    && snoo)

let () =
  snoo ~f:(fun foo ->
    foo
    && snoo)

let variants a =
  match String.split a ~on:'-' with
  | [ s1; s2; s3 ] ->
    let a0 = String.concat ~sep:"" [ s1; s2] in
    let a1 = String.concat ~sep:"-" [ s1; s2; s3; "055" ] in (* comment *)
    List.map [ a0; a1; a]
      ~f:(fun a_s -> lookup a_s)
    |! List.flatten
  | _ -> failwith "bad"

let f a1 a2 a3
    b1 b2 b3 d1 d2 d3 = {
  aa = func1 a1 a2 a3;
  bb = func2
    b1 b2 b3;
  (* FIXME: Here it is reasonable to have '|' aligned with 'match' *)
  cc = (match c with
       | A -> 1
       | B -> 2);
  dd = func3
    d1 d2 d3;
}

let fv =
  map3
    a
    b
    c
    ~f:(fun
      x
      y
      z
    ->
      match x y z with
      | `No)

(* https://forge.ocamlcore.org/tracker/index.php?func=detail&aid=644&group_id=43&atid=255 *)
let b =
  match z with
  | 0 -> fun x -> x
  | 1 -> fun x -> 1


module type X =
struct
  val f : float -> float
  (** This comment should be under "val", like other doc comments and not
   aligned to the left margin. *)
end

let test () =                           (* bug#927 *)
  if a then
    if b then x
    else if c then y
    else z
  else something

let f x =
  if x = 1 then print "hello";
  print "there";
  print "everywhere"

let f x =
  if print "hello"; x = 1 then print "hello";
  print "there"

let f x =
  if x = 1 then let y = 2 in print "hello";
                             print "there"
  else print "toto"

let f x =
  match x with
  | 1 -> let x = 2 in
         if x = 1 then print "hello"
  | 2 -> print "there"

let f x =
  if x = 1 then match x with
                | 1 -> print "hello"
                | 2 -> print "there"
  else print "toto"

let f x =
  x + 4 +
    x + 5 +
    x + 6

let splitting_long_expression =
  quad.{band, i3} <- quad.{band, i3} +. g +.
                       area_12 *. (P.potential x13 y13 +. P.potential x23 y23)

let () =
  (* Beware of lexing ".;" as a single token!  *)
  A.Axes.box vp;
  A.fx vp (E.on_ray u0) 0. 2000.;
  A.Viewport.set_color vp A.Color.green

let f x =
  1
and g y =
  2

let x =
  let module M =
    struct
    end
  in 0

let x =
  try a
  with Not_found ->
    b
let x =
  try a
  with Not_found ->
       b
     | _ ->
       c
let x =
  try a
  with Not_found ->
       if a then b
     | flag when String.is_prefix flag ~prefix:"-" ->
       a
     | _ ->
       c

let x = "toto try \
         tata"
