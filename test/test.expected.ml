module Normal :
  sig
    type t =
      | Foo of int 
      | Bar 
      | Exception [@@deriving variants]
    include
      sig
        val foo : int -> t
        val bar : t
        val exception_ : t
        val is_foo : t -> bool
        val is_bar : t -> bool
        val is_exception_ : t -> bool
        val foo_val : t -> int option
        val bar_val : t -> unit option
        val exception__val : t -> unit option
        module Variants :
        sig
          val foo : (int -> t) Variantslib.Variant.t
          val bar : t Variantslib.Variant.t
          val exception_ : t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              foo:('acc__ -> (int -> t) Variantslib.Variant.t -> 'acc__) ->
                bar:('acc__ -> t Variantslib.Variant.t -> 'acc__) ->
                  exception_:('acc__ -> t Variantslib.Variant.t -> 'acc__) ->
                    'acc__
          val iter :
            foo:((int -> t) Variantslib.Variant.t -> unit) ->
              bar:(t Variantslib.Variant.t -> unit) ->
                exception_:(t Variantslib.Variant.t -> unit) -> unit
          val map :
            t ->
              foo:((int -> t) Variantslib.Variant.t -> int -> 'result__) ->
                bar:(t Variantslib.Variant.t -> 'result__) ->
                  exception_:(t Variantslib.Variant.t -> 'result__) ->
                    'result__
          val make_matcher :
            foo:((int -> t) Variantslib.Variant.t ->
                   'acc__0 -> ((int -> 'result__) * 'acc__1))
              ->
              bar:(t Variantslib.Variant.t ->
                     'acc__1 -> ((unit -> 'result__) * 'acc__2))
                ->
                exception_:(t Variantslib.Variant.t ->
                              'acc__2 -> ((unit -> 'result__) * 'acc__3))
                  -> 'acc__0 -> ((t -> 'result__) * 'acc__3)
          val to_rank : t -> int
          val to_name : t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type t =
      | Foo of int 
      | Bar 
      | Exception [@@deriving variants]
    include
      struct
        let foo v0 = Foo v0
        let bar = Bar
        let exception_ = Exception
        let is_foo = function | Foo _ -> true | _ -> false[@@warning "-4"]
        let is_bar = function | Bar -> true | _ -> false[@@warning "-4"]
        let is_exception_ = function | Exception -> true | _ -> false
          [@@warning "-4"]
        let foo_val =
          function
          | Foo v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let bar_val =
          function | Bar -> Stdlib.Option.Some () | _ -> Stdlib.Option.None
          [@@warning "-4"]
        let exception__val =
          function
          | Exception -> Stdlib.Option.Some ()
          | _ -> Stdlib.Option.None[@@warning "-4"]
        module Variants =
          struct
            let foo =
              { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo
              }
            let bar =
              { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar
              }
            let exception_ =
              {
                Variantslib.Variant.name = "Exception";
                rank = 2;
                constructor = exception_
              }
            let fold ~init:init__  ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              exception__fun__ (bar_fun__ (foo_fun__ init__ foo) bar)
                exception_
            let iter ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              (foo_fun__ foo : unit);
              (bar_fun__ bar : unit);
              (exception__fun__ exception_ : unit)
            let map t__ ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              match t__ with
              | Foo v0 -> foo_fun__ foo v0
              | Bar -> bar_fun__ bar
              | Exception -> exception__fun__ exception_
            let make_matcher ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  compile_acc__ =
              let (foo_gen__, compile_acc__) = foo_fun__ foo compile_acc__ in
              let (bar_gen__, compile_acc__) = bar_fun__ bar compile_acc__ in
              let (exception__gen__, compile_acc__) =
                exception__fun__ exception_ compile_acc__ in
              ((map ~foo:(fun _ -> foo_gen__) ~bar:(fun _ -> bar_gen__ ())
                  ~exception_:(fun _ -> exception__gen__ ())), compile_acc__)
            let to_rank = function | Foo _ -> 0 | Bar -> 1 | Exception -> 2
            let to_name =
              function
              | Foo _ -> "Foo"
              | Bar -> "Bar"
              | Exception -> "Exception"
            let descriptions = [("Foo", 1); ("Bar", 0); ("Exception", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Normal_inline_record :
  sig
    type t =
      | Foo of {
      a: int ;
      b: string } 
      | Bar [@@deriving variants]
    include
      sig
        val foo : a:int -> b:string -> t
        val bar : t
        val is_foo : t -> bool
        val is_bar : t -> bool
        val foo_val : t -> ([ `a of int ] * [ `b of string ]) option
        val bar_val : t -> unit option
        module Variants :
        sig
          val foo : (a:int -> b:string -> t) Variantslib.Variant.t
          val bar : t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              foo:('acc__ ->
                     (a:int -> b:string -> t) Variantslib.Variant.t -> 'acc__)
                ->
                bar:('acc__ -> t Variantslib.Variant.t -> 'acc__) -> 'acc__
          val iter :
            foo:((a:int -> b:string -> t) Variantslib.Variant.t -> unit) ->
              bar:(t Variantslib.Variant.t -> unit) -> unit
          val map :
            t ->
              foo:((a:int -> b:string -> t) Variantslib.Variant.t ->
                     a:int -> b:string -> 'result__)
                -> bar:(t Variantslib.Variant.t -> 'result__) -> 'result__
          val make_matcher :
            foo:((a:int -> b:string -> t) Variantslib.Variant.t ->
                   'acc__0 -> ((a:int -> b:string -> 'result__) * 'acc__1))
              ->
              bar:(t Variantslib.Variant.t ->
                     'acc__1 -> ((unit -> 'result__) * 'acc__2))
                -> 'acc__0 -> ((t -> 'result__) * 'acc__2)
          val to_rank : t -> int
          val to_name : t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type t =
      | Foo of {
      a: int ;
      b: string } 
      | Bar [@@deriving variants]
    include
      struct
        let foo ~a:v0  ~b:v1  = Foo { a = v0; b = v1 }
        let bar = Bar
        let is_foo = function | Foo _ -> true | _ -> false[@@warning "-4"]
        let is_bar = function | Bar -> true | _ -> false[@@warning "-4"]
        let foo_val =
          function
          | Foo { a = v0; b = v1 } -> Stdlib.Option.Some ((`a v0), (`b v1))
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let bar_val =
          function | Bar -> Stdlib.Option.Some () | _ -> Stdlib.Option.None
          [@@warning "-4"]
        module Variants =
          struct
            let foo =
              { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo
              }
            let bar =
              { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar
              }
            let fold ~init:init__  ~foo:foo_fun__  ~bar:bar_fun__  =
              bar_fun__ (foo_fun__ init__ foo) bar
            let iter ~foo:foo_fun__  ~bar:bar_fun__  =
              (foo_fun__ foo : unit); (bar_fun__ bar : unit)
            let map t__ ~foo:foo_fun__  ~bar:bar_fun__  =
              match t__ with
              | Foo { a = v0; b = v1 } -> foo_fun__ foo ~a:v0 ~b:v1
              | Bar -> bar_fun__ bar
            let make_matcher ~foo:foo_fun__  ~bar:bar_fun__  compile_acc__ =
              let (foo_gen__, compile_acc__) = foo_fun__ foo compile_acc__ in
              let (bar_gen__, compile_acc__) = bar_fun__ bar compile_acc__ in
              ((map ~foo:(fun _ -> foo_gen__) ~bar:(fun _ -> bar_gen__ ())),
                compile_acc__)
            let to_rank = function | Foo _ -> 0 | Bar -> 1
            let to_name = function | Foo _ -> "Foo" | Bar -> "Bar"
            let descriptions = [("Foo", 2); ("Bar", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Poly :
  sig
    type t = [ `Foo of int  | `Bar  | `Exception ][@@deriving variants]
    include
      sig
        val foo : int -> t
        val bar : t
        val exception_ : t
        val is_foo : t -> bool
        val is_bar : t -> bool
        val is_exception_ : t -> bool
        val foo_val : t -> int option
        val bar_val : t -> unit option
        val exception__val : t -> unit option
        module Variants :
        sig
          val foo : (int -> t) Variantslib.Variant.t
          val bar : t Variantslib.Variant.t
          val exception_ : t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              foo:('acc__ -> (int -> t) Variantslib.Variant.t -> 'acc__) ->
                bar:('acc__ -> t Variantslib.Variant.t -> 'acc__) ->
                  exception_:('acc__ -> t Variantslib.Variant.t -> 'acc__) ->
                    'acc__
          val iter :
            foo:((int -> t) Variantslib.Variant.t -> unit) ->
              bar:(t Variantslib.Variant.t -> unit) ->
                exception_:(t Variantslib.Variant.t -> unit) -> unit
          val map :
            t ->
              foo:((int -> t) Variantslib.Variant.t -> int -> 'result__) ->
                bar:(t Variantslib.Variant.t -> 'result__) ->
                  exception_:(t Variantslib.Variant.t -> 'result__) ->
                    'result__
          val make_matcher :
            foo:((int -> t) Variantslib.Variant.t ->
                   'acc__0 -> ((int -> 'result__) * 'acc__1))
              ->
              bar:(t Variantslib.Variant.t ->
                     'acc__1 -> ((unit -> 'result__) * 'acc__2))
                ->
                exception_:(t Variantslib.Variant.t ->
                              'acc__2 -> ((unit -> 'result__) * 'acc__3))
                  -> 'acc__0 -> ((t -> 'result__) * 'acc__3)
          val to_rank : t -> int
          val to_name : t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type t = [ `Foo of int  | `Bar  | `Exception ][@@deriving variants]
    include
      struct
        let foo v0 = `Foo v0
        let bar = `Bar
        let exception_ = `Exception
        let is_foo = function | `Foo _ -> true | _ -> false[@@warning "-4"]
        let is_bar = function | `Bar -> true | _ -> false[@@warning "-4"]
        let is_exception_ = function | `Exception -> true | _ -> false
          [@@warning "-4"]
        let foo_val =
          function | `Foo v -> Stdlib.Option.Some v | _ -> Stdlib.Option.None
          [@@warning "-4"]
        let bar_val =
          function | `Bar -> Stdlib.Option.Some () | _ -> Stdlib.Option.None
          [@@warning "-4"]
        let exception__val =
          function
          | `Exception -> Stdlib.Option.Some ()
          | _ -> Stdlib.Option.None[@@warning "-4"]
        module Variants =
          struct
            let foo =
              { Variantslib.Variant.name = "Foo"; rank = 0; constructor = foo
              }
            let bar =
              { Variantslib.Variant.name = "Bar"; rank = 1; constructor = bar
              }
            let exception_ =
              {
                Variantslib.Variant.name = "Exception";
                rank = 2;
                constructor = exception_
              }
            let fold ~init:init__  ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              exception__fun__ (bar_fun__ (foo_fun__ init__ foo) bar)
                exception_
            let iter ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              (foo_fun__ foo : unit);
              (bar_fun__ bar : unit);
              (exception__fun__ exception_ : unit)
            let map t__ ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  =
              match t__ with
              | `Foo v0 -> foo_fun__ foo v0
              | `Bar -> bar_fun__ bar
              | `Exception -> exception__fun__ exception_
            let make_matcher ~foo:foo_fun__  ~bar:bar_fun__ 
              ~exception_:exception__fun__  compile_acc__ =
              let (foo_gen__, compile_acc__) = foo_fun__ foo compile_acc__ in
              let (bar_gen__, compile_acc__) = bar_fun__ bar compile_acc__ in
              let (exception__gen__, compile_acc__) =
                exception__fun__ exception_ compile_acc__ in
              ((map ~foo:(fun _ -> foo_gen__) ~bar:(fun _ -> bar_gen__ ())
                  ~exception_:(fun _ -> exception__gen__ ())), compile_acc__)
            let to_rank =
              function | `Foo _ -> 0 | `Bar -> 1 | `Exception -> 2
            let to_name =
              function
              | `Foo _ -> "Foo"
              | `Bar -> "Bar"
              | `Exception -> "Exception"
            let descriptions = [("Foo", 1); ("Bar", 0); ("Exception", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Wildcard :
  sig
    type _ t =
      | A 
      | B [@@deriving variants]
    include
      sig
        val a : _ t
        val b : _ t
        val is_a : _ t -> bool
        val is_b : _ t -> bool
        val a_val : _ t -> unit option
        val b_val : _ t -> unit option
        module Variants :
        sig
          val a : _ t Variantslib.Variant.t
          val b : _ t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              a:('acc__ -> _ t Variantslib.Variant.t -> 'acc__) ->
                b:('acc__ -> _ t Variantslib.Variant.t -> 'acc__) -> 'acc__
          val iter :
            a:(_ t Variantslib.Variant.t -> unit) ->
              b:(_ t Variantslib.Variant.t -> unit) -> unit
          val map :
            _ t ->
              a:(_ t Variantslib.Variant.t -> 'result__) ->
                b:(_ t Variantslib.Variant.t -> 'result__) -> 'result__
          val make_matcher :
            a:(_ t Variantslib.Variant.t ->
                 'acc__0 -> ((unit -> 'result__) * 'acc__1))
              ->
              b:(_ t Variantslib.Variant.t ->
                   'acc__1 -> ((unit -> 'result__) * 'acc__2))
                -> 'acc__0 -> ((_ t -> 'result__) * 'acc__2)
          val to_rank : _ t -> int
          val to_name : _ t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type _ t =
      | A 
      | B [@@deriving variants]
    include
      struct
        let a = A
        let b = B
        let is_a = function | A -> true | _ -> false[@@warning "-4"]
        let is_b = function | B -> true | _ -> false[@@warning "-4"]
        let a_val =
          function | A -> Stdlib.Option.Some () | _ -> Stdlib.Option.None
          [@@warning "-4"]
        let b_val =
          function | B -> Stdlib.Option.Some () | _ -> Stdlib.Option.None
          [@@warning "-4"]
        module Variants =
          struct
            let a =
              { Variantslib.Variant.name = "A"; rank = 0; constructor = a }
            let b =
              { Variantslib.Variant.name = "B"; rank = 1; constructor = b }
            let fold ~init:init__  ~a:a_fun__  ~b:b_fun__  =
              b_fun__ (a_fun__ init__ a) b
            let iter ~a:a_fun__  ~b:b_fun__  =
              (a_fun__ a : unit); (b_fun__ b : unit)
            let map t__ ~a:a_fun__  ~b:b_fun__  =
              match t__ with | A -> a_fun__ a | B -> b_fun__ b
            let make_matcher ~a:a_fun__  ~b:b_fun__  compile_acc__ =
              let (a_gen__, compile_acc__) = a_fun__ a compile_acc__ in
              let (b_gen__, compile_acc__) = b_fun__ b compile_acc__ in
              ((map ~a:(fun _ -> a_gen__ ()) ~b:(fun _ -> b_gen__ ())),
                compile_acc__)
            let to_rank = function | A -> 0 | B -> 1
            let to_name = function | A -> "A" | B -> "B"
            let descriptions = [("A", 0); ("B", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Gadt :
  sig
    type _ t =
      | Bool: bool -> bool t 
      | Int: int -> int t 
      | Cond: {
      cond: bool t ;
      true_branch: 'a t ;
      false_branch: 'a t } -> 'a t 
      | Pair: 'a t * 'b t -> ('a * 'b) t 
      | Swap: ('a * 'b) t -> ('b * 'a) t 
      | Fst: ('a * 'b) t -> 'a t 
      | Snd: ('a * 'b) t -> 'b t [@@deriving variants]
    include
      sig
        val bool : bool -> bool t
        val int : int -> int t
        val cond :
          cond:bool t -> true_branch:'a t -> false_branch:'a t -> 'a t
        val pair : 'a t -> 'b t -> ('a * 'b) t
        val swap : ('a * 'b) t -> ('b * 'a) t
        val fst : ('a * 'b) t -> 'a t
        val snd : ('a * 'b) t -> 'b t
        val is_bool : _ t -> bool
        val is_int : _ t -> bool
        val is_cond : _ t -> bool
        val is_pair : _ t -> bool
        val is_swap : _ t -> bool
        val is_fst : _ t -> bool
        val is_snd : _ t -> bool
        val bool_val : _ t -> bool option
        val int_val : _ t -> int option
        val cond_val :
          'a t ->
            ([ `cond of bool t ] * [ `true_branch of 'a t ] *
              [ `false_branch of 'a t ]) option
        val pair_val : ('a * 'b) t -> ('a t * 'b t) option
        val swap_val : ('b * 'a) t -> ('a * 'b) t option
        module Variants :
        sig
          val bool : (bool -> bool t) Variantslib.Variant.t
          val int : (int -> int t) Variantslib.Variant.t
          val cond :
            (cond:bool t -> true_branch:'a t -> false_branch:'a t -> 'a t)
              Variantslib.Variant.t
          val pair : ('a t -> 'b t -> ('a * 'b) t) Variantslib.Variant.t
          val swap : (('a * 'b) t -> ('b * 'a) t) Variantslib.Variant.t
          val fst : (('a * 'b) t -> 'a t) Variantslib.Variant.t
          val snd : (('a * 'b) t -> 'b t) Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              bool:('acc__ ->
                      (bool -> bool t) Variantslib.Variant.t -> 'acc__)
                ->
                int:('acc__ -> (int -> int t) Variantslib.Variant.t -> 'acc__)
                  ->
                  cond:('acc__ ->
                          (cond:bool t ->
                             true_branch:'a t -> false_branch:'a t -> 'a t)
                            Variantslib.Variant.t -> 'acc__)
                    ->
                    pair:('acc__ ->
                            ('a t -> 'b t -> ('a * 'b) t)
                              Variantslib.Variant.t -> 'acc__)
                      ->
                      swap:('acc__ ->
                              (('a * 'b) t -> ('b * 'a) t)
                                Variantslib.Variant.t -> 'acc__)
                        ->
                        fst:('acc__ ->
                               (('a * 'b) t -> 'a t) Variantslib.Variant.t ->
                                 'acc__)
                          ->
                          snd:('acc__ ->
                                 (('a * 'b) t -> 'b t) Variantslib.Variant.t
                                   -> 'acc__)
                            -> 'acc__
          val iter :
            bool:((bool -> bool t) Variantslib.Variant.t -> unit) ->
              int:((int -> int t) Variantslib.Variant.t -> unit) ->
                cond:((cond:bool t ->
                         true_branch:'a t -> false_branch:'a t -> 'a t)
                        Variantslib.Variant.t -> unit)
                  ->
                  pair:(('a t -> 'b t -> ('a * 'b) t) Variantslib.Variant.t
                          -> unit)
                    ->
                    swap:((('a * 'b) t -> ('b * 'a) t) Variantslib.Variant.t
                            -> unit)
                      ->
                      fst:((('a * 'b) t -> 'a t) Variantslib.Variant.t ->
                             unit)
                        ->
                        snd:((('a * 'b) t -> 'b t) Variantslib.Variant.t ->
                               unit)
                          -> unit
          val to_rank : _ t -> int
          val to_name : _ t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type _ t =
      | Bool: bool -> bool t 
      | Int: int -> int t 
      | Cond: {
      cond: bool t ;
      true_branch: 'a t ;
      false_branch: 'a t } -> 'a t 
      | Pair: 'a t * 'b t -> ('a * 'b) t 
      | Swap: ('a * 'b) t -> ('b * 'a) t 
      | Fst: ('a * 'b) t -> 'a t 
      | Snd: ('a * 'b) t -> 'b t [@@deriving variants]
    include
      struct
        let bool v0 = Bool v0
        let int v0 = Int v0
        let cond ~cond:v0  ~true_branch:v1  ~false_branch:v2  =
          Cond { cond = v0; true_branch = v1; false_branch = v2 }
        let pair v0 v1 = Pair (v0, v1)
        let swap v0 = Swap v0
        let fst v0 = Fst v0
        let snd v0 = Snd v0
        let is_bool (type _x__001_) (t : _x__001_ t) =
          match t with | Bool _ -> true | _ -> false[@@warning "-4"]
        let is_int (type _x__003_) (t : _x__003_ t) =
          match t with | Int _ -> true | _ -> false[@@warning "-4"]
        let is_cond (type _x__005_) (t : _x__005_ t) =
          match t with | Cond _ -> true | _ -> false[@@warning "-4"]
        let is_pair (type _x__006_) (t : _x__006_ t) =
          match t with | Pair _ -> true | _ -> false[@@warning "-4"]
        let is_swap (type _x__007_) (t : _x__007_ t) =
          match t with | Swap _ -> true | _ -> false[@@warning "-4"]
        let is_fst (type _x__008_) (t : _x__008_ t) =
          match t with | Fst _ -> true | _ -> false[@@warning "-4"]
        let is_snd (type _x__009_) (t : _x__009_ t) =
          match t with | Snd _ -> true | _ -> false[@@warning "-4"]
        let bool_val (type _x__002_) (t : _x__002_ t) =
          match t with
          | Bool v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4-56"]
        let int_val (type _x__004_) (t : _x__004_ t) =
          match t with
          | Int v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4-56"]
        let cond_val (type a) (t : a t) =
          match t with
          | Cond { cond = v0; true_branch = v1; false_branch = v2 } ->
              Stdlib.Option.Some
                ((`cond v0), (`true_branch v1), (`false_branch v2))
          | _ -> Stdlib.Option.None[@@warning "-4-56"]
        let pair_val (type a) (type b) (t : (a * b) t) =
          match t with
          | Pair (v0, v1) -> Stdlib.Option.Some (v0, v1)
          | _ -> Stdlib.Option.None[@@warning "-4-56"]
        let swap_val (type a) (type b) (t : (b * a) t) =
          match t with
          | Swap v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4-56"]
        module Variants =
          struct
            let bool =
              {
                Variantslib.Variant.name = "Bool";
                rank = 0;
                constructor = bool
              }
            let int =
              { Variantslib.Variant.name = "Int"; rank = 1; constructor = int
              }
            let cond =
              {
                Variantslib.Variant.name = "Cond";
                rank = 2;
                constructor = cond
              }
            let pair =
              {
                Variantslib.Variant.name = "Pair";
                rank = 3;
                constructor = pair
              }
            let swap =
              {
                Variantslib.Variant.name = "Swap";
                rank = 4;
                constructor = swap
              }
            let fst =
              { Variantslib.Variant.name = "Fst"; rank = 5; constructor = fst
              }
            let snd =
              { Variantslib.Variant.name = "Snd"; rank = 6; constructor = snd
              }
            let fold ~init:init__  ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  ~pair:pair_fun__  ~swap:swap_fun__ 
              ~fst:fst_fun__  ~snd:snd_fun__  =
              snd_fun__
                (fst_fun__
                   (swap_fun__
                      (pair_fun__
                         (cond_fun__ (int_fun__ (bool_fun__ init__ bool) int)
                            cond) pair) swap) fst) snd
            let iter ~bool:bool_fun__  ~int:int_fun__  ~cond:cond_fun__ 
              ~pair:pair_fun__  ~swap:swap_fun__  ~fst:fst_fun__ 
              ~snd:snd_fun__  =
              (bool_fun__ bool : unit);
              (int_fun__ int : unit);
              (cond_fun__ cond : unit);
              (pair_fun__ pair : unit);
              (swap_fun__ swap : unit);
              (fst_fun__ fst : unit);
              (snd_fun__ snd : unit)
            let to_rank (type _x__011_) (t : _x__011_ t) =
              match t with
              | Bool _ -> 0
              | Int _ -> 1
              | Cond _ -> 2
              | Pair _ -> 3
              | Swap _ -> 4
              | Fst _ -> 5
              | Snd _ -> 6
            let to_name (type _x__010_) (t : _x__010_ t) =
              match t with
              | Bool _ -> "Bool"
              | Int _ -> "Int"
              | Cond _ -> "Cond"
              | Pair _ -> "Pair"
              | Swap _ -> "Swap"
              | Fst _ -> "Fst"
              | Snd _ -> "Snd"
            let descriptions =
              [("Bool", 1);
              ("Int", 1);
              ("Cond", 3);
              ("Pair", 2);
              ("Swap", 1);
              ("Fst", 1);
              ("Snd", 1)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Gadt_arity :
  sig
    type (_, _) t =
      | Eq: ('a, 'a) t [@@deriving variants]
    include
      sig
        val eq : ('a, 'a) t
        val is_eq : (_, _) t -> bool
        val eq_val : ('a, 'a) t -> unit option
        module Variants :
        sig
          val eq : ('a, 'a) t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              eq:('acc__ -> ('a, 'a) t Variantslib.Variant.t -> 'acc__) ->
                'acc__
          val iter : eq:(('a, 'a) t Variantslib.Variant.t -> unit) -> unit
          val map :
            (_, _) t ->
              eq:(('a, 'a) t Variantslib.Variant.t -> 'result__) -> 'result__
          val make_matcher :
            eq:(('a, 'a) t Variantslib.Variant.t ->
                  'acc__0 -> ((unit -> 'result__) * 'acc__1))
              -> 'acc__0 -> (((_, _) t -> 'result__) * 'acc__1)
          val to_rank : (_, _) t -> int
          val to_name : (_, _) t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type (_, _) t =
      | Eq: ('a, 'a) t [@@deriving variants]
    include
      struct
        let eq = Eq
        let is_eq (type _x__012_) (type _x__013_)
          (t : (_x__012_, _x__013_) t) = match t with | Eq -> true[@@warning
                                                                    "-4"]
        let eq_val (type a) (t : (a, a) t) =
          match t with | Eq -> Stdlib.Option.Some ()[@@warning "-4-56"]
        module Variants =
          struct
            let eq =
              { Variantslib.Variant.name = "Eq"; rank = 0; constructor = eq }
            let fold ~init:init__  ~eq:eq_fun__  = eq_fun__ init__ eq
            let iter ~eq:eq_fun__  = (eq_fun__ eq : unit)
            let map (type _x__018_) (type _x__019_)
              (t__ : (_x__018_, _x__019_) t) ~eq:eq_fun__  =
              match t__ with | Eq -> eq_fun__ eq
            let make_matcher ~eq:eq_fun__  compile_acc__ =
              let (eq_gen__, compile_acc__) = eq_fun__ eq compile_acc__ in
              ((map ~eq:(fun _ -> eq_gen__ ())), compile_acc__)
            let to_rank (type _x__016_) (type _x__017_)
              (t : (_x__016_, _x__017_) t) = match t with | Eq -> 0
            let to_name (type _x__014_) (type _x__015_)
              (t : (_x__014_, _x__015_) t) = match t with | Eq -> "Eq"
            let descriptions = [("Eq", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
