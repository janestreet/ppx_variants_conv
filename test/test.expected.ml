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
        val a : 'a t
        val b : 'a t
        val is_a : 'a t -> bool
        val is_b : 'a t -> bool
        val a_val : 'a t -> unit option
        val b_val : 'a t -> unit option
        module Variants :
        sig
          val a : 'a t Variantslib.Variant.t
          val b : 'a t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              a:('acc__ -> 'a t Variantslib.Variant.t -> 'acc__) ->
                b:('acc__ -> 'a t Variantslib.Variant.t -> 'acc__) -> 'acc__
          val iter :
            a:('a t Variantslib.Variant.t -> unit) ->
              b:('a t Variantslib.Variant.t -> unit) -> unit
          val map :
            'a t ->
              a:('a t Variantslib.Variant.t -> 'result__) ->
                b:('a t Variantslib.Variant.t -> 'result__) -> 'result__
          val make_matcher :
            a:('a t Variantslib.Variant.t ->
                 'acc__0 -> ((unit -> 'result__) * 'acc__1))
              ->
              b:('a t Variantslib.Variant.t ->
                   'acc__1 -> ((unit -> 'result__) * 'acc__2))
                -> 'acc__0 -> (('a t -> 'result__) * 'acc__2)
          val to_rank : 'a t -> int
          val to_name : 'a t -> string
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
      | Cond: bool t * 'a t * 'a t -> 'a t [@@deriving variants]
    include
      sig
        val bool : bool -> bool t
        val int : int -> int t
        val cond : bool t -> 'a t -> 'a t -> 'a t
        val is_bool : 'a t -> bool
        val is_int : 'a t -> bool
        val is_cond : 'a t -> bool
        val bool_val : 'a t -> bool option
        val int_val : 'a t -> int option
        val cond_val : 'a t -> (bool t * 'a t * 'a t) option
        module Variants :
        sig
          val bool : (bool -> bool t) Variantslib.Variant.t
          val int : (int -> int t) Variantslib.Variant.t
          val cond : (bool t -> 'a t -> 'a t -> 'a t) Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              bool:('acc__ ->
                      (bool -> bool t) Variantslib.Variant.t -> 'acc__)
                ->
                int:('acc__ -> (int -> int t) Variantslib.Variant.t -> 'acc__)
                  ->
                  cond:('acc__ ->
                          (bool t -> 'a t -> 'a t -> 'a t)
                            Variantslib.Variant.t -> 'acc__)
                    -> 'acc__
          val iter :
            bool:((bool -> bool t) Variantslib.Variant.t -> unit) ->
              int:((int -> int t) Variantslib.Variant.t -> unit) ->
                cond:((bool t -> 'a t -> 'a t -> 'a t) Variantslib.Variant.t
                        -> unit)
                  -> unit
          val map :
            'a t ->
              bool:((bool -> bool t) Variantslib.Variant.t ->
                      bool -> 'result__)
                ->
                int:((int -> int t) Variantslib.Variant.t -> int -> 'result__)
                  ->
                  cond:((bool t -> 'a t -> 'a t -> 'a t)
                          Variantslib.Variant.t ->
                          bool t -> 'a t -> 'a t -> 'result__)
                    -> 'result__
          val make_matcher :
            bool:((bool -> bool t) Variantslib.Variant.t ->
                    'acc__0 -> ((bool -> 'result__) * 'acc__1))
              ->
              int:((int -> int t) Variantslib.Variant.t ->
                     'acc__1 -> ((int -> 'result__) * 'acc__2))
                ->
                cond:((bool t -> 'a t -> 'a t -> 'a t) Variantslib.Variant.t
                        ->
                        'acc__2 ->
                          ((bool t -> 'a t -> 'a t -> 'result__) * 'acc__3))
                  -> 'acc__0 -> (('a t -> 'result__) * 'acc__3)
          val to_rank : 'a t -> int
          val to_name : 'a t -> string
          val descriptions : (string * int) list
        end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end =
  struct
    type _ t =
      | Bool: bool -> bool t 
      | Int: int -> int t 
      | Cond: bool t * 'a t * 'a t -> 'a t [@@deriving variants]
    include
      struct
        let bool v0 = Bool v0
        let int v0 = Int v0
        let cond v0 v1 v2 = Cond (v0, v1, v2)
        let is_bool (type a) (t : a t) =
          match t with | Bool _ -> true | _ -> false[@@warning "-4"]
        let is_int (type a) (t : a t) =
          match t with | Int _ -> true | _ -> false[@@warning "-4"]
        let is_cond (type a) (t : a t) =
          match t with | Cond _ -> true | _ -> false[@@warning "-4"]
        let bool_val (type a) (t : a t) =
          match t with
          | Bool v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let int_val (type a) (t : a t) =
          match t with
          | Int v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let cond_val (type a) (t : a t) =
          match t with
          | Cond (v0, v1, v2) -> Stdlib.Option.Some (v0, v1, v2)
          | _ -> Stdlib.Option.None[@@warning "-4"]
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
            let fold ~init:init__  ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  =
              cond_fun__ (int_fun__ (bool_fun__ init__ bool) int) cond
            let iter ~bool:bool_fun__  ~int:int_fun__  ~cond:cond_fun__  =
              (bool_fun__ bool : unit);
              (int_fun__ int : unit);
              (cond_fun__ cond : unit)
            let map (type a) (t__ : a t) ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  =
              match t__ with
              | Bool v0 -> bool_fun__ bool v0
              | Int v0 -> int_fun__ int v0
              | Cond (v0, v1, v2) -> cond_fun__ cond v0 v1 v2
            let make_matcher ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  compile_acc__ =
              let (bool_gen__, compile_acc__) = bool_fun__ bool compile_acc__ in
              let (int_gen__, compile_acc__) = int_fun__ int compile_acc__ in
              let (cond_gen__, compile_acc__) = cond_fun__ cond compile_acc__ in
              ((map ~bool:(fun _ -> bool_gen__) ~int:(fun _ -> int_gen__)
                  ~cond:(fun _ -> cond_gen__)), compile_acc__)
            let to_rank (type a) (t : a t) =
              match t with | Bool _ -> 0 | Int _ -> 1 | Cond _ -> 2
            let to_name (type a) (t : a t) =
              match t with
              | Bool _ -> "Bool"
              | Int _ -> "Int"
              | Cond _ -> "Cond"
            let descriptions = [("Bool", 1); ("Int", 1); ("Cond", 3)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Gadt_inline_record :
  sig
    type _ t =
      | Bool: bool -> bool t 
      | Int: int -> int t 
      | Cond: {
      cond: bool t ;
      true_case: 'a t ;
      false_case: 'a t } -> 'a t [@@deriving variants]
    include
      sig
        val bool : bool -> bool t
        val int : int -> int t
        val cond : cond:bool t -> true_case:'a t -> false_case:'a t -> 'a t
        val is_bool : 'a t -> bool
        val is_int : 'a t -> bool
        val is_cond : 'a t -> bool
        val bool_val : 'a t -> bool option
        val int_val : 'a t -> int option
        val cond_val :
          'a t ->
            ([ `cond of bool t ] * [ `true_case of 'a t ] *
              [ `false_case of 'a t ]) option
        module Variants :
        sig
          val bool : (bool -> bool t) Variantslib.Variant.t
          val int : (int -> int t) Variantslib.Variant.t
          val cond :
            (cond:bool t -> true_case:'a t -> false_case:'a t -> 'a t)
              Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              bool:('acc__ ->
                      (bool -> bool t) Variantslib.Variant.t -> 'acc__)
                ->
                int:('acc__ -> (int -> int t) Variantslib.Variant.t -> 'acc__)
                  ->
                  cond:('acc__ ->
                          (cond:bool t ->
                             true_case:'a t -> false_case:'a t -> 'a t)
                            Variantslib.Variant.t -> 'acc__)
                    -> 'acc__
          val iter :
            bool:((bool -> bool t) Variantslib.Variant.t -> unit) ->
              int:((int -> int t) Variantslib.Variant.t -> unit) ->
                cond:((cond:bool t ->
                         true_case:'a t -> false_case:'a t -> 'a t)
                        Variantslib.Variant.t -> unit)
                  -> unit
          val map :
            'a t ->
              bool:((bool -> bool t) Variantslib.Variant.t ->
                      bool -> 'result__)
                ->
                int:((int -> int t) Variantslib.Variant.t -> int -> 'result__)
                  ->
                  cond:((cond:bool t ->
                           true_case:'a t -> false_case:'a t -> 'a t)
                          Variantslib.Variant.t ->
                          cond:bool t ->
                            true_case:'a t -> false_case:'a t -> 'result__)
                    -> 'result__
          val make_matcher :
            bool:((bool -> bool t) Variantslib.Variant.t ->
                    'acc__0 -> ((bool -> 'result__) * 'acc__1))
              ->
              int:((int -> int t) Variantslib.Variant.t ->
                     'acc__1 -> ((int -> 'result__) * 'acc__2))
                ->
                cond:((cond:bool t ->
                         true_case:'a t -> false_case:'a t -> 'a t)
                        Variantslib.Variant.t ->
                        'acc__2 ->
                          ((cond:bool t ->
                              true_case:'a t -> false_case:'a t -> 'result__)
                            * 'acc__3))
                  -> 'acc__0 -> (('a t -> 'result__) * 'acc__3)
          val to_rank : 'a t -> int
          val to_name : 'a t -> string
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
      true_case: 'a t ;
      false_case: 'a t } -> 'a t [@@deriving variants]
    include
      struct
        let bool v0 = Bool v0
        let int v0 = Int v0
        let cond ~cond:v0  ~true_case:v1  ~false_case:v2  =
          Cond { cond = v0; true_case = v1; false_case = v2 }
        let is_bool (type a) (t : a t) =
          match t with | Bool _ -> true | _ -> false[@@warning "-4"]
        let is_int (type a) (t : a t) =
          match t with | Int _ -> true | _ -> false[@@warning "-4"]
        let is_cond (type a) (t : a t) =
          match t with | Cond _ -> true | _ -> false[@@warning "-4"]
        let bool_val (type a) (t : a t) =
          match t with
          | Bool v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let int_val (type a) (t : a t) =
          match t with
          | Int v0 -> Stdlib.Option.Some v0
          | _ -> Stdlib.Option.None[@@warning "-4"]
        let cond_val (type a) (t : a t) =
          match t with
          | Cond { cond = v0; true_case = v1; false_case = v2 } ->
              Stdlib.Option.Some
                ((`cond v0), (`true_case v1), (`false_case v2))
          | _ -> Stdlib.Option.None[@@warning "-4"]
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
            let fold ~init:init__  ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  =
              cond_fun__ (int_fun__ (bool_fun__ init__ bool) int) cond
            let iter ~bool:bool_fun__  ~int:int_fun__  ~cond:cond_fun__  =
              (bool_fun__ bool : unit);
              (int_fun__ int : unit);
              (cond_fun__ cond : unit)
            let map (type a) (t__ : a t) ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  =
              match t__ with
              | Bool v0 -> bool_fun__ bool v0
              | Int v0 -> int_fun__ int v0
              | Cond { cond = v0; true_case = v1; false_case = v2 } ->
                  cond_fun__ cond ~cond:v0 ~true_case:v1 ~false_case:v2
            let make_matcher ~bool:bool_fun__  ~int:int_fun__ 
              ~cond:cond_fun__  compile_acc__ =
              let (bool_gen__, compile_acc__) = bool_fun__ bool compile_acc__ in
              let (int_gen__, compile_acc__) = int_fun__ int compile_acc__ in
              let (cond_gen__, compile_acc__) = cond_fun__ cond compile_acc__ in
              ((map ~bool:(fun _ -> bool_gen__) ~int:(fun _ -> int_gen__)
                  ~cond:(fun _ -> cond_gen__)), compile_acc__)
            let to_rank (type a) (t : a t) =
              match t with | Bool _ -> 0 | Int _ -> 1 | Cond _ -> 2
            let to_name (type a) (t : a t) =
              match t with
              | Bool _ -> "Bool"
              | Int _ -> "Int"
              | Cond _ -> "Cond"
            let descriptions = [("Bool", 1); ("Int", 1); ("Cond", 3)]
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
        val is_eq : ('a, 'b) t -> bool
        val eq_val : ('a, 'b) t -> unit option
        module Variants :
        sig
          val eq : ('a, 'a) t Variantslib.Variant.t
          val fold :
            init:'acc__ ->
              eq:('acc__ -> ('a, 'a) t Variantslib.Variant.t -> 'acc__) ->
                'acc__
          val iter : eq:(('a, 'a) t Variantslib.Variant.t -> unit) -> unit
          val map :
            ('a, 'b) t ->
              eq:(('a, 'a) t Variantslib.Variant.t -> 'result__) -> 'result__
          val make_matcher :
            eq:(('a, 'a) t Variantslib.Variant.t ->
                  'acc__0 -> ((unit -> 'result__) * 'acc__1))
              -> 'acc__0 -> ((('a, 'b) t -> 'result__) * 'acc__1)
          val to_rank : ('a, 'b) t -> int
          val to_name : ('a, 'b) t -> string
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
        let is_eq (type b) (type a) (t : (a, b) t) =
          match t with | Eq -> true[@@warning "-4"]
        let eq_val (type b) (type a) (t : (a, b) t) =
          match t with | Eq -> Stdlib.Option.Some ()[@@warning "-4"]
        module Variants =
          struct
            let eq =
              { Variantslib.Variant.name = "Eq"; rank = 0; constructor = eq }
            let fold ~init:init__  ~eq:eq_fun__  = eq_fun__ init__ eq
            let iter ~eq:eq_fun__  = (eq_fun__ eq : unit)
            let map (type b) (type a) (t__ : (a, b) t) ~eq:eq_fun__  =
              match t__ with | Eq -> eq_fun__ eq
            let make_matcher ~eq:eq_fun__  compile_acc__ =
              let (eq_gen__, compile_acc__) = eq_fun__ eq compile_acc__ in
              ((map ~eq:(fun _ -> eq_gen__ ())), compile_acc__)
            let to_rank (type b) (type a) (t : (a, b) t) =
              match t with | Eq -> 0
            let to_name (type b) (type a) (t : (a, b) t) =
              match t with | Eq -> "Eq"
            let descriptions = [("Eq", 0)]
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
