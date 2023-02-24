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
