module Type

type Type =
  | Num
  | Bool
  | Func of Type * Type
  | TypeError
