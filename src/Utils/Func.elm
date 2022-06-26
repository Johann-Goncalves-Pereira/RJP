module Utils.Func exposing (aplR)


aplR : a -> (a -> b) -> b
aplR f x =
    x f
