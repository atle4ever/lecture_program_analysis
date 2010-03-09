let rec iter (n, f) =
  let fun_composit f1 f2 =
    fun x -> f1 (f2 x)
  in

  let identity x =
    x
  in

    if n < 0 then
      raise (Error "# of iter is less than 0")
    else if n = 0 then
      identity
    else
      fun_composit f (iter ((n-1), f))
