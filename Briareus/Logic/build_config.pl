:- [reporules],
   setof(X, build_config(X), CFGS),
   print(CFGS),
   halt.
