:- [buildcfg],
   setof(X, build_config2(X), CFGS),
   print(CFGS),
   halt.
