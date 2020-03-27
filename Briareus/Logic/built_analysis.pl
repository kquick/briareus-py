:- [buildcfg,reportrules,analysis],
   setof(X, T^report(T,X), CFGS),
   findall(A, analysis(A), AS),
   findall(N, action(N), NS),
   findall(D, do(D), DS),
   append([CFGS,AS,NS,DS], PRINTS),
   print(PRINTS),
   halt.
