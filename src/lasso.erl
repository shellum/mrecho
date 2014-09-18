-module(lasso).

-export([get_content/1]).
-export([ replace/1]).

get_content(File) ->
  Content = file:read_file(File),
  Content.

replace(X) ->
  case is_list(X) of
    false -> maps:fold(fun map_folder/3, "my name is age", X);
    true  -> lists:foldl(fun list_folder/2, "my name is age", X)
  end.


map_folder(K, V, Acc) ->
  re:replace(Acc, K, V, [global,{return, list}]).

list_folder(T, Acc) ->
  {K,V}=T,
  re:replace(Acc, K, V, [global,{return, list}]).
