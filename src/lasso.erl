-module(lasso).

-export([get_content/2]).
-export([replace/2]).

get_content(File, Data) ->
  {ok, Content} = file:read_file(File),
  New_content = replace(Content, Data),
  New_content.

replace(Content, Data) ->
  case is_list(Data) of
    false -> maps:fold(fun map_folder/3, Content, Data);
    true  -> lists:foldl(fun list_folder/2, Content, Data)
  end.

map_folder(K, V, Acc) ->
  re:replace(Acc, K, V, [global,{return, list}]).

list_folder(T, Acc) ->
  {K,V}=T,
  re:replace(Acc, K, V, [global,{return, list}]).
