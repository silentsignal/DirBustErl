-module(usb64).
-export([decode/1, encode/1, encode_to_string/1]).

encode_to_string(Value) -> binary_to_list(encode(Value)).
encode(Value) -> switch("+/", "-_", base64:encode(Value)).
decode(Value) -> base64:decode(switch("-_", "+/", list_to_binary(Value))).

switch([], [], Value) -> Value;
switch([Pattern | Patterns], [Replacement | Replacements], Value) ->
	NewValue = binary:replace(Value, <<Pattern>>, <<Replacement>>, [global]),
	switch(Patterns, Replacements, NewValue).
