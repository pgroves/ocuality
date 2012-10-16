
val makePickAllCompound1:
	toStringFun: ('z -> string) ->
	field1Cmp: 'a Comparer.t ->
	getField1:('z -> 'a) ->
	'z Comparer.t


val makePickAllCompound2:
	toStringFun: ('z -> string) ->
	field1Cmp: 'a Comparer.t ->
	field2Cmp: 'b Comparer.t ->
	getField1:('z -> 'a) ->
	getField2:('z -> 'b) ->
	'z Comparer.t

val makePickAllCompound8:
	toStringFun: ('z -> string) ->
	field1Cmp: 'a Comparer.t ->
	field2Cmp: 'b Comparer.t ->
	field3Cmp: 'c Comparer.t ->
	field4Cmp: 'd Comparer.t ->
	field5Cmp: 'e Comparer.t ->
	field6Cmp: 'f Comparer.t ->
	field7Cmp: 'g Comparer.t ->
	field8Cmp: 'h Comparer.t ->
	getField1:('z -> 'a) ->
	getField2:('z -> 'b) ->
	getField3:('z -> 'c) ->
	getField4:('z -> 'd) ->
	getField5:('z -> 'e) ->
	getField6:('z -> 'f) ->
	getField7:('z -> 'g) ->
	getField8:('z -> 'h) ->
	'z Comparer.t



val makePickOneCompound2:
	toStringFun: ('z -> string) ->
	field1Cmp: 'a Comparer.t ->
	field2Cmp: 'b Comparer.t ->
	getField1Opt: ('z -> 'a option) ->
	getField2Opt: ('z -> 'b option) ->
	'z Comparer.t

val makePickOneCompound8:
	toStringFun: ('z -> string) ->
	field1Cmp: 'a Comparer.t ->
	field2Cmp: 'b Comparer.t ->
	field3Cmp: 'c Comparer.t ->
	field4Cmp: 'd Comparer.t ->
	field5Cmp: 'e Comparer.t ->
	field6Cmp: 'f Comparer.t ->
	field7Cmp: 'g Comparer.t ->
	field8Cmp: 'h Comparer.t ->
	getField1Opt: ('z -> 'a option) ->
	getField2Opt: ('z -> 'b option) ->
	getField3Opt: ('z -> 'c option) ->
	getField4Opt: ('z -> 'd option) ->
	getField5Opt: ('z -> 'e option) ->
	getField6Opt: ('z -> 'f option) ->
	getField7Opt: ('z -> 'g option) ->
	getField8Opt: ('z -> 'h option) ->
	'z Comparer.t

