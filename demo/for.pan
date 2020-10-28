fun main() {
  let sum = 0;
  for i in 100..0 {
      sum = sum + i;
  }
  print(sum);
  return;
}
fun print(a:int) {
}


push CodeObject is <code object /Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan at ??? file "pan", line 1>
register name="int", ty: Int
notFound
Symbol is Symbol { name: "int", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Int }
after register name="int", ty: Int
register name="float", ty: Float
notFound
Symbol is Symbol { name: "float", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Float }
after register name="float", ty: Float
register name="string", ty: Str
notFound
Symbol is Symbol { name: "string", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Str }
after register name="string", ty: Str
register name="bool", ty: Bool
notFound
Symbol is Symbol { name: "bool", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Bool }
after register name="bool", ty: Bool
register name="Any", ty: Any
notFound
Symbol is Symbol { name: "Any", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Any }
after register name="Any", ty: Any
register name="print", ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false })
notFound
Symbol is Symbol { name: "print", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false }) }
after register name="print", ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false })
register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
notFound
Symbol is Symbol { name: "Color", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true }) }
after register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
type is :Fn(FnType { name: "main", arg_types: [], type_args: [], ret_type: Unknown, is_pub: false, is_static: false })
function is :FunctionDefinition { doc: [], loc: Loc(1, 13, 1), name: Some(Identifier { loc: Loc(1, 13, 8), name: "main" }), name_loc: Loc(1, 13, 8), params: [], is_pub: false, is_static: false, returns: None, body: Some(Block(Loc(1, 13, 1), [VariableDefinition(Loc(1, 14, 28), VariableDeclaration { loc: Loc(1, 14, 13), ty: None, name: Identifier { loc: Loc(1, 14, 13), name: "color2" } }, Some(FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })]))), Expression(Loc(1, 15, 16), FunctionCall(Loc(1, 15, 16), Variable(Identifier { loc: Loc(1, 15, 8), name: "print" }), [Variable(Identifier { loc: Loc(1, 15, 15), name: "color2" })])), Return(Loc(1, 16, 13), None)])) }
register name="main", ty: Fn(FnType { name: "main", arg_types: [], type_args: [], ret_type: Unknown, is_pub: false, is_static: false })
notFound
Symbol is Symbol { name: "main", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Fn(FnType { name: "main", arg_types: [], type_args: [], ret_type: Unknown, is_pub: false, is_static: false }) }
after register name="main", ty: Fn(FnType { name: "main", arg_types: [], type_args: [], ret_type: Unknown, is_pub: false, is_static: false })
type is :Fn(FnType { name: "print", arg_types: [("a", Int, true)], type_args: [], ret_type: Unknown, is_pub: false, is_static: false })
function is :FunctionDefinition { doc: [], loc: Loc(1, 18, 1), name: Some(Identifier { loc: Loc(1, 18, 9), name: "print" }), name_loc: Loc(1, 18, 9), params: [(Loc(1, 18, 15), Some(Parameter { loc: Loc(1, 18, 15), ty: Variable(Identifier { loc: Loc(1, 18, 15), name: "int" }), is_ref: false, is_mut: false, name: Some(Identifier { loc: Loc(1, 18, 11), name: "a" }) }))], is_pub: false, is_static: false, returns: None, body: Some(Block(Loc(1, 18, 1), [])) }
register name="print", ty: Fn(FnType { name: "print", arg_types: [("a", Int, true)], type_args: [], ret_type: Unknown, is_pub: false, is_static: false })
found!
Symbol is Symbol { name: "print", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false, is_parameter: false, is_free: false, ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false }) }
after register name="print", ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false })
register name="int", ty: Int
found!
Symbol is Symbol { name: "int", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false, is_parameter: false, is_free: false, ty: Int }
after register name="int", ty: Int
register name="float", ty: Float
found!
Symbol is Symbol { name: "float", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false, is_parameter: false, is_free: false, ty: Float }
after register name="float", ty: Float
register name="string", ty: Str
found!
Symbol is Symbol { name: "string", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false, is_parameter: false, is_free: false, ty: Str }
after register name="string", ty: Str
register name="bool", ty: Bool
found!
Symbol is Symbol { name: "bool", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false, is_parameter: false, is_free: false, ty: Bool }
after register name="bool", ty: Bool
register name="self", ty: Str
notFound
Symbol is Symbol { name: "self", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Str }
after register name="self", ty: Str
register name="Red", ty: Reference("Red", [])
notFound
Symbol is Symbol { name: "Red", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("Red", []) }
after register name="Red", ty: Reference("Red", [])
register name="Green", ty: Reference("Green", [])
notFound
Symbol is Symbol { name: "Green", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("Green", []) }
after register name="Green", ty: Reference("Green", [])
register name="Blue", ty: Reference("Blue", [])
notFound
Symbol is Symbol { name: "Blue", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("Blue", []) }
after register name="Blue", ty: Reference("Blue", [])
register name="Black", ty: Reference("Black", [])
notFound
Symbol is Symbol { name: "Black", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("Black", []) }
after register name="Black", ty: Reference("Black", [])
register name="White", ty: Reference("White", [])
notFound
Symbol is Symbol { name: "White", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("White", []) }
after register name="White", ty: Reference("White", [])
register name="is_warm", ty: Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false })
notFound
Symbol is Symbol { name: "is_warm", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }) }
after register name="is_warm", ty: Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false })
statement is Block(Loc(1, 8, 4), [Return(Loc(1, 9, 29), Some(Equal(Loc(1, 9, 21), Variable(Identifier { loc: Loc(1, 9, 19), name: "self" }), FunctionCall(Loc(1, 9, 29), Variable(Identifier { loc: Loc(1, 9, 27), name: "Black" }), []))))])
statement is Return(Loc(1, 9, 29), Some(Equal(Loc(1, 9, 21), Variable(Identifier { loc: Loc(1, 9, 19), name: "self" }), FunctionCall(Loc(1, 9, 29), Variable(Identifier { loc: Loc(1, 9, 27), name: "Black" }), []))))
register name="self", ty: Str
notFound
Symbol is Symbol { name: "self", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Str }
after register name="self", ty: Str
register name="Black", ty: Reference("Black", [])
notFound
Symbol is Symbol { name: "Black", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Reference("Black", []) }
after register name="Black", ty: Reference("Black", [])
register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
found!
Symbol is Symbol { name: "Color", scope: Unknown, is_param: false, is_referenced: false, is_assigned: true, is_parameter: false, is_free: false, ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true }) }
after register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
statement is Block(Loc(1, 13, 1), [VariableDefinition(Loc(1, 14, 28), VariableDeclaration { loc: Loc(1, 14, 13), ty: None, name: Identifier { loc: Loc(1, 14, 13), name: "color2" } }, Some(FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })]))), Expression(Loc(1, 15, 16), FunctionCall(Loc(1, 15, 16), Variable(Identifier { loc: Loc(1, 15, 8), name: "print" }), [Variable(Identifier { loc: Loc(1, 15, 15), name: "color2" })])), Return(Loc(1, 16, 13), None)])
statement is VariableDefinition(Loc(1, 14, 28), VariableDeclaration { loc: Loc(1, 14, 13), ty: None, name: Identifier { loc: Loc(1, 14, 13), name: "color2" } }, Some(FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })])))
dddddregister symbol: "color2"
bbbbbb symbol: "color2"
register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
notFound
Symbol is Symbol { name: "Color", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true }) }
after register name="Color", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
right expression is FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })])
register name="color2", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
notFound
Symbol is Symbol { name: "color2", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true }) }
after register name="color2", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
statement is Expression(Loc(1, 15, 16), FunctionCall(Loc(1, 15, 16), Variable(Identifier { loc: Loc(1, 15, 8), name: "print" }), [Variable(Identifier { loc: Loc(1, 15, 15), name: "color2" })]))
register name="print", ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false })
notFound
Symbol is Symbol { name: "print", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false }) }
after register name="print", ty: Fn(FnType { name: "print", arg_types: [("value", Any, false)], type_args: [], ret_type: Any, is_pub: true, is_static: false })
register name="color2", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
found!
Symbol is Symbol { name: "color2", scope: Unknown, is_param: false, is_referenced: false, is_assigned: true, is_parameter: false, is_free: false, ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true }) }
after register name="color2", ty: Enum(EnumType { name: "Color", type_args: [], variants: [("Red", Reference("Red", [Int])), ("Green", Reference("Green", [Float])), ("Blue", Reference("Blue", [Int, Int, Int])), ("Black", Reference("Black", [])), ("White", Reference("White", []))], methods: [("is_warm", Fn(FnType { name: "is_warm", arg_types: [], type_args: [], ret_type: Unknown, is_pub: true, is_static: false }))], is_pub: true })
statement is Return(Loc(1, 16, 13), None)
paramis("a", Int, true),
register name="int", ty: Int
notFound
Symbol is Symbol { name: "int", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Int }
after register name="int", ty: Int
register name="a", ty: Int
notFound
Symbol is Symbol { name: "a", scope: Unknown, is_param: false, is_referenced: false, is_assigned: false, is_parameter: false, is_free: false, ty: Int }
after register name="a", ty: Int
statement is Block(Loc(1, 18, 1), [])
size before is1
push CodeObject is <code object Color at ??? file "", line 0>
push CodeObject is <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>
Compiling Block(Loc(1, 8, 4), [Return(Loc(1, 9, 29), Some(Equal(Loc(1, 9, 21), Variable(Identifier { loc: Loc(1, 9, 19), name: "self" }), FunctionCall(Loc(1, 9, 29), Variable(Identifier { loc: Loc(1, 9, 27), name: "Black" }), []))))])
Compiling Return(Loc(1, 9, 29), Some(Equal(Loc(1, 9, 21), Variable(Identifier { loc: Loc(1, 9, 19), name: "self" }), FunctionCall(Loc(1, 9, 29), Variable(Identifier { loc: Loc(1, 9, 27), name: "Black" }), []))))
Looking up "self"
pop code
Looking up "is_warm"
pop code
Looking up "Color"
push CodeObject is <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>
Compiling Block(Loc(1, 13, 1), [VariableDefinition(Loc(1, 14, 28), VariableDeclaration { loc: Loc(1, 14, 13), ty: None, name: Identifier { loc: Loc(1, 14, 13), name: "color2" } }, Some(FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })]))), Expression(Loc(1, 15, 16), FunctionCall(Loc(1, 15, 16), Variable(Identifier { loc: Loc(1, 15, 8), name: "print" }), [Variable(Identifier { loc: Loc(1, 15, 15), name: "color2" })])), Return(Loc(1, 16, 13), None)])
Compiling VariableDefinition(Loc(1, 14, 28), VariableDeclaration { loc: Loc(1, 14, 13), ty: None, name: Identifier { loc: Loc(1, 14, 13), name: "color2" } }, Some(FunctionCall(Loc(1, 14, 28), Attribute(Loc(1, 14, 25), Variable(Identifier { loc: Loc(1, 14, 21), name: "Color" }), Some(Identifier { loc: Loc(1, 14, 25), name: "Red" }), None), [NumberLiteral(Loc(1, 195, 197), BigInt { sign: Plus, data: BigUint { data: [10] } })])))
Looking up "color2"
Compiling Expression(Loc(1, 15, 16), FunctionCall(Loc(1, 15, 16), Variable(Identifier { loc: Loc(1, 15, 8), name: "print" }), [Variable(Identifier { loc: Loc(1, 15, 15), name: "color2" })]))
Looking up "print"
Looking up "color2"
Compiling Return(Loc(1, 16, 13), None)
pop code
Looking up "main"
push CodeObject is <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>
Compiling Block(Loc(1, 18, 1), [])
pop code
Looking up "int"
Looking up "print"
after size before is 1
cccc after size before is 1
pop code
instruction LoadConst { value: Struct(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }) }
instruction StoreName { name: "Color", scope: Free }
instruction LoadConst { value: Code { code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9> } }
instruction LoadConst { value: String { value: "main" } }
instruction MakeFunction
instruction StoreName { name: "main", scope: Free }
instruction LoadConst { value: String { value: "a" } }
instruction LoadName { name: "int", scope: Free }
instruction BuildMap { size: 1, unpack: false, for_call: false }
instruction LoadConst { value: Code { code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16> } }
instruction LoadConst { value: String { value: "print" } }
instruction MakeFunction
instruction StoreName { name: "print", scope: Free }
instruction LoadName { name: "main", scope: Free }
instruction CallFunction { typ: Positional(0) }
instruction Pop
instruction LoadConst { value: None }
instruction ReturnValue
locals: RefCell { value: {} }
lasti:0
instruction LoadConst { value: Struct(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }) }
Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] })
lasti:1
instruction StoreName { name: "Color", scope: Free }
lasti:2
instruction LoadConst { value: Code { code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9> } }
Code(<code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>)
lasti:3
instruction LoadConst { value: String { value: "main" } }
Str("main")
lasti:4
instruction MakeFunction
code is:Code(<code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>)
lasti:5
instruction StoreName { name: "main", scope: Free }
lasti:6
instruction LoadConst { value: String { value: "a" } }
Str("a")
lasti:7
instruction LoadName { name: "int", scope: Free }
load_name value: Nil,栈名:0x7ffeef5cf578
lasti:8
instruction BuildMap { size: 1, unpack: false, for_call: false }
lasti:9
instruction LoadConst { value: Code { code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16> } }
Code(<code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>)
lasti:10
instruction LoadConst { value: String { value: "print" } }
Str("print")
lasti:11
instruction MakeFunction
code is:Code(<code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>)
lasti:12
instruction StoreName { name: "print", scope: Free }
lasti:13
instruction LoadName { name: "main", scope: Free }
load_name value: Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true }),栈名:0x7ffeef5cf578
lasti:14
instruction CallFunction { typ: Positional(0) }
call_function
ddd args:[Nil]
ddd func_def:Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true })
code is:Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true })
cao  function name:"main",equal = print: false
code is:Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true })
globals: ("print", Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true }))
globals: ("Color", Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }))
globals: ("main", Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true }))
locals: RefCell { value: {} }
locals: RefCell { value: {} }
lasti:0
instruction LoadName { name: "Color", scope: Global }
load_name value: Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }),栈名:0x7ffeef5c8f88
lasti:1
instruction LoadConst { value: String { value: "Red" } }
Str("Red")
lasti:2
instruction LoadConst { value: Integer { value: BigInt { sign: Plus, data: BigUint { data: [10] } } } }
Int(10)
lasti:3
instruction LoadBuildEnum(3)
lasti:4
instruction StoreName { name: "color2", scope: Free }
lasti:5
instruction LoadName { name: "print", scope: Free }
load_name value: Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true }),栈名:0x7ffeef5c8f88
lasti:6
instruction LoadName { name: "color2", scope: Free }
load_name value: Obj(RefCell { value: EnumObj(EnumObj { typ: Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }), field_map: Some([Int(10)]), item_name: Str("Red") }) }),栈名:0x7ffeef5c8f88
lasti:7
instruction CallFunction { typ: Positional(1) }
call_function
ddd args:[Obj(RefCell { value: EnumObj(EnumObj { typ: Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }), field_map: Some([Int(10)]), item_name: Str("Red") }) })]
ddd func_def:Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true })
code is:Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true })
cao  function name:"print",equal = print: true
结果为Obj(RefCell { value: EnumObj(EnumObj { typ: Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }), field_map: Some([Int(10)]), item_name: Str("Red") }) })
aaaaglobals: ("print", Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true }))
aaaaglobals: ("color2", Obj(RefCell { value: EnumObj(EnumObj { typ: Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }), field_map: Some([Int(10)]), item_name: Str("Red") }) }))
aaaaglobals: ("Color", Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }))
aaaaglobals: ("main", Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true }))
bbbbblocals: RefCell { value: {} }
bbbbblocals: RefCell { value: {} }
lasti:8
instruction LoadConst { value: None }
Nil
lasti:9
instruction ReturnValue
aaaaglobals: ("print", Fn(FnValue { name: "print", code: <code object print at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 16>, has_return: true }))
aaaaglobals: ("Color", Type(TypeValue { name: "Color", methods: [("is_warm", <code object is_warm at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 0>)], static_fields: [] }))
aaaaglobals: ("main", Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/enum.pan", line 9>, has_return: true }))
bbbbblocals: RefCell { value: {} }
lasti:15
instruction Pop
lasti:16
instruction LoadConst { value: None }
Nil
lasti:17
instruction ReturnValue

Process finished with exit code 0




