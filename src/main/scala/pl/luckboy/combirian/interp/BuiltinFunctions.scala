package pl.luckboy.combirian.interp
import scala.collection.immutable.HashMap
import scala.collection.mutable

object BuiltinFunctions 
{
  private def mapFromTupleValues(values: Seq[Value]) =
    values.foldLeft(Right(Map()): Either[ErrorValue, Map[SharedValue, SharedValue]]) {
      case (Right(values), TupleValue(Seq(key, value))) => Right(values + (key.shared -> value.shared))
      case _                                            => Left(ErrorValue("elements aren't pairs", Seq()))
    }
  
  private val argCountsAndFuns = Map[BuiltinFunction.Value, (Int, PartialFunction[Seq[Value], Value])](
      BuiltinFunction.Neg -> Tuple2(1, { 
        case Seq(IntValue(x))   => IntValue(- x)
        case Seq(FloatValue(x)) => FloatValue(- x)
      }),
      BuiltinFunction.Add -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y))                           => IntValue(x + y)
        case Seq(FloatValue(x), FloatValue(y))                       => FloatValue(x + y)
        case Seq(StringValue(x), StringValue(y))                     => StringValue(x + y)
        case Seq(VectorValue(elems), value)                          => VectorValue(elems :+ value.shared)
        case Seq(ArrayValue(elems), value)                           => NonSharedArrayValue((elems :+ value.shared).toArray)
        case Seq(map: AbstractMapValue, TupleValue(Seq(key, value))) => map.updated(key, value) 
      }),
      BuiltinFunction.Sub -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y))     => IntValue(x - y)
        case Seq(FloatValue(x), FloatValue(y)) => FloatValue(x - y)
        case Seq(map: AbstractMapValue, key)   => map.removed(key)
      }),
      BuiltinFunction.Mul -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y))     => IntValue(x * y)
        case Seq(FloatValue(x), FloatValue(y)) => FloatValue(x * y)
        case Seq(StringValue(x), IntValue(y))  => StringValue(x * y.toInt)
      }),
      BuiltinFunction.Div -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y))     => if(y != 0) IntValue(x / y) else ErrorValue("divide by zero", Seq())
        case Seq(FloatValue(x), FloatValue(y)) => FloatValue(x / y)
      }),
      BuiltinFunction.Mod -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y))     => if(y != 0) IntValue(x % y) else ErrorValue("divide by zero", Seq())
        case Seq(FloatValue(x), FloatValue(y)) => FloatValue(x % y)
      }),
      BuiltinFunction.Not -> Tuple2(1, {
        case Seq(TrueValue)   => FalseValue
        case Seq(FalseValue)  => TrueValue
        case Seq(IntValue(x)) => IntValue(~ x)
      }),
      BuiltinFunction.And -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x & y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => IntValue(x & y)
      }),
      BuiltinFunction.Or -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x | y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => IntValue(x | y)
      }),
      BuiltinFunction.Xor -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x ^ y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => IntValue(x ^ y)
      }),
      BuiltinFunction.LeftShift -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y)) => IntValue(x << y)
      }),
      BuiltinFunction.RightShift -> Tuple2(2, {
        case Seq(IntValue(x), IntValue(y)) => IntValue(x >> y)
      }),
      BuiltinFunction.Eq -> Tuple2(2, {
        case Seq(value1, value2) => if(value1 == value2) TrueValue else FalseValue
      }),
      BuiltinFunction.NotEq -> Tuple2(2, {
        case Seq(value1, value2) => if(value1 != value2) TrueValue else FalseValue
      }),
      BuiltinFunction.Lt -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x < y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => if(x < y) TrueValue else FalseValue
        case Seq(FloatValue(x), FloatValue(y))     => if(x < y) TrueValue else FalseValue
        case Seq(StringValue(x), StringValue(y))   => if(x < y) TrueValue else FalseValue
      }),
      BuiltinFunction.Le -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x <= y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => if(x <= y) TrueValue else FalseValue
        case Seq(FloatValue(x), FloatValue(y))     => if(x <= y) TrueValue else FalseValue
        case Seq(StringValue(x), StringValue(y))   => if(x <= y) TrueValue else FalseValue
      }),
      BuiltinFunction.Gt -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x > y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => if(x > y) TrueValue else FalseValue
        case Seq(FloatValue(x), FloatValue(y))     => if(x > y) TrueValue else FalseValue
        case Seq(StringValue(x), StringValue(y))   => if(x > y) TrueValue else FalseValue
      }),
      BuiltinFunction.Ge -> Tuple2(2, {
        case Seq(BooleanValue(x), BooleanValue(y)) => if(x >= y) TrueValue else FalseValue
        case Seq(IntValue(x), IntValue(y))         => if(x >= y) TrueValue else FalseValue
        case Seq(FloatValue(x), FloatValue(y))     => if(x >= y) TrueValue else FalseValue
        case Seq(StringValue(x), StringValue(y))   => if(x >= y) TrueValue else FalseValue
      }),
      BuiltinFunction.Vector -> Tuple2(2, {
        case Seq(IntValue(n), value) => VectorValue(Vector.fill(n.toInt)(value.shared))
      }),
      BuiltinFunction.Array -> Tuple2(2, {
        case Seq(IntValue(n), value) => NonSharedArrayValue(Array.fill(n.toInt)(value.shared))
      }),
      BuiltinFunction.Size -> Tuple2(1, {
        case Seq(AbstractSeqValue(elems)) => IntValue(elems.size)
        case Seq(AbstractMapValue(elems)) => IntValue(elems.size)
        case Seq(StringValue(x))          => IntValue(x.length)
      }),
      BuiltinFunction.Haskey -> Tuple2(2, {
        case Seq(IntValue(i), AbstractSeqValue(elems)) => if(i >= 0 && i < elems.size) TrueValue else FalseValue
        case Seq(_, _: AbstractSeqValue)               => FalseValue
        case Seq(key, AbstractMapValue(elems))         => if(elems.contains(key.shared)) TrueValue else FalseValue
        case Seq(IntValue(i), StringValue(x))          => if(i >= 0 && i < x.length) TrueValue else FalseValue
        case Seq(_, _: StringValue)                    => FalseValue
      }),
      BuiltinFunction.Keys -> Tuple2(1, {
        case Seq(AbstractSeqValue(elems)) => VectorValue((0 until elems.size).map(IntValue(_)))
        case Seq(AbstractMapValue(elems)) => VectorValue(elems.keys.toSeq)
        case Seq(StringValue(x))          => VectorValue((0 until x.size).map(IntValue(_)))
      }),
      BuiltinFunction.Keyarray -> Tuple2(1, {
        case Seq(AbstractSeqValue(elems)) => NonSharedArrayValue((0 until elems.size).map(IntValue(_)).toArray)
        case Seq(AbstractMapValue(elems)) => NonSharedArrayValue(elems.keys.toArray)
        case Seq(StringValue(x))          => NonSharedArrayValue((0 until x.size).map(IntValue(_)).toArray)
      }),
      BuiltinFunction.Nth -> Tuple2(2, {
        case Seq(IntValue(i), AbstractSeqValue(elems)) => if(i >= 0 && i < elems.size) elems(i.toInt) else ErrorValue("index out of bounds", Seq())
        case Seq(key, AbstractMapValue(elems))         => elems.getOrElse(key.shared, ErrorValue("key isn't found", Seq()))
        case Seq(IntValue(i), StringValue(x))          => if(i >= 0 && i < x.length) CharValue(x(i.toInt)) else ErrorValue("index out of bounds", Seq())
      }),
      BuiltinFunction.Updated -> Tuple2(3, {
        case Seq(IntValue(i), value, seq: AbstractSeqValue) => seq.updated(i.toInt, value)
        case Seq(key, value, map: AbstractMapValue)         => map.updated(key, value)
        case Seq(IntValue(i), CharValue(x), StringValue(y)) => if(i >= 0 && i < y.length) StringValue(y.updated(i.toInt, x)) else  ErrorValue("index out of bounds", Seq())
      }),
      BuiltinFunction.Istypeof -> Tuple2(1, {
        case Seq(value) =>
          value match {
            case _: BooleanValue => CharValue('b')
            case _: CharValue    => CharValue('c')
            case _: IntValue     => CharValue('i')
            case _: FloatValue   => CharValue('f')
            case _: StringValue  => CharValue('s')
            case _: FunValue     => CharValue('F')
            case _: TupleValue   => CharValue('t')
            case _: VectorValue  => CharValue('v')
            case _: MapValue     => CharValue('m')
            case _: ArrayValue   => CharValue('a')
            case _: HashValue    => CharValue('h')
            case _               => CharValue('!')
          }
      }),
      BuiltinFunction.Charfrom -> Tuple2(1, {
        case Seq(value: CharValue) => value
        case Seq(IntValue(x))      => CharValue(x.toChar)
        case Seq(FloatValue(x))    => CharValue(x.toChar)
      }),
      BuiltinFunction.Intfrom -> Tuple2(1, {
        case Seq(CharValue(x))    => IntValue(x)
        case Seq(value: IntValue) => value
        case Seq(FloatValue(x))   => IntValue(x.toInt)
      }),
      BuiltinFunction.Floatfrom -> Tuple2(1, {
        case Seq(CharValue(x))      => FloatValue(x)
        case Seq(IntValue(x))       => FloatValue(x)
        case Seq(value: FloatValue) => value
      }),
      BuiltinFunction.Stringfrom -> Tuple2(1, {
        case Seq(value: StringValue) => value
        case Seq(value)              => StringValue(value.toString)
      }),
      BuiltinFunction.Tuplefrom -> Tuple2(1, {
        case Seq(StringValue(x))          => NonSharedTupleValue(x.map(CharValue))
        case Seq(value: TupleValue)       => value
        case Seq(AbstractSeqValue(elems)) => NonSharedTupleValue(elems)
        case Seq(AbstractMapValue(elems)) => NonSharedTupleValue(elems.map { case (key, value) => NonSharedTupleValue(Array(key, value)) }.toSeq)
      }),
      BuiltinFunction.Vectorfrom -> Tuple2(1, {
        case Seq(StringValue(x))          => VectorValue(x.map(CharValue))
        case Seq(TupleValue(elems))       => VectorValue(elems.map { _.shared })
        case Seq(value: VectorValue)      => value
        case Seq(ArrayValue(elems))       => VectorValue(elems)
        case Seq(AbstractMapValue(elems)) => VectorValue(elems.map { case (key, value) => SharedTupleValue(Vector(key, value)) }.toSeq)
      }),
      BuiltinFunction.Mapfrom -> Tuple2(1, {
        case Seq(AbstractSeqValue(elems))  => mapFromTupleValues(elems).fold(value => value, MapValue(_))
        case Seq(value: MapValue)          => value
        case Seq(AbstractMapValue(elems))  => MapValue(elems)
      }),
      BuiltinFunction.Arrayfrom -> Tuple2(1, {
        case Seq(StringValue(x))          => NonSharedArrayValue(x.map(CharValue).toArray)
        case Seq(TupleValue(elems))       => NonSharedArrayValue(elems.map { _.shared }.toArray)
        case Seq(VectorValue(elems))      => NonSharedArrayValue(elems.toArray)
        case Seq(value: ArrayValue)       => value
        case Seq(AbstractMapValue(elems)) => NonSharedArrayValue(elems.map { case (key, value) => SharedTupleValue(Array(key, value)) }.toArray)
      }),
      BuiltinFunction.Hashfrom -> Tuple2(1, {
        case Seq(AbstractSeqValue(elems))  => mapFromTupleValues(elems).fold(value => value, map => NonSharedHashValue(mutable.HashMap() ++= map))
        case Seq(value: HashValue)         => value
        case Seq(AbstractMapValue(elems))  => NonSharedHashValue(mutable.HashMap() ++= elems)
      }),
      BuiltinFunction.Size2 -> Tuple2(1, {
        case Seq(first @ AbstractSeqValue(elems)) => NonSharedTupleValue(Array(first, IntValue(elems.size)))
        case Seq(first @ AbstractMapValue(elems)) => NonSharedTupleValue(Array(first, IntValue(elems.size)))
        case Seq(first @ StringValue(x))          => NonSharedTupleValue(Array(first, IntValue(x.length)))
      }),
      BuiltinFunction.Haskey2 -> Tuple2(2, {
        case Seq(IntValue(i), first @ AbstractSeqValue(elems)) => NonSharedTupleValue(Array(first, if(i >= 0 && i < elems.size) TrueValue else FalseValue))
        case Seq(_, first: AbstractSeqValue)                   => NonSharedTupleValue(Array(first, FalseValue))
        case Seq(key, first @ AbstractMapValue(elems))         => NonSharedTupleValue(Array(first, if(elems.contains(key.shared)) TrueValue else FalseValue))
        case Seq(IntValue(i), first @ StringValue(x))          => NonSharedTupleValue(Array(first, if(i >= 0 && i < x.length) TrueValue else FalseValue))
        case Seq(_, first: StringValue)                        => NonSharedTupleValue(Array(first, FalseValue))
      }),
      BuiltinFunction.Keys2 -> Tuple2(1, {
        case Seq(first @ AbstractSeqValue(elems)) => NonSharedTupleValue(Array(first, VectorValue((0 until elems.size).map(IntValue(_)))))
        case Seq(first @ AbstractMapValue(elems)) => NonSharedTupleValue(Array(first, VectorValue(elems.keys.toSeq)))
        case Seq(first @ StringValue(x))          => NonSharedTupleValue(Array(first, VectorValue((0 until x.length).map(IntValue(_)).toSeq)))
      }),
      BuiltinFunction.Keyarray2 -> Tuple2(1, {
        case Seq(first @ AbstractSeqValue(elems)) => NonSharedTupleValue(Array(first, NonSharedArrayValue((0 until elems.size).map(IntValue(_)).toArray)))
        case Seq(first @ AbstractMapValue(elems)) => NonSharedTupleValue(Array(first, NonSharedArrayValue(elems.keys.toArray)))
        case Seq(first @ StringValue(x))          => NonSharedTupleValue(Array(first, NonSharedArrayValue((0 until x.length).map(IntValue(_)).toArray)))
      }),
      BuiltinFunction.Nth2 -> Tuple2(2, {
        case Seq(IntValue(i), first @ AbstractSeqValue(elems)) => if(i >= 0 && i < elems.size) NonSharedTupleValue(Array(first, elems(i.toInt))) else ErrorValue("index out of bounds", Seq())
        case Seq(key, first @ AbstractMapValue(elems))         => elems.get(key.shared).map { value => NonSharedTupleValue(Array(first, value)) }.getOrElse(ErrorValue("key isn't found", Seq()))
        case Seq(IntValue(i), first @ StringValue(x))          => if(i >= 0 && i < x.length) NonSharedTupleValue(Array(first, CharValue(x(i.toInt)))) else ErrorValue("index out of bounds", Seq())
      })
      )
}