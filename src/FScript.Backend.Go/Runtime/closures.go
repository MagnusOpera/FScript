package fsruntime

type Closure struct{ Call func(any) any }

func AsClosure(value any) Closure         { return value.(Closure) }
func Call(function any, argument any) any { return AsClosure(function).Call(argument) }

func IntrinsicClosure(name string, arity int) Closure {
	var build func([]any) Closure
	build = func(arguments []any) Closure {
		return Closure{Call: func(value any) any {
			next := append(append([]any{}, arguments...), value)
			if len(next) == arity {
				return DispatchIntrinsic(name, next)
			}
			return build(next)
		}}
	}
	return build(nil)
}

func HostClosure(name string, arity int) Closure {
	var build func(int) Closure
	build = func(count int) Closure {
		return Closure{Call: func(value any) any {
			_ = value
			if count+1 == arity {
				panic("Unsupported host external: " + name)
			}
			return build(count + 1)
		}}
	}
	return build(0)
}

func DispatchIntrinsic(name string, args []any) any {
	switch name {
	case "List.map":
		callback, values := AsClosure(args[0]), List(args[1])
		result := make([]any, len(values))
		for index, value := range values {
			result[index] = callback.Call(value)
		}
		return result
	case "List.iter":
		callback := AsClosure(args[0])
		for _, value := range List(args[1]) {
			callback.Call(value)
		}
		return struct{}{}
	case "List.choose":
		callback, result := AsClosure(args[0]), []any{}
		for _, value := range List(args[1]) {
			chosen := AsOption(callback.Call(value))
			if chosen.HasValue {
				result = append(result, chosen.Value)
			}
		}
		return result
	case "List.collect":
		callback, result := AsClosure(args[0]), []any{}
		for _, value := range List(args[1]) {
			result = append(result, List(callback.Call(value))...)
		}
		return result
	case "List.exists":
		callback := AsClosure(args[0])
		for _, value := range List(args[1]) {
			if Bool(callback.Call(value)) {
				return true
			}
		}
		return false
	case "List.distinct":
		result := []any{}
		for _, value := range List(args[0]) {
			found := false
			for _, existing := range result {
				if Equal(existing, value) {
					found = true
					break
				}
			}
			if !found {
				result = append(result, value)
			}
		}
		return result
	case "List.fold":
		callback, state := AsClosure(args[0]), args[1]
		for _, value := range List(args[2]) {
			state = Call(callback.Call(state), value)
		}
		return state
	case "List.filter":
		callback, result := AsClosure(args[0]), []any{}
		for _, value := range List(args[1]) {
			if Bool(callback.Call(value)) {
				result = append(result, value)
			}
		}
		return result
	case "List.tryFind":
		callback := AsClosure(args[0])
		for _, value := range List(args[1]) {
			if Bool(callback.Call(value)) {
				return Some(value)
			}
		}
		return None()
	case "List.tryGet":
		callback := AsClosure(args[0])
		for index, value := range List(args[1]) {
			if Bool(callback.Call(value)) {
				return Some(int64(index))
			}
		}
		return None()
	case "List.tryItem":
		index, values := Int(args[0]), List(args[1])
		if index < 0 || index >= int64(len(values)) {
			return None()
		}
		return Some(values[index])
	case "Option.defaultWith":
		option := AsOption(args[1])
		if option.HasValue {
			return option.Value
		}
		return Call(args[0], struct{}{})
	case "Option.map":
		option := AsOption(args[1])
		if !option.HasValue {
			return None()
		}
		return Some(Call(args[0], option.Value))
	case "Map.add":
		values := append([]MapEntry{}, AsMap(args[2]).Entries...)
		return OrderedMap{Entries: mapSet(values, args[0], args[1], true)}
	case "Map.ofList":
		values := []MapEntry{}
		for _, pair := range List(args[0]) {
			tuple := List(pair)
			values = mapSet(values, tuple[0], tuple[1], true)
		}
		return OrderedMap{Entries: values}
	case "Map.fold":
		callback, state := AsClosure(args[0]), args[1]
		for _, entry := range AsMap(args[2]).Entries {
			state = Call(Call(callback.Call(state), entry.Key), entry.Value)
		}
		return state
	case "Map.filter":
		callback, result := AsClosure(args[0]), []MapEntry{}
		for _, entry := range AsMap(args[1]).Entries {
			if Bool(Call(callback.Call(entry.Key), entry.Value)) {
				result = append(result, entry)
			}
		}
		return OrderedMap{Entries: result}
	case "Map.choose":
		callback, result := AsClosure(args[0]), []MapEntry{}
		for _, entry := range AsMap(args[1]).Entries {
			chosen := AsOption(Call(callback.Call(entry.Key), entry.Value))
			if chosen.HasValue {
				result = append(result, MapEntry{Key: entry.Key, Value: chosen.Value})
			}
		}
		return OrderedMap{Entries: result}
	case "Map.map":
		callback, result := AsClosure(args[0]), []MapEntry{}
		for _, entry := range AsMap(args[1]).Entries {
			result = append(result, MapEntry{Key: entry.Key, Value: callback.Call(entry.Value)})
		}
		return OrderedMap{Entries: result}
	case "Map.iter":
		callback := AsClosure(args[0])
		for _, entry := range AsMap(args[1]).Entries {
			Call(callback.Call(entry.Key), entry.Value)
		}
		return struct{}{}
	case "Map.remove":
		result := []MapEntry{}
		for _, entry := range AsMap(args[1]).Entries {
			if !Equal(entry.Key, args[0]) {
				result = append(result, entry)
			}
		}
		return OrderedMap{Entries: result}
	default:
		return Intrinsic(name, args)
	}
}
