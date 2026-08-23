package fsruntime

import (
	"fmt"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

type Option struct {
	HasValue bool
	Value    any
}
type Union struct {
	TypeName string
	CaseName string
	HasValue bool
	Value    any
}
type MapEntry struct {
	Key   any
	Value any
}
type OrderedMap struct{ Entries []MapEntry }
type MapPart struct {
	IsSpread bool
	Key      any
	Value    any
	Map      OrderedMap
}

func Unit(value any) struct{}         { return struct{}{} }
func Int(value any) int64             { return value.(int64) }
func Float(value any) float64         { return value.(float64) }
func Bool(value any) bool             { return value.(bool) }
func String(value any) string         { return value.(string) }
func List(value any) []any            { return value.([]any) }
func Record(value any) map[string]any { return value.(map[string]any) }
func AsOption(value any) Option       { return value.(Option) }
func AsUnion(value any) Union         { return value.(Union) }
func AsMap(value any) OrderedMap      { return value.(OrderedMap) }
func Some(value any) Option           { return Option{HasValue: true, Value: value} }
func None() Option                    { return Option{} }
func IsSome(value any) bool           { return AsOption(value).HasValue }
func OptionValue(value any) any       { return AsOption(value).Value }
func MakeUnion(typeName string, caseName string) Union {
	return Union{TypeName: typeName, CaseName: caseName}
}
func MakeUnionValue(typeName string, caseName string, value any) Union {
	return Union{TypeName: typeName, CaseName: caseName, HasValue: true, Value: value}
}
func IsUnion(value any, typeName string, caseName string) bool {
	item := AsUnion(value)
	return item.TypeName == typeName && item.CaseName == caseName
}
func UnionHasValue(value any) bool         { return AsUnion(value).HasValue }
func UnionValue(value any) any             { return AsUnion(value).Value }
func HasField(value any, name string) bool { _, ok := Record(value)[name]; return ok }
func Equal(left any, right any) bool       { return reflect.DeepEqual(left, right) }
func Cons(head any, tail []any) []any      { return append([]any{head}, tail...) }
func Append(left []any, right []any) []any { return append(append([]any{}, left...), right...) }
func Tail(value any) []any                 { return append([]any{}, List(value)[1:]...) }
func Range(first int64, last int64) []any {
	step := int64(1)
	if first > last {
		step = -1
	}
	values := []any{}
	for current := first; ; current += step {
		values = append(values, current)
		if current == last {
			break
		}
	}
	return values
}
func UpdateRecord(value any, updates map[string]any) map[string]any {
	result := map[string]any{}
	for key, item := range Record(value) {
		result[key] = item
	}
	for key, item := range updates {
		result[key] = item
	}
	return result
}
func Pair(key any, value any) MapPart { return MapPart{Key: key, Value: value} }
func SpreadMap(value any) MapPart     { return MapPart{IsSpread: true, Map: AsMap(value)} }
func mapSet(entries []MapEntry, key any, value any, replace bool) []MapEntry {
	for index, entry := range entries {
		if Equal(entry.Key, key) {
			if replace {
				entries[index].Value = value
			}
			return entries
		}
	}
	return append(entries, MapEntry{Key: key, Value: value})
}
func BuildMap(parts []MapPart) OrderedMap {
	entries := []MapEntry{}
	for _, part := range parts {
		if part.IsSpread {
			for _, entry := range part.Map.Entries {
				entries = mapSet(entries, entry.Key, entry.Value, false)
			}
		} else {
			entries = mapSet(entries, part.Key, part.Value, true)
		}
	}
	sort.SliceStable(entries, func(left int, right int) bool { return Format(entries[left].Key) < Format(entries[right].Key) })
	return OrderedMap{Entries: entries}
}
func Index(value any, index any) Option {
	switch target := value.(type) {
	case []any:
		position := Int(index)
		if position < 0 || position >= int64(len(target)) {
			return None()
		}
		return Some(target[position])
	case OrderedMap:
		for _, entry := range target.Entries {
			if Equal(entry.Key, index) {
				return Some(entry.Value)
			}
		}
		return None()
	default:
		panic("Index access requires a list or map")
	}
}
func Intrinsic(name string, args []any) any {
	switch name {
	case "Console.writeLine":
		fmt.Println(String(args[0]))
		return struct{}{}
	case "ignore":
		return struct{}{}
	case "List.empty":
		return []any{}
	case "Map.empty":
		return OrderedMap{}
	case "Int.tryParse":
		value, error := strconv.ParseInt(String(args[0]), 10, 64)
		if error != nil {
			return None()
		}
		return Some(value)
	case "Float.tryParse":
		value, error := strconv.ParseFloat(String(args[0]), 64)
		if error != nil {
			return None()
		}
		return Some(value)
	case "Bool.tryParse":
		value, error := strconv.ParseBool(String(args[0]))
		if error != nil {
			return None()
		}
		return Some(value)
	case "Int.toString", "Float.toString", "Bool.toString":
		return Format(args[0])
	case "String.replace":
		return strings.ReplaceAll(String(args[2]), String(args[0]), String(args[1]))
	case "String.indexOf":
		index := strings.Index(String(args[1]), String(args[0]))
		if index < 0 {
			return None()
		}
		return Some(int64(index))
	case "String.toLower":
		return strings.ToLower(String(args[0]))
	case "String.toUpper":
		return strings.ToUpper(String(args[0]))
	case "String.substring":
		start, length, value := Int(args[0]), Int(args[1]), String(args[2])
		if start < 0 || length < 0 || start+length > int64(len(value)) {
			return None()
		}
		return Some(value[start : start+length])
	case "String.concat":
		values := List(args[1])
		parts := make([]string, len(values))
		for index, value := range values {
			parts[index] = String(value)
		}
		return strings.Join(parts, String(args[0]))
	case "String.split":
		values := strings.Split(String(args[1]), String(args[0]))
		result := make([]any, len(values))
		for index, value := range values {
			result[index] = value
		}
		return result
	case "String.endsWith":
		return strings.HasSuffix(String(args[1]), String(args[0]))
	case "List.length":
		return int64(len(List(args[0])))
	case "List.rev":
		values := List(args[0])
		result := make([]any, len(values))
		for index, value := range values {
			result[len(values)-index-1] = value
		}
		return result
	case "List.tryHead":
		values := List(args[0])
		if len(values) == 0 {
			return None()
		}
		return Some(values[0])
	case "List.tail":
		return Tail(args[0])
	case "List.append":
		return Append(List(args[0]), List(args[1]))
	case "List.contains":
		for _, value := range List(args[1]) {
			if Equal(args[0], value) {
				return true
			}
		}
		return false
	case "Option.isNone":
		return !IsSome(args[0])
	case "Option.isSome":
		return IsSome(args[0])
	case "Option.defaultValue":
		option := AsOption(args[1])
		if option.HasValue {
			return option.Value
		}
		return args[0]
	case "Map.count":
		return int64(len(AsMap(args[0]).Entries))
	case "Map.containsKey":
		return IsSome(Index(args[1], args[0]))
	case "Map.tryGet":
		return Index(args[1], args[0])
	default:
		panic("Unsupported FScript intrinsic: " + name)
	}
}
func Format(value any) string {
	switch item := value.(type) {
	case bool:
		if item {
			return "true"
		}
		return "false"
	case Option:
		if !item.HasValue {
			return "None"
		}
		return "Some " + Format(item.Value)
	case Union:
		if !item.HasValue {
			return item.CaseName
		}
		return item.CaseName + " " + Format(item.Value)
	case OrderedMap:
		parts := make([]string, len(item.Entries))
		for index, entry := range item.Entries {
			parts[index] = "[" + Format(entry.Key) + "] = " + Format(entry.Value)
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	case []any:
		parts := make([]string, len(item))
		for index, value := range item {
			parts[index] = Format(value)
		}
		return "[" + strings.Join(parts, ";") + "]"
	default:
		return fmt.Sprint(value)
	}
}
func Interpolate(values []any) string {
	var result strings.Builder
	for _, value := range values {
		result.WriteString(Format(value))
	}
	return result.String()
}
func Fail(value any) { panic(fmt.Sprint(value)) }
