package fsruntime

import (
	"fmt"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"unicode/utf16"
)

type Option struct {
	HasValue bool
	Value    any
}
type Tuple []any
type TypeToken struct{ Value string }
type Union struct {
	TypeName string
	CaseName string
	HasValue bool
	Value    any
}
type UnionCarrier interface {
	FScriptUnion() Union
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

func utf16Index(value string, needle string) int64 {
	valueUnits := utf16.Encode([]rune(value))
	needleUnits := utf16.Encode([]rune(needle))
	if len(needleUnits) == 0 {
		return 0
	}
	for start := 0; start+len(needleUnits) <= len(valueUnits); start++ {
		matches := true
		for offset, unit := range needleUnits {
			if valueUnits[start+offset] != unit {
				matches = false
				break
			}
		}
		if matches {
			return int64(start)
		}
	}
	return -1
}

func utf16Substring(value string, start int64, length int64) Option {
	units := utf16.Encode([]rune(value))
	if start < 0 || length < 0 || start+length > int64(len(units)) {
		return None()
	}
	return Some(string(utf16.Decode(units[start : start+length])))
}

func Unit(value any) struct{} { return struct{}{} }
func Int(value any) int64     { return value.(int64) }
func Float(value any) float64 { return value.(float64) }
func Bool(value any) bool     { return value.(bool) }
func String(value any) string { return value.(string) }
func List(value any) []any    { return value.([]any) }
func AsTuple(value any) Tuple { return value.(Tuple) }
func TryRecord(value any) (map[string]any, bool) {
	if record, ok := value.(map[string]any); ok {
		return record, true
	}
	reflected := reflect.ValueOf(value)
	if reflected.Kind() == reflect.Pointer {
		reflected = reflected.Elem()
	}
	if reflected.Kind() != reflect.Struct {
		return nil, false
	}
	result := map[string]any{}
	for index := 0; index < reflected.NumField(); index++ {
		field := reflected.Type().Field(index)
		if name := field.Tag.Get("fscript"); name != "" {
			result[name] = reflected.Field(index).Interface()
		}
	}
	if len(result) == 0 {
		return nil, false
	}
	return result, true
}
func Record(value any) map[string]any {
	record, ok := TryRecord(value)
	if !ok {
		panic("Record access requires a record")
	}
	return record
}
func AsOption(value any) Option { return value.(Option) }
func TryUnion(value any) (Union, bool) {
	if item, ok := value.(Union); ok {
		return item, true
	}
	if item, ok := value.(UnionCarrier); ok {
		return item.FScriptUnion(), true
	}
	return Union{}, false
}
func AsUnion(value any) Union {
	item, ok := TryUnion(value)
	if !ok {
		panic("Union access requires a union")
	}
	return item
}
func AsMap(value any) OrderedMap { return value.(OrderedMap) }
func Some(value any) Option      { return Option{HasValue: true, Value: value} }
func None() Option               { return Option{} }
func IsSome(value any) bool      { return AsOption(value).HasValue }
func OptionValue(value any) any  { return AsOption(value).Value }
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
func UnionHasValue(value any) bool { return AsUnion(value).HasValue }
func UnionValue(value any) any     { return AsUnion(value).Value }
func HasField(value any, name string) bool {
	record, ok := TryRecord(value)
	if !ok {
		return false
	}
	_, found := record[name]
	return found
}
func normalizeStructural(value any) any {
	if item, ok := TryUnion(value); ok {
		if item.HasValue {
			item.Value = normalizeStructural(item.Value)
		}
		return item
	}
	reflected := reflect.ValueOf(value)
	if reflected.IsValid() && (reflected.Kind() == reflect.Struct || (reflected.Kind() == reflect.Pointer && !reflected.IsNil() && reflected.Elem().Kind() == reflect.Struct)) {
		if record, ok := TryRecord(value); ok {
			result := map[string]any{}
			for name, field := range record {
				result[name] = normalizeStructural(field)
			}
			return result
		}
	}
	switch item := value.(type) {
	case []any:
		result := make([]any, len(item))
		for index, element := range item {
			result[index] = normalizeStructural(element)
		}
		return result
	case Tuple:
		result := make(Tuple, len(item))
		for index, element := range item {
			result[index] = normalizeStructural(element)
		}
		return result
	case map[string]any:
		result := map[string]any{}
		for name, field := range item {
			result[name] = normalizeStructural(field)
		}
		return result
	case Option:
		if item.HasValue {
			item.Value = normalizeStructural(item.Value)
		}
		return item
	case OrderedMap:
		entries := make([]MapEntry, len(item.Entries))
		for index, entry := range item.Entries {
			entries[index] = MapEntry{Key: normalizeStructural(entry.Key), Value: normalizeStructural(entry.Value)}
		}
		return OrderedMap{Entries: entries}
	default:
		return value
	}
}
func Equal(left any, right any) bool {
	return reflect.DeepEqual(normalizeStructural(left), normalizeStructural(right))
}
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
		switch strings.ToLower(String(args[0])) {
		case "true":
			return Some(true)
		case "false":
			return Some(false)
		default:
			return None()
		}
	case "Int.toString", "Float.toString", "Bool.toString":
		return Format(args[0])
	case "String.replace":
		return strings.ReplaceAll(String(args[2]), String(args[0]), String(args[1]))
	case "String.indexOf":
		index := utf16Index(String(args[1]), String(args[0]))
		if index < 0 {
			return None()
		}
		return Some(index)
	case "String.toLower":
		return strings.ToLower(String(args[0]))
	case "String.toUpper":
		return strings.ToUpper(String(args[0]))
	case "String.substring":
		start, length, value := Int(args[0]), Int(args[1]), String(args[2])
		return utf16Substring(value, start, length)
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
func Format(value any) string { return formatValue(value, false) }
func formatValue(value any, nested bool) string {
	if item, ok := TryUnion(value); ok {
		if !item.HasValue {
			return item.CaseName
		}
		return item.CaseName + " " + formatValue(item.Value, true)
	}
	switch item := value.(type) {
	case bool:
		if item {
			return "true"
		}
		return "false"
	case string:
		return strconv.Quote(item)
	case TypeToken:
		return "<type " + item.Value + ">"
	case struct{}:
		return "()"
	case Option:
		if !item.HasValue {
			return "None"
		}
		return "Some " + formatValue(item.Value, true)
	case OrderedMap:
		parts := make([]string, len(item.Entries))
		for index, entry := range item.Entries {
			parts[index] = formatValue(entry.Key, true) + " => " + formatValue(entry.Value, true)
		}
		return "map { " + strings.Join(parts, "; ") + " }"
	case []any:
		parts := make([]string, len(item))
		for index, value := range item {
			parts[index] = formatValue(value, true)
		}
		return "[" + strings.Join(parts, ";") + "]"
	case Tuple:
		parts := make([]string, len(item))
		for index, value := range item {
			parts[index] = formatValue(value, true)
		}
		return "(" + strings.Join(parts, ", ") + ")"
	case map[string]any:
		names := make([]string, 0, len(item))
		for name := range item {
			names = append(names, name)
		}
		sort.Strings(names)
		parts := make([]string, len(names))
		for index, name := range names {
			parts[index] = name + " = " + formatValue(item[name], true)
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	default:
		if reflected := reflect.ValueOf(value); reflected.IsValid() && (reflected.Kind() == reflect.Struct || (reflected.Kind() == reflect.Pointer && !reflected.IsNil() && reflected.Elem().Kind() == reflect.Struct)) {
			if record, ok := TryRecord(value); ok {
				return formatValue(record, nested)
			}
		}
		return fmt.Sprint(value)
	}
}
func Interpolate(values []any) string {
	var result strings.Builder
	for _, value := range values {
		result.WriteString(formatInterpolationValue(value))
	}
	return result.String()
}

func formatInterpolationValue(value any) string {
	if item, ok := TryUnion(value); ok {
		if !item.HasValue {
			return item.CaseName
		}
		return item.CaseName + " " + formatInterpolationValue(item.Value)
	}
	switch item := value.(type) {
	case string:
		return item
	case TypeToken:
		return "<type " + item.Value + ">"
	case struct{}:
		return "()"
	case Option:
		if !item.HasValue {
			return "None"
		}
		return "Some " + formatInterpolationValue(item.Value)
	case OrderedMap:
		parts := make([]string, len(item.Entries))
		for index, entry := range item.Entries {
			key, ok := entry.Key.(string)
			if ok {
				parts[index] = strconv.Quote(key) + " => " + formatInterpolationValue(entry.Value)
			} else {
				parts[index] = formatInterpolationValue(entry.Key) + " => " + formatInterpolationValue(entry.Value)
			}
		}
		return "map { " + strings.Join(parts, "; ") + " }"
	case []any:
		parts := make([]string, len(item))
		for index, value := range item {
			parts[index] = formatInterpolationValue(value)
		}
		return "[" + strings.Join(parts, ";") + "]"
	case Tuple:
		parts := make([]string, len(item))
		for index, value := range item {
			parts[index] = formatInterpolationValue(value)
		}
		return "(" + strings.Join(parts, ", ") + ")"
	case map[string]any:
		names := make([]string, 0, len(item))
		for name := range item {
			names = append(names, name)
		}
		sort.Strings(names)
		parts := make([]string, len(names))
		for index, name := range names {
			parts[index] = name + " = " + formatInterpolationValue(item[name])
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	default:
		if record, ok := TryRecord(value); ok {
			return formatInterpolationValue(record)
		}
		return fmt.Sprint(value)
	}
}
func Fail(value any) { panic(fmt.Sprint(value)) }
