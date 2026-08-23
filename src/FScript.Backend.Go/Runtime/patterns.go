package fsruntime

import "strings"

type Pattern struct {
	Kind     string
	Symbol   int
	Literal  any
	Items    []Pattern
	Fields   []FieldPattern
	Clauses  []MapPatternClause
	Tail     *Pattern
	TypeName string
	CaseName string
}
type FieldPattern struct {
	Name    string
	Pattern Pattern
}
type MapPatternClause struct {
	Key   Pattern
	Value Pattern
}

func PatternPtr(value Pattern) *Pattern { return &value }

func mergePatternBindings(left map[int]any, right map[int]any) (map[int]any, bool) {
	result := map[int]any{}
	for key, value := range left {
		result[key] = value
	}
	for key, value := range right {
		if existing, found := result[key]; found && !Equal(existing, value) {
			return nil, false
		}
		result[key] = value
	}
	return result, true
}

func matchRuntimePattern(value any, pattern Pattern) (map[int]any, bool) {
	switch pattern.Kind {
	case "wildcard":
		return map[int]any{}, true
	case "binding":
		return map[int]any{pattern.Symbol: value}, true
	case "literal":
		return map[int]any{}, Equal(value, pattern.Literal)
	case "empty-list":
		values, ok := value.([]any)
		return map[int]any{}, ok && len(values) == 0
	case "none":
		option, ok := value.(Option)
		return map[int]any{}, ok && !option.HasValue
	case "some":
		option, ok := value.(Option)
		if !ok || !option.HasValue {
			return nil, false
		}
		return matchRuntimePattern(option.Value, pattern.Items[0])
	case "cons":
		values, ok := value.([]any)
		if !ok || len(values) == 0 {
			return nil, false
		}
		left, matched := matchRuntimePattern(values[0], pattern.Items[0])
		if !matched {
			return nil, false
		}
		right, matched := matchRuntimePattern(values[1:], pattern.Items[1])
		if !matched {
			return nil, false
		}
		return mergePatternBindings(left, right)
	case "tuple":
		values, ok := value.(Tuple)
		if !ok || len(values) != len(pattern.Items) {
			return nil, false
		}
		bindings := map[int]any{}
		for index, item := range pattern.Items {
			next, matched := matchRuntimePattern(values[index], item)
			if !matched {
				return nil, false
			}
			bindings, matched = mergePatternBindings(bindings, next)
			if !matched {
				return nil, false
			}
		}
		return bindings, true
	case "record":
		values, ok := value.(map[string]any)
		if !ok {
			return nil, false
		}
		bindings := map[int]any{}
		for _, field := range pattern.Fields {
			item, found := values[field.Name]
			if !found {
				return nil, false
			}
			next, matched := matchRuntimePattern(item, field.Pattern)
			if !matched {
				return nil, false
			}
			bindings, matched = mergePatternBindings(bindings, next)
			if !matched {
				return nil, false
			}
		}
		return bindings, true
	case "union":
		item, ok := value.(Union)
		if !ok || item.TypeName != pattern.TypeName || item.CaseName != pattern.CaseName || (len(pattern.Items) == 0) != !item.HasValue {
			return nil, false
		}
		if len(pattern.Items) == 0 {
			return map[int]any{}, true
		}
		return matchRuntimePattern(item.Value, pattern.Items[0])
	case "type":
		return map[int]any{}, TypeMatches(value, pattern.TypeName)
	case "map":
		item, ok := value.(OrderedMap)
		if !ok {
			return nil, false
		}
		remaining := append([]MapEntry{}, item.Entries...)
		bindings := map[int]any{}
		for _, clause := range pattern.Clauses {
			found := false
			for index, entry := range remaining {
				keyBindings, keyMatched := matchRuntimePattern(entry.Key, clause.Key)
				if !keyMatched {
					continue
				}
				withKey, merged := mergePatternBindings(bindings, keyBindings)
				if !merged {
					continue
				}
				valueBindings, valueMatched := matchRuntimePattern(entry.Value, clause.Value)
				if !valueMatched {
					continue
				}
				withValue, merged := mergePatternBindings(withKey, valueBindings)
				if !merged {
					continue
				}
				bindings = withValue
				remaining = append(remaining[:index], remaining[index+1:]...)
				found = true
				break
			}
			if !found {
				return nil, false
			}
		}
		if pattern.Tail != nil {
			tailBindings, matched := matchRuntimePattern(OrderedMap{Entries: remaining}, *pattern.Tail)
			if !matched {
				return nil, false
			}
			return mergePatternBindings(bindings, tailBindings)
		}
		if len(pattern.Clauses) > 0 || len(remaining) == 0 {
			return bindings, true
		}
		return nil, false
	default:
		return nil, false
	}
}

func PatternMatches(value any, pattern Pattern) bool {
	_, matched := matchRuntimePattern(value, pattern)
	return matched
}
func PatternBinding(value any, pattern Pattern, symbol int) any {
	bindings, matched := matchRuntimePattern(value, pattern)
	if !matched {
		panic("Pattern did not match")
	}
	return bindings[symbol]
}
func TypeMatches(value any, typeName string) bool {
	switch typeName {
	case "unit":
		_, ok := value.(struct{})
		return ok
	case "int":
		_, ok := value.(int64)
		return ok
	case "float":
		_, ok := value.(float64)
		return ok
	case "bool":
		_, ok := value.(bool)
		return ok
	case "string":
		_, ok := value.(string)
		return ok
	}
	if strings.HasSuffix(typeName, " list") {
		_, ok := value.([]any)
		return ok
	}
	if strings.HasSuffix(typeName, " option") {
		_, ok := value.(Option)
		return ok
	}
	if strings.HasPrefix(typeName, "{") {
		_, ok := value.(map[string]any)
		return ok
	}
	if item, ok := value.(Union); ok {
		return item.TypeName == typeName
	}
	_, record := value.(map[string]any)
	return record
}
