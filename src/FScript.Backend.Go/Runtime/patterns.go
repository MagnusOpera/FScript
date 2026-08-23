package fsruntime

import "strings"

type ProjectionStep struct {
	Kind  string
	Index int
	Name  string
}

type PatternCheck struct {
	Kind       string
	Projection []ProjectionStep
	Literal    any
	Arity      int
	Name       string
	TypeName   string
	CaseName   string
	HasPayload bool
}

type PatternBindingPlan struct {
	Symbol     int
	Projection []ProjectionStep
}

type PatternPlan struct {
	Checks   []PatternCheck
	Bindings []PatternBindingPlan
	Maps     []MapPatternPlan
}

type MapPatternPlan struct {
	Projection            []ProjectionStep
	Clauses               []MapPatternClausePlan
	Tail                  *PatternPlan
	RequireEmptyRemainder bool
}

type MapPatternClausePlan struct {
	Key   PatternPlan
	Value PatternPlan
}

func PatternPlanPtr(value PatternPlan) *PatternPlan { return &value }

func copyPatternBindings(values map[int]any) map[int]any {
	result := map[int]any{}
	for key, value := range values {
		result[key] = value
	}
	return result
}

func addPatternBinding(values map[int]any, symbol int, value any) (map[int]any, bool) {
	if existing, found := values[symbol]; found && !Equal(existing, value) {
		return nil, false
	}
	result := copyPatternBindings(values)
	result[symbol] = value
	return result, true
}

func projectPatternValue(value any, projection []ProjectionStep) (any, bool) {
	current := value
	for _, step := range projection {
		switch step.Kind {
		case "list-head":
			values, ok := current.([]any)
			if !ok || len(values) == 0 {
				return nil, false
			}
			current = values[0]
		case "list-tail":
			values, ok := current.([]any)
			if !ok || len(values) == 0 {
				return nil, false
			}
			current = values[1:]
		case "tuple-item":
			values, ok := current.(Tuple)
			if !ok || step.Index < 0 || step.Index >= len(values) {
				return nil, false
			}
			current = values[step.Index]
		case "record-field":
			values, ok := TryRecord(current)
			if !ok {
				return nil, false
			}
			item, found := values[step.Name]
			if !found {
				return nil, false
			}
			current = item
		case "option-value":
			option, ok := current.(Option)
			if !ok || !option.HasValue {
				return nil, false
			}
			current = option.Value
		case "union-payload":
			union, ok := TryUnion(current)
			if !ok || !union.HasValue {
				return nil, false
			}
			current = union.Value
		default:
			return nil, false
		}
	}
	return current, true
}

func checkPatternPlan(root any, check PatternCheck) bool {
	value, ok := projectPatternValue(root, check.Projection)
	if !ok {
		return false
	}
	switch check.Kind {
	case "literal":
		return Equal(value, check.Literal)
	case "empty-list":
		values, ok := value.([]any)
		return ok && len(values) == 0
	case "non-empty-list":
		values, ok := value.([]any)
		return ok && len(values) > 0
	case "tuple-arity":
		values, ok := value.(Tuple)
		return ok && len(values) == check.Arity
	case "record-field":
		values, ok := TryRecord(value)
		if !ok {
			return false
		}
		_, found := values[check.Name]
		return found
	case "some":
		option, ok := value.(Option)
		return ok && option.HasValue
	case "none":
		option, ok := value.(Option)
		return ok && !option.HasValue
	case "union-case":
		item, ok := TryUnion(value)
		return ok && item.TypeName == check.TypeName && item.CaseName == check.CaseName && item.HasValue == check.HasPayload
	case "type":
		return TypeMatches(value, check.TypeName)
	default:
		return false
	}
}

func matchPatternPlan(value any, plan PatternPlan, initial map[int]any) (map[int]any, bool) {
	for _, check := range plan.Checks {
		if !checkPatternPlan(value, check) {
			return nil, false
		}
	}
	bindings := copyPatternBindings(initial)
	for _, binding := range plan.Bindings {
		item, ok := projectPatternValue(value, binding.Projection)
		if !ok {
			return nil, false
		}
		bindings, ok = addPatternBinding(bindings, binding.Symbol, item)
		if !ok {
			return nil, false
		}
	}
	for _, mapPlan := range plan.Maps {
		projected, ok := projectPatternValue(value, mapPlan.Projection)
		if !ok {
			return nil, false
		}
		item, ok := projected.(OrderedMap)
		if !ok {
			return nil, false
		}
		bindings, ok = matchMapPatternPlan(item.Entries, mapPlan, bindings)
		if !ok {
			return nil, false
		}
	}
	return bindings, true
}

func matchMapPatternPlan(entries []MapEntry, plan MapPatternPlan, initial map[int]any) (map[int]any, bool) {
	var matchClauses func([]MapEntry, []MapPatternClausePlan, map[int]any) (map[int]any, bool)
	matchClauses = func(remaining []MapEntry, clauses []MapPatternClausePlan, bindings map[int]any) (map[int]any, bool) {
		if len(clauses) == 0 {
			if plan.Tail != nil {
				return matchPatternPlan(OrderedMap{Entries: remaining}, *plan.Tail, bindings)
			}
			if plan.RequireEmptyRemainder && len(remaining) != 0 {
				return nil, false
			}
			return bindings, true
		}
		clause := clauses[0]
		for index, entry := range remaining {
			withKey, matched := matchPatternPlan(entry.Key, clause.Key, bindings)
			if !matched {
				continue
			}
			withValue, matched := matchPatternPlan(entry.Value, clause.Value, withKey)
			if !matched {
				continue
			}
			next := append([]MapEntry{}, remaining[:index]...)
			next = append(next, remaining[index+1:]...)
			if result, matched := matchClauses(next, clauses[1:], withValue); matched {
				return result, true
			}
		}
		return nil, false
	}
	return matchClauses(append([]MapEntry{}, entries...), plan.Clauses, initial)
}

func MatchPatternPlan(value any, plan PatternPlan) (map[int]any, bool) {
	return matchPatternPlan(value, plan, map[int]any{})
}

func PatternBinding(bindings map[int]any, symbol int) any {
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
		_, ok := TryRecord(value)
		return ok
	}
	if item, ok := TryUnion(value); ok {
		return item.TypeName == typeName
	}
	_, record := TryRecord(value)
	return record
}
