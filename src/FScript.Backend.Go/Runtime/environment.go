package fsruntime

import (
	"os"
	"path/filepath"
)

func Environment() map[string]any {
	arguments := make([]any, len(os.Args)-1)
	for index, argument := range os.Args[1:] {
		arguments[index] = argument
	}
	return map[string]any{"ScriptName": Some(filepath.Base(os.Args[0])), "Arguments": arguments}
}
