import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docs: [
    'intro',
    {
      type: 'category',
      label: 'Learn',
      items: ['learn/quickstart', 'learn/editor-setup', 'learn/debugging', 'learn/task-sharing-analysis', 'learn/language-tour'],
    },
    {
      type: 'category',
      label: 'Language',
      items: [
        'language/values-and-bindings',
        'language/functions',
        'language/indentation-and-layout',
        'language/control-flow',
        'language/collections',
        'language/type-declarations',
        'language/pattern-matching',
        'language/type-system',
        'language/structural-vs-named-annotations',
        'language/modules-imports-exports',
        'language/tasks-and-concurrency',
      ],
    },
    {
      type: 'category',
      label: 'Examples',
      items: ['examples/guided-examples', 'examples/common-patterns', 'examples/ffi-graphics'],
    },
    {
      type: 'category',
      label: 'Compilation',
      items: ['compiler/compilation', 'compiler/compile-to-dotnet', 'compiler/compile-to-csharp', 'compiler/compiler-optimizations', 'compiler/interpret-ir'],
    },
    {
      type: 'category',
      label: 'Stdlib',
      items: [
        'stdlib/stdlib-overview',
        'stdlib/stdlib-builtins',
        'stdlib/stdlib-console',
        'stdlib/stdlib-task',
        'stdlib/stdlib-list',
        'stdlib/stdlib-option',
        'stdlib/stdlib-map',
        'stdlib/stdlib-string',
        'stdlib/stdlib-scalars',
        'stdlib/stdlib-path',
        'stdlib/stdlib-glob',
        'stdlib/stdlib-semver',
        'stdlib/stdlib-encoding',
        'stdlib/stdlib-fs',
        'stdlib/stdlib-json',
        'stdlib/stdlib-regex',
        'stdlib/stdlib-xml',
        'stdlib/stdlib-hash',
        'stdlib/stdlib-guid',
        'stdlib/stdlib-recipes',
      ],
    },
    {
      type: 'category',
      label: 'Embedding',
      items: [
        'embedding/embedding-overview',
        'embedding/real-world-embedding',
        'embedding/fable-javascript',
        'embedding/register-externs',
        'embedding/host-managed-types',
        'embedding/resolver-and-includes',
        'embedding/sandbox-and-safety',
      ],
    },
    {
      type: 'category',
      label: 'Reference',
      items: [
        'reference/syntax-reference',
        'reference/native-types-reference',
        'reference/stdlib-reference',
        'reference/cli-reference',
        'reference/environment-configuration',
        'reference/editor-reference',
      ],
    },
    'whats-new',
  ],
};

export default sidebars;
