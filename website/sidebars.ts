import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docs: [
    'intro',
    {
      type: 'category',
      label: 'Learn',
      items: ['learn/quickstart', 'learn/editor-setup', 'learn/language-tour'],
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
        'language/pattern-matching',
        'language/type-system',
        'language/structural-vs-named-annotations',
        'language/type-declarations',
        'language/modules-imports-exports',
      ],
    },
    {
      type: 'category',
      label: 'Examples',
      items: ['examples/guided-examples', 'examples/common-patterns'],
    },
    {
      type: 'category',
      label: 'Embedding',
      items: [
        'embedding/embedding-overview',
        'embedding/real-world-embedding',
        'embedding/type-provider',
        'embedding/register-externs',
        'embedding/resolver-and-includes',
        'embedding/sandbox-and-safety',
      ],
    },
    {
      type: 'category',
      label: 'Stdlib',
      items: [
        'stdlib/stdlib-overview',
        'stdlib/stdlib-list',
        'stdlib/stdlib-option',
        'stdlib/stdlib-map',
        'stdlib/stdlib-string',
        'stdlib/stdlib-scalars',
        'stdlib/stdlib-recipes',
      ],
    },
    {
      type: 'category',
      label: 'Reference',
      items: ['reference/cli-reference', 'reference/stdlib-reference'],
    },
  ],
};

export default sidebars;
