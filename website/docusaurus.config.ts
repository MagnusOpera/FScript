import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'FScript',
  tagline: 'A compact, statically typed language built to live inside your product.',
  favicon: 'img/favicon.ico',

  future: {
    v4: true,
  },

  url: 'https://magnusopera.github.io',
  baseUrl: '/FScript/',

  organizationName: 'MagnusOpera',
  projectName: 'FScript',

  onBrokenLinks: 'throw',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          routeBasePath: 'manual',
          // On release/tag builds we set FSCRIPT_DOCS_LAST_VERSION to the tag (e.g. 0.60.0)
          // so the freshly released docs are treated as latest and don't show "unmaintained".
          lastVersion: process.env.FSCRIPT_DOCS_LAST_VERSION ?? 'current',
          versions: {
            current: {
              label: 'Next',
            },
          },
          editUrl: 'https://github.com/MagnusOpera/FScript/tree/main/website/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/fscript-social-card.png',
    colorMode: {
      defaultMode: 'dark',
      disableSwitch: false,
      respectPrefersColorScheme: false,
    },
    navbar: {
      title: 'FScript',
      logo: {
        alt: 'FScript Logo',
        src: 'img/fscript-icon.png',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docs',
          position: 'left',
          label: 'Docs',
        },
        {
          type: 'docsVersionDropdown',
          position: 'left',
          dropdownActiveClassDisabled: true,
        },
        {
          to: '/sandbox',
          label: 'Playground',
          position: 'left',
        },
        {
          href: 'https://github.com/MagnusOpera/FScript',
          label: 'GitHub ↗',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      copyright: `<span class="footer-brand">© ${new Date().getFullYear()} <a href="https://magnusopera.io"><img src="/FScript/img/magnus-opera-logo.svg" alt="" /> Magnus Opera SAS</a></span>`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['fsharp', 'bash'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
