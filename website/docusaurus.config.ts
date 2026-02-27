import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'FScript',
  tagline: 'A lightweight, embeddable interpreter with an F#/ML-style language.',
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
          lastVersion: 'current',
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
      defaultMode: 'light',
      disableSwitch: false,
      respectPrefersColorScheme: true,
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
          href: 'https://github.com/MagnusOpera/FScript',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting Started',
              to: '/manual/learn/quickstart',
            },
            {
              label: 'Embedding',
              to: '/manual/embedding/overview',
            },
          ],
        },
        {
          title: 'Project',
          items: [
            {
              label: 'Repository',
              href: 'https://github.com/MagnusOpera/FScript',
            },
            {
              label: 'Changelog',
              href: 'https://github.com/MagnusOpera/FScript/blob/main/CHANGELOG.md',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Magnus Opera.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['fsharp', 'bash'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
