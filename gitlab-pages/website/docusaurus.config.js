// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');
const singleTheme = require('prism-react-renderer/themes/duotoneLight');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'LIGO',
  tagline: 'LIGO is a friendly smart contract language for Tezos',
  url: 'https://ligolang.org',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.svg',
  projectName: 'ligo',
  organizationName: 'Marigold',

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: '../docs',
          sidebarPath: require.resolve('./sidebars.js'),
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          // editUrl:
          //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        }
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        logo: {
          alt: 'LIGO Logo',
          src: 'img/logo/logo.svg',
          srcDark: 'img/logo/logo-night.svg'
        },
        items: [
          { type: 'docsVersionDropdown', position: 'left' },
          { href: 'https://ide.ligolang.org/', label: 'Try Online', position: 'left', target: '_self' },
          { to: 'docs/intro/installation', label: 'Install', position: 'left' },
          { to: 'docs/intro/introduction', label: 'Docs', position: 'left' },
          {
            to: 'docs/tutorials/getting-started',
            label: 'Tutorials',
            position: 'left'
          },
          { href: 'https://academy.ligolang.org/', label: 'Academy', position: 'left', target: '_self' },
          { href: 'https://forum.tezosagora.org/tag/ligo', label: 'Blog', position: 'left' },
          { to: '/contact', label: 'Ask Questions', position: 'left' },
          { to: 'docs/faq/intro', label: 'FAQ', position: 'left' },
          { to: 'docs/next/intro/changelog', label: 'Changelog', position: 'left' }
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Install',
                to: 'docs/intro/installation',
              },
              {
                label: 'CLI Commands',
                to: 'docs/api/cli-commands',
              },
              {
                label: 'Cheat Sheet',
                to: 'docs/api/cheat-sheet',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Tezos Stack Exchange',
                href: 'https://tezos.stackexchange.com/questions/tagged/ligo',
              },
              {
                label: 'Discord',
                href: 'https://discord.gg/9rhYaEt',
              },
              {
                label: 'Telegram',
                href: 'https://t.me/LigoLang',
              },
              {
                label: 'Riot',
                href: 'https://riot.im/app/#/room/#ligo-public:matrix.org',
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/LigoLang',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Tutorials',
                to: 'docs/tutorials/getting-started',
              },
              {
                label: 'Blog',
                to: 'https://forum.tezosagora.org/tag/ligo',
              },
              {
                label: 'GitLab',
                href: 'https://gitlab.com/ligolang/ligo',
              },
              {
                label: 'Contribute',
                to: 'docs/contributors/origin',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} LIGO. All rights reserved.`,
      },
      image: 'img/logo/logo.png',
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        singleTheme: singleTheme
      },
      algolia: {
        // The application ID provided by Algolia
        appId: 'BH4D9OD16A',

        // Public API key: it is safe to commit it
        apiKey: 'b773aa611ab70b8e44a657eae5539176',

        indexName: 'ligolang',

        // Optional: see doc section below
        contextualSearch: true,

        // Optional: Algolia search parameters
        searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: 'search',

      },
    }),
  plugins: [
    [
      './src/plugins/syntax', {},
    ],
    // '@aldridged/docusaurus-plugin-lunr'
    require.resolve("@cmfcmf/docusaurus-search-local")
  ]
};

module.exports = config;
