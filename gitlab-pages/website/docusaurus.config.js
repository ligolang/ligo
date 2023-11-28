// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const singleTheme = require("prism-react-renderer/themes/duotoneLight");

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Ligo",
  url: "https://ligolang.org",
  baseUrl: "/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/favicon.svg",
  projectName: "ligo",
  organizationName: "Marigold",
  customFields: {
    REACT_APP_GOOGLE_ANALYTICS_ID: "G-V5S4SDLK4Z",
  },
  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  plugins: ["@ligo/syntax", "docusaurus-plugin-sass"],

  presets: [
    [
      "@docusaurus/preset-classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: "../docs",
          sidebarPath: require.resolve("./sidebars.js"),
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          // editUrl:
          //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        theme: {
          customCss: [
            require.resolve("./src/css/colors.css"),
            require.resolve("./src/css/breakpoints.scss"),
            require.resolve("./src/css/theme-light.css"),
            require.resolve("./src/css/custom.css"),
          ],
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        defaultMode: "dark",
        disableSwitch: false,
        respectPrefersColorScheme: false,
      },
      announcementBar: {
        id: "support_us",
        content:
          "<b>Ligo v1 is now available ! Check the migration guide : https://ligolang.org/docs/next/faq/v1-migration-guide ! </b>",
        backgroundColor: "#003ee0",
        textColor: "#efefef",
        isCloseable: false,
      },
      navbar: {
        logo: {
          width: "104",
          height: "32",
          alt: "LIGO Logo",
          src: "img/logo/logo.svg",
          srcDark: "img/logo/logo-night.svg",
        },
        items: [
          { type: "docsVersionDropdown", position: "left" },
          { to: "docs/intro/introduction", label: "Docs", position: "left", target: "_self" },
          { to: "docs/reference/toplevel", label: "API", position: "left" },
          { to: "docs/faq/intro", label: "FAQ", position: "left" },
          { to: "https://packages.ligolang.org/packages", label: "Registry", position: "right" },
          { to: "https://ide.ligolang.org/", label: "Web IDE", position: "right" },
        ],
      },
      footer: {
        logo: {
          alt: "Ligo Logo",
          src: "img/ligo-logo.svg",
          srcDark: "img/ligo-logo-light.svg",
          width: 50,
          height: 50,
        },
        style: "dark",
        links: [
          {
            title: "LEARN LIGO",
            items: [
              {
                label: "Get started",
                to: "docs/intro/introduction",
              },
              {
                label: "Registry",
                href: "https://packages.ligolang.org/packages",
              },
              {
                label: "Ligo CLI",
                to: "docs/api/cli-commands",
              },
              {
                label: "Changelog",
                to: "docs/intro/changelog",
              },
              {
                label: "FAQ",
                to: "docs/faq/intro",
              },
            ],
          },
          {
            title: "COMMUNITY",
            items: [
              {
                label: "Discord",
                href: "https://discord.gg/tezos",
                rel: "noopener noreferrer nofollow",
              },
              {
                label: "YouTube",
                href: "https://www.youtube.com/playlist?list=PLmDOlM4rqLvInuervAU4YuU39ThuHw3fM",
                rel: "noopener noreferrer nofollow",
              },
              {
                label: "X",
                href: "https://x.com/LigoLang",
                rel: "noopener noreferrer nofollow",
              },
              {
                label: "Telegram",
                href: "https://t.me/LigoLang",
                rel: "noopener noreferrer nofollow",
              },
              {
                label: "LinkedIn",
                href: "https://www.linkedin.com/company/marigold-ligolang/",
                rel: "noopener noreferrer nofollow",
              },
            ],
          },
          {
            title: "CONTRIBUTE",
            items: [
              {
                label: "Sources",
                href: "https://gitlab.com/ligolang/ligo",
                rel: "noopener noreferrer nofollow",
              },
              {
                label: "Issues",
                href: "https://gitlab.com/ligolang/ligo/-/issues",
                rel: "noopener noreferrer nofollow",
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} LIGO. All rights reserved.`,
      },
      image: "img/logo/logo.png",
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        singleTheme: singleTheme,
      },
      algolia: {
        // The application ID provided by Algolia
        appId: "ZJTW93II01",

        // Public API key: it is safe to commit it
        apiKey: "666cd6151b57b31964fece17ad094ba9",

        indexName: "ligolang",

        // Optional: see doc section below
        contextualSearch: true,

        // Optional: Algolia search parameters
        searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: "search",
      },
    }),
};

module.exports = config;
