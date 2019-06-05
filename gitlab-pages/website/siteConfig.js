/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

// List of projects/orgs using your project for the users page.
const partners = [
  {
    caption: 'Nomadic Labs',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://media.licdn.com/dms/image/C560BAQGE4y1pddQlrw/company-logo_200_200/0?e=2159024400&v=beta&t=aszI_Js3VuY7P20n1pYVAJmeRhM1kC_ftMrowcQa2ZQ',
    infoLink: 'https://www.nomadic-labs.com/',
    pinned: true,
  },
  {
    caption: 'Tocqueville Group',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://tqgroup.io/static/images/logo.svg',
    infoLink: 'https://tqgroup.io/',
    pinned: true,
  },
  {
    caption: 'Stove Labs',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://stove-labs.com/logo_transparent.png',
    infoLink: 'https://stove-labs.com',
    pinned: true,
  },
];

const team = [
  {
    caption: 'Gabriel Alfour',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://thepowerofthedream.org/wp-content/uploads/2015/09/generic-profile-picture-600x600.jpg',
    infoLink: '#',
    pinned: true,
  },
  {
    caption: 'Georges Duperon',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://thepowerofthedream.org/wp-content/uploads/2015/09/generic-profile-picture-600x600.jpg',
    infoLink: '#',
    pinned: true,
  },
  {
    caption: 'Christian Rinderknecht',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://thepowerofthedream.org/wp-content/uploads/2015/09/generic-profile-picture-600x600.jpg',
    infoLink: '#',
    pinned: true,
  },
  {
    caption: 'Brice Aldrich',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://thepowerofthedream.org/wp-content/uploads/2015/09/generic-profile-picture-600x600.jpg',
    infoLink: '#',
    pinned: true,
  },
  {
    caption: 'Matej Sima',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: 'https://scontent-frt3-2.xx.fbcdn.net/v/t1.0-9/56644817_2276459725943174_4007605942056124416_n.jpg?_nc_cat=107&_nc_ht=scontent-frt3-2.xx&oh=e8a86a2cfe76798cbdc28a0769ebccb1&oe=5D5423F0',
    infoLink: 'https://sk.linkedin.com/in/matejsima',
    pinned: true,
  },
];

const siteConfig = {
  title: 'LIGO', // Title for your website.
  tagline: 'LIGO is a statically typed high-level smart-contract language that compiles down to Michelson. It seeks to be easy to use, extensible and safe.',
  url: 'https://your-docusaurus-test-site.com', // Your website URL
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'ligo',
  organizationName: 'marigold',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'setup-installation', label: 'Docs'},
    {doc: 'api-cli-commands', label: 'CLI'},
    {doc: 'contributors/origin', label: 'Contributing'},
  ],

  // If you have users set above, you add it here:
  partners,
  team,

  /* path to images for header/footer */
  headerIcon: 'img/logo.svg',
  footerIcon: 'img/favicon.ico',
  favicon: 'img/favicon.ico',

  /* Colors for website */
  colors: {
    primaryColor: '#1A1A1A',
    secondaryColor: '#1A1A1A',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Marigold`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/undraw_online.svg',
  twitterImage: 'img/undraw_tweetstorm.svg',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
    repoUrl: 'https://gitlab.com/ligolang/ligo',
};

module.exports = siteConfig;
