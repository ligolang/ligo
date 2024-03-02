const fs = require('fs');
const path = require('path');
const api_value = require('./api_value'); // Assurez-vous que le chemin est correct

const targetDirectory = process.argv[2] ? path.resolve(process.argv[2]) : __dirname;

const sidebarPath = path.join(targetDirectory, 'sidebars.js');

const sidebarContent = require(sidebarPath);

// Mise à jour de la section 'API' -> 'Language' avec le nouveau contenu
sidebarContent.API.Language = api_value;

// Conversion de l'objet mis à jour en chaîne pour l'écriture dans le fichier
// Préserve le formatage pour une meilleure lisibilité
const updatedContent = `/**
// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = ${JSON.stringify(sidebarContent, null, 2)};

module.exports = sidebars;
`;

// Écriture du contenu mis à jour dans 'sidebar.js'
fs.writeFileSync(sidebarPath, updatedContent, 'utf8');
