import i18n from 'i18next';

const init = (resources, options) => {
  i18n
  .init({
    resources,
    fallbackLng: 'en',
    lng: 'en',
    debug: false,

    interpolation: {
      escapeValue: false, // not needed for react as it escapes by default
    },

    ...options,
  })

  return i18n
}

const t = i18n.t.bind(i18n)

export {
  init,
  t,
  i18n,
}
