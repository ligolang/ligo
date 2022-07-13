import { getColor } from '~/base-components/ui-components'
import convert from 'color-convert'

import * as monaco from 'monaco-editor'

const hex = value => {
  const hexValue = Number(value || 0).toString(16)
  return hexValue.length === 1 ? `0${hex}` : hexValue
}

export default async function () {
  const primaryColorRgb = getColor('--color-primary')
  const rgb = primaryColorRgb.match(/\((\d+),\s*(\d+),\s*(\d+)\)/)
  const hsl = convert.rgb.hsl(+rgb[1], +rgb[2], +rgb[3])
  const primaryColor = convert.hsl.hex(hsl[0], hsl[1], hsl[2])
  const lightPrimaryColor = convert.hsl.hex(hsl[0], 100, 75)

  monaco.editor.defineTheme('obsidians',
    {
      base: 'vs',
      inherit: true,
      rules: [],
      colors: {
        'editor.background': '#fff',
        'editor.lineHighlightBackground': '#fff',
        'editorLineNumber.foreground': '#888',
        'menu.foreground': '#FFFFFF',
        'menu.background': '#eff5ff',
        'menu.selectionBackground': '#d0e3ff'
      }
    }

  // {
    // base: 'vs' // can also be vs-dark or hc-black
    // inherit: true // can also be false to completely replace the builtin rules
    // colors: {
    //   "editor.foreground": '#F9FAF4',
    //   "editor.background": '#252527',
    //   "editor.selectionBackground": '#494950',
    //   "editor.lineHighlightBackground": '#2F2F32',
    //   "editorCursor.foreground": '#F9FAF4',
    //   "textLink.foreground": `#${primaryColor}`,
    //   "focusBorder": `#${primaryColor}`,
    //   // "inputOption.activeBackground": "#AD7AFF88",
    //   // "input.border": `#${primaryColor}`,
    //   "menu.foreground": '#FFFFFF',
    //   "menu.background": '#252527',
    //   "menu.selectionBackground": `#${primaryColor}`,
    //   "list.focusForeground": '#F9FAF4',
    //   "list.focusBackground": `#${primaryColor}`,
    //   // "list.highlightForeground": '#F9FAF4,
    // },
    // rules: [
    //   { token: '', foreground: 'F9FAF4', background: '252527' },
    //   { token: 'constant.language', foreground: '569CD6' },
    //   { token: 'variable.language', foreground: '569CD6' },
    //   { token: 'constant.numeric', foreground: 'B5CEA8' },
    //   { token: 'variable.other.enummember', foreground: 'B5CEA8' },
    //   { token: 'keyword.operator.plus.exponent', foreground: 'B5CEA8' },
    //   { token: 'keyword.operator.minus.exponent', foreground: 'B5CEA8' },

    //   { token: 'storage', foreground: '569CD6' },
    //   { token: 'storage.type', foreground: '569CD6' },
    //   { token: 'storage.modifier', foreground: '569CD6' },
    //   { token: 'keyword.operator.noexcept', foreground: '569CD6' },
    //   { token: 'string', foreground: 'CE9178' },
    //   { token: 'string.tag', foreground: 'CE9178' },
    //   { token: 'string.value', foreground: 'CE9178' },
    //   { token: 'string.regexp', foreground: 'D16969' },

    //   { token: 'keyword', foreground: lightPrimaryColor },
    //   { token: 'keyword.control', foreground: lightPrimaryColor },
    //   { token: 'keyword.operator', foreground: 'D4D4D4' },
    //   { token: 'keyword.operator.new', foreground: '569CD6' },
    //   { token: 'keyword.operator.expression', foreground: '569CD6' },
    //   { token: 'keyword.operator.cast', foreground: '569CD6' },
    //   { token: 'keyword.operator.sizeof', foreground: '569CD6' },
    //   { token: 'keyword.operator.alignof', foreground: '569CD6' },
    //   { token: 'keyword.operator.typeid', foreground: '569CD6' },
    //   { token: 'keyword.operator.alignas', foreground: '569CD6' },
    //   { token: 'keyword.operator.instanceof', foreground: '569CD6' },
    //   { token: 'keyword.operator.logical.python', foreground: '569CD6' },
    //   { token: 'keyword.operator.wordlike', foreground: '569CD6' },
    //   { token: 'keyword.other.unit', foreground: 'B5CEA8' },

    //   { token: 'variable.language', foreground: '569CD6' },

    //   { token: 'entity.name.function', foreground: 'DCDCAA' },
    //   { token: 'support.function', foreground: 'DCDCAA' },

    //   { token: 'variable', foreground: '9CDCFE' },
    //   { token: 'support.variable', foreground: '9CDCFE' },
    //   { token: 'variable.other.constant', foreground: '4FC1FF' },

    //   { token: 'type', foreground: '66D9EF', fontStyle: 'italic' },
    //   { token: 'function', foreground: lightPrimaryColor, fontStyle: 'italic' },
      // { token: 'type.eosio', foreground: '66D9EF', fontStyle: 'bold' },
    // ]
  // })
  )
}
