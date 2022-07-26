export default function getColor (color) {
  if (color.startsWith('--color')) {
    color = getComputedStyle(document.documentElement).getPropertyValue(color)
  }
  const d = document.createElement('div')
  d.style.color = color

  document.body.appendChild(d)
  const rgbColor = window.getComputedStyle(d).color
  document.body.removeChild(d)

  return rgbColor
}