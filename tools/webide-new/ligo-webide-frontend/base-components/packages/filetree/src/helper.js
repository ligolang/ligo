const computeType = (valueArr) => {
  const errorCount = valueArr.filter(item => item.type === 'error').length
  const warningCount = valueArr.filter(item => item.type === 'warning').length

  return {
    type: errorCount ? 'error' : warningCount ? 'warning' : 'default',
    count: errorCount || warningCount || -1
  }
}

const updateEachNode = (typeInfo, pathInfo, treeMap) => {
  const refreshNode = (cur, nodeValue, isLeaf) => {
    if (!nodeValue) {
      treeMap[cur] = {
        name: cur,
        type: typeInfo.type,
        isLeaf: isLeaf
      }
      if (isLeaf) {
        treeMap[cur]['count'] = typeInfo.count
      }
    } else {
      const oldDefault = nodeValue.type === 'default'
      const oldError = nodeValue.type === 'error'
      const oldWarning = !oldDefault && !oldError
      const newError = typeInfo.type === 'error'
      if (oldError) {
        return
      }
      if (oldWarning) {
        nodeValue.type = newError ? 'error' : 'warning'
        if (isLeaf) {
          nodeValue.count = newError ? typeInfo.count : nodeValue.count
        }
        return
      }
      if (oldDefault) {
        nodeValue.type = typeInfo.type
        if (isLeaf) {
          nodeValue.count = typeInfo.count
        }
      }
    }
  }

  pathInfo.forEach((cur, index) => {
    const isLeaf = index === pathInfo.length - 1
    const nodeValue = treeMap[cur]
    refreshNode(cur, nodeValue, isLeaf)
  })
}

const updateErrorInfo = (decorations, fileKey) => {
  const keyArr = Object.keys(decorations)

  return keyArr.reduce((prev, cur) => {
    const typeInfo = computeType(decorations[cur])
    const pathInfo = cur.replace(fileKey + '/', '').split('/')

    updateEachNode(typeInfo, pathInfo, prev)
    return prev
  }, {})
}

const travelTree = (treeData, fn, extraValue, stopCheck) => {
  const travel = (tree, fn) => {
    fn(tree, extraValue)
    if (!tree.children || stopCheck(tree)) return
    for (let i = 0; i < tree.children.length; i++) {
      travel(tree.children[i], fn)
    }
  }
  travel(treeData, fn)
}

export {
  updateErrorInfo,
  travelTree
}
