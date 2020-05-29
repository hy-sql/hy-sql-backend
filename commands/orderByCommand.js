const parseOrderBy = (slicedCommandAsStringArray) => {
    console.log('hello', slicedCommandAsStringArray)
    return slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'ORDER BY'
        ? {
            keyword: slicedCommandAsStringArray
                .slice(0, 2)
                .join(' ')
                .toUpperCase(),
            columnName: slicedCommandAsStringArray[2],
            order: slicedCommandAsStringArray
                .slice(3)
                .join(' ')
                .toUpperCase()
                .trim(),
        }
        : null
}

const hasOrderByKeywords = (fullCommandAsStringArray) => {
    const hasOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const hasBy = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasOrder > 0 && hasBy > 0 ? hasOrder < hasBy : false
}

const hasWhereOrderByKeywords = (fullCommandAsStringArray) => {
    const hasWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )
    const hasOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )
    const hasBy = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasWhere > 0 && hasOrder > 0 && hasBy > 0
        ? hasWhere < hasOrder && hasOrder < hasBy
        : false
}

module.exports = { parseOrderBy, hasOrderByKeywords, hasWhereOrderByKeywords }
